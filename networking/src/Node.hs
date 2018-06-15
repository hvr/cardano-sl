{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns                #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE ExistentialQuantification   #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GADTSyntax                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE RecursiveDo                 #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeApplications            #-}

module Node (

      Node(..)
    , LL.NodeId(..)
    , LL.NodeEnvironment(..)
    , LL.defaultNodeEnvironment
    , LL.ReceiveDelay
    , LL.noReceiveDelay
    , LL.constantReceiveDelay
    , nodeEndPointAddress
    , NodeAction(..)
    , node

    , LL.NodeEndPoint(..)
    , simpleNodeEndPoint
    , manualNodeEndPoint

    , LL.NodeState(..)

    , MessageCode
    , Message (..)

    , Converse(..)
    , Conversation(..)
    , ConversationActions(send, recv)
    , converseWith

    , Listener (..)
    , ListenerAction

    , LL.Statistics(..)
    , LL.PeerStatistics(..)

    , LL.Timeout(..)

    ) where

import           Control.Concurrent.STM
import           Control.Exception (Exception (..), SomeException, catch,
                                    mask, throwIO)
import           Control.Monad (unless, when)
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Proxy (Proxy (..))
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           Formatting (sformat, shown, (%))
import qualified Network.Transport as NT
import           Node.Conversation
import           Node.Internal (ChannelIn, ChannelOut)
import qualified Node.Internal as LL
import           Node.Message.Class (Message (..), MessageCode, Serializable', Serializable (..), pack,
                                     unpack, coerceExtRep)
import           Node.Message.Decoder (ByteOffset, Decoder (..), DecoderStep (..), continueDecoding)
import           Pos.Util.Trace (Trace, Severity (..), traceWith)
import           System.Random (StdGen)


data Node = Node {
      nodeId         :: LL.NodeId
    , nodeEndPoint   :: NT.EndPoint
    , nodeStatistics :: IO (LL.Statistics)
    }

nodeEndPointAddress :: Node -> NT.EndPointAddress
nodeEndPointAddress (Node addr _ _) = LL.nodeEndPointAddress addr

data Input t = Input t | End
  deriving (Functor)

data LimitExceeded = LimitExceeded
  deriving (Show, Typeable)

instance Exception LimitExceeded

-- | Custom exception thrown by recvNext
--
-- This carries the fields of the 'Fail' constructor of 'Decoder'
data NoParse = NoParse !BS.ByteString !ByteOffset !T.Text
  deriving (Show, Typeable)

instance Exception NoParse where
  displayException (NoParse trailing offset err) =
       "recvNext failed with " ++ T.unpack err
    ++ " (length trailing = " ++ show (BS.length trailing)
    ++ ", offset = " ++ show offset ++ ")"

-- | A ListenerAction with existential snd and rcv types and suitable
--   constraints on them.
data Listener packingType peerData where
  Listener
    :: Message rcv
    => Serializable' packingType IO snd
    -> Serializable attr packingType IO rcv' rcv
    -> ListenerAction packingType peerData snd rcv
    -> Listener packingType peerData

-- | A listener that handles an incoming bi-directional conversation.
type ListenerAction packingType peerData snd rcv =
       -- TODO do not take the peer data here, it's already in scope because
       -- the listeners are given as a function of the remote peer's
       -- peer data. This remains just because cardano-sl will need a big change
       -- to use it properly.
       peerData
    -> LL.NodeId
    -> ConversationActions snd rcv
    -> IO ()

-- | Gets message type basing on type of incoming messages
listenerMessageCode :: Listener packing peerData -> MessageCode
listenerMessageCode (Listener 
        _ _ (_ :: peerData -> LL.NodeId -> ConversationActions snd rcv -> IO ())
    ) = messageCode (Proxy :: Proxy rcv)

type ListenerIndex packing peerData =
    Map MessageCode (Listener packing peerData)

makeListenerIndex :: [Listener packing peerData]
                  -> (ListenerIndex packing peerData, [MessageCode])
makeListenerIndex = foldr combine (M.empty, [])
    where
    combine action (dict, existing) =
        let name = listenerMessageCode action
            (replaced, dict') = M.insertLookupWithKey (\_ _ _ -> action) name action dict
            overlapping = maybe [] (const [name]) replaced
        in  (dict', overlapping ++ existing)

nodeConverse
    :: forall packingType peerData .
       LL.Node packingType peerData
    -> Serializable' packingType IO MessageCode
    -> Converse packingType peerData
nodeConverse nodeUnit serializeMessageCode = Converse nodeConverse
  where

    mtu = LL.nodeMtu (LL.nodeEnvironment nodeUnit)

    nodeConverse
        :: forall t .
           LL.NodeId
        -> (peerData -> Conversation packingType t)
        -> IO t
    nodeConverse = \nodeId k ->
        LL.withInOutChannel nodeUnit nodeId $ \peerData inchan outchan -> case k peerData of
            Conversation
                serializeSnd
                serializeRcv
                (converse :: ConversationActions snd rcv -> IO t) -> do
                    let msgCode = messageCode (Proxy :: Proxy snd)
                        cactions :: ConversationActions snd rcv
                        cactions = nodeConversationActions
                            nodeUnit
                            nodeId
                            serializeSnd
                            serializeRcv
                            inchan
                            outchan
                    pack serializeMessageCode msgCode >>= LL.writeMany mtu outchan
                    converse cactions


-- | Conversation actions for a given peer and in/out channels.
nodeConversationActions
    :: forall attr packingType peerData snd rcv' rcv .
       LL.Node packingType peerData
    -> LL.NodeId
    -> Serializable' packingType IO snd
    -> Serializable attr packingType IO rcv' rcv
    -> ChannelIn
    -> ChannelOut
    -> ConversationActions snd rcv
nodeConversationActions node _ serializeSnd serializeRcv inchan outchan =
    ConversationActions nodeSend nodeRecv nodeSendRaw
    where

    mtu = LL.nodeMtu (LL.nodeEnvironment node)

    nodeSend = \body ->
        pack serializeSnd body >>= nodeSendRaw

    nodeRecv :: Word32 -> IO (Maybe rcv)
    nodeRecv limit = do
        next <- recvNext serializeRcv (fromIntegral limit :: Int) inchan
        case next of
            End     -> pure Nothing
            Input t -> pure (Just t)

    nodeSendRaw = LL.writeMany mtu outchan

data NodeAction packing peerData t =
    NodeAction (peerData -> [Listener packing peerData])
               (Converse packing peerData -> IO t)

simpleNodeEndPoint
    :: NT.Transport
    -> IO LL.Statistics
    -> LL.NodeEndPoint
simpleNodeEndPoint transport _ = LL.NodeEndPoint {
      newNodeEndPoint = NT.newEndPoint transport
    , closeNodeEndPoint = NT.closeEndPoint
    }

manualNodeEndPoint
    :: NT.EndPoint
    -> IO LL.Statistics
    -> LL.NodeEndPoint
manualNodeEndPoint ep _ = LL.NodeEndPoint {
      newNodeEndPoint = pure $ Right ep
    , closeNodeEndPoint = NT.closeEndPoint
    }

-- | Spin up a node. You must give a function to create listeners given the
--   'NodeId', and an action to do given the 'NodeId' and sending actions.
--
--   The 'NodeAction' must be lazy in the components of the 'Node' passed to
--   it. Its 'NodeId', for instance, may be useful for the listeners, but is
--   not defined until after the node's end point is created, which cannot
--   happen until the listeners are defined--as soon as the end point is brought
--   up, traffic may come in and cause a listener to run, so they must be
--   defined first.
--
--   The node will stop and clean up once that action has completed. If at
--   this time there are any listeners running, they will be allowed to
--   finish.
node
    :: forall packingType peerData t .
       Trace IO (Severity, T.Text)
    -> (IO LL.Statistics -> LL.NodeEndPoint)
    -> (IO LL.Statistics -> LL.ReceiveDelay)
       -- ^ delay on receiving input events.
    -> (IO LL.Statistics -> LL.ReceiveDelay)
       -- ^ delay on receiving new connections.
    -> StdGen
    -> Serializable' packingType IO peerData
    -> Serializable' packingType IO MessageCode
    -> peerData
    -> LL.NodeEnvironment
    -> (Node -> NodeAction packingType peerData t)
    -> IO t
node logTrace mkEndPoint mkReceiveDelay mkConnectDelay prng serializePeerData serializeMessageCode peerData nodeEnv k = do
    rec { let nId = LL.nodeId llnode
              endPoint = LL.nodeEndPoint llnode
              nodeUnit = Node nId endPoint (LL.nodeStatistics llnode)
              NodeAction mkListeners (act :: Converse packingType peerData -> IO t) = k nodeUnit
              -- Index the listeners by message name, for faster lookup.
              -- TODO: report conflicting names, or statically eliminate them using
              -- DataKinds and TypeFamilies.
              listenerIndices :: peerData -> ListenerIndex packingType peerData 
              listenerIndices = fmap (fst . makeListenerIndex) mkListeners
              converse :: Converse packingType peerData
              converse = nodeConverse llnode serializeMessageCode
        ; llnode <- LL.startNode
              serializePeerData
              logTrace
              peerData
              (mkEndPoint . LL.nodeStatistics)
              (mkReceiveDelay . LL.nodeStatistics)
              (mkConnectDelay . LL.nodeStatistics)
              prng
              nodeEnv
              (handlerInOut llnode listenerIndices)
        }
    -- The node is held open for the duration of the 'act'.
    -- Any exception in there kills the node, whereas normal termination
    -- gracefully stops the node. In the latter case, the node will wait for
    -- any running network handlers to finish.
    mask $ \restore -> do
        t <- restore (act converse) `catch` \e -> do
            logException e
            LL.killNode llnode
            throwIO e
        logNormalShutdown
        LL.stopNode llnode
        return t
  where
    logNormalShutdown :: IO ()
    logNormalShutdown = traceWith logTrace (Info, "stopping normally")
    logException :: SomeException -> IO ()
    logException e = traceWith logTrace (Error, sformat ("stopping with exception " % shown) e)
    -- Handle incoming data from a bidirectional connection: try to read the
    -- message name, then choose a listener and fork a thread to run it.
    handlerInOut
        :: LL.Node packing peerData
        -> (peerData -> ListenerIndex packing peerData)
        -> peerData
        -> LL.NodeId
        -> ChannelIn
        -> ChannelOut
        -> IO ()
    handlerInOut nodeUnit listenerIndices peerData peerId inchan outchan = do
        let listenerIndex = listenerIndices peerData
        -- Use maxBound to receive the MessageCode.
        -- The understanding is that the Serializable instance for it against
        -- the given packing type shouldn't accept arbitrarily-long input (it's
        -- a Word16, surely it serializes to (2 + c) bytes for some c).
        input <- recvNext serializeMessageCode maxBound inchan
        case input of
            End -> traceWith logTrace (Debug, "handlerInOut : unexpected end of input")
            Input msgCode -> do
                let listener = M.lookup msgCode listenerIndex
                case listener of
                    Just (Listener serializeSnd serializeRcv action) ->
                        let cactions = nodeConversationActions
                                nodeUnit
                                peerId
                                serializeSnd
                                serializeRcv
                                inchan
                                outchan
                        in  action peerData peerId cactions
                    Nothing -> traceWith logTrace (Error, sformat ("no listener for "%shown) msgCode)

-- | Try to receive and parse the next message, subject to a limit on the
--   number of bytes which will be read.
--
--   An empty ByteString will never be passed to a decoder.
recvNext
    :: forall attr packingType rcv' rcv .
       Serializable attr packingType IO rcv' rcv
    -> Int
    -> ChannelIn
    -> IO (Input rcv)
recvNext serializeRcv limit (LL.ChannelIn channel) = readNonEmpty (return End) $ \bs -> do
    -- limit' is the number of bytes that 'go' is allowed to pull.
    -- It's assumed that reading from the channel will bring in at most
    -- some limited number of bytes, so 'go' may bring in at most this
    -- many more than the limit.
    let limit' = limit - BS.length bs
    decoderStep <- runDecoder (unpack serializeRcv)
    (trailing, bsRep,  outcome) <- continueDecoding decoderStep bs >>= go limit' bs
    unless (BS.null trailing) (atomically $ unGetTChan channel (Just trailing))
    return $ coerceExtRep serializeRcv bsRep <$> outcome
  where

    readNonEmpty :: IO t -> (BS.ByteString -> IO t) -> IO t
    readNonEmpty nothing just = do
        mbs <- atomically $ readTChan channel
        case mbs of
            Nothing -> nothing
            Just bs -> if BS.null bs then readNonEmpty nothing just else just bs

    go !remaining !bs decoderStep = case decoderStep of
        Fail trailing offset err -> throwIO $ NoParse trailing offset err
        Done trailing _ thing -> return (trailing, bs, Input thing)
        Partial next -> do
            when (remaining < 0) (throwIO LimitExceeded)
            readNonEmpty (runDecoder (next Nothing) >>= go remaining bs) $ \bs' ->
                let remaining' = remaining - BS.length bs'
                in  runDecoder (next (Just bs')) >>= go remaining' (bs <> bs')
