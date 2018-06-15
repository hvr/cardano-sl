{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}

module Node.Message.Class
    ( SerAttrNone
    , SerAttrExtRep
    , Serializable (..)
    , Serializable'
    , pack
    , unpack
    , coerceExtRep
    , hoistSerializable

    , Message (..)
    , messageCode'

    , MessageCode
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T
import           Data.Word (Word16)
import qualified Formatting as F
import           Node.Message.Decoder (Decoder, hoistDecoder)

-- * Message name

type MessageCode = Word16

-- | Defines type with it's own `MessageCode`.
class Message m where
    -- | Uniquely identifies this type
    messageCode :: Proxy m -> MessageCode

    -- | Description of message, for debug purposes
    formatMessage :: m -> T.Text
    default formatMessage :: F.Buildable m => m -> T.Text
    formatMessage = F.sformat F.build

-- | As `messageName`, but accepts message itself, may be more convinient is most cases.
messageCode' :: Message m => m -> MessageCode
messageCode' = messageCode . proxyOf
  where
    proxyOf :: a -> Proxy a
    proxyOf _ = Proxy

data SerAttrNone
data SerAttrExtRep

-- |
-- Terms which define a way to (de)-serialize object @r@ with given packing type @p@.
-- It abstract incremental deserialization and can be build on top of `Bi`
-- instance.
data Serializable attr packingType m t' t where
    -- | (de)serialize without external representation, e.g. using `Bi` class.
    Serializable           :: (t -> m LBS.ByteString) -> Decoder m t -> Serializable SerAttrNone packingType m t t
    -- | (de)serialize with external representation, e.g. using `BiExtRep`
    -- class.
    SerializableWithExtRep :: (t -> m LBS.ByteString) -> Decoder m t' -> (BS.ByteString -> t' -> t) -> Serializable SerAttrExtRep packingType m t' t 

type Serializable' packingType m t = Serializable SerAttrNone packingType m t t

pack :: Serializable packingType attr m t' t
     -> t
     -> m LBS.ByteString
pack (Serializable f _) = f
pack (SerializableWithExtRep f _ _) = f

unpack :: Serializable packingType attr m t' t
       -> Decoder m t'
unpack (Serializable _ f) = f
unpack (SerializableWithExtRep _ f _) = f

coerceExtRep :: Serializable attr packingType m t' t
             -> BS.ByteString
             -> t'
             -> t
coerceExtRep (Serializable           _  _  ) = \_ t -> t
coerceExtRep (SerializableWithExtRep _  _ f) = f

hoistSerializable
    :: forall attr packingType m n t' t .
       Functor n
    => (forall a. m a -> n a)
    -> Serializable attr packingType m t' t
    -> Serializable attr packingType n t' t
hoistSerializable f (Serializable packS (unpackS :: Decoder m t))
    = Serializable (f . packS) (hoistDecoder f unpackS)
hoistSerializable f (SerializableWithExtRep packS unpackS coerceExtRepS)
    = SerializableWithExtRep (f . packS) (hoistDecoder f unpackS) coerceExtRepS
