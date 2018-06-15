{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       , biSer
       , biSerIO
       , biExtRepSer
       , biExtRepSerIO
       ) where

import           Universum

import           Control.Monad.ST hiding (stToIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder.Extra as Builder

import           Node.Message.Class (Serializable', Serializable (..), SerAttrExtRep, hoistSerializable)
import qualified Node.Message.Decoder as TW

import           Pos.Binary.Class (Bi (..), BiExtRep (..), DecoderAttrKind (..))
import qualified Pos.Binary.Class as Bi

data BiP = BiP

biPackMsg :: Bi.Encoding -> LBS.ByteString
biPackMsg = Builder.toLazyByteStringWith strategy mempty . Bi.toBuilder
  where
    strategy = Builder.untrimmedStrategy 1024 4096

type M = ST RealWorld

unpackMsg :: Bi.Decoder RealWorld t -> (Proxy t -> Text) -> TW.Decoder M t
unpackMsg decoder getLabel = TW.Decoder (fromDecoder Proxy getLabel (Bi.deserialiseIncremental decoder))

fromDecoder :: Proxy t -> (Proxy t -> Text) -> M (Bi.IDecode RealWorld t) -> M (TW.DecoderStep M t)
fromDecoder p getLabel x = do
    nextStep <- x
    case nextStep of
      (Bi.Partial cont)    -> return $ TW.Partial $ \bs -> TW.Decoder $ fromDecoder p getLabel (cont bs)
      (Bi.Done bs off t)   -> return (TW.Done bs off t)
      (Bi.Fail bs off exn) -> do
          let msg = "fromDecoder failure for " <> getLabel p <> ": " <> show exn <> ", leftover: " <> show bs
          return (TW.Fail bs off msg)

biSer :: forall t. Bi t => Serializable' BiP M t
biSer = Serializable packBi (unpackMsg Bi.decode Bi.label)
    where
    packBi :: t -> M LBS.ByteString
    packBi = pure . biPackMsg . encode

biSerIO :: forall t. Bi t => Serializable' BiP IO t
biSerIO = hoistSerializable stToIO biSer

biExtRepSer :: forall t. BiExtRep t => Serializable SerAttrExtRep BiP M (t 'AttrOffsets) (t 'AttrExtRep)
biExtRepSer = SerializableWithExtRep
    packBiExtRep
    (unpackMsg Bi.decodeWithOffsets Bi.labelExtRep)
    spliceExtRep
    where
        packBiExtRep :: t 'AttrExtRep -> M LBS.ByteString
        packBiExtRep = pure . biPackMsg . encodeExtRep

biExtRepSerIO :: forall t. BiExtRep t => Serializable SerAttrExtRep BiP IO (t 'AttrOffsets) (t 'AttrExtRep)
biExtRepSerIO = hoistSerializable stToIO biExtRepSer
