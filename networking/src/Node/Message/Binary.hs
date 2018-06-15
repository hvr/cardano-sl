{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Node.Message.Binary
    ( BinaryP
    , binarySerialization
    ) where

import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Node.Message.Class (Serializable', Serializable (..))
import           Node.Message.Decoder (Decoder (..), DecoderStep (..))

data BinaryP

packBinary :: Bin.Put -> LBS.ByteString
packBinary =
    BS.toLazyByteStringWith
        (BS.untrimmedStrategy 256 4096)
        LBS.empty
    . Bin.execPut

fromBinaryDecoder :: Applicative m => Bin.Decoder t -> DecoderStep m t
fromBinaryDecoder (Bin.Done bs bo t)   = Done bs (fromIntegral bo) t
fromBinaryDecoder (Bin.Fail bs bo err) = Fail bs (fromIntegral bo) (T.pack err)
fromBinaryDecoder (Bin.Partial k)      = Partial (Decoder . pure . fromBinaryDecoder . k)

binarySerialization
    :: forall m t. (Bin.Binary t, Applicative m)
    => Serializable' BinaryP m t
binarySerialization = Serializable packB unpackB
    where
    packB :: t -> m LBS.ByteString
    packB = pure . packBinary . Bin.put

    unpackB :: Decoder m t
    unpackB = Decoder $ pure $ fromBinaryDecoder $ Bin.runGetIncremental Bin.get
