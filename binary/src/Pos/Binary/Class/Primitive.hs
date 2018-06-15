{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}
-- | Useful functions for serialization/deserialization.

module Pos.Binary.Class.Primitive
       ( serialize
       , serializeWith
       , serialize'
       , serializeBuilder
       -- * Deserialize inside the Decoder monad
       , deserialize
       , deserializeWithExtRep
       , deserialize'
       , deserializeWithExtRep'
       -- * Unsafe deserialization
       , unsafeDeserialize
       , unsafeDeserialize'
       , CBOR.Write.toStrictByteString
       , Raw(..)
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       -- * Temporary functions
       , biSize
       -- * Backward-compatible functions
       , decodeFull
       , decodeFull'
       -- * Low-level, fine-grained functions
       , deserializeOrFail
       , deserializeOrFail'
       -- * CBOR in CBOR
       , encodeKnownCborDataItem
       , encodeUnknownCborDataItem
       , decodeKnownCborDataItem
       , decodeUnknownCborDataItem
       -- * Cyclic redundancy check
       , encodeCrcProtected
       , decodeCrcProtected
       -- * Testing utils
       , fillExtRep
       , fillExtRep'
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write
import           Control.Exception.Safe (impureThrow)
import           Control.Monad.ST (ST, runST)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           Data.Digest.CRC32 (CRC32 (..))
import           Data.Typeable (typeOf)
import           Formatting (sformat, shown, (%))
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class.Core (Bi (..), BiExtRep (..), DecoderAttrKind (..), cborError, enforceSize, toCborError)

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a lazy 'BSL.ByteString' and is constructed
-- incrementally.
serialize :: Bi a => a -> BSL.ByteString
-- 1024 is the size of the first buffer, 4096 is the size of subsequent
-- buffers. Chosen because they seem to give good performance. They are not
-- sacred.
serialize = serializeWith 1024 4096

-- | Serialize a Haskell value to an external binary representation.
--
-- The output is represented as a strict 'BS.ByteString'.
serialize' :: Bi a => a -> BS.ByteString
serialize' = BSL.toStrict . serialize

-- | Serialize into a Builder. Useful if you want to throw other ByteStrings
-- around it.
serializeBuilder :: Bi a => a -> Builder
serializeBuilder = CBOR.Write.toBuilder . encode

-- | Serialize using the safe allocation strategy with a given first and
-- subsequent chunk size.
serializeWith :: Bi a => Int -> Int -> a -> BSL.ByteString
serializeWith firstChunk nextChunk = Builder.toLazyByteStringWith strategy mempty . serializeBuilder
  where
    strategy = Builder.safeStrategy firstChunk nextChunk

-- | Deserialize a Haskell value from the external binary representation
-- (which must have been made using 'serialize' or related function).
--
-- /Throws/: @'CBOR.Read.DeserialiseFailure'@ if the given external
-- representation is invalid or does not correspond to a value of the
-- expected type.
unsafeDeserialize :: Bi a => BSL.ByteString -> a
unsafeDeserialize = either impureThrow identity . bimap fst fst . deserializeOrFail decode

-- | Strict variant of 'deserialize'.
unsafeDeserialize' :: Bi a => BS.ByteString -> a
unsafeDeserialize' = unsafeDeserialize . BSL.fromStrict

-- | Run `decodeFull` in the `Decoder` monad, failing (using Decoder.fail) in
-- case the process failed or not the whole input was consumed.
-- We are not generalising this function to any monad as doing so would allow
-- us to cheat, as not all the monads have a sensible `fail` implementation.
-- Expect the whole input to be consumed.
deserialize :: Bi a => BSL.ByteString -> D.Decoder s a
deserialize = toCborError . decodeFull decode label

-- | Deserialize using `BiExtRep` class.
deserializeWithExtRep :: BiExtRep a => BSL.ByteString -> D.Decoder s (a 'AttrExtRep)
deserializeWithExtRep bs
      = toCborError (spliceExtRep (BSL.toStrict bs) <$> decodeFull decodeWithOffsets labelExtRep bs)

-- | Strict version of `deserialize`.
deserialize' :: Bi a => BS.ByteString -> D.Decoder s a
deserialize' = deserialize . BSL.fromStrict

-- | Strict version of `deserializeWithExtRep`.
deserializeWithExtRep' :: BiExtRep a => BS.ByteString -> D.Decoder s (a 'AttrExtRep)
deserializeWithExtRep' = deserializeWithExtRep . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
-- failing if there are leftovers. In a nutshell, the `full` here implies
-- the contract of this function is that what you feed as input needs to
-- be consumed entirely.
decodeFull
    :: forall a
     . (forall s. D.Decoder s a)
    -> (Proxy a -> Text)
    -> BSL.ByteString
    -> Either Text a
decodeFull decoder getLabel bs0 = case deserializeOrFail decoder bs0 of
  Right (x, leftover) -> case BS.null leftover of
      True  -> pure x
      False ->
          let msg = "decodeFull failed for " <> getLabel (Proxy @a) <>
                    "! Leftover found: " <> show leftover
          in Left msg
  Left  (e, _) ->
      Left $ "decodeFull failed for " <> getLabel (Proxy @a) <> ": " <> show e

decodeFull'
    :: (forall s. D.Decoder s a)
    -> (Proxy a -> Text)
    -> BS.ByteString
    -> Either Text a
decodeFull' decoder getLabel = decodeFull decoder getLabel . BSL.fromStrict

-- | Deserialize a Haskell value from the external binary representation,
-- returning either (leftover, value) or a (leftover, @'DeserialiseFailure'@).
deserializeOrFail
    :: forall a
     . (forall s. D.Decoder s a)
    -> BSL.ByteString
    -> Either (CBOR.Read.DeserialiseFailure, BS.ByteString)
              (a, BS.ByteString)
deserializeOrFail decoder bs0 =
    runST (supplyAllInput bs0 =<< deserializeIncremental decoder)
  where
    supplyAllInput bs' (CBOR.Read.Done bs _ x) =
      return (Right (x, bs <> BSL.toStrict bs'))
    supplyAllInput bs (CBOR.Read.Partial k)  =
      case bs of
        BSL.Chunk chunk bs' -> k (Just chunk) >>= supplyAllInput bs'
        BSL.Empty           -> k Nothing      >>= supplyAllInput BSL.Empty
    supplyAllInput _ (CBOR.Read.Fail bs _ exn) = return (Left (exn, bs))

-- | Strict variant of 'deserializeOrFail'.
deserializeOrFail'
    :: (forall s. D.Decoder s a)
    -> BS.ByteString
    -> Either (CBOR.Read.DeserialiseFailure, BS.ByteString)
              (a, BS.ByteString)
deserializeOrFail' decoder = deserializeOrFail decoder . BSL.fromStrict

----------------------------------------


-- | Deserialize a Haskell value from the external binary representation.
--
-- This allows /input/ data to be provided incrementally, rather than all in one
-- go. It also gives an explicit representation of deserialisation errors.
--
-- Note that the incremental behaviour is only for the input data, not the
-- output value: the final deserialized value is constructed and returned as a
-- whole, not incrementally.
deserializeIncremental :: D.Decoder s a -> ST s (CBOR.Read.IDecode s a)
deserializeIncremental = CBOR.Read.deserialiseIncremental

----------------------------------------------------------------------------
-- Raw
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for signalling that a bytestring should be
-- processed as a sequence of bytes, not as a separate entity. It's used in
-- crypto and binary code.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable, NFData)

----------------------------------------------------------------------------
-- Helper functions, types, classes
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' representing a serialized value of
-- type 'a'. This wrapper is used to delay decoding of some data. Note
-- that by default nothing guarantees that the stored 'ByteString' is
-- a valid representation of some value of type 'a'.
newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable, NFData)

-- | A simple helper class simplifying work with 'AsBinary'.
class AsBinaryClass a where
    asBinary :: a -> AsBinary a
    fromBinary :: AsBinary a -> Either Text a

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Byte
biSize = fromIntegral . BSL.length . serialize
{-# INLINE biSize #-}

----------------------------------------------------------------------------
-- CBORDataItem
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
----------------------------------------------------------------------------

-- | Encode and serialise the given `a` and sorrounds it with the semantic tag 24.
-- In CBOR diagnostic notation:
-- >>> 24(h'DEADBEEF')
encodeKnownCborDataItem :: Bi a => a -> E.Encoding
encodeKnownCborDataItem = encodeUnknownCborDataItem . serialize

-- | Like `encodeKnownCborDataItem`, but assumes nothing about the shape of
-- input object, so that it must be passed as a binary `ByteString` blob.
-- It's the caller responsibility to ensure the input `ByteString` correspond
-- indeed to valid, previously-serialised CBOR data.
encodeUnknownCborDataItem :: BSL.ByteString -> E.Encoding
encodeUnknownCborDataItem x = E.encodeTag 24 <> encode x

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- failing if the tag cannot be found.
decodeCborDataItemTag :: D.Decoder s ()
decodeCborDataItemTag = do
    t <- D.decodeTagCanonical
    when (t /= 24) $ cborError $
        "decodeCborDataItem: expected a bytestring with \
        \CBOR (marked by tag 24), found tag: " <> show t

-- | Remove the the semantic tag 24 from the enclosed CBOR data item,
-- decoding back the inner `ByteString` as a proper Haskell type.
-- Consume its input in full.
decodeKnownCborDataItem :: Bi a => D.Decoder s a
decodeKnownCborDataItem = do
    bs <- decodeUnknownCborDataItem
    toCborError $ decodeFull' decode label bs

-- | Like `decodeKnownCborDataItem`, but assumes nothing about the Haskell
-- type we want to deserialise back, therefore it yields the `ByteString`
-- Tag 24 surrounded (stripping such tag away).
-- In CBOR notation, if the data was serialised as:
-- >>> 24(h'DEADBEEF')
-- then `decodeUnknownCborDataItem` yields the inner 'DEADBEEF', unchanged.
decodeUnknownCborDataItem :: D.Decoder s ByteString
decodeUnknownCborDataItem = do
    decodeCborDataItemTag
    D.decodeBytesCanonical

-- | Encodes a type `a` , protecting it from tampering/network-transport-alteration by
-- protecting it with a CRC.
encodeCrcProtected :: Bi a => a -> E.Encoding
encodeCrcProtected x =
    E.encodeListLen 2 <> encodeUnknownCborDataItem body <> encode (crc32 body)
  where
    body = serialize x

-- | Decodes a CBOR blob into a type `a`, checking the serialised CRC corresponds to the computed one.
decodeCrcProtected :: forall s a. Bi a => D.Decoder s a
decodeCrcProtected = do
    enforceSize ("decodeCrcProtected: " <> show (typeOf (Proxy @a))) 2
    body <- decodeUnknownCborDataItem
    expectedCrc  <- decode
    let actualCrc :: Word32
        actualCrc = crc32 body
    let crcErrorFmt = "decodeCrcProtected, expected CRC " % shown % " was not the computed one, which was " % shown
    when (actualCrc /= expectedCrc) $ cborError (sformat crcErrorFmt expectedCrc actualCrc)
    toCborError $ decodeFull' decode label body

-- | Useful function for tests, but expensive since it serializes and
-- deserializes the object with external representation.
fillExtRep
    :: forall a. (Bi (a 'AttrNone), BiExtRep a)
    => a 'AttrNone
    -> Either Text (a 'AttrExtRep)
fillExtRep = fmap fst . fillExtRep'

-- | Like `fillExtRep` but also return serialized value.
fillExtRep'
    :: forall a. (Bi (a 'AttrNone), BiExtRep a)
    => a 'AttrNone
    -> Either Text (a 'AttrExtRep, ByteString)
fillExtRep' a =
    let bs = serialize' a
    in (,bs) . spliceExtRep bs <$> decodeFull' decodeWithOffsets labelExtRep bs
