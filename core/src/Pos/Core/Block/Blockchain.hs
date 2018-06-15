{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}

-- | This module contains some general definitions related to blocks
-- and headers. The heart of this module is 'Blockchain' type class.

module Pos.Core.Block.Blockchain
       ( Blockchain (..)
       , GenericBlockHeader (..)
       , GenericBlock (..)

       -- * Smart constructors
       , mkGenericHeader
       , mkGenericHeader'
       , mkGenericBlock
       , mkGenericBlock'

       -- * Lenses
       -- ** Header
       , gbhProtocolMagic
       , gbhPrevBlock
       , gbhBodyProof
       , gbhConsensus
       , gbhExtra
       , gbhDecoderAttr

       -- ** Block
       , gbBody
       , gbHeader
       , gbExtra
       , gbPrevBlock
       , gbBodyProof
       , gbConsensus
       , gbDecoderAttr
       , gbHeaderDecoderAttr
       ) where

import           Universum

import           Codec.CBOR.Decoding (peekByteOffset)
import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (throwError))
import           Formatting (build, sformat, (%))

import           Pos.Binary.Class (Bi (..), BiExtRep (..), Decoder,
                                   DecoderAttr (..), DecoderAttrKind (..),
                                   encodeListLen, enforceSize, spliceExtRep')
import           Pos.Crypto (ProtocolMagic (..))

----------------------------------------------------------------------------
-- Blockchain class
----------------------------------------------------------------------------

-- | Blockchain type class generalizes some functionality common for
-- different blockchains.
class Blockchain p (attr :: DecoderAttrKind) where
    -- | Proof of data stored in the body. Ensures immutability.
    type BodyProof p :: *
    -- | Consensus data which can be used to check consensus properties.
    type ConsensusData p :: *
    -- | Whatever extra data.
    type ExtraHeaderData p :: *
    type ExtraHeaderData p = ()
    -- | Block header used in this blockchain.
    type BBlockHeader p attr :: *
    type BBlockHeader p attr = GenericBlockHeader p attr
    -- | Hash of 'BBlockHeader'. This is something like @Hash (BBlockHeader p)@.
    type BHeaderHash p :: *

    -- | Body contains payload and other heavy data.
    type Body p :: *
    -- | Whatever extra data.
    type ExtraBodyData p :: *
    type ExtraBodyData p = ()
    -- | Block used in this blockchain.
    type BBlock p attr :: *
    type BBlock p attr = GenericBlock p attr

    mkBodyProof :: Body p -> BodyProof p

    -- | Check whether 'BodyProof' corresponds to 'Body.
    checkBodyProof :: MonadError Text m => Body p -> BodyProof p -> m ()
    default checkBodyProof ::
        (MonadError Text m, Buildable (BodyProof p), Eq (BodyProof p)) =>
        Body p -> BodyProof p -> m ()
    checkBodyProof body proof = do
        let calculatedProof = mkBodyProof @p @attr body
        let errMsg =
                sformat ("Incorrect proof of body. "%
                         "Proof in header: "%build%
                         ", calculated proof: "%build)
                proof calculatedProof
        unless (calculatedProof == proof) $ throwError errMsg

----------------------------------------------------------------------------
-- Generic types
----------------------------------------------------------------------------

-- | Header of block contains some kind of summary. There are various
-- benefits which people get by separating header from other data.
--
-- The constructor has `Unsafe' prefix in its name, because there in
-- general there may be some invariants which must hold for the
-- contents of header.
data GenericBlockHeader b attr = UnsafeGenericBlockHeader
    { _gbhProtocolMagic :: !ProtocolMagic
      -- | Pointer to the header of the previous block.
    , _gbhPrevBlock     :: !(BHeaderHash b)
    , -- | Proof of body.
      _gbhBodyProof     :: !(BodyProof b)
    , -- | Consensus data to verify consensus algorithm.
      _gbhConsensus     :: !(ConsensusData b)
    , -- | Any extra data.
      _gbhExtra         :: !(ExtraHeaderData b)
    , -- | Decodec attributes.
      _gbhDecoderAttr   :: !(DecoderAttr attr)
    } deriving (Generic)

deriving instance
    ( Show (BHeaderHash b)
    , Show (BodyProof b)
    , Show (ConsensusData b)
    , Show (ExtraHeaderData b)
    ) => Show (GenericBlockHeader b attr)

deriving instance
    ( Eq (BHeaderHash b)
    , Eq (BodyProof b)
    , Eq (ConsensusData b)
    , Eq (ExtraHeaderData b)
    ) => Eq (GenericBlockHeader b attr)

instance ( Typeable b
         , Bi (BHeaderHash b)
         , Bi (BodyProof b)
         , Bi (ConsensusData b)
         , Bi (ExtraHeaderData b)
         ) =>
         Bi (GenericBlockHeader b 'AttrNone) where
    encode bh =  encodeListLen 5
              <> encode (getProtocolMagic (_gbhProtocolMagic bh))
              <> encode (_gbhPrevBlock bh)
              <> encode (_gbhBodyProof bh)
              <> encode (_gbhConsensus bh)
              <> encode (_gbhExtra bh)
    decode = do
        enforceSize "GenericBlockHeader b" 5
        _gbhProtocolMagic <- ProtocolMagic <$> decode
        _gbhPrevBlock <- decode
        _gbhBodyProof <- decode
        _gbhConsensus <- decode
        _gbhExtra     <- decode
        let _gbhDecoderAttr = DecoderAttrNone
        pure UnsafeGenericBlockHeader {..}

instance ( Typeable b
         , Bi (BHeaderHash b)
         , Bi (BodyProof b)
         , Bi (ConsensusData b)
         , Bi (ExtraHeaderData b)
         ) => BiExtRep (GenericBlockHeader b) where
    spliceExtRep bs h = h
        { _gbhDecoderAttr = (spliceExtRep' bs $ _gbhDecoderAttr h) }
    forgetExtRep h = h { _gbhDecoderAttr = DecoderAttrNone }

    decodeWithOffsets :: forall s. Decoder s (GenericBlockHeader b 'AttrOffsets)
    decodeWithOffsets = do
        start <- peekByteOffset
        bh <- decode @(GenericBlockHeader b 'AttrNone)
        end <- peekByteOffset
        return $ bh { _gbhDecoderAttr = (DecoderAttrOffsets start end) }

instance
    ( NFData (BHeaderHash b)
    , NFData (BodyProof b)
    , NFData (ConsensusData b)
    , NFData (ExtraHeaderData b)
    ) => NFData (GenericBlockHeader b attr)

-- | In general Block consists of header and body. It may contain
-- extra data as well.
--
-- The constructor has `Unsafe' prefix in its name, because there are
-- some invariants which must hold for the contents of block. For
-- instance, for generic block proof of body must correspond to the
-- body itself. Also there may be other invariants specific for
-- particular blockchains.
data GenericBlock b attr = UnsafeGenericBlock
    { _gbHeader       :: !(GenericBlockHeader b attr)
    , _gbBody         :: !(Body b)
    , _gbExtra        :: !(ExtraBodyData b)
    , _gbDecoderAttr  :: !(DecoderAttr attr)
    } deriving (Generic)

deriving instance
    ( Show (GenericBlockHeader b attr)
    , Show (Body b)
    , Show (ExtraBodyData b)
    ) => Show (GenericBlock b attr)

deriving instance
    ( Eq (BHeaderHash b)
    , Eq (Body b)
    , Eq (BodyProof b)
    , Eq (ConsensusData b)
    , Eq (ExtraBodyData b)
    , Eq (ExtraHeaderData b)
    ) => Eq (GenericBlock b attr)

instance ( Typeable b
         , Bi (BHeaderHash b)
         , Bi (Body b)
         , Bi (BodyProof b)
         , Bi (ConsensusData b)
         , Bi (ExtraBodyData b)
         , Bi (ExtraHeaderData b)
         ) => Bi (GenericBlock b 'AttrNone) where
    encode gb =  encodeListLen 3
              <> encode (_gbHeader gb)
              <> encode (_gbBody gb)
              <> encode (_gbExtra gb)
    decode = do
        enforceSize "GenericBlock" 3
        _gbHeader <- decode
        _gbBody   <- decode
        _gbExtra  <- decode
        let _gbDecoderAttr = DecoderAttrNone
        pure UnsafeGenericBlock {..}

instance ( Typeable b
         , Bi (BHeaderHash b)
         , Bi (BodyProof b)
         , Bi (ConsensusData b)
         , Bi (ExtraHeaderData b)
         , Bi (Body b)
         , Bi (ExtraBodyData b)
         ) =>
         BiExtRep (GenericBlock b) where
    spliceExtRep bs (UnsafeGenericBlock {..}) =
        UnsafeGenericBlock
            (spliceExtRep bs $ _gbHeader)
            (_gbBody)
            (_gbExtra)
            (spliceExtRep' bs _gbDecoderAttr)
    forgetExtRep (UnsafeGenericBlock {..})
        = UnsafeGenericBlock
            (forgetExtRep _gbHeader)
            _gbBody
            _gbExtra
            DecoderAttrNone

    decodeWithOffsets = do
        start <- peekByteOffset
        enforceSize "GenericBlock" 3
        _gbHeader <- decodeWithOffsets
        _gbBody   <- decode
        _gbExtra  <- decode
        end <- peekByteOffset
        -- Subtract two bytes (check out `Either` instance).
        let _gbDecoderAttr = DecoderAttrOffsets start end
        return $ UnsafeGenericBlock {..}

-- Derived partially in Instances
--instance
--    ( NFData (GenericBlockHeader b)
--    , NFData (Body b)
--    , NFData (ExtraBodyData b)
--    ) => NFData (GenericBlock b)

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'GenericBlockHeader'.
-- "Smart" because it makes the body proof for you and then runs your
-- consensus function.
mkGenericHeader
    :: forall attr b .
       ( Blockchain b attr )
    => ProtocolMagic
    -> BHeaderHash b
    -> Body b
    -> (BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> DecoderAttr attr
    -> GenericBlockHeader b attr
mkGenericHeader pm hashPrev body consensus extra decAttr =
    UnsafeGenericBlockHeader pm hashPrev proof (consensus proof) extra decAttr
  where
    proof = mkBodyProof @b @attr body

mkGenericHeader'
    :: forall b .
       ( Blockchain b 'AttrNone )
    => ProtocolMagic
    -> BHeaderHash b
    -> Body b
    -> (BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> GenericBlockHeader b 'AttrNone
mkGenericHeader' pm hashPrev body consensus extra =
    mkGenericHeader pm hashPrev body consensus extra DecoderAttrNone

-- | Smart constructor for 'GenericBlock'.
-- "Smart" because it uses the 'mkGenericHeader' "smart" constructor.
mkGenericBlock
    :: forall attr b .
       ( Blockchain b attr )
    => ProtocolMagic
    -> BHeaderHash b
    -> Body b
    -> (BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> DecoderAttr attr -- ^ header decoder's attributes
    -> DecoderAttr attr -- ^ block decoder's attributes
    -> GenericBlock b attr
mkGenericBlock pm hashPrev body consensus extraH extra decAttrH decAttr =
    UnsafeGenericBlock header body extra decAttr
  where
    header = mkGenericHeader pm hashPrev body consensus extraH decAttrH

mkGenericBlock'
    :: forall b .
       ( Blockchain b 'AttrNone )
    => ProtocolMagic
    -> BHeaderHash b
    -> Body b
    -> (BodyProof b -> ConsensusData b)
    -> ExtraHeaderData b
    -> ExtraBodyData b
    -> GenericBlock b 'AttrNone
mkGenericBlock' pm hashPrev body consensus extraH extra =
    mkGenericBlock pm hashPrev body consensus extraH extra DecoderAttrNone DecoderAttrNone

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

makeLenses ''GenericBlockHeader
makeLenses ''GenericBlock

-- | Lens from 'GenericBlock' to 'BHeaderHash' of its parent.
gbPrevBlock :: Lens' (GenericBlock b attr) (BHeaderHash b)
gbPrevBlock = gbHeader . gbhPrevBlock

-- | Lens from 'GenericBlock' to 'BodyProof'.
gbBodyProof :: Lens' (GenericBlock b attr) (BodyProof b)
gbBodyProof = gbHeader . gbhBodyProof

-- | Lens from 'GenericBlock' to 'ConsensusData'.
gbConsensus :: Lens' (GenericBlock b attr) (ConsensusData b)
gbConsensus = gbHeader . gbhConsensus

-- | Lens from 'GenericBlock' to header's 'DecoderAttr'.
gbHeaderDecoderAttr :: Lens' (GenericBlock b attr) (DecoderAttr attr)
gbHeaderDecoderAttr = gbHeader . gbhDecoderAttr
