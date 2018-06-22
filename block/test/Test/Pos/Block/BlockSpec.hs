{-# LANGUAGE DataKinds #-}
module Test.Pos.Block.BlockSpec
    ( spec )
    where

import           Universum

import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, again, (===), (.&.))

import           Pos.Binary.Class (Bi, BiExtRep (..), DecoderAttrKind (..), DecoderAttr (..), EitherExtRep (..), fillExtRep, fillExtRep', serialize')
import           Pos.Core.Block (Blockchain (..), Block, BlockHeader, GenericBlock (..), GenericBlockHeader (..),
                                 MainBlockchain, GenesisBlockchain, headerHash)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMinTTL (..), VssMaxTTL (..))
import           Pos.Core.Configuration (withGenesisHash, withProtocolConstants) 
import           Pos.Crypto.Configuration (ProtocolMagic (..), withProtocolMagic)
import           Pos.Crypto.Hashing (unsafeHash)
import           Pos.Arbitrary.Block ()

-- This property is implied by `blockHeaderExtRepProp`.
blockHeaderHeaderHashProp :: BlockHeader 'AttrNone -> Property
blockHeaderHeaderHashProp bh = 
    let bhExtRep = fillExtRep bh
    in case bhExtRep of
        Left  _   -> error "fillExtRep failed"
        Right bh' -> headerHash bh' === headerHash bh

-- |
-- External representation of `GenericBlockHeader b 'AttrExtRep` should be equal to
-- serialized value of `BlockHeader 'AttrNone`.
genericBlockHeaderExtRepProp
    :: forall b attr .
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    =>  GenericBlockHeader b attr
    -> Property
genericBlockHeaderExtRepProp gbh =
    let gbhExtRep :: Either Text (GenericBlockHeader b 'AttrExtRep, ByteString)
        gbhExtRep = case _gbhDecoderAttr gbh of
            DecoderAttrNone         -> fillExtRep' gbh
            DecoderAttrOffsets _ _  -> fillExtRep' $ forgetExtRep gbh
            DecoderAttrExtRep _     -> Right (gbh, serialize' $ forgetExtRep gbh)
    in case gbhExtRep of
        Left  err       ->
            error $ "fillExtRep failed: " <> err
        Right (gbh', bs) ->
            bs === case _gbhDecoderAttr gbh' of
                DecoderAttrExtRep bs' -> bs'

blockFillExtRep' :: Block 'AttrNone -> Either Text (Block 'AttrExtRep, ByteString)
blockFillExtRep' = fmap (first runEitherExtRep) . fillExtRep' . EitherExtRep

-- |
-- Check `genericBlockHeaderExtRepProp` property for a header of a generic block.
genericBlockHeaderInBlockProp
    :: forall b .
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (Body b)
       , Bi (ExtraBodyData b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    => GenericBlock b 'AttrNone
    -> Property
genericBlockHeaderInBlockProp gb =
    let gbExtRep = fillExtRep gb
    in case gbExtRep of
        Left _        ->
            error "fillExtRep failed"
        Right gb' ->
            genericBlockHeaderExtRepProp $ _gbHeader gb'

-- |
-- Header hashe of `Block attr` should be indepenend of `attr`.
blockHeaderHashProp :: Block 'AttrNone -> Property
blockHeaderHashProp b =
    let bExtRep = blockFillExtRep' b
    in case bExtRep of 
        Left _         ->
            error "fillExtRep failed"
        Right (b', _) ->
            headerHash b === headerHash b'

-- |
-- External representation of `Block 'AttrExtRep` should be equal to serialized
-- value of `Block 'AttrNone`.
genericBlockExtRepProp
    :: forall b . 
       ( Typeable b
       , Bi (BHeaderHash b)
       , Bi (Body b)
       , Bi (ExtraBodyData b)
       , Bi (BodyProof b)
       , Bi (ConsensusData b)
       , Bi (ExtraHeaderData b)
       )
    => GenericBlock b 'AttrNone
    -> Property
genericBlockExtRepProp gb =
    let gbExtRep = fillExtRep' gb
    in case gbExtRep of
        Left  _        ->
            error "fillExtRep failed"
        Right (gb', bs) ->
            bs === case _gbDecoderAttr gb' of
                DecoderAttrExtRep bs' -> bs'

spec :: Spec
spec = withProtocolMagic pm $ withProtocolConstants pc $ withGenesisHash (unsafeHash @Text "genesis") $ do
    describe "BlockHeader properties" $ do
        describe "BiExtRep properties" $ do
            prop blockHeaderHeaderHashDesc $ again blockHeaderHeaderHashProp
            prop genericBlockHeaderExtRepDesc $ again
                  $ genericBlockHeaderExtRepProp @GenesisBlockchain @'AttrNone
                .&. genericBlockHeaderExtRepProp @MainBlockchain @'AttrNone
    describe "Block properties" $ do
        describe "BiExtRep properties" $ do
            prop blockHeaderHashDesc $ again blockHeaderHashProp
            prop genericBlockExtRepDesc $ again
                  $ genericBlockExtRepProp @GenesisBlockchain
                .&. genericBlockExtRepProp @MainBlockchain
            prop genericBlockHeaderInBlockDesc $ again
                  $ genericBlockHeaderInBlockProp @GenesisBlockchain
                .&. genericBlockHeaderInBlockProp @MainBlockchain
    where
        pc = ProtocolConstants
            { pcK = 2160
            , pcVssMinTTL = VssMinTTL 2
            , pcVssMaxTTL = VssMaxTTL 6
            }
        pm = ProtocolMagic 1
        blockHeaderHeaderHashDesc = "headerHash of `BlockHeader 'AttrNone` and `BlockHeader 'ExtRep` should be equal"
        genericBlockHeaderExtRepDesc = "external representation of `BlockHeader 'AttrExtRep` should be equal to serialized value of `BlockHeader 'AttrNone`"
        blockHeaderHashDesc = "headerHash of `Block 'AttrNone` and `Block 'AttrExtRep` should be equal"
        genericBlockExtRepDesc = "external representation of `GenericBlock b 'AttrExtRep` should be equal to serialized value of `GenericBlock b 'AttrNone`"
        genericBlockHeaderInBlockDesc = "external representation of `BlockHeader` from a `Block` should be equal to serialized to its serialized value"
