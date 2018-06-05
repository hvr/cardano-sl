{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the main blockchain of course.

module Pos.Core.Block.Main.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, stext, (%))
import           Serokell.Util (Color (Magenta), colorize, listJson)

import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Block.Main.Lens (mainBlockBlockVersion, mainBlockDifficulty,
                                           mainBlockSlot, mainBlockSoftwareVersion,
                                           mainHeaderBlockVersion, mainHeaderDifficulty,
                                           mainHeaderLeaderKey, mainHeaderSlot,
                                           mainHeaderSoftwareVersion, mbTxs, mcdDifficulty,
                                           mehBlockVersion, mehSoftwareVersion)
import           Pos.Core.Block.Main.Types (MainBody (..), MainExtraHeaderData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..), HeaderHash, MainBlock,
                                             MainBlockHeader, MainConsensusData (..),
                                             blockHeaderHash, headerHashHexF)
import           Pos.Core.Class (HasBlockVersion (..), HasDifficulty (..), HasEpochIndex (..),
                                 HasEpochOrSlot (..), HasHeaderHash (..), HasSoftwareVersion (..),
                                 IsHeader, IsMainHeader (..))
import           Pos.Core.Slotting.Types (EpochOrSlot (..), slotIdF)

instance NFData (MainBlock attr)

instance Buildable (MainBlockHeader attr) where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("MainBlockHeader:\n"%
             "    hash: "%headerHashHexF%"\n"%
             "    previous block: "%headerHashHexF%"\n"%
             "    slot: "%slotIdF%"\n"%
             "    difficulty: "%int%"\n"%
             "    leader: "%build%"\n"%
             "    signature: "%build%"\n"%
             build
            )
            gbhHeaderHash
            _gbhPrevBlock
            _mcdSlot
            _mcdDifficulty
            _mcdLeaderKey
            _mcdSignature
            _gbhExtra
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderMain gbh
        MainConsensusData {..} = _gbhConsensus

instance Buildable (MainBlock attr) where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             "  transactions ("%int%" items): "%listJson%"\n"%
             "  "%build%"\n"%
             "  "%build%"\n"%
             "  update payload: "%build%"\n"%
             "  "%build
            )
            (colorize Magenta "MainBlock")
            _gbHeader
            (length txs)
            txs
            _mbDlgPayload
            _mbSscPayload
            _mbUpdatePayload
            _gbExtra
      where
        MainBody {..} = _gbBody
        txs = _gbBody ^. mbTxs

instance HasEpochIndex (MainBlock attr) where
    epochIndexL = mainBlockSlot . epochIndexL

instance HasEpochIndex (MainBlockHeader attr) where
    epochIndexL = mainHeaderSlot . epochIndexL

instance HasEpochOrSlot (MainBlockHeader attr) where
    getEpochOrSlot = EpochOrSlot . Right . view mainHeaderSlot

instance HasEpochOrSlot (MainBlock attr) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

instance HasHeaderHash (MainBlockHeader attr) where
    headerHash = blockHeaderHash . BlockHeaderMain

instance HasHeaderHash (MainBlock attr) where
    headerHash = blockHeaderHash . BlockHeaderMain . _gbHeader

instance HasDifficulty MainConsensusData where
    difficultyL = mcdDifficulty

instance HasDifficulty (MainBlockHeader attr) where
    difficultyL = mainHeaderDifficulty

instance HasDifficulty (MainBlock attr) where
    difficultyL = mainBlockDifficulty

instance HasBlockVersion MainExtraHeaderData where
    blockVersionL = mehBlockVersion

instance HasSoftwareVersion MainExtraHeaderData where
    softwareVersionL = mehSoftwareVersion

instance HasBlockVersion (MainBlock attr) where
    blockVersionL = mainBlockBlockVersion

instance HasSoftwareVersion (MainBlock attr) where
    softwareVersionL = mainBlockSoftwareVersion

instance HasBlockVersion (MainBlockHeader attr) where
    blockVersionL = mainHeaderBlockVersion

instance HasSoftwareVersion (MainBlockHeader attr) where
    softwareVersionL = mainHeaderSoftwareVersion

instance IsHeader (MainBlockHeader attr)

instance IsMainHeader (MainBlockHeader attr) where
    headerSlotL = mainHeaderSlot
    headerLeaderKeyL = mainHeaderLeaderKey
