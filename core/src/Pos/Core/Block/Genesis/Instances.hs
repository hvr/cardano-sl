{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Miscellaneous instances, etc. Related to the genesis blockchain of course.

module Pos.Core.Block.Genesis.Instances
       (
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, int, sformat, stext, (%))
import           Serokell.Util (Color (Magenta), colorize)

import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..), gbHeader,
                                            gbhConsensus)
import           Pos.Core.Block.Genesis.Lens (gcdDifficulty, gcdEpoch)
import           Pos.Core.Block.Genesis.Types (GenesisBody (..), GenesisConsensusData (..))
import           Pos.Core.Block.Union.Types (BlockHeader (..), GenesisBlock, GenesisBlockHeader,
                                             HeaderHash, blockHeaderHash, headerHashHexF)
import           Pos.Core.Class (HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                                 HasHeaderHash (..), IsGenesisHeader, IsHeader)
import           Pos.Core.Common (slotLeadersF)
import           Pos.Core.Slotting.Types (EpochOrSlot (..))

instance NFData (GenesisBlock attr)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Buildable (GenesisBlockHeader attr) where
    build gbh@UnsafeGenericBlockHeader {..} =
        bprint
            ("GenesisBlockHeader:\n"%
             "    hash: "%headerHashHexF%"\n"%
             "    previous block: "%headerHashHexF%"\n"%
             "    epoch: "%build%"\n"%
             "    difficulty: "%int%"\n"
            )
            gbhHeaderHash
            _gbhPrevBlock
            _gcdEpoch
            _gcdDifficulty
      where
        gbhHeaderHash :: HeaderHash
        gbhHeaderHash = blockHeaderHash $ BlockHeaderGenesis gbh
        GenesisConsensusData {..} = _gbhConsensus

instance Buildable (GenesisBlock attr) where
    build UnsafeGenericBlock {..} =
        bprint
            (stext%":\n"%
             "  "%build%
             stext
            )
            (colorize Magenta "GenesisBlock")
            _gbHeader
            formatLeaders
      where
        GenesisBody {..} = _gbBody
        formatIfNotNull formatter l = if null l then mempty else sformat formatter l
        formatLeaders = formatIfNotNull
            ("  leaders: "%slotLeadersF%"\n") (toList _gbLeaders)

----------------------------------------------------------------------------
-- Pos.Core.Class
----------------------------------------------------------------------------

instance HasEpochIndex (GenesisBlock attr) where
    epochIndexL = gbHeader . gbhConsensus . gcdEpoch

instance HasEpochIndex (GenesisBlockHeader attr) where
    epochIndexL = gbhConsensus . gcdEpoch

instance HasEpochOrSlot (GenesisBlockHeader attr) where
    getEpochOrSlot = EpochOrSlot . Left . _gcdEpoch . _gbhConsensus

instance HasEpochOrSlot (GenesisBlock attr) where
    getEpochOrSlot = getEpochOrSlot . _gbHeader

-- NB. it's not a mistake that these instances require @Bi BlockHeader@
-- instead of @Bi GenesisBlockHeader@. We compute header's hash by
-- converting it to a BlockHeader first.

instance HasHeaderHash (GenesisBlockHeader attr) where
    headerHash = blockHeaderHash . BlockHeaderGenesis

instance HasHeaderHash (GenesisBlock attr) where
    headerHash = blockHeaderHash . BlockHeaderGenesis . _gbHeader

instance HasDifficulty GenesisConsensusData where
    difficultyL = gcdDifficulty

instance HasDifficulty (GenesisBlockHeader attr) where
    difficultyL = gbhConsensus . difficultyL

instance HasDifficulty (GenesisBlock attr) where
    difficultyL = gbHeader . difficultyL

instance IsHeader (GenesisBlockHeader attr)
instance IsGenesisHeader (GenesisBlockHeader attr)
