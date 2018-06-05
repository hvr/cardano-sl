{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- the Getter instances from lens cause a redundant Functor

-- | United miscellaneous functionality.

module Pos.Core.Block.Union.Instances
       ( getBlockHeader
       , blockHeader
       , ComponentBlock (..)
       ) where

import           Universum

import           Control.Lens (Getter, choosing, lens, to)
import qualified Data.Text.Buildable as Buildable

import           Pos.Core.Block.Blockchain (GenericBlock (..))
import           Pos.Core.Block.Genesis ()
import           Pos.Core.Block.Main ()
import           Pos.Core.Block.Union.Types (Block, BlockHeader (..) ,blockHeaderHash,
                                             choosingBlockHeader)
import           Pos.Core.Class (HasDifficulty (..), HasEpochIndex (..), HasEpochOrSlot (..),
                                 HasHeaderHash (..), HasPrevBlock (..), IsGenesisHeader, IsHeader,
                                 IsMainHeader (..))
import           Pos.Core.Slotting.Types (EpochOrSlot (..))
import           Pos.Util.Some (Some)

----------------------------------------------------------------------------
-- Buildable
----------------------------------------------------------------------------

instance Buildable (BlockHeader attr) where
    build = \case
        BlockHeaderGenesis bhg -> Buildable.build bhg
        BlockHeaderMain    bhm -> Buildable.build bhm

----------------------------------------------------------------------------
-- HasHeaderHash
----------------------------------------------------------------------------

instance  HasHeaderHash (BlockHeader attr) where
    headerHash = blockHeaderHash

instance HasHeaderHash (Block attr) where
    headerHash = blockHeaderHash . getBlockHeader

-- | Take 'BlockHeader' from either 'GenesisBlock' or 'MainBlock'.
getBlockHeader :: Block attr -> BlockHeader attr
getBlockHeader = \case
    Left  gb -> BlockHeaderGenesis (_gbHeader gb)
    Right mb -> BlockHeaderMain    (_gbHeader mb)

blockHeader :: Getter (Block attr) (BlockHeader attr)
blockHeader = to getBlockHeader

-- | Representation of 'Block' passed to a component.
data ComponentBlock payload =
    ComponentBlockGenesis (Some IsGenesisHeader)
    | ComponentBlockMain
       { bcmHeader  :: !(Some IsMainHeader)
       , bcmPayload :: !payload }

instance HasHeaderHash (ComponentBlock a) where
    headerHash (ComponentBlockGenesis genesisHeader) = headerHash genesisHeader
    headerHash (ComponentBlockMain mainHeader _)     = headerHash mainHeader

----------------------------------------------------------------------------
-- HasDifficulty
----------------------------------------------------------------------------

instance HasDifficulty (BlockHeader attr) where
    difficultyL = choosingBlockHeader difficultyL difficultyL

instance HasDifficulty (Block attr) where
    difficultyL = choosing difficultyL difficultyL

-----------------------------------------------------------------------------
--- HasEpochIndex
-----------------------------------------------------------------------------

instance HasEpochIndex (BlockHeader attr) where
    epochIndexL = choosingBlockHeader epochIndexL epochIndexL

----------------------------------------------------------------------------
-- IsHeader
----------------------------------------------------------------------------

instance IsHeader (BlockHeader attr)

----------------------------------------------------------------------------
-- HasPrevBlock
----------------------------------------------------------------------------

instance HasPrevBlock (ComponentBlock a) where
    prevBlockL = lens getter setter
      where
        getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. prevBlockL
        getter (ComponentBlockMain mainHeader _)     = mainHeader ^. prevBlockL
        setter (ComponentBlockGenesis genesisHeader) e =
            ComponentBlockGenesis (genesisHeader & prevBlockL .~ e)
        setter (ComponentBlockMain mainHeader payload) e =
            ComponentBlockMain (mainHeader & prevBlockL .~ e) payload

----------------------------------------------------------------------------
-- HasEpochIndex
----------------------------------------------------------------------------

instance HasEpochIndex (ComponentBlock a) where
    epochIndexL = lens getter setter
        where
            getter (ComponentBlockGenesis genesisHeader) = genesisHeader ^. epochIndexL
            getter (ComponentBlockMain mainHeader _)     = mainHeader ^. epochIndexL
            setter (ComponentBlockGenesis genesisHeader) e =
                ComponentBlockGenesis (genesisHeader & epochIndexL .~ e)
            setter (ComponentBlockMain mainHeader payload) e =
                ComponentBlockMain (mainHeader & epochIndexL .~ e) payload

----------------------------------------------------------------------------
-- HasEpochOrSlot
----------------------------------------------------------------------------

instance HasEpochOrSlot (BlockHeader attr) where
    getEpochOrSlot = view (choosingBlockHeader (to getEpochOrSlot) (to getEpochOrSlot))

instance HasEpochOrSlot (ComponentBlock a) where
    getEpochOrSlot (ComponentBlockMain a _)  = EpochOrSlot $ Right $ a ^. headerSlotL
    getEpochOrSlot (ComponentBlockGenesis a) = EpochOrSlot $ Left $ a ^. epochIndexL
