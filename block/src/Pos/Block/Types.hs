-- | Types used for block processing: most importantly, 'Undo' and 'Blund'.

module Pos.Block.Types
       ( SlogUndo (..)
       , Undo (..)
       , Blund

       , LastKnownHeader
       , LastKnownHeaderTag
       , MonadLastKnownHeader

       , RecoveryHeaderTag
       , RecoveryHeader
       , MonadRecoveryHeader
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Block.Slog.Types (SlogUndo (..))
import           Pos.Communication.Protocol (NodeId)
import           Pos.Core (HasConfiguration, HasDifficulty (..), HasHeaderHash (..))
import           Pos.Core.Block (Block, BlockHeader)
import           Pos.Core.Txp (TxpUndo)
import           Pos.Delegation.Types (DlgUndo)
import           Pos.Update.Poll.Types (USUndo)
import           Pos.Util.Util (HasLens (..))

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoDlg  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Eq, Show, Generic)

instance NFData Undo

-- | Block and its Undo.
type Blund attr = (Block attr, Undo)

instance HasConfiguration => Buildable Undo where
    build Undo{..} =
        bprint ("Undo:\n"%
                "  undoTx: "%listJson%"\n"%
                "  undoDlg: "%build%"\n"%
                "  undoUS: "%build%"\n"%
                "  undoSlog: "%build)
               (map (bprint listJson) undoTx) undoDlg undoUS undoSlog

instance HasDifficulty (Blund attr) where
    difficultyL = _1 . difficultyL

instance HasHeaderHash (Block attr) => HasHeaderHash (Blund attr) where
    headerHash = headerHash . fst

-- | For a description of what these types mean,
-- please refer to @NodeContext@ in @Pos.Context.Context@.
data LastKnownHeaderTag
type LastKnownHeader attr = TVar (Maybe (BlockHeader attr))
type MonadLastKnownHeader attr ctx m
     = (MonadReader ctx m, HasLens LastKnownHeaderTag ctx (LastKnownHeader attr))

data RecoveryHeaderTag
type RecoveryHeader attr = STM.TMVar (NodeId, BlockHeader attr)
type MonadRecoveryHeader attr ctx m
     = (MonadReader ctx m, HasLens RecoveryHeaderTag ctx (RecoveryHeader attr))
