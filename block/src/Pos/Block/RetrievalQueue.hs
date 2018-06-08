-- | Block retrieval queue with accompanying datatypes.
module Pos.Block.RetrievalQueue
       ( BlockRetrievalQueueTag
       , BlockRetrievalQueue
       , BlockRetrievalTask(..)
       ) where

import           Universum

import           Control.Concurrent.STM (TBQueue)

import           Pos.Binary.Class (DecoderAttrKind (AttrExtRep))
import           Pos.Core.Block (BlockHeader)
import           Pos.Network.Types (NodeId)

-- | Task that is put in the block retrieval queue for the retrieval
-- worker to perform.
data BlockRetrievalTask = BlockRetrievalTask
    { brtHeader    :: !(BlockHeader 'AttrExtRep)
      -- ^ Header we're insterested in together with external, i.e.
      -- @'ByteString'@ representations.  It is required since block hash
      -- verification is postponed untill `BlockRetrievalTask` is handled.
    , brtContinues :: !Bool
      -- ^ If it was tentatively classified as "direct continuation of
      -- our chain".
    }

data BlockRetrievalQueueTag

-- | Queue types.
type BlockRetrievalQueue = TBQueue (NodeId, BlockRetrievalTask)
