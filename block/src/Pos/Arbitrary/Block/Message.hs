{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Arbitrary.Block.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Ssc (SscPayloadDependsOnSlot (..))
import           Pos.Arbitrary.Update ()
import qualified Pos.Block.Network.Types as T
import           Pos.Binary.Class (DecoderAttrKind (..))
import           Pos.Core (HasGenesisHash, HasProtocolConstants, HasProtocolMagic)
import           Pos.Core.Ssc (SscPayload)

import           Test.Pos.Txp.Arbitrary ()
import           Test.Pos.Util.Chrono ()

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( HasProtocolConstants
         , HasProtocolMagic
         ) =>
         Arbitrary (T.MsgHeaders 'AttrNone) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscPayloadDependsOnSlot
         , HasProtocolConstants
         , HasProtocolMagic
         , HasGenesisHash
         ) =>
         Arbitrary (T.MsgBlock 'AttrNone) where
    arbitrary = genericArbitrary
    shrink = genericShrink
