-- | Block constructors and basic functions.

module Pos.Block.Base
       ( mkMainBlockExplicit
       , mkMainBlockExplicit'
       , mkMainBlock
       , mkMainBlock'
       , mkMainHeaderExplicit
       , mkMainHeaderExplicit'
       , mkMainHeader
       , mkMainHeader'
       , emptyMainBody

       , mkGenesisHeader
       , mkGenesisHeader'
       , mkGenesisBlock
       , mkGenesisBlock'
       , genesisBlock0
       , genesisBlock0'
       ) where

import           Universum

import           Data.Default (Default (def))

import           Pos.Binary.Class (DecoderAttrKind (AttrNone), DecoderAttr (..))
import           Pos.Block.BHelpers ()
import           Pos.Core (BlockVersion, ChainDifficulty, EpochIndex, GenesisHash (..),
                           HasDifficulty (..), HasProtocolConstants, HeaderHash, LocalSlotIndex,
                           SlotId, SlotLeaders, SoftwareVersion, headerHash, getGenesisHeaderHash)
import           Pos.Core.Block (BlockHeader, BlockSignature (..), GenericBlock (..), GenesisBlock,
                                 GenesisBlockHeader, GenesisBody (..), GenesisConsensusData (..),
                                 GenesisExtraBodyData (..), GenesisExtraHeaderData (..), MainBlock,
                                 MainBlockHeader, MainBody (..), MainConsensusData (..),
                                 MainExtraBodyData (..), MainExtraHeaderData (..), MainToSign (..),
                                 mkGenericHeader)
import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (..), hash, proxySign, sign,
                             toPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Delegation.Types (ProxySKBlockInfo)
import           Pos.Ssc.Base (defaultSscPayload)
import           Pos.Txp.Base (emptyTxPayload)

----------------------------------------------------------------------------
-- Main smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'MainBlockHeader'.
mkMainHeader
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader 'AttrNone)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> DecoderAttr attr
    -> MainBlockHeader attr
mkMainHeader pm prevHeader slotId sk pske body extra decAttr =
    mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extra decAttr
  where
    prevHash = either getGenesisHeaderHash headerHash prevHeader
    difficulty = either (const 0) (succ . view difficultyL) prevHeader

mkMainHeader'
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader 'AttrNone)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> MainBlockHeader 'AttrNone
mkMainHeader' pm prevHeader slotId sk pske body extra =
    mkMainHeader pm prevHeader slotId sk pske body extra DecoderAttrNone

-- | Make a 'MainBlockHeader' for a given slot, with a given body, parent hash,
-- and difficulty. This takes care of some signing and consensus data.
mkMainHeaderExplicit
    :: ProtocolMagic
    -> HeaderHash -- ^ Parent
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> DecoderAttr attr
    -> MainBlockHeader attr
mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extra decAttr =
    mkGenericHeader pm prevHash body consensus extra decAttr
  where
    makeSignature toSign (psk,_) =
        BlockPSignatureHeavy $ proxySign pm SignMainBlockHeavy sk psk toSign
    signature proof =
        let toSign = MainToSign prevHash proof slotId difficulty extra
        in maybe
               (BlockSignature $ sign pm SignMainBlock sk toSign)
               (makeSignature toSign)
               pske
    leaderPk = maybe (toPublic sk) snd pske
    consensus proof =
        MainConsensusData
        { _mcdSlot = slotId
        , _mcdLeaderKey = leaderPk
        , _mcdDifficulty = difficulty
        , _mcdSignature = signature proof
        }

mkMainHeaderExplicit'
    :: ProtocolMagic
    -> HeaderHash -- ^ Parent
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainExtraHeaderData
    -> MainBlockHeader 'AttrNone
mkMainHeaderExplicit' pm prevHash difficulty slotId sk pske body extra =
    mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extra DecoderAttrNone

-- | Smart constructor for 'MainBlock'.
mkMainBlock
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> Either GenesisHash (BlockHeader attr)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> DecoderAttr attr -- ^ header's attributes
    -> DecoderAttr attr -- ^ block's attributes
    -> MainBlock attr
mkMainBlock pm bv sv prevHeader slotId sk pske body decHeaderAttrs decAttr =
    mkMainBlockExplicit pm bv sv prevHash difficulty slotId sk pske body decHeaderAttrs decAttr
 where
    prevHash = either getGenesisHeaderHash headerHash prevHeader
    difficulty = either (const 0) (succ . view difficultyL) prevHeader

mkMainBlock'
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> Either GenesisHash (BlockHeader 'AttrNone)
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainBlock 'AttrNone
mkMainBlock' pm bv sv prevHeader slotId sk pske body = mkMainBlock pm bv sv prevHeader slotId sk pske body DecoderAttrNone DecoderAttrNone

-- | Smart constructor for 'MainBlock', without requiring the entire previous
-- 'BlockHeader'. Instead, you give its hash and the difficulty of this block.
-- These are derived from the previous header in 'mkMainBlock' so if you have
-- the previous header, consider using that one.
mkMainBlockExplicit
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> HeaderHash
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> DecoderAttr attr -- ^ header's attributes
    -> DecoderAttr attr -- ^ block's attributes
    -> MainBlock attr
mkMainBlockExplicit pm bv sv prevHash difficulty slotId sk pske body decHeaderAttr decAttr =
    UnsafeGenericBlock
        (mkMainHeaderExplicit pm prevHash difficulty slotId sk pske body extraH decHeaderAttr)
        body
        extraB
        decAttr
  where
    extraB :: MainExtraBodyData
    extraB = MainExtraBodyData (mkAttributes ())
    extraH :: MainExtraHeaderData
    extraH =
        MainExtraHeaderData
            bv
            sv
            (mkAttributes ())
            (hash extraB)

mkMainBlockExplicit'
    :: ProtocolMagic
    -> BlockVersion
    -> SoftwareVersion
    -> HeaderHash
    -> ChainDifficulty
    -> SlotId
    -> SecretKey
    -> ProxySKBlockInfo
    -> MainBody
    -> MainBlock 'AttrNone
mkMainBlockExplicit' pm bv sv prevHash difficulty slotId sk pske body =
    mkMainBlockExplicit pm bv sv prevHash difficulty slotId sk pske body DecoderAttrNone DecoderAttrNone


-- | Empty (i. e. no payload) body of main block for given local slot index.
emptyMainBody
    :: HasProtocolConstants
    => LocalSlotIndex
    -> MainBody
emptyMainBody slot =
    MainBody
    { _mbTxPayload = emptyTxPayload
    , _mbSscPayload = defaultSscPayload slot
    , _mbDlgPayload = def
    , _mbUpdatePayload = def
    }

----------------------------------------------------------------------------
-- Genesis smart constructors
----------------------------------------------------------------------------

-- | Smart constructor for 'GenesisBlockHeader'. Uses 'mkGenericHeader'.
mkGenesisHeader
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader attr)
    -> EpochIndex
    -> GenesisBody
    -> DecoderAttr attr
    -> GenesisBlockHeader attr
mkGenesisHeader pm prevHeader epoch body decAttr =
    -- here we know that genesis header construction can not fail
    mkGenericHeader
        pm
        (either getGenesisHeaderHash headerHash prevHeader)
        body
        consensus
        (GenesisExtraHeaderData $ mkAttributes ())
        decAttr
  where
    difficulty = either (const 0) (view difficultyL) prevHeader
    consensus = const (GenesisConsensusData {_gcdEpoch = epoch, _gcdDifficulty = difficulty})

mkGenesisHeader'
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader 'AttrNone)
    -> EpochIndex
    -> GenesisBody
    -> GenesisBlockHeader 'AttrNone
mkGenesisHeader' pm prevHeader epoch body = mkGenesisHeader pm prevHeader epoch body DecoderAttrNone

-- | Smart constructor for 'GenesisBlock'.
mkGenesisBlock
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader attr)
    -> EpochIndex
    -> SlotLeaders
    -> DecoderAttr attr -- ^ header's attributes
    -> DecoderAttr attr -- ^ block's attributes
    -> GenesisBlock attr
mkGenesisBlock pm prevHeader epoch leaders decHeaderAttr decAttr =
    UnsafeGenericBlock header body extra decAttr
  where
    header = mkGenesisHeader pm prevHeader epoch body decHeaderAttr
    body = GenesisBody leaders
    extra = GenesisExtraBodyData $ mkAttributes ()

mkGenesisBlock'
    :: ProtocolMagic
    -> Either GenesisHash (BlockHeader 'AttrNone)
    -> EpochIndex
    -> SlotLeaders
    -> GenesisBlock 'AttrNone
mkGenesisBlock' pm prevHeader epoch leaders = mkGenesisBlock pm prevHeader epoch leaders DecoderAttrNone DecoderAttrNone

-- | Creates the very first genesis block.
genesisBlock0
    :: ProtocolMagic
    -> GenesisHash
    -> SlotLeaders
    -> DecoderAttr attr -- ^ headers' attributes
    -> DecoderAttr attr -- ^ block's attributes
    -> GenesisBlock attr
genesisBlock0 pm genesisHash leaders decHeaderAttr decAttr =
    mkGenesisBlock pm (Left genesisHash) 0 leaders decHeaderAttr decAttr

genesisBlock0'
    :: ProtocolMagic
    -> GenesisHash
    -> SlotLeaders
    -> GenesisBlock 'AttrNone
genesisBlock0' pm genesisHash leaders = genesisBlock0 pm genesisHash leaders DecoderAttrNone DecoderAttrNone
