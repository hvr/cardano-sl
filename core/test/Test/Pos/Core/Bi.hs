{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-dodgy-imports   #-}
module Test.Pos.Core.Bi
    ( tests
    -- , roundTripAddressBi
    , exampleSharesDistribution
    ) where

import           Universum

import           Cardano.Crypto.Wallet (xprv, xpub)
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!), zipWith4)
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Time.Units (Millisecond, fromMicroseconds)
import qualified Data.Vector as V
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Binary.Class (Raw (..), asBinary)
import           Pos.Core.Block (BlockBodyAttributes, BlockHeader (..), BlockHeaderAttributes,
                                 BlockSignature (..), GenesisBlockHeader, GenesisBody (..),
                                 GenesisBody (..), GenesisConsensusData (..),
                                 GenesisExtraHeaderData (..), GenesisProof (..), HeaderHash,
                                 MainBlockHeader, MainBody (..), MainConsensusData (..),
                                 MainExtraBodyData (..), MainExtraHeaderData (..), MainProof (..),
                                 MainToSign (..), mkGenericHeader, mkGenesisHeader,
                                 mkMainHeaderExplicit)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                                  AddrStakeDistribution (..), AddrType (..), Address (..),
                                  BlockCount (..), ChainDifficulty (..), Coeff (..), Coin (..),
                                  CoinPortion (..), IsBootstrapEraAddr (..),
                                  IsBootstrapEraAddr (..), Script (..), ScriptVersion,
                                  ScriptVersion, SharedSeed (..), SlotLeaders, StakeholderId,
                                  StakesList, StakesMap, TxFeePolicy (..), TxSizeLinear (..),
                                  coinPortionDenominator, makeAddress, makePubKeyAddress,
                                  mkMultiKeyDistr)
import           Pos.Core.Configuration (CoreConfiguration (..), GenesisConfiguration (..),
                                         GenesisHash (..))
import           Pos.Core.Delegation (DlgPayload (..), HeavyDlgIndex (..), LightDlgIndices (..),
                                      ProxySKBlockInfo, ProxySKHeavy)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisAvvmBalances (..),
                                   GenesisDelegation (..), GenesisInitializer (..),
                                   GenesisProtocolConstants (..), GenesisSpec (..),
                                   TestnetBalanceOptions (..), mkGenesisSpec)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                    LocalSlotIndex (..), SlotCount (..), SlotId (..), TimeDiff (..),
                                    Timestamp (..), localSlotIndexMaxBound, localSlotIndexMinBound)
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap, InnerSharesMap,
                               Opening, OpeningsMap, SharesDistribution, SharesMap,
                               SignedCommitment, SscPayload (..), SscProof, VssCertificate (..),
                               VssCertificatesHash, VssCertificatesMap (..), mkCommitmentsMap,
                               mkSscProof, mkVssCertificate, mkVssCertificatesMap,
                               randCommitmentAndOpening)
import           Pos.Core.Ssc (VssCertificate (..), mkVssCertificate)
import           Pos.Core.Txp (Tx (..), TxAttributes, TxAux (..), TxId, TxIn (..), TxInWitness (..),
                               TxOut (..), TxOutAux (..), TxPayload (..), TxProof (..), TxSig,
                               TxSigData (..), TxWitness (..), mkTxPayload)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..), BlockVersionData (..),
                                  BlockVersionModifier (..), SoftforkRule (..),
                                  SoftwareVersion (..), SystemTag (..), UpAttributes, UpId,
                                  UpdateData (..), UpdatePayload (..), UpdateProof,
                                  UpdateProposal (..), UpdateProposalToSign (..), UpdateProposals,
                                  UpdateVote (..), VoteId, mkUpdateVote)
import           Pos.Crypto (HDAddressPayload (..), Hash, PassPhrase, ProtocolMagic (..),
                             PublicKey (..), RedeemPublicKey, RedeemSignature, SecretKey (..),
                             SignTag (..), deterministicVssKeyGen, hash, redeemDeterministicKeyGen,
                             redeemSign, sign, toVssPublicKey)
import           Pos.Crypto.Hashing (AbstractHash (..), Hash (..), HashAlgorithm, WithHash,
                                     abstractHash, hash, withHash)
import           Pos.Crypto.HD (HDAddressPayload (..), HDPassphrase (..))
import           Pos.Crypto.Random (deterministic)
import           Pos.Crypto.SecretSharing (DecShare, EncShare, Secret, SecretProof, VssKeyPair,
                                           VssPublicKey, decryptShare, deterministicVssKeyGen,
                                           genSharedSecret, toVssPublicKey)
import           Pos.Crypto.Signing (EncryptedSecretKey, ProxyCert, ProxySecretKey, ProxySignature,
                                     PublicKey (..), SafeSigner (..), SecretKey (..), SignTag (..),
                                     Signature, Signed, createPsk, deterministicKeyGen, mkSigned,
                                     noPassEncrypt, proxySign, pskDelegatePk, safeCreateProxyCert,
                                     safeCreatePsk, sign, signEncoded, toPublic)
import           Pos.Crypto.Signing.Redeem (RedeemPublicKey, RedeemSecretKey, RedeemSignature,
                                            redeemDeterministicKeyGen, redeemSign)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Merkle (mkMerkleTree, mtRoot)

import           Serokell.Data.Memory.Units (Byte)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (discoverGolden, discoverRoundTrip, eachOf,
                                                          goldenTestBi, roundTripsBiBuildable,
                                                          roundTripsBiShow)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genProtocolMagic)

--------------------------------------------------------------------------------
-- Pos.Core.Block
--------------------------------------------------------------------------------

-- BlockBodyAttributes
golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/BlockBodyAttributes"
  where
    bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

-- BlockHeader
golden_BlockHeaderGenesis :: Property
golden_BlockHeaderGenesis = goldenTestBi bhg "test/golden/BlockHeaderGenesis"
  where
    bhg = mkGenesisHeader (ProtocolMagic 0)
                          (Left (GenesisHash prevHash))
                          (EpochIndex 11)
                          genesisBody
    genesisBody = GenesisBody (sId1 :| [sId2])
    prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a
    sId1 = coerce (hash ("stakeholder 1" :: Text))
    sId2 = coerce (hash ("stakeholder 2" :: Text))

-- golden_BlockHeaderMain :: Property
-- golden_BlockHeaderMain = goldenTestBi bhm "test/golden/BlockHeaderMain"
--   where
--     bhm = mkMainHeaderExplicit (ProtocolMagic 0) prevHash
--                                (const 5) exampleSlotId
--                                sk pske
--                                body extra
--     prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a
--     sk = undefined
--     pske = undefined
--     body = undefined
--     extra = undefined

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 10 (feedPMC genBlockHeader) roundTripsBiBuildable

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 10 (feedPMC genBlockSignature) roundTripsBiBuildable

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 (feedPM genGenesisBlockHeader) roundTripsBiBuildable

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

-- GenesisHash is just a newtype around a Hash, and lacks Bi instances. The newtype is
-- unwrapped when constructing a block, so it doesn't appear anywhere and we don't need
-- to test it.

-- HeaderHash
golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi hh "test/golden/HeaderHash"
  where hh = coerce (hash ("HeaderHash" :: Text)) :: HeaderHash

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable

golden_GenesisProof :: Property
golden_GenesisProof = goldenTestBi gp "test/golden/GenesisProof"
  where gp = GenesisProof (abstractHash exampleSlotLeaders)

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 20 (feedPMC genMainBlockHeader) roundTripsBiBuildable

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow

roundTripMainConsensusData :: Property
roundTripMainConsensusData = eachOf 20 (feedPMC genMainConsensusData) roundTripsBiShow

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable

roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 20 (feedPMC genMainToSign) roundTripsBiShow


-- group 1

-- TODO mhueschen grok why this doesn't have a Bi instance, but (Attributes AddrAttributes) does
-- ^ see module Pos.Core.Common.AddrAttributes
-- roundTripAddrAttributesBi :: Property
-- roundTripAddrAttributesBi = eachOf 1000 genAddrAttributes roundTripsBiBuildable

golden_Address :: Property
golden_Address = goldenTestBi a "test/golden/Address"
  where
    a = makeAddress exampleAddrSpendingData_PubKey attrs
    attrs = AddrAttributes hap BootstrapEraDistr
    hap = Just (HDAddressPayload (getBytes 32 32))

roundTripAddressBi :: Property
roundTripAddressBi = eachOf 1000 genAddress roundTripsBiBuildable

-- AddrSpendingData
golden_AddrSpendingData_PubKey :: Property
golden_AddrSpendingData_PubKey = goldenTestBi exampleAddrSpendingData_PubKey
                                              "test/golden/AddrSpendingData_PubKey"

golden_AddrSpendingData_Script :: Property
golden_AddrSpendingData_Script = goldenTestBi asd "test/golden/AddrSpendingData_Script"
  where asd = ScriptASD exampleScript

golden_AddrSpendingData_Redeem :: Property
golden_AddrSpendingData_Redeem = goldenTestBi asd "test/golden/AddrSpendingData_Redeem"
  where
    asd = RedeemASD redeemPublicKey
    Just redeemPublicKey = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

golden_AddrSpendingData_Unknown :: Property
golden_AddrSpendingData_Unknown = goldenTestBi asd "test/golden/AddrSpendingData_Unknown"
  where asd = UnknownASD 247 (getBytes 3 32)

roundTripAddrSpendingDataBi :: Property
roundTripAddrSpendingDataBi = eachOf 1000 genAddrSpendingData roundTripsBiBuildable

-- AddrStakeDistribution
golden_AddrStakeDistribution_Bootstrap :: Property
golden_AddrStakeDistribution_Bootstrap =
    goldenTestBi BootstrapEraDistr "test/golden/AddrStakeDistribution_Bootstrap"

golden_AddrStakeDistribution_SingleKey :: Property
golden_AddrStakeDistribution_SingleKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_SingleKey"
  where
    asd = SingleKeyDistr (abstractHash examplePublicKey)

golden_AddrStakeDistribution_UnsafeMultiKey :: Property
golden_AddrStakeDistribution_UnsafeMultiKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_UnsafeMultiKey"
  where
    asd   =  M.fromList (zip sis coins) :: Map StakeholderId CoinPortion
    sis   = [si1, si2, si3]
    coins = map (CoinPortion . exp10_14) [3,2,5]
    exp10_14 x = x * (10 :: Word64) ^ (14 :: Word64)
    Right si1 = abstractHash . PublicKey <$> xpub (getBytes  0 64)
    Right si2 = abstractHash . PublicKey <$> xpub (getBytes 13 64)
    Right si3 = abstractHash . PublicKey <$> xpub (getBytes 27 64)

roundTripAddrStakeDistributionBi :: Property
roundTripAddrStakeDistributionBi = eachOf 1000 genAddrStakeDistribution roundTripsBiBuildable

-- AddrType
golden_AddrType_PK :: Property
golden_AddrType_PK = goldenTestBi at "test/golden/AddrType_PK"
  where at = ATPubKey

golden_AddrType_S :: Property
golden_AddrType_S = goldenTestBi at "test/golden/AddrType_S"
  where at = ATScript

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestBi at "test/golden/AddrType_R"
  where at = ATRedeem

golden_AddrType_U :: Property
golden_AddrType_U = goldenTestBi at "test/golden/AddrType_U"
  where at = ATUnknown 57

roundTripAddrTypeBi :: Property
roundTripAddrTypeBi = eachOf 1000 genAddrType roundTripsBiShow

-- BlockCount
golden_BlockCount :: Property
golden_BlockCount = goldenTestBi bc "test/golden/BlockCount"
  where bc = BlockCount 999

roundTripBlockCountBi :: Property
roundTripBlockCountBi = eachOf 1000 genBlockCount roundTripsBiBuildable

-- ChainDifficulty
golden_ChainDifficulty :: Property
golden_ChainDifficulty = goldenTestBi cd "test/golden/ChainDifficulty"
  where cd = ChainDifficulty (BlockCount 9999)

roundTripChainDifficultyBi :: Property
roundTripChainDifficultyBi = eachOf 1000 genChainDifficulty roundTripsBiBuildable

-- Coeff
golden_Coeff :: Property
golden_Coeff = goldenTestBi c "test/golden/Coeff"
  where c = Coeff (MkFixed 101)

roundTripCoeffBi :: Property
roundTripCoeffBi = eachOf 1000 genCoeff roundTripsBiBuildable

-- Coin
golden_Coin :: Property
golden_Coin = goldenTestBi c "test/golden/Coin"
  where c = Coin 9732

roundTripCoinBi :: Property
roundTripCoinBi = eachOf 1000 genCoin roundTripsBiBuildable

-- CoinPortion
golden_CoinPortion :: Property
golden_CoinPortion = goldenTestBi c "test/golden/CoinPortion"
  where c = CoinPortion 9702

roundTripCoinPortionBi :: Property
roundTripCoinPortionBi = eachOf 1000 genCoinPortion roundTripsBiBuildable

-- Script
golden_Script :: Property
golden_Script = goldenTestBi exampleScript "test/golden/Script"

roundTripScriptBi :: Property
roundTripScriptBi = eachOf 1000 genScript roundTripsBiBuildable

-- ScriptVersion
golden_ScriptVersion :: Property
golden_ScriptVersion = goldenTestBi sv "test/golden/ScriptVersion"
  where sv = 6001 :: ScriptVersion

roundTripScriptVersionBi :: Property
roundTripScriptVersionBi = eachOf 1000 genScriptVersion roundTripsBiBuildable

-- SharedSeed
golden_SharedSeed :: Property
golden_SharedSeed = goldenTestBi s "test/golden/SharedSeed"
  where s = SharedSeed (getBytes 8 32)

roundTripSharedSeedBi :: Property
roundTripSharedSeedBi = eachOf 1000 genSharedSeed roundTripsBiBuildable

-- SlotLeaders
golden_SlotLeaders :: Property
golden_SlotLeaders = goldenTestBi exampleSlotLeaders "test/golden/SlotLeaders"

roundTripSlotLeadersBi :: Property
roundTripSlotLeadersBi = eachOf 1000 genSlotLeaders roundTripsBiShow

-- StakeholderId
golden_StakeholderId :: Property
golden_StakeholderId =
    goldenTestBi exampleStakeholderId "test/golden/StakeholderId"

roundTripStakeholderIdBi :: Property
roundTripStakeholderIdBi = eachOf 1000 genStakeholderId roundTripsBiBuildable

-- StakesList
golden_StakesList :: Property
golden_StakesList = goldenTestBi exampleStakesList "test/golden/StakesList"

roundTripStakesListBi :: Property
roundTripStakesListBi = eachOf 1000 genStakesList roundTripsBiShow

-- StakesMap
golden_StakesMap :: Property
golden_StakesMap = goldenTestBi sm "test/golden/StakesMap"
  where sm = HM.fromList exampleStakesList

roundTripStakesMapBi :: Property
roundTripStakesMapBi = eachOf 1000 genStakesMap roundTripsBiShow

-- TxFeePolicy
golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear = goldenTestBi tfp "test/golden/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = Coeff (MkFixed 99)
    c2 = Coeff (MkFixed 777)

golden_TxFeePolicy_Unknown :: Property
golden_TxFeePolicy_Unknown = goldenTestBi tfp "test/golden/TxFeePolicy_Unknown"
  where
    tfp = TxFeePolicyUnknown 101 (getBytes 40 32)

roundTripTxFeePolicyBi :: Property
roundTripTxFeePolicyBi = eachOf 1000 genTxFeePolicy roundTripsBiBuildable

-- TxSizeLinear
golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestBi tsl "test/golden/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = Coeff (MkFixed 999)
    c2 = Coeff (MkFixed 77)

roundTripTxSizeLinearBi :: Property
roundTripTxSizeLinearBi = eachOf 1000 genTxSizeLinear roundTripsBiBuildable

-- group 2

-- no Bi instance
-- roundTripGenesisConfigurationBi :: Property
-- roundTripGenesisConfigurationBi = eachOf 1000 genGenesisConfiguration roundTripsBiBuildable

-- no Bi instance
-- roundTripCoreConfigurationBi :: Property
-- roundTripCoreConfigurationBi = eachOf 1000 genCoreConfiguration roundTripsBiBuildable

-- DlgPayload
golden_DlgPayload :: Property
golden_DlgPayload = goldenTestBi dp "test/golden/DlgPayload"
  where dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genDlgPayload) roundTripsBiBuildable

-- HeavyDlgIndex
golden_HeavyDlgIndex :: Property
golden_HeavyDlgIndex = goldenTestBi hdi "test/golden/HeavyDlgIndex"
  where hdi = staticHeavyDlgIndexes !! 0

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

-- LightDlgIndices
golden_LightDlgIndices :: Property
golden_LightDlgIndices = goldenTestBi ldi "test/golden/LightDlgIndices"
  where ldi = LightDlgIndices (EpochIndex 7, EpochIndex 88)

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

-- ProxySKBlockInfo
golden_ProxySKBlockInfo_Nothing :: Property
golden_ProxySKBlockInfo_Nothing = goldenTestBi pskbi "test/golden/ProxySKBlockInfo_Nothing"
  where pskbi = Nothing :: ProxySKBlockInfo

golden_ProxySKBlockInfo_Just :: Property
golden_ProxySKBlockInfo_Just = goldenTestBi pskbi "test/golden/ProxySKBlockInfo_Just"
  where pskbi = Just (staticProxySKHeavys !! 0, examplePublicKey) :: ProxySKBlockInfo

roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 200 (feedPM genProxySKBlockInfo) roundTripsBiShow

-- ProxySKHeavy
golden_ProxySKHeavy :: Property
golden_ProxySKHeavy = goldenTestBi skh "test/golden/ProxySKHeavy"
  where skh = staticProxySKHeavys !! 0

roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 200 (feedPM genProxySKHeavy) roundTripsBiBuildable

-- no Bi instance
-- roundTripFakeAvvmOptionsBi :: Property
-- roundTripFakeAvvmOptionsBi = eachOf 1000 genFakeAvvmOptions roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisAvvmBalancesBi :: Property
-- roundTripGenesisAvvmBalancesBi = eachOf 1000 genGenesisAvvmBalances roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisDelegationBi :: Property
-- roundTripGenesisDelegationBi = eachOf 1000 genGenesisDelegation roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisProtocolConstantsBi :: Property
-- roundTripGenesisProtocolConstantsBi = eachOf 1000 genGenesisProtocolConstants roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisSpecBi :: Property
-- roundTripGenesisSpecBi = eachOf 1000 genGenesisSpec roundTripsBiBuildable

-- no Bi instance
-- roundTripTestnetBalanceOptionsBi :: Property
-- roundTripTestnetBalanceOptionsBi = eachOf 1000 genTestnetBalanceOptions roundTripsBiBuildable

-- no Bi instance
-- roundTripVssMaxTTLBi :: Property
-- roundTripVssMaxTTLBi = eachOf 1000 genVssMaxTTL roundTripsBiBuildable

-- no Bi instance
-- roundTripVssMinTTLBi :: Property
-- roundTripVssMinTTLBi = eachOf 1000 genVssMinTTL roundTripsBiBuildable

-- EpochIndex
golden_EpochIndex :: Property
golden_EpochIndex = goldenTestBi exampleEpochIndex "test/golden/EpochIndex"

roundTripEpochIndexBi :: Property
roundTripEpochIndexBi = eachOf 1000 genEpochIndex roundTripsBiBuildable

-- EpochOrSlot
golden_EpochOrSlotEI :: Property
golden_EpochOrSlotEI = goldenTestBi eos "test/golden/EpochOrSlotEI"
  where eos = EpochOrSlot (Left (EpochIndex 14))

golden_EpochOrSlotSI :: Property
golden_EpochOrSlotSI = goldenTestBi eos "test/golden/EpochOrSlotSI"
  where eos = EpochOrSlot (Right exampleSlotId)

roundTripEpochOrSlotBi :: Property
roundTripEpochOrSlotBi = eachOf 1000 (feedPC genEpochOrSlot) roundTripsBiBuildable

-- FlatSlotId
golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestBi fsi "test/golden/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

roundTripFlatSlotIdBi :: Property
roundTripFlatSlotIdBi = eachOf 1000 genFlatSlotId roundTripsBiBuildable

-- LocalSlotIndex
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex = goldenTestBi lsi "test/golden/LocalSlotIndex"
  where lsi = UnsafeLocalSlotIndex 52

roundTripLocalSlotIndexBi :: Property
roundTripLocalSlotIndexBi = eachOf 1000 (feedPC genLocalSlotIndex) roundTripsBiBuildable

-- SlotCount
golden_SlotCount :: Property
golden_SlotCount = goldenTestBi sc "test/golden/SlotCount"
  where sc = SlotCount 474747

roundTripSlotCountBi :: Property
roundTripSlotCountBi = eachOf 1000 genSlotCount roundTripsBiBuildable

-- SlotId
golden_SlotId :: Property
golden_SlotId = goldenTestBi exampleSlotId "test/golden/SlotId"

roundTripSlotIdBi :: Property
roundTripSlotIdBi = eachOf 1000 (feedPC genSlotId) roundTripsBiBuildable

-- TimeDiff
golden_TimeDiff :: Property
golden_TimeDiff = goldenTestBi td "test/golden/TimeDiff"
  where td = TimeDiff 4747

roundTripTimeDiffBi :: Property
roundTripTimeDiffBi = eachOf 1000 genTimeDiff roundTripsBiBuildable


-- JORDAN's TYPES

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

golden_ApplicationName :: Property
golden_ApplicationName = goldenTestBi aN "test/golden/ApplicationName"
    where aN = ApplicationName "Golden"

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 10 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

golden_Attributes :: Property
golden_Attributes = goldenTestBi attrib "test/golden/Attributes"
    where attrib = mkAttributes ()

roundTripAttributes :: Property
roundTripAttributes = eachOf 10 (genAttributes (pure ())) roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockVersion
--------------------------------------------------------------------------------

golden_BlockVersion :: Property
golden_BlockVersion = goldenTestBi bVer "test/golden/BlockVersion"
    where bVer = BlockVersion 1 1 1


roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 10 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

golden_BlockVersionData :: Property
golden_BlockVersionData = goldenTestBi bVerDat "test/golden/BlockVersionData"
    where bVerDat = exampleBlockVersionData

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 10 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionModifier
--------------------------------------------------------------------------------

golden_BlockVersionModifier :: Property
golden_BlockVersionModifier = goldenTestBi bVerMod "test/golden/BlockVersionModifier"
    where bVerMod = exampleBlockVersionModifier

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier = eachOf 10 genBlockVersionModifier roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

golden_Commitment :: Property
golden_Commitment = goldenTestBi exampleCommitment "test/golden/Commitment"

roundTripCommitment :: Property
roundTripCommitment = eachOf 10 genCommitment roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

golden_CommitmentsMap :: Property
golden_CommitmentsMap =
  goldenTestBi exampleCommitmentsMap "test/golden/CommitmentsMap"

roundTripCommitmentsMap :: Property
roundTripCommitmentsMap = eachOf 10 (genCommitmentsMap $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsSignature
--------------------------------------------------------------------------------

golden_CommitmentSignature :: Property
golden_CommitmentSignature =
    goldenTestBi exampleCommitmentSignature "test/golden/CommitmentSignature"

roundTripCommitmentSignature :: Property
roundTripCommitmentSignature = eachOf 10 (genCommitmentSignature $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

golden_BlockHashRaw :: Property
golden_BlockHashRaw = goldenTestBi hRaw "test/golden/HashRaw"
    where hRaw = (abstractHash $ Raw ("9" ) :: Hash Raw)

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 10 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

-- come back later, decshare lives in `crypto`

--golden_InnerSharesMap :: Property
--golden_InnerSharesMap = goldenTestBi iSm "test/golden/InnerSharesMap"
   -- where iSm = HM.fromList [(9,)]

roundTripInnerSharesMap :: Property
roundTripInnerSharesMap = eachOf 10 genInnerSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------

golden_MerkleTree :: Property
golden_MerkleTree = goldenTestBi mTree "test/golden/MerkleTree"
    where mTree = mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]


roundTripMerkleTree :: Property
roundTripMerkleTree = eachOf 10 (genMerkleTree genHashRaw) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------

golden_MerkleRoot :: Property
golden_MerkleRoot = goldenTestBi mTree "test/golden/MerkleRoot"
    where mTree = mtRoot $ mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

roundTripMerkleRoot :: Property
roundTripMerkleRoot = eachOf 10 (genMerkleRoot genHashRaw) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

golden_Opening :: Property
golden_Opening = goldenTestBi exampleOpening "test/golden/Opening"

roundTripOpening :: Property
roundTripOpening = eachOf 10 genOpening roundTripsBiBuildable

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

golden_OpeningsMap :: Property
golden_OpeningsMap = goldenTestBi exampleOpeningsMap "test/golden/OpeningsMap"

roundTripOpeningsMap :: Property
roundTripOpeningsMap = eachOf 10 genOpeningsMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

golden_SignedCommitment :: Property
golden_SignedCommitment =
    goldenTestBi exampleSignedCommitment "test/golden/SignedCommitment"

roundTripSignedCommitment :: Property
roundTripSignedCommitment =
    eachOf 10 (genSignedCommitment $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesDistribution
--------------------------------------------------------------------------------

golden_SharesDistribution :: Property
golden_SharesDistribution =
    goldenTestBi exampleSharesDistribution "test/golden/SharesDistribution"

roundTripSharesDistribution :: Property
roundTripSharesDistribution = eachOf 10 genSharesDistribution roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

roundTripSharesMap :: Property
roundTripSharesMap = eachOf 10 genSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

golden_SoftforkRule :: Property
golden_SoftforkRule = goldenTestBi sfR "test/golden/SoftforkRule"
    where sfR = SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

golden_SoftwareVersion :: Property
golden_SoftwareVersion = goldenTestBi swV "test/golden/SoftwareVersion"
    where swV = SoftwareVersion (ApplicationName "Golden") 99

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------
-- TODO: Need VssCertificatesMap, luke is doing this
--golden_SscPayload_Cert :: Property
--golden_SscPayload_Cert = goldenTestBi sscP_cert "test/golden/SscPayload_Cert"
--    where sscP_cert = CertificatesPayload

roundTripSscPayload :: Property
roundTripSscPayload = eachOf 10 (feedPM genSscPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

roundTripSscProof :: Property
roundTripSscProof = eachOf 10 (genSscProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

golden_SystemTag :: Property
golden_SystemTag = goldenTestBi sysT "test/golden/SystemTag"
    where sysT = SystemTag "golden"

roundTripSystemTag :: Property
roundTripSystemTag = eachOf 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TimeStamp
--------------------------------------------------------------------------------

golden_Timestamp :: Property
golden_Timestamp = goldenTestBi timeStamp "test/golden/TimeStamp"
  where
    timeStamp = Timestamp $ fromMicroseconds 47

roundTripTimestamp :: Property
roundTripTimestamp = eachOf 10 genTimestamp roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

golden_Tx :: Property
golden_Tx = goldenTestBi tx "test/golden/Tx"
    where
        tx = UnsafeTx txInList txOutList (mkAttributes ())

roundTripTx :: Property
roundTripTx = eachOf 10 genTx roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

golden_TxAttributes :: Property
golden_TxAttributes = goldenTestBi txA "test/golden/TxAttributes"
    where
        txA = mkAttributes ()


roundTripTxAttributes :: Property
roundTripTxAttributes = eachOf 10 genTxAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

-- fails
roundTripTxAux :: Property
roundTripTxAux = eachOf 1000 (feedPM genTxAux) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

golden_HashTx :: Property
golden_HashTx = goldenTestBi hashTx "test/golden/HashTx"

roundTripHashTx :: Property
roundTripHashTx = eachOf 10 genTxHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------


golden_TxInUtxo :: Property
golden_TxInUtxo = goldenTestBi txInUtxo "test/golden/TxIn_Utxo"

golden_TxInUnknown :: Property
golden_TxInUnknown = goldenTestBi txInUnknown "test/golden/TxIn_Unknown"

roundTripTxIn :: Property
roundTripTxIn = eachOf 10 genTxIn roundTripsBiBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

golden_TxId :: Property
golden_TxId = goldenTestBi txId "test/golden/TxId"

roundTripTxId :: Property
roundTripTxId = eachOf 10 genTxId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

golden_TxInList :: Property
golden_TxInList = goldenTestBi txInList "test/golden/TxInList"

roundTripTxInList :: Property
roundTripTxInList = eachOf 10 genTxInList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

golden_PkWitness :: Property
golden_PkWitness = goldenTestBi pkWitness "test/golden/TxInWitness_PkWitness"
     where
        pkWitness = PkWitness examplePublicKey txSig

golden_ScriptWitness :: Property
golden_ScriptWitness = goldenTestBi scriptWitness "test/golden/TxInWitness_ScriptWitness"
    where
        scriptWitness = ScriptWitness validatorScript redeemerScript
        validatorScript = Script 47 "serialized script"
        redeemerScript = Script 47 "serialized script"


golden_RedeemWitness :: Property
golden_RedeemWitness = goldenTestBi redeemWitness "test/golden/TxInWitness_RedeemWitness"
    where
        redeemWitness = RedeemWitness exampleRedeemPublicKey exampleRedeemSignature

golden_UnknownWitnessType :: Property
golden_UnknownWitnessType = goldenTestBi unkWitType "test/golden/TxInWitness_UnknownWitnessType"
    where
        unkWitType = UnknownWitnessType 47 "forty seven"

-- 4000 because this should generate 1000 for each constructor

roundTripTxInWitness :: Property
roundTripTxInWitness = eachOf 10 (genTxInWitness $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

golden_TxOutList :: Property
golden_TxOutList = goldenTestBi txOutList "test/golden/TxOutList"

roundTripTxOutList :: Property
roundTripTxOutList = eachOf 10 genTxOutList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

golden_TxOut :: Property
golden_TxOut = goldenTestBi txOut "test/golden/TxOut"

roundTripTxOut :: Property
roundTripTxOut = eachOf 10 genTxOut roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutAux
--------------------------------------------------------------------------------

golden_TxOutAux :: Property
golden_TxOutAux =  goldenTestBi txOutAux "test/golden/TxOutAux"
    where
        txOutAux = TxOutAux txOut

roundTripTxOutAux :: Property
roundTripTxOutAux = eachOf 10 genTxOutAux roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

-- fails
roundTripTxPayload :: Property
roundTripTxPayload = eachOf 1000 (feedPM genTxPayload) roundTripsBiShow

--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

golden_TxProof :: Property
golden_TxProof =  goldenTestBi txP "test/golden/TxProof"
    where
        txP = TxProof 32 mroot hashWit
        mroot = mtRoot $ mkMerkleTree [(UnsafeTx txInList txOutList (mkAttributes ()))]
        hashWit = hash $ [(V.fromList [(PkWitness examplePublicKey txSig)])]

roundTripTxProof :: Property
roundTripTxProof = eachOf 10 (genTxProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

golden_TxSig :: Property
golden_TxSig = goldenTestBi txSigGold "test/golden/TxSig"
    where
        txSigGold = sign (ProtocolMagic 0) SignForTestingOnly
                         exampleSecretKey txSigData

roundTripTxSig :: Property
roundTripTxSig = eachOf 10 (genTxSig $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

golden_TxSigData :: Property
golden_TxSigData = goldenTestBi txSigData "test/golden/TxSigData"

roundTripTxSigData :: Property
roundTripTxSigData = eachOf 10 genTxSigData roundTripsBiShow

--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

golden_TxWitness :: Property
golden_TxWitness = goldenTestBi txWit "test/golden/TxWitness"
    where
        txWit = V.fromList [(PkWitness examplePublicKey txSig)]

roundTripTxWitness :: Property
roundTripTxWitness = eachOf 10 (genTxWitness $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpAttributes
--------------------------------------------------------------------------------

golden_UpAttributes :: Property
golden_UpAttributes = goldenTestBi upA "test/golden/UpAttributes"
    where
        upA = mkAttributes ()

roundTripUpAttributes :: Property
roundTripUpAttributes = eachOf 10 genUpAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 10 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

roundTripUpdatePayload :: Property
roundTripUpdatePayload = eachOf 10 (feedPM genUpdatePayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 10 (genUpdateProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

roundTripUpdateProposal :: Property
roundTripUpdateProposal = eachOf 10 (genUpdateProposal $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

roundTripUpdateProposals :: Property
roundTripUpdateProposals = eachOf 10 (feedPM genUpdateProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateProposalToSign
--------------------------------------------------------------------------------

roundTripUpdateProposalToSign :: Property
roundTripUpdateProposalToSign = eachOf 10 genUpdateProposalToSign roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 10 (genUpdateVote $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

roundTripUpId :: Property
roundTripUpId = eachOf 10 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 10 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------
-- TODO:
--golden_VoteId :: Property
--golden_VoteId = goldenTestBi vID "test/golden/VoteId"
--    where
--        vID =

roundTripVoteId :: Property
roundTripVoteId = eachOf 10 (feedPM genVoteId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

golden_VssCertificate :: Property
golden_VssCertificate = goldenTestBi exampleVssCertificate "test/golden/VssCertificate"

roundTripVssCertificate :: Property
roundTripVssCertificate = eachOf 10 (genVssCertificate $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash = eachOf 10 (feedPM genVssCertificatesHash) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

roundTripVssCertificatesMap :: Property
roundTripVssCertificatesMap = eachOf 10 (genVssCertificatesMap $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- TODO move this into util package? or crypto.gen?
feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do pm <- genProtocolMagic
                  pc <- genProtocolConstants
                  genA pm pc

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

exampleCommitment :: Commitment
exampleCommitment = fst exampleCommitmentOpening

exampleCommitmentOpening :: (Commitment, Opening)
exampleCommitmentOpening =
  let numKeys   = 128 :: Int
      -- parties   = 20 :: Integer
      threshold = 15 :: Integer
      vssKeys   = replicate numKeys exampleVssPublicKey
  in  deterministic "commitmentOpening"
      $ randCommitmentAndOpening threshold (fromList vssKeys)

exampleCommitmentSignature :: CommitmentSignature
exampleCommitmentSignature =
    sign
      (ProtocolMagic 0)
      SignForTestingOnly
      exampleSecretKey
      (exampleEpochIndex, exampleCommitment)

exampleCommitmentsMap :: CommitmentsMap
exampleCommitmentsMap =
    let numCommitments = 1
        signedCommitments = replicate numCommitments exampleSignedCommitment
    in  mkCommitmentsMap signedCommitments

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleOpening :: Opening
exampleOpening = snd exampleCommitmentOpening

exampleOpeningsMap :: OpeningsMap
exampleOpeningsMap =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        openings = replicate mapSize exampleOpening
    in  HM.fromList $ zip stakeholderIds openings

exampleSharesDistribution :: SharesDistribution
exampleSharesDistribution =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s = [1337]
    in  HM.fromList $ zip stakeholderIds word16s

exampleSignedCommitment :: SignedCommitment
exampleSignedCommitment =
    (examplePublicKey, exampleCommitment, exampleCommitmentSignature)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = abstractHash examplePublicKey :: StakeholderId

exampleVssPublicKey :: VssPublicKey
exampleVssPublicKey = toVssPublicKey mkVssKeyPair
  where
    mkVssKeyPair = deterministicVssKeyGen $ (getBytes 0 32)

hashTx :: Hash Tx
hashTx = coerce (hash "golden" :: Hash Text)

txId :: TxId
txId = hashTx

txInUnknown :: TxIn
txInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

txInUtxo :: TxIn
txInUtxo = TxInUtxo hashTx 47 -- TODO: loop here

txInList :: (NonEmpty TxIn)
txInList = fromList [txInUtxo]

txOut :: TxOut
txOut = TxOut (makePubKeyAddress (IsBootstrapEraAddr True) pkey) (Coin 47)
    where
        Right pkey = PublicKey <$> xpub (getBytes 0 64)

txOutList :: (NonEmpty TxOut)
txOutList = fromList [txOut]

txSig :: TxSig
txSig = sign (ProtocolMagic 0) SignForTestingOnly exampleSecretKey txSigData

txSigData :: TxSigData
txSigData = TxSigData hashTx

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleBlockVersionModifier :: BlockVersionModifier
exampleBlockVersionModifier = BlockVersionModifier
                              (Just (999 :: ScriptVersion))
                              (Just (999 :: Millisecond))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just (99 :: FlatSlotId))
                              (Just sfrule')
                              (Just $ TxFeePolicyTxSizeLinear tslin')
                              (Just $ EpochIndex 99)
    where
        tslin' = TxSizeLinear co1 co2
        co1 = Coeff (MkFixed 999)
        co2 = Coeff (MkFixed 77)
        sfrule' = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))


exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature = redeemSign (ProtocolMagic 0) SignForTestingOnly rsk txSigData
    where
        rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = (exampleSecretKeys 10 2) !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> xprv (getBytes start 128)
                   in sk

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = map abstractHash (examplePublicKeys 15 3)
    coins = map Coin [79, 44, 9999999]

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (fromList (examplePublicKeys 16 3))

exampleVssCertificate :: VssCertificate
exampleVssCertificate =
    mkVssCertificate
        (ProtocolMagic 0)
        exampleSecretKey
        (asBinary (toVssPublicKey $ deterministicVssKeyGen ("golden" :: ByteString)))
        (EpochIndex 11)

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4 safeCreatePsk
                               staticProtocolMagics staticSafeSigners
                               (examplePublicKeys 1 6) staticHeavyDlgIndexes

staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5,1,3,27,99,247]

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0..5]

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
