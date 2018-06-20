{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module TransactionSpecs (transactionSpecs) where

import           Universum
import qualified Serokell.Util.Base16 as B16

import           Cardano.Wallet.API.V1.Errors hiding (describe)
import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Pos.Binary.Class (decodeFull', serialize')
import qualified Pos.Core as Core
import           Pos.Crypto (SecretKey, SignTag (..), encToPublic,
                             encodeBase58PublicKey, hash, noPassEncrypt, sign)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Util


transactionSpecs :: WalletRef -> WalletClient IO -> Spec
transactionSpecs wRef wc = do
    describe "Transactions" $ do
        it "posted transactions appear in the index" $ do
            genesis <- genesisWallet wc
            (fromAcct, _) <- firstAccountAndId wc genesis

            wallet <- sampleWallet wRef wc
            (toAcct, toAddr) <- firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId genesis
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = halfOf (accAmount fromAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                halfOf (V1 c) = V1 (Core.mkCoin (Core.getCoin c `div` 2))

            etxn <- postTransaction wc payment

            txn <- fmap wrData etxn `shouldPrism` _Right

            eresp <- getTransactionIndex wc (Just (walId wallet)) (Just (accIndex toAcct)) Nothing
            resp <- fmap wrData eresp `shouldPrism` _Right

            map txId resp `shouldContain` [txId txn]

        it "estimate fees of a well-formed transaction" $ do
            ws <- (,)
                <$> (randomCreateWallet >>= createWalletCheck wc)
                <*> (randomCreateWallet >>= createWalletCheck wc)

            ((fromAcct, _), (_toAcct, toAddr)) <- (,)
                <$> firstAccountAndId wc (fst ws)
                <*> firstAccountAndId wc (snd ws)

            let amount = V1 (Core.mkCoin 42)

            let payment = Payment
                    { pmtSource = PaymentSource
                        { psWalletId = walId (fst ws)
                        , psAccountIndex = accIndex fromAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = amount
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }

            efee <- getTransactionFee wc payment
            case efee of
                Right fee ->
                    feeEstimatedAmount (wrData fee)
                        `shouldSatisfy`
                            (> amount)
                Left (ClientWalletError (NotEnoughMoney _)) ->
                    pure ()
                Left err ->
                    expectationFailure $
                        "Expected either a successful fee or a NotEnoughMoney "
                        <> " error, got: "
                        <> show err

        it "fails if you spend too much money" $ do
            wallet <- sampleWallet wRef wc
            (toAcct, toAddr) <- firstAccountAndId wc wallet

            let payment = Payment
                    { pmtSource =  PaymentSource
                        { psWalletId = walId wallet
                        , psAccountIndex = accIndex toAcct
                        }
                    , pmtDestinations = pure PaymentDistribution
                        { pdAddress = addrId toAddr
                        , pdAmount = tooMuchCash (accAmount toAcct)
                        }
                    , pmtGroupingPolicy = Nothing
                    , pmtSpendingPassword = Nothing
                    }
                tooMuchCash (V1 c) = V1 (Core.mkCoin (Core.getCoin c * 2))
            etxn <- postTransaction wc payment

            void $ etxn `shouldPrism` _Left

        it "create unsigned transaction and submit it to the blockchain" $ do
            -- create genesis wallet, it is initial source of money
            genesis <- genesisWallet wc
            (genesisAccount, _) <- firstAccountAndId wc genesis

            -- create external wallet, the source of test payment
            (srcWalletSecretKey, _srcWalletEncSecretKey, srcWalletPublicKey) <- makeWalletKeys
            (srcExtWallet, defaultSrcAccount) <- makeExtWalletBasedOn srcWalletPublicKey

            -- create and store new address for source wallet,
            -- we need it to send money from genesis wallet
            srcWalletAddress <- makeAddressAndStoreIt srcWalletPublicKey
                                                      srcExtWallet
                                                      defaultSrcAccount
            -- send some money to source wallet
            let initPayment = makePayment genesis
                                          genesisAccount
                                          srcWalletAddress
                                          1000000000
            txResp <- postTransaction wc initPayment
            void $ txResp `shouldPrism` _Right
            -- now source wallet contains some money

            -- create another external wallet, the destination of test payment
            (_, _, dstWalletPublicKey) <- makeWalletKeys
            (dstExtWallet, defaultDstAccount) <- makeExtWalletBasedOn dstWalletPublicKey

            -- create and store new address for destination wallet,
            -- we need it to send money from source wallet
            dstWalletAddress <- makeAddressAndStoreIt dstWalletPublicKey
                                                      dstExtWallet
                                                      defaultDstAccount
            -- test payment
            let testPayment = makePayment srcExtWallet
                                          defaultSrcAccount
                                          dstWalletAddress
                                          100000000
            rawTxResp <- postUnsignedTransaction wc testPayment
            rawTx <- rawTxResp `shouldPrism` _Right

            -- now we have a raw transaction, let's sign it (as if Ledger device did it)
            let (RawTransaction txInHexFormat) = wrData rawTx
                (Right txSerialized) = B16.decode txInHexFormat
                (Right (tx :: Core.Tx)) = decodeFull' txSerialized
                txHash = hash tx
                protocolMagic = Core.ProtocolMagic 125 -- Some random value, it's just for test cluster.
                txSignature = sign protocolMagic SignTx srcWalletSecretKey txHash
                txSignatureInHexFormat = B16.encode $ serialize' txSignature
                srcWalletPublicKeyAsBase58 = encodeBase58PublicKey srcWalletPublicKey
                signedTx = SignedTransaction srcWalletPublicKeyAsBase58
                                             txInHexFormat
                                             txSignatureInHexFormat

            -- submit signed transaction
            signedTxResp <- postSignedTransaction wc signedTx
            void $ signedTxResp `shouldPrism` _Right
            -- submittedTx <- signedTxResp `shouldPrism` _Right
            -- submittedTx `shouldBe` tx

            -- (Core.getCoin $ unV1 $ walBalance srcExtWallet) `shouldBe` 1000000000
  where
    makePayment srcWallet srcAccount dstAddress amount = Payment
        { pmtSource = PaymentSource
            { psWalletId = walId srcWallet
            , psAccountIndex = accIndex srcAccount
            }
        , pmtDestinations = pure PaymentDistribution
            { pdAddress = V1 dstAddress
            , pdAmount = V1 (Core.mkCoin amount)
            }
        , pmtGroupingPolicy = Nothing
        , pmtSpendingPassword = Nothing
        }

    makeAddressAndStoreIt publicKey wallet anAccount = do
        let anAddress = Core.makeRootPubKeyAddress publicKey
            anAddressAsBase58 = Core.addrToBase58Text anAddress

        storeResp <- postStoreAddress wc
                                      (walId wallet)
                                      (accIndex anAccount)
                                      anAddressAsBase58
        void $ storeResp `shouldPrism` _Right
        pure anAddress

    makeExtWalletBasedOn publicKey = do
        -- extWallet <- sampleExternalWallet wRef wc
        -- defaultAccount <- firstAccountInExtWallet wc extWallet
        
        newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
        extWallet <- createExternalWalletCheck wc newExtWallet
        defaultAccount <- firstAccountInExtWallet wc extWallet
        pure (extWallet, defaultAccount)

    makeWalletKeys = do
        secretKey <- randomSecretKey
        let encSecretKey = noPassEncrypt secretKey
            publicKey    = encToPublic encSecretKey
        pure (secretKey, encSecretKey, publicKey)

    randomSecretKey :: IO SecretKey
    randomSecretKey = generate arbitrary
