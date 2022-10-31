{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module StakePoolDelegation where

import Control.Concurrent qualified as IO
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on, (&))
import Data.Map qualified as Map
import Streaming.Prelude qualified as S
import System.Directory qualified as IO
import System.FilePath ((</>))

import Hedgehog (Property, assert, (===))
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as HE
import Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- import Cardano.Api.Shelley qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Streaming qualified as CS
import Helpers
import Marconi.Index.StakePoolDelegation qualified as M

tests :: TestTree
tests = testGroup "StakePoolDelegation"
  [ testPropertyNamed "prop_stake_delegation" "test" test
  ]

test :: Property
test = H.integration . HE.runFinallies . workspace "chairman" $ \tempAbsBasePath' -> do

  -- start testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  (confPath, localNodeConnectInfo, conf, runtime) <- startTestnet_ base tempAbsBasePath'
  socketPath <- getSocketPathAbs conf runtime

  -- start indexer
  liftIO $ IO.forkIO $ void $ S.effects $ let
       source = CS.ledgerStates confPath socketPath
    in source & S.chain (\e -> putStrLn "got event")
              & M.toEvents
              & M.sqlite ":memory:"

  -- stake something

  utxoVKeyFile <- HE.note $ tempAbsBasePath' </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- HE.note $ tempAbsBasePath' </> "shelley/utxo-keys/utxo1.skey"
  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <-
    readAs (C.AsVerificationKey C.AsGenesisUTxOKey) utxoVKeyFile
  genesisSKey :: C.SigningKey C.GenesisUTxOKey <-
    readAs (C.AsSigningKey C.AsGenesisUTxOKey) utxoSKeyFile
  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      (getNetworkId runtime)
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr
  (tx1in, C.TxOut _ v _ _) <- do
    utxo <- findUTxOByAddress localNodeConnectInfo (C.toAddressAny address)
    headM $ Map.toList $ C.unUTxO utxo

  pparams <- getAlonzoProtocolParams localNodeConnectInfo
  let txBodyContent = (emptyTxBodyContent 10_000 pparams)
        { C.txIns = [(tx1in, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
        }
  tx1body :: C.TxBody C.AlonzoEra <- HE.leftFail $ C.makeTransactionBody txBodyContent
  let
    kw :: C.KeyWitness C.AlonzoEra
    kw = C.makeShelleyKeyWitness tx1body (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx1 = C.makeSignedTransaction [kw] tx1body
  submitTx localNodeConnectInfo tx1

  -- observe indexer

  True === True
