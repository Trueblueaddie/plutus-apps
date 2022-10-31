{-# LANGUAGE OverloadedStrings #-}

module StakePoolDelegation where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (Property, assert, (===))
import Hedgehog.Extras.Test qualified as HE
import System.Directory qualified as IO
import Test.Base qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

-- import Test.Base qualified as H

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
  (socketPathAbs, networkId, tempAbsPath) <- startTestnet base tempAbsBasePath'

  -- start indexer
  let indexer = CS.ledgerStates conf socket
        & toEvents
        & firstEventOfEveryEpoch
  & sqlite db
  & S.effects

  -- stake something
  -- observe indexer

  True === True
