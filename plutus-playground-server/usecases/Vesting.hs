{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Vesting where
-- TRIM TO HERE
-- Vesting scheme as a PLC contract
import Control.Lens (view)
import Control.Monad (void, when)
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Text qualified as T

import Ledger (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScriptWithDatumInTx, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval qualified as Interval
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test
import Plutus.V1.Ledger.Api (Address, POSIXTime, POSIXTimeRange, Validator)
import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..))
import Plutus.V1.Ledger.Contexts qualified as Validation
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)
import Prelude as Haskell (Semigroup (..), show)

{- |
    A simple vesting scheme. Money is locked by a contract and may only be
    retrieved after some time has passed.

    This is our first example of a contract that covers multiple transactions,
    with a contract state that changes over time.

    In our vesting scheme the money will be released in two _tranches_ (parts):
    A smaller part will be available after an initial number of time has
    passed, and the entire amount will be released at the end. The owner of the
    vesting scheme does not have to take out all the money at once: They can
    take out any amount up to the total that has been released so far. The
    remaining funds stay locked and can be retrieved later.

    Let's start with the data types.

-}

type VestingSchema =
        Endpoint "vest funds" ()
        .\/ Endpoint "retrieve funds" Value

-- | Tranche of a vesting scheme.
data VestingTranche = VestingTranche {
    vestingTrancheDate   :: POSIXTime,
    vestingTrancheAmount :: Value
    } deriving Generic

PlutusTx.makeLift ''VestingTranche

-- | A vesting scheme consisting of two tranches. Each tranche defines a date
--   (POSIX time) after which an additional amount can be spent.
data VestingParams = VestingParams {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche,
    vestingOwner    :: PaymentPubKeyHash
    } deriving Generic

PlutusTx.makeLift ''VestingParams

{-# INLINABLE totalAmount #-}
-- | The total amount vested
totalAmount :: VestingParams -> Value
totalAmount VestingParams{vestingTranche1,vestingTranche2} =
    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

{-# INLINABLE availableFrom #-}
-- | The amount guaranteed to be available from a given tranche in a given time range.
availableFrom :: VestingTranche -> POSIXTimeRange -> Value
availableFrom (VestingTranche d v) range =
    -- The valid range is an open-ended range starting from the tranche vesting date
    let validRange = Interval.from d
    -- If the valid range completely contains the argument range (meaning in particular
    -- that the start time of the argument range is after the tranche vesting date), then
    -- the money in the tranche is available, otherwise nothing is available.
    in if validRange `Interval.contains` range then v else zero

availableAt :: VestingParams -> POSIXTime -> Value
availableAt VestingParams{vestingTranche1, vestingTranche2} sl =
    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =
            if sl >= vestingTrancheDate then vestingTrancheAmount else mempty
    in foldMap f [vestingTranche1, vestingTranche2]

{-# INLINABLE remainingFrom #-}
-- | The amount that has not been released from this tranche yet
remainingFrom :: VestingTranche -> POSIXTimeRange -> Value
remainingFrom t@VestingTranche{vestingTrancheAmount} range =
    vestingTrancheAmount - availableFrom t range

{-# INLINABLE validate #-}
validate :: VestingParams -> () -> () -> ScriptContext -> Bool
validate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    let
        remainingActual  = Validation.valueLockedBy txInfo (Validation.ownHash ctx)

        remainingExpected =
            remainingFrom vestingTranche1 txInfoValidRange
            + remainingFrom vestingTranche2 txInfoValidRange

    in remainingActual `Value.geq` remainingExpected
            -- The policy encoded in this contract
            -- is "vestingOwner can do with the funds what they want" (as opposed
            -- to "the funds must be paid to vestingOwner"). This is enforcey by
            -- the following condition:
            && Validation.txSignedBy txInfo (unPaymentPubKeyHash vestingOwner)
            -- That way the recipient of the funds can pay them to whatever address they
            -- please, potentially saving one transaction.

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = ()

vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . typedValidator

typedValidator :: VestingParams -> Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidatorParam @Vesting
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

contractAddress :: VestingParams -> Address
contractAddress = Scripts.validatorAddress . typedValidator

vestingContract :: VestingParams -> Contract () VestingSchema T.Text ()
vestingContract vesting = selectList [vest, retrieve]
  where
    vest = endpoint @"vest funds" $ \_ -> vestFundsC vesting
    retrieve = endpoint @"retrieve funds" $ \payment -> do
        liveness <- retrieveFundsC vesting payment
        case liveness of
            Alive -> awaitPromise retrieve
            Dead  -> pure ()

payIntoContract :: Value -> TxConstraints () ()
payIntoContract = mustPayToTheScriptWithDatumInTx ()

vestFundsC
    :: VestingParams
    -> Contract () s T.Text ()
vestFundsC vesting = do
    let txn = payIntoContract (totalAmount vesting)
    mkTxConstraints (Constraints.typedValidatorLookups $ typedValidator vesting) txn
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx

data Liveness = Alive | Dead

retrieveFundsC
    :: VestingParams
    -> Value
    -> Contract () s T.Text Liveness
retrieveFundsC vesting payment = do
    let inst = typedValidator vesting
        addr = Scripts.validatorAddress inst
    now <- fst <$> currentNodeClientTimeRange
    unspentOutputs <- utxosAt addr
    let
        currentlyLocked = foldMap (view Tx.ciTxOutValue) (Map.elems unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount vesting - availableAt vesting now
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError
        $ T.unwords
            [ "Cannot take out"
            , T.pack (show payment) `T.append` "."
            , "The maximum is"
            , T.pack (show maxPayment) `T.append` "."
            , "At least"
            , T.pack (show mustRemainLocked)
            , "must remain locked by the script."
            ]

    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead  -> mempty
        txn = Constraints.collectFromTheScript unspentOutputs ()
                <> remainingOutputs
                <> mustValidateIn (Interval.from now)
                <> mustBeSignedBy (vestingOwner vesting)
                -- we don't need to add a pubkey output for 'vestingOwner' here
                -- because this will be done by the wallet when it balances the
                -- transaction.
    mkTxConstraints (Constraints.typedValidatorLookups inst
                  <> Constraints.unspentOutputs unspentOutputs) txn
      >>= adjustUnbalancedTx >>= void . submitUnbalancedTx
    return liveness

endpoints :: Contract () VestingSchema T.Text ()
endpoints = vestingContract vestingParams
  where
    vestingOwner = mockWalletPaymentPubKeyHash w1
    vestingParams =
        VestingParams {vestingTranche1, vestingTranche2, vestingOwner}
    vestingTranche1 =
        VestingTranche
            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 20000, vestingTrancheAmount = Ada.lovelaceValueOf 50_000_000}
    vestingTranche2 =
        VestingTranche
            {vestingTrancheDate = TimeSlot.scSlotZeroTime def + 40000, vestingTrancheAmount = Ada.lovelaceValueOf 30_000_000}

mkSchemaDefinitions ''VestingSchema

$(mkKnownCurrencies [])
