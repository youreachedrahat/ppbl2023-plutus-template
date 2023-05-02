{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FaucetMiniProject.Validator where

import GHC.Generics (Generic)
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidatorParam, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show)

import FaucetMiniProject.Types

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: FaucetParams -> FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
treasuryValidator fp datum redeemer ctx =
  let info :: TxInfo
      info = scriptContextTxInfo ctx

      allTokens :: [CurrencySymbol]
      allTokens = symbols $ valueSpent info

      inputHasAccessToken :: Bool
      inputHasAccessToken = (accessTokenSymbol fp) `elem` allTokens

      valueToReceiver :: Value
      valueToReceiver = valuePaidTo info (senderPkh redeemer)

      outputHasAccessToken :: Bool
      outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol fp) (accessTokenName redeemer)) >= 1

      outputHasFaucetToken :: Bool
      outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol fp) (faucetTokenName datum)) == (withdrawalAmount datum)

      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i -> txInInfoResolved i

      -- What edge cases can you think of?
      ownOutput :: TxOut
      ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _ -> traceError "expected exactly one faucet output"

      faucetInputValue :: Value
      faucetInputValue = txOutValue ownInput

      faucetOutputValue :: Value
      faucetOutputValue = txOutValue ownOutput

      faucetContractGetsRemainingTokens :: Bool
      faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol fp) (faucetTokenName datum) - (withdrawalAmount datum) <= (valueOf faucetOutputValue (faucetTokenSymbol fp) (faucetTokenName datum)))

      getDatum :: Maybe FaucetDatum
      getDatum = case txOutDatum ownOutput of
        (OutputDatum (Datum d)) -> case fromBuiltinData d of
          Nothing -> Nothing
          Just mrd -> Just $ unsafeFromBuiltinData @FaucetDatum mrd
        _ -> Nothing

      checkDatum :: Bool
      checkDatum = case getDatum of
        Nothing -> False
        Just d -> d == datum
   in traceIfFalse "Need access token" inputHasAccessToken
        && traceIfFalse "Need access token" outputHasAccessToken
        && traceIfFalse "Need access token" outputHasFaucetToken
        && traceIfFalse "Need access token" faucetContractGetsRemainingTokens
        && traceIfFalse "Need access token" checkDatum

-- TOKEN GATING
-- check that accessToken is present -- no need for accessTokenName?

-- RATE LIMIT
-- check withdrawalAmount of faucetTokenSymbol with faucetTokenName

-- SOME STUFF
-- check that tokens are sent to PKH - this is one way to handle Eternl multi-address
-- check that datum int is correct...but this is just for show. what else could datum do?

-- In the case where there is less than the datum amount, allow there to be no UTxO returned to faucet contract.
-- Change this contract to handle the case where its originator can empty it

data FaucetTypes

instance ValidatorTypes FaucetTypes where
  type DatumType FaucetTypes = FaucetDatum
  type RedeemerType FaucetTypes = FaucetRedeemer

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator fp = go fp
  where
    go =
      mkTypedValidatorParam @FaucetTypes
        $$(PlutusTx.compile [||treasuryValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator
