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

import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, mkTypedValidatorParam, validatorScript, ValidatorTypes)
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show)
import GHC.Generics              (Generic)

data FaucetParams = FaucetParams
  { accessTokenSymbol   :: !CurrencySymbol
  , accessTokenName     :: !TokenName
  , faucetTokenSymbol   :: !CurrencySymbol
  , faucetTokenName     :: !TokenName
  , withdrawalAmount    :: !Integer
  } deriving (Eq, Ord, Show, Generic)

PlutusTx.makeLift ''FaucetParams

data FaucetRedeemer = FaucetRedeemer 
    { senderPkh         :: !PubKeyHash
    , senderTokenName   :: !TokenName
    }

PlutusTx.unstableMakeIsData ''FaucetRedeemer
PlutusTx.makeLift ''FaucetRedeemer

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: FaucetParams -> Integer -> FaucetRedeemer -> ScriptContext -> Bool
treasuryValidator fp datum redeemer ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx

        recieverPkh :: PubKeyHash
        recieverPkh = senderPkh redeemer

        allTokens :: [CurrencySymbol]
        allTokens = symbols $ valueSpent info

        inputHasAccessToken :: Bool
        inputHasAccessToken = (accessTokenSymbol fp) `elem` allTokens

        valueToReceiver :: Value
        valueToReceiver = valuePaidTo info recieverPkh

        outputHasAccessToken :: Bool
        outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol fp) (senderTokenName redeemer)) >= 1

        outputHasFaucetToken :: Bool
        outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol fp) (faucetTokenName fp)) == (withdrawalAmount fp)

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of 
            Nothing -> traceError "faucet input missing"
            Just i -> txInInfoResolved i

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of 
            [o]  -> o
            _    -> traceError "expected exactly one faucet output"

        faucetInputValue :: Value
        faucetInputValue = txOutValue ownInput

        faucetOutputValue :: Value
        faucetOutputValue = txOutValue ownOutput

        faucetContractGetsRemainingTokens :: Bool
        faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol fp) (faucetTokenName fp) - (withdrawalAmount fp) <= (valueOf faucetOutputValue (faucetTokenSymbol fp) (faucetTokenName fp)))

        checkDatum :: Bool
        checkDatum = True
    
    in 
        traceIfFalse "Need access token" inputHasAccessToken &&
        traceIfFalse "Need access token" outputHasAccessToken &&
        traceIfFalse "Need access token" outputHasFaucetToken &&
        traceIfFalse "Need access token" faucetContractGetsRemainingTokens &&
        traceIfFalse "Need access token" checkDatum

-- TOKEN GATING
-- check that accessToken is present -- no need for accessTokenName?

-- RATE LIMIT
-- check withdrawalAmount of faucetTokenSymbol with faucetTokenName

-- SOME STUFF
-- check that tokens are sent to PKH - this is one way to handle Eternl multi-address
-- check that datum int is correct...but this is just for show. what else could datum do?



data FaucetTypes

instance ValidatorTypes FaucetTypes where
    type DatumType FaucetTypes = Integer
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




