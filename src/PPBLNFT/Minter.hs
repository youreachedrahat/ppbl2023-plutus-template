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
{-# LANGUAGE RecordWildCards #-}

module PPBLNFT.Minter where

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api as PlutusApi
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import qualified Prelude as Pr
import GHC.Generics (Generic)
import Prelude (Show (..))

data PolicyParams = PolicyParams
  { contributorTokenPolicyId :: CurrencySymbol,
    nftValidator :: ValidatorHash
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''PolicyParams


{-# INLINABLE mkPolicy #-}
mkPolicy :: PolicyParams -> TokenName -> ScriptContext -> Bool
mkPolicy _param _tn _ctx =
  let
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    allInputValues :: Value
    allInputValues = valueSpent info

    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    -- Check that input has the PPBL 2023 Token.
    -- This check is actually redundant, give the check for txContributorToken below,
    -- but it gives us a chance to test validation in different ways.
    -- Think of this as the "learning version" of this minting contract. In production, we could omit inputHasContributorToken.
    inputHasContributorToken :: Bool
    inputHasContributorToken = contributorTokenPolicyId _param `elem` inVals

    -- Check that one token is minted, with a TokenName matching the one specified in redeemer
    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == _tn && amt == 1
        _                -> False

    -- Check that redeemer matches the TokenName of the PPBL2023 Token, without the leading 222.
    -- Notice how inputHasContributorToken, checkMintedAmount, and checkTokenName could all be combined.
    checkTokenName :: Bool
    checkTokenName = dropByteString 3 (unTokenName txContributorToken) == (unTokenName _tn)
      where
        txContributorToken :: TokenName
        txContributorToken = case [ tn | (cs, tn, _) <- flattenValue allInputValues, cs == contributorTokenPolicyId _param ] of
          [a] -> a
          _   -> traceError "Missing Contributor Token"

    -----------------------------------------------------------------------------------------------
    -- Upcoming at Live Coding
    -- Nothing in this validator prevents duplicate tokens from being minted.
    -- For that we'll need to extend our application a bit. Can you imagine any ways to do so?
    -----------------------------------------------------------------------------------------------

  in
    traceIfFalse "Must mint one token" checkMintedAmount &&
    traceIfFalse "Input must include a PPBL 2023 Token" inputHasContributorToken &&
    traceIfFalse "Minted token name must match PPBL 2023" checkTokenName

wrappedPolicy :: PolicyParams -> BuiltinData -> BuiltinData -> ()
wrappedPolicy pp r c = check (mkPolicy pp (PlutusApi.unsafeFromBuiltinData r) (PlutusApi.unsafeFromBuiltinData c))

policy :: PolicyParams -> PlutusApi.MintingPolicy
policy sp = PlutusApi.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| wrappedPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

plutusScript :: PolicyParams -> Script
plutusScript sp = unMintingPolicyScript $ policy sp

validator :: PolicyParams -> Validator
validator sp = Validator $ plutusScript sp
