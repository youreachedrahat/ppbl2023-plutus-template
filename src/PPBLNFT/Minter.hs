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
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)

{-# INLINABLE mkPolicy #-}
mkPolicy :: Integer -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = _redeemer == 1618033988

-- minting requires a PPBL 2023 Token
-- can only mint a token with a matching name
-- can mint one token
-- redeemer must be some "secret phrase"

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = mkUntypedMintingPolicy mkPolicy

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript
