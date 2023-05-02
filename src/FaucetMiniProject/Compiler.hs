{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module FaucetMiniProject.Compiler (writeFaucetValidatorScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import FaucetMiniProject.Types
import FaucetMiniProject.Validator as Validator

faucetParams :: FaucetParams
faucetParams =
  FaucetParams
    { accessTokenSymbol = "",
      faucetTokenSymbol = ""
    }

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeFaucetValidatorScript :: IO (Either (FileError ()) ())
writeFaucetValidatorScript = writeValidator "output/my-faucet-script.plutus" $ Validator.validator faucetParams
