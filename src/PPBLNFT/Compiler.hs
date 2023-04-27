{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PPBLNFT.Compiler (writePlutusNFTMintingScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import PPBLNFT.Minter as Minter

ppblPolicyParams :: PolicyParams
ppblPolicyParams = PolicyParams
  { contributorTokenPolicyId = "05cf1f9c1e4cdcb6702ed2c978d55beff5e178b206b4ec7935d5e056",
    nftValidator = "3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712"
  }


writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writePlutusNFTMintingScript :: IO (Either (FileError ()) ())
writePlutusNFTMintingScript = writeValidator "output/mint-nft.plutus" $ validator ppblPolicyParams
