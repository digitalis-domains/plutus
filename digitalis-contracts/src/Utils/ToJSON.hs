{-# LANGUAGE TypeApplications #-}

module Utils.ToJSON
  ( writeJSON,
    writeOnlyJSON,
    prettyPrintJSON,
  )
where

import Cardano.Api hiding (prettyPrintJSON)
import Cardano.Api.Shelley (PlutusScript (..), fromPlutusData)
import Codec.Serialise (serialise)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Lazy as BSL (writeFile)
import Data.ByteString.Short (toShort)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import PlutusLedgerApi.V3 (SerialisedScript)
import PlutusTx (ToData, toData)
import Prelude as Haskell
  ( FilePath,
    IO,
    Maybe (..),
    (.),
  )

writeJSON :: ToData a => Haskell.FilePath -> a -> Haskell.IO ()
writeJSON file = BSL.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . toData

contractDescription :: TextEnvelopeDescr
contractDescription = "..............."

writeOnlyJSON :: SerialisedScript -> ByteString
writeOnlyJSON = textEnvelopeToJSON @(PlutusScript PlutusScriptV3) (Just contractDescription) . PlutusScriptSerialised . toShort . toStrict . serialise

prettyPrintJSON :: ToJSON a => a -> Text
prettyPrintJSON = toLazyText . encodePrettyToTextBuilder
