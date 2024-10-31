{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=12 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize=True #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace=True #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Utils.WriteValidator
  ( writeCBORValidator,
    writeValidator,
  )
where

import Cardano.Api hiding (prettyPrintJSON)
import Cardano.Api.Error (displayError)
import Cardano.Api.Shelley (PlutusScript (..))
import Control.Monad.Writer (WriterT (..), runWriterT)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (length)
import Data.List (break, intercalate, reverse)
import Data.List.Split (chunksOf)
import PlutusCore.Pretty (prettyPlcReadableDef)
import PlutusLedgerApi.V3 (Data (..), SerialisedScript, deserialiseScript, serialiseCompiledCode, uncheckedDeserialiseUPLC)
import PlutusTx (BuiltinData, CompiledCode)
import PlutusTx.Code (getPlc, sizePlc)
import PlutusTx.Prelude (BuiltinUnit)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((<.>), (</>))
-- ##############  Utils ############# --
import Utils.ExBudget (conwayPV, getCPUExBudget, getMemoryExBudget, plutusCore)
import Utils.ToJSON (writeOnlyJSON)
import Prelude as Haskell (Bool (..), Char, Either (..), FilePath, IO, Maybe (..), Show, error, fst, print, putStrLn, show, snd, writeFile, ($), (++), (.), (<$>), (<>), (==))

format :: Show a => a -> [Char]
format x = h ++ t
  where
    sp = break (== '.') $ show x
    h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
    t = snd sp

scriptHash :: SerialisedScript -> ScriptHash
scriptHash = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised

contractDescription :: TextEnvelopeDescr
contractDescription = "..............."

writeCBORValidator :: FilePath -> SerialisedScript -> IO (Either (FileError ()) ())
writeCBORValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV3) (File file) (Just contractDescription) . PlutusScriptSerialised

writeValidator :: CompiledCode (BuiltinData -> BuiltinUnit) -> Data -> [Char] -> [Char] -> [Char] -> IO ()
writeValidator cC d contractName conDir coresDir = do
  let contractScript = serialiseCompiledCode cC
      plc = getPlc cC
      plcSize = sizePlc cC
      program = uncheckedDeserialiseUPLC contractScript
      semVar = "B"
      deserialiseOrError = runExceptT $ fst <$> runWriterT @() (deserialiseScript conwayPV contractScript)
      scriptEval = case deserialiseOrError of
        Right deserialisedSRC ->
          case deserialisedSRC of
            Right se -> se
            _ -> error "deserialisedSRC Failed"
        _ -> error "deserialiseOrError Failed"
  createDirectoryIfMissing True conDir
  createDirectoryIfMissing True $ coresDir </> contractName
  result <- writeCBORValidator (conDir </> contractName <.> ".plutus") contractScript
  case result of
    Left err -> print $ displayError err
    Right () -> do
      Haskell.writeFile (coresDir </> contractName </> "prettifiedTypedPlutusCore" <.> "txt") (show $ prettyPlcReadableDef plc)
      Haskell.writeFile (coresDir </> contractName </> "rawTypePlutusCore" <.> "txt") (show plc)
      Haskell.writeFile (coresDir </> contractName </> "prettifiedUntypedPlutusCore" <.> "txt") (show (prettyPlcReadableDef program))
      Haskell.writeFile (coresDir </> contractName </> "rawUntypedPlutusCore" <.> "txt") (show program)

      Haskell.putStrLn "\n<----------------------------------------CONTRACT INFO----------------------------------------->\n"
      Haskell.putStrLn $ "       Name:                       " ++ contractName
      Haskell.putStrLn $ "       Location:                   " ++ conDir </> contractName
      Haskell.putStrLn $ "       Cores Location:             " ++ coresDir </> contractName
      Haskell.putStrLn $ "       PLC Size (Bytes):           " <> show (format plcSize)
      Haskell.putStrLn $ "       CBOR Size (Bytes):          " <> show (format $ LBS.length $ writeOnlyJSON contractScript)
      Haskell.putStrLn $ "       CBOR Binary Size (Bytes):   " <> show (format $ length (plutusCore contractScript))
      -- Haskell.putStrLn    $   "       Plutus Binary Size (Bytes): "   <>  show (format $  programSize program)
      -- Haskell.putStrLn $ "       CPU Usage (Step/PSec):      " <> show (format $ getCPUExBudget semVar d scriptEval)
      -- Haskell.putStrLn $ "       Memory Usage (Bytes):       " <> show (format $ getMemoryExBudget semVar d scriptEval)
      Haskell.putStrLn $ "       Script Hash:                " <> show (scriptHash contractScript)
      Haskell.putStrLn "\n<---------------------------------------------------------------------------------------------->\n"
