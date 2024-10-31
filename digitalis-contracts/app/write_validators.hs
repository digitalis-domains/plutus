{-# LANGUAGE OverloadedStrings #-}

-- ##############  ADMINISTRATION ############# --
import Administration.BMAT.Policy as BMAT
  ( BMATParams (..),
    compiledCode,
  )
import Administration.CAT.Policy as CAT (compiledCode)
import Administration.CAT.Types as CAT
  ( CATAction (..),
    CATParams (..),
  )
import Administration.ContractsController.Types as ContractsController
  ( ContractsControllerAction (..),
    ContractsControllerDatum (..),
    ContractsControllerParams (..),
  )
import Administration.ContractsController.Validator as ContractsController (compiledCode)
-- ##############  DNS ############# --

import DNS.DNSMinter.Policy as DNSMinter (compiledCode)
import DNS.DNSMinter.Types as DNSMinter
  ( DNSMinterAction (..),
    DNSMinterParams (..),
  )
import DNS.DNSReference.Types as DNSReference
  ( DNSRecord (..),
    DNSReferenceAction (..),
    DNSReferenceDatum (..),
    DNSReferenceParams (..),
  )
import DNS.DNSReference.Validator as DNSReference (compiledCode)
import DNS.DRAT.Policy as DRAT (compiledCode)
import DNS.DRAT.Types as DRAT
  ( DRATAction (..),
    DRATParams (..),
  )
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.String (fromString)
-- ##############  Marketplace ############# --
import Marketplace.EnglishAuction.Types as EnglishAuction
  ( EnglishAuctionAction (..),
    EnglishAuctionDatum (..),
    EnglishAuctionParams (..),
  )
import Marketplace.EnglishAuction.Validator as EnglishAuction (compiledCode)
import Marketplace.MHABAT.Policy as MHABAT (compiledCode)
import Marketplace.MHABAT.Types as MHABAT
  ( MHABATAction (..),
    MHABATParams (..),
  )
import Marketplace.MintingDNSAuction.Types as MintingDNSAuction
  ( MintingDNSAuctionAction (..),
    MintingDNSAuctionDatum (..),
    MintingDNSAuctionParams (..),
  )
import Marketplace.MintingDNSAuction.Validator as MintingDNSAuction (compiledCode)
import Marketplace.VABAT.Policy as VABAT (compiledCode)
import Marketplace.VABAT.Types as VABAT
  ( VABATAction (..),
    VABATParams (..),
  )
import Marketplace.VickreyAuction.Types as VickreyAuction
  ( VickreyAuctionAction (..),
    VickreyAuctionDatum (..),
    VickreyAuctionParams (..),
  )
import Marketplace.VickreyAuction.Validator as VickreyAuction (compiledCode)
import PlutusLedgerApi.V1.Time
  ( DiffMilliSeconds (..),
    fromMilliSeconds,
  )
import PlutusLedgerApi.V3
  ( Address (..),
    BuiltinByteString,
    Credential (..),
    CurrencySymbol,
    Data (..),
    POSIXTime (..),
    PubKeyHash (..),
    ScriptHash (..),
    StakingCredential (..),
    ToData (toBuiltinData),
    TokenName (..),
    toData,
  )
import PlutusTx.Prelude
  ( Bool (..),
    Integer,
    Maybe (..),
    (<>),
  )
-- ##############  Treasury ############# --

-- ##############  Staking ############# --
import Staking.Types as Staking (StakingParams (..))
import Staking.Validator as Staking (compiledCode)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Text.Printf (printf)
import Treasury.MainTreasury.Types as MainTreasury
  ( MainTreasuryAction (..),
    MainTreasuryDatum (..),
    MainTreasuryParams (..),
  )
import Treasury.MainTreasury.Validator as MainTreasury (compiledCode)
-- ##############  Utils ############# --
import Utils.String (readMaybeInteger)
import Utils.ToJSON (prettyPrintJSON)
import Utils.WriteValidator (writeValidator)
import Prelude
  ( IO,
    String,
    map,
    read,
    ($),
    (++),
  )

main :: IO ()
main = do
  let printSCResult n c p d distDir coreDir = do
        createDirectoryIfMissing True distDir
        createDirectoryIfMissing True coreDir
        printf (n ++ "Params: %s\n") (prettyPrintJSON p)
        writeValidator (c p) d n distDir coreDir
  args <- getArgs
  case args of
    distDir : coreDir : name : xs ->
      case name of
        "CAT" -> case xs of
          [initializationDeadline'] -> do
            let initializationDeadline = fromMilliSeconds $ DiffMilliSeconds $ read initializationDeadline'
                params = CAT.CATParams initializationDeadline
            printSCResult name CAT.compiledCode params sampleDataCATToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "ContractsController" -> case xs of
          [currencySymbolOfCAT'] -> do
            let currencySymbolOfCAT = fromString currencySymbolOfCAT'
                params = ContractsController.ContractsControllerParams currencySymbolOfCAT
            printSCResult name ContractsController.compiledCode params sampleDataContractsControllerToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "BMAT" -> case xs of
          [memberNum', boardMemberPKHs', inputUTxOId', inputUTxOIdx'] -> do
            let memberNum = fromString memberNum'
                boardMemberPKHs = map fromString (splitOn "," boardMemberPKHs')
                inputUTxOId = fromString inputUTxOId'
                inputUTxOIdx = fromJust $ readMaybeInteger inputUTxOIdx'
                params = BMAT.BMATParams boardMemberPKHs inputUTxOId inputUTxOIdx
            printSCResult (name ++ "_" ++ memberNum) BMAT.compiledCode params sampleDataBMATToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        -------------------------------------------------------------
        --                       DNS SECTOR                        --
        -------------------------------------------------------------

        "DNSMinter" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = DNSMinter.DNSMinterParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name DNSMinter.compiledCode params sampleDataDNSMinterToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "DNSReference" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = DNSReference.DNSReferenceParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name DNSReference.compiledCode params sampleDataDNSReferenceToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "DRAT" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = DRAT.DRATParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name DRAT.compiledCode params sampleDataDRATToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        -------------------------------------------------------------
        --                    MARKETPLACE SECTOR                   --
        -------------------------------------------------------------

        "MintingDNSAuction" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = MintingDNSAuction.MintingDNSAuctionParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name MintingDNSAuction.compiledCode params sampleDataMintingDNSAuctionToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "MHABAT" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = MHABAT.MHABATParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name MHABAT.compiledCode params sampleDataMHABATToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "VickreyAuction" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'

                params = VickreyAuction.VickreyAuctionParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name VickreyAuction.compiledCode params sampleDataVickreyAuctionToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "VABAT" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = VABAT.VABATParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name VABAT.compiledCode params sampleDataVABATToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "EnglishAuction" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT', builtinDomainName'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                builtinDomainName = fromString builtinDomainName'
                params = EnglishAuction.EnglishAuctionParams contractsControllerSH currencySymbolOfCAT builtinDomainName
            printSCResult name EnglishAuction.compiledCode params sampleDataEnglishAuctionToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        -------------------------------------------------------------
        --                      TREASURY SECTOR                    --
        -------------------------------------------------------------

        "MainTreasury" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                params = MainTreasury.MainTreasuryParams contractsControllerSH currencySymbolOfCAT
            printSCResult name MainTreasury.compiledCode params sampleDataMainTreasuryToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        -------------------------------------------------------------
        --                      STAKING SECTOR                     --
        -------------------------------------------------------------

        "Staking" -> case xs of
          [contractsControllerSH', currencySymbolOfCAT'] -> do
            let contractsControllerSH = fromString contractsControllerSH'
                currencySymbolOfCAT = fromString currencySymbolOfCAT'
                params = Staking.StakingParams contractsControllerSH currencySymbolOfCAT
            printSCResult name Staking.compiledCode params sampleDataStakingToCalc distDir coreDir
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid first argument \n\n"
    _ -> printf "\n ERROR: Invalid number of arguments \n\n"

-- Sample data to calculate size
sampleCurrencySymbol :: CurrencySymbol
sampleCurrencySymbol = "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleTokenName :: TokenName
sampleTokenName = "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleBuiltinByteString :: BuiltinByteString
sampleBuiltinByteString = "BuiltinByteString"

samplePubKeyHash :: PubKeyHash
samplePubKeyHash = "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleScriptHash :: ScriptHash
sampleScriptHash = "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleRange :: (Integer, Integer, Integer)
sampleRange = (1, 2, 3)

sampleInteger :: Integer
sampleInteger = 42

sampleZero :: Integer
sampleZero = 0

sampleCredential :: Credential
sampleCredential = PubKeyCredential "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleStakingCredential :: Maybe StakingCredential
sampleStakingCredential = Just $ StakingHash $ PubKeyCredential "a512a21ba616d7b8897445ad269d78f27185a3e8aa553774d36fa967"

sampleAddress :: Address
sampleAddress = Address sampleCredential sampleStakingCredential

samplePOSIXTime :: POSIXTime
samplePOSIXTime = 1698542012500

-- BMAT Data

sampleDataBMATToCalc :: Data
sampleDataBMATToCalc = toData sampleZero

-- CAT Data
sampleCATRed :: CATAction
sampleCATRed = DigitalisBoardInception

sampleDataCATToCalc :: Data
sampleDataCATToCalc = toData sampleCATRed

-- ContractsController Data
sampleContractsControllerRefScriptDatum :: ContractsController.ContractsControllerDatum
sampleContractsControllerRefScriptDatum =
  ContractsControllerRefScriptDatum
    [ sampleCurrencySymbol,
      sampleCurrencySymbol
    ]
    [ sampleCurrencySymbol,
      sampleCurrencySymbol,
      sampleCurrencySymbol,
      sampleCurrencySymbol
    ]
    samplePOSIXTime

sampleContractsControllerRed :: ContractsController.ContractsControllerAction
sampleContractsControllerRed = ContractsControllerManagement

sampleDataContractsControllerToCalc :: Data
sampleDataContractsControllerToCalc = List [toData sampleContractsControllerRefScriptDatum, toData sampleContractsControllerRed]

-- DNSReference Data
sampleDNSRecord :: DNSReference.DNSRecord
sampleDNSRecord =
  DNSRecord
    sampleBuiltinByteString
    (Just sampleInteger)
    sampleBuiltinByteString
    sampleBuiltinByteString

sampleDNSReferenceDatum :: DNSReference.DNSReferenceDatum
sampleDNSReferenceDatum =
  DNSReferenceDatum
    sampleTokenName
    [sampleDNSRecord]
    Nothing

sampleDNSReferenceRed :: DNSReference.DNSReferenceAction
sampleDNSReferenceRed = Update

sampleDataDNSReferenceToCalc :: Data
sampleDataDNSReferenceToCalc = List [toData sampleDNSReferenceDatum, toData sampleDNSReferenceRed]

-- DRAT Data
sampleDRATRed :: DRATAction
sampleDRATRed = MintDRAT

sampleDataDRATToCalc :: Data
sampleDataDRATToCalc = toData sampleDRATRed

-- DNSMinter Data
sampleDNSMinterRed :: DNSMinterAction
sampleDNSMinterRed =
  MintDNS
    sampleBuiltinByteString

sampleDataDNSMinterToCalc :: Data
sampleDataDNSMinterToCalc = toData sampleDNSMinterRed

-- MHABAT Data
sampleMHABATRed :: MHABATAction
sampleMHABATRed = MintMHABAT

sampleDataMHABATToCalc :: Data
sampleDataMHABATToCalc = toData sampleMHABATRed

-- MintingDNSAuction Data
sampleMintingDNSAuctionDatum :: MintingDNSAuction.MintingDNSAuctionDatum
sampleMintingDNSAuctionDatum =
  MintingDNSAuction.AuctionDatum
    sampleTokenName
    sampleCurrencySymbol
    samplePOSIXTime
    samplePOSIXTime
    sampleInteger
    sampleInteger
    sampleAddress
    sampleInteger

sampleMintingDNSAuctionRed :: MintingDNSAuction.MintingDNSAuctionAction
sampleMintingDNSAuctionRed =
  MintingDNSAuction.BidDisclosure
    sampleInteger
    sampleBuiltinByteString

sampleDataMintingDNSAuctionToCalc :: Data
sampleDataMintingDNSAuctionToCalc = List [toData sampleMintingDNSAuctionDatum, toData sampleMintingDNSAuctionRed]

-- VABAT Data
sampleVABATRed :: VABATAction
sampleVABATRed = MintVABAT

sampleDataVABATToCalc :: Data
sampleDataVABATToCalc = toData sampleVABATRed

-- VickreyAuction Data
sampleVickreyAuctionDatum :: VickreyAuction.VickreyAuctionDatum
sampleVickreyAuctionDatum =
  VickreyAuction.AuctionDatum
    sampleTokenName
    sampleCurrencySymbol
    samplePOSIXTime
    samplePOSIXTime
    sampleAddress
    (Just sampleAddress)
    sampleInteger

sampleVickreyAuctionRed :: VickreyAuction.VickreyAuctionAction
sampleVickreyAuctionRed =
  VickreyAuction.BidDisclosure
    sampleInteger
    sampleBuiltinByteString

sampleDataVickreyAuctionToCalc :: Data
sampleDataVickreyAuctionToCalc = List [toData sampleVickreyAuctionDatum, toData sampleVickreyAuctionRed]

-- EnglishAuction Data
sampleEnglishAuctionDatum :: EnglishAuction.EnglishAuctionDatum
sampleEnglishAuctionDatum =
  EnglishAuction.AuctionDatum
    sampleTokenName
    sampleCurrencySymbol
    samplePOSIXTime
    sampleAddress
    (Just sampleAddress)
    sampleInteger

sampleEnglishAuctionRed :: EnglishAuction.EnglishAuctionAction
sampleEnglishAuctionRed = EnglishAuction.Bidding

sampleDataEnglishAuctionToCalc :: Data
sampleDataEnglishAuctionToCalc = List [toData sampleEnglishAuctionDatum, toData sampleEnglishAuctionRed]

-- MainTreasury Data
sampleMainTreasuryDatum :: MainTreasury.MainTreasuryDatum
sampleMainTreasuryDatum =
  MainTreasuryDatum
    sampleInteger

sampleMainTreasuryRed :: MainTreasury.MainTreasuryAction
sampleMainTreasuryRed = MainTreasuryManagement

sampleDataMainTreasuryToCalc :: Data
sampleDataMainTreasuryToCalc = List [toData sampleMainTreasuryDatum, toData sampleMainTreasuryRed]

-- Staking Data

sampleDataStakingToCalc :: Data
sampleDataStakingToCalc = toData sampleAddress
