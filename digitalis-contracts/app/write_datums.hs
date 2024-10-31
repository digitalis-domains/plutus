import Administration.ContractsController.Types (ContractsControllerDatum (..))
import DNS.DNSMinter.Types (DNSMinterRefScriptDatum (..))
import DNS.DNSReference.Types
  ( AdditionalDataExample (..),
    DNSRecord (..),
    DNSReferenceDatum (..),
    DNSReferenceRefScriptDatum (..),
  )
import DNS.DRAT.Types (DRATRefScriptDatum (..))
import Data.List.Split (splitOn)
import Data.Maybe
  ( fromJust,
    mapMaybe,
  )
import Data.String (fromString)
import Marketplace.EnglishAuction.Types as EnglishAuction
  ( EnglishAuctionDatum (..),
    EnglishAuctionRefScriptDatum (..),
    MarketplaceInfo (..),
  )
import Marketplace.MHABAT.Types (MHABATRefScriptDatum (..))
import Marketplace.MintingDNSAuction.Types as MintingDNSAuction
  ( MintingDNSAuctionDatum (..),
    MintingDNSAuctionRefScriptDatum (..),
  )
import Marketplace.VABAT.Types (VABATRefScriptDatum (..))
import Marketplace.VickreyAuction.Types as VickreyAuction
  ( MarketplaceInfo (..),
    VickreyAuctionDatum (..),
    VickreyAuctionRefScriptDatum (..),
  )
import PlutusLedgerApi.V1.Time
  ( DiffMilliSeconds (..),
    fromMilliSeconds,
  )
import PlutusTx (toBuiltinData)
import PlutusTx.Builtins.Internal
  ( appendByteString,
    blake2b_256,
  )
import Staking.Types (StakingRefScriptDatum (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath.Posix
  ( (<.>),
    (</>),
  )
import Text.Printf (printf)
import Treasury.MainTreasury.Types
  ( MainTreasuryDatum (..),
    MainTreasuryRefScriptDatum (..),
  )
import Utils.Address (tryReadAddress)
import Utils.String (readMaybeInteger)
import Utils.ToJSON
  ( prettyPrintJSON,
    writeJSON,
  )
import Prelude
  ( Bool (..),
    Either (..),
    IO,
    Maybe (..),
    String,
    map,
    not,
    null,
    read,
    ($),
    (++),
  )

main :: IO ()
main = do
  args <- getArgs
  let printResult d c n dat innerDat = do
        createDirectoryIfMissing True (d </> c)
        printf "\nDatum location: %s\n" (d </> c </> n <.> "json")
        printf "%s\n" (prettyPrintJSON dat)
        case innerDat of
          Nothing -> printf ""
          Just d -> printf "\"contents\": %s\n" (prettyPrintJSON d)
        writeJSON (d </> c </> n <.> ".json") dat
  case args of
    datDir : contract : name : xs -> case contract of
      -------------------------------------------------------------
      --                   CONTRACTS CONTROLLER                  --
      -------------------------------------------------------------

      "ContractsController" -> case name of
        "ContractsControllerRefScriptDatum" -> case xs of
          [coreTeamCSs', boardCSs', annualAssemblyDeadline'] -> do
            let coreTeamCSs = map fromString (splitOn "," coreTeamCSs')
                boardCSs = map fromString (splitOn "," boardCSs')
                annualAssemblyDeadline = fromMilliSeconds $ DiffMilliSeconds $ read annualAssemblyDeadline'
                dat = ContractsControllerRefScriptDatum coreTeamCSs boardCSs annualAssemblyDeadline
            printResult datDir contract name dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "BoardMemberDatum" -> case xs of
          [memberNum', boardMemberPKHs'] -> do
            let boardMemberPKHs = map fromString (splitOn "," boardMemberPKHs')
                memberNum = fromString memberNum'
                dat = BoardMemberDatum boardMemberPKHs
            printResult datDir contract (name ++ "_" ++ memberNum) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                         DNS MINTER                      --
      -------------------------------------------------------------

      "DNSMinter" -> case name of
        "DNSMinterRefScriptDatum" -> case xs of
          [domainName', dNSAdminPKH', auctionAddress', treasuryAddress', feeType'] -> do
            let domainName = fromString domainName'
                dNSAdminPKH = fromString dNSAdminPKH'
                auctionAddress = fromJust $ tryReadAddress auctionAddress'
                treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                refDatum = DNSMinterRefScriptDatum domainName dNSAdminPKH auctionAddress treasuryAddress (toBuiltinData treasuryDatum)
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                             DRAT                        --
      -------------------------------------------------------------

      "DRAT" -> case name of
        "DRATRefScriptDatum" -> case xs of
          [domainName', currencySymbolOfDNS', auctionAddress', dNSReferenceAddress'] -> do
            let domainName = fromString domainName'
                currencySymbolOfDNS = fromString currencySymbolOfDNS'
                auctionAddress = fromJust $ tryReadAddress auctionAddress'
                dNSReferenceAddress = fromJust $ tryReadAddress dNSReferenceAddress'
                refDatum = DRATRefScriptDatum domainName currencySymbolOfDNS auctionAddress dNSReferenceAddress
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                      DNS REFERENCE                      --
      -------------------------------------------------------------

      "DNSReference" -> case name of
        "DNSReferenceRefScriptDatum" -> case xs of
          [domainName', currencySymbolOfDRAT', currencySymbolOfDNS'] -> do
            let domainName = fromString domainName'
                currencySymbolOfDRAT = fromString currencySymbolOfDRAT'
                currencySymbolOfDNS = fromString currencySymbolOfDNS'
                refDatum = DNSReferenceRefScriptDatum domainName currencySymbolOfDRAT currencySymbolOfDNS
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "DNSReferenceDatum" -> case xs of
          [index', origin', dnsRecords', additionalData'] -> do
            let index = fromString index'
                origin'' = fromString origin'
                origin = fromString origin'
                str =
                  case dnsRecords' of
                    list | not (null list) -> splitOn ";" dnsRecords'
                    _ -> []
                parseInputs :: String -> Maybe DNSRecord
                parseInputs str' =
                  case splitOn "," str' of
                    [lhs', ttl', rtype', rdata'] -> do
                      let lhs = fromString lhs'
                          ttl = case ttl' of
                            "" -> Nothing
                            input -> readMaybeInteger input
                          rtype = fromString rtype'
                          rdata = fromString rdata'
                      Just $ DNSRecord lhs ttl rtype rdata
                    _ -> Nothing
                dnsRecords =
                  case str of
                    [] -> []
                    _ -> mapMaybe parseInputs str
                additionalData =
                  case splitOn "," additionalData' of
                    [] -> Nothing
                    [bsExample', intExample'] -> do
                      let bsExample = fromString bsExample'
                          intExample = fromJust $ readMaybeInteger intExample'
                      Just $ toBuiltinData $ AdditionalDataExample bsExample intExample
                    _ -> Nothing
                dat = DNSReferenceDatum origin dnsRecords additionalData
            printResult datDir contract (name ++ "_" ++ origin'' ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                          MHABAT                         --
      -------------------------------------------------------------

      "MHABAT" -> case name of
        "MHABATRefScriptDatum" -> case xs of
          [domainName', dnsCS', auctionAddress', treasuryAddress', feeType'] -> do
            let domainName = fromString domainName'
                dnsCS = fromString dnsCS'
                auctionAddress = fromJust $ tryReadAddress auctionAddress'
                treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                refDatum = MHABATRefScriptDatum domainName dnsCS auctionAddress treasuryAddress (toBuiltinData treasuryDatum)
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                    MINTING DNS AUCTION                  --
      -------------------------------------------------------------

      "MintingDNSAuction" -> case name of
        "MintingDNSAuctionRefScriptDatum" -> case xs of
          [domainName', dnsCS', currencySymbolOfDRAT', currencySymbolOfMHABAT', treasuryAddress', feeType'] -> do
            let domainName = fromString domainName'
                dnsCS = fromString dnsCS'
                currencySymbolOfDRAT = fromString currencySymbolOfDRAT'
                currencySymbolOfMHABAT = fromString currencySymbolOfMHABAT'
                treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                refDatum = MintingDNSAuctionRefScriptDatum domainName dnsCS currencySymbolOfDRAT currencySymbolOfMHABAT treasuryAddress (toBuiltinData treasuryDatum)
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "AuctionDatum" -> case xs of
          [index', dnsTN', dnsCS', bidingDeadline', revealingDeadline', seatCost', biddingFee', highestBidder', highestBid'] -> do
            let index = fromString index'
                dnsTN = fromString dnsTN'
                dnsCS = fromString dnsCS'
                bidingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger bidingDeadline'
                revealingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger revealingDeadline'
                seatCost = fromJust $ readMaybeInteger seatCost'
                biddingFee = fromJust $ readMaybeInteger biddingFee'
                highestBidder = fromJust $ tryReadAddress highestBidder'
                highestBid = fromJust $ readMaybeInteger highestBid'
                dat = MintingDNSAuction.AuctionDatum dnsTN dnsCS bidingDeadline revealingDeadline seatCost biddingFee highestBidder highestBid
            printResult datDir contract (name ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "BidDatum" -> case xs of
          [index', user', bidAmount', dnsTN', dnsCS', bid', salt', bidderAddr', revealingDeadline'] -> do
            let index = fromString index'
                user = fromString user'
                bidAmount = fromString bidAmount'
                dnsTN = fromString dnsTN'
                dnsCS = fromString dnsCS'
                secretBid = blake2b_256 (fromString bid' `appendByteString` fromString salt')
                bidderAddr = fromJust $ tryReadAddress bidderAddr'
                revealingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger revealingDeadline'
                dat = MintingDNSAuction.BidDatum dnsTN dnsCS secretBid bidderAddr revealingDeadline
            printResult datDir contract (user ++ "_" ++ bidAmount ++ "_" ++ name ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                           VABAT                         --
      -------------------------------------------------------------

      "VABAT" -> case name of
        "VABATRefScriptDatum" -> case xs of
          [domainName', dnsCS', auctionAddress', seatCost'] -> do
            let domainName = fromString domainName'
                dnsCS = fromString dnsCS'
                auctionAddress = fromJust $ tryReadAddress auctionAddress'
                seatCost = fromJust $ readMaybeInteger seatCost'
                refDatum = VABATRefScriptDatum domainName dnsCS auctionAddress seatCost
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                      VICKREY AUCTION                    --
      -------------------------------------------------------------

      "VickreyAuction" -> case name of
        "VickreyAuctionRefScriptDatum" -> case xs of
          [domainName', dnsCS', currencySymbolOfVABAT', marketplaceInfo', treasuryAddress', feeType'] -> do
            let domainName = fromString domainName'
                dnsCS = fromString dnsCS'
                currencySymbolOfVABAT = fromString currencySymbolOfVABAT'
                parseInputs :: String -> Maybe VickreyAuction.MarketplaceInfo
                parseInputs str = case mapMaybe readMaybeInteger (splitOn "," str) of
                  [part1, part2, part3, part4] ->
                    Just $ VickreyAuction.MarketplaceInfo part1 part2 part3 part4
                  _ -> Nothing
                marketplaceInfo = fromJust $ parseInputs marketplaceInfo'
                treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                refDatum = VickreyAuctionRefScriptDatum domainName dnsCS currencySymbolOfVABAT marketplaceInfo treasuryAddress (toBuiltinData treasuryDatum)
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "AuctionDatum" -> case xs of
          [index', dnsTN', dnsCS', bidingDeadline', revealingDeadline', owner', highestBidder', highestBid'] -> do
            let index = fromString index'
                dnsTN = fromString dnsTN'
                dnsCS = fromString dnsCS'
                bidingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger bidingDeadline'
                revealingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger revealingDeadline'
                owner = fromJust $ tryReadAddress owner'
                highestBidder = case highestBidder' of
                  input | Just result <- tryReadAddress input -> Just result
                  _ -> Nothing
                highestBid = fromJust $ readMaybeInteger highestBid'
                dat = VickreyAuction.AuctionDatum dnsTN dnsCS bidingDeadline revealingDeadline owner highestBidder highestBid
            printResult datDir contract (name ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "BidDatum" -> case xs of
          [index', user', bidAmount', dnsTN', dnsCS', bid', salt', bidderAddr', revealingDeadline'] -> do
            let index = fromString index'
                user = fromString user'
                bidAmount = fromString bidAmount'
                dnsTN = fromString dnsTN'
                dnsCS = fromString dnsCS'
                secretBid = blake2b_256 (fromString bid' `appendByteString` fromString salt')
                bidderAddr = fromJust $ tryReadAddress bidderAddr'
                revealingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger revealingDeadline'
                dat = VickreyAuction.BidDatum dnsTN dnsCS secretBid bidderAddr revealingDeadline
            printResult datDir contract (user ++ "_" ++ bidAmount ++ "_" ++ name ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                      ENGLISH AUCTION                    --
      -------------------------------------------------------------

      "EnglishAuction" -> case name of
        "EnglishAuctionRefScriptDatum" -> case xs of
          [domainName', dnsCS', bidDiff', marketplaceInfo', treasuryAddress', feeType'] -> do
            let domainName = fromString domainName'
                dnsCS = fromString dnsCS'
                bidDiff = fromJust $ readMaybeInteger bidDiff'
                parseInputs :: String -> Maybe EnglishAuction.MarketplaceInfo
                parseInputs str = case mapMaybe readMaybeInteger (splitOn "," str) of
                  [part1, part2, part3, part4] ->
                    Just $ EnglishAuction.MarketplaceInfo part1 part2 part3 part4
                  _ -> Nothing
                marketplaceInfo = fromJust $ parseInputs marketplaceInfo'
                treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                refDatum = EnglishAuctionRefScriptDatum domainName dnsCS bidDiff marketplaceInfo treasuryAddress (toBuiltinData treasuryDatum)
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "AuctionDatum" -> case xs of
          [index', dnsTN', dnsCS', bidingDeadline', owner', highestBidder', highestBid'] -> do
            let index = fromString index'
                dnsTN = fromString dnsTN'
                dnsCS = fromString dnsCS'
                bidingDeadline = fromMilliSeconds $ DiffMilliSeconds $ fromJust $ readMaybeInteger bidingDeadline'
                owner = fromJust $ tryReadAddress owner'
                highestBidder = case highestBidder' of
                  input | Just result <- tryReadAddress input -> Just result
                  _ -> Nothing
                highestBid = fromJust $ readMaybeInteger highestBid'
                dat = EnglishAuction.AuctionDatum dnsTN dnsCS bidingDeadline owner highestBidder highestBid
            printResult datDir contract (name ++ "_" ++ index) dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      -------------------------------------------------------------
      --                      MAIN TREASURY                      --
      -------------------------------------------------------------

      "MainTreasury" -> case name of
        "MainTreasuryRefScriptDatum" -> case xs of
          [mainTreasuryBoard', treasurerPKH'] -> do
            let mainTreasuryBoard = map fromString (splitOn "," mainTreasuryBoard')
                treasurerPKH = fromString treasurerPKH'
                refDatum = MainTreasuryRefScriptDatum mainTreasuryBoard treasurerPKH
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        "MainTreasuryDatum" -> case xs of
          [feeType'] -> do
            let feeType'' = fromString feeType'
                feeType = fromJust $ readMaybeInteger feeType'
                dat = MainTreasuryDatum feeType
            printResult datDir contract (name ++ "_" ++ feeType'') dat (Nothing :: Maybe String)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid argument\n\n"
      -------------------------------------------------------------
      --                        STAKING                          --
      -------------------------------------------------------------

      "Staking" -> case name of
        "StakingRefScriptDatum" -> case xs of
          [treasuryAddress', poolID', feeType'] -> do
            let treasuryAddress = fromJust $ tryReadAddress treasuryAddress'
                treasuryDatum = MainTreasuryDatum $ fromJust (readMaybeInteger feeType')
                poolID = fromString poolID'
                refDatum = StakingRefScriptDatum treasuryAddress (toBuiltinData treasuryDatum) poolID
                dat = ContractRefScriptDatum (toBuiltinData refDatum)
            printResult datDir contract name dat (Just refDatum)
          _ -> printf "\n ERROR: Invalid number of arguments\n\n"
        _ -> printf "\n ERROR: Invalid third argument\n\n"
      _ -> printf "\n ERROR: Invalid second argument \n\n"
    _ -> printf "\n ERROR: Invalid first argument \n\n"
