import Administration.CAT.Types as CAT (CATAction (..))
import Administration.ContractsController.Types as ContractsController (ContractsControllerAction (..))
import DNS.DNSMinter.Types as DNSMinter (DNSMinterAction (..))
import DNS.DNSReference.Types as DNSReference (DNSReferenceAction (..))
import DNS.DRAT.Types as DRAT (DRATAction (..))
import Data.Maybe (fromJust)
import Data.String (fromString)
import Marketplace.EnglishAuction.Types as EnglishAuction (EnglishAuctionAction (..))
import Marketplace.MHABAT.Types as MHABAT (MHABATAction (..))
import Marketplace.MintingDNSAuction.Types as MintingDNSAuction (MintingDNSAuctionAction (..))
import Marketplace.VABAT.Types as VABAT (VABATAction (..))
import Marketplace.VickreyAuction.Types as VickreyAuction (VickreyAuctionAction (..))
import PlutusTx (toBuiltinData)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath.Posix
  ( (<.>),
    (</>),
  )
import Text.Printf (printf)
import Treasury.MainTreasury.Types as MainTreasury (MainTreasuryAction (..))
import Utils.Address (tryReadAddress)
import Utils.String (readMaybeInteger)
import Utils.ToJSON
  ( prettyPrintJSON,
    writeJSON,
  )
import Prelude
  ( Bool (..),
    IO,
    ($),
    (++),
  )

main :: IO ()
main = do
  args <- getArgs
  let printResult r c n red = do
        createDirectoryIfMissing True (r </> c)
        printf "\nRedeemer location: %s\n" (r </> c </> n <.> "json")
        printf "%s\n" (prettyPrintJSON red)
        writeJSON (r </> c </> n <.> ".json") red
  case args of
    redDir : contract : name : xs -> case contract of
      -------------------------------------------------------------
      --                        GENERAL                          --
      -------------------------------------------------------------

      "General" -> case name of
        "Unit" -> do
          printResult redDir contract name ()
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                          CAT                           --
      -------------------------------------------------------------

      "CAT" -> case name of
        "DigitalisBoardInception" -> do
          let red = DigitalisBoardInception
          printResult redDir contract name red
        "DigitalisBoardAction" -> do
          let red = DigitalisBoardAction
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                    SECTOR CONTROLLER                    --
      -------------------------------------------------------------

      "ContractsController" -> case name of
        "ContractsControllerManagement" -> do
          let red = ContractsControllerManagement
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                      DNS REFERENCE                      --
      -------------------------------------------------------------

      "DNSReference" -> case name of
        "Update" -> do
          let red = Update
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                          DRAT                           --
      -------------------------------------------------------------

      "DRAT" -> case name of
        "MintDRAT" -> do
          let red = MintDRAT
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                       DNS MINTER                        --
      -------------------------------------------------------------

      "DNSMinter" -> case name of
        "MintDNS" | [index', dnsTokenName'] <- xs -> do
          let index = fromString index'
              dnsTokenName = fromString dnsTokenName'
              red = MintDNS dnsTokenName
          printResult redDir contract (name ++ "_" ++ index) red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                    MINTING DNS AUCTION                  --
      -------------------------------------------------------------

      "MintingDNSAuction" -> case name of
        "UpdateBid" -> do
          let red = MintingDNSAuction.UpdateBid
          printResult redDir contract name red
        "BidDisclosure" | [index', user', bid', salt'] <- xs -> do
          let index = fromString index'
              user = fromString user'
              bid = fromJust $ readMaybeInteger bid'
              salt = fromString salt'
              red = MintingDNSAuction.BidDisclosure bid salt
          printResult redDir contract (user ++ "_" ++ name ++ "_" ++ index) red
        "DNSRedeeming" -> do
          let red = MintingDNSAuction.DNSRedeeming
          printResult redDir contract name red
        "BidClaiming" -> do
          let red = MintingDNSAuction.BidClaiming
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                         MHABAT                          --
      -------------------------------------------------------------

      "MHABAT" -> case name of
        "MintMHABAT" -> do
          let red = MintMHABAT
          printResult redDir contract name red
        "BurnMHABAT" -> do
          let red = BurnMHABAT
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                    VICKREY AUCTION                      --
      -------------------------------------------------------------

      "VickreyAuction" -> case name of
        "UpdateBid" -> do
          let red = VickreyAuction.UpdateBid
          printResult redDir contract name red
        "BidDisclosure" | [index', user', bid', salt'] <- xs -> do
          let index = fromString index'
              user = fromString user'
              bid = fromJust $ readMaybeInteger bid'
              salt = fromString salt'
              red = VickreyAuction.BidDisclosure bid salt
          printResult redDir contract (user ++ "_" ++ name ++ "_" ++ index) red
        "DNSRedeeming" -> do
          let red = VickreyAuction.DNSRedeeming
          printResult redDir contract name red
        "BidClaiming" -> do
          let red = VickreyAuction.BidClaiming
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                          VABAT                          --
      -------------------------------------------------------------

      "VABAT" -> case name of
        "MintVABAT" -> do
          let red = MintVABAT
          printResult redDir contract name red
        "BurnVABAT" -> do
          let red = BurnVABAT
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                      ENGLISH AUCTION                    --
      -------------------------------------------------------------

      "EnglishAuction" -> case name of
        "Bidding" -> do
          let red = EnglishAuction.Bidding
          printResult redDir contract name red
        "DNSRedeeming" -> do
          let red = EnglishAuction.DNSRedeeming
          printResult redDir contract name red
        "CancelAuction" -> do
          let red = EnglishAuction.CancelAuction
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      -------------------------------------------------------------
      --                      FEE TREASURY                       --
      -------------------------------------------------------------

      "MainTreasury" -> case name of
        "MainTreasuryManagement" -> do
          let red = MainTreasuryManagement
          printResult redDir contract name red
        _ -> printf "\n ERROR: Invalid third arguments\n\n"
      _ -> printf "\n ERROR: Invalid second argument \n\n"
    _ -> printf "\n ERROR: Invalid first argument \n\n"
