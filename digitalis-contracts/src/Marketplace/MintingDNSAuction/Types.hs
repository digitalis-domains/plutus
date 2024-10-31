module Marketplace.MintingDNSAuction.Types
  ( MintingDNSAuctionParams (..),
    MintingDNSAuctionDatum (..),
    MintingDNSAuctionAction (..),
    MintingDNSAuctionRefScriptDatum (..),
    Bid,
    Salt,
    SecretBid,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, POSIXTime, ScriptHash, TokenName)
import PlutusTx (BuiltinData)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString)
import PlutusTx.Prelude (Bool (..), Eq, Integer, (&&), (==))
import qualified Prelude as Haskell

data MintingDNSAuctionParams = MintingDNSAuctionParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''MintingDNSAuctionParams

type Bid = Integer

type Salt = BuiltinByteString

type SecretBid = BuiltinByteString

data MintingDNSAuctionDatum
  = -- Data structure representing auction-related information stored on-chain as datum
    AuctionDatum
      { dnsTN :: TokenName, -- The token name being auctioned
        dnsCS :: CurrencySymbol, -- The currency symbol of the auctioned token
        bidingDeadline :: POSIXTime, -- Deadline for placing bids
        revealingDeadline :: POSIXTime, -- Deadline for revealing bids
        seatCost :: Integer, -- Cost to participate in the auction
        biddingFee :: Integer,
        highestBidder :: Address, -- Address of the current highest bidder
        highestBid :: Bid -- Current highest bid amount
      }
  | -- Data type to represent the bid-related information stored on-chain
    BidDatum
      { dnsTN :: TokenName, -- The token name being bid on
        dnsCS :: CurrencySymbol, -- The currency symbol of the DNS
        secretBid :: SecretBid, -- The hash of combination of bid and salt
        bidderAddr :: Address, -- The bidder's address
        revealingDeadline :: POSIXTime -- Deadline for claiming the auctioned token
      }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MintingDNSAuctionDatum where
  {-# INLINEABLE (==) #-}
  (==) :: MintingDNSAuctionDatum -> MintingDNSAuctionDatum -> Bool
  AuctionDatum a b c d e f g h == AuctionDatum a' b' c' d' e' f' g' h'
    | a == a' && b == b' && c == c' && d == d'
        && e == e'
        && f == f'
        && g == g'
        && h == h' =
      True
  BidDatum a b c d e == BidDatum a' b' c' d' e'
    | a == a' && b == b' && c == c' && d == d'
        && e == e' =
      True
  _ == _ = False

PlutusTx.makeIsDataIndexed
  ''MintingDNSAuctionDatum
  [ ('AuctionDatum, 1),
    ('BidDatum, 2)
  ]

data MintingDNSAuctionAction
  = UpdateBid
  | BidDisclosure Bid Salt
  | DNSRedeeming
  | BidClaiming
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''MintingDNSAuctionAction
  [ ('UpdateBid, 1),
    ('BidDisclosure, 2),
    ('DNSRedeeming, 3),
    ('BidClaiming, 4)
  ]

data MintingDNSAuctionRefScriptDatum = MintingDNSAuctionRefScriptDatum
  { domainName :: BuiltinByteString,
    dnsCS :: CurrencySymbol,
    currencySymbolOfDRAT :: CurrencySymbol,
    currencySymbolOfMHABAT :: CurrencySymbol,
    treasuryAddress :: Address,
    treasuryDatum :: BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MintingDNSAuctionRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: MintingDNSAuctionRefScriptDatum -> MintingDNSAuctionRefScriptDatum -> Bool
  MintingDNSAuctionRefScriptDatum a b c d e f == MintingDNSAuctionRefScriptDatum a' b' c' d' e' f'
    | a == a' && b == b' && c == c' && d == d' && e == e' && f == f' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''MintingDNSAuctionRefScriptDatum [('MintingDNSAuctionRefScriptDatum, 1)]
