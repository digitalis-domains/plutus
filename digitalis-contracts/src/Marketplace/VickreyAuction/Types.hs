module Marketplace.VickreyAuction.Types
  ( VickreyAuctionParams (..),
    VickreyAuctionDatum (..),
    VickreyAuctionAction (..),
    VickreyAuctionRefScriptDatum (..),
    MarketplaceInfo (..),
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
import PlutusTx.Prelude
  ( Bool (..),
    Eq,
    Integer,
    Maybe (..),
    (&&),
    (==),
  )
import qualified Prelude as Haskell

data VickreyAuctionParams = VickreyAuctionParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''VickreyAuctionParams

type Bid = Integer

type Salt = BuiltinByteString

type SecretBid = BuiltinByteString

data VickreyAuctionDatum
  = -- Data structure representing auction-related information stored on-chain as datum
    AuctionDatum
      { dnsTN :: TokenName, -- The token name being auctioned
        dnsCS :: CurrencySymbol, -- The currency symbol of the auctioned token
        bidingDeadline :: POSIXTime, -- Deadline for placing bids
        revealingDeadline :: POSIXTime, -- Deadline for revealing bids
        owner :: Address, -- Address of the owner of DNS
        highestBidder :: Maybe Address, -- Address of the current highest bidder
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

instance Eq VickreyAuctionDatum where
  {-# INLINEABLE (==) #-}
  (==) :: VickreyAuctionDatum -> VickreyAuctionDatum -> Bool
  AuctionDatum a b c d e f g == AuctionDatum a' b' c' d' e' f' g'
    | a == a' && b == b' && c == c' && d == d'
        && e == e'
        && f == f'
        && g == g' =
      True
  BidDatum a b c d e == BidDatum a' b' c' d' e'
    | a == a' && b == b' && c == c' && d == d'
        && e == e' =
      True
  _ == _ = False

PlutusTx.makeIsDataIndexed
  ''VickreyAuctionDatum
  [ ('AuctionDatum, 1),
    ('BidDatum, 2)
  ]

data VickreyAuctionAction
  = UpdateBid
  | BidDisclosure Bid Salt
  | DNSRedeeming
  | BidClaiming
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''VickreyAuctionAction
  [ ('UpdateBid, 1),
    ('BidDisclosure, 2),
    ('DNSRedeeming, 3),
    ('BidClaiming, 4)
  ]

data MarketplaceInfo = MarketplaceInfo
  { precision :: Integer,
    minFee :: Integer,
    feePercentage :: Integer,
    tradeThreshold :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MarketplaceInfo where
  {-# INLINEABLE (==) #-}
  (==) :: MarketplaceInfo -> MarketplaceInfo -> Bool
  MarketplaceInfo a b c d == MarketplaceInfo a' b' c' d'
    | a == a' && b == b' && c == c' && d == d' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''MarketplaceInfo [('MarketplaceInfo, 1)]

data VickreyAuctionRefScriptDatum = VickreyAuctionRefScriptDatum
  { domainName :: BuiltinByteString,
    dnsCS :: CurrencySymbol,
    currencySymbolOfVABAT :: CurrencySymbol,
    marketplaceInfo :: MarketplaceInfo,
    treasuryAddress :: Address,
    treasuryDatum :: BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq VickreyAuctionRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: VickreyAuctionRefScriptDatum -> VickreyAuctionRefScriptDatum -> Bool
  VickreyAuctionRefScriptDatum a b c d e f == VickreyAuctionRefScriptDatum a' b' c' d' e' f'
    | a == a' && b == b' && c == c' && d == d' && e == e' && f == f' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''VickreyAuctionRefScriptDatum [('VickreyAuctionRefScriptDatum, 1)]
