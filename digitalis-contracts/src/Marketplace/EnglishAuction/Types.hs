module Marketplace.EnglishAuction.Types
  ( EnglishAuctionParams (..),
    EnglishAuctionDatum (..),
    EnglishAuctionAction (..),
    EnglishAuctionRefScriptDatum (..),
    MarketplaceInfo (..),
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

data EnglishAuctionParams = EnglishAuctionParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''EnglishAuctionParams

data EnglishAuctionDatum =
  -- Data structure representing auction-related information stored on-chain as datum
  AuctionDatum
  { dnsTN :: TokenName, -- The token name being auctioned
    dnsCS :: CurrencySymbol, -- The currency symbol of the auctioned token
    bidingDeadline :: POSIXTime, -- Deadline for placing bids
    owner :: Address, -- Address of the owner of DNS
    highestBidder :: Maybe Address, -- Address of the current highest bidder
    highestBid :: Integer -- Current highest bid amount
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq EnglishAuctionDatum where
  {-# INLINEABLE (==) #-}
  (==) :: EnglishAuctionDatum -> EnglishAuctionDatum -> Bool
  AuctionDatum a b c d e f == AuctionDatum a' b' c' d' e' f'
    | a == a' && b == b' && c == c' && d == d'
        && e == e'
        && f == f' =
      True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''EnglishAuctionDatum [('AuctionDatum, 1)]

data EnglishAuctionAction
  = Bidding
  | DNSRedeeming
  | CancelAuction
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''EnglishAuctionAction
  [ ('Bidding, 1),
    ('DNSRedeeming, 2),
    ('CancelAuction, 3)
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

data EnglishAuctionRefScriptDatum = EnglishAuctionRefScriptDatum
  { domainName :: BuiltinByteString,
    dnsCS :: CurrencySymbol,
    bidDiff :: Integer,
    marketplaceInfo :: MarketplaceInfo,
    treasuryAddress :: Address,
    treasuryDatum :: BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq EnglishAuctionRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: EnglishAuctionRefScriptDatum -> EnglishAuctionRefScriptDatum -> Bool
  EnglishAuctionRefScriptDatum a b c d e f == EnglishAuctionRefScriptDatum a' b' c' d' e' f'
    | a == a' && b == b' && c == c' && d == d' && e == e' && f == f' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''EnglishAuctionRefScriptDatum [('EnglishAuctionRefScriptDatum, 1)]
