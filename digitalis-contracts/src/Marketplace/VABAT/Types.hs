module Marketplace.VABAT.Types
  ( VABATParams (..),
    VABATAction (..),
    VABATRefScriptDatum (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, ScriptHash)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString)
import PlutusTx.Prelude (Bool (..), Eq, Integer, (&&), (==))
import qualified Prelude as Haskell

data VABATParams = VABATParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''VABATParams

data VABATAction
  = MintVABAT
  | BurnVABAT
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''VABATAction
  [ ('MintVABAT, 1),
    ('BurnVABAT, 2)
  ]

data VABATRefScriptDatum = VABATRefScriptDatum
  { domainName :: BuiltinByteString,
    dnsCS :: CurrencySymbol,
    auctionAddress :: Address,
    seatCost :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq VABATRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: VABATRefScriptDatum -> VABATRefScriptDatum -> Bool
  VABATRefScriptDatum a b c d == VABATRefScriptDatum a' b' c' d'
    | a == a' && b == b' && c == c' && d == d' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''VABATRefScriptDatum [('VABATRefScriptDatum, 1)]
