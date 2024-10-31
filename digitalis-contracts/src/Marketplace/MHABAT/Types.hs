module Marketplace.MHABAT.Types
  ( MHABATParams (..),
    MHABATAction (..),
    MHABATRefScriptDatum (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, ScriptHash)
import PlutusTx (BuiltinData)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString)
import PlutusTx.Prelude (Bool (..), Eq, (&&), (==))
import qualified Prelude as Haskell

data MHABATParams = MHABATParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''MHABATParams

data MHABATAction
  = MintMHABAT
  | BurnMHABAT
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''MHABATAction
  [ ('MintMHABAT, 1),
    ('BurnMHABAT, 2)
  ]

data MHABATRefScriptDatum = MHABATRefScriptDatum
  { domainName :: BuiltinByteString,
    dnsCS :: CurrencySymbol,
    auctionAddress :: Address,
    treasuryAddress :: Address,
    treasuryDatum :: BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MHABATRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: MHABATRefScriptDatum -> MHABATRefScriptDatum -> Bool
  MHABATRefScriptDatum a b c d e == MHABATRefScriptDatum a' b' c' d' e'
    | a == a' && b == b' && c == c' && d == d' && e == e' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''MHABATRefScriptDatum [('MHABATRefScriptDatum, 1)]
