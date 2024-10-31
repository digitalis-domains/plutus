module DNS.DRAT.Types
  ( DRATParams (..),
    DRATAction (..),
    DRATRefScriptDatum (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, ScriptHash)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString)
import PlutusTx.Prelude (Bool (..), Eq, (&&), (==))
import qualified Prelude as Haskell

data DRATParams = DRATParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''DRATParams

data DRATRefScriptDatum = DRATRefScriptDatum
  { domainName :: BuiltinByteString,
    currencySymbolOfDNS :: CurrencySymbol,
    auctionAddress :: Address,
    dNSReferenceAddress :: Address
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq DRATRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: DRATRefScriptDatum -> DRATRefScriptDatum -> Bool
  DRATRefScriptDatum a b c d == DRATRefScriptDatum a' b' c' d'
    | a == a' && b == b' && c == c' && d == d' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''DRATRefScriptDatum [('DRATRefScriptDatum, 1)]

data DRATAction
  = MintDRAT
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''DRATAction
  [ ('MintDRAT, 1)
  ]
