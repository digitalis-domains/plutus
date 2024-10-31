module DNS.DNSMinter.Types
  ( DNSMinterParams (..),
    DNSMinterAction (..),
    DNSMinterRefScriptDatum (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, PubKeyHash, ScriptHash)
import PlutusTx (BuiltinData)
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString)
import PlutusTx.Prelude (Bool (..), Eq, (&&), (==))
import qualified Prelude as Haskell

data DNSMinterParams = DNSMinterParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving
    (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''DNSMinterParams

data DNSMinterRefScriptDatum = DNSMinterRefScriptDatum
  { domainName :: BuiltinByteString,
    dNSAdminPKH :: PubKeyHash,
    auctionAddress :: Address,
    treasuryAddress :: Address,
    treasuryDatum :: BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq DNSMinterRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: DNSMinterRefScriptDatum -> DNSMinterRefScriptDatum -> Bool
  DNSMinterRefScriptDatum a b c d e == DNSMinterRefScriptDatum a' b' c' d' e'
    | a == a' && b == b' && c == c' && d == d' && e == e' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''DNSMinterRefScriptDatum [('DNSMinterRefScriptDatum, 1)]

newtype DNSMinterAction = MintDNS
  { dnsTokenName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''DNSMinterAction
  [ ('MintDNS, 1)
  ]
