module DNS.DNSReference.Types
  ( DNSReferenceParams (..),
    DNSReferenceDatum (..),
    DNSReferenceAction (..),
    DNSRecord (..),
    DNSReferenceRefScriptDatum (..),
    AdditionalDataExample (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (CurrencySymbol, ScriptHash, TokenName)
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

data DNSReferenceParams = DNSReferenceParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol,
    builtinDomainName :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''DNSReferenceParams

data DNSRecord = DNSRecord
  { lhs :: BuiltinByteString, -- 255
    ttl :: Maybe Integer,
    rtype :: BuiltinByteString,
    rdata :: BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq DNSRecord where
  {-# INLINEABLE (==) #-}
  (==) :: DNSRecord -> DNSRecord -> Bool
  DNSRecord a b c d == DNSRecord a' b' c' d'
    | a == a' && b == b' && c == c' && d == d' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''DNSRecord [('DNSRecord, 1)]

data DNSReferenceDatum = DNSReferenceDatum
  { origin :: TokenName,
    dnsRecords :: [DNSRecord],
    additionalData :: Maybe BuiltinData
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq DNSReferenceDatum where
  {-# INLINEABLE (==) #-}
  (==) :: DNSReferenceDatum -> DNSReferenceDatum -> Bool
  DNSReferenceDatum a b c == DNSReferenceDatum a' b' c'
    | a == a' && b == b' && c == c' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''DNSReferenceDatum [('DNSReferenceDatum, 1)]

data AdditionalDataExample = AdditionalDataExample
  { byteStringExample :: BuiltinByteString,
    integerExample :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq AdditionalDataExample where
  {-# INLINEABLE (==) #-}
  (==) :: AdditionalDataExample -> AdditionalDataExample -> Bool
  AdditionalDataExample a b == AdditionalDataExample a' b'
    | a == a' && b == b' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''AdditionalDataExample [('AdditionalDataExample, 1)]

data DNSReferenceRefScriptDatum = DNSReferenceRefScriptDatum
  { domainName :: BuiltinByteString,
    currencySymbolOfDRAT :: CurrencySymbol,
    currencySymbolOfDNS :: CurrencySymbol
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq DNSReferenceRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: DNSReferenceRefScriptDatum -> DNSReferenceRefScriptDatum -> Bool
  DNSReferenceRefScriptDatum a b c == DNSReferenceRefScriptDatum a' b' c'
    | a == a' && b == b' && c == c' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''DNSReferenceRefScriptDatum [('DNSReferenceRefScriptDatum, 1)]

data DNSReferenceAction
  = Update
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''DNSReferenceAction
  [ ('Update, 1)
  ]
