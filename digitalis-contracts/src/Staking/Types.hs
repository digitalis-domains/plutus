module Staking.Types
  ( StakingRefScriptDatum (..),
    StakingParams (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (Address, CurrencySymbol, PubKeyHash, ScriptHash)
import PlutusTx (BuiltinData)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (..), Eq, (&&), (==))
import qualified Prelude as Haskell

data StakingParams = StakingParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''StakingParams

data StakingRefScriptDatum = StakingRefScriptDatum
  { treasuryAddress :: Address,
    treasuryDatum :: BuiltinData,
    poolID :: PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq StakingRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: StakingRefScriptDatum -> StakingRefScriptDatum -> Bool
  StakingRefScriptDatum a b c == StakingRefScriptDatum a' b' c'
    | (a == a') && (b == b') && (c == c') =
      True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''StakingRefScriptDatum [('StakingRefScriptDatum, 1)]
