module Treasury.MainTreasury.Types
  ( MainTreasuryParams (..),
    MainTreasuryDatum (..),
    MainTreasuryAction (..),
    MainTreasuryRefScriptDatum (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (CurrencySymbol, PubKeyHash, ScriptHash)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (..), Eq, Integer, (&&), (==))
import qualified Prelude as Haskell

data MainTreasuryParams = MainTreasuryParams
  { contractsControllerSH :: ScriptHash,
    currencySymbolOfCAT :: CurrencySymbol
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''MainTreasuryParams

newtype MainTreasuryDatum = MainTreasuryDatum
  { feeType :: Integer -- "0" auction bidding fee, "1" treasury fee, "2" staking reward, etc.
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MainTreasuryDatum where
  {-# INLINEABLE (==) #-}
  (==) :: MainTreasuryDatum -> MainTreasuryDatum -> Bool
  MainTreasuryDatum a == MainTreasuryDatum a'
    | a == a' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''MainTreasuryDatum [('MainTreasuryDatum, 1)]

data MainTreasuryAction
  = MainTreasuryManagement
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''MainTreasuryAction
  [ ('MainTreasuryManagement, 1)
  ]

data MainTreasuryRefScriptDatum = MainTreasuryRefScriptDatum
  { mainTreasuryBoardPKHs :: [PubKeyHash],
    treasurerPKH :: PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq MainTreasuryRefScriptDatum where
  {-# INLINEABLE (==) #-}
  (==) :: MainTreasuryRefScriptDatum -> MainTreasuryRefScriptDatum -> Bool
  MainTreasuryRefScriptDatum a b == MainTreasuryRefScriptDatum a' b'
    | a == a' && b == b' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''MainTreasuryRefScriptDatum [('MainTreasuryRefScriptDatum, 1)]
