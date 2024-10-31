module Administration.ContractsController.Types
  ( ContractsControllerParams (..),
    ContractsControllerDatum (..),
    ContractsControllerAction (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, PubKeyHash)
import PlutusTx (BuiltinData)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (..), Eq, (&&), (==))
import qualified Prelude as Haskell

newtype ContractsControllerParams = ContractsControllerParams
  { currencySymbolOfCAT :: CurrencySymbol
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''ContractsControllerParams

data ContractsControllerDatum
  = ContractsControllerRefScriptDatum
      { coreTeamCSs :: [CurrencySymbol],
        digitalisBoardCSs :: [CurrencySymbol],
        annualAssemblyDeadline :: POSIXTime
      }
  | BoardMemberDatum
      { boardMemberPKHs :: [PubKeyHash]
      }
  | ContractRefScriptDatum BuiltinData
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Eq ContractsControllerDatum where
  {-# INLINEABLE (==) #-}
  (==) :: ContractsControllerDatum -> ContractsControllerDatum -> Bool
  ContractsControllerRefScriptDatum a b c == ContractsControllerRefScriptDatum a' b' c'
    | a == a' && b == b' && c == c' = True
  BoardMemberDatum a == BoardMemberDatum a'
    | a == a' = True
  ContractRefScriptDatum a == ContractRefScriptDatum a'
    | a == a' = True
  _ == _ = False

PlutusTx.makeIsDataIndexed
  ''ContractsControllerDatum
  [ ('ContractsControllerRefScriptDatum, 1),
    ('BoardMemberDatum, 2),
    ('ContractRefScriptDatum, 3)
  ]

data ContractsControllerAction
  = ContractsControllerManagement
  | BoardMemberAction
  | AnnualAssemblyAction
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''ContractsControllerAction
  [ ('ContractsControllerManagement, 1),
    ('BoardMemberAction, 2),
    ('AnnualAssemblyAction, 3)
  ]
