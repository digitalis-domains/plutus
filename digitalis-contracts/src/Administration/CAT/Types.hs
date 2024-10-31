module Administration.CAT.Types
  ( CATParams (..),
    CATAction (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Orphans ()
import PlutusLedgerApi.V3 (POSIXTime)
import qualified PlutusTx
import qualified Prelude as Haskell

newtype CATParams = CATParams
  { initializationDeadline :: POSIXTime
  }
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''CATParams

data CATAction
  = DigitalisBoardInception
  | DigitalisBoardAction
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed
  ''CATAction
  [ ('DigitalisBoardInception, 1),
    ('DigitalisBoardAction, 2)
  ]
PlutusTx.makeLift ''CATAction
