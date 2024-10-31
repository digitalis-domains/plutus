{- ---------------------DEBUG PLUGINS----------------------- -}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:verbosity=2 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:context-level=1 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:typecheck #-}

{- ---------------------OPTIMIZATIONS PLUGINS----------------------- -}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-pir=12 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=12 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-cse-iterations=4 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:simplifier-unwrap-cancel #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:simplifier-beta #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:simplifier-inline #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:strictify-bindings #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:simplifier-remove-dead-bindings #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:relaxed-float-in #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:relaxed-float-in #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:verbosity=0 #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:inline-constants #-}

{- ---------------------LANGUAGE PLUGINS----------------------- -}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module  Administration.BMAT.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            ,   BMATParams (..)
            )
                where

import              GHC.Generics                        (Generic)
import              PlutusLedgerApi.V1.Value            (TokenName (..), flattenValue)
import              PlutusLedgerApi.V3                  (TxInInfo (..),Value,CurrencySymbol(..),TxId(..),
                                                        TxOutRef (..),PubKeyHash
                                                        )
import              PlutusTx                            (BuiltinData,unsafeApplyCode,
                                                        CompiledCode,
                                                        compile,
                                                        liftCode,
                                                        )
import qualified    PlutusTx
import              PlutusTx.Builtins                   (emptyByteString)
import              PlutusTx.Prelude                    (Bool (..), Integer, any, elem,
                                                         error, filter, length, otherwise,
                                                        (&&), (==))
import              PlutusCore.Version                  (plcVersion110)
import              PlutusTx.Builtins.Internal          (BuiltinUnit(..), unitval)
import qualified    Prelude                     as Haskell
import Data.Aeson (FromJSON, ToJSON)
import Orphans ()

import Utils.LazyScriptContext (lazyOwnCurrencySymbol,lazyTxInfoInputs,lazyTxInfoMint,lazyTxInfoSignatories)
data BMATParams = BMATParams
    {   boardMemberPKHs ::  [PubKeyHash]
    ,   inputUTxOId     ::  TxId
    ,   inputUTxOIdx    ::  Integer
    }
        deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
        deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''BMATParams

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy
    ::  BMATParams
    ->  CurrencySymbol
    ->  [TxInInfo]
    ->  Value
    ->  [PubKeyHash]
    ->  Bool
mkPolicy
    BMATParams{..}
    ownCurrencySymbol
    txInfoInputs
    txInfoMint
    txInfoSignatories =
        let txOutRef' = TxOutRef inputUTxOId inputUTxOIdx
            emptyTokenName = TokenName emptyByteString
        in      any (`elem` txInfoSignatories) boardMemberPKHs
            &&  any (\i -> txInInfoOutRef i == txOutRef') txInfoInputs
            &&  length (filter (\(cs,tn,amt) -> cs == ownCurrencySymbol && amt == 1 && tn == emptyTokenName) (flattenValue txInfoMint)) == 1
            &&  length (flattenValue txInfoMint) == 1

{-============================================== END OF MINTING POLICY SECTION =================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: BMATParams -> BuiltinData -> BuiltinUnit
mkUntypedPolicy params ctx
    |   mkPolicy
            params
            (lazyOwnCurrencySymbol ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoMint ctx)
            (lazyTxInfoSignatories ctx)
            = unitval
    |   otherwise = error()

compiledCode :: BMATParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedPolicy||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
