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

module Staking.Validator
    ( compiledCode
    ) where

import           PlutusLedgerApi.V1.Value                 (adaSymbol, adaToken,
                                                           symbols, valueOf)
import           PlutusLedgerApi.V3                         (
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                          Address (..),Lovelace(..),
                                                           TxOut (..),Delegatee( DelegStake ),
                                                           OutputDatum (..),
                                                           TxCert(..),
                                                           Map,
                                                           ScriptInfo (..),
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,UnsafeFromData,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (toList)
import           PlutusTx.Prelude                         (Bool (..),Integer,
                                                           Maybe (..), any,
                                                           elem, error, find,
                                                           otherwise, sum,
                                                           traceError,
                                                           traceIfFalse, (&&),
                                                           (==), (>=))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext (lazyTxInfoOutputs, lazyTxInfoReferenceInputs, lazyTxInfoWdrl, lazyScriptInfo)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Staking.Types                            (StakingParams (..),
                                                           StakingRefScriptDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  StakingParams
    ->  [TxOut]
    ->  Map Credential Lovelace
    ->  [TxInInfo]
    ->  ScriptInfo
    ->  Bool
mkValidator
    StakingParams{..}
    txInfoOutputs
    txInfoWdrl
    txInfoReferenceInputs
    scriptContextScriptInfo
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   StakingRefScriptDatum treasuryAddress treasuryDatum poolID <- unsafeFromBuiltinData refData
            =
                case scriptContextScriptInfo of

                    -- registration of stake key. Restrictions needed?
                    CertifyingScript _ (TxCertRegStaking _ _)     -> True

                    -- unregister stake key. Necessary to allow this action?
                    -- CertifyingScript _ (TxCertUnRegStaking _ _)   -> True

                    -- delegate to pool
                    -- pool defined in token datum locked at ContractsControllerValidator (read only)
                    -- everyone is allowed
                    CertifyingScript _ (TxCertDelegStaking _ (DelegStake poolID')) -> traceIfFalse "wrong stake pool" (poolID' == poolID)

                    -- get rewards
                    -- output address and datum defined in token datum locked at ContractsControllerValidator (read only)
                    -- everyone is allowed
                    RewardingScript cred ->   traceIfFalse "insufficient reward sharing"
                                            (correctRewarding treasuryAddress treasuryDatum (getLovelace (getAmount cred)))

                    _              -> traceError "action not possible"

        |   otherwise = traceError "Staking 1"

        where

            correctRewarding :: Address -> BuiltinData -> Integer -> Bool
            correctRewarding addr bData amt =
                any (   \o ->       (txOutAddress o == addr)
                                &&
                                    (valueOf (txOutValue o) adaSymbol adaToken >= amt)
                                &&
                                    (getTxOutInlineDatum o == OutputDatum (Datum bData))
                    )   txInfoOutputs

            getAmount :: Credential -> Lovelace
            getAmount cred = go (toList txInfoWdrl) []
                where
                    go :: [(Credential, Lovelace)] -> [Lovelace] -> Lovelace
                    go [] [] = traceError "withdrawal not found"
                    go [] amts = sum amts
                    go ((cred', amt) : xs) amts
                        | cred' == cred = go xs (amt : amts)
                        | otherwise     = go xs amts

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "Staking 2"

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find (    \i ->
                                                (   case txOutAddress (txInInfoResolved i) of
                                                        (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                        _ -> False
                                                )
                                            &&
                                                currencySymbolOfCAT `elem` symbols (txOutValue (txInInfoResolved i))
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "Staking 3"


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
--                                    COMPILATION                                       --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: StakingParams -> BuiltinData -> BuiltinUnit
mkUntypedValidator params ctx
    |   mkValidator
            params
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoWdrl ctx)
            (lazyTxInfoReferenceInputs ctx)
            (lazyScriptInfo ctx)
            = unitval
    |   otherwise = error()

compiledCode :: StakingParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
