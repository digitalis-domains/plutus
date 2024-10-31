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

module Treasury.MainTreasury.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusLedgerApi.V3                         (
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                           ScriptHash (..),Address (..),
                                                           TxOut (..),CurrencySymbol (..),
                                                           OutputDatum (..),TokenName (..),
                                                           TxOutRef(..),Value (..),
                                                           PubKeyHash,

                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,UnsafeFromData,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Builtins                        (divideInteger)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), all,
                                                           elem, error, filter,
                                                           find, length,
                                                           otherwise,
                                                           traceError,
                                                           traceIfFalse,
                                                           (&&), (+), (==),
                                                           (>=), (||))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..),unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext (lazyTxOutRef,lazyTxInfoInputs, lazyRedeemer, lazyTxInfoReferenceInputs, lazyTxInfoSignatories)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Treasury.MainTreasury.Types              (MainTreasuryAction (..),
                                                           MainTreasuryParams (..),
                                                           MainTreasuryRefScriptDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  MainTreasuryParams
    ->  MainTreasuryAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [PubKeyHash]
    ->  [TxInInfo]
    -> Bool
mkValidator
    MainTreasuryParams{..}
    redeemer
    ownTxOutRef
    txInfoInputs
    txInfoSignatories
    txInfoReferenceInputs

        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   MainTreasuryRefScriptDatum mainTreasuryBoardPKHs treasurerPKH <- unsafeFromBuiltinData refData
            =
                case redeemer of

                    -----------------------------------------------------------------------------------------
                    --                                 FEE TREASURY MANAGEMENT                             --
                    -----------------------------------------------------------------------------------------

                    MainTreasuryManagement ->
                        traceIfFalse "MainTreasury 1.1"
                            (       all (`elem` txInfoSignatories) mainTreasuryBoardPKHs
                                ||
                                    (
                                        (length (filter (`elem` txInfoSignatories) mainTreasuryBoardPKHs) >= (length mainTreasuryBoardPKHs `divideInteger` 2) + 1)
                                    &&
                                        (treasurerPKH `elem` txInfoSignatories)
                                    )
                            )

        |   otherwise = traceError "MainTreasury 1"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "MainTreasury 2"

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "MainTreasury 3"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find (    \i ->
                                                (   case txOutAddress (txInInfoResolved i) of
                                                        (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                        _ -> False
                                                )
                                            &&
                                                fromJust (txOutReferenceScript (txInInfoResolved i)) == ownHash
                                            &&
                                                valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (scriptHashToTokenName ownHash) == 1
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "MainTreasury 4"
                        where
                            fromJust :: Maybe a -> a
                            fromJust (Just x) = x
                            fromJust _        = traceError "MainTreasury 5"

                            scriptHashToTokenName :: ScriptHash -> TokenName
                            scriptHashToTokenName (ScriptHash hash) = TokenName hash


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
--                                    COMPILATION                                       --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: MainTreasuryParams -> BuiltinData -> BuiltinUnit
mkUntypedValidator params ctx
    |   mkValidator
            params
            (lazyRedeemer ctx)
            (lazyTxOutRef ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoSignatories ctx)
            (lazyTxInfoReferenceInputs ctx)
            = unitval
    |   otherwise = error()

compiledCode :: MainTreasuryParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
