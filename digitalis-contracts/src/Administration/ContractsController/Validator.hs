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

module Administration.ContractsController.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusLedgerApi.V1.Interval              (contains, from, to)
import           PlutusLedgerApi.V1.Value                 (flattenValue)
import           PlutusLedgerApi.V3                         (POSIXTimeRange,Address (..),Credential (..),CurrencySymbol (..),
                                                           TokenName (..),
                                                           Value (..),
                                                           TxInInfo (..),Datum (..),
                                                           ScriptHash (..),
                                                           TxOut (..),
                                                           OutputDatum (..),
                                                           TxOutRef(..),
                                                           PubKeyHash,
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Builtins                        (divideInteger,
                                                           emptyByteString)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), all, any,
                                                           elem, error, filter,
                                                           find, length, map,
                                                           not, null, otherwise,
                                                           traceError,
                                                           traceIfFalse,
                                                           (&&), (+), (<), (<=),
                                                           (==), (>=))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..),unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext ( lazyDatum, lazyTxOutRef,lazyTxInfoInputs, lazyRedeemer, lazyTxInfoOutputs, lazyTxInfoReferenceInputs, lazyTxInfoSignatories, lazyTxInfoValidRange)

import           Administration.ContractsController.Types (ContractsControllerAction (..),
                                                           ContractsControllerDatum (..),
                                                           ContractsControllerParams (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  ContractsControllerParams
    ->  ContractsControllerDatum
    ->  ContractsControllerAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Bool
mkValidator
    ContractsControllerParams{..}
    datum
    redeemer
    ownTxOutRef
    txInfoInputs
    txInfoOutputs
    txInfoValidRange
    txInfoSignatories
    txInfoReferenceInputs
        |   ContractsControllerRefScriptDatum coreTeamCSs digitalisBoardCSs annualAssemblyDeadline <- getTxOutInlineDatum getScriptRefTxOut
            =
                case (redeemer, datum) of

                    -----------------------------------------------------------------------------------------
                    --                            CONTRACTS CONTROLLER MANAGEMENT                          --
                    -----------------------------------------------------------------------------------------

                    (ContractsControllerManagement, dat) ->
                        case dat of
                            ContractsControllerRefScriptDatum {}
                                |   ContractsControllerRefScriptDatum coreTeamCSs' digitalisBoardCSs' annualAssemblyDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                                    ->
                                        traceIfFalse "ContractsController 1.1"
                                            (didDigitalisBoardMembersSinged digitalisBoardCSs)
                                    &&  traceIfFalse "ContractsController 1.2"
                                            (any isBoardMemberCSPresent coreTeamCSs)
                                    &&  traceIfFalse "ContractsController 1.3"
                                            (valueOf (txOutValue singleScriptOutputUTxO) currencySymbolOfCAT emptyTokenName == 1)
                                    &&  traceIfFalse "ContractsController 1.4"
                                            (not (null digitalisBoardCSs'))
                                    &&  traceIfFalse "ContractsController 1.5"
                                            (coreTeamCSs == coreTeamCSs')
                                    &&  traceIfFalse "ContractsController 1.6"
                                            (annualAssemblyDeadline == annualAssemblyDeadline')
                                    &&  traceIfFalse "ContractsController 1.7"
                                            (to annualAssemblyDeadline `contains` txInfoValidRange)

                            ContractRefScriptDatum {} ->
                                    traceIfFalse "ContractsController 1.8"
                                        (didDigitalisBoardMembersSinged digitalisBoardCSs)
                                &&  traceIfFalse "ContractsController 1.9"
                                        (any isBoardMemberCSPresent coreTeamCSs)

                            _ -> False

                    -----------------------------------------------------------------------------------------
                    --                         CONTRACTS BOARD MEMBER ACTION                               --
                    -----------------------------------------------------------------------------------------

                    (BoardMemberAction,
                        BoardMemberDatum boardMemberPKHs) ->
                                traceIfFalse "ContractsController 2.1"
                                    (isMember digitalisBoardCSs)
                            &&  traceIfFalse "ContractsController 2.2"
                                    (any (`elem` txInfoSignatories) boardMemberPKHs)

                    -----------------------------------------------------------------------------------------
                    --                                 ANNUAL ASSEMBLY ACTION                              --
                    -----------------------------------------------------------------------------------------

                    (AnnualAssemblyAction, ContractsControllerRefScriptDatum {})
                        |   ContractsControllerRefScriptDatum coreTeamCSs' digitalisBoardCSs' annualAssemblyDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ->
                                    traceIfFalse "ContractsController 1.1"
                                        (didDigitalisBoardMembersSinged digitalisBoardCSs)
                                &&  traceIfFalse "ContractsController 1.2"
                                        (any isBoardMemberCSPresent coreTeamCSs)
                                &&  traceIfFalse "ContractsController 1.3"
                                        (all isBoardMemberCSPresent digitalisBoardCSs')
                                &&  traceIfFalse "ContractsController 1.4"
                                        (all isBoardMemberCSPresent coreTeamCSs')
                                &&  traceIfFalse "ContractsController 1.5"
                                        (valueOf (txOutValue singleScriptOutputUTxO) currencySymbolOfCAT emptyTokenName == 1)
                                &&  traceIfFalse "ContractsController 1.6"
                                        (not (null digitalisBoardCSs'))
                                &&  traceIfFalse "ContractsController 1.7"
                                        (not (null coreTeamCSs'))
                                &&  traceIfFalse "ContractsController 1.8"
                                        (annualAssemblyDeadline + 31536000000 <= annualAssemblyDeadline')
                                &&  traceIfFalse "ContractsController 1.9"
                                        (from annualAssemblyDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "ContractsController 1.10"
                                        (isScriptCBORExistInTxOut (txOutReferenceScript singleScriptOutputUTxO))

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    (_,_) -> traceIfFalse "ContractsController 3.1" False

        |   otherwise = traceError "ContractsController 1"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            getContinuingOutputs :: [TxOut]
            getContinuingOutputs
                |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput = filter (f txOutAddress) txInfoOutputs
                |   otherwise = traceError "ContractsController 2"
                    where
                        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "ContractsController 3"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            emptyTokenName :: TokenName
            emptyTokenName = TokenName emptyByteString

            isOwnSH :: Address -> Bool
            isOwnSH (Address (ScriptCredential sh) _) = sh == ownHash
            isOwnSH _                                 = False

            isScriptCBORExistInTxOut :: Maybe ScriptHash -> Bool
            isScriptCBORExistInTxOut (Just sh)  = sh == ownHash
            isScriptCBORExistInTxOut _          = False

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find  (   \i ->

                                                isOwnSH (txOutAddress (txInInfoResolved i))
                                            &&
                                                valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT emptyTokenName == 1
                                            &&
                                                isScriptCBORExistInTxOut (txOutReferenceScript (txInInfoResolved i))
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "ContractsController 4"


            getTxOutInlineDatum :: TxOut -> ContractsControllerDatum
            getTxOutInlineDatum tx
                |   ( OutputDatum ( Datum inline )) <- txOutDatum tx
                =   unsafeFromBuiltinData @ContractsControllerDatum inline
                |   otherwise = traceError "ContractsController 5"

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput = txInInfoResolved i
                |   otherwise   =   traceError "ContractsController 6"

            singleScriptOutputUTxO :: TxOut
            singleScriptOutputUTxO
                |   [o]         <-  getContinuingOutputs = o
                |   otherwise   =   traceError "ContractsController 7"

            isBoardMemberCSPresent :: CurrencySymbol -> Bool
            isBoardMemberCSPresent cs
                |   Just i <- find (\i ->       valueOf (txOutValue (txInInfoResolved i)) cs emptyTokenName == 1
                                            &&
                                                isOwnSH (txOutAddress (txInInfoResolved i))

                                    ) txInfoReferenceInputs
                ,   BoardMemberDatum boardMemberPKHs <- getTxOutInlineDatum (txInInfoResolved i)
                =   any (`elem` txInfoSignatories) boardMemberPKHs
                |   otherwise = False

            didDigitalisBoardMembersSinged ::  [CurrencySymbol] -> Bool
            didDigitalisBoardMembersSinged [] = True
            didDigitalisBoardMembersSinged csList
                |   length csList < 3 = all isBoardMemberCSPresent csList
                |   length csList >= 3 =
                        let numOfSignatories = length (filter (== True) (map isBoardMemberCSPresent csList))
                        in numOfSignatories >= (length csList `divideInteger` 2) + 1
                |   otherwise = False

            isMember ::  [CurrencySymbol] -> Bool
            isMember [] = False
            isMember csList =
                any (\(cs,_,_) -> cs `elem` csList) (flattenValue (txOutValue singleScriptInputUTxO))


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: ContractsControllerParams -> BuiltinData -> BuiltinUnit
mkUntypedValidator params ctx
    |   mkValidator
            params
            (lazyDatum ctx)
            (lazyRedeemer ctx)
            (lazyTxOutRef ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoValidRange ctx)
            (lazyTxInfoSignatories ctx)
            (lazyTxInfoReferenceInputs ctx)
            = unitval
    |   otherwise = error()

compiledCode :: ContractsControllerParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
