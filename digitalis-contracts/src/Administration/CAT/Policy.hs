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

module Administration.CAT.Policy
  ( mkPolicy,
    mkUntypedPolicy,
    compiledCode,
  )
where

import Administration.CAT.Types
  ( CATAction (..),
    CATParams (..),
  )
import Administration.ContractsController.Types (ContractsControllerDatum (..))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V1.Interval
  ( Extended (..),
    Interval (..),
    UpperBound (..),
    contains,
    to,
  )
import PlutusLedgerApi.V1.Value
  ( flattenValue,
    valueOf,
  )
import PlutusLedgerApi.V3
  ( CurrencySymbol (..),
    Datum (..),
    OutputDatum (..),
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash,
    TokenName (..),
    TxInInfo (..),
    TxOut (..),
    Value,
  )
import PlutusTx
  ( BuiltinData,
    CompiledCode,
    compile,
    liftCode,
    unsafeApplyCode,
    unsafeFromBuiltinData,
  )
import PlutusTx.Builtins
  ( divideInteger,
    emptyByteString,
  )
import PlutusTx.Builtins.Internal (BuiltinUnit, unitval)
import PlutusTx.Prelude
  ( Bool (..),
    Maybe (..),
    all,
    any,
    elem,
    error,
    filter,
    find,
    length,
    map,
    not,
    otherwise,
    traceError,
    traceIfFalse,
    (&&),
    (+),
    (<),
    (<=),
    (==),
    (>=),
  )
import Utils.LazyScriptContext (lazyOwnCurrencySymbol, lazyRedeemer, lazyTxInfoMint, lazyTxInfoOutputs, lazyTxInfoReferenceInputs, lazyTxInfoSignatories, lazyTxInfoValidRange)

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINEABLE mkPolicy #-}
mkPolicy ::
  CATParams ->
  CATAction ->
  CurrencySymbol ->
  POSIXTimeRange ->
  [PubKeyHash] ->
  [TxOut] ->
  [TxInInfo] ->
  Value ->
  Bool
mkPolicy
  CATParams {..}
  redeemer
  ownCurrencySymbol
  txInfoValidRange
  txInfoSignatories
  txInfoOutputs
  txInfoReferenceInputs
  txInfoMint =
    case redeemer of
      -----------------------------------------------------------------------------------------
      --                             DIGITALIS BOARD INCEPTION                               --
      -----------------------------------------------------------------------------------------

      DigitalisBoardInception
        | ContractsControllerRefScriptDatum coreTeamCSs digitalisBoard annualAssemblyDeadline <- getTxOutInlineDatum getDigitalisBoardTxOut ->
          traceIfFalse
            "CAT 1.1"
            (to initializationDeadline `contains` txInfoValidRange)
            && traceIfFalse
              "CAT 1.2"
              (all isBoardMemberCSPresent digitalisBoard)
            && traceIfFalse
              "CAT 1.3"
              (all isBoardMemberCSPresent coreTeamCSs)
            && traceIfFalse
              "CAT 1.4"
              (getCurrentTime + 31536000000 <= annualAssemblyDeadline)
            && traceIfFalse
              "CAT 1.5"
              wasMintedExactly
      -----------------------------------------------------------------------------------------
      --                               DIGITALIS BOARD ACTION                                --
      -----------------------------------------------------------------------------------------

      DigitalisBoardAction
        | ContractsControllerRefScriptDatum coreTeamCSs digitalisBoard annualAssemblyDeadline <- getTxOutInlineDatum getDigitalisBoardRefUTxO ->
          traceIfFalse
            "CAT 2.1"
            (didDigitalisBoardMembersSinged digitalisBoard)
            && traceIfFalse
              "CAT 2.2"
              (any isBoardMemberCSPresent coreTeamCSs)
            && traceIfFalse
              "CAT 2.3"
              (not isEmptyTokenMinting)
            && traceIfFalse
              "CAT 2.4"
              (to annualAssemblyDeadline `contains` txInfoValidRange)

      -----------------------------------------------------------------------------------------
      --                                   ANY OTHER CASE                                    --
      -----------------------------------------------------------------------------------------

      _ -> traceIfFalse "CAT 1" False
    where
      emptyTokenName :: TokenName
      emptyTokenName = TokenName emptyByteString

      getDigitalisBoardTxOut :: TxOut
      getDigitalisBoardTxOut
        | Just o <-
            find
              ( \o ->
                  valueOf (txOutValue o) ownCurrencySymbol emptyTokenName == 1
              )
              txInfoOutputs =
          o
        | otherwise = traceError "CAT 2"

      getDigitalisBoardRefUTxO :: TxOut
      getDigitalisBoardRefUTxO
        | Just i <-
            find
              ( \i ->
                  valueOf (txOutValue (txInInfoResolved i)) ownCurrencySymbol emptyTokenName == 1
              )
              txInfoReferenceInputs =
          txInInfoResolved i
        | otherwise = traceError "CAT 3"

      getTxOutInlineDatum :: TxOut -> ContractsControllerDatum
      getTxOutInlineDatum tx
        | (OutputDatum (Datum inline)) <- txOutDatum tx =
          unsafeFromBuiltinData @ContractsControllerDatum inline
        | otherwise = traceError "CAT 4"

      isBoardMemberCSPresent :: CurrencySymbol -> Bool
      isBoardMemberCSPresent cs
        | Just i <- find (\i -> valueOf (txOutValue (txInInfoResolved i)) cs emptyTokenName == 1) txInfoReferenceInputs,
          BoardMemberDatum boardMemberPKHs <- getTxOutInlineDatum (txInInfoResolved i) =
          any (`elem` txInfoSignatories) boardMemberPKHs
        | otherwise = False

      didDigitalisBoardMembersSinged :: [CurrencySymbol] -> Bool
      didDigitalisBoardMembersSinged [] = False
      didDigitalisBoardMembersSinged csList
        | length csList < 3 = all isBoardMemberCSPresent csList
        | length csList >= 3 =
          let numOfSignatories = length (filter (== True) (map isBoardMemberCSPresent csList))
           in numOfSignatories >= (length csList `divideInteger` 2) + 1
        | otherwise = False

      wasMintedExactly :: Bool
      wasMintedExactly =
        length (filter (\(cs, tn, amt) -> cs == ownCurrencySymbol && amt == 1 && tn == emptyTokenName) (flattenValue txInfoMint)) == 1
          && length (flattenValue txInfoMint) == 1

      isEmptyTokenMinting :: Bool
      isEmptyTokenMinting =
        any (\(cs, tn, _) -> cs == ownCurrencySymbol && tn == emptyTokenName) (flattenValue txInfoMint)

      getCurrentTime :: POSIXTime
      getCurrentTime =
        case ivTo txInfoValidRange of
          UpperBound (Finite t) _ -> t
          _ -> traceError "CAT 4"

-- getCurrentTime_2 :: POSIXTime
-- getCurrentTime_2 =
--     case ivFrom txInfoValidRange of
--         LowerBound (Finite t) _ -> t
--         _                       -> traceError "CAT 4"

{-============================================== END OF MINTING POLICY SECTION =================================================-}

------------------------------------------------------------------------------------------

-- |                                  COMPILATION                                     | --

------------------------------------------------------------------------------------------

{-# INLINEABLE mkUntypedPolicy #-}
mkUntypedPolicy :: CATParams -> BuiltinData -> BuiltinUnit
mkUntypedPolicy params ctx
  | mkPolicy
      params
      (lazyRedeemer ctx)
      (lazyOwnCurrencySymbol ctx)
      (lazyTxInfoValidRange ctx)
      (lazyTxInfoSignatories ctx)
      (lazyTxInfoOutputs ctx)
      (lazyTxInfoReferenceInputs ctx)
      (lazyTxInfoMint ctx) =
    unitval
  | otherwise = error ()

compiledCode :: CATParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
  $$(PlutusTx.compile [||mkUntypedPolicy||])
    `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
