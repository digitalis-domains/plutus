module Utils.ExBudget
  ( scriptExBudget,
    getCPUExBudget,
    getMemoryExBudget,
    plutusCore,
    conwayPV,
  )
where

import Codec.Serialise (serialise)
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (WriterT (..), runWriterT)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short
  ( ShortByteString,
    toShort,
  )
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import PlutusCore.Default (BuiltinSemanticsVariant (..), DefaultFun)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParamsForVariant)
import PlutusCore.Evaluation.Machine.ExMemory (CostingInteger)
import PlutusLedgerApi.V3 (CostModelParams, Data (..), ExBudget, ExCPU (..), ExMemory (..), MajorProtocolVersion (..), ScriptForEvaluation, SerialisedScript, VerboseMode (Verbose), evaluateScriptCounting, exBudgetCPU, exBudgetMemory, mkEvaluationContext)
import Prelude (Either (..), Maybe (..), String, error, fst, map, otherwise, snd, ($), (<$>))

conwayPV :: MajorProtocolVersion
conwayPV = MajorProtocolVersion 9

plutusCore :: SerialisedScript -> ShortByteString
plutusCore scr = toShort $ toStrict $ serialise scr

builtinSemanticsVariantReader :: String -> Maybe (BuiltinSemanticsVariant DefaultFun)
builtinSemanticsVariantReader =
  \case
    "A" -> Just DefaultFunSemanticsVariantA
    "B" -> Just DefaultFunSemanticsVariantB
    "C" -> Just DefaultFunSemanticsVariantC
    _ -> Nothing

flattenCostModel :: CostModelParams -> [Int64]
flattenCostModel cm = map snd (sortOn fst (Map.toList cm))

scriptExBudget :: String -> Data -> ScriptForEvaluation -> ExBudget
scriptExBudget strVar d scr
  | Just semVar <- builtinSemanticsVariantReader strVar,
    Just costModel <- defaultCostModelParamsForVariant semVar,
    let contextOrError = runExceptT $ fst <$> runWriterT (mkEvaluationContext (flattenCostModel costModel)),
    Right evaluationContext <- contextOrError,
    ec <- case evaluationContext of
      Right ec -> ec
      _ -> error "evaluationContext Failed",
    ([], Right exB) <- evaluateScriptCounting conwayPV Verbose ec scr d =
    exB
  | otherwise = error "scriptExBudget Failed"

getCPUExBudget :: String -> Data -> ScriptForEvaluation -> CostingInteger
getCPUExBudget strVar d scr = case exBudgetCPU (scriptExBudget strVar d scr) of ExCPU unit -> unit

getMemoryExBudget :: String -> Data -> ScriptForEvaluation -> CostingInteger
getMemoryExBudget strVar d scr = case exBudgetMemory (scriptExBudget strVar d scr) of ExMemory unit -> unit
