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

module DNS.DNSReference.Validator
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
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,UnsafeFromData,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Builtins                        (emptyByteString)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), all,
                                                            error, find,
                                                           not, null, otherwise,filter,
                                                           traceError,
                                                           traceIfFalse,
                                                           (&&), (/=), (==))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext ( lazyDatum, lazyTxOutRef,lazyTxInfoInputs, lazyRedeemer, lazyTxInfoOutputs, lazyTxInfoReferenceInputs)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           DNS.DNSReference.Types                   (DNSRecord (..),
                                                           DNSReferenceAction (..),
                                                           DNSReferenceDatum (..),
                                                           DNSReferenceParams (..),
                                                           DNSReferenceRefScriptDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  DNSReferenceParams
    ->  DNSReferenceDatum
    ->  DNSReferenceAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [TxOut]
    ->  [TxInInfo]
    -> Bool
mkValidator
    DNSReferenceParams{..}
    datum
    redeemer
    ownTxOutRef
    txInfoInputs
    txInfoOutputs
    txInfoReferenceInputs

        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   DNSReferenceRefScriptDatum domainName currencySymbolOfDRAT currencySymbolOfDNS <- unsafeFromBuiltinData refData
            =
                case ( redeemer, datum ) of

                    -----------------------------------------------------------------------------------------
                    --                                    UPDATE ACTION                                    --
                    -----------------------------------------------------------------------------------------

                    ( Update ,
                        DNSReferenceDatum origin _ _)
                            |   DNSReferenceDatum origin' dnsRecords' _ <- getTxOutInlineDatum singleScriptOutputUTxO
                                ->      traceIfFalse "DNSReference 1.1"
                                            (builtinDomainName == domainName)
                                    &&  traceIfFalse "DNSReference 1.2"
                                            (origin == origin')
                                    &&  traceIfFalse "DNSReference 1.3"
                                            (evaluationOfDART currencySymbolOfDRAT origin)
                                    &&  traceIfFalse "DNSReference 1.4"
                                            (evaluationOfDNS currencySymbolOfDNS origin)
                                    &&  traceIfFalse "DNSReference 1.5"
                                            (case dnsRecords' of
                                                dnsRecords'' | null dnsRecords'' -> False
                                                dnsRecords'' | not (null dnsRecords'') -> recordsValidation dnsRecords''
                                                _ -> False
                                            )

        |   otherwise = traceError "DNSReference 1"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            getContinuingOutputs :: [TxOut]
            getContinuingOutputs
                |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput = filter (f txOutAddress) txInfoOutputs
                |   otherwise = traceError "DNSReference 2"
                    where
                        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "DNSReference 3"

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                |   ( OutputDatum ( Datum inline )) <- txOutDatum tx
                =   unsafeFromBuiltinData @dat inline
                |   otherwise = traceError "DNSReference 4"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput = txInInfoResolved i
                |   otherwise   =   traceError "DNSReference 5"

            singleScriptOutputUTxO :: TxOut
            singleScriptOutputUTxO
                |   [o]         <-  getContinuingOutputs = o
                |   otherwise   =   traceError "DNSReference 6"

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
                |   otherwise = traceError "DNSReference 7"
                        where
                            fromJust :: Maybe a -> a
                            fromJust (Just x) = x
                            fromJust _        = traceError "DNSReference 8"

                            scriptHashToTokenName :: ScriptHash -> TokenName
                            scriptHashToTokenName (ScriptHash hash) = TokenName hash

            evaluationOfDNS :: CurrencySymbol -> TokenName -> Bool
            evaluationOfDNS cs tn
                |   Just i <- find ( \i -> valueOf (txOutValue (txInInfoResolved i)) cs tn == 1) txInfoInputs
                ,   Just o <- find ( \o -> valueOf (txOutValue o) cs tn == 1) txInfoOutputs
                =   txOutAddress (txInInfoResolved i) == txOutAddress o
                |   otherwise = traceError "DNSReference 9"

            evaluationOfDART :: CurrencySymbol -> TokenName -> Bool
            evaluationOfDART cs tn =
                    valueOf (txOutValue singleScriptInputUTxO) cs tn == 1
                &&  valueOf (txOutValue singleScriptOutputUTxO) cs tn == 1

            recordsValidation :: [DNSRecord] -> Bool
            recordsValidation recordsList =
                all (   \record ->      (emptyByteString /= lhs record)
                                    &&  (   case ttl record of
                                                Nothing  -> True
                                                Just num -> num /= 0
                                        )
                                    &&  (emptyByteString /= rtype record)
                                    &&  (emptyByteString /= rdata record)
                    )   recordsList


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
-- |                                  COMPILATION                                     | --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: DNSReferenceParams -> BuiltinData -> BuiltinUnit
mkUntypedValidator params ctx
    |   mkValidator
            params
            (lazyDatum ctx)
            (lazyRedeemer ctx)
            (lazyTxOutRef ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoReferenceInputs ctx)
            = unitval
    |   otherwise = error()

compiledCode :: DNSReferenceParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
