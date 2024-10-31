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
module  DNS.DRAT.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            )
                where

import           PlutusLedgerApi.V1.Value                 (flattenValue,
                                                           symbols, valueOf)
import           PlutusLedgerApi.V3                         (
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                           ScriptHash (..),Address (..),
                                                           TxOut (..),CurrencySymbol (..),
                                                           OutputDatum (..),TokenName (..),
                                                           Value (..),
                                                           Redeemer (..),Map,
                                                           ScriptPurpose (..),
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,UnsafeFromData,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Prelude                         (Bool (..),
                                                           Maybe (..), elem,
                                                           error, filter, find,
                                                           length, null,
                                                           otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (==))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext (lazyTxInfoInputs,lazyOwnCurrencySymbol, lazyRedeemer, lazyTxInfoMint, lazyTxInfoOutputs, lazyTxInfoReferenceInputs,lazyTxInfoRedeemers)

import Orphans ()
import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           DNS.DNSReference.Types                   (DNSReferenceDatum (..))
import           DNS.DRAT.Types                           (DRATAction (..),
                                                           DRATParams (..),
                                                           DRATRefScriptDatum (..))
import           Marketplace.MintingDNSAuction.Types      (MintingDNSAuctionAction (..),
                                                           MintingDNSAuctionDatum (..))


{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy
    ::  DRATParams
    ->  DRATAction
    ->  CurrencySymbol
    ->  [TxInInfo]
    ->  [TxOut]
    ->  [TxInInfo]
    ->  Value
    ->  Map ScriptPurpose Redeemer
    ->  Bool
mkPolicy
    DRATParams{..}
    redeemer
    ownCurrencySymbol
    txInfoInputs
    txInfoOutputs
    txInfoReferenceInputs
    txInfoMint
    txInfoRedeemers
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   DRATRefScriptDatum domainName currencySymbolOfDNS auctionAddress dNSReferenceAddress <- unsafeFromBuiltinData refData
        ,   traceIfFalse "DRAT 1" (builtinDomainName == domainName)
            =
                case redeemer of

                    -----------------------------------------------------------------------------------------
                    --                                      MINT DRAT                                      --
                    -----------------------------------------------------------------------------------------

                    MintDRAT ->
                        let auctionTxIn = getAuctionTxIn currencySymbolOfDNS auctionAddress
                        in case getTxOutInlineDatum $ txInInfoResolved auctionTxIn of
                            AuctionDatum dnsTN dnsCS _ _ _ _ _ _
                                |   DNSReferenceDatum origin dnsRecords _ <- getTxOutInlineDatum $ getDNSReferenceTxOut dNSReferenceAddress dnsTN
                                ,   DNSRedeeming <- getAuctionRedeemer auctionTxIn
                                    ->      traceIfFalse "DRAT 1.1"
                                                (wasMintedExactly dnsTN)
                                        &&  traceIfFalse "DRAT 1.2"
                                                (origin == dnsTN)
                                        &&  traceIfFalse "DRAT 1.3"
                                                (currencySymbolOfDNS == dnsCS)
                                        &&  traceIfFalse "DRAT 1.4"
                                                (null dnsRecords)

                            _ -> traceIfFalse "DRAT 1.6" False

        |   otherwise = traceError "DRAT 2"

        where

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "DRAT 3"

            wasMintedExactly :: TokenName -> Bool
            wasMintedExactly tn =
                    length (filter (\(cs,tn',amt') -> cs == ownCurrencySymbol && tn' == tn && amt' == 1) (flattenValue txInfoMint)) == 1
                &&  length (flattenValue txInfoMint) == 1

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find (    \i ->
                                                (   case txOutAddress (txInInfoResolved i) of
                                                        (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                        _ -> False
                                                )
                                            &&
                                                fromJust (txOutReferenceScript (txInInfoResolved i)) == ownScriptHash
                                            &&
                                                valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (scriptHashToTokenName ownScriptHash) == 1
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "DRAT 4"
                        where
                            fromJust :: Maybe a -> a
                            fromJust (Just x) = x
                            fromJust _        = traceError "DRAT 5"

                            ownScriptHash :: ScriptHash
                            ownScriptHash = currencySymbolToScriptHash ownCurrencySymbol
                                where
                                    currencySymbolToScriptHash :: CurrencySymbol -> ScriptHash
                                    currencySymbolToScriptHash (CurrencySymbol hash) = ScriptHash hash

                            scriptHashToTokenName :: ScriptHash -> TokenName
                            scriptHashToTokenName (ScriptHash hash) = TokenName hash

            getDNSReferenceTxOut :: Address -> TokenName -> TxOut
            getDNSReferenceTxOut addr tn
                |   Just o <- find  (  \o ->
                                                valueOf (txOutValue o) ownCurrencySymbol tn == 1
                                            &&
                                                txOutAddress o == addr
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "DRAT 6"

            getAuctionTxIn :: CurrencySymbol -> Address -> TxInInfo
            getAuctionTxIn cs addr
                |   Just i <- find  (  \i ->
                                                cs `elem` symbols (txOutValue (txInInfoResolved i))
                                            &&
                                                txOutAddress (txInInfoResolved i) == addr
                                    )
                                    txInfoInputs = i
                |   otherwise = traceError "DRAT 7"

            getAuctionRedeemer :: TxInInfo -> MintingDNSAuctionAction
            getAuctionRedeemer tx
                |   Just rawMintingRedeemer <- lookup (Spending (txInInfoOutRef tx)) txInfoRedeemers
                =   unsafeFromBuiltinData @MintingDNSAuctionAction $ getRedeemer rawMintingRedeemer
                |   otherwise = traceError "DRAT 8"

{-============================================== END OF MINTING POLICY SECTION =================================================-}

-------------------------------------------------------------------------------------------
--                                     COMPILATION                                       --
-------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: DRATParams -> BuiltinData -> BuiltinUnit
mkUntypedPolicy params ctx
    |   mkPolicy
            params
            (lazyRedeemer ctx)
            (lazyOwnCurrencySymbol ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoReferenceInputs ctx)
            (lazyTxInfoMint ctx)
            (lazyTxInfoRedeemers ctx)
            = unitval
    |   otherwise = error()

compiledCode :: DRATParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedPolicy||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params