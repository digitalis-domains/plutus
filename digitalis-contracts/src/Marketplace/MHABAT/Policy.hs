
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
module  Marketplace.MHABAT.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            )
                where

import           PlutusLedgerApi.V1.Interval              (contains, from, to)
import           PlutusLedgerApi.V1.Value                 (
                                                           adaSymbol, adaToken,
                                                           flattenValue,
                                                           symbols, valueOf)
import           PlutusLedgerApi.V3                        (CurrencySymbol (..),
                                                           TokenName (..),Credential (..),PubKeyHash,
                                                          Datum (..),Value,
                                                           Address (..),
                                                           ScriptHash (..),POSIXTimeRange,
                                                           TxInInfo (..),OutputDatum (..),
                                                           TxOut (..),
                                                           )
import           PlutusTx                                 (BuiltinData,
                                                            UnsafeFromData,
                                                            unsafeApplyCode,
                                                           CompiledCode,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), elem,
                                                           error, filter, find,
                                                           length, otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (==))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext (lazyOwnCurrencySymbol, lazyRedeemer, lazyTxInfoInputs, lazyTxInfoMint, lazyTxInfoOutputs,lazyTxInfoValidRange, lazyTxInfoReferenceInputs, lazyTxInfoSignatories)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Marketplace.MHABAT.Types                 (MHABATAction (..),
                                                           MHABATParams (..),
                                                           MHABATRefScriptDatum (..))
import           Marketplace.MintingDNSAuction.Types      (MintingDNSAuctionDatum (..))

{-==============================================================================================================================-}
{-==========                                          MINTING POLICY SECTION                                          ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy
    ::  MHABATParams
    ->  MHABATAction
    ->  CurrencySymbol
    ->  [TxInInfo]
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Value
    ->  Bool
mkPolicy
    MHABATParams{..}
    redeemer
    ownCurrencySymbol
    txInfoInputs
    txInfoOutputs
    txInfoValidRange
    txInfoSignatories
    txInfoReferenceInputs
    txInfoMint
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   MHABATRefScriptDatum domainName dnsCS auctionAddress treasuryAddress treasuryDatum <- unsafeFromBuiltinData refData
        ,   traceIfFalse "MHABAT 1" (builtinDomainName == domainName)
            =
                case redeemer of

                    -----------------------------------------------------------------------------------------
                    --                                    SECRET BIDDING                                   --
                    -----------------------------------------------------------------------------------------

                    MintMHABAT
                        |   AuctionDatum dnsTN dnsCS' bidingDeadline revealingDeadline seatCost biddingFee _ _ <- getTxOutInlineDatum $ getAuctionRefTxOut auctionAddress dnsCS
                        ,   BidDatum dnsTN' dnsCS'' _ bidderAddr revealingDeadline' <- getTxOutInlineDatum $ getAuctionTxOut auctionAddress dnsTN seatCost
                        ,   treasuryDatum' <- getTxOutInlineDatum $ getTreasuryTxOut treasuryAddress biddingFee
                            ->
                                    traceIfFalse "MHABAT 1.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "MHABAT 1.2"
                                        (dnsCS == dnsCS'')
                                &&  traceIfFalse "MHABAT 1.3"
                                        (revealingDeadline == revealingDeadline')
                                &&  traceIfFalse "MHABAT 1.4"
                                        (treasuryDatum == treasuryDatum')
                                &&  traceIfFalse "MHABAT 1.5"
                                        (dnsTN == dnsTN')
                                &&  traceIfFalse "MHABAT 1.6"
                                        (txSignedByAddress bidderAddr)
                                &&  traceIfFalse "MHABAT 1.7"
                                        (wasMintedBurntExactly dnsTN 1)
                                &&  traceIfFalse "MHABAT 1.8"
                                        (to bidingDeadline `contains` txInfoValidRange)

                    -----------------------------------------------------------------------------------------
                    --                                      BURN MHABAT                                    --
                    -----------------------------------------------------------------------------------------

                    BurnMHABAT
                        |   BidDatum dnsTN dnsCS' _ bidderAddr revealingDeadline <- getTxOutInlineDatum $ getAuctionTxIn auctionAddress
                            ->
                                    traceIfFalse "MHABAT 2.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "MHABAT 2.2"
                                        (wasMintedBurntExactly dnsTN (-1))
                                &&  traceIfFalse "MHABAT 2.3"
                                        (from revealingDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "MHABAT 2.4"
                                        (txSignedByAddress bidderAddr)

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    _ -> traceIfFalse "MHABAT 3.1" False

        |   otherwise = traceError "MHABAT 2"

        where

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "MHABAT 3"

            wasMintedBurntExactly :: TokenName -> Integer -> Bool
            wasMintedBurntExactly tn amt =
                    length (filter (\(cs,tn',amt') -> cs == ownCurrencySymbol && tn == tn' && amt == amt') (flattenValue txInfoMint)) == 1
                &&  length (flattenValue txInfoMint) == 1

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find  (   \i ->
                                            (   case txOutAddress (txInInfoResolved i) of
                                                    (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                    _ -> False
                                            )
                                        &&
                                            fromJust (txOutReferenceScript (txInInfoResolved i)) == currencySymbolToScriptHash ownCurrencySymbol
                                        &&
                                            valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (currencySymbolToTokenName ownCurrencySymbol) == 1
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "MHABAT 4"

                    where
                        fromJust :: Maybe a -> a
                        fromJust (Just x) = x
                        fromJust _        = traceError "MHABAT 5"

                        currencySymbolToScriptHash :: CurrencySymbol -> ScriptHash
                        currencySymbolToScriptHash (CurrencySymbol hash) = ScriptHash hash

                        currencySymbolToTokenName :: CurrencySymbol -> TokenName
                        currencySymbolToTokenName (CurrencySymbol hash) = TokenName hash

            getAuctionRefTxOut :: Address -> CurrencySymbol -> TxOut
            getAuctionRefTxOut addr cs
                |   Just i <- find  (   \i ->
                                            txOutAddress (txInInfoResolved i) == addr
                                        &&
                                            cs `elem` symbols (txOutValue (txInInfoResolved i))
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "MHABAT 6"

            adaAmountOf :: Value -> Integer
            adaAmountOf val = valueOf val adaSymbol adaToken

            getAuctionTxOut :: Address -> TokenName -> Integer -> TxOut
            getAuctionTxOut addr tn amt
                |   Just o <- find  (  \o ->
                                                valueOf (txOutValue o) ownCurrencySymbol tn == 1
                                            &&
                                                txOutAddress o == addr
                                            &&
                                                adaAmountOf (txOutValue o) == amt
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "MHABAT 7"

            getTreasuryTxOut :: Address -> Integer -> TxOut
            getTreasuryTxOut addr amt
                |   Just o <- find  (  \o ->
                                                (txOutAddress o == addr)
                                            &&
                                                (adaAmountOf (txOutValue o) == amt)
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "MHABAT 8"

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "MHABAT 9"

            getAuctionTxIn :: Address -> TxOut
            getAuctionTxIn addr
                |   Just i <- find  (  \i ->
                                                ownCurrencySymbol `elem` symbols (txOutValue (txInInfoResolved i))
                                            &&
                                                txOutAddress (txInInfoResolved i) == addr
                                    )
                                    txInfoInputs = txInInfoResolved i
                |   otherwise = traceError "MHABAT 10"

{-============================================== END OF MINTING POLICY SECTION =================================================-}

-------------------------------------------------------------------------------------------
-- |                                   COMPILATION                                     | --
-------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: MHABATParams -> BuiltinData -> BuiltinUnit
mkUntypedPolicy params ctx
    |   mkPolicy
            params
            (lazyRedeemer ctx)
            (lazyOwnCurrencySymbol ctx)
            (lazyTxInfoInputs ctx)
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoValidRange ctx)
            (lazyTxInfoSignatories ctx)
            (lazyTxInfoReferenceInputs ctx)
            (lazyTxInfoMint ctx)
            = unitval
    |   otherwise = error()

compiledCode :: MHABATParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedPolicy||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params