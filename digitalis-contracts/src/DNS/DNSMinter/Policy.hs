
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

module  DNS.DNSMinter.Policy
            (   mkPolicy
            ,   mkUntypedPolicy
            ,   compiledCode
            )
                where


import           PlutusLedgerApi.V1.Interval              (contains, to)
import           PlutusLedgerApi.V1.Value                 (
                                                           adaSymbol, adaToken,
                                                           flattenValue,
                                                           valueOf)
import           PlutusLedgerApi.V3                         (
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                           ScriptHash (..),Address (..),
                                                           TxOut (..),CurrencySymbol (..),
                                                           OutputDatum (..),TokenName (..),
                                                           Value (..),POSIXTimeRange,
                                                           PubKeyHash,
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,
                                                           compile,UnsafeFromData,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), elem,
                                                           error, filter, find,
                                                           length, otherwise,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (<), (==))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext (lazyOwnCurrencySymbol, lazyRedeemer, lazyTxInfoMint, lazyTxInfoOutputs,lazyTxInfoValidRange, lazyTxInfoReferenceInputs, lazyTxInfoSignatories)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           DNS.DNSMinter.Types                      (DNSMinterAction (..),
                                                           DNSMinterParams (..),
                                                           DNSMinterRefScriptDatum (..))
import           Marketplace.MintingDNSAuction.Types      (MintingDNSAuctionDatum (..))

{-==============================================================================================================================-}
{-==========                                              POLICY SECTION                                              ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkPolicy #-}
mkPolicy
    ::  DNSMinterParams
    ->  DNSMinterAction
    ->  CurrencySymbol
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Value
    ->  Bool
mkPolicy
    DNSMinterParams{..}
    redeemer
    ownCurrencySymbol
    txInfoOutputs
    txInfoValidRange
    txInfoSignatories
    txInfoReferenceInputs
    txInfoMint
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   DNSMinterRefScriptDatum domainName dNSAdminPKH auctionAddress treasuryAddress treasuryDatum <- unsafeFromBuiltinData refData
        ,   traceIfFalse "DNSMinter 1" (builtinDomainName == domainName)
            =
                case redeemer of

                    -----------------------------------------------------------------------------------------
                    --                                      MINT DNS                                       --
                    -----------------------------------------------------------------------------------------

                    MintDNS dnsTokenName
                        |   AuctionDatum dnsTN dnsCS bidingDeadline revealingDeadline _ biddingFee highestBidder highestBid <- getTxOutInlineDatum $ getAuctionTxOut auctionAddress (TokenName dnsTokenName)
                        ,   treasuryDatum' <- getTxOutInlineDatum $ getTreasuryTxOut treasuryAddress biddingFee
                            ->
                                let auctionTxOutADA = adaAmountOf (txOutValue (getAuctionTxOut auctionAddress (TokenName dnsTokenName)))
                                in  traceIfFalse "DNSMinter 1.1"
                                        (dNSAdminPKH `elem` txInfoSignatories)
                                &&  traceIfFalse "DNSMinter 1.2"
                                        (txSignedByAddress highestBidder)
                                &&  traceIfFalse "DNSMinter 1.3"
                                        (to bidingDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "DNSMinter 1.4"
                                        (bidingDeadline < revealingDeadline)
                                &&  traceIfFalse "DNSMinter 1.5"
                                        (isRefInputsLengthEqualTo 1)
                                &&  traceIfFalse "DNSMinter 1.6"
                                        (TokenName dnsTokenName == dnsTN)
                                &&  traceIfFalse "DNSMinter 1.7"
                                        (ownCurrencySymbol == dnsCS)
                                &&  traceIfFalse "DNSMinter 1.8"
                                        (wasMintedBurntExactly dnsTN 1 1)
                                &&  traceIfFalse "DNSMinter 1.9"
                                        (auctionTxOutADA == highestBid)
                                &&  traceIfFalse "DNSMinter 1.10"
                                        (treasuryDatum == treasuryDatum')

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    _ -> traceIfFalse "DNSMinter 2.1" False

        |   otherwise = traceError "DNSMinter 2"

        where

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "DNSMinter 3"

            fromJust :: Maybe a -> a
            fromJust (Just x) = x
            fromJust _        = traceError "DNSMinter 4"

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
                |   otherwise = traceError "DNSMinter 5"

                    where

                        currencySymbolToScriptHash :: CurrencySymbol -> ScriptHash
                        currencySymbolToScriptHash (CurrencySymbol hash) = ScriptHash hash

                        currencySymbolToTokenName :: CurrencySymbol -> TokenName
                        currencySymbolToTokenName (CurrencySymbol hash) = TokenName hash

            isRefInputsLengthEqualTo :: Integer -> Bool
            isRefInputsLengthEqualTo int = length txInfoReferenceInputs == int

            adaAmountOf :: Value -> Integer
            adaAmountOf val = valueOf val adaSymbol adaToken

            getAuctionTxOut :: Address -> TokenName -> TxOut
            getAuctionTxOut addr tn
                |   Just o <- find  (  \o ->
                                                valueOf (txOutValue o) ownCurrencySymbol tn == 1
                                            &&
                                                txOutAddress o == addr
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "DNSMinter 6"

            wasMintedBurntExactly ::  TokenName -> Integer -> Integer-> Bool
            wasMintedBurntExactly tn amt len =
                    length (filter (\(cs,tn',amt') -> cs == ownCurrencySymbol && tn == tn' && amt == amt') (flattenValue txInfoMint)) == 1
                &&  length (flattenValue txInfoMint) == len

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "DNSMinter 7"

            getTreasuryTxOut :: Address -> Integer -> TxOut
            getTreasuryTxOut addr amt
                |   Just o <- find  (  \o ->

                                                txOutAddress o == addr
                                            &&
                                                adaAmountOf (txOutValue o) == amt
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "DNSMinter 8"


{-============================================== END OF MINTING POLICY SECTION =================================================-}

-------------------------------------------------------------------------------------------
--                                     COMPILATION                                       --
-------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedPolicy #-}
mkUntypedPolicy :: DNSMinterParams -> BuiltinData -> BuiltinUnit
mkUntypedPolicy params ctx
    |   mkPolicy
            params
            (lazyRedeemer ctx)
            (lazyOwnCurrencySymbol ctx)
            (lazyTxInfoOutputs ctx)
            (lazyTxInfoValidRange ctx)
            (lazyTxInfoSignatories ctx)
            (lazyTxInfoReferenceInputs ctx)
            (lazyTxInfoMint ctx)
            = unitval
    |   otherwise = error()

compiledCode :: DNSMinterParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedPolicy||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params