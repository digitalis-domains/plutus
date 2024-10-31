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

module Marketplace.EnglishAuction.Validator
    (   mkValidator
    ,   mkUntypedValidator
    ,   compiledCode
    )   where

import           Data.Maybe                               (fromMaybe)
import           PlutusLedgerApi.V1.Interval              (contains, from, to)
import           PlutusLedgerApi.V1.Value                 (
                                                           adaSymbol, adaToken)
import           PlutusLedgerApi.V3                         (
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                           ScriptHash (..),Address (..),
                                                           TxOut (..),CurrencySymbol (..),
                                                           OutputDatum (..),TokenName (..),
                                                           TxOutRef(..),Value (..),POSIXTimeRange,
                                                           PubKeyHash,
                                                           )
import           PlutusTx                                 (BuiltinData,unsafeApplyCode,
                                                           CompiledCode,UnsafeFromData,
                                                           compile,
                                                           liftCode,
                                                           unsafeFromBuiltinData)
import           PlutusTx.AssocMap                        (lookup)
import           PlutusTx.Prelude                         (Bool (..), Integer,
                                                           Maybe (..), any,
                                                           elem, error, find,
                                                           otherwise,
                                                           traceError,filter,
                                                           traceIfFalse, ($),
                                                           (&&), (*), (+), (-),
                                                           (<=), (==), (>),
                                                           (||))
import          PlutusTx.Builtins.Internal                  (BuiltinUnit(..), unitval, divideInteger)
import          PlutusCore.Version                          (plcVersion110)
import          Utils.LazyScriptContext                     ( lazyDatum,
                                                                lazyTxOutRef,
                                                                lazyTxInfoInputs,
                                                                lazyRedeemer,
                                                                lazyTxInfoOutputs,
                                                                lazyTxInfoReferenceInputs,
                                                                lazyTxInfoSignatories,
                                                                lazyTxInfoValidRange)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Marketplace.EnglishAuction.Types         (EnglishAuctionAction (..),
                                                           EnglishAuctionDatum (..),
                                                           EnglishAuctionParams (..),
                                                           EnglishAuctionRefScriptDatum (..),
                                                           MarketplaceInfo (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  EnglishAuctionParams
    ->  EnglishAuctionDatum
    ->  EnglishAuctionAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Bool
mkValidator
    EnglishAuctionParams{..}
    datum
    redeemer
    ownTxOutRef
    txInfoInputs
    txInfoOutputs
    txInfoValidRange
    txInfoSignatories
    txInfoReferenceInputs
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   EnglishAuctionRefScriptDatum domainName dnsCS bidDiff MarketplaceInfo{..} treasuryAddress treasuryDatum <- unsafeFromBuiltinData refData
        ,   traceIfFalse "EnglishAuction 1" (builtinDomainName == domainName)
            =
                case ( redeemer , datum ) of

                    -----------------------------------------------------------------------------------------
                    --                                    BIDDING ACTION                                   --
                    -----------------------------------------------------------------------------------------

                    ( Bidding
                        ,   AuctionDatum dnsTN dnsCS' bidingDeadline owner highestBidder highestBid )
                            |   AuctionDatum dnsTN' dnsCS'' bidingDeadline' owner' highestBidder' highestBid' <- getTxOutInlineDatum singleScriptOutputUTxO
                                ->  let newBid      = adaAmountOf (txOutValue singleScriptOutputUTxO)
                                        bidderAddr  = fromJust highestBidder'
                                    in  traceIfFalse "EnglishAuction 1.1"
                                            (doesSCUTxOsHaveNFT dnsCS dnsTN)
                                    &&  traceIfFalse "EnglishAuction 1.2"
                                            (       (dnsTN == dnsTN')
                                                &&  (dnsCS == dnsCS')
                                                &&  (dnsCS == dnsCS'')
                                                &&  (bidingDeadline == bidingDeadline')
                                                &&  (owner       == owner')
                                                &&  (highestBid' == newBid)
                                            )
                                    &&  traceIfFalse "EnglishAuction 1.3"
                                            (to bidingDeadline `contains` txInfoValidRange)
                                    &&  traceIfFalse "EnglishAuction 1.4"
                                            (txSignedByAddress bidderAddr)
                                    &&  traceIfFalse "EnglishAuction 1.5"
                                            (   case (highestBidder, highestBid) of
                                                    (Nothing, bid')     ->  bid' + bidDiff <= highestBid'
                                                    (Just addr, bid')   ->      isAddrGettingPaidExactly addr bid'
                                                                            &&  bid' + bidDiff <= highestBid'
                                            )

                    -----------------------------------------------------------------------------------------
                    --                                  DNS REDEEMING ACTION                               --
                    -----------------------------------------------------------------------------------------

                    ( DNSRedeeming
                        ,   AuctionDatum dnsTN dnsCS' bidingDeadline owner highestBidder highestBid )
                            ->
                                let bidderAddr = fromJust highestBidder
                                    digitalisFee = calculateFee precision minFee feePercentage tradeThreshold highestBid
                                    treasuryDatum' = getTxOutInlineDatum $ getTreasuryTxOut treasuryAddress digitalisFee
                                in  traceIfFalse "EnglishAuction 2.1"
                                        (       (dnsCS == dnsCS')
                                            &&  (treasuryDatum == treasuryDatum')
                                        )
                                &&  traceIfFalse "EnglishAuction 2.2"
                                        (from bidingDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "EnglishAuction 2.3"
                                        (valueOf (txOutValue singleScriptInputUTxO) dnsCS dnsTN == 1)
                                &&  traceIfFalse "EnglishAuction 2.4"
                                        (       txSignedByAddress bidderAddr
                                            ||
                                                txSignedByAddress owner
                                        )
                                &&  traceIfFalse "EnglishAuction 2.5"
                                        (isAddrGettingPaidExactly owner (highestBid - digitalisFee))


                    -----------------------------------------------------------------------------------------
                    --                                CANCEL AUCTION ACTION                                --
                    -----------------------------------------------------------------------------------------

                    ( CancelAuction
                        ,   AuctionDatum dnsTN dnsCS' _ owner Nothing _ )
                            ->
                                    traceIfFalse "EnglishAuction 3.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "EnglishAuction 3.2"
                                        (   any (  \o ->    txOutAddress o == owner
                                                        &&  txOutValue o == txOutValue singleScriptInputUTxO
                                                        &&  valueOf (txOutValue o) dnsCS' dnsTN == 1
                                                )   txInfoOutputs
                                        )

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    (_,_) -> traceIfFalse "EnglishAuction 4.1" False

        |   otherwise = traceError "EnglishAuction 2"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            getContinuingOutputs :: [TxOut]
            getContinuingOutputs
                |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput = filter (f txOutAddress) txInfoOutputs
                |   otherwise = traceError "EnglishAuction 2"
                    where
                        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "EnglishAuction 3"

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "EnglishAuction 3"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput = txInInfoResolved i
                |   otherwise   =   traceError "EnglishAuction 4"

            singleScriptOutputUTxO :: TxOut
            singleScriptOutputUTxO
                |   [o]         <-  getContinuingOutputs = o
                |   otherwise   =   traceError "EnglishAuction 5"

            adaAmountOf :: Value -> Integer
            adaAmountOf val = valueOf val adaSymbol adaToken

            doesSCUTxOsHaveNFT :: CurrencySymbol -> TokenName -> Bool
            doesSCUTxOsHaveNFT cs tn =
                    valueOf (txOutValue singleScriptInputUTxO) cs tn == 1
                &&  valueOf (txOutValue singleScriptOutputUTxO) cs tn == 1

            fromJust :: Maybe a -> a
            fromJust (Just x) = x
            fromJust _        = traceError "EnglishAuction 6"

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "EnglishAuction 7"

            getScriptRefTxOut :: TxOut
            getScriptRefTxOut
                |   Just i <- find  (   \i ->
                                            (   case txOutAddress (txInInfoResolved i) of
                                                    (Address (ScriptCredential sh) _) -> sh == contractsControllerSH
                                                    _ -> False
                                            )
                                        &&
                                            fromJust (txOutReferenceScript (txInInfoResolved i)) == scriptHashToScriptHash ownHash
                                        &&
                                            valueOf (txOutValue (txInInfoResolved i)) currencySymbolOfCAT (scriptHashToTokenName ownHash) == 1
                                    )
                                    txInfoReferenceInputs = txInInfoResolved i
                |   otherwise = traceError "EnglishAuction 8"
                    where

                        scriptHashToScriptHash :: ScriptHash -> ScriptHash
                        scriptHashToScriptHash (ScriptHash hash) = ScriptHash hash

                        scriptHashToTokenName :: ScriptHash -> TokenName
                        scriptHashToTokenName (ScriptHash hash) = TokenName hash

            isAddrGettingPaidExactly :: Address -> Integer -> Bool
            isAddrGettingPaidExactly addr int =
                any (\o -> txOutAddress o == addr && adaAmountOf (txOutValue o) == int) txInfoOutputs

            getTreasuryTxOut :: Address -> Integer -> TxOut
            getTreasuryTxOut addr amt
                |   Just o <- find  (  \o ->
                                                (txOutAddress o == addr)
                                            &&
                                                (adaAmountOf (txOutValue o) == amt)
                                    )
                                    txInfoOutputs = o
                |   otherwise = traceError "EnglishAuction 9"

            calculateFee :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
            calculateFee prs minFee feePct th amt
                |   amt <= th  = minFee
                |   amt > th = (amt * prs) `divideInteger` feePct
                |   otherwise = traceError "EnglishAuction 10"


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
--                                    COMPILATION                                       --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: EnglishAuctionParams -> BuiltinData -> BuiltinUnit
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


compiledCode :: EnglishAuctionParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params