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

module Marketplace.MintingDNSAuction.Validator
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
                                                           Redeemer (..),
                                                           PubKeyHash,Map,
                                                            ScriptPurpose (..)
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
                                                           otherwise,filter,
                                                           traceError,
                                                           traceIfFalse, ($),
                                                           (&&), (<), (==))
import           Utils.String                             (integerAsBuiltinByteString)
import          PlutusTx.Builtins.Internal                  (appendByteString,blake2b_256,BuiltinUnit(..), unitval)
import          PlutusCore.Version                          (plcVersion110)
import Utils.LazyScriptContext ( lazyDatum,lazyTxInfoRedeemers, lazyTxOutRef,lazyTxInfoInputs, lazyRedeemer, lazyTxInfoOutputs, lazyTxInfoReferenceInputs, lazyTxInfoSignatories, lazyTxInfoValidRange)

import Orphans ()
import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           DNS.DRAT.Types                           (DRATAction (..))
import           Marketplace.MHABAT.Types                 (MHABATAction (..))
import           Marketplace.MintingDNSAuction.Types      (Bid,
                                                           MintingDNSAuctionAction (..),
                                                           MintingDNSAuctionDatum (..),
                                                           MintingDNSAuctionParams (..),
                                                           MintingDNSAuctionRefScriptDatum (..),
                                                           Salt, SecretBid)


{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  MintingDNSAuctionParams
    ->  MintingDNSAuctionDatum
    ->  MintingDNSAuctionAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Map ScriptPurpose Redeemer
    ->  Bool
mkValidator
    MintingDNSAuctionParams{..}
    datum
    redeemer
    ownTxOutRef
    txInfoInputs
    txInfoOutputs
    txInfoValidRange
    txInfoSignatories
    txInfoReferenceInputs
    txInfoRedeemers
        |   ContractRefScriptDatum refData <- getTxOutInlineDatum getScriptRefTxOut
        ,   MintingDNSAuctionRefScriptDatum domainName dnsCS currencySymbolOfDRAT currencySymbolOfMHABAT treasuryAddress treasuryDatum <- unsafeFromBuiltinData refData
        ,   traceIfFalse "MintingDNSAuction 1" (builtinDomainName == domainName)
            =
                case ( redeemer , datum ) of

                    -----------------------------------------------------------------------------------------
                    --                                   UPDATE BID ACTION                                 --
                    -----------------------------------------------------------------------------------------

                    ( UpdateBid
                        ,   BidDatum dnsTN dnsCS' _ bidderAddr revealingDeadline )
                            |   BidDatum dnsTN' dnsCS'' _ bidderAddr' revealingDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ,   AuctionDatum dnsTN'' dnsCS''' bidingDeadline _ _ _ _ _ <- getTxOutInlineDatum $ getRefUTxO dnsCS dnsTN
                                ->
                                        traceIfFalse "MintingDNSAuction 1.1"
                                            (doesSCUTxOsHaveNFT currencySymbolOfMHABAT dnsTN)
                                    &&  traceIfFalse "MintingDNSAuction 1.2"
                                            (dnsTN == dnsTN')
                                    &&  traceIfFalse "MintingDNSAuction 1.3"
                                            (dnsTN == dnsTN'')
                                    &&  traceIfFalse "MintingDNSAuction 1.4"
                                            (dnsCS == dnsCS')
                                    &&  traceIfFalse "MintingDNSAuction 1.5"
                                            (dnsCS == dnsCS'')
                                    &&  traceIfFalse "MintingDNSAuction 1.6"
                                            (dnsCS == dnsCS''')
                                    &&  traceIfFalse "MintingDNSAuction 1.7"
                                            (revealingDeadline == revealingDeadline')
                                    &&  traceIfFalse "MintingDNSAuction 1.8"
                                            (bidderAddr == bidderAddr')
                                    &&  traceIfFalse "MintingDNSAuction 1.9"
                                            isSCInputOutputCorrect
                                    &&  traceIfFalse "MintingDNSAuction 1.10"
                                            (txSignedByAddress bidderAddr)
                                    &&  traceIfFalse "MintingDNSAuction 1.11"
                                            (to bidingDeadline `contains` txInfoValidRange)


                    -----------------------------------------------------------------------------------------
                    --                                BID DISCLOSURE ACTION                                --
                    -----------------------------------------------------------------------------------------

                    ( BidDisclosure bid salt
                        ,   AuctionDatum dnsTN dnsCS' bidingDeadline revealingDeadline _ _ highestBidder highestBid )
                            |   AuctionDatum dnsTN' dnsCS'' bidingDeadline' revealingDeadline' _ _ highestBidder' highestBid' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ,   BidDatum dnsTN'' dnsCS''' secretBid bidderAddr _ <- getTxOutInlineDatum $ getRefUTxO currencySymbolOfMHABAT dnsTN
                                ->
                                        traceIfFalse "MintingDNSAuction 2.1"
                                            (doesSCUTxOsHaveNFT dnsCS dnsTN)
                                    &&  traceIfFalse "MintingDNSAuction 2.2"
                                            (dnsTN == dnsTN')
                                    &&  traceIfFalse "MintingDNSAuction 2.3"
                                            (dnsTN == dnsTN'')
                                    &&  traceIfFalse "MintingDNSAuction 2.4"
                                            (dnsCS == dnsCS')
                                    &&  traceIfFalse "MintingDNSAuction 2.5"
                                            (dnsCS == dnsCS'')
                                    &&  traceIfFalse "MintingDNSAuction 2.6"
                                            (dnsCS == dnsCS''')
                                    &&  traceIfFalse "MintingDNSAuction 2.7"
                                            (bidingDeadline == bidingDeadline')
                                    &&  traceIfFalse "MintingDNSAuction 2.8"
                                            (revealingDeadline == revealingDeadline')
                                    &&  traceIfFalse "MintingDNSAuction 2.9"
                                            (highestBid' == bid)
                                    &&  traceIfFalse "MintingDNSAuction 2.10"
                                            (highestBidder' == bidderAddr)
                                    &&  traceIfFalse "MintingDNSAuction 2.11"
                                            (from bidingDeadline `contains` txInfoValidRange)
                                    &&  traceIfFalse "MintingDNSAuction 2.12"
                                            (to revealingDeadline `contains` txInfoValidRange)
                                    &&  traceIfFalse "MintingDNSAuction 2.13"
                                            (txSignedByAddress bidderAddr)
                                    &&  traceIfFalse "MintingDNSAuction 2.14"
                                            (highestBid < highestBid')
                                    &&  traceIfFalse "MintingDNSAuction 2.15"
                                            (bidEvaluation secretBid bid salt)
                                    &&  traceIfFalse "MintingDNSAuction 2.16"
                                            (isSCOutputCorrect bid)
                                    &&  traceIfFalse "MintingDNSAuction 2.17"
                                            (isAddrGettingPaidExactly highestBidder highestBid)


                    -----------------------------------------------------------------------------------------
                    --                                  DNS REDEEMING ACTION                               --
                    -----------------------------------------------------------------------------------------

                    ( DNSRedeeming
                        ,   AuctionDatum dnsTN dnsCS' _ revealingDeadline _ _ highestBidder highestBid )
                        |   MintDRAT <- getCSRedeemer currencySymbolOfDRAT
                        ,   treasuryDatum' <- getTxOutInlineDatum $ getTreasuryTxOut treasuryAddress highestBid
                            ->
                                    traceIfFalse "MintingDNSAuction 3.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "MintingDNSAuction 3.2"
                                        (treasuryDatum == treasuryDatum')
                                &&  traceIfFalse "MintingDNSAuction 3.3"
                                        (from revealingDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "MintingDNSAuction 3.4"
                                        (valueOf (txOutValue singleScriptInputUTxO) dnsCS dnsTN == 1)
                                &&  traceIfFalse "MintingDNSAuction 3.5"
                                        (txSignedByAddress highestBidder)
                                &&  traceIfFalse "MintingDNSAuction 3.6"
                                        (any (\o -> txOutAddress o == highestBidder && valueOf (txOutValue o) dnsCS dnsTN == 1) txInfoOutputs)


                    -----------------------------------------------------------------------------------------
                    --                                  BID CLAIMING ACTION                                --
                    -----------------------------------------------------------------------------------------

                    ( BidClaiming
                        ,   BidDatum _ dnsCS' _ bidderAddr _ )
                        |   BurnMHABAT <- getCSRedeemer currencySymbolOfMHABAT
                            ->
                                    traceIfFalse "MintingDNSAuction 4.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "MintingDNSAuction 4.2"
                                        (isAddrGettingPaidExactly bidderAddr (adaAmountOf (txOutValue singleScriptInputUTxO)))

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    (_,_) -> traceIfFalse "MintingDNSAuction 5.1" False

        |   otherwise = traceError "MintingDNSAuction 2"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            getContinuingOutputs :: [TxOut]
            getContinuingOutputs
                |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput = filter (f txOutAddress) txInfoOutputs
                |   otherwise = traceError "MintingDNSAuction 2"
                    where
                        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "MintingDNSAuction 3"

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "MintingDNSAuction 3"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput = txInInfoResolved i
                |   otherwise   =   traceError "MintingDNSAuction 4"

            singleScriptOutputUTxO :: TxOut
            singleScriptOutputUTxO
                |   [o]         <-  getContinuingOutputs = o
                |   otherwise   =   traceError "MintingDNSAuction 5"

            adaAmountOf :: Value -> Integer
            adaAmountOf val = valueOf val adaSymbol adaToken

            getRefUTxO :: CurrencySymbol -> TokenName  -> TxOut
            getRefUTxO cs tn
                |   Just i  <-  find (  \i ->
                                            (   case txOutAddress (txInInfoResolved i) of
                                                    (Address (ScriptCredential sh) _) -> sh == ownHash
                                                    _ -> False
                                            )
                                        &&
                                            valueOf (txOutValue (txInInfoResolved i)) cs tn == 1
                                    )  txInfoReferenceInputs
                                = txInInfoResolved i
                |   otherwise   =   traceError "MintingDNSAuction 6"

            doesSCUTxOsHaveNFT :: CurrencySymbol -> TokenName -> Bool
            doesSCUTxOsHaveNFT cs tn =
                    valueOf (txOutValue singleScriptInputUTxO) cs tn == 1
                &&  valueOf (txOutValue singleScriptOutputUTxO) cs tn == 1

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "MintingDNSAuction 7"

            getScriptRefTxOut ::  TxOut
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
                |   otherwise = traceError "MintingDNSAuction 8"
                    where

                        scriptHashToScriptHash :: ScriptHash -> ScriptHash
                        scriptHashToScriptHash (ScriptHash hash) = ScriptHash hash

                        scriptHashToTokenName :: ScriptHash -> TokenName
                        scriptHashToTokenName (ScriptHash hash) = TokenName hash

            fromJust :: Maybe a -> a
            fromJust (Just x) = x
            fromJust _        = traceError "MintingDNSAuction 9"

            isSCInputOutputCorrect :: Bool
            isSCInputOutputCorrect =
                    txOutValue singleScriptOutputUTxO == txOutValue singleScriptInputUTxO

            bidEvaluation :: SecretBid -> Bid -> Salt -> Bool
            bidEvaluation secretBid bid salt =
                blake2b_256 (integerAsBuiltinByteString bid `appendByteString` salt) == secretBid

            isSCOutputCorrect :: Bid -> Bool
            isSCOutputCorrect revealedBid =
                adaAmountOf (txOutValue singleScriptOutputUTxO) == revealedBid

            isAddrGettingPaidExactly :: Address -> Bid -> Bool
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
                |   otherwise = traceError "MintingDNSAuction 10"

            getCSRedeemer :: forall rad. (UnsafeFromData rad) => CurrencySymbol -> rad
            getCSRedeemer cs
                |   Just rawMintingRedeemer <- lookup (Minting cs) txInfoRedeemers
                =   unsafeFromBuiltinData @rad $ getRedeemer rawMintingRedeemer
                |   otherwise = traceError "MintingDNSAuction 11"

{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
--                                    COMPILATION                                       --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: MintingDNSAuctionParams -> BuiltinData -> BuiltinUnit
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
            (lazyTxInfoRedeemers ctx)
            = unitval
    |   otherwise = error()


compiledCode :: MintingDNSAuctionParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
