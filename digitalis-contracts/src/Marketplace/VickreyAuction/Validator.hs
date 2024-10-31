
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
module Marketplace.VickreyAuction.Validator
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
                                                           otherwise,
                                                           traceError,filter,
                                                           traceIfFalse, ($),
                                                           (&&), (*), (-), (<),
                                                           (<=), (==), (>),
                                                           (||))
import           Utils.String                             (integerAsBuiltinByteString)
import          PlutusTx.Builtins.Internal                  (appendByteString,blake2b_256,BuiltinUnit(..), unitval, divideInteger)
import          PlutusCore.Version                          (plcVersion110)
import          Utils.LazyScriptContext                     ( lazyDatum,
                                                                lazyTxOutRef,
                                                                lazyTxInfoInputs,
                                                                lazyRedeemer,
                                                                lazyTxInfoOutputs,
                                                                lazyTxInfoReferenceInputs,
                                                                lazyTxInfoSignatories,
                                                                lazyTxInfoValidRange,
                                                                lazyTxInfoRedeemers)

import           Administration.ContractsController.Types (ContractsControllerDatum (..))
import           Marketplace.VABAT.Types                  (VABATAction (..))
import           Marketplace.VickreyAuction.Types         (Bid,
                                                           MarketplaceInfo (..),
                                                           Salt, SecretBid,
                                                           VickreyAuctionAction (..),
                                                           VickreyAuctionDatum (..),
                                                           VickreyAuctionParams (..),
                                                           VickreyAuctionRefScriptDatum (..))

{-==============================================================================================================================-}
{-==========                                            VALIDATOR SECTION                                             ==========-}
{-==============================================================================================================================-}

{-# INLINABLE mkValidator #-}
mkValidator
    ::  VickreyAuctionParams
    ->  VickreyAuctionDatum
    ->  VickreyAuctionAction
    ->  TxOutRef
    ->  [TxInInfo]
    ->  [TxOut]
    ->  POSIXTimeRange
    ->  [PubKeyHash]
    ->  [TxInInfo]
    ->  Map ScriptPurpose Redeemer
    ->  Bool
mkValidator
    VickreyAuctionParams{..}
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
        ,   VickreyAuctionRefScriptDatum  domainName dnsCS currencySymbolOfVABAT MarketplaceInfo{..} treasuryAddress treasuryDatum <- unsafeFromBuiltinData refData
        ,   traceIfFalse "VickreyAuction 1" (builtinDomainName == domainName)
            =
                case ( redeemer , datum ) of

                    -----------------------------------------------------------------------------------------
                    --                                   UPDATE BID ACTION                                 --
                    -----------------------------------------------------------------------------------------

                    ( UpdateBid
                        ,   BidDatum dnsTN dnsCS' _ bidderAddr revealingDeadline )
                            |   BidDatum dnsTN' dnsCS'' _ bidderAddr' revealingDeadline' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ,   AuctionDatum dnsTN'' dnsCS''' bidingDeadline _ _ _ _ <- getTxOutInlineDatum $ getRefUTxO dnsCS dnsTN
                                ->
                                        traceIfFalse "VickreyAuction 1.1"
                                            (doesSCUTxOsHaveNFT currencySymbolOfVABAT dnsTN)
                                    &&  traceIfFalse "VickreyAuction 1.2"
                                            (       (dnsTN == dnsTN')
                                                &&  (dnsTN == dnsTN'')
                                                &&  (dnsCS == dnsCS')
                                                &&  (dnsCS == dnsCS'')
                                                &&  (dnsCS == dnsCS''')
                                                &&  (revealingDeadline == revealingDeadline')
                                                &&  (bidderAddr == bidderAddr')
                                            )
                                    &&  traceIfFalse "VickreyAuction 1.3"
                                            isSCInputOutputCorrect
                                    &&  traceIfFalse "VickreyAuction 1.4"
                                            (txSignedByAddress bidderAddr)
                                    &&  traceIfFalse "VickreyAuction 1.5"
                                            (to bidingDeadline `contains` txInfoValidRange)

                    -----------------------------------------------------------------------------------------
                    --                                BID DISCLOSURE ACTION                                --
                    -----------------------------------------------------------------------------------------

                    ( BidDisclosure bid salt
                        ,   AuctionDatum dnsTN dnsCS' bidingDeadline revealingDeadline owner highestBidder highestBid )
                            |   AuctionDatum dnsTN' dnsCS'' bidingDeadline' revealingDeadline'  owner' highestBidder' highestBid' <- getTxOutInlineDatum singleScriptOutputUTxO
                            ,   BidDatum dnsTN'' dnsCS''' secretBid bidderAddr _ <- getTxOutInlineDatum $ getRefUTxO currencySymbolOfVABAT dnsTN
                                ->
                                        traceIfFalse "VickreyAuction 2.1"
                                            (doesSCUTxOsHaveNFT dnsCS dnsTN)
                                    &&  traceIfFalse "VickreyAuction 2.2"
                                            (       (dnsTN == dnsTN')
                                                &&  (dnsTN == dnsTN'')
                                                &&  (dnsCS == dnsCS')
                                                &&  (dnsCS == dnsCS'')
                                                &&  (dnsCS == dnsCS''')
                                                &&  (bidingDeadline == bidingDeadline')
                                                &&  (revealingDeadline == revealingDeadline')
                                                &&  (owner          == owner')
                                                &&  (fromJust highestBidder' == bidderAddr)
                                                &&  (highestBid'    == bid)
                                            )
                                    &&  traceIfFalse "VickreyAuction 2.3"
                                            (       (from bidingDeadline `contains` txInfoValidRange)
                                                &&  (to revealingDeadline `contains` txInfoValidRange)
                                            )
                                    &&  traceIfFalse "VickreyAuction 2.4"
                                            (txSignedByAddress bidderAddr)
                                    &&  traceIfFalse "VickreyAuction 2.5"
                                            (bidEvaluation secretBid bid salt)
                                    &&  traceIfFalse "VickreyAuction 2.6"
                                            (isSCOutputCorrect bid)
                                    &&  traceIfFalse "VickreyAuction 2.7"
                                            (   case (highestBidder, highestBid) of
                                                    (Nothing, bid')     ->  bid' < bid
                                                    (Just addr, bid')   ->      isAddrGettingPaidExactly addr bid'
                                                                            &&  bid' < bid
                                            )

                    -----------------------------------------------------------------------------------------
                    --                                  DNS REDEEMING ACTION                               --
                    -----------------------------------------------------------------------------------------

                    ( DNSRedeeming
                        ,   AuctionDatum dnsTN dnsCS' _ revealingDeadline owner highestBidder highestBid )
                            ->
                                let bidderAddr = fromJust highestBidder
                                    digitalisFee = calculateFee precision minFee feePercentage tradeThreshold highestBid
                                    treasuryDatum' = getTxOutInlineDatum $ getTreasuryTxOut treasuryAddress digitalisFee
                                in  traceIfFalse "VickreyAuction 3.1"
                                        (       (dnsCS == dnsCS')
                                            &&  (treasuryDatum == treasuryDatum')
                                        )
                                &&  traceIfFalse "VickreyAuction 3.2"
                                        (from revealingDeadline `contains` txInfoValidRange)
                                &&  traceIfFalse "VickreyAuction 3.3"
                                        (valueOf (txOutValue singleScriptInputUTxO) dnsCS dnsTN == 1)
                                &&  traceIfFalse "VickreyAuction 3.4"
                                        (       txSignedByAddress bidderAddr
                                            ||
                                                txSignedByAddress owner
                                        )
                                &&  traceIfFalse "VickreyAuction 3.5"
                                        (isAddrGettingPaidExactly owner (highestBid - digitalisFee))


                    -----------------------------------------------------------------------------------------
                    --                                  BID CLAIMING ACTION                                --
                    -----------------------------------------------------------------------------------------

                    ( BidClaiming
                        ,   BidDatum _ dnsCS' _ bidderAddr revealingDeadline )
                        |   BurnVABAT <- getCSRedeemer currencySymbolOfVABAT
                            ->
                                    traceIfFalse "VickreyAuction 4.1"
                                        (dnsCS == dnsCS')
                                &&  traceIfFalse "VickreyAuction 4.2"
                                        (isAddrGettingPaidExactly bidderAddr (adaAmountOf (txOutValue singleScriptInputUTxO)))
                                &&  traceIfFalse "VickreyAuction 4.3"
                                        (from revealingDeadline `contains` txInfoValidRange)

                    -----------------------------------------------------------------------------------------
                    --                                   ANY OTHER CASE                                    --
                    -----------------------------------------------------------------------------------------

                    (_,_) -> traceIfFalse "VickreyAuction 5.1" False

        |   otherwise = traceError "VickreyAuction 2"

        where

            findOwnInput :: Maybe TxInInfo
            findOwnInput = find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == ownTxOutRef) txInfoInputs

            getContinuingOutputs :: [TxOut]
            getContinuingOutputs
                |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput = filter (f txOutAddress) txInfoOutputs
                |   otherwise = traceError "VickreyAuction 2"
                    where
                        f addr TxOut{txOutAddress=otherAddress} = addr == otherAddress

            ownHash ::  ScriptHash
            ownHash |   Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=Address (ScriptCredential sh) _}} <- findOwnInput = sh
                    |   otherwise = traceError "VickreyAuction 3"

            getTxOutInlineDatum :: forall dat. (UnsafeFromData dat) => TxOut -> dat
            getTxOutInlineDatum tx
                | ( OutputDatum ( Datum inline )) <- txOutDatum tx
                = unsafeFromBuiltinData @dat inline
                | otherwise = traceError "VickreyAuction 3"

            valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
            valueOf (Value mp) cur tn
                |   Just i <- lookup cur mp = fromMaybe 0 (lookup tn i)
                |   otherwise = 0

            singleScriptInputUTxO :: TxOut
            singleScriptInputUTxO
                |   Just i      <-  findOwnInput = txInInfoResolved i
                |   otherwise   =   traceError "VickreyAuction 4"

            singleScriptOutputUTxO :: TxOut
            singleScriptOutputUTxO
                |   [o]         <-  getContinuingOutputs = o
                |   otherwise   =   traceError "VickreyAuction 5"

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
                |   otherwise   =   traceError "VickreyAuction 6"

            doesSCUTxOsHaveNFT :: CurrencySymbol -> TokenName -> Bool
            doesSCUTxOsHaveNFT cs tn =
                    valueOf (txOutValue singleScriptInputUTxO) cs tn == 1
                &&  valueOf (txOutValue singleScriptOutputUTxO) cs tn == 1

            txSignedByAddress :: Address -> Bool
            txSignedByAddress (Address (PubKeyCredential pkh) _) = pkh `elem` txInfoSignatories
            txSignedByAddress _ = traceError "VickreyAuction 7"

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
                    |   otherwise = traceError "VickreyAuction 8"
                        where

                            scriptHashToScriptHash :: ScriptHash -> ScriptHash
                            scriptHashToScriptHash (ScriptHash hash) = ScriptHash hash

                            scriptHashToTokenName :: ScriptHash -> TokenName
                            scriptHashToTokenName (ScriptHash hash) = TokenName hash


            fromJust :: Maybe a -> a
            fromJust (Just x) = x
            fromJust _        = traceError "VickreyAuction 9"

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
                |   otherwise = traceError "VickreyAuction 10"

            calculateFee :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
            calculateFee prs minFee feePct th amt
                |   amt <= th  = minFee
                |   amt > th = (amt * prs) `divideInteger` feePct
                |   otherwise = traceError "VickreyAuction 11"

            getCSRedeemer :: forall rad. (UnsafeFromData rad) => CurrencySymbol -> rad
            getCSRedeemer cs
                |   Just rawMintingRedeemer <- lookup (Minting cs) txInfoRedeemers
                =   unsafeFromBuiltinData @rad $ getRedeemer rawMintingRedeemer
                |   otherwise = traceError "VickreyAuction 12"


{-=================================================== END OF VALIDATOR SECTION ====================================================-}

------------------------------------------------------------------------------------------
--                                    COMPILATION                                       --
------------------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator :: VickreyAuctionParams -> BuiltinData -> BuiltinUnit
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


compiledCode :: VickreyAuctionParams -> CompiledCode (BuiltinData -> BuiltinUnit)
compiledCode params =
    $$(PlutusTx.compile [||mkUntypedValidator||])
        `unsafeApplyCode` PlutusTx.liftCode plcVersion110 params
