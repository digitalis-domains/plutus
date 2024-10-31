module Utils.LazyScriptContext
  ( lazyTxInfoInputs,
    lazyTxInfoReferenceInputs,
    lazyTxInfoOutputs,
    lazyTxInfoFee,
    lazyTxInfoMint,
    lazyTxInfoTxCerts,
    lazyTxInfoWdrl,
    lazyTxInfoValidRange,
    lazyTxInfoSignatories,
    lazyTxInfoRedeemers,
    lazyTxInfoData,
    lazyTxInfoId,
    lazyTxInfoVotes,
    lazyTxInfoProposalProcedures,
    lazyTxInfoCurrentTreasuryAmount,
    lazyTxInfoTreasuryDonation,
    getContext,
    lazyTxInfo,
    lazyRedeemer,
    lazyDatum,
    lazyTxOutRef,
    lazyOwnCurrencySymbol,
    lazyScriptInfo,
  )
where

import           PlutusLedgerApi.V3                         (Lovelace,
                                                           TxInInfo (..),Datum (..),Credential (..),
                                                            ScriptInfo(..),
                                                           TxOut (..),CurrencySymbol (..),
                                                           TxOutRef(..),Value (..),POSIXTimeRange,
                                                           Redeemer (..),ProposalProcedure(..),GovernanceActionId(..),
                                                           PubKeyHash,Map,Vote,Voter,DatumHash,
                                                            ScriptPurpose (..),TxId(..),TxCert(..)
                                                           )

import           PlutusTx                                 (BuiltinData,
                                                           UnsafeFromData,
                                                           unsafeFromBuiltinData)
import          PlutusTx.Builtins.Internal                  (head, tail, snd,BuiltinList, unsafeDataAsConstr)
import           PlutusTx.Prelude                         (Maybe (..),(.))

{-# INLINABLE constrArgs #-}
constrArgs :: BuiltinData -> BuiltinList BuiltinData
constrArgs = snd . unsafeDataAsConstr

{-# INLINEABLE getContext #-}
getContext :: BuiltinData -> BuiltinList BuiltinData
getContext = constrArgs

{-# INLINEABLE lazyRedeemer #-}
lazyRedeemer :: forall rad. (UnsafeFromData rad) => BuiltinData -> rad
lazyRedeemer = unsafeFromBuiltinData @rad . getRedeemer . unsafeFromBuiltinData . head . tail . getContext

{-# INLINEABLE scriptInfoData #-}
scriptInfoData :: BuiltinData -> BuiltinData
scriptInfoData = head . tail . tail . getContext

{-# INLINEABLE lazyScriptInfo #-}
lazyScriptInfo :: BuiltinData -> ScriptInfo
lazyScriptInfo = unsafeFromBuiltinData . head . tail . tail . getContext

{-# INLINEABLE lazyDatum #-}
lazyDatum :: forall dat. (UnsafeFromData dat) => BuiltinData -> dat
lazyDatum = unsafeFromBuiltinData @dat . getDatum . unsafeFromBuiltinData . head . constrArgs  . head . tail . constrArgs . scriptInfoData

{-# INLINEABLE lazyTxOutRef #-}
lazyTxOutRef :: BuiltinData -> TxOutRef
lazyTxOutRef = unsafeFromBuiltinData . head . constrArgs . scriptInfoData

{-# INLINEABLE lazyOwnCurrencySymbol #-}
lazyOwnCurrencySymbol :: BuiltinData -> CurrencySymbol
lazyOwnCurrencySymbol = CurrencySymbol . unsafeFromBuiltinData . head . constrArgs . scriptInfoData

{-# INLINEABLE lazyTxInfo #-}
lazyTxInfo :: BuiltinData -> BuiltinList BuiltinData
lazyTxInfo = constrArgs . head . getContext

{-# INLINEABLE lazyTxInfoInputs #-}
lazyTxInfoInputs :: BuiltinData -> [TxInInfo]
lazyTxInfoInputs = unsafeFromBuiltinData . head . lazyTxInfo

{-# INLINEABLE lazyTxInfoReferenceInputs #-}
lazyTxInfoReferenceInputs :: BuiltinData -> [TxInInfo]
lazyTxInfoReferenceInputs =
  unsafeFromBuiltinData . head . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoOutputs #-}
lazyTxInfoOutputs :: BuiltinData -> [TxOut]
lazyTxInfoOutputs =
  unsafeFromBuiltinData . head . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoFee #-}
lazyTxInfoFee :: BuiltinData -> Lovelace
lazyTxInfoFee =
  unsafeFromBuiltinData . head . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoMint #-}
lazyTxInfoMint :: BuiltinData -> Value
lazyTxInfoMint =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoTxCerts #-}
lazyTxInfoTxCerts :: BuiltinData -> [TxCert]
lazyTxInfoTxCerts =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoWdrl #-}
lazyTxInfoWdrl :: BuiltinData -> Map Credential Lovelace
lazyTxInfoWdrl =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoValidRange #-}
lazyTxInfoValidRange :: BuiltinData -> POSIXTimeRange
lazyTxInfoValidRange =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoSignatories #-}
lazyTxInfoSignatories :: BuiltinData -> [PubKeyHash]
lazyTxInfoSignatories =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoRedeemers #-}
lazyTxInfoRedeemers :: BuiltinData -> Map ScriptPurpose Redeemer
lazyTxInfoRedeemers =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoData #-}
lazyTxInfoData :: BuiltinData -> Map DatumHash Datum
lazyTxInfoData =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoId #-}
lazyTxInfoId :: BuiltinData -> TxId
lazyTxInfoId =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoVotes #-}
lazyTxInfoVotes :: BuiltinData -> Map Voter (Map GovernanceActionId Vote)
lazyTxInfoVotes =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoProposalProcedures #-}
lazyTxInfoProposalProcedures :: BuiltinData -> [ProposalProcedure]
lazyTxInfoProposalProcedures =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoCurrentTreasuryAmount #-}
lazyTxInfoCurrentTreasuryAmount :: BuiltinData -> Maybe Lovelace
lazyTxInfoCurrentTreasuryAmount =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo

{-# INLINEABLE lazyTxInfoTreasuryDonation #-}
lazyTxInfoTreasuryDonation :: BuiltinData -> Maybe Lovelace
lazyTxInfoTreasuryDonation =
  unsafeFromBuiltinData . head . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . tail . lazyTxInfo