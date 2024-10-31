module Orphans where

import Cardano.Api (Quantity (..))
import qualified Codec.CBOR.Write as Write
import qualified Codec.Serialise as Serialise
import Codec.Serialise.Class (Serialise)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Functor
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import PlutusCore.Data (Data)
import PlutusLedgerApi.V3
  ( Address (..),
    Credential (..),
    CurrencySymbol (..),
    Datum (..),
    DatumHash (..),
    POSIXTime (..),
    PubKeyHash (..),
    ScriptHash (..),
    ScriptPurpose (..),
    StakingCredential (..),
    TokenName (..),
    TxId (..),
    TxOutRef (..),
    Value (..),
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude (Bool (..), Eq, (==))
import qualified PlutusTx.Prelude as PlutusTx
import Prelude (Either (..), Enum, Integral, Real, String, either, fail, pure, show, ($), (&&), (.), (<$>))

deriving newtype instance Enum Quantity

deriving newtype instance Real Quantity

deriving newtype instance Integral Quantity

deriving anyclass instance (ToJSON k, ToJSON v) => ToJSON (AssocMap.Map k v)

deriving anyclass instance (FromJSON k, FromJSON v) => FromJSON (AssocMap.Map k v)

deriving anyclass instance FromJSON Address

deriving anyclass instance ToJSON Address

deriving anyclass instance FromJSON CurrencySymbol

deriving anyclass instance ToJSON CurrencySymbol

deriving anyclass instance FromJSON Value

deriving anyclass instance ToJSON Value

deriving anyclass instance FromJSON Credential

deriving anyclass instance ToJSON Credential

deriving anyclass instance FromJSON StakingCredential

deriving anyclass instance ToJSON StakingCredential

deriving anyclass instance FromJSON POSIXTime

deriving anyclass instance ToJSON POSIXTime

deriving anyclass instance FromJSON TokenName

deriving anyclass instance ToJSON TokenName

deriving anyclass instance FromJSON ScriptHash

deriving anyclass instance ToJSON ScriptHash

deriving anyclass instance FromJSON Datum

deriving anyclass instance ToJSON Datum

deriving anyclass instance FromJSON DatumHash

deriving anyclass instance ToJSON DatumHash

deriving anyclass instance FromJSON TxOutRef

deriving anyclass instance ToJSON TxOutRef

deriving anyclass instance ToJSON TxId

deriving anyclass instance FromJSON TxId

deriving anyclass instance JSON.ToJSONKey TxId

deriving anyclass instance JSON.FromJSONKey TxId

instance ToJSON PlutusTx.BuiltinByteString where
  toJSON = JSON.String . encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
  parseJSON v = PlutusTx.toBuiltin <$> decodeByteString v

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinData where
  parseJSON v = parseJSON v Data.Functor.<&> PlutusTx.dataToBuiltinData

deriving anyclass instance ToJSON PubKeyHash

deriving anyclass instance FromJSON PubKeyHash

deriving anyclass instance JSON.FromJSONKey PubKeyHash

deriving anyclass instance JSON.ToJSONKey PubKeyHash

deriving via (JSONViaSerialise Data) instance ToJSON Data

deriving via (JSONViaSerialise Data) instance FromJSON Data

-- | Newtype for deriving 'ToJSON' and 'FromJSON' for types that have a 'Serialise'
-- instance by just encoding the serialized bytes as a JSON string.
newtype JSONViaSerialise a = JSONViaSerialise a

instance Serialise a => ToJSON (JSONViaSerialise a) where
  toJSON (JSONViaSerialise a) = JSON.String $ encodeSerialise a

instance Serialise a => FromJSON (JSONViaSerialise a) where
  parseJSON v = JSONViaSerialise <$> decodeSerialise v

encodeByteString :: BS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

tryDecode :: Text.Text -> Either String BS.ByteString
tryDecode = Base16.decode . TE.encodeUtf8

decodeByteString :: JSON.Value -> JSON.Parser BS.ByteString
decodeByteString = JSON.withText "ByteString" (either fail pure . tryDecode)

encodeSerialise :: Serialise a => a -> Text.Text
encodeSerialise = encodeByteString . Write.toStrictByteString . Serialise.encode

decodeSerialise :: Serialise a => JSON.Value -> JSON.Parser a
decodeSerialise = decodeByteString >=> go
  where
    go bs =
      case first show $ Serialise.deserialiseOrFail $ BSL.fromStrict bs of
        Left e -> fail e
        Right v -> pure v

instance Eq ScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting a == Minting a' = a == a'
  Spending a == Spending a' = a == a'
  -- Rewarding a     == Rewarding a'     = a == a'
  -- Certifying a b  == Certifying a' b' = a == a' && b == b'
  -- Voting a        == Voting a'        = a == a'
  -- Proposing a b   == Proposing a' b'  = a == a' && b == b'
  _ == _ = False

-- instance Eq ScriptPurpose where
--     {-# INLINABLE (==) #-}
--     Minting a       == Minting a'       = a == a'
--     Spending a      == Spending a'      = a == a'
--     _ == _                              = False

-- instance Eq ProposalProcedure where
--     {-# INLINABLE (==) #-}
--     ProposalProcedure a b c == ProposalProcedure a' b' c'
--         = a == a' && b == b' && c == c'
--     _ == _                              = False
