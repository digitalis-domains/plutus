module Utils.Address
  ( tryReadAddress,
    pkhFromAddress,
    scrHashFromAddress,
    addrStrWithSpkhPAddr,
  )
where

import Cardano.Api.Shelley (Address (..), AddressAny (..), AsType (..), deserialiseAddress)
import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.BaseTypes as CLB
import Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text as Text (pack)
import PlutusLedgerApi.V1.Address as Plutus
import PlutusLedgerApi.V1.Credential as Plutus
import PlutusLedgerApi.V1.Crypto as Plutus
import PlutusLedgerApi.V2 as Plutus
import Prelude (Maybe (..), String, error, fromIntegral, ($))

addrStrWithSpkhPAddr :: String -> String -> Plutus.Address
addrStrWithSpkhPAddr addr spkh = Plutus.Address (ScriptCredential $ scrHashFromAddress addr) (Just (StakingHash $ ScriptCredential $ fromString spkh))

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (Cardano.Ledger.Hashes.ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ScriptHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h)) = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x) = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) (CLB.TxIx y) (CertIx z))) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull = Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ Text.pack x of
  Nothing -> Nothing
  Just (AddressByron _) -> Nothing
  Just (AddressShelley (ShelleyAddress _ p s)) ->
    Just
      Plutus.Address
        { Plutus.addressCredential = credentialLedgerToPlutus p,
          Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

pkhFromAddress :: String -> PubKeyHash
pkhFromAddress x = case tryReadAddress x of
  Nothing -> error "address reading error"
  Just addr -> fromJust $ Plutus.toPubKeyHash addr

scrHashFromAddress :: String -> Plutus.ScriptHash
scrHashFromAddress x = case tryReadAddress x of
  Nothing -> error "address reading error"
  Just addr -> fromJust $ Plutus.toScriptHash addr