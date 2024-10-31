#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

# Check if DOMAIN_NAME argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 DOMAIN_NAME"
    exit 1
fi

DOMAIN_NAME="$1"

CAT_POLICY_ID=$(getDigitalisContractSH "$CAT_CONTRACT_NAME")
CONTRACTS_CONTROLLER_VALIDATOR_HASH=$(getDigitalisContractSH "$CONTRACTS_CONTROLLER_CONTRACT_NAME")

CONTRACTS_PATH="${BUILD_PATH}/${DOMAIN_NAME}/contracts"
CORES_PATH="${DATA_PATH}/${DOMAIN_NAME}/cores"

# DNSMinter
printf "\033[1;32m[+] DNSMinter Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$DNS_MINTER_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# DNSReference
printf "\033[1;32m[+] DNSReference Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$DNS_REFERENCE_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# DRAT
printf "\033[1;32m[+] DRAT Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$DRAT_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# MintingDNSAuction
printf "\033[1;32m[+] MintingDNSAuction Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# MHABAT
printf "\033[1;32m[+] MHABAT Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$MHABAT_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# VickreyAuction
printf "\033[1;32m[+] VickreyAuction Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$VICKREY_AUCTION_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# VABAT
printf "\033[1;32m[+] VABAT Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$VABAT_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"

# EnglishAuction
printf "\033[1;32m[+] EnglishAuction Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$CONTRACTS_PATH" \
    "$CORES_PATH" \
    "$ENGLISH_AUCTION_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID" \
    "$DOMAIN_NAME"
