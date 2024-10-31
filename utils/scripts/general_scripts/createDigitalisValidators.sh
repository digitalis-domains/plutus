#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

# CAT
printf "\033[1;32m[+] CAT Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$DIGITALIS_CONTRACTS_PATH" \
    "$DIGITALIS_CORES_PATH" \
    "$CAT_CONTRACT_NAME" \
    $(($(get_current_POSIX_time) + 3600000))
CAT_POLICY_ID=$(getDigitalisContractSH "$CAT_CONTRACT_NAME")

# ContractsController
printf "\033[1;32m[+] ContractsController Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$DIGITALIS_CONTRACTS_PATH" \
    "$DIGITALIS_CORES_PATH" \
    "$CONTRACTS_CONTROLLER_CONTRACT_NAME" \
    "$CAT_POLICY_ID"
CONTRACTS_CONTROLLER_VALIDATOR_HASH=$(getDigitalisContractSH "$CONTRACTS_CONTROLLER_CONTRACT_NAME")

# MainTreasury
printf "\033[1;32m[+] MainTreasury Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$DIGITALIS_CONTRACTS_PATH" \
    "$DIGITALIS_CORES_PATH" \
    "$MAIN_TREASURY_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID"

# Staking
printf "\033[1;32m[+] Staking Contract: \033[0m"
cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
    "$DIGITALIS_CONTRACTS_PATH" \
    "$DIGITALIS_CORES_PATH" \
    "$STAKING_CONTRACT_NAME" \
    "$CONTRACTS_CONTROLLER_VALIDATOR_HASH" \
    "$CAT_POLICY_ID"
