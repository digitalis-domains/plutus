#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating Digitalis Reference Script UTxOs ..."

cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$DIGITALIS_REDEEMERS_PATH" \
    "$CAT_CONTRACT_NAME" \
    "DigitalisBoardAction" \
    >/dev/null 2>&1

DIGITALIS_BOARD_ACTION_REDEEMER=$(getDigitalisRedeemer "${CAT_CONTRACT_NAME}" "DigitalisBoardAction")

CAT_POLICY_ID=$(getDigitalisContractSH "${CAT_CONTRACT_NAME}")
BMAT_1_POLICY_ID=$(getDigitalisContractSH "${BMAT_1_CONTRACT_NAME}")
BMAT_2_POLICY_ID=$(getDigitalisContractSH "${BMAT_2_CONTRACT_NAME}")
BMAT_3_POLICY_ID=$(getDigitalisContractSH "${BMAT_3_CONTRACT_NAME}")
BMAT_4_POLICY_ID=$(getDigitalisContractSH "${BMAT_4_CONTRACT_NAME}")
BMAT_5_POLICY_ID=$(getDigitalisContractSH "${BMAT_5_CONTRACT_NAME}")
BMAT_6_POLICY_ID=$(getDigitalisContractSH "${BMAT_6_CONTRACT_NAME}")
STAKING_VALIDATOR_HASH=$(getDigitalisContractSH "$STAKING_CONTRACT_NAME")
MAIN_TREASURY_VALIDATOR_HASH=$(getDigitalisContractSH "$MAIN_TREASURY_CONTRACT_NAME")

STAKING_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${STAKING_VALIDATOR_HASH}
MAIN_TREASURY_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${MAIN_TREASURY_VALIDATOR_HASH}

CONTRACTS_CONTROLLER_CONTRACT=$(getDigitalisContract "${CONTRACTS_CONTROLLER_CONTRACT_NAME}")
STAKING_CONTRACT=$(getDigitalisContract "${STAKING_CONTRACT_NAME}")
MAIN_TREASURY_CONTRACT=$(getDigitalisContract "${MAIN_TREASURY_CONTRACT_NAME}")

CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")
MAIN_TREASURY_CONTRACT_ADDRESS=$(getContractAddress "$MAIN_TREASURY_CONTRACT" "${STAKING_CONTRACT}")
STAKING_CONTRACT_ADDRESS=$(getStakeAddress "${STAKING_CONTRACT}")

DIGITALIS_BOARD_MEMBER_1_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_1_POLICY_ID")
DIGITALIS_BOARD_MEMBER_2_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_2_POLICY_ID")
DIGITALIS_BOARD_MEMBER_3_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_3_POLICY_ID")
DIGITALIS_BOARD_MEMBER_4_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_4_POLICY_ID")
DIGITALIS_BOARD_MEMBER_5_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_5_POLICY_ID")
DIGITALIS_BOARD_MEMBER_6_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_6_POLICY_ID")

main() {
    local CONTRACT_NAME
    local CONTRACT
    local AUTH_TOKEN
    local DATUM
    CONTRACT_NAME="$1"
    CONTRACT="$2"
    AUTH_TOKEN="$3"
    DATUM="$4"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] '$CONTRACT_NAME' Reference Script UTxO ..."

    if [[ -f "/tmp/$DIGITALIS_WALLET.json" ]]; then
        rm /tmp/"$DIGITALIS_WALLET".json
    fi
    cardano-cli query utxo \
        --address "$DIGITALIS_WALLET_ADDRESS" \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file /tmp/"$DIGITALIS_WALLET".json
    if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' /tmp/"$DIGITALIS_WALLET".json) && -f "/tmp/$DIGITALIS_WALLET.json" ]]; then
        printf "\n$RED%b\n\n" "[-] ERROR: NO Any UTxOs Found At '$DIGITALIS_WALLET' Address"
        exit 1
    fi
    txIn=$(get_address_biggest_lovelace "$DIGITALIS_WALLET_ADDRESS")

    collateralTxIn=$(get_address_biggest_lovelace "$COLLATERAL_ADDRESS")

    CATcontractRefScriptUTxO=$(get_digitalis_txID "$CAT_CONTRACT_NAME""_ref_script")

    contractsControllerContractRefScriptUTxO=$(get_digitalis_txID "$CONTRACTS_CONTROLLER_CONTRACT_NAME""_ref_script")

    sleep 1

    nowSlotNumber=$(get_current_slot_number)
    submissionTime=$((nowSlotNumber + 60))

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS + 1000000 + 1 ${AUTH_TOKEN}" \
        --tx-out-inline-datum-file "$DATUM" \
        --tx-out-reference-script-file "$CONTRACT" \
        --mint="1 ${AUTH_TOKEN}" \
        --mint-tx-in-reference "${CATcontractRefScriptUTxO}" \
        --mint-plutus-script-v3 \
        --mint-reference-tx-in-redeemer-file "$DIGITALIS_BOARD_ACTION_REDEEMER" \
        --policy-id="${CAT_POLICY_ID}" \
        --read-only-tx-in-reference "$contractsControllerContractRefScriptUTxO" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_1_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_2_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_3_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_4_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_5_TXID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_6_TXID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --invalid-hereafter "$submissionTime" \
        2>&1 | tee /tmp/tmp >/dev/null 2>&1

    minAmount=$(sed -n '2p' /tmp/tmp)
    IFS=' ' read -ra outputs <<<"$minAmount"
    IFS=' ' read -ra minAmount <<<"${outputs[3]}"
    echo -e "\033[1;32m[+] Minimum ADA Was Needed For '$CONTRACT_NAME' Reference Script UTxO:\033[0m" "${minAmount[0]}"
    rm /tmp/tmp

    sleep 1

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS + ${minAmount[0]} + 1 ${AUTH_TOKEN}" \
        --tx-out-inline-datum-file "$DATUM" \
        --tx-out-reference-script-file "$CONTRACT" \
        --mint="1 ${AUTH_TOKEN}" \
        --mint-tx-in-reference "${CATcontractRefScriptUTxO}" \
        --mint-plutus-script-v3 \
        --mint-reference-tx-in-redeemer-file "$DIGITALIS_BOARD_ACTION_REDEEMER" \
        --policy-id="${CAT_POLICY_ID}" \
        --read-only-tx-in-reference "$contractsControllerContractRefScriptUTxO" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_1_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_2_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_3_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_4_TXID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_5_TXID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$DIGITALIS_BOARD_MEMBER_6_TXID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --invalid-hereafter "$submissionTime" \
        >/dev/null

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.signed \
        --tx-body-file "$DIGITALIS_TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
        --signing-key-file "$DIGITALIS_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$CORE_TEAM_MEMBER_1_FIRST_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$CORE_TEAM_MEMBER_2_FIRST_WALLET_PAYMENT_SKEY" \
        >/dev/null

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$DIGITALIS_TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.signed \
        >/dev/null

    sleep 1

    txID=$(get_digitalis_txID "create_""$CONTRACT_NAME""_ref_script")
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    sleep 1

    echo -e "\033[1;32m[+] '$CONTRACT_NAME Ref UTxO' Has Been Created Successfully.\033[0m"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The '$CONTRACT_NAME' Reference Script Utxo Is In The Contracts Controller Contract Address."

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] But Please Wait For Tx Submission & Confirmation To Get UTxO Information ..."

    result=""
    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$txID")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying To Create The '$CONTRACT_NAME' Reference Script Utxo."
        main "$CONTRACT_NAME" "$CONTRACT" "$AUTH_TOKEN" "$DATUM"
    fi

    add_ref_txid "$CONTRACT_NAME" "$txID"

    # printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please press 'Enter' to continue."
    # read -r
}

##
## Staking
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$STAKING_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1

jq --argjson newContent '{
        "'"$STAKING_CONTRACT_NAME"'": {
            "Address": "'"$STAKING_CONTRACT_ADDRESS"'"
        }
    }' '. += $newContent' "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

printf "\n\033[1;32m[+] StakingRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DIGITALIS_DATUMS_PATH" \
    "$STAKING_CONTRACT_NAME" \
    "StakingRefScriptDatum" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$POOL_ID" \
    "$STAKING_FEE_TYPE"
STAKING_REF_SCRIPT_DATUM=$(getDigitalisDatum "${STAKING_CONTRACT_NAME}" "StakingRefScriptDatum")
main "$STAKING_CONTRACT_NAME" "$STAKING_CONTRACT" "$STAKING_REF_SCRIPT_AUTH_TOKEN" "$STAKING_REF_SCRIPT_DATUM"

##
## MainTreasury
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$MAIN_TREASURY_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1
echo -e "\033[1;32m[+] The '$MAIN_TREASURY_CONTRACT_NAME' Contract Address: \033[0m" "$MAIN_TREASURY_CONTRACT_ADDRESS"

jq --argjson newContent '{
        "'"$MAIN_TREASURY_CONTRACT_NAME"'": {
            "Address": "'"$MAIN_TREASURY_CONTRACT_ADDRESS"'"
        }
    }' '. += $newContent' "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] MainTreasuryRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DIGITALIS_DATUMS_PATH" \
    "$MAIN_TREASURY_CONTRACT_NAME" \
    "MainTreasuryRefScriptDatum" \
    "$TREASURY_BOARD_MEMBER_1_PAYMENT_PKH,$TREASURY_BOARD_MEMBER_2_PAYMENT_PKH,$TREASURY_BOARD_MEMBER_3_PAYMENT_PKH" \
    "$TREASURER_PAYMENT_PKH"
MAIN_TREASURY_REF_SCRIPT_DATUM=$(getDigitalisDatum "${MAIN_TREASURY_CONTRACT_NAME}" "MainTreasuryRefScriptDatum")
main "$MAIN_TREASURY_CONTRACT_NAME" "$MAIN_TREASURY_CONTRACT" "$MAIN_TREASURY_REF_SCRIPT_AUTH_TOKEN" "$MAIN_TREASURY_REF_SCRIPT_DATUM"

jq --argjson newContent '{
        "'"WalletsMnemonic"'": {
            "DNSMintingAdmin": "'"$(getWalletMnemonic "${DNS_ADMIN_1}")"'",
            "MintingDNSAuctionAdmin": "'"$(getWalletMnemonic "${MARKETPLACE_ADMIN_1}")"'",
            "DNSReferenceAdmin": "'"$(getWalletMnemonic "${DNS_ADMIN_2}")"'"
        }
    }' '. += $newContent' "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json
