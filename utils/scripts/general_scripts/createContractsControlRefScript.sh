#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating Contracts Controller Reference Script UTxO ..."

main() {

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

    contractRefScriptUTxO=$(get_digitalis_txID "$CAT_CONTRACT_NAME""_ref_script")

    cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
        "$DIGITALIS_REDEEMERS_PATH" \
        "$CAT_CONTRACT_NAME" \
        "DigitalisBoardInception" \
        >/dev/null 2>&1

    DIGITALIS_BOARD_INCEPTION_REDEEMER=$(getDigitalisRedeemer "${CAT_CONTRACT_NAME}" "DigitalisBoardInception")

    BMAT_1_POLICY_ID=$(getDigitalisContractSH "${BMAT_1_CONTRACT_NAME}")
    BMAT_2_POLICY_ID=$(getDigitalisContractSH "${BMAT_2_CONTRACT_NAME}")
    BMAT_3_POLICY_ID=$(getDigitalisContractSH "${BMAT_3_CONTRACT_NAME}")
    BMAT_4_POLICY_ID=$(getDigitalisContractSH "${BMAT_4_CONTRACT_NAME}")
    BMAT_5_POLICY_ID=$(getDigitalisContractSH "${BMAT_5_CONTRACT_NAME}")
    BMAT_6_POLICY_ID=$(getDigitalisContractSH "${BMAT_6_CONTRACT_NAME}")

    annualAssemblyDeadline=$(($(get_current_POSIX_time) + 31539600000))

    printf "\n\033[1;32m[+] ContractsControllerRefScriptDatum: \033[0m"
    cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
        "$DIGITALIS_DATUMS_PATH" \
        "$CONTRACTS_CONTROLLER_CONTRACT_NAME" \
        "ContractsControllerRefScriptDatum" \
        "$BMAT_5_POLICY_ID,$BMAT_6_POLICY_ID" \
        "$BMAT_1_POLICY_ID,$BMAT_2_POLICY_ID,$BMAT_3_POLICY_ID,$BMAT_4_POLICY_ID" \
        "$annualAssemblyDeadline"

    sleep 1

    CONTRACTS_CONTROLLER_REF_SCRIPT_DATUM=$(getDigitalisDatum "${CONTRACTS_CONTROLLER_CONTRACT_NAME}" "ContractsControllerRefScriptDatum")
    CONTRACTS_CONTROLLER_CONTRACT=$(getDigitalisContract "${CONTRACTS_CONTROLLER_CONTRACT_NAME}")
    STAKING_CONTRACT=$(getDigitalisContract "${STAKING_CONTRACT_NAME}")
    CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")

    CAT_POLICY_ID=$(getDigitalisContractSH "${CAT_CONTRACT_NAME}")

    digitalis_board_member_1_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_1_POLICY_ID")
    digitalis_board_member_2_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_2_POLICY_ID")
    digitalis_board_member_3_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_3_POLICY_ID")
    digitalis_board_member_4_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_4_POLICY_ID")
    digitalis_board_member_5_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_5_POLICY_ID")
    digitalis_board_member_6_txID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_6_POLICY_ID")

    nowSlotNumber=$(get_current_slot_number)
    submissionTime=$((nowSlotNumber + 60))

    echo -e "\033[1;32m[+] The '$CONTRACTS_CONTROLLER_CONTRACT_NAME' Contract Address: \033[0m" "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS"

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"${CONTRACTS_CONTROLLER_CONTRACT_NAME}"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS + 1000000 + 1 ${CAT_POLICY_ID}" \
        --tx-out-inline-datum-file "$CONTRACTS_CONTROLLER_REF_SCRIPT_DATUM" \
        --tx-out-reference-script-file "$CONTRACTS_CONTROLLER_CONTRACT" \
        --mint="1 ${CAT_POLICY_ID}" \
        --mint-tx-in-reference "${contractRefScriptUTxO}" \
        --mint-plutus-script-v3 \
        --mint-reference-tx-in-redeemer-file "$DIGITALIS_BOARD_INCEPTION_REDEEMER" \
        --policy-id="${CAT_POLICY_ID}" \
        --read-only-tx-in-reference "$digitalis_board_member_1_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_2_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_3_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_4_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_5_txID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_6_txID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --invalid-hereafter "$submissionTime" \
        2>&1 | tee /tmp/tmp >/dev/null 2>&1

    minAmount=$(sed -n '2p' /tmp/tmp)
    IFS=' ' read -ra outputs <<<"$minAmount"
    IFS=' ' read -ra minAmount <<<"${outputs[3]}"
    echo -e "\033[1;32m[+] Minimum ADA Was Needed For '$CONTRACTS_CONTROLLER_CONTRACT_NAME' Reference Script UTxO:\033[0m" "${minAmount[0]}"
    rm /tmp/tmp

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"${CONTRACTS_CONTROLLER_CONTRACT_NAME}"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS + ${minAmount[0]} + 1 ${CAT_POLICY_ID}" \
        --tx-out-inline-datum-file "$CONTRACTS_CONTROLLER_REF_SCRIPT_DATUM" \
        --tx-out-reference-script-file "$CONTRACTS_CONTROLLER_CONTRACT" \
        --mint="1 ${CAT_POLICY_ID}" \
        --mint-tx-in-reference "${contractRefScriptUTxO}" \
        --mint-plutus-script-v3 \
        --mint-reference-tx-in-redeemer-file "$DIGITALIS_BOARD_INCEPTION_REDEEMER" \
        --policy-id="${CAT_POLICY_ID}" \
        --read-only-tx-in-reference "$digitalis_board_member_1_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_2_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_3_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_4_txID" \
        --required-signer-hash "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_5_txID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_1_FIRST_WALLET_PAYMENT_PKH" \
        --read-only-tx-in-reference "$digitalis_board_member_6_txID" \
        --required-signer-hash "$CORE_TEAM_MEMBER_2_FIRST_WALLET_PAYMENT_PKH" \
        --invalid-hereafter "$submissionTime" \
        >/dev/null

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"${CONTRACTS_CONTROLLER_CONTRACT_NAME}"_ref_script.signed \
        --tx-body-file "$DIGITALIS_TX_PATH"/"${CONTRACTS_CONTROLLER_CONTRACT_NAME}"_ref_script.body \
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
        --tx-file "$DIGITALIS_TX_PATH"/"${CONTRACTS_CONTROLLER_CONTRACT_NAME}"_ref_script.signed \
        >/dev/null

    txID=$(get_digitalis_txID "$CONTRACTS_CONTROLLER_CONTRACT_NAME""_ref_script")
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    sleep 1

    echo -e "\033[1;32m[+] $CONTRACTS_CONTROLLER_CONTRACT_NAME Reference Script UTxO Has Been Created Successfully.\033[0m"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The '$CONTRACTS_CONTROLLER_CONTRACT_NAME Reference Utxo' Is In Contracts Controller Contract With 'Contracts Board Members Policy IDs' In Its Datum:"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation ..."

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
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying To Create The '$CAT_CONTRACT_NAME' Reference Script Utxo."
        main
    fi

    create_config_file

    jq -n \
        --arg contractName "$CONTRACTS_CONTROLLER_CONTRACT_NAME" \
        --arg address "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" \
        '{ ($contractName): { "Address": $address } }' \
        >"${REPO_HOME}/config.json"

    add_ref_txid "$CONTRACTS_CONTROLLER_CONTRACT_NAME" "$txID"

}

main

# printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please press 'Enter' to continue."
# read -r
