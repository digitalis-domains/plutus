#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Issuing Board Members UTxOs ..."

cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$DIGITALIS_REDEEMERS_PATH" \
    "General" \
    "Unit" \
    >/dev/null

UNIT_REDEEMER=$(getDigitalisRedeemer "${GENERAL}" "Unit")
CONTRACTS_CONTROLLER_CONTRACT=$(getDigitalisContract "${CONTRACTS_CONTROLLER_CONTRACT_NAME}")
STAKING_CONTRACT=$(getDigitalisContract "${STAKING_CONTRACT_NAME}")
CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")

main() {

    NUM="$1"
    CONTRACT_NAME="$2"
    FIRST_WALLET="$3"
    SECOND_WALLET="$4"
    THIRD_WALLET="$5"

    FIRST_WALLET_PAYMENT_PKH=$(getWalletPaymentPKH "${FIRST_WALLET}")
    FIRST_WALLET_PAYMENT_SKEY=$(getWalletPaymentSkey "${FIRST_WALLET}")
    SECOND_WALLET_PAYMENT_PKH=$(getWalletPaymentPKH "${SECOND_WALLET}")
    THIRD_WALLET_PAYMENT_PKH=$(getWalletPaymentPKH "${THIRD_WALLET}")

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
    IFS="#" read -r txID txIDx <<<"$txIn"
    collateralTxIn=$(get_address_biggest_lovelace "$COLLATERAL_ADDRESS")

    printf "\033[1;32m[+] Board Member 'No.%s' BMAT Contract: \033[0m" "$NUM"
    cabal run "$WRITE_VALIDATORS_SCRIPT_NAME" \
        "$DIGITALIS_CONTRACTS_PATH" \
        "$DIGITALIS_CORES_PATH" \
        "$BMAT_CONTRACT_NAME" \
        "$NUM" \
        "$FIRST_WALLET_PAYMENT_PKH,$SECOND_WALLET_PAYMENT_PKH,$THIRD_WALLET_PAYMENT_PKH" \
        "$txID" \
        "$txIDx"

    sleep 1

    printf "\n\033[1;32m[+] Board Member 'No.%s' BoardMemberDatum : \033[0m" "$NUM"
    cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
        "$DIGITALIS_DATUMS_PATH" \
        "$CONTRACTS_CONTROLLER_CONTRACT_NAME" \
        "BoardMemberDatum" \
        "$NUM" \
        "$FIRST_WALLET_PAYMENT_PKH,$SECOND_WALLET_PAYMENT_PKH,$THIRD_WALLET_PAYMENT_PKH"

    DATUM=$(getDigitalisDatum "${CONTRACTS_CONTROLLER_CONTRACT_NAME}" "BoardMemberDatum_$NUM")
    CONTRACT=$(getDigitalisContract "${CONTRACT_NAME}")
    BMAT_POLICY_ID=$(getDigitalisContractSH "${CONTRACT_NAME}")

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"$CONTRACT_NAME"_minting.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS + 2000000 + 1 $BMAT_POLICY_ID" \
        --tx-out-inline-datum-file "$DATUM" \
        --mint="1 $BMAT_POLICY_ID" \
        --mint-script-file "$CONTRACT" \
        --mint-redeemer-file "$UNIT_REDEEMER" \
        --required-signer-hash "$FIRST_WALLET_PAYMENT_PKH" \
        >/dev/null

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"$CONTRACT_NAME"_minting.signed \
        --tx-body-file "$DIGITALIS_TX_PATH"/"$CONTRACT_NAME"_minting.body \
        --signing-key-file "$DIGITALIS_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$FIRST_WALLET_PAYMENT_SKEY" \
        >/dev/null

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$DIGITALIS_TX_PATH"/"$CONTRACT_NAME"_minting.signed \
        >/dev/null

    txID=$(get_digitalis_txID "$CONTRACT_NAME""_minting")
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    sleep 1

    echo -e "\033[1;32m[+] $CONTRACT_NAME UTxO Has Been Created Successfully.\033[0m"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The Created Board Member 'No.$NUM' UTxO Is In The Contracts Controller Contract With The Board Member 'PKHs' In Its Datum."

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
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying To Create The '$CONTRACT_NAME' Utxo."
        main "$NUM" "$CONTRACT_NAME" "$FIRST_WALLET" "$SECOND_WALLET" "$THIRD_WALLET"
    fi
}

main "1" "$BMAT_1_CONTRACT_NAME" "$DIGITALIS_BOARD_MEMBER_1_FIRST_WALLET" "$DIGITALIS_BOARD_MEMBER_1_SECOND_WALLET" "$DIGITALIS_BOARD_MEMBER_1_THIRD_WALLET"
main "2" "$BMAT_2_CONTRACT_NAME" "$DIGITALIS_BOARD_MEMBER_2_FIRST_WALLET" "$DIGITALIS_BOARD_MEMBER_2_SECOND_WALLET" "$DIGITALIS_BOARD_MEMBER_2_THIRD_WALLET"
main "3" "$BMAT_3_CONTRACT_NAME" "$DIGITALIS_BOARD_MEMBER_3_FIRST_WALLET" "$DIGITALIS_BOARD_MEMBER_3_SECOND_WALLET" "$DIGITALIS_BOARD_MEMBER_3_THIRD_WALLET"
main "4" "$BMAT_4_CONTRACT_NAME" "$DIGITALIS_BOARD_MEMBER_4_FIRST_WALLET" "$DIGITALIS_BOARD_MEMBER_4_SECOND_WALLET" "$DIGITALIS_BOARD_MEMBER_4_THIRD_WALLET"
main "5" "$BMAT_5_CONTRACT_NAME" "$CORE_TEAM_MEMBER_1_FIRST_WALLET" "$CORE_TEAM_MEMBER_1_SECOND_WALLET" "$CORE_TEAM_MEMBER_1_THIRD_WALLET"
main "6" "$BMAT_6_CONTRACT_NAME" "$CORE_TEAM_MEMBER_2_FIRST_WALLET" "$CORE_TEAM_MEMBER_2_SECOND_WALLET" "$CORE_TEAM_MEMBER_2_THIRD_WALLET"
