#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

set -e

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating CAT Reference Script UTxO ..."

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

    CAT_CONTRACT=$(getDigitalisContract "${CAT_CONTRACT_NAME}")

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"$CAT_CONTRACT_NAME"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$DIGITALIS_WALLET_ADDRESS + 1000000" \
        --tx-out-reference-script-file "$CAT_CONTRACT" \
        2>&1 | tee /tmp/tmp >/dev/null 2>&1

    minAmount=$(sed -n '2p' /tmp/tmp)
    IFS=' ' read -ra outputs <<<"$minAmount"
    IFS=' ' read -ra minAmount <<<"${outputs[3]}"
    echo -e "\033[1;32m[+] Minimum ADA Was Needed For '$CAT_CONTRACT_NAME' Reference Script UTxO:\033[0m" "${minAmount[0]}"
    rm /tmp/tmp

    cardano-cli conway transaction build \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"$CAT_CONTRACT_NAME"_ref_script.body \
        --change-address "$DIGITALIS_WALLET_ADDRESS" \
        --tx-in-collateral="$collateralTxIn" \
        --tx-in="$txIn" \
        --tx-out="$DIGITALIS_WALLET_ADDRESS + ${minAmount[0]}" \
        --tx-out-reference-script-file "$CAT_CONTRACT" \
        >/dev/null

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$DIGITALIS_TX_PATH"/"$CAT_CONTRACT_NAME"_ref_script.signed \
        --tx-body-file "$DIGITALIS_TX_PATH"/"$CAT_CONTRACT_NAME"_ref_script.body \
        --signing-key-file "$DIGITALIS_WALLET_PAYMENT_SKEY" \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        >/dev/null

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$DIGITALIS_TX_PATH"/"$CAT_CONTRACT_NAME"_ref_script.signed \
        >/dev/null

    txID=$(get_digitalis_txID "$CAT_CONTRACT_NAME""_ref_script")
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    sleep 1

    echo -e "\033[1;32m[+] CAT Reference Script UTxO Has Been Created Successfully.\033[0m"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The '$CAT_CONTRACT_NAME Reference UTxO' In Digitalis Wallet:"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation To Get UTxO Information ..."

    result=""
    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "$DIGITALIS_WALLET_ADDRESS" "$txID")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying To Create The '$CAT_CONTRACT_NAME' Reference Script Utxo."
        main
    fi
}

main

# printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please press 'Enter' to continue."
# read -r
