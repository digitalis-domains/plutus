#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

# set -e

# Check if DOMAIN_NAME argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 DOMAIN_NAME"
    exit 1
fi

DOMAIN_NAME="$1"
DATUMS_PATH="${DATA_PATH}/${DOMAIN_NAME}/datums"
TX_PATH="${DATA_PATH}/${DOMAIN_NAME}/transactions"

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating All Reference Script UTxOs ..."

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

CONTRACTS_CONTROLLER_CONTRACT=$(getDigitalisContract "${CONTRACTS_CONTROLLER_CONTRACT_NAME}")
STAKING_CONTRACT=$(getDigitalisContract "${STAKING_CONTRACT_NAME}")
MAIN_TREASURY_CONTRACT=$(getDigitalisContract "${MAIN_TREASURY_CONTRACT_NAME}")

CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")
MAIN_TREASURY_CONTRACT_ADDRESS=$(getContractAddress "$MAIN_TREASURY_CONTRACT" "${STAKING_CONTRACT}")

DIGITALIS_BOARD_MEMBER_1_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_1_POLICY_ID")
DIGITALIS_BOARD_MEMBER_2_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_2_POLICY_ID")
DIGITALIS_BOARD_MEMBER_3_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_3_POLICY_ID")
DIGITALIS_BOARD_MEMBER_4_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_4_POLICY_ID")
DIGITALIS_BOARD_MEMBER_5_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_5_POLICY_ID")
DIGITALIS_BOARD_MEMBER_6_TXID=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$BMAT_6_POLICY_ID")

DNS_REFERENCE_VALIDATOR_HASH=$(getTLDContractSH "$DNS_REFERENCE_CONTRACT_NAME" "$DOMAIN_NAME")
MINTING_DNS_AUCTION_VALIDATOR_HASH=$(getTLDContractSH "$MINTING_DNS_AUCTION_CONTRACT_NAME" "$DOMAIN_NAME")
VICKREY_AUCTION_VALIDATOR_HASH=$(getTLDContractSH "$VICKREY_AUCTION_CONTRACT_NAME" "$DOMAIN_NAME")
VABAT_POLICY_ID=$(getTLDContractSH "$VABAT_CONTRACT_NAME" "$DOMAIN_NAME")
ENGLISH_AUCTION_VALIDATOR_HASH=$(getTLDContractSH "$ENGLISH_AUCTION_CONTRACT_NAME" "$DOMAIN_NAME")
DNS_MINTER_POLICY_ID=$(getTLDContractSH "$DNS_MINTER_CONTRACT_NAME" "$DOMAIN_NAME")
DRAT_POLICY_ID=$(getTLDContractSH "$DRAT_CONTRACT_NAME" "$DOMAIN_NAME")
MHABAT_POLICY_ID=$(getTLDContractSH "$MHABAT_CONTRACT_NAME" "$DOMAIN_NAME")

DNS_REFERENCE_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DNS_REFERENCE_VALIDATOR_HASH}
MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${MINTING_DNS_AUCTION_VALIDATOR_HASH}
VICKREY_AUCTION_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${VICKREY_AUCTION_VALIDATOR_HASH}
VABAT_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${VABAT_POLICY_ID}
ENGLISH_AUCTION_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${ENGLISH_AUCTION_VALIDATOR_HASH}
DNS_MINTER_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DNS_MINTER_POLICY_ID}
DRAT_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DRAT_POLICY_ID}
MHABAT_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${MHABAT_POLICY_ID}

DNS_REFERENCE_CONTRACT=$(getTLDContract "${DNS_REFERENCE_CONTRACT_NAME}" "$DOMAIN_NAME")
MINTING_DNS_AUCTION_CONTRACT=$(getTLDContract "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "$DOMAIN_NAME")
VICKREY_AUCTION_CONTRACT=$(getTLDContract "${VICKREY_AUCTION_CONTRACT_NAME}" "$DOMAIN_NAME")
ENGLISH_AUCTION_CONTRACT=$(getTLDContract "${ENGLISH_AUCTION_CONTRACT_NAME}" "$DOMAIN_NAME")
VABAT_CONTRACT=$(getTLDContract "${VABAT_CONTRACT_NAME}" "$DOMAIN_NAME")
DNS_MINTER_CONTRACT=$(getTLDContract "${DNS_MINTER_CONTRACT_NAME}" "$DOMAIN_NAME")
DRAT_CONTRACT=$(getTLDContract "${DRAT_CONTRACT_NAME}" "$DOMAIN_NAME")
MHABAT_CONTRACT=$(getTLDContract "${MHABAT_CONTRACT_NAME}" "$DOMAIN_NAME")

DNS_REFERENCE_CONTRACT_ADDRESS=$(getContractAddress "$DNS_REFERENCE_CONTRACT" "${STAKING_CONTRACT}")
MINTING_DNS_AUCTION_CONTRACT_ADDRESS=$(getContractAddress "$MINTING_DNS_AUCTION_CONTRACT" "${STAKING_CONTRACT}")
VICKREY_AUCTION_CONTRACT_ADDRESS=$(getContractAddress "$VICKREY_AUCTION_CONTRACT" "${STAKING_CONTRACT}")
ENGLISH_AUCTION_CONTRACT_ADDRESS=$(getContractAddress "$ENGLISH_AUCTION_CONTRACT" "${STAKING_CONTRACT}")

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
        --out-file "$TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
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
        --out-file "$TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
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
        --out-file "$TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.signed \
        --tx-body-file "$TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.body \
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
        --tx-file "$TX_PATH"/create_"${CONTRACT_NAME}"_ref_script.signed \
        >/dev/null

    sleep 1

    txID=$(get_tld_txID "create_""$CONTRACT_NAME""_ref_script" "$DOMAIN_NAME")
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

    add_TLD_ref_txid "$DOMAIN_NAME" "$CONTRACT_NAME" "$txID"

    # printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please press 'Enter' to continue."
    # read -r
}

##
## DNSMinter
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$DNS_MINTER_CONTRACT_NAME' Reference Script UTxO ..."

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$DNS_MINTER_CONTRACT_NAME" \
    --arg policy_id "$DNS_MINTER_POLICY_ID" \
    '.[$domain_name][$contract_name] |= (. + { "PolicyID": $policy_id }) // { "PolicyID": $policy_id }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] DNSMinterRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$DNS_MINTER_CONTRACT_NAME" \
    "DNSMinterRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_ADMIN_1_PAYMENT_PKH" \
    "$MINTING_DNS_AUCTION_CONTRACT_ADDRESS" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$DNS_MINTING_FEE_TYPE"
DNS_MINTER_REF_SCRIPT_DATUM=$(getTLDDatum "${DNS_MINTER_CONTRACT_NAME}" "DNSMinterRefScriptDatum" "${DOMAIN_NAME}")
main "$DNS_MINTER_CONTRACT_NAME" "$DNS_MINTER_CONTRACT" "$DNS_MINTER_REF_SCRIPT_AUTH_TOKEN" "$DNS_MINTER_REF_SCRIPT_DATUM"

##
## MintingDNSAuction
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$MINTING_DNS_AUCTION_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1
echo -e "\033[1;32m[+] The '$MINTING_DNS_AUCTION_CONTRACT_NAME' Contract Address: \033[0m" "$MINTING_DNS_AUCTION_CONTRACT_ADDRESS"

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    --arg address "$MINTING_DNS_AUCTION_CONTRACT_ADDRESS" \
    '.[$domain_name][$contract_name] |= (. + { "Address": $address }) // { "Address": $address }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] MintingDNSAuctionRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    "MintingDNSAuctionRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$DRAT_POLICY_ID" \
    "$MHABAT_POLICY_ID" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$DNS_AUCTION_FEE_TYPE"
MINTING_DNS_AUCTION_REF_SCRIPT_DATUM=$(getTLDDatum "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "MintingDNSAuctionRefScriptDatum" "${DOMAIN_NAME}")
main "$MINTING_DNS_AUCTION_CONTRACT_NAME" "$MINTING_DNS_AUCTION_CONTRACT" "$MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN" "$MINTING_DNS_AUCTION_REF_SCRIPT_DATUM"

##
## MHABAT
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$MHABAT_CONTRACT_NAME' Reference Script UTxO ..."

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$MHABAT_CONTRACT_NAME" \
    --arg policy_id "$MHABAT_POLICY_ID" \
    '.[$domain_name][$contract_name] |= (. + { "PolicyID": $policy_id }) // { "PolicyID": $policy_id }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] MHABATRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$MHABAT_CONTRACT_NAME" \
    "MHABATRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$MINTING_DNS_AUCTION_CONTRACT_ADDRESS" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$DNS_AUCTION_BIDDING_FEE_TYPE"
MHABAT_REF_SCRIPT_DATUM=$(getTLDDatum "${MHABAT_CONTRACT_NAME}" "MHABATRefScriptDatum" "${DOMAIN_NAME}")
main "$MHABAT_CONTRACT_NAME" "$MHABAT_CONTRACT" "$MHABAT_REF_SCRIPT_AUTH_TOKEN" "$MHABAT_REF_SCRIPT_DATUM"

##
## DNSReference
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$DNS_REFERENCE_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1
echo -e "\033[1;32m[+] The '$DNS_REFERENCE_CONTRACT_NAME' Contract Address: \033[0m" "$DNS_REFERENCE_CONTRACT_ADDRESS"

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$DNS_REFERENCE_CONTRACT_NAME" \
    --arg address "$DNS_REFERENCE_CONTRACT_ADDRESS" \
    '.[$domain_name][$contract_name] |= (. + { "Address": $address }) // { "Address": $address }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] DNSReferenceRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$DNS_REFERENCE_CONTRACT_NAME" \
    "DNSReferenceRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DRAT_POLICY_ID" \
    "$DNS_MINTER_POLICY_ID"
DNS_REFERENCE_REF_SCRIPT_DATUM=$(getTLDDatum "${DNS_REFERENCE_CONTRACT_NAME}" "DNSReferenceRefScriptDatum" "${DOMAIN_NAME}")
main "$DNS_REFERENCE_CONTRACT_NAME" "$DNS_REFERENCE_CONTRACT" "$DNS_REFERENCE_REF_SCRIPT_AUTH_TOKEN" "$DNS_REFERENCE_REF_SCRIPT_DATUM"

##
## DRAT
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$DRAT_CONTRACT_NAME' Reference Script UTxO ..."

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$DRAT_CONTRACT_NAME" \
    --arg policy_id "$DRAT_POLICY_ID" \
    '.[$domain_name][$contract_name] |= (. + { "PolicyID": $policy_id }) // { "PolicyID": $policy_id }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] DRATRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$DRAT_CONTRACT_NAME" \
    "DRATRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$MINTING_DNS_AUCTION_CONTRACT_ADDRESS" \
    "$DNS_REFERENCE_CONTRACT_ADDRESS"
DRAT_REF_SCRIPT_DATUM=$(getTLDDatum "${DRAT_CONTRACT_NAME}" "DRATRefScriptDatum" "${DOMAIN_NAME}")
main "$DRAT_CONTRACT_NAME" "$DRAT_CONTRACT" "$DRAT_REF_SCRIPT_AUTH_TOKEN" "$DRAT_REF_SCRIPT_DATUM"

##
## VickreyAuction
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$VICKREY_AUCTION_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1
echo -e "\033[1;32m[+] The '$VICKREY_AUCTION_CONTRACT_NAME' Contract Address: \033[0m" "$VICKREY_AUCTION_CONTRACT_ADDRESS"

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$VICKREY_AUCTION_CONTRACT_NAME" \
    --arg address "$VICKREY_AUCTION_CONTRACT_ADDRESS" \
    '.[$domain_name][$contract_name] |= (. + { "Address": $address }) // { "Address": $address }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] VickreyAuctionRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$VICKREY_AUCTION_CONTRACT_NAME" \
    "VickreyAuctionRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$VABAT_POLICY_ID" \
    "$PRECISION,$MIN_FEE,$FEE_PERCENTAGE,$TRADE_THRESHOLD" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$AUCTION_FEE_TYPE"
VICKREY_AUCTION_REF_SCRIPT_DATUM=$(getTLDDatum "${VICKREY_AUCTION_CONTRACT_NAME}" "VickreyAuctionRefScriptDatum" "${DOMAIN_NAME}")
main "$VICKREY_AUCTION_CONTRACT_NAME" "$VICKREY_AUCTION_CONTRACT" "$VICKREY_AUCTION_REF_SCRIPT_AUTH_TOKEN" "$VICKREY_AUCTION_REF_SCRIPT_DATUM"

##
## VABAT
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$VABAT_CONTRACT_NAME' Reference Script UTxO ..."

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$VABAT_CONTRACT_NAME" \
    --arg policy_id "$VABAT_POLICY_ID" \
    '.[$domain_name][$contract_name] |= (. + { "PolicyID": $policy_id }) // { "PolicyID": $policy_id }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] VABATRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$VABAT_CONTRACT_NAME" \
    "VABATRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$VICKREY_AUCTION_CONTRACT_ADDRESS" \
    "$SEAT_COST"
VABAT_REF_SCRIPT_DATUM=$(getTLDDatum "${VABAT_CONTRACT_NAME}" "VABATRefScriptDatum" "${DOMAIN_NAME}")
main "$VABAT_CONTRACT_NAME" "$VABAT_CONTRACT" "$VABAT_REF_SCRIPT_AUTH_TOKEN" "$VABAT_REF_SCRIPT_DATUM"

##
## EnglishAuction
##

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating '$ENGLISH_AUCTION_CONTRACT_NAME' Reference Script UTxO ..."
sleep 1
echo -e "\033[1;32m[+] The '$ENGLISH_AUCTION_CONTRACT_NAME' Contract Address: \033[0m" "$ENGLISH_AUCTION_CONTRACT_ADDRESS"

jq --arg domain_name "$DOMAIN_NAME" \
    --arg contract_name "$ENGLISH_AUCTION_CONTRACT_NAME" \
    --arg address "$ENGLISH_AUCTION_CONTRACT_ADDRESS" \
    '.[$domain_name][$contract_name] |= (. + { "Address": $address }) // { "Address": $address }' \
    "${REPO_HOME}"/config.json >"${REPO_HOME}"/config.json.tmp
mv "${REPO_HOME}"/config.json.tmp "${REPO_HOME}"/config.json

sleep 1
printf "\n\033[1;32m[+] EnglishAuctionRefScriptDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$ENGLISH_AUCTION_CONTRACT_NAME" \
    "EnglishAuctionRefScriptDatum" \
    "$DOMAIN_NAME" \
    "$DNS_MINTER_POLICY_ID" \
    "$BID_DIFF" \
    "$PRECISION,$MIN_FEE,$FEE_PERCENTAGE,$TRADE_THRESHOLD" \
    "$MAIN_TREASURY_CONTRACT_ADDRESS" \
    "$AUCTION_FEE_TYPE"
ENGLISH_AUCTION_REF_SCRIPT_DATUM=$(getTLDDatum "${ENGLISH_AUCTION_CONTRACT_NAME}" "EnglishAuctionRefScriptDatum" "${DOMAIN_NAME}")
main "$ENGLISH_AUCTION_CONTRACT_NAME" "$ENGLISH_AUCTION_CONTRACT" "$ENGLISH_AUCTION_REF_SCRIPT_AUTH_TOKEN" "$ENGLISH_AUCTION_REF_SCRIPT_DATUM"
