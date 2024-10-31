#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

reset
clear
set -e

# Logo
printf "\n"
printf "$BLUE%b" "                      %@@@@@@@%                                                                                                                               "
printf "$BLUE%b" "                    %@@@@@@@@@@@@%                       %%%@@%                                                                %%%@%%                         "
printf "$BLUE%b" "       %@@@@%     %@@@@@@@@@@@@@@@%                       %@@@%                                                                 %%@%%                         "
printf "$BLUE%b" "      %@@@@@@%   %@@@@@@@@@@@@@@@@@%                       @@@%     @@@%                     %@@@%                               %@%%     %@@%                "
printf "$BLUE%b" "      %@@@@@@%   %@@@@@@@@@@@@@@@@@%                       @@@%    %@@@%                     %@@@%      %%                       %@%%    %@@@@%               "
printf "$BLUE%b" "       %@@@@%    @@@@@@@@@@@@@@@@@@%                       @@@%                               %%%      @@%                       %@%%                         "
printf "$BLUE%b" "                 %@@@@@@@@@@@@@@@@@%                       @@@%                                       %@@%                       %@%%                         "
printf "$BLUE%b" "                  %@@@@@@@@@@@@@@@@                %%%%%%@@@@@%    %@@@%      %@%%%%%@@@@@% %%@@@%  %%%@@@%%%%    %%%%@@@%       %@%%   %%%@@%     %%%%%%@@%  "
printf "$BLUE%b" "       %@@@@@@@@%  %@@@@@@@@@@@@@%               %@@%     %@@@%     %@@%    %@@%    %@@%%%   %@@@%    %@@%      %@%    %@@%      %@%%    %@@@%    %@@     %%  "
printf "$BLUE%b" "     %@@@@@@@@@@@@%  %%@@@@@@@%%                %@@%       @@@%     @@@%    @@@%     %@@@     %@@%    %@@%      %@%     @@@%     %@%%     %@@%    @@@%        "
printf "$BLUE%b" "   %@@@@@@@@@@@@@@@@                           %@@@        @@@%     @@@%   %@@@%     %@@@     @@@%    %@@%              %@@%     %@%%     %@@%    %@@@@%      "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@                          @@@%        @@@%     @@@%    %@@%     @@@%     @@@%    %@@%         %%%%%%@@%     %@%%     %@@%     %%@@@@@%   "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@%    %@@@%%               %@@@%       @@@%     @@@%     %@@@%%%%@%       @@@%    %@@%      %@@%    %@@%     %@%%     %@@%         %@@@@% "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@%   %@@@@@@%              %@@@%       @@@%     @@@%     %%%%%%%%         %@@%    %@@%     %@@%     %@@%     %@%%     %@@%   %%      %@@% "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@    @@@@@@@%               %@@@@%   %%@@@%     @@@%    @@@%%             @@@%    %@@@%    %@@@%   %@@@%     %@%%     %@@%   %@%     %@@  "
printf "$BLUE%b" "   %@@@@@@@@@@@@@@@@%    %%@@@@%                  %@@@@@%%%%@@%%% %%%@@@%%  %%@@@@@@@@@@@%  %%@@@@%%   %@@@@%%  %%@@@@%%%@@@%% %%%@@@%% %%@@@@%%  %@@%%%%%%   "
printf "$BLUE%b" "    %%@@@@@@@@@@@@%                                                           %%%%%%%%%@@@%                                                                   "
printf "$BLUE%b" "       %@@@@@@@@%                                                           %@%        %@@%                                                                   "
printf "$BLUE%b" "                                                                           @@%%        %@@%                                                                   "
printf "$BLUE%b" "                                                                           %@@%        %%%                                                                    "
printf "$BLUE%b" "                                                                            %@@@@%%%%%%%                                                                      "
printf "$BLUE%b" "                                                                               %@@@%%                                                                         "
printf "\n"
printf "$YELLOW%b" "                                                                          D I G I T A L I S                                                                 "
printf "$GREEN%b" "                                                                     https://digitalis.domains/                                                              "
printf "\n"
printf "$RED%b" "<--------------------------------------------------------------------- DNS REFERENCE UPDATING --------------------------------------------------------------->"
printf "\n\n"

sleep 1

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Enter The TLD/Domain-Name (Please Add Dot '.' at the beginning): "
read -r input
domainName=$input

DATUMS_PATH="${DATA_PATH}/${domainName}/datums"
REDEEMERS_PATH="${DATA_PATH}/${domainName}/redeemers"
TX_PATH="${DATA_PATH}/${domainName}/transactions"

CAT_POLICY_ID=$(getDigitalisContractSH "${CAT_CONTRACT_NAME}")
CONTRACTS_CONTROLLER_CONTRACT=$(getDigitalisContract "${CONTRACTS_CONTROLLER_CONTRACT_NAME}")
STAKING_CONTRACT=$(getDigitalisContract "${STAKING_CONTRACT_NAME}")
CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")
DNS_REFERENCE_VALIDATOR_HASH=$(getTLDContractSH "$DNS_REFERENCE_CONTRACT_NAME" "$domainName")
DNS_MINTER_POLICY_ID=$(getTLDContractSH "$DNS_MINTER_CONTRACT_NAME" "$domainName")
DRAT_POLICY_ID=$(getTLDContractSH "$DRAT_CONTRACT_NAME" "$domainName")
DNS_REFERENCE_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DNS_REFERENCE_VALIDATOR_HASH}
DNS_REFERENCE_CONTRACT=$(getTLDContract "${DNS_REFERENCE_CONTRACT_NAME}" "$domainName")
DNS_REFERENCE_CONTRACT_ADDRESS=$(getContractAddress "$DNS_REFERENCE_CONTRACT" "${STAKING_CONTRACT}")

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter the scenario index:"
read -r input
index=$input

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'DNS' token name:"
read -r input
DNSTokenName=$input
DNSTokenName_hex=$(echo -n "${DNSTokenName}" | xxd -p | tr -d '\n')

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'User Index' (1 to 7):"
read -r input

if [[ $input -ge 1 && $input -le 7 ]]; then
    USER=$(selectUser "$input")

else
    echo "Invalid User Index. Please enter a number between 1 and 7."
    exit 1
fi

USER_ADDRESS=$(getWalletAddr "${USER}")
USER_SKEY=$(getWalletPaymentSkey "${USER}")

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Current State of The DNS Reference UTxO:"

DRAT="${DRAT_POLICY_ID}.${DNSTokenName_hex}"

contractTxIn=$(get_UTxO_by_token "$DNS_REFERENCE_CONTRACT_ADDRESS" "$DRAT")
find_utxo_by_txID "${DNS_REFERENCE_CONTRACT_ADDRESS}" "$contractTxIn"

NUM=1
DNS_JSON="{"
DNS_STRING=""

collect_records() {
    local LHS TTL RType RData
    printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Enter 'LHS' for DNS Record $NUM:"
    read -r LHS
    printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Enter 'TTL' for DNS Record $NUM:"
    read -r TTL
    printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Enter 'RType' for DNS Record $NUM:"
    read -r RType
    printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Enter 'RData' for DNS Record $NUM:"
    read -r RData

    DNS_JSON+="\"DNS Record $NUM\":{"
    DNS_JSON+="\"LHS\":\"$LHS\","
    DNS_JSON+="\"TTL\":\"$TTL\","
    DNS_JSON+="\"RType\":\"$RType\","
    DNS_JSON+="\"RData\":\"$RData\""
    DNS_JSON+="},"

    DNS_STRING+="$LHS,$TTL,$RType,$RData;"

    printf "$WHITE%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Do you want to add more records? (y/n):"
    read -r choice
    if [ "$choice" = "y" ] || [ "$choice" = "Y" ]; then
        NUM=$((NUM + 1))
        collect_records
    fi
}

collect_records

DNS_JSON=${DNS_JSON%,}
DNS_JSON+="}"
pretty_DNS_json=$(echo "$DNS_JSON" | jq .)
echo "$pretty_DNS_json"

printf "$CYAN%b" "[$(date +%Y-%m-c%d\ %H:%M:%S)] Creating necessary files for DNS record updating ..."

printf "\n\033[1;32m[+] DNSReferenceDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$DNS_REFERENCE_CONTRACT_NAME" \
    "DNSReferenceDatum" \
    "$index" \
    "$DNSTokenName" \
    "$DNS_STRING" \
    "additionalData,42"

contractTxOutDatum=$(getTLDDatum "$DNS_REFERENCE_CONTRACT_NAME" "DNSReferenceDatum""_""$DNSTokenName""_""$index" "${domainName}")

printf "\n\033[1;32m[+] Update: \033[0m"
cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$REDEEMERS_PATH" \
    "$DNS_REFERENCE_CONTRACT_NAME" \
    "Update"

updateRedeemer=$(getTLDRedeemer "${DNS_REFERENCE_CONTRACT_NAME}" "Update" "${domainName}")

if [[ -f "/tmp/$USER.json" ]]; then
    rm /tmp/"$USER".json
fi
cardano-cli query utxo \
    --address "$USER_ADDRESS" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --out-file /tmp/"$USER".json
if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' /tmp/"$USER".json) && -f "/tmp/$USER.json" ]]; then
    printf "\n$RED%b\n\n" "[-] ERROR: NO Any UTxOs Found At '$USER' Address"
    exit 1
fi

txIn=$(get_address_biggest_lovelace "$USER_ADDRESS")

collateralTxIn=$(get_address_biggest_lovelace "$COLLATERAL_ADDRESS")

dnsReferenceRefScriptUTxO=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$DNS_REFERENCE_REF_SCRIPT_AUTH_TOKEN")

contractTxIn=$(get_UTxO_by_token "$DNS_REFERENCE_CONTRACT_ADDRESS" "$DRAT")

contractTxInAmount=$(get_UTxO_lovelace_amount "$DNS_REFERENCE_CONTRACT_ADDRESS" "$contractTxIn")

DNS="${DNS_MINTER_POLICY_ID}.${DNSTokenName_hex}"

dnsTxIn=$(get_UTxO_by_token "$USER_ADDRESS" "$DNS")

dnsTxInAmount=$(get_UTxO_lovelace_amount "$USER_ADDRESS" "$dnsTxIn")

dnsTxOut="${USER_ADDRESS} + ${dnsTxInAmount} + 1 ${DNS}"

contractTxOut="${DNS_REFERENCE_CONTRACT_ADDRESS} + ${contractTxInAmount} + 1 ${DRAT}"

main() {
    commandOutput=$(
        cardano-cli conway transaction build \
            --testnet-magic "$MAGIC_TESTNET_NUMBER" \
            --out-file "$TX_PATH"/"$USER"_update_dns_record_on_"${DNSTokenName}"_"$index".body \
            --change-address "$USER_ADDRESS" \
            --tx-in-collateral="$collateralTxIn" \
            --tx-in="$txIn" \
            --tx-in="$dnsTxIn" \
            --tx-in="$contractTxIn" \
            --tx-out="$contractTxOut" \
            --tx-out-inline-datum-file "$contractTxOutDatum" \
            --tx-out="$dnsTxOut" \
            --spending-tx-in-reference="$dnsReferenceRefScriptUTxO" \
            --spending-plutus-script-v3 \
            --spending-reference-tx-in-inline-datum-present \
            --spending-reference-tx-in-redeemer-file "$updateRedeemer"
    )
    sleep 1
    IFS=':' read -ra outputs <<<"$commandOutput"
    IFS=' ' read -ra fee <<<"${outputs[1]}"
    echo -e "\033[1;32m[+] Tx Fee: \033[0m" "${fee[1]}"

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/"$USER"_update_dns_record_on_"${DNSTokenName}"_"$index".signed \
        --tx-body-file "$TX_PATH"/"$USER"_update_dns_record_on_"${DNSTokenName}"_"$index".body \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$USER_SKEY"

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$TX_PATH"/"$USER"_update_dns_record_on_"${DNSTokenName}"_"$index".signed \
        >/dev/null

    echo -e "\033[1;32m[+] The Bid Claiming Was Successful.\033[0m"

    txID=$(get_tld_txID "$USER"_update_dns_record_on_"${DNSTokenName}"_"$index" "$domainName")

    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation ..."

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The Updated DNS Reference UTxO In The DNS Reference Contract Address:"

    result=""
    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "${DNS_REFERENCE_CONTRACT_ADDRESS}" "$txID")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying Transaction for Bid Claiming on '$DNSTokenName' Again."
        main
    fi
}

main

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
