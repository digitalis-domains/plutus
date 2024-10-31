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
printf "$RED%b" "<------------------------------------------------------------------------- DNS REDEEMING -------------------------------------------------------------------->"
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
MAIN_TREASURY_CONTRACT=$(getDigitalisContract "${MAIN_TREASURY_CONTRACT_NAME}")
CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")
MAIN_TREASURY_CONTRACT_ADDRESS=$(getContractAddress "$MAIN_TREASURY_CONTRACT" "${STAKING_CONTRACT}")
MINTING_DNS_AUCTION_VALIDATOR_HASH=$(getTLDContractSH "$MINTING_DNS_AUCTION_CONTRACT_NAME" "$domainName")
DNS_MINTER_POLICY_ID=$(getTLDContractSH "$DNS_MINTER_CONTRACT_NAME" "$domainName")
DRAT_POLICY_ID=$(getTLDContractSH "$DRAT_CONTRACT_NAME" "$domainName")
MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${MINTING_DNS_AUCTION_VALIDATOR_HASH}
DRAT_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DRAT_POLICY_ID}
DNS_REFERENCE_CONTRACT=$(getTLDContract "${DNS_REFERENCE_CONTRACT_NAME}" "$domainName")
MINTING_DNS_AUCTION_CONTRACT=$(getTLDContract "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "$domainName")
DNS_REFERENCE_CONTRACT_ADDRESS=$(getContractAddress "$DNS_REFERENCE_CONTRACT" "${STAKING_CONTRACT}")
MINTING_DNS_AUCTION_CONTRACT_ADDRESS=$(getContractAddress "$MINTING_DNS_AUCTION_CONTRACT" "${STAKING_CONTRACT}")

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter the scenario index:"
read -r input
index=$input

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'DNS' token name:"
read -r input
DNSTokenName=$input
DNSTokenName_hex=$(echo -n "${DNSTokenName}" | xxd -p | tr -d '\n')
DNS="${DNS_MINTER_POLICY_ID}.${DNSTokenName_hex}"

auctionTxID=$(cat "$TX_PATH"/"${DNSTokenName}"_"$index"_AuctionUTxO.txID)
auctionUTxO="${auctionTxID}#0"

if [ $? -ne 0 ]; then
    printf "\n$RED%b\n\n" "[-] ERROR: Token not found"
    exit 1
fi

output="/tmp/utxos.json"
cardano-cli query utxo --address "${MINTING_DNS_AUCTION_CONTRACT_ADDRESS}" --testnet-magic "$MAGIC_TESTNET_NUMBER" --out-file $output
auctionUTxO_JSON=$(find_utxo_by_txID_and_JSON "$auctionUTxO" $output)
rm $output

result=$(extract_values "$auctionUTxO_JSON")

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Auction Information:"

highestBid=$(echo "$result" | jq -r '.bid')
highestBid_ada=$(echo "$result" | jq -r '.bid_ada')
echo -e "\033[1;32m[+] The Highest Bid in ADA:\033[0m" "$highestBid_ada"

USER=$(echo "$result" | jq -r '.currentHighestBidder')
echo -e "\033[1;32m[+] The Highest Bidder:\033[0m" "$USER"

USER_ADDRESS=$(getWalletAddr "${USER}")
USER_PKH=$(getWalletPaymentPKH "${USER}")
USER_SKEY=$(getWalletPaymentSkey "${USER}")

printf "$CYAN%b" "[$(date +%Y-%m-c%d\ %H:%M:%S)] Creating necessary files for DNS redeeming ..."

printf "\n\033[1;32m[+] DNSReferenceDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$DNS_REFERENCE_CONTRACT_NAME" \
    "DNSReferenceDatum" \
    "$index" \
    "$DNSTokenName" \
    "" \
    ""

dnsReferenceDatum=$(getTLDDatum "$DNS_REFERENCE_CONTRACT_NAME" "DNSReferenceDatum""_""$DNSTokenName""_""$index" "${domainName}")

sleep 1

printf "\n\033[1;32m[+] MainTreasuryDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DIGITALIS_DATUMS_PATH" \
    "$MAIN_TREASURY_CONTRACT_NAME" \
    "MainTreasuryDatum" \
    "$DNS_AUCTION_FEE_TYPE"

mainTreasuryDatum=$(getDigitalisDatum "$MAIN_TREASURY_CONTRACT_NAME" "MainTreasuryDatum""_""$DNS_AUCTION_FEE_TYPE")

sleep 1

printf "\n\033[1;32m[+] MintDRAT: \033[0m"
cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$REDEEMERS_PATH" \
    "$DRAT_CONTRACT_NAME" \
    "MintDRAT"

mintDRATredeemer=$(getTLDRedeemer "${DRAT_CONTRACT_NAME}" "MintDRAT" "${domainName}")

sleep 1

printf "\n\033[1;32m[+] DNSRedeeming: \033[0m"
cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$REDEEMERS_PATH" \
    "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    "DNSRedeeming"

mintingDNSAuctionDNSRedeemingRedeemer=$(getTLDRedeemer "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "DNSRedeeming" "${domainName}")

sleep 1

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

mintingDNSAuctionRefScriptUTxO=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN")

DRATRefScriptUTxO=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$DRAT_REF_SCRIPT_AUTH_TOKEN")

userTxOut="${USER_ADDRESS} + ${MIN_UTXO} + 1 $DNS"

treasuryTxOut="${MAIN_TREASURY_CONTRACT_ADDRESS} + ${highestBid}"

DRAT="${DRAT_POLICY_ID}.${DNSTokenName_hex}"

dnsReferenceTxOut="${DNS_REFERENCE_CONTRACT_ADDRESS} + ${MIN_UTXO} + 1 ${DRAT}"

echo -e "\033[1;32m[+] Tx Output To DNS Reference Contract Address:\033[0m" "$dnsReferenceTxOut"
sleep 1
echo -e "\033[1;32m[+] Tx Output To Fee Treasury Address:\033[0m" "$treasuryTxOut"
sleep 1
echo -e "\033[1;32m[+] Tx Output To '$USER' Wallet Address:\033[0m" "$userTxOut"

# echo -e "\033[1;32m[+] $mainTreasuryDatum:\033[0m" "$mainTreasuryDatum"
# echo -e "\033[1;32m[+] $dnsReferenceDatum:\033[0m" "$dnsReferenceDatum"
# echo -e "\033[1;32m[+] $mintDRATredeemer:\033[0m" "$mintDRATredeemer"
# echo -e "\033[1;32m[+] $mintingDNSAuctionDNSRedeemingRedeemer:\033[0m" "$mintingDNSAuctionDNSRedeemingRedeemer"

main() {
    nowSlotNumber=$(get_current_slot_number)

    commandOutput=$(
        cardano-cli conway transaction build \
            --testnet-magic "$MAGIC_TESTNET_NUMBER" \
            --out-file "$TX_PATH"/"$USER"_redeem_"${DNSTokenName}"_"$index".body \
            --change-address "$USER_ADDRESS" \
            --tx-in-collateral="$collateralTxIn" \
            --tx-in="$txIn" \
            --tx-in="$auctionUTxO" \
            --tx-out="$userTxOut" \
            --tx-out="$treasuryTxOut" \
            --tx-out-inline-datum-file "$mainTreasuryDatum" \
            --tx-out="$dnsReferenceTxOut" \
            --tx-out-inline-datum-file "$dnsReferenceDatum" \
            --mint="1 ${DRAT}" \
            --mint-tx-in-reference "${DRATRefScriptUTxO}" \
            --mint-plutus-script-v3 \
            --mint-reference-tx-in-redeemer-file "$mintDRATredeemer" \
            --policy-id="${DRAT_POLICY_ID}" \
            --spending-tx-in-reference="$mintingDNSAuctionRefScriptUTxO" \
            --spending-plutus-script-v3 \
            --spending-reference-tx-in-inline-datum-present \
            --spending-reference-tx-in-redeemer-file "$mintingDNSAuctionDNSRedeemingRedeemer" \
            --required-signer-hash "$USER_PKH" \
            --invalid-before "$nowSlotNumber"
    )
    sleep 1
    IFS=':' read -ra outputs <<<"$commandOutput"
    IFS=' ' read -ra fee <<<"${outputs[1]}"
    echo -e "\033[1;32m[+] Tx Fee: \033[0m" "${fee[1]}"

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/"$USER"_redeem_"${DNSTokenName}"_"$index".signed \
        --tx-body-file "$TX_PATH"/"$USER"_redeem_"${DNSTokenName}"_"$index".body \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$USER_SKEY"

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$TX_PATH"/"$USER"_redeem_"${DNSTokenName}"_"$index".signed \
        >/dev/null

    echo -e "\033[1;32m[+] The Bid Disclosure Was Successful.\033[0m"

    txID=$(cardano-cli transaction txid --tx-file "$TX_PATH"/"$USER"_redeem_"${DNSTokenName}"_"$index".signed)
    utxo0="${txID}#0"
    utxo1="${txID}#1"
    utxo2="${txID}#2"
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$utxo0"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation ..."

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The DNS Reference UTxO In The DNS Reference Contract Address:"

    result=""
    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "${DNS_REFERENCE_CONTRACT_ADDRESS}" "$utxo2")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying Transaction for DNS Redeeming on '$DNSTokenName' Again."
        main
    fi

}

main

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The Fee UTxO In The Fee Treasury Address:"

sleep 1

find_utxo_by_txID "${MAIN_TREASURY_CONTRACT_ADDRESS}" "$utxo1"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The DNS UTxO In The '$USER' Wallet Address:"

sleep 1

find_utxo_by_txID "${USER_ADDRESS}" "$utxo0"

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
