#!/bin/bash

. ../env/general.conf
. ../env/wallets.conf

reset
clear
# set -e

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
printf "$RED%b" "<-------------------------------------------------------------------------- MINTING DNS --------------------------------------------------------------------->"
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
DNS_MINTER_POLICY_ID=$(getTLDContractSH "$DNS_MINTER_CONTRACT_NAME" "$domainName")
DNS_MINTER_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${DNS_MINTER_POLICY_ID}
MINTING_DNS_AUCTION_CONTRACT=$(getTLDContract "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "$domainName")
MINTING_DNS_AUCTION_CONTRACT_ADDRESS=$(getContractAddress "$MINTING_DNS_AUCTION_CONTRACT" "${STAKING_CONTRACT}")

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter an index for starting a scenario:"
read -r input
index=$input

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'DNS' token name to mint:"
read -r input
DNSTokenName=$input

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'bidding period' in minutes:"
read -r input
biddingPeriod=$((input * 60 * 1000))
biddingDeadline=$(($(get_current_POSIX_time) + biddingPeriod))

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'revealing period' in minutes:"
read -r input
revealingPeriod=$((input * 60 * 1000))
revealingDeadline=$((biddingDeadline + revealingPeriod))

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'seat cost' in ADA:"
read -r input
# seatCost=$input
seatCost=$((input * 1000000))

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'bidding fee' in ADA:"
read -r input
# biddingFee=$input
biddingFee=$((input * 1000000))

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'first bid' in ADA:"
read -r input
# firstBid=$input
firstBid=$((input * 1000000))

printf "$CYAN%b" "[$(date +%Y-%m-c%d\ %H:%M:%S)] Creating necessary files for minting ..."

printf "\n\033[1;32m[+] MintingDNSAuction - AuctionDatum: \033[0m"

cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DATUMS_PATH" \
    "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    "AuctionDatum" \
    "$index" \
    "$DNSTokenName" \
    "$DNS_MINTER_POLICY_ID" \
    "$biddingDeadline" \
    "$revealingDeadline" \
    "$seatCost" \
    "$biddingFee" \
    "$USER_1_ADDRESS" \
    "$firstBid"

mintingDNSAuctionDatum=$(getTLDDatum "$MINTING_DNS_AUCTION_CONTRACT_NAME" "AuctionDatum""_""$index" "${domainName}")

sleep 1

printf "\n\033[1;32m[+] MintingDNSAuction - MintDNS: \033[0m"
cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$REDEEMERS_PATH" \
    "$DNS_MINTER_CONTRACT_NAME" \
    "MintDNS" \
    "$index" \
    "$DNSTokenName"

mintDNSRedeemer=$(getTLDRedeemer "${DNS_MINTER_CONTRACT_NAME}" "MintDNS""_""$index" "${domainName}")

sleep 1

printf "\n\033[1;32m[+] MainTreasuryDatum: \033[0m"
cabal run "$WRITE_DATUMS_SCRIPT_NAME" \
    "$DIGITALIS_DATUMS_PATH" \
    "$MAIN_TREASURY_CONTRACT_NAME" \
    "MainTreasuryDatum" \
    "$DNS_MINTING_FEE_TYPE"

mainTreasuryDatum=$(getDigitalisDatum "$MAIN_TREASURY_CONTRACT_NAME" "MainTreasuryDatum""_""$DNS_MINTING_FEE_TYPE")

hexTokenName=$(echo -n "${DNSTokenName}" | xxd -p | tr -d '\n')
DNS="${DNS_MINTER_POLICY_ID}.${hexTokenName}"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Minting '$DNSTokenName' ..."

if [[ -f "/tmp/$USER_1.json" ]]; then
    rm /tmp/"$USER_1".json
fi
cardano-cli query utxo \
    --address "$USER_1_ADDRESS" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --out-file /tmp/"$USER_1".json
if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' /tmp/"$USER_1".json) && -f "/tmp/$USER_1.json" ]]; then
    printf "\n$RED%b\n\n" "[-] ERROR: NO Any UTxOs Found At '$USER_1' Address"
    exit 1
fi

txIn=$(get_address_biggest_lovelace "$USER_1_ADDRESS")

collateralTxIn=$(get_address_biggest_lovelace "$COLLATERAL_ADDRESS")

DNSMinterRefScriptUTxO=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$DNS_MINTER_REF_SCRIPT_AUTH_TOKEN")

auctionTxOut="${MINTING_DNS_AUCTION_CONTRACT_ADDRESS} + ${firstBid} + 1 $DNS"

treasuryTxOut="${MAIN_TREASURY_CONTRACT_ADDRESS} + ${biddingFee}"

sleep 1

echo -e "\033[1;32m[+] Tx Output To Auction Contract Address:\033[0m" "$auctionTxOut"
sleep 1
echo -e "\033[1;32m[+] Tx Output To Fee Treasury:\033[0m" "$treasuryTxOut"

main() {
    nowSlotNumber=$(get_current_slot_number)
    submissionTime=$((nowSlotNumber + 60))

    commandOutput=$(
        cardano-cli conway transaction build \
            --testnet-magic "$MAGIC_TESTNET_NUMBER" \
            --out-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".body \
            --change-address "$USER_1_ADDRESS" \
            --tx-in-collateral="$collateralTxIn" \
            --tx-in="$txIn" \
            --tx-out="$auctionTxOut" \
            --tx-out-inline-datum-file "$mintingDNSAuctionDatum" \
            --tx-out="$treasuryTxOut" \
            --tx-out-inline-datum-file "$mainTreasuryDatum" \
            --mint="1 $DNS" \
            --mint-tx-in-reference "${DNSMinterRefScriptUTxO}" \
            --mint-plutus-script-v3 \
            --mint-reference-tx-in-redeemer-file "$mintDNSRedeemer" \
            --policy-id="${DNS_MINTER_POLICY_ID}" \
            --required-signer-hash "$USER_1_PAYMENT_PKH" \
            --required-signer-hash "$DNS_ADMIN_1_PAYMENT_PKH" \
            --invalid-hereafter "$submissionTime"
    )
    sleep 1
    IFS=':' read -ra outputs <<<"$commandOutput"
    IFS=' ' read -ra fee <<<"${outputs[1]}"
    echo -e "\033[1;32m[+] Tx Fee: \033[0m" "${fee[1]}"

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".signed \
        --tx-body-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".body \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$USER_1_PAYMENT_SKEY" \
        --signing-key-file "$DNS_ADMIN_1_PAYMENT_SKEY" \
        >/dev/null

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".signed \
        >/dev/null

    echo -e "\033[1;32m[+] '$DNSTokenName' Has Been Successfully Minted.\033[0m"

    cardano-cli transaction txid --tx-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".signed | tee "$TX_PATH"/"${DNSTokenName}"_"$index"_AuctionUTxO.txID >/dev/null

    txID=$(cardano-cli transaction txid --tx-file "$TX_PATH"/"${DNSTokenName}"_mint_"$index".signed)
    utxo0="${txID}#0"
    utxo1="${txID}#1"

    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation ..."

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The Bid UTxO In The DNS Auction Contract Address:"

    result=""
    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "${MINTING_DNS_AUCTION_CONTRACT_ADDRESS}" "$utxo0")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying Transaction To Mint '$DNSTokenName' Again."
        main
    fi

}

main

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The Fee UTxO In The Fee Treasury Address:"

sleep 1

find_utxo_by_txID "${MAIN_TREASURY_CONTRACT_ADDRESS}" "$utxo1"

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
