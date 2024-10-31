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
printf "$RED%b" "<------------------------------------------------------------------------- BID DISCLOSURE ------------------------------------------------------------------->"
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
MINTING_DNS_AUCTION_VALIDATOR_HASH=$(getTLDContractSH "$MINTING_DNS_AUCTION_CONTRACT_NAME" "$domainName")
DNS_MINTER_POLICY_ID=$(getTLDContractSH "$DNS_MINTER_CONTRACT_NAME" "$domainName")
MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN=${CAT_POLICY_ID}.${MINTING_DNS_AUCTION_VALIDATOR_HASH}
MINTING_DNS_AUCTION_CONTRACT=$(getTLDContract "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "$domainName")
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

biddingDeadline=$(echo "$result" | jq -r '.biddingDeadline')
biddingDeadline_human=$(echo "$result" | jq -r '.biddingDeadline_human')
echo -e "\033[1;32m[+] Bidding Deadline:\033[0m" "$biddingDeadline_human"

revealingDeadline=$(echo "$result" | jq -r '.revealingDeadline')
revealingDeadline_human=$(echo "$result" | jq -r '.revealingDeadline_human')
echo -e "\033[1;32m[+] Reveal Deadline:\033[0m" "$revealingDeadline_human"

currentHighestBid=$(echo "$result" | jq -r '.bid')
currentHighestBid_ada=$(echo "$result" | jq -r '.bid_ada')
echo -e "\033[1;32m[+] Current Highest Bid in ADA:\033[0m" "$currentHighestBid_ada"

currentHighestBidder=$(echo "$result" | jq -r '.currentHighestBidder')
echo -e "\033[1;32m[+] Current Highest Bidder:\033[0m" "$currentHighestBidder"

seatCost_ada=$(echo "$result" | jq -r '.seatCost_ada')
seatCost=$(echo "$result" | jq -r '.seatCost')
echo -e "\033[1;32m[+] Seat cost in ADA:\033[0m" "$seatCost_ada"

biddingFee=$(echo "$result" | jq -r '.biddingFee')
biddingFee_ada=$(echo "$result" | jq -r '.biddingFee_ada')
echo -e "\033[1;32m[+] Bidding fee in ADA:\033[0m" "$biddingFee_ada"

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'User Index' (1 to 7):"
read -r input

if [[ $input -ge 1 && $input -le 7 ]]; then
    USER=$(selectUser "$input")

else
    echo "Invalid User Index. Please enter a number between 1 and 7."
    exit 1
fi

USER_ADDRESS=$(getWalletAddr "${USER}")
USER_PKH=$(getWalletPaymentPKH "${USER}")
USER_SKEY=$(getWalletPaymentSkey "${USER}")

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'Bidding' amount in ADA:"
read -r input
# biddingAmount=$input
biddingAmount=$((input * 1000000))

printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please enter 'Salt' aka secret word:"
read -r input
salt=$input

printf "$CYAN%b" "[$(date +%Y-%m-c%d\ %H:%M:%S)] Creating necessary files for bid disclosure ..."

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
    "$USER_ADDRESS" \
    "$biddingAmount"

mintingDNSAuctionDatum=$(getTLDDatum "$MINTING_DNS_AUCTION_CONTRACT_NAME" "AuctionDatum""_""$index" "${domainName}")

sleep 1

printf "\n\033[1;32m[+] MintingDNSAuction - BidDisclosure: \033[0m"
cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$REDEEMERS_PATH" \
    "$MINTING_DNS_AUCTION_CONTRACT_NAME" \
    "BidDisclosure" \
    "$index" \
    "$USER" \
    "$biddingAmount" \
    "$salt"

mintingDNSAuctionBidDisclosureRedeemer=$(getTLDRedeemer "${MINTING_DNS_AUCTION_CONTRACT_NAME}" "$USER""_""BidDisclosure""_""$index" "${domainName}")

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

previousHighestBidderTxOut="$(getWalletAddr "$currentHighestBidder") + ${currentHighestBid}"

contractTxOut="${MINTING_DNS_AUCTION_CONTRACT_ADDRESS} + ${biddingAmount} + 1 $DNS"

bidTxID=$(get_tld_txID "$USER"_bidding_"$biddingAmount"_on_"${DNSTokenName}"_"$index" "$domainName")

echo -e "\033[1;32m[+] Tx Output To Auction Contract Address:\033[0m" "$contractTxOut"

main() {
    nowSlotNumber=$(get_current_slot_number)
    submissionTime=$((nowSlotNumber + 60))

    commandOutput=$(
        cardano-cli conway transaction build \
            --testnet-magic "$MAGIC_TESTNET_NUMBER" \
            --out-file "$TX_PATH"/"$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index".body \
            --change-address "$USER_ADDRESS" \
            --tx-in-collateral="$collateralTxIn" \
            --tx-in="$txIn" \
            --tx-in="$auctionUTxO" \
            --tx-out="$contractTxOut" \
            --tx-out-inline-datum-file "$mintingDNSAuctionDatum" \
            --spending-tx-in-reference="$mintingDNSAuctionRefScriptUTxO" \
            --spending-plutus-script-v3 \
            --spending-reference-tx-in-inline-datum-present \
            --spending-reference-tx-in-redeemer-file "$mintingDNSAuctionBidDisclosureRedeemer" \
            --tx-out="$previousHighestBidderTxOut" \
            --read-only-tx-in-reference "$bidTxID" \
            --required-signer-hash "$USER_PKH" \
            --invalid-hereafter "$submissionTime" \
            --invalid-before "$nowSlotNumber"
    )
    sleep 1
    IFS=':' read -ra outputs <<<"$commandOutput"
    IFS=' ' read -ra fee <<<"${outputs[1]}"
    echo -e "\033[1;32m[+] Tx Fee: \033[0m" "${fee[1]}"

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/"$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index".signed \
        --tx-body-file "$TX_PATH"/"$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index".body \
        --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
        --signing-key-file "$USER_SKEY"

    cardano-cli conway transaction submit \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --tx-file "$TX_PATH"/"$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index".signed \
        >/dev/null

    echo -e "\033[1;32m[+] The Bid Disclosure Was Successful.\033[0m"

    cardano-cli transaction txid --tx-file "$TX_PATH"/"$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index".signed |
        tee "$TX_PATH"/"${DNSTokenName}"_"$index"_AuctionUTxO.txID >/dev/null

    txID=$(get_tld_txID "$USER"_disclosed_"$biddingAmount"_on_"${DNSTokenName}"_"$index" "$domainName")

    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please Wait For Tx Submission & Confirmation ..."

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] The New Auction UTxO In The DNS Auction Contract Address:"

    result=""

    while IFS= read -r result; do
        if [ "$result" != "null" ]; then
            echo "$result"
        else
            break
        fi
    done < <(find_utxo_by_txID "${MINTING_DNS_AUCTION_CONTRACT_ADDRESS}" "$txID")

    if [ "$result" == "null" ]; then
        printf "\n$RED%b\n\n" "[-] ERROR: Tx Submission & Confirmation Failed."
        sleep 1
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying Transaction for Bid Disclosure on '$DNSTokenName' Again."
        main
    fi

}

main

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
