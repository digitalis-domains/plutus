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
printf "$BLUE%b" "       %@@@@%    @@@@@@@@@@@@@@@@@@%                       @@@%      %%                       %%%      @@%                       %@%%      %%                 "
printf "$BLUE%b" "                 %@@@@@@@@@@@@@@@@@%                    %% @@@%                   %                   %@@%                       %@%%                  %%     "
printf "$BLUE%b" "                  %@@@@@@@@@@@@@@@@                %%%%%%@@@@@%    %@@@%      %@%%%%%@@@@@% %%@@@%  %%%@@@%%%%    %%%%@@@%       %@%%   %%%@@%     %%%%%%@@%  "
printf "$BLUE%b" "       %@@@@@@@@%  %@@@@@@@@@@@@@%               %@@%     %@@@%     %@@%    %@@%    %@@%%%   %@@@%    %@@%      %@%    %@@%      %@%%    %@@@%    %@@     %%  "
printf "$BLUE%b" "     %@@@@@@@@@@@@%  %%@@@@@@@%%                %@@%       @@@%     @@@%    @@@%     %@@@     %@@%    %@@%      %@%     @@@%     %@%%     %@@%    @@@%        "
printf "$BLUE%b" "   %@@@@@@@@@@@@@@@@                           %@@@        @@@%     @@@%   %@@@%     %@@@     @@@%    %@@%              %@@%     %@%%     %@@%    %@@@@%      "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@                          @@@%        @@@%     @@@%    %@@%     @@@%     @@@%    %@@%         %%%%%%@@%     %@%%     %@@%     %%@@@@@%   "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@%    %@@@%%               %@@@%       @@@%     @@@%     %@@@%%%%@%       @@@%    %@@%      %@@%    %@@%     %@%%     %@@%         %@@@@% "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@%   %@@@@@@%              %@@@%       @@@%     @@@%     %%%%%%%%         %@@%    %@@%     %@@%     %@@%     %@%%     %@@%   %%      %@@% "
printf "$BLUE%b" "  %@@@@@@@@@@@@@@@@@@    @@@@@@@%               %@@@@%   %%@@@%     @@@%    @@@%%             @@@%    %@@@%    %@@@%   %@@@%     %@%%     %@@%   %@%     %@@  "
printf "$BLUE%b" "   %@@@@@@@@@@@@@@@@%    %%@@@@%                  %@@@@@%%%%@@%%% %%%@@@%%  %%@@@@@@@@@@@%  %%@@@@%%   %@@@@%%  %%@@@@%%%@@@%%%%%@@@%%% %%@@@@%%  %@@%%%%%%   "
printf "$BLUE%b" "    %%@@@@@@@@@@@@%                                  @%                       %%%%%%%%%@@@%               @        %%                                @%@      "
printf "$BLUE%b" "       %@@@@@@@@%                                                           %@%        %@@%                                                                   "
printf "$BLUE%b" "                                                                           @@%%        %@@%                                                                   "
printf "$BLUE%b" "                                                                           %@@%        %%%                                                                    "
printf "$BLUE%b" "                                                                            %@@@@%%%%%%%                                                                      "
printf "$BLUE%b" "                                                                               %@@@%%                                                                         "
printf "\n"
printf "$YELLOW%b" "                                                                         D I G I T A L I S                                                                  "
printf "$GREEN%b" "                                                                    https://digitalis.domains/                                                               "
printf "\n"
printf "$RED%b" "<------------------------------------------------------- REMOVING ALL REFERENCE SCRIPT UTXOS --------------------------------------------------------->"
printf "\n\n"

check_process cardano-node

printf "$YELLOW%b" "[+] Please Enter The New TLD Name (Please Add Dot '.' at the beginning): "
read -r tldName

DOMAIN_NAME=$tldName

TX_PATH="${DATA_PATH}/${DOMAIN_NAME}/transactions"

cabal run "$WRITE_REDEEMERS_SCRIPT_NAME" \
    "$DIGITALIS_REDEEMERS_PATH" \
    "$CONTRACTS_CONTROLLER_CONTRACT_NAME" \
    "ContractsControllerManagement" \
    >/dev/null 2>&1

CONTRACTS_CONTROLLER_MANAGEMENT_REDEEMER=$(getDigitalisRedeemer "${CONTRACTS_CONTROLLER_CONTRACT_NAME}" "ContractsControllerManagement")
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

CONTRACTS_CONTROLLER_CONTRACT_ADDRESS=$(getContractAddress "$CONTRACTS_CONTROLLER_CONTRACT" "${STAKING_CONTRACT}")

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

main() {

    local CONTRACT_NAME
    local AUTH_TOKEN
    CONTRACT_NAME="$1"
    AUTH_TOKEN="$2"

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Removing '$CONTRACT_NAME' Reference Script UTxO ..."

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

    contractRefScriptUTxO=$(get_UTxO_by_token "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$AUTH_TOKEN")

    contractRefScriptUTxOAmount=$(get_UTxO_lovelace_amount "$CONTRACTS_CONTROLLER_CONTRACT_ADDRESS" "$contractRefScriptUTxO")

    contractsControllerContractRefScriptUTxO=$(get_digitalis_txID "$CONTRACTS_CONTROLLER_CONTRACT_NAME""_ref_script")

    digitalisTxOut="$DIGITALIS_WALLET_ADDRESS + ${contractRefScriptUTxOAmount}"

    echo -e "\033[1;32m[+] Tx Output To Digitalis Wallet Address:\033[0m" "$digitalisTxOut"

    sleep 1

    nowSlotNumber=$(get_current_slot_number)
    submissionTime=$((nowSlotNumber + 60))

    commandOutput=$(
        cardano-cli conway transaction build \
            --testnet-magic "$MAGIC_TESTNET_NUMBER" \
            --out-file "$TX_PATH"/remove_"${CONTRACT_NAME}"_ref_script.body \
            --change-address "$DIGITALIS_WALLET_ADDRESS" \
            --tx-in-collateral="$collateralTxIn" \
            --tx-in="$txIn" \
            --tx-in="$contractRefScriptUTxO" \
            --tx-out="$digitalisTxOut" \
            --spending-tx-in-reference="$contractsControllerContractRefScriptUTxO" \
            --spending-plutus-script-v3 \
            --spending-reference-tx-in-inline-datum-present \
            --spending-reference-tx-in-redeemer-file "$CONTRACTS_CONTROLLER_MANAGEMENT_REDEEMER" \
            --mint="-1 ${AUTH_TOKEN}" \
            --mint-tx-in-reference "${CATcontractRefScriptUTxO}" \
            --mint-plutus-script-v3 \
            --mint-reference-tx-in-redeemer-file "$DIGITALIS_BOARD_ACTION_REDEEMER" \
            --policy-id="${CAT_POLICY_ID}" \
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
            --invalid-hereafter "$submissionTime"
    )
    IFS=':' read -ra outputs <<<"$commandOutput"
    IFS=' ' read -ra fee <<<"${outputs[1]}"
    echo -e "\033[1;32m[+] Tx Fee: \033[0m" "${fee[1]}"

    cardano-cli conway transaction sign \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/remove_"${CONTRACT_NAME}"_ref_script.signed \
        --tx-body-file "$TX_PATH"/remove_"${CONTRACT_NAME}"_ref_script.body \
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
        --tx-file "$TX_PATH"/remove_"${CONTRACT_NAME}"_ref_script.signed \
        >/dev/null

    sleep 1

    txID=$(get_tld_txID "remove_""$CONTRACT_NAME""_ref_script" "$DOMAIN_NAME")
    echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$txID"

    sleep 1

    echo -e "\033[1;32m[+] '$CONTRACT_NAME Ref UTxO' Has Been Removed Successfully.\033[0m"

    sleep 1

    printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] But Please Wait For Tx Submission & Confirmation To Get UTxO Information ..."

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
        printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Retrying To Remove The '$CONTRACT_NAME' Reference Script Utxo."
        main "$CONTRACT_NAME" "$AUTH_TOKEN"
    fi

    # printf "$YELLOW%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Please press 'Enter' to continue."
    # read -r

}

main "$MINTING_DNS_AUCTION_CONTRACT_NAME" "$MINTING_DNS_AUCTION_REF_SCRIPT_AUTH_TOKEN"
main "$DNS_MINTER_CONTRACT_NAME" "$DNS_MINTER_REF_SCRIPT_AUTH_TOKEN"
main "$MHABAT_CONTRACT_NAME" "$MHABAT_REF_SCRIPT_AUTH_TOKEN"
main "$DNS_REFERENCE_CONTRACT_NAME" "$DNS_REFERENCE_REF_SCRIPT_AUTH_TOKEN"
main "$DRAT_CONTRACT_NAME" "$DRAT_REF_SCRIPT_AUTH_TOKEN"
main "$VICKREY_AUCTION_CONTRACT_NAME" "$VICKREY_AUCTION_REF_SCRIPT_AUTH_TOKEN"
main "$VABAT_CONTRACT_NAME" "$VABAT_REF_SCRIPT_AUTH_TOKEN"
main "$ENGLISH_AUCTION_CONTRACT_NAME" "$ENGLISH_AUCTION_REF_SCRIPT_AUTH_TOKEN"

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
