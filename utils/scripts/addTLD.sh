#!/bin/bash

. ./env/general.conf
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
printf "$RED%b" "<-------------------------------------------------------------------- ADD NEW TOP LEVEL DOMAIN -------------------------------------------------------------->"
printf "\n\n"

sleep 1

printf "$YELLOW%b" "[+] Please Enter The TLD/Domain-Name (Please Add Dot '.' at the beginning): "
read -r tldName

checkAndCreateDir "${BUILD_PATH}/${tldName}"
checkAndCreateDir "${BUILD_PATH}/${tldName}/contracts"
checkAndCreateDir "${BUILD_PATH}/${tldName}/scripts_hashes"
checkAndCreateDir "${DATA_PATH}/${tldName}"
checkAndCreateDir "${DATA_PATH}/${tldName}/datums"
checkAndCreateDir "${DATA_PATH}/${tldName}/redeemers"
checkAndCreateDir "${DATA_PATH}/${tldName}/cores"
checkAndCreateDir "${DATA_PATH}/${tldName}/transactions"

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Compiling ${tldName} TLD contracts ..."
cd "${GENERAL_SCRIPTS_PATH}" || exit 1
"${GENERAL_SCRIPTS_PATH}"/createTLDValidators.sh "$tldName" || exit 1
cd "${SCRIPTS_PATH}" || exit 1

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Initializing Creation Of ${tldName} Smart Contracts ..."

check_process cardano-node

cd "${GENERAL_SCRIPTS_PATH}" || exit 1

"${GENERAL_SCRIPTS_PATH}"/createTLDRefScriptUTxOs.sh "$tldName" || exit 1

cd "${SCRIPTS_PATH}" || exit 1

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
