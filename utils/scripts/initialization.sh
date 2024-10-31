#!/bin/bash

# Make script executable and run it
# mixaxim@mixaxim:~$ chmod +x Initialization.sh; ./Initialization.sh

. ./env/general.conf

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
printf "$RED%b" "<------------------------------------------------------------------------- INITIALIZATION  ------------------------------------------------------------------>"
printf "\n\n"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating initial necessary directories ..."

checkAndCreateDir "${DIST_PATH}"
checkAndCreateDir "${DATA_PATH}"
checkAndCreateDir "${BUILD_PATH}"
checkAndCreateDir "${DIGITALIS_BUILD_PATH}"
checkAndCreateDir "${DIGITALIS_CONTRACTS_PATH}"
checkAndCreateDir "${DIGITALIS_SH_PATH}"
checkAndCreateDir "${DIGITALIS_DATA_PATH}"
checkAndCreateDir "${DIGITALIS_DATUMS_PATH}"
checkAndCreateDir "${DIGITALIS_REDEEMERS_PATH}"
checkAndCreateDir "${DIGITALIS_CORES_PATH}"
checkAndCreateDir "${DIGITALIS_TX_PATH}"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Making all bash scripts executable ..."

chmod +x "${SCRIPTS_PATH}"/addTLD.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createDigitalisValidators.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createDigitalisRefScriptUTxOs.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createBoardMembersUTxOs.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createCATRefScript.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createContractsControlRefScript.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createTLDRefScriptUTxOs.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/createTLDValidators.sh
chmod +x "${GENERAL_SCRIPTS_PATH}"/removeTLDAllRefScriptUTxOs.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/1_MintingDNS.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/2_Bidding.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/3_Updating.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/4_BidDisclosure.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/5_DNSRedeeming.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/6_BidClaiming.sh
chmod +x "${DIGITALIS_PLATFORM_SCRIPTS_PATH}"/7_DNSRefUpdating.sh

# sleep 1

# printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Updating Cabal ..."
# cd "${REPO_HOME}" || exit 1
# cabal update
# cd "${SCRIPTS_PATH}" || exit 1

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Compiling Digitalis contracts ..."
cd "${GENERAL_SCRIPTS_PATH}" || exit 1
"${GENERAL_SCRIPTS_PATH}"/createDigitalisValidators.sh || exit 1
cd "${SCRIPTS_PATH}" || exit 1

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Initializing Creation Of Digitalis Smart Contracts System..."

check_process cardano-node

cd "${GENERAL_SCRIPTS_PATH}" || exit 1

"${GENERAL_SCRIPTS_PATH}"/createBoardMembersUTxOs.sh || exit 1

"${GENERAL_SCRIPTS_PATH}"/createCATRefScript.sh || exit 1

"${GENERAL_SCRIPTS_PATH}"/createContractsControlRefScript.sh || exit 1

"${GENERAL_SCRIPTS_PATH}"/createDigitalisRefScriptUTxOs.sh || exit 1

cd "${SCRIPTS_PATH}" || exit 1

printf "\n$RED%b\n\n" "<------------------------------------------------------------------------ DONE ----------------------------------------------------------------------->"
