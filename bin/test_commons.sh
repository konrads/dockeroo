#!/usr/bin/env bash

green="\033[32m"
red="\033[31m"
reset="\033[0m"

assert_eq() {
  if [ "$2" = "$3" ]
  then
    echo -e "  ${green}$1: $2 = $3${reset}"
  else
    echo -e "  ${red}$1: $2 <> $3${reset}"
    exit -1
  fi
}

assert_lt() {
  if [ "$2" -lt "$3" ]
  then
    echo -e "  ${green}$1: $2 < $3${reset}"
  else
    echo -e "  ${red}$1: $2 >= $3${reset}"
    exit -1
  fi
}

assert_gt() {
  if [ "$2" -gt "$3" ]
  then
    echo -e "  ${green}$1: $2 > $3${reset}"
  else
    echo -e "  ${red}$1: $2 <= $3${reset}"
    exit -1
  fi
}

assert_le() {
  if [ "$2" -le "$3" ]
  then
    echo -e "  ${green}$1: $2 <= $3${reset}"
  else
    echo -e "  ${red}$1: $2 > $3${reset}"
    exit -1
  fi
}

assert_ge() {
  if [ "$2" -ge "$3" ]
  then
    echo -e "  ${green}$1: $2 >= $3${reset}"
  else
    echo -e "  ${red}$1: $2 < $3${reset}"
    exit -1
  fi
}
