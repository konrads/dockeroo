#!/usr/bin/env bash

green="\033[32m"
red="\033[31m"
reset="\033[0m"

kerl_install() {
  local otp_ver=$1
  HOME=/opt/kerl kerl build $otp_ver $otp_ver
  mkdir -p /opt/kerl/releases/$otp_ver
  HOME=/opt/kerl kerl install $otp_ver /opt/kerl/releases/$otp_ver
}

get_ip() {
  echo $(grep `hostname` /etc/hosts | awk '{print $1}')
}

replace_in_file() {
  local re=$1
  local replacement=$2
  local target_file=$3
  sed -i "s/$re/$replacement/g" $target_file
}

create_rhel_sudo_user() {
  local user=$1
  groupadd -r sudo
  groupadd -r $user
  useradd -m -r -g $user $user
  echo "$user:$user" | chpasswd
  echo -e "%sudo\tALL=(ALL)\tNOPASSWD: ALL" >> /etc/sudoers
  usermod -aG sudo $user 
}

create_deb_sudo_user() {
  local user=$1
  groupadd -r $user
  useradd -m -r -g $user $user
  echo "$user:$user" | chpasswd
  echo -e "%$user\tALL=(ALL)\tNOPASSWD: ALL" >> /etc/sudoers.d/$user
  usermod -aG sudo $user 
}
