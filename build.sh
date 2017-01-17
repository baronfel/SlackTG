#!/usr/bin/env bash

set -eu
set -o pipefail

cd `dirname $0`

PAKET_BOOTSTRAPPER_EXE=.paket/paket.bootstrapper.exe
PAKET_EXE=.paket/paket.exe
FAKE_EXE=packages/build/FAKE/tools/FAKE.exe

FSIARGS=""
OS=${OS:-"unknown"}
if [[ "$OS" != "Windows_NT" ]]
then
  FSIARGS="--fsiargs -d:MONO"
fi

function run() {
  if [[ "$OS" != "Windows_NT" ]]
  then
    mono "$@"
  else
    "$@"
  fi
}

function yesno() {
  # NOTE: Defaults to NO
  read -p "$1 [y/N] " ynresult
  case "$ynresult" in
    [yY]*) true ;;
    *) false ;;
  esac
}

run $PAKET_EXE restore
run $FAKE_EXE "$@" $FSIARGS build.fsx
