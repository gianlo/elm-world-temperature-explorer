#!/bin/sh

ELM_VERSION=0.19.1

echo Installing elm version $ELM_VERSION
curl -L -o elm.gz https://github.com/elm/compiler/releases/download/$ELM_VERSION/binary-for-linux-64-bit.gz
gunzip elm.gz
chmod +x elm
sudo mv elm /usr/local/bin
echo Elm compiler installed successfully
