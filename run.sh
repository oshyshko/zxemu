#!/bin/sh

set -xue

# enableGUI >> main
stack ghci --ghci-options -fno-ghci-sandbox << EOF
main
EOF
