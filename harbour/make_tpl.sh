#!/bin/bash

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system for Harbour (Linux version)
#
# For further information about the GNU make system please
# check doc/gmake.txt
# ---------------------------------------------------------------

export HB_ARCHITECTURE=linux
export HB_COMPILER=gcc

# ---------------------------------------------------------------
# Start the GNU make system

make clean
make

