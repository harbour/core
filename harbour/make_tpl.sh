#!/bin/bash

# Flavour: Bash script

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system for Harbour
#
# For further information about the GNU make system please
# check doc/gmake.txt
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# Usage: make_tpl.sh <command>
#
# The following commands are currently supported:
#  - all (default)
#  - clean
#  - install
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# Configuration for "install" command:

export HB_BIN_INSTALL=bin/
export HB_LIB_INSTALL=lib/
export HB_INC_INSTALL=include/

# ---------------------------------------------------------------
# The following HB_ARCHITECTURE values are currently supported:
#  - dos
#  - win32
#  - linux
#  - os2

export HB_ARCHITECTURE=linux

# ---------------------------------------------------------------
# The following HB_COMPILER values are currently supported:
#  - When HB_ARCHITECTURE=dos
#    - bcc31
#    - djgpp
#    - watcom
#  - When HB_ARCHITECTURE=win32
#    - bcc32
#    - gcc
#    - icc
#    - msvc
#  - When HB_ARCHITECTURE=linux
#    - gcc
#  - When HB_ARCHITECTURE=os2
#    - gcc
#    - icc

export HB_COMPILER=gcc

# ---------------------------------------------------------------
# Fine tuning of C compiler parameters for "all" command:

export C_USR=

# ---------------------------------------------------------------
# Start the GNU make system

make $*

