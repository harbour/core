#!/bin/bash
# 
# $Id$
# 

# Flavour: Bash script

# ---------------------------------------------------------------
# Template to initialize the environment before starting
# the GNU make system for Harbour
#
# For further information about the GNU make system please
# check doc/gmake.txt
#
# Copyright 1999-2000 Victor Szakats (info@szelvesz.hu)
# See doc/license.txt for licensing terms.
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
#  - w32
#  - linux
#  - os2

export HB_ARCHITECTURE=linux

# ---------------------------------------------------------------
# The following HB_COMPILER values are currently supported:
#  - When HB_ARCHITECTURE=dos
#    - bcc16
#    - djgpp
#    - watcom
#  - When HB_ARCHITECTURE=w32
#    - bcc32
#    - gcc
#    - mingw32
#    - icc
#    - msvc
#  - When HB_ARCHITECTURE=linux
#    - gcc
#  - When HB_ARCHITECTURE=os2
#    - gcc
#    - icc

export HB_COMPILER=gcc

# ---------------------------------------------------------------
# Fine tuning the compiler parameters for "all" command:

export PRG_USR= 
export C_USR=
export L_USR=

# ---------------------------------------------------------------
# Start the GNU make system

make $*
