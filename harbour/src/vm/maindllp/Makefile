#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../

C_SOURCES := \
   maindllp.c \

LIBNAME := hbmaindllp

# always export symbols
HB_CFLAGS += -DHB_DYNLIB

include $(TOP)$(ROOT)config/lib.mk
