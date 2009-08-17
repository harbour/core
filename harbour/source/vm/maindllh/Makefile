#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../

C_SOURCES := \
   maindllh.c \

LIBNAME := hbmaindllh

# always export symbols
HB_CFLAGS += -DHB_DYNLIB

include $(TOP)$(ROOT)config/lib.mk
