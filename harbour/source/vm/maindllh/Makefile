#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

vpath %.c ../

C_SOURCES := \
   maindllh.c \

LIBNAME := hbmaindllh

# always export symbols
HB_CFLAGS += -DHB_DYNLIB

include $(TOP)$(ROOT)config/lib.cf
