#
# $Id$
#

all : first

BIN_EXT :=

HB_GT_LIBS += gttrm

ifeq ($(HB_SHELL),sh)
   ifneq ($(filter $(HB_COMPILER),gcc icc),)
      ifeq ($(filter -fPIC,$(HB_USER_CFLAGS)),)
         ifeq ($(filter -fpic,$(HB_USER_CFLAGS)),)
            _UNAME_M := $(shell uname -m)
            ifeq ($(findstring 86,$(_UNAME_M)),)
               HB_CFLAGS += -fPIC
            else
               ifneq ($(findstring 64,$(_UNAME_M)),)
                  HB_CFLAGS += -fPIC
               endif
            endif
         endif
      endif
   endif
endif
