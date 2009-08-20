#
# $Id$
#

# GNU MAKE file for Open Watcom C/C++ compiler

# ---------------------------------------------------------------
# See option docs here:
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/cpopts.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlink.html
#    http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/compiler-tools/wlib.html
# ---------------------------------------------------------------

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

ifeq ($(HB_BUILD_MODE),c)
   CC := wcc386
endif
ifeq ($(HB_BUILD_MODE),cpp)
   CC := wpp386
endif
# Build in C++ mode by default
ifeq ($(HB_BUILD_MODE),)
   CC := wpp386
endif
CC_IN :=
CC_OUT := -fo=

CPPFLAGS := -zq -bt=nt -bm
CFLAGS :=
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -w3
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CPPFLAGS += -6s -fp6

   # optimization flags
   # don't enable -ol optimization in OpenWatcom 1.1 - gives buggy code
   # -oxaht
   CPPFLAGS += -onaehtr -s -ei -zp4 -zt0
   #CPPFLAGS += -obl+m
   ifeq ($(CC),wpp386)
      CPPFLAGS += -oi+
   else
      CPPFLAGS += -oi
   endif
else
   CPPFLAGS += -3s
endif

CPPFLAGS += -i. -i$(HB_INC_COMPILE)

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -d2
endif

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif
LDFLAGS += SYS nt

include $(TOP)$(ROOT)config/common/watcom.mk

include $(TOP)$(ROOT)config/rules.mk
