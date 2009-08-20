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

OBJ_EXT := .o
LIB_PREF :=
LIB_EXT := .lib

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

CPPFLAGS := -zq -bt=linux
CFLAGS :=
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -w3
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CPPFLAGS += -6r -fp6

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
   CPPFLAGS += -3r
endif

CPPFLAGS += -i. -i$(HB_INC_COMPILE)

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -d2
endif

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif
LDFLAGS += SYS linux

LDLIBS := $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))

comma := ,
LDFILES_COMMA = $(subst $(subst x,x, ),$(comma) ,$(^F))
LDLIBS_COMMA := $(subst $(subst x,x, ),$(comma) ,$(strip $(LDLIBS)))
ifneq ($(HB_SHELL),sh)
   LD_RULE = $(LD) $(LDFLAGS) $(HB_USER_LDFLAGS) NAME $(BIN_DIR)/$@. FILE $(LDFILES_COMMA)
else
   LD_RULE = $(LD) $(LDFLAGS) $(HB_USER_LDFLAGS) NAME $(BIN_DIR)/$@ FILE $(LDFILES_COMMA)
endif
ifneq ($(LDLIBS_COMMA),)
   LD_RULE += LIB $(LDLIBS_COMMA)
endif

AR := wlib
# ARFLAGS := -q -c -n -fa
ARFLAGS := -q -c -n
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ $(foreach file,$(^F),-+$(file))

ifeq ($(HB_SHELL),dos)

   # disable DOS/4GW Banner
   export DOS4G := quiet

   # work arround to DOS command line size limit
   ifeq ($(CC),wcc386)
      export WCC386 := $(strip $(CPPFLAGS))
   else
      export WPP386 := $(strip $(CPPFLAGS))
   endif
   CPPFLAGS :=

   export HARBOURCMD := $(HB_FLAGS)
   HB_FLAGS :=
endif

include $(TOP)$(ROOT)config/rules.mk
