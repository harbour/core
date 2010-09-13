#
# $Id$
#

# GNU Make file for Open Watcom C/C++ compiler

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

ifeq ($(HB_BUILD_MODE),cpp)
   CC := wpp386
else
   CC := wcc386
endif
CC_IN :=
CC_OUT := -fo=

CFLAGS += -zq -bt=nt -bm
LDFLAGS += OP quiet

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -w3
else
   CFLAGS += -w1
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CFLAGS += -6s -fp6

   # optimization flags
   # don't enable -ol optimization in OpenWatcom 1.1 - gives buggy code
   # -oxaht
   CFLAGS += -onaehtr -s -ei -zp4 -zt0
   #CFLAGS += -obl+m
   ifeq ($(CC),wpp386)
      CFLAGS += -oi+
   else
      CFLAGS += -oi
   endif
else
   CFLAGS += -3s
endif

CFLAGS += -i. -i$(HB_HOST_INC)

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -d2
endif

RC := wrc
RC_OUT := -fo=
RCFLAGS += -q -r -zm -bt=nt

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif
LDFLAGS += SYS nt

LDLIBS := $(foreach lib,$(HB_USER_LIBS),$(lib))
LDLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))
LDLIBS += $(foreach lib,$(SYSLIBS),$(lib))

DY := $(LD)
DFLAGS += OP quiet SYS nt_dll
DY_OUT :=
DLIBS := $(foreach lib,$(HB_USER_LIBS),$(lib))
DLIBS += $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))
DLIBS += $(foreach lib,$(SYSLIBS),$(lib))
DLIBS := $(strip $(DLIBS))

ifneq ($(DLIBS),)
   comma := ,
   DLIBS_COMMA := LIB $(subst $(subst x,x, ),$(comma) ,$(DLIBS))
else
   DLIBS_COMMA :=
endif

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)FILE '$(file)'$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) NAME '$(subst /,$(DIRSEP),$(DYN_DIR)/$@)' OP implib='$(IMP_FILE)' @__dyn__.tmp $(DLIBS_COMMA)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/common/watcom.mk
