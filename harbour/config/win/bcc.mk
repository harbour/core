#
# $Id$
#

# GNU MAKE file for Borland/CodeGear C/C++ 32-bit (4.x-)

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := bcc32.exe
CC_IN := -c
CC_OUT := -o

CPPFLAGS := -I. -I$(HB_INC_COMPILE)
CFLAGS := -q -tWM
LDFLAGS :=
DFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -w -w-sig- -Q
else
   CFLAGS += -w-
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # for some reason -6 generates the exact same code as -4 with both 5.5 and 5.8.
   # -5 seems to be significantly slower than both. [vszakats]
   CFLAGS += -d -6 -O2 -OS -Ov -Oi -Oc
endif

ifeq ($(HB_BUILD_MODE),cpp)
   CFLAGS += -P
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -y -v
endif

# Hack to autoconfig bcc, and not require properly set .cfg files in its bin dir.
# It only works if we know compiler location.
ifneq ($(HB_COMP_PATH_PUB),)
   HB_CFLAGS += $(subst /,$(DIRSEP),-I"$(HB_COMP_PATH_PUB)../Include")
   LDFLAGS   += $(subst /,$(DIRSEP),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
   DFLAGS    += $(subst /,$(DIRSEP),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
endif

LD := ilink32.exe
LDFLAGS += -Gn -Tpe $(LIBPATHS)
LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) c0x32.obj $(^F), $(subst /,$(DIRSEP),$(BIN_DIR)/$@), nul, $(subst /,$(DIRSEP),$(LDLIBS)) cw32mt.lib import32.lib $(LDSTRIP)

LIBPATHS := -L$(LIB_DIR)
# It's probably not necessary in native Windows but I need it
# for my Linux box because -L<path> seems to not work with WINE
LDLIBS := $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib)$(LIB_EXT))
LDLIBS += $(foreach lib,$(SYSLIBS),$(lib)$(LIB_EXT))

AR := tlib.exe
ARFLAGS := /P128
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) "$(subst /,\,$(LIB_DIR)/$@)" $(foreach file,$(?F),-+$(file))

ifneq ($(HB_SHELL),sh)
   ifeq ($(HB_SHELL_XP),)

      ifeq ($(HB_SHELL),nt)
         LINECONT := ^&
      else
         LINECONT := &
      endif

      # NOTE: Command-line limit length defeating methods found below
      #       are only needed to support pre-Windows XP systems, where
      #       limit is 2047 chars. [vszakats]

      # NOTE: The empty line directly before 'endef' HAVE TO exist!
      define lib_object
         @$(ECHO) $(ECHOQUOTE)-+$(subst /,\,$(file)) $(LINECONT)$(ECHOQUOTE) >> __lib__.tmp

      endef

      define create_library
         $(if $(wildcard __lib__.tmp),@$(RM) __lib__.tmp,)
         $(foreach file,$(?F),$(lib_object))
         @$(ECHO) $(ECHOQUOTE)-+$(ECHOQUOTE)>> __lib__.tmp
         $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) "$(subst /,\,$(LIB_DIR)/$@)" @__lib__.tmp
      endef

      AR_RULE = $(create_library)

   endif
endif

DY := ilink32.exe
DFLAGS += -q -Gn -C -aa -Tpd -Gi -x
DY_OUT :=
DLIBS := $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib)$(LIB_EXT))
DLIBS += $(foreach lib,$(SYSLIBS),$(lib)$(LIB_EXT))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(subst /,\,$(file)) +$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   @$(ECHO) $(ECHOQUOTE), $(subst /,\,$(DYN_DIR)/$@),, $(subst /,\,$(DLIBS)) cw32mt.lib import32.lib$(ECHOQUOTE) >> __dyn__.tmp
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) c0d32.obj @__dyn__.tmp
   @$(CP) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT)) $(subst /,$(DIRSEP),$(IMP_FILE))
   @$(RM) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT))
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
