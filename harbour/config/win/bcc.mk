#
# $Id$
#

# GNU Make file for Borland/CodeGear/Embarcadero C/C++ 32-bit (4.x-)

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

HB_DYN_COPT := -DHB_DYNLIB

CC := bcc32.exe
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

CFLAGS += -q -tWM -CP437

ifeq ($(HB_BUILD_WARN),no)
   CFLAGS += -w-sig- -w-aus- -w-ccc- -w-csu- -w-par- -w-rch- -w-ucp- -w-use- -w-prc-
else
   CFLAGS += -w -Q -w-sig-
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

ifneq ($(HB_HOST_PLAT_UNIX),)
   BACKSLASH := $(subst /,\,\\)
else
   BACKSLASH := $(subst /,\,\)
endif

ifeq ($(HB_SHELL),sh)
   ECHOBACKSLASH := $(BACKSLASH)$(BACKSLASH)
else
   ECHOBACKSLASH := $(BACKSLASH)
endif

# Hack to autoconfig bcc, and not require properly set .cfg files in its bin dir.
# It only works if we know compiler location.
ifneq ($(HB_COMP_PATH_PUB),)
   HB_CFLAGS += $(subst /,$(BACKSLASH),-I"$(HB_COMP_PATH_PUB)../Include")
   LDFLAGS   += $(subst /,$(BACKSLASH),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
   DFLAGS    += $(subst /,$(BACKSLASH),-L"$(HB_COMP_PATH_PUB)../Lib" -L"$(HB_COMP_PATH_PUB)../Lib/PSDK")
endif

RC := brcc32.exe
RC_OUT := -fo

LD := ilink32.exe
LIBPATHS := $(subst /,$(BACKSLASH),-L"$(LIB_DIR)")
LDFLAGS += $(LIBPATHS) -Gn -Tpe
LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) c0x32.obj $(filter-out %$(RES_EXT),$(^F)), "$(subst /,$(BACKSLASH),$(BIN_DIR)/$@)", nul, $(LDLIBS) cw32mt import32,, $(filter %$(RES_EXT),$(^F)) $(LDSTRIP)

LDLIBS := $(strip $(HB_USER_LIBS) $(LIBS) $(SYSLIBS))

AR := tlib.exe
ARFLAGS += /P128
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) "$(subst /,$(BACKSLASH),$(LIB_DIR)/$@)" $(foreach file,$(?F),-+$(file))

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
      define library_object
         @$(ECHO) $(ECHOQUOTE)-+$(subst /,$(ECHOBACKSLASH),$(file)) $(LINECONT)$(ECHOQUOTE) >> __lib__.tmp

      endef

      define create_library
         $(if $(wildcard __lib__.tmp),@$(RM) __lib__.tmp,)
         $(foreach file,$(?F),$(library_object))
         @$(ECHO) $(ECHOQUOTE)-+$(ECHOQUOTE)>> __lib__.tmp
         $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) "$(subst /,$(BACKSLASH),$(LIB_DIR)/$@)" @__lib__.tmp
      endef

      AR_RULE = $(create_library)

   endif
endif

DY := ilink32.exe
DFLAGS += -q -Gn -C -aa -Tpd -Gi -x
DY_OUT :=
# NOTE: .lib extension not added to keep line short enough to work on Win9x/ME
DLIBS := $(HB_USER_LIBS) $(LIBS) $(SYSLIBS) cw32mt import32

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)$(subst /,$(ECHOBACKSLASH),$(file)) +$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   @$(ECHO) $(ECHOQUOTE), $(subst /,$(ECHOBACKSLASH),$(DYN_DIR)/$@),, $(subst /,$(ECHOBACKSLASH),$(DLIBS))$(ECHOQUOTE) >> __dyn__.tmp
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) c0d32.obj @__dyn__.tmp
   @$(CP) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT)) $(subst /,$(DIRSEP),$(IMP_FILE))
   @$(RM) $(subst /,$(DIRSEP),$(DYN_DIR)/$(basename $@)$(LIB_EXT))
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
