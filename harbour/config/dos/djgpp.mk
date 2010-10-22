#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := gpp
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a
DYN_EXT := .dxe

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
else
   CFLAGS += -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -gstabs+
endif

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = $(HB_CCPATH)$(HB_CCPREFIX)strip -S $(LIB_DIR)/$@
else
   ARSTRIP := @$(ECHO) .
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
else
   LDSTRIP :=
   DYSTRIP :=
endif

SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_LIBNAME_CURSES),)
      HB_LIBNAME_CURSES := pdcurses
   endif
   ifneq ($(filter gtcrs, $(LIBS)),)
      SYSLIBS += $(HB_LIBNAME_CURSES)
   endif
   ifneq ($(HB_HAS_WATT),)
      SYSLIBPATHS += $(HB_LIB_WATT)
      SYSLIBS += watt
   endif
   SYSLIBS += hbpmcom
endif

SYSLIBS += m

LD := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#       It causes that every command will be separated by LF
define lib_object
   @$(ECHO) $(ECHOQUOTE)ADDMOD $(file)$(ECHOQUOTE) >> __lib__.tmp

endef

# We have to use script to overcome the DOS limit of max 128 characters
# in commmand line
define create_library
   @$(ECHO) $(ECHOQUOTE)CREATE $(LIB_DIR)/$@$(ECHOQUOTE) > __lib__.tmp
   $(foreach file,$(^F),$(lib_object))
   @$(ECHO) $(ECHOQUOTE)SAVE$(ECHOQUOTE) >> __lib__.tmp
   @$(ECHO) $(ECHOQUOTE)END$(ECHOQUOTE) >> __lib__.tmp
   $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -M < __lib__.tmp
   -$(RANLIB) $(LIB_DIR)/$@
   $(ARSTRIP)
endef

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define link_file
   @$(ECHO) $(ECHOQUOTE)$(file)$(ECHOQUOTE) >> __link__.tmp

endef

define link_exe_file
   @$(ECHO) $(ECHOQUOTE)$(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) $(LD_OUT)$(BIN_DIR)/$@$(ECHOQUOTE) > __link__.tmp
   $(foreach file,$(^F),$(link_file))
   $(foreach file,$(LIBPATHS),$(link_file))
   $(foreach file,$(LDLIBS),$(link_file))
   $(LD) @__link__.tmp
endef

AR := $(HB_CCPATH)$(HB_CCPREFIX)ar
RANLIB := $(HB_CCPATH)$(HB_CCPREFIX)ranlib
AR_RULE = $(create_library)

LD_RULE = $(link_exe_file)

ifeq ($(HB_BUILD_DYN),dostest)

   DY := dxe3gen
   DFLAGS += $(LIBPATHS)
   DY_OUT := -o$(subst x,x, )
   DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

   # due to limited size of ld parameter list use libraries directly
   HB_DYN_FROM_LIBS := yes
   DFLAGS += --whole-archive
   DLIBS :=

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   define dynlib_object
      @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __dyn__.tmp

   endef
   define create_dynlib
      @$(ECHO) $(ECHOQUOTE)$(DFLAGS) $(HB_USER_DFLAGS)$(ECHOQUOTE) > __dyn__.tmp
      @$(ECHO) $(ECHOQUOTE)$(DY_OUT)$(DYN_DIR)/$@$(ECHOQUOTE) >> __dyn__.tmp
      @$(ECHO) $(ECHOQUOTE)-Y $(IMP_FILE) -U $(DYSTRIP)$(ECHOQUOTE) >> __dyn__.tmp
      $(foreach file,$^,$(dynlib_object))
      @$(ECHO) $(ECHOQUOTE)$(DLIBS)$(ECHOQUOTE) >> __dyn__.tmp
      $(DY) @__dyn__.tmp
   endef

   DY_RULE = $(create_dynlib)

endif # HB_BUILD_DYN

include $(TOP)$(ROOT)config/rules.mk
