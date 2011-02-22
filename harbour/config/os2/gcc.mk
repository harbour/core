#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF :=
ifeq ($(HB_COMPILER),gccomf)
   LIB_EXT := .lib
else
   LIB_EXT := .a
endif

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

ifeq ($(HB_COMPILER),gccomf)
   CFLAGS += -Zomf
   LDFLAGS += -Zomf
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ifeq ($(HB_COMPILER),gccomf)
      ARSTRIP = & ${HB_CCPATH}${HB_CCPREFIX}stripomf -S $(LIB_DIR)/$@
   else
      ARSTRIP = & ${HB_CCPATH}${HB_CCPREFIX}strip -S $(LIB_DIR)/$@
   endif
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif

SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   SYSLIBS += socket
endif

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

ifeq ($(HB_COMPILER),gccomf)
   # NOTE: The empty line directly before 'endef' HAS TO exist!
   #       It causes that every command will be separated by LF
   define lib_object
      $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -p128 r $(LIB_DIR)/$@ $(file)$(ECHOQUOTE)

   endef

   define create_library
      $(if $(wildcard $(subst /,$(DIRSEP),$(LIB_FILE))),@$(RM) $(subst /,$(DIRSEP),$(LIB_FILE)),)
      $(foreach file,$^,$(lib_object))
   endef
else
   # NOTE: The empty line directly before 'endef' HAS TO exist!
   #       It causes that every command will be separated by LF
   define lib_object
      @$(ECHO) $(ECHOQUOTE)ADDMOD $(file)$(ECHOQUOTE) >> __lib__.tmp

   endef

   # We have to use a script to overcome the AR limit of max 850 characters
   # in commmand line
   define create_library
      $(if $(wildcard $(subst /,$(DIRSEP),$(LIB_FILE))),@$(RM) $(subst /,$(DIRSEP),$(LIB_FILE)),)
      @$(ECHO) $(ECHOQUOTE)CREATE $(LIB_DIR)/$@$(ECHOQUOTE) > __lib__.tmp
      $(foreach file,$^,$(lib_object))
      @$(ECHO) $(ECHOQUOTE)SAVE$(ECHOQUOTE) >> __lib__.tmp
      @$(ECHO) $(ECHOQUOTE)END$(ECHOQUOTE) >> __lib__.tmp
      $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -M < __lib__.tmp
   endef
endif

# Under OS/2 || isn't a command separator (inside a shell, that is); correct separator is &

ifeq ($(HB_COMPILER),gccomf)
   AR := $(HB_CCPATH)$(HB_CCPREFIX)emxomfar
   AR_RULE = $(create_library) $(ARSTRIP)
else
   AR := $(HB_CCPATH)$(HB_CCPREFIX)ar
   AR_RULE = $(create_library) $(ARSTRIP) & $(RM) __lib__.tmp
endif

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
ifeq ($(HB_COMPILER),gccomf)
   DFLAGS += -Zomf
endif
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAS TO exist!
define dynlib_object
   @$(ECHO) $(subst $(DIRSEP),/,$(file))>> __dyn__.tmp
   @emxexp $(file)>> __dyn__.def

endef

define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(if $(wildcard __dyn__.def),@$(RM) __dyn__.def,)
   @$(ECHO) $(ECHOQUOTE)LIBRARY $(DYNNAME2) INITINSTANCE TERMINSTANCE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)PROTMODE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)CODE PRELOAD MOVEABLE DISCARDABLE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)DATA PRELOAD MOVEABLE MULTIPLE NONSHARED$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)EXPORTS$(ECHOQUOTE) >> __dyn__.def
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ -Wl,@__dyn__.tmp __dyn__.def $(DLIBS) $(DYSTRIP)
   emximp -o $(IMP_FILE) $(DYN_DIR)/$@
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
