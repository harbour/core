#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

ifeq ($(HB_COMPILER),gccomf)
   OBJ_EXT := .obj
   LIB_PREF :=
   LIB_EXT := .lib
else
   OBJ_EXT := .o
   LIB_PREF :=
   LIB_EXT := .a
endif

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CPPFLAGS := -I. -I$(HB_INC_COMPILE)
CFLAGS :=
LDFLAGS :=

ifeq ($(HB_COMPILER),gccomf)
   CFLAGS += -Zomf
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -Wall -W
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

SYSLIBS :=
SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   SYSLIBS += socket
endif

LD := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

# NOTE: The empty line directly before 'endef' HAS TO exist!
#       It causes that every command will be separated by LF
#define lib_object
#   @$(ECHO) $(ECHOQUOTE)ADDMOD $(file)$(ECHOQUOTE) >> __lib__.tmp
#
#endef

ifeq ($(HB_COMPILER),gccomf)
   define create_library
      $(if $(wildcard $(subst /,$(DIRSEP),$(LIB_FILE))),@$(RM) $(subst /,$(DIRSEP),$(LIB_FILE)),)
      for %i in ( *$(OBJ_EXT) ) do $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -p128 r $(LIB_DIR)/$@ %i$(ECHOQUOTE)
   endef
else
   # We have to use a script to overcome the AR limit of max 850 characters
   # in commmand line
   define create_library
      $(if $(wildcard $(subst /,$(DIRSEP),$(LIB_FILE))),@$(RM) $(subst /,$(DIRSEP),$(LIB_FILE)),)
      @$(ECHO) $(ECHOQUOTE)CREATE $(LIB_DIR)/$@$(ECHOQUOTE) > __lib__.tmp
      for %i in ( *$(OBJ_EXT) ) do @$(ECHO) $(ECHOQUOTE)ADDMOD %i$(ECHOQUOTE) >> __lib__.tmp
      @$(ECHO) $(ECHOQUOTE)SAVE$(ECHOQUOTE) >> __lib__.tmp
      @$(ECHO) $(ECHOQUOTE)END$(ECHOQUOTE) >> __lib__.tmp
      $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -M < __lib__.tmp
   endef
endif

# Under OS/2 || isn't a command separator (inside a shell, that is); correct separator is &

ifeq ($(HB_COMPILER),gccomf)
   AR := $(HB_CCPATH)$(HB_CCPREFIX)emxomfar
   ARFLAGS :=
   AR_RULE = $(create_library) $(ARSTRIP)
else
   AR := $(HB_CCPATH)$(HB_CCPREFIX)ar
   ARFLAGS :=
   AR_RULE = $(create_library) $(ARSTRIP) & $(RM) __lib__.tmp
endif

DY := $(CC)
DFLAGS := -shared $(LIBPATHS)
ifeq ($(HB_COMPILER),gccomf)
   DFLAGS += -Zomf
endif
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAS TO exist!
define dyn_object
   @$(ECHO) $(subst $(DIRSEP),/,$(file))>> __dyn__.tmp
   @emxexp $(file)>> __dyn__.def

endef

define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(if $(wildcard __dyn__.def),@$(RM) __dyn__.def,)
   @$(ECHO) $(ECHOQUOTE)LIBRARY $(DYNNAME) INITINSTANCE TERMINSTANCE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)PROTMODE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)CODE PRELOAD MOVEABLE DISCARDABLE$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)DATA PRELOAD MOVEABLE MULTIPLE NONSHARED$(ECHOQUOTE) >> __dyn__.def
   @$(ECHO) $(ECHOQUOTE)EXPORTS$(ECHOQUOTE) >> __dyn__.def
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ @__dyn__.tmp __dyn__.def $(DLIBS) $(DYSTRIP)
   emximp -o $(IMP_FILE) $(DYN_DIR)/$@
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
