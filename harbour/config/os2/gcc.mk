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
LIB_EXT := .a

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
CC_OUT := -o

CPPFLAGS := -I. -I$(HB_INC_COMPILE)
CFLAGS :=
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -Wall -W
endif

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

ifeq ($(C_MAIN),)
   ifeq ($(HB_GT_LIB),os2pm)
      # If building a PM program, override the main object.
      LDFLAGS += $(TOP)$(ROOT)source/vm/$(OBJ_DIR)/mainpm.o
   endif
endif

SYSLIBS :=
SYSLIBPATHS :=

ifneq ($(HB_LINKING_RTL),)
   ifeq ($(C_MAIN),)
      ifeq ($(filter os2pm,$(HB_GT_LIB)),os2pm)
         # Special handling for PM mode
         LIBS += $(HB_GT_LIB)
         LIBS += gtos2
      endif
   endif

   SYSLIBS += socket
endif

LD := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))

# static linking with GCC 3.2.2 libc as not require its presence on user system
LDFLAGS += $(LIBPATHS)

ifeq ($(C_MAIN),)
   ifeq ($(HB_GT_LIB),os2pm)
      # Override the default link rule in order to add a call to emxbind
      LD_RULE = $(LD) $(CFLAGS) $(LD_OUT)$(BIN_DIR)/$@ $(^F) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) $(LDLIBS) & emxbind -ep $@
   endif
endif

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#       It causes that every command will be separated by LF
#define lib_object
#   @$(ECHO) $(ECHOQUOTE)ADDMOD $(file)$(ECHOQUOTE) >> __lib__.tmp
#
#endef

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

# Under OS/2 || isn't a command separator (inside a shell, that is); correct separator is &
AR := $(HB_CCPATH)$(HB_CCPREFIX)ar
ARFLAGS :=
AR_RULE = $(create_library) & $(RM) __lib__.tmp

#DY := $(CC)
#DFLAGS := -shared $(LIBPATHS)
#DY_OUT := $(LD_OUT)
#DLIBS := $(foreach lib,$(SYSLIBS),-l$(lib))
#
# NOTE: The empty line directly before 'endef' HAVE TO exist!
#define dyn_object
#   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp
#
#endef
#define create_dynlib
#   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
#   $(foreach file,$^,$(dyn_object))
#   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) -Wl,--output-def,$(DYN_DIR)/$(basename $@).def,--out-implib,$(IMP_FILE)
#endef
#
#DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
