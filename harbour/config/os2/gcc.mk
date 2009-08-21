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

CC := $(HB_CMP)
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

LD := $(HB_CMP)
LD_OUT := -o$(subst x,x, )

ifeq ($(C_MAIN),)
   ifeq ($(HB_GT_LIB),os2pm)
      # If building a PM program, override the main object.
      LDFLAGS += $(TOP)$(ROOT)source/vm/$(OBJ_DIR)/mainpm.o
   endif
endif

LIBPATHS := -L$(LIB_DIR)
LDLIBS := $(foreach lib,$(LIBS),-l$(lib))

# Add the specified GT driver library
ifneq ($(filter hbrtl, $(LIBS)),)
   ifeq ($(C_MAIN),)
      ifeq ($(filter os2pm,$(HB_GT_LIB)),os2pm)
         # Special handling for PM mode
         LDLIBS += -l$(HB_GT_LIB)
         LDLIBS += -lgtos2
      endif
   endif

   LDLIBS += -lsocket
endif

# statical linking with GCC 3.2.2 libc as not require its presence on user system
LDFLAGS += $(LIBPATHS)

ifeq ($(C_MAIN),)
   ifeq ($(HB_GT_LIB),os2pm)
      # Override the default link rule in order to add a call to emxbind
      LD_RULE = $(LD) $(CFLAGS) $(LD_OUT)$(BIN_DIR)/$@ $(^F) $(LDFLAGS) $(HB_USER_LDFLAGS) $(LDLIBS) & emxbind -ep $@
   endif
endif

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#       It causes that every command will be separated by LF
#define lib_object
#   @$(ECHO) ADDMOD $(file) >> __lib__.tmp
#
#endef

# We have to use a script to overcome the AR limit of max 850 characters
# in commmand line
define create_library
   $(if $(wildcard $(subst /,$(DIRSEP),$(LIB_FILE))),@$(RM) $(subst /,$(DIRSEP),$(LIB_FILE)),)
   @$(ECHO) CREATE $(LIB_DIR)/$@ > __lib__.tmp
   for %i in ( *$(OBJ_EXT) ) do @$(ECHO) ADDMOD %i >> __lib__.tmp
   @$(ECHO) SAVE >> __lib__.tmp
   @$(ECHO) END >> __lib__.tmp
   $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) -M < __lib__.tmp
endef

# Under OS/2 || isn't a command separator (inside a shell, that is); correct separator is &
AR := ar
ARFLAGS :=
AR_RULE = $(create_library) & $(RM) __lib__.tmp

include $(TOP)$(ROOT)config/rules.mk
