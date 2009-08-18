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

CPPFLAGS := -zq -bt=dos
CFLAGS :=
LDFLAGS :=

ifneq ($(HB_BUILD_WARN),no)
   CPPFLAGS += -w3
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # architecture flags
   CPPFLAGS += -5r -fp5

   # optimization flags
   # don't enable -ol optimization in OpenWatcom 1.1 - gives buggy code
   CPPFLAGS += -onaehtr -s -ei -zp4 -zt0
   #CPPFLAGS += -obl+m
   ifeq ($(CC),wpp386)
      CPPFLAGS += -oi+
   else
      CPPFLAGS += -oi
   endif
endif

CPPFLAGS += -i. -i$(HB_INC_COMPILE)

ifeq ($(HB_BUILD_DEBUG),yes)
   CPPFLAGS += -d2
endif

ifeq ($(CC),wcc386)
   ifneq ($(HB_HOST_ARCH),linux)
      CPPFLAGS := $(subst /,\,$(CPPFLAGS))
      CC_RULE = $(CC) $(subst /,\,$(HB_INC_DEPEND)) $(CPPFLAGS) $(subst /,\,$(CFLAGS)) $(subst /,\,$(HB_CFLAGS)) $(subst /,\,$(HB_USER_CFLAGS)) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(CC_IN)$(subst /,\,$<)
   endif
endif

# work arround to DOS command line size limit
ifeq ($(CC),wcc386)
   export WCC386 := $(strip $(CPPFLAGS))
else
   export WPP386 := $(strip $(CPPFLAGS))
endif
CPPFLAGS :=

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#       It causes that every command will be separated by LF
define link_file
   @$(ECHO) FILE $(file) >> __link__.tmp

endef

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define link_lib
   @$(ECHO) LIB $(lib) >> __link__.tmp

endef

define link_exe_file
   @$(ECHO) $(LDFLAGS) NAME $(BIN_DIR)/$@ > __link__.tmp
   $(foreach file,$(^F),$(link_file))
   $(foreach lib,$(LDLIBS),$(link_lib))
   -$(LD) @__link__.tmp
endef

LD := wlink
ifeq ($(HB_BUILD_DEBUG),yes)
   LDFLAGS += DEBUG ALL
endif
# different SYS values: dos4g (default), pmodew (commercial), causeway
ifeq ($(LIBNAME),hbpp)
   # we force causeway here as workaround for reduced command line size in dos4g
   LDFLAGS += SYS causeway
else
   LDFLAGS += SYS dos4g OP STUB=wstubq.exe
endif

LDLIBS := $(foreach lib,$(LIBS),$(LIB_DIR)/$(lib))

LD_RULE = $(link_exe_file) $(HB_USER_LDFLAGS)

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define lib_object
   @$(ECHO) -+$(file) >> __lib__.tmp

endef

define create_library
   @$(ECHO) $(LIB_DIR)/$@ > __lib__.tmp
   $(foreach file,$(^F),$(lib_object))
   $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) @__lib__.tmp
endef

AR := wlib
ARFLAGS := -q -p=64 -c -n
AR_RULE = $(create_library)

# disable DOS/4GW Banner
export DOS4G := quiet

include $(TOP)$(ROOT)config/rules.mk

# work arround to DOS command line size limit
export HARBOURCMD := $(HB_FLAGS)
HB_FLAGS :=
