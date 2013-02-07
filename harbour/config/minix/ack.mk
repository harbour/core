#
# $Id$
#

## XXX: completely untested

ifeq ($(HB_BUILD_MODE),cpp)
   $(error ! C++ mode is not yet supported on $(HB_PLATFORM) with $(HB_COMPILER))
   HB_CMP := CC
else
   HB_CMP := cc
endif

HB_BUILD_SHARED := no
HB_BUILD_DYN := no

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN := -c
CC_OUT := -o

CFLAGS += -D_MINIX=1 -D_POSIX_SOURCE=1 -I. -I$(HB_HOST_INC)

ifeq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O0
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(CC)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPREFIX)ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

include $(TOP)$(ROOT)config/rules.mk
