#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := sunCC
else
   HB_CMP := suncc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB -KPIC

CC := $(HB_CCACHE) $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
# NOTE: The ending space after -o is important, please preserve it.
#       Now solved with '$(subst x,x, )' expression.
CC_OUT := -o$(subst x,x, )

CXX := $(HB_CCACHE) $(HB_CCPATH)$(HB_CCPREFIX)sunCC$(HB_CCPOSTFIX)

CFLAGS += -I. -I$(HB_HOST_INC)

# try to keep `-fast' as left as possible, as later optim
# flags may override values set by `-fast', and this way
# no warnings are generated.

ifneq ($(HB_BUILD_OPTIM),no)
   # Together with $(HB_ISAOPT) above, these are supposed to (somewhat)
   # conform to the Blastwave build standards, see
   #    http://wiki.blastwave.org/mediawiki/index.php/Build_Standards
   # Try to keep them this way.
   CFLAGS += -fast
   CFLAGS += -xnolibmopt
endif

export HB_ISAOPT

CFLAGS += $(HB_ISAOPT)
LDFLAGS += $(HB_ISAOPT)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -erroff=%none
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -G $(HB_ISAOPT) $(LIBPATHS)
ifneq ($(HB_BUILD_OPTIM),no)
   DFLAGS += -fast -xnolibmopt
endif
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) -h $(DYN_NAME_CPT) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)

include $(TOP)$(ROOT)config/rules.mk
