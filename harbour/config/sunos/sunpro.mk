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

CC := $(HB_CCACHE) $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
CC_IN := -c
# NOTE: The ending space after -o is important, please preserve it.
#       Now solved with '$(subst x,x, )' expression.
CC_OUT := -o$(subst x,x, )

CPPFLAGS := -I. -I$(HB_INC_COMPILE)
CFLAGS :=
LDFLAGS :=

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
   # workaround for problems in x86 PIC builds exploited by hbpcre library code
   ifeq ($(LIBNAME),hbpcre)
      ifneq ($(findstring sparc,$(shell isalist)),sparc)
         CFLAGS += -xbuiltin=%none
      endif
   endif
endif

# force position independent code for harbour shared library
# it's not optimal but without support for compilation in two passes
# we have to use this option also for static libraries and binaries
ifeq ($(findstring sparc,$(shell isalist)),sparc)
   CFLAGS += -xcode=pic32
else
   CFLAGS += -KPIC
endif

export HB_ISAOPT

CFLAGS += $(HB_ISAOPT)
LDFLAGS += $(HB_ISAOPT)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -erroff=%none
else
   CFLAGS += -erroff=%all
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(HB_CCACHE) $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(LIB_DIR)

LDLIBS := $(foreach lib,$(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(foreach dir,$(LIBPATHS) $(SYSLIBPATHS),-L$(dir))

AR := ar
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) cr $(LIB_DIR)/$@ $(^F) || ( $(RM) $(LIB_DIR)/$@ && false )

DY := $(CC)
DFLAGS := -G $(HB_ISAOPT) $(foreach dir,$(SYSLIBPATHS),-L$(dir))
ifneq ($(HB_BUILD_OPTIM),no)
   DFLAGS += -fast -xnolibmopt
endif
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS)

include $(TOP)$(ROOT)config/rules.mk
