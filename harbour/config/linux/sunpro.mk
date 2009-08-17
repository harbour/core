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

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)
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
      CFLAGS += -xbuiltin=%none
   endif
endif

# force position independent code for harbour shared library
# it's not optimal but without support for compilation in two passes
# we have to use this option also for static libraries and binaries
CFLAGS += -KPIC

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

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)
LD_OUT := -o$(subst x,x, )

LIBPATHS := -L$(LIB_DIR)
LDLIBS := $(foreach lib,$(LIBS),-l$(lib))

ifneq ($(findstring hbrtl, $(LIBS)),)
   # Add the specified GT driver library
   ifneq ($(findstring gtcrs, $(LIBS)),)
      ifeq ($(HB_CRS_LIB),)
         HB_CRS_LIB := ncurses
      endif
      LDLIBS += -l$(HB_CRS_LIB)
   endif
   ifneq ($(findstring gtsln, $(LIBS)),)
      LDLIBS += -lslang
   endif
   ifneq ($(findstring gtxwc, $(LIBS)),)
      LDLIBS += -lX11
      LIBPATHS += -L/usr/X11R6/lib
   endif

   # HB_GPM_MOUSE: use gpm mouse driver
   ifeq ($(HB_GPM_MOUSE),yes)
      LDLIBS += -lgpm
   endif

   ifneq ($(findstring -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
      LDLIBS += -lpcre
   endif

   ifneq ($(findstring -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
      LDLIBS += -lz
   endif

   LDLIBS += -lrt -ldl
endif

LDLIBS += -lm

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPREFIX)ar
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) crs $(LIB_DIR)/$@ $(^F) || ( $(RM) $(LIB_DIR)/$@ && false )

include $(TOP)$(ROOT)config/rules.mk
