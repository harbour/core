#
# $Id$
#

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
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

LD := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCPOSTFIX)
LD_OUT := -o

LIBPATHS := -L$(LIB_DIR)
LDLIBS := $(foreach lib,$(LIBS),-l$(lib))

ifneq ($(filter hbrtl, $(LIBS)),)
   # Add the specified GT driver library
   ifeq ($(HB_CRS_LIB),)
      HB_CRS_LIB := ncurses
   endif
   ifneq ($(filter gtcrs, $(LIBS)),)
      LDLIBS += -l$(HB_CRS_LIB)
   endif
   ifneq ($(filter gtsln, $(LIBS)),)
      LDLIBS += -lslang
      # In BSD, slang still needs curses :(
      ifeq ($(filter gtcrs, $(LIBS)),)
         LDLIBS += -l$(HB_CRS_LIB)
      endif
   endif
   ifneq ($(filter gtxwc, $(LIBS)),)
      LDLIBS += -lX11
     #LIBPATHS += -L/usr/X11R6/lib64
      LIBPATHS += -L/usr/X11R6/lib
   endif

   LIBPATHS += -L/usr/local/lib

   ifneq ($(filter -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
      LDLIBS += -lpcre
   endif

   ifneq ($(filter -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
      LDLIBS += -lz
   endif
endif

LDLIBS += -lm

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPREFIX)ar
ARFLAGS :=
AR_RULE = $(AR) $(ARFLAGS) $(HB_USER_AFLAGS) r $(LIB_DIR)/$@ $(^F) || $(RM) $(LIB_DIR)/$@

DY := $(CC)
DFLAGS := -shared -fPIC
DY_OUT := -o$(subst x,x, )
DLIBS :=

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dyn_object
   @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dyn_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ @__dyn__.tmp
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
