HB_BUILD_SHARED := no

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := tcc.exe
CC_IN := -c
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
   # CFLAGS += -Wextra
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

#RC := windres
#RC_OUT := -o$(subst x,x, )
#RCFLAGS += -I. -I$(HB_HOST_INC) -O coff

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(3RDLIB_DIR),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),-l$(lib))

# Add the standard C entry
ifneq ($(HB_LINKING_RTL),)
   ifeq ($(HB_MAIN),)
      LDLIBS += -lhbmainstd
   endif
endif

LDFLAGS += $(LIBPATHS)

AR := tiny_libmaker.exe
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ $^ ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(3RDLIBS) $(SYSLIBS),-l$(lib))
DY_RULE = $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS)

include $(TOP)$(ROOT)config/rules.mk
