ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC) -c

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   # Use -O2 instead of -O3 here.
   CFLAGS += -O2
   ifneq ($(HB_BUILD_DEBUG),yes)
      # It's the default in 4.6 and up
      ifneq ($(filter $(HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405),)
         CFLAGS += -fomit-frame-pointer
      endif
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

RC := $(HB_CCPATH)$(HB_CCPREFIX)windres
RC_OUT := -o$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC) -O coff

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && ${HB_CCPATH}${HB_CCPREFIX}strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := $(HB_CCPATH)$(HB_CCPREFIX)ar

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define library_object
   @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file))$(ECHOQUOTE) >> __lib__.tmp

endef
define create_library
   $(if $(wildcard __lib__.tmp),@$(RM) __lib__.tmp,)
   $(foreach file,$^,$(library_object))
   ( $(AR) rcs $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) $(LIB_DIR)/$@ @__lib__.tmp $(ARSTRIP) ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )
endef

AR_RULE = $(create_library)

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

# NOTE: The empty line directly before 'endef' HAVE TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp $(DLIBS) -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def $(DYSTRIP)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
