ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := zig c++
else
   HB_CMP := zig cc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

HB_DYN_COPT := -DHB_DYNLIB

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
ifneq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CC_IN :=
else
   CC_IN := -c
endif
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

CFLAGS += -Wno-error=date-time
# NOTE: no reproducible builds guaranteed by Harbour 3.2

ifneq ($(HB_ZIG_TARGET),)
   LDFLAGS += -target $(HB_ZIG_TARGET)
   DFLAGS += -target $(HB_ZIG_TARGET)
   CFLAGS += -target $(HB_ZIG_TARGET)
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
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

LD := $(CC)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += $(LIBPATHS)

AR := zig ar

AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

ifeq ($(HB_SHELL),nt)
   define dynlib_object
      @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file)) $(ECHOQUOTE) >> __dyn__.tmp

   endef
   # no-op under cmd.exe shell
   define dynlib_ln

   endef

# FIXME: symlinks for dynlibs generated when cross compiling from windows
#        to unix are not created, decide if we want a copy or something else
#
#  define dynlib_ln
#     $(LN) $(subst /,\,$(DYN_DIR)\$(LIB_PREF)$(basename $@)$(LIB_EXT)) $(subst /,\,$(LIB_DIR)\$(LIB_PREF)$(basename $@)$(LIB_EXT))
#  endef

else
   define dynlib_object
      @$(ECHO) -n $(ECHOQUOTE)$(subst \,/,$(file)) $(ECHOQUOTE) >> __dyn__.tmp

   endef
   define dynlib_ln
      $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)
   endef
endif

TMPSPEC := @__dyn__.tmp

define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $(TMPSPEC) $(DLIBS) $(IMPLIBFLAGS) $(DYSTRIP) $(DYSTRIP)
   $(dynlib_ln)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
