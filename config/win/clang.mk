ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := clang++
else
   HB_CMP := clang
endif

OBJ_EXT := .o
ifeq ($(MSYSTEM),)
   LIB_PREF :=
   LIB_EXT := .lib
else
   LIB_PREF := lib
   LIB_EXT := .a
endif

HB_DYN_COPT := -DHB_DYNLIB

CC := $(HB_CCPATH)$(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
ifneq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CC_IN :=
else
   CC_IN := -c
endif
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)

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

AR := $(HB_CCPREFIX)llvm-ar

AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(subst /,$(DIRSEP),$(LIB_DIR)/$@) && $(FALSE) )

# TODO: add resource compiler detect chain in this stage: GNU windres, llvm-windres, llvm-rc

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

ifeq ($(HB_SHELL),nt)
   define dynlib_object
      @$(ECHO) $(ECHOQUOTE)$(subst \,/,$(file)) $(ECHOQUOTE) >> __dyn__.tmp

   endef
   ifneq ($(MSYSTEM),)
      # no-op on MSYS under cmd.exe shell
      define dynlib_ln

      endef
   else
      define dynlib_ln
         $(LN) $(subst /,\,$(DYN_DIR)\$(LIB_PREF)$(basename $@)$(LIB_EXT)) $(subst /,\,$(LIB_DIR)\$(LIB_PREF)$(basename $@)$(LIB_EXT))
      endef
   endif
else
   define dynlib_object
      @$(ECHO) -n $(ECHOQUOTE)$(subst \,/,$(file)) $(ECHOQUOTE) >> __dyn__.tmp

   endef
   define dynlib_ln
      $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)
   endef
endif

TMPSPEC := @__dyn__.tmp

# setting HB_USER_FIXES=--mingw-script[...]
# may help to workaround if old clang + MinGW linker is in use,
# build may fail either with too long command line or unrecognized argument
ifneq ($(filter --mingw-script, $(HB_USER_FIXES)),)
   # NOTE: The empty line directly before 'endef' HAS TO exist!
   override define dynlib_object
      @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

   endef
   TMPSPEC := __dyn__.tmp
endif

define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $(TMPSPEC) $(DLIBS) $(IMPLIBFLAGS) $(DYSTRIP) $(DYSTRIP)
   $(dynlib_ln)
endef

# clang distributed by MS uses lld-link, libs are *.lib not lib*.a
# in opposite MSYS/MinGW needs args to actually make an implib
ifeq ($(MSYSTEM),)
   LDFLAGS += -Wl,-subsystem:console
   DFLAGS += -Wl,-subsystem:console
else
   IMPLIBFLAGS = -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def
endif


DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
