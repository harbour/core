ifeq ($(HB_BUILD_MODE),cpp)
   ifneq ($(findstring clang$(subst x, ,x)version$(subst x, ,x)1,$(shell clang --version)),)
      HB_BUILD_MODE := c
   endif
endif

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := clang++
else
   HB_CMP := clang
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

ifneq ($(HB_PLATFORM),darwin)
   HB_DYN_COPT := -DHB_DYNLIB -fPIC
endif

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)
ifeq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CFLAGS += -c
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Weverything
   CFLAGS += -Wno-padded -Wno-cast-align -Wno-float-equal -Wno-missing-prototypes
   CFLAGS += -Wno-disabled-macro-expansion -Wno-undef -Wno-unused-macros -Wno-variadic-macros -Wno-documentation
   CFLAGS += -Wno-switch-enum
   ifeq ($(HB_PLATFORM),darwin)
      ifeq ($(filter $(HB_COMPILER_VER),0304 0305 0306),)
         CFLAGS += -Wno-reserved-id-macro
      endif
   else
      ifeq ($(filter $(HB_COMPILER_VER),0304 0305),)
         CFLAGS += -Wno-reserved-id-macro
      endif
   endif
   CFLAGS += -Wno-empty-translation-unit
   # These are potentially useful. -Wsign-conversion would require proper HB_SIZE/HB_ISIZ cleanup.
   CFLAGS += -Wno-sign-conversion -Wno-shorten-64-to-32 -Wno-conversion -Wno-bad-function-cast
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

#CFLAGS += -D_FORTIFY_SOURCE=2

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

ifeq ($(HB_PLATFORM),darwin)
   AR := libtool
   AR_RULE = ( $(AR) -static -no_warning_for_no_symbols $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) -o $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

   DY := $(AR)
   DFLAGS += -dynamic -flat_namespace -undefined warning -multiply_defined suppress -single_module $(LIBPATHS)
   DY_OUT := -o$(subst x,x, )
   DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

   DY_RULE = $(DY) $(DFLAGS) -install_name "$(DYN_NAME_NVR)" -compatibility_version $(HB_VER_MAJOR).$(HB_VER_MINOR) -current_version $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)
else
   AR := $(HB_CCPREFIX)ar
   AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

   DY := $(CC)
   DFLAGS += -shared $(LIBPATHS)
   DY_OUT := -o$(subst x,x, )
   DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

   DY_RULE = $(DY) $(DFLAGS) -Wl,-soname,$(DYN_NAME_CPT) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) && $(LN) $(@F) $(DYN_FILE_NVR) && $(LN) $(@F) $(DYN_FILE_CPT)
endif

include $(TOP)$(ROOT)config/rules.mk
