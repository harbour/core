# https://developer.mozilla.org/docs/WebAssembly/C_to_wasm
# https://kripken.github.io/emscripten-site/docs/compiling/Building-Projects.html

ifeq ($(HB_BUILD_MODE),cpp)
   ifneq ($(findstring clang$(subst x, ,x)version$(subst x, ,x)1,$(shell clang --version)),)
      HB_BUILD_MODE := c
   endif
endif

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := em++
else
   HB_CMP := emcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
# NOTE: The ending space after -o is important, please preserve it.
CC_OUT := -o$(subst x,x, )

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

ifneq ($(HB_BUILD_OPTIM),no)
   ifeq ($(HB_BUILD_DEBUG),yes)
      ifeq ($(filter $(HB_COMPILER_VER),0304 0305 0306 0307 0308 0309),)
         CFLAGS += -O0
      else
         CFLAGS += -O1
      endif
   else
      CFLAGS += -O3
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(CC)
# NOTE: The ending space after -o is important, please preserve it.
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

LDFLAGS += -sWASM=1 $(LIBPATHS)

AR := $(LLVM_ROOT)/llvm-ar
AR_RULE = ( $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) rcs \
   $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) \
   || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY := $(CC)
DFLAGS += -shared $(LIBPATHS)
DY_OUT := -o$(subst x,x, )
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

DY_RULE = $(DY) $(DFLAGS) -Wl,-soname,$(DYN_NAME_CPT) $(HB_USER_DFLAGS) \
   $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) \
   && $(LN) $(@F) $(DYN_FILE_NVR) \
   && $(LN) $(@F) $(DYN_FILE_CPT)

include $(TOP)$(ROOT)config/rules.mk
