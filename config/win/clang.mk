# currently supporting clang-cl (not clang-gcc)

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -Weverything
   CFLAGS += -Wno-padded -Wno-cast-align -Wno-float-equal -Wno-missing-prototypes
   CFLAGS += -Wno-disabled-macro-expansion -Wno-undef -Wno-unused-macros -Wno-variadic-macros -Wno-documentation
   CFLAGS += -Wno-switch-enum
   ifeq ($(filter $(HB_COMPILER_VER),0305),)
      CFLAGS += -Wno-reserved-id-macro
   endif
   # These are potentially useful. -Wsign-conversion would require proper HB_SIZE/HB_ISIZ cleanup.
   CFLAGS += -Wno-sign-conversion -Wno-shorten-64-to-32 -Wno-conversion -Wno-bad-function-cast
   CFLAGS += -Wno-language-extension-token
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/msvc.mk
