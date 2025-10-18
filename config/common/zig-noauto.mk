ifneq ($(HB_COMPILER_ORI),)
   ifeq ($(filter -target, $(HB_USER_CFLAGS)),)
      ifeq ($(HB_ZIG_TARGET),)
         HB_CPU := $(HB_HOST_CPU)
      else
         ifeq ($(HB_CPU),)
            $(error ! zig toolchain helper setting HB_ZIG_TARGET is specified, please also set equivalent HB_CPU and possibly HB_PLATFORM, HB_HOST_BIN to proceed with cross-compilation )
         endif
      endif
   else
      ifeq ($(HB_CPU),)
         $(error ! zig toolchain -target option is specified in HB_USER_CFLAGS, please also set equivalent HB_CPU and possibly HB_PLATFORM, HB_HOST_BIN to proceed with cross-compilation )
      endif
   endif
   ifneq ($(HB_CPU),$(HB_HOST_CPU))
      ifeq ($(HB_BUILD_NAME),)
         ifeq ($(HB_CPU),x86_64)
            export HB_BUILD_NAME := 64
         else
            export HB_BUILD_NAME := $(HB_CPU)
         endif
      endif
   endif
endif
