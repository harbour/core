# Brecht Sanders winlibs clang distribution and possiblty others are impossible
# to detect from path alone as clang.exe resides in the same directory with gcc,
# in Harbour 3.2 gcc has priority over clang for backwards compatibility

# supp. actions if the set HB_COMPILER=clang was specified, not auto-detected

ifneq ($(HB_COMPILER_ORI),)
   HB_COMP_PATH := $(call find_in_path_raw,$(HB_CCPREFIX)clang$(HB_HOST_BIN_EXT))
   HB_COMP_PWD  := $(call dir_with_spaces,$(HB_COMP_PATH))

   ifneq ($(HB_COMP_PATH),)
      # following $(HB_CCPREFIX)x86_64..., etc. looks wrong, but it's intentional
      # the goal is to pacify this multilib env best guessing when HB_CCPREFIX= is set
      ifneq ($(wildcard $(HB_COMP_PWD)../$(HB_CCPREFIX)x86_64-w64-mingw32/lib/lib*.a),)
         MSYSTEM := CLANG64
         HB_CPU := x86_64
      else
      ifneq ($(wildcard $(HB_COMP_PWD)../$(HB_CCPREFIX)i686-w64-mingw32/lib/lib*.a),)
         MSYSTEM := CLANG32
         HB_CPU := x86
      else
      ifneq ($(wildcard $(HB_COMP_PWD)../$(HB_CCPREFIX)aarch64-w64-mingw32/lib/lib*.a),)
         MSYSTEM := CLANGARM64
         HB_CPU := arm64
      else
      ifneq ($(findstring /VC/Tools/Llvm/ARM64/,$(HB_COMP_PATH)),)
         MSYSTEM :=
         HB_CPU := arm64
      else
      ifneq ($(findstring /VC/Tools/Llvm/x64/,$(HB_COMP_PATH)),)
         MSYSTEM :=
         HB_CPU := x86_64
      else
      ifneq ($(findstring mingw64,$(HB_COMP_PWD)),)
         MSYSTEM := CLANG64
      else
      ifneq ($(findstring mingw32,$(HB_COMP_PWD)),)
         MSYSTEM := CLANG32
         HB_CPU := x86
      else
         MSYSTEM := $(shell $(HB_CCPREFIX)clang --version)
         ifneq ($(findstring x86_64-pc-windows-msvc,$(MSYSTEM)),)
            MSYSTEM :=
            HB_CPU := x86_64
         else
         ifneq ($(findstring i686-pc-windows-msvc,$(MSYSTEM)),)
            MSYSTEM :=
            HB_CPU := x86
         else
         ifneq ($(findstring aarch64-pc-windows-msvc,$(MSYSTEM)),)
            MSYSTEM :=
            HB_CPU := arm64
         else
         ifneq ($(findstring x86_64-w64-windows-gnu,$(MSYSTEM)),)
            HB_CPU := x86_64
            MSYSTEM := CLANG64
         else
         ifneq ($(findstring i686-w64-windows-gnu,$(MSYSTEM)),)
            HB_CPU := x86
            MSYSTEM := CLANG32
         else
         ifneq ($(findstring aarch64-w64-windows-gnu,$(MSYSTEM)),)
            HB_CPU := arm64
            MSYSTEM := CLANGARM64
         else
         ifneq ($(findstring -windows-gnu,$(MSYSTEM)),)
            MSYSTEM := CLANG
         else
            MSYSTEM :=
         endif
         endif
         endif
         endif
         endif
         endif
         endif
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      export MSYSTEM
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
endif
