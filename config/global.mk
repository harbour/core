# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

# GNU make docs:
#    https://www.gnu.org/software/make/manual/make.html
#    http://wanderinghorse.net/computing/make/
#    https://blog.jgc.org/2013/02/updated-list-of-my-gnu-make-articles.html
#    https://lists.gnu.org/archive/html/help-make/
#    http://make.paulandlesley.org/
# Portable shell programming:
#    https://www.gnu.org/software/autoconf/manual/html_node/Portable-Shell.html
#    https://www.gnu.org/software/bash/manual/bashref.html
#    https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html
# GNU coding standards:
#    https://www.gnu.org/prep/standards/standards.html
# GNU Make NEWS:
#    http://git.savannah.gnu.org/cgit/make.git/tree/NEWS

# NOTE: $(realpath/abspath) need GNU Make 3.81 or upper
# NOTE: $(eval) needs GNU Make 3.80 or upper

ifeq ($(GLOBAL_MK_),)
GLOBAL_MK_ := yes

-include $(TOP)$(ROOT)user.mk

HB_VER_MAJOR     := 3
HB_VER_MINOR     := 4
HB_VER_RELEASE   := 0
# Status (dev, alpha1, alpha2, beta1, beta2, rc1, rc2, empty for final)
HB_VER_STATUS    := dev
# Short status (d, a1, a2, b1, b2, r1, r2, empty for final)
HB_VER_STATUS_SH := d

export HB_VER_MAJOR
export HB_VER_MINOR

ifeq ($(HB_VER_STATUS_SH),)
   HB_VER_STATUS_SH := $(HB_VER_MINOR)$(HB_VER_RELEASE)
endif

# Arbitrary pattern which we do not expect to occur in real-world path names
substpat := !@!@

# This is not strictly necessary, but it does signficantly reduce
# the number of rules that make has to evaluate otherwise, which may give
# a performance boost on a slow system.
.SUFFIXES:

.PHONY: all clean install

_make_ver_min := 3.81
_make_ver_ok := $(filter $(_make_ver_min),$(firstword $(sort $(MAKE_VERSION) $(_make_ver_min))))
ifeq ($(_make_ver_ok),)
   ifeq ($(_make_ver_warn),)
      $(warning ! Warning: GNU Make version $(MAKE_VERSION) found, $(_make_ver_min) or upper recommended for Harbour)
      export _make_ver_warn := yes
   endif
endif

# Detect GNU Make version compatibility (unsupported functions return empty value in GNU Make)
ifneq ($(abspath .),)
   _MAKE_COMPAT_381 := yes
endif
# $(eval _MAKE_COMPAT_380 := yes)

# Users must specify HB_SRC_ROOTPATH only for < 3.81 GNU Make versions
# (without '$(realpath)' function). For newer ones we clear it
# to avoid messing things up.
ifneq ($(_MAKE_COMPAT_381),)
   HB_SRC_ROOTPATH :=
else
   # Condition it to have forward slashes, a guaranteed ending slash and no double slashes, if specified
   ifneq ($(HB_SRC_ROOTPATH),)
      HB_SRC_ROOTPATH := $(subst //,/,$(subst \,/,$(HB_SRC_ROOTPATH))/)
   endif
endif

find_in_path     = $(strip $(subst $(substpat), ,$(firstword $(subst |, ,$(subst $(subst x, ,x),$(substpat),$(filter-out |,$(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(PATH))),|$(wildcard $(subst //,/,$(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1))$(HB_HOST_BIN_EXT)))))))))
find_in_path_raw = $(strip $(subst $(substpat), ,$(firstword $(subst |, ,$(subst $(subst x, ,x),$(substpat),$(filter-out |,$(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(PATH))),|$(wildcard $(subst //,/,$(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1))))))))))
find_in_path_par = $(strip $(subst $(substpat), ,$(firstword $(subst |, ,$(subst $(subst x, ,x),$(substpat),$(filter-out |,$(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(2))),|$(wildcard $(subst //,/,$(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1))$(HB_HOST_BIN_EXT)))))))))
find_in_path_prw = $(strip $(subst $(substpat), ,$(firstword $(subst |, ,$(subst $(subst x, ,x),$(substpat),$(filter-out |,$(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(2))),|$(wildcard $(subst //,/,$(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1))))))))))

# Some presets based on HB_BUILD_NAME
ifneq ($(HB_BUILD_NAME),)
   export HB_BUILD_NAME := $(subst /,,$(subst \,/,$(HB_BUILD_NAME)))
   ifeq ($(HB_BUILD_NAME),.r)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.rp)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_MODE ?= cpp
   else
   ifeq ($(HB_BUILD_NAME),.d)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.dp)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_MODE ?= cpp
   endif
   endif
   endif
   endif
endif

ifeq ($(HB_INIT_DONE),)
   # Store the original value
   export HB_MAKECMDGOALS := $(MAKECMDGOALS)

   ifeq ($(HB_BUILD_PKG),yes)

      # We need some >= 3.81 GNU Make feature to make this option work,
      # or we need HB_SRC_ROOTPATH to be specified by user.
      ifeq ($(_MAKE_COMPAT_381),)
         ifeq ($(HB_SRC_ROOTPATH),)
            export HB_BUILD_PKG := no
         endif
      endif
      # 'clean' and 'install' are required when building a release package
      ifeq ($(filter clean,$(HB_MAKECMDGOALS)),)
         export HB_BUILD_PKG := no
      else
      ifeq ($(filter install,$(HB_MAKECMDGOALS)),)
         export HB_BUILD_PKG := no
      else
      ifneq ($(ROOT),./)
         export HB_BUILD_PKG := no
      endif
      endif
      endif

      ifeq ($(HB_BUILD_PKG),no)
         $(warning ! Warning: Use 'clean install' from Harbour root directory to create a release package.)
      endif

      # Enforce some basic settings for release packages
      export HB_BUILD_DYN := yes
      export HB_BUILD_OPTIM := yes
      export HB_BUILD_DEBUG := no
      export HB_BUILD_SHARED := no
      export HB_INSTALL_IMPLIB := yes
      export HB_REBUILD_EXTERN := no
      export HB_REBUILD_PARSER := no
   endif

   # Cannot build shared tools if we don't create dlls
   ifeq ($(HB_BUILD_DYN),no)
      export HB_BUILD_SHARED := no
   endif

   ifeq ($(HB_INSTALL_IMPLIB),yes)
      # 'install' is required to create import libraries
      ifeq ($(filter install,$(HB_MAKECMDGOALS)),)
         export HB_INSTALL_IMPLIB := no
         $(warning ! Warning: HB_INSTALL_IMPLIB option has an effect only if 'install' is requested.)
      endif
   endif

   ifneq ($(__HB_MT),)
      ifeq ($(filter $(__HB_MT),yes no),)
         export __HB_MT :=
      endif
   endif
endif

# Make platform detection
ifneq ($(findstring COMMAND,$(SHELL)),)
   HB_MAKE_PLAT := dos
else
ifneq ($(findstring sh.exe,$(SHELL)),)
   HB_MAKE_PLAT := win
else
ifneq ($(findstring CMD.EXE,$(SHELL)),)
   HB_MAKE_PLAT := os2
else
   HB_MAKE_PLAT := unix
endif
endif
endif

ifeq ($(HB_INIT_DONE),)

   ifeq ($(_MAKE_COMPAT_381),)
      ifeq ($(HB_SRC_ROOTPATH),)
         $(warning ! Warning: Using < 3.81 GNU Make version and empty HB_SRC_ROOTPATH. Some features may not work.)
      endif
   endif

   $(info ! Building Harbour $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)$(HB_VER_STATUS) from source)
   $(info ! MAKE: $(MAKE) $(MAKE_VERSION) '$(SHELL)' $(HB_MAKECMDGOALS) $(MAKEFLAGS) $(if $(MAKESHELL),MAKESHELL: $(MAKESHELL),))
   ifneq ($(HB_USER_PRGFLAGS),)
      $(info ! HB_USER_PRGFLAGS: $(HB_USER_PRGFLAGS))
   endif
   ifneq ($(HB_USER_CFLAGS),)
      $(info ! HB_USER_CFLAGS: $(HB_USER_CFLAGS))
   endif
   ifneq ($(HB_USER_LDFLAGS),)
      $(info ! HB_USER_LDFLAGS: $(HB_USER_LDFLAGS))
   endif
   ifneq ($(HB_USER_AFLAGS),)
      $(info ! HB_USER_AFLAGS: $(HB_USER_AFLAGS))
   endif
   ifneq ($(HB_USER_DFLAGS),)
      $(info ! HB_USER_DFLAGS: $(HB_USER_DFLAGS))
   endif
   ifneq ($(HB_USER_LIBS),)
      $(info ! HB_USER_LIBS: $(HB_USER_LIBS))
   endif
   ifneq ($(HB_INSTALL_PREFIX),)
      $(info ! HB_INSTALL_PREFIX: $(HB_INSTALL_PREFIX))
   endif
   ifneq ($(HB_INSTALL_BIN),)
      $(info ! HB_INSTALL_BIN: $(HB_INSTALL_BIN))
   endif
   ifneq ($(HB_INSTALL_LIB),)
      $(info ! HB_INSTALL_LIB: $(HB_INSTALL_LIB))
   endif
   ifneq ($(HB_INSTALL_DYN),)
      $(info ! HB_INSTALL_DYN: $(HB_INSTALL_DYN))
   endif
   ifneq ($(HB_INSTALL_INC),)
      $(info ! HB_INSTALL_INC: $(HB_INSTALL_INC))
   endif
   ifneq ($(HB_INSTALL_DOC),)
      $(info ! HB_INSTALL_DOC: $(HB_INSTALL_DOC))
   endif
   ifneq ($(HB_INSTALL_MAN),)
      $(info ! HB_INSTALL_MAN: $(HB_INSTALL_MAN))
   endif
   ifneq ($(HB_INSTALL_ETC),)
      $(info ! HB_INSTALL_ETC: $(HB_INSTALL_ETC))
   endif
   ifneq ($(HB_BUILD_NAME),)
      $(info ! HB_BUILD_NAME: $(HB_BUILD_NAME))
   endif
   ifneq ($(HB_BUILD_PKG),)
      $(info ! HB_BUILD_PKG: $(HB_BUILD_PKG))
   endif
   ifneq ($(HB_BUILD_DYN),)
      $(info ! HB_BUILD_DYN: $(HB_BUILD_DYN))
   endif
   ifneq ($(HB_BUILD_CONTRIB_DYN),)
      $(info ! HB_BUILD_CONTRIB_DYN: $(HB_BUILD_CONTRIB_DYN))
   endif
   ifneq ($(HB_BUILD_SHARED),)
      $(info ! HB_BUILD_SHARED: $(HB_BUILD_SHARED))
   endif
   ifneq ($(HB_BUILD_DEBUG),)
      $(info ! HB_BUILD_DEBUG: $(HB_BUILD_DEBUG))
   endif
   ifneq ($(HB_BUILD_STRIP),)
      $(info ! HB_BUILD_STRIP: $(HB_BUILD_STRIP))
   endif
   ifneq ($(HB_BUILD_OPTIM),)
      $(info ! HB_BUILD_OPTIM: $(HB_BUILD_OPTIM))
   endif
   ifneq ($(HB_BUILD_MODE),)
      $(info ! HB_BUILD_MODE: $(HB_BUILD_MODE))
   endif
   ifneq ($(HB_BUILD_3RDEXT),)
      $(info ! HB_BUILD_3RDEXT: $(HB_BUILD_3RDEXT))
   endif
   ifneq ($(HB_BUILD_PARTS),)
      $(info ! HB_BUILD_PARTS: $(HB_BUILD_PARTS))
   endif
   ifneq ($(HB_REBUILD_EXTERN),)
      $(info ! HB_REBUILD_EXTERN: $(HB_REBUILD_EXTERN))
   endif
   ifneq ($(HB_REBUILD_PARSER),)
      $(info ! HB_REBUILD_PARSER: $(HB_REBUILD_PARSER))
   endif
   ifneq ($(HB_INSTALL_IMPLIB),)
      $(info ! HB_INSTALL_IMPLIB: $(HB_INSTALL_IMPLIB))
   endif
   ifneq ($(__HB_MT),)
      $(info ! __HB_MT: $(__HB_MT))
   endif
endif

# Shell detection
ifneq ($(SHLVL),)
   HB_SHELL := sh
   SHELL := /bin/sh
else
   ifeq ($(patsubst /bin/%sh,sh,$(SHELL)),sh)
      HB_SHELL := sh
      SHELL := /bin/sh
   else
   ifneq ($(OS2_SHELL),)
      HB_SHELL := os2
      SHELL := $(COMSPEC)
   else
      ifneq ($(ComSpec),)
         COMSPEC := $(ComSpec)
      endif
      ifeq ($(COMSPEC),)
         ifeq ($(OS),Windows_NT)
            COMSPEC := cmd.exe
         else
            COMSPEC := command.com
         endif
      endif
      SHELL := $(COMSPEC)
      ifneq ($(findstring COMMAND,$(COMSPEC)),)
         HB_SHELL := dos
      else
      ifneq ($(findstring command,$(COMSPEC)),)
         HB_SHELL := dos
      else
         HB_SHELL := nt
         _VER := $(shell ver)
         ifeq ($(strip $(findstring 3.5,$(shell ver))$(findstring 4.0,$(shell ver))$(findstring 5.0,$(shell ver))),)
            HB_SHELL_XP := (xp)
         endif
      endif
      endif
   endif
   endif
endif

# NOTE: This can be need if we want to run some internal command which are
#       missing from GNU Make's internal auto-detection list. Like 'move' on
#       non-*nix shells. [vszakats]
CMDPREF :=
ifneq ($(HB_SHELL),sh)
   ifneq ($(COMSPEC),)
      CMDPREF := $(COMSPEC) /c
   endif
endif

# Directory separator default
ifeq ($(DIRSEP),)
   DIRSEP := /
   ifneq ($(HB_SHELL),sh)
      DIRSEP := $(subst /,\,\)
   endif
endif
# Path separator default
ifeq ($(PTHSEP),)
   # small hack, it's hard to detect what is real path separator because
   # some shells in MS-DOS/Windows translates MS-DOS style paths to POSIX form
   ifeq ($(subst ;,:,$(PATH)),$(PATH))
      PTHSEP := :
   else
      PTHSEP := ;
   endif
endif

ifeq ($(HB_HOST_PLAT),)
   ifeq ($(windir)$(WINDIR),)
      # Using "quasi-functions" instead of $(eval) solution to stay compatible
      # with < 3.80 GNU Make versions
      _DETPLAT_STR := $(OSTYPE)
      include $(TOP)$(ROOT)config/detplat.mk
      ifeq ($(HB_HOST_PLAT),)
         _DETPLAT_STR := $(MACHTYPE)
         include $(TOP)$(ROOT)config/detplat.mk
         ifeq ($(HB_HOST_PLAT),)
            _DETPLAT_STR := $(OS)
            include $(TOP)$(ROOT)config/detplat.mk
            ifeq ($(HB_HOST_PLAT),)
               _DETPLAT_STR := $(shell uname -s)
               include $(TOP)$(ROOT)config/detplat.mk
            endif
         endif
      endif
      _DETPLAT_STR :=
   endif
endif

ifeq ($(HB_HOST_PLAT),)
   ifneq ($(OS2_SHELL),)
      HB_HOST_PLAT := os2
   else
   ifneq ($(windir)$(WINDIR),)
      HB_HOST_PLAT := win
      ifeq ($(OS),)
         HB_HOST_PLAT_WIN9X := yes
      endif
   else
   ifeq ($(HB_SHELL),dos)
      HB_HOST_PLAT := dos
   else
   ifneq ($(HB_PLATFORM),)
      HB_HOST_PLAT := $(HB_PLATFORM)
   endif
   endif
   endif
   endif
endif

ifneq ($(filter $(HB_HOST_PLAT),win),)
   ifeq ($(HB_BUILD_CONTRIB_DYN),)
      export HB_BUILD_CONTRIB_DYN := yes
   endif
endif

ifneq ($(filter $(HB_HOST_PLAT),win wce dos os2),)
   HB_HOST_BIN_EXT := .exe
else
   HB_HOST_BIN_EXT :=
endif

HB_HOST_CPU :=
ifeq ($(HB_HOST_PLAT),win)
   ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
      HB_HOST_CPU := x86_64
   else
   ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
      HB_HOST_CPU := x86_64
   else
   ifeq ($(PROCESSOR_ARCHITECTURE),IA64)
      HB_HOST_CPU := ia64
   else
      HB_HOST_CPU := x86
   endif
   endif
   endif
else
ifneq ($(filter $(HB_HOST_PLAT),dos os2),)
   HB_HOST_CPU := x86
else
   _UNAME_M := $(shell uname -m)
   ifneq ($(findstring ppc64,$(_UNAME_M)),)
      HB_HOST_CPU := ppc64
   else
   ifneq ($(findstring ppc,$(_UNAME_M)),)
      HB_HOST_CPU := ppc
   else
   ifneq ($(findstring Power,$(_UNAME_M)),)
      HB_HOST_CPU := ppc
   else
   ifneq ($(findstring arm,$(_UNAME_M)),)
      HB_HOST_CPU := arm
   else
   ifneq ($(findstring ia64,$(_UNAME_M)),)
      HB_HOST_CPU := ia64
   else
   ifneq ($(findstring sparc64,$(_UNAME_M)),)
      HB_HOST_CPU := sparc64
   else
   ifneq ($(findstring sparc,$(_UNAME_M)),)
      HB_HOST_CPU := sparc32
   else
   ifneq ($(findstring mips,$(_UNAME_M)),)
      HB_HOST_CPU := mips
   else
   ifneq ($(findstring alpha,$(_UNAME_M)),)
      HB_HOST_CPU := alpha
   else
   ifneq ($(findstring 9000,$(_UNAME_M)),)
      HB_HOST_CPU := parisc
   else
   ifneq ($(findstring parisc,$(_UNAME_M)),)
      HB_HOST_CPU := parisc
   else
   ifneq ($(findstring x86_64,$(_UNAME_M)),)
      HB_HOST_CPU := x86_64
   else
   ifneq ($(findstring 86,$(_UNAME_M)),)
      HB_HOST_CPU := x86
   else
   ifneq ($(findstring 64,$(_UNAME_M)),)
      HB_HOST_CPU := x86_64
   else
   ifneq ($(findstring BePC,$(_UNAME_M)),)
      HB_HOST_CPU := x86
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
   endif
endif
endif

ifeq ($(HB_INIT_DONE),)
   $(info ! HB_HOST_PLAT: $(HB_HOST_PLAT)$(if $(HB_HOST_CPU), ($(HB_HOST_CPU)),)  HB_SHELL: $(HB_SHELL))
endif

HB_PLAT_AUTO :=
ifeq ($(HB_PLATFORM),)
   HB_PLATFORM := $(HB_HOST_PLAT)
   ifneq ($(HB_COMPILER),)
      ifeq ($(HB_COMPILER),djgpp)
         HB_PLATFORM := dos
      else
      ifneq ($(filter $(HB_COMPILER),msvcarm msvcmips msvcsh mingwarm poccarm),)
         HB_PLATFORM := wce
      else
      ifneq ($(filter $(HB_COMPILER),mingw mingw64 msvc msvc64 msvcia64 bcc bcc64 xcc pocc pocc64),)
         HB_PLATFORM := win
      endif
      endif
      endif
   endif
   ifneq ($(findstring vxworks,$(WIND_PLATFORM)),)
      HB_PLATFORM := vxworks
      ifeq ($(HB_CPU),)
         HB_CPU := x86
      endif
   endif
   ifneq ($(HB_PLATFORM),)
      HB_PLAT_AUTO := (auto-detected)
   endif
endif

HB_COMPILER_ORI := $(HB_COMPILER)

# enable CC auto-detection in *nix cross builds
HB_CC_DET :=
ifneq ($(HB_HOST_PLAT),$(HB_PLATFORM))
   ifeq ($(filter $(HB_HOST_PLAT),win dos os2),)
      ifeq ($(HB_CCPATH)$(HB_CCPREFIX)$(HB_CCSUFFIX),)
         HB_CC_DET := yes
      endif
      ifeq ($(HB_COMPILER),)
         ifeq ($(HB_PLATFORM),win)
            HB_CC_DET := yes
            HB_COMPILER := mingw
         else
         ifeq ($(HB_PLATFORM),wce)
            HB_CC_DET := yes
            HB_COMPILER := mingwarm
         else
         ifeq ($(HB_PLATFORM),dos)
            HB_CC_DET := yes
            HB_COMPILER := djgpp
         endif
         endif
         endif
      endif
   endif
endif

HB_COMP_AUTO :=
HB_COMP_PATH :=
HB_COMP_PATH_VER_DET :=
ifeq ($(HB_COMPILER),)
   ifneq ($(filter $(HB_PLATFORM),win wce),)
      HB_COMP_PATH := $(call find_in_path,arm-wince-mingw32ce-gcc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := mingwarm
         HB_PLATFORM := wce
         HB_CCPREFIX := arm-wince-mingw32ce-
         HB_CPU := arm
      else
         HB_COMP_PATH := $(call find_in_path,arm-mingw32ce-gcc)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := mingwarm
            HB_PLATFORM := wce
            HB_CCPREFIX := arm-mingw32ce-
            HB_CPU := arm
         else
            HB_COMP_PATH := $(call find_in_path,i386-mingw32ce-gcc)
            ifneq ($(HB_COMP_PATH),)
               HB_COMPILER := mingw
               HB_PLATFORM := wce
               HB_CCPREFIX := i386-mingw32ce-
            else
               HB_COMP_PATH := $(call find_in_path_raw,cygstart.exe)
               ifneq ($(HB_COMP_PATH),)
                  # Check for a gcc executable in the same directory
                  ifeq ($(wildcard $(dir $(HB_COMP_PATH))gcc$(HB_HOST_BIN_EXT)),)
                     HB_COMP_PATH :=
                  endif
               endif
               ifneq ($(HB_COMP_PATH),)
                  HB_COMPILER := gcc
                  HB_PLATFORM := cygwin
               else
                  HB_COMP_PATH := $(call find_in_path,djasm)
                  ifneq ($(HB_COMP_PATH),)
                     HB_PLATFORM := dos
                     HB_COMPILER := djgpp
                     ifneq ($(HB_HOST_CPU),x86)
                        $(error ! Error: DJGPP cross-builds are only possible on 32-bit Windows hosts)
                     endif
                  else
                     # tdragon DWARF-2 build (4.4.1)
                     HB_COMP_PATH := $(call find_in_path,gcc-dw2)
                     ifneq ($(HB_COMP_PATH),)
                        HB_CCSUFFIX := -dw2
                        ifeq ($(HB_CPU),x86_64)
                           HB_COMPILER := mingw64
                        else
                           HB_COMPILER := mingw
                        endif
                     else
                        # Equation Solution build (requires x86_64 host)
                        HB_COMP_PATH := $(call find_in_path,x86_64-pc-mingw32-gcc)
                        ifneq ($(HB_COMP_PATH),)
                           ifeq ($(HB_CPU),x86)
                              HB_COMPILER := mingw
                           else
                              HB_COMPILER := mingw64
                              HB_CPU := x86_64
                           endif
                        else
                           HB_COMP_PATH := $(call find_in_path,gcc)
                           ifneq ($(HB_COMP_PATH),)
                              # Check for a mingw64-tdm gcc executable in the same directory
                              ifneq ($(wildcard $(dir $(HB_COMP_PATH))x86_64-w64-mingw32-gcc$(HB_HOST_BIN_EXT)),)
                                 ifeq ($(HB_CPU),x86)
                                    HB_COMPILER := mingw
                                 else
                                    HB_COMPILER := mingw64
                                    HB_CPU := x86_64
                                 endif
                              else
                                 ifeq ($(HB_CPU),x86_64)
                                    HB_COMPILER := mingw64
                                 else
                                    HB_COMPILER := mingw
                                 endif
                                 HB_COMP_PATH_VER_DET := $(dir $(HB_COMP_PATH))gcc
                              endif
                           else
                              HB_COMP_PATH := $(call find_in_path,wcc386)
                              ifneq ($(HB_COMP_PATH),)
                                 HB_COMPILER := watcom
                              else
                                 HB_COMP_PATH := $(call find_in_path_raw,clarm.exe)
                                 ifneq ($(HB_COMP_PATH),)
                                    HB_COMPILER := msvcarm
                                    HB_PLATFORM := wce
                                    HB_CPU := arm
                                 else
                                    HB_COMP_PATH := $(call find_in_path_raw,armasm.exe)
                                    ifneq ($(HB_COMP_PATH),)
                                       HB_COMPILER := msvcarm
                                       HB_PLATFORM := wce
                                       HB_CPU := arm
                                       HB_COMP_PATH_VER_DET := $(subst armasm.exe,cl.exe,$(HB_COMP_PATH))
                                    else
                                       HB_COMP_PATH := $(call find_in_path_raw,idis.exe)
                                       ifneq ($(HB_COMP_PATH),)
                                          HB_COMPILER := iccia64
                                          HB_CPU := ia64
                                       else
                                          HB_COMP_PATH := $(call find_in_path_raw,clang-cl.exe)
                                          ifneq ($(HB_COMP_PATH),)
                                             HB_COMPILER := clang
                                          else
                                             HB_COMP_PATH := $(call find_in_path_raw,icl.exe)
                                             ifneq ($(HB_COMP_PATH),)
                                                HB_COMPILER := icc
                                             else
                                                HB_COMP_PATH := $(call find_in_path_raw,ml64.exe)
                                                ifneq ($(HB_COMP_PATH),)
                                                   HB_COMPILER := msvc64
                                                   HB_CPU := x86_64
                                                   HB_COMP_PATH_VER_DET := $(subst ml64.exe,cl.exe,$(HB_COMP_PATH))
                                                else
                                                   HB_COMP_PATH := $(call find_in_path_raw,ias.exe)
                                                   ifneq ($(HB_COMP_PATH),)
                                                      HB_COMPILER := msvcia64
                                                      HB_CPU := ia64
                                                   else
                                                      HB_COMP_PATH := $(call find_in_path_raw,cl.exe)
                                                      ifneq ($(HB_COMP_PATH),)
                                                         HB_COMPILER := msvc
                                                      else
                                                         # TODO: Add bcc64 auto-detection
                                                         HB_COMP_PATH := $(call find_in_path_raw,bcc32.exe)
                                                         ifneq ($(HB_COMP_PATH),)
                                                            HB_COMPILER := bcc
                                                         else
                                                            HB_COMP_PATH := $(call find_in_path_raw,pocc.exe)
                                                            ifneq ($(HB_COMP_PATH),)
                                                               ifneq ($(call find_in_path_prw,coredll.lib,$(LIB)),)
                                                                  HB_PLATFORM := wce
                                                                  HB_COMPILER := poccarm
                                                                  HB_CPU := arm
                                                               else
                                                                  ifneq ($(call find_in_path_prw,dbgeng.lib,$(LIB)),)
                                                                     HB_COMPILER := pocc64
                                                                     HB_CPU := x86_64
                                                                  else
                                                                     HB_COMPILER := pocc
                                                                  endif
                                                               endif
                                                            else
                                                               HB_COMP_PATH := $(call find_in_path_raw,xCC.exe)
                                                               ifneq ($(HB_COMP_PATH),)
                                                                  HB_COMPILER := xcc
                                                               else
                                                                  HB_COMP_PATH := $(call find_in_path_raw,dmc.exe)
                                                                  ifneq ($(HB_COMP_PATH),)
                                                                     HB_COMPILER := dmc
                                                                  else
                                                                     # mingw-w64 build (32-bit hosted)
                                                                     HB_COMP_PATH := $(call find_in_path,i686-w64-mingw32-gcc)
                                                                     ifneq ($(HB_COMP_PATH),)
                                                                        HB_CCPREFIX := i686-w64-mingw32-
                                                                        ifeq ($(HB_CPU),x86_64)
                                                                           HB_COMPILER := mingw64
                                                                        else
                                                                           HB_COMPILER := mingw
                                                                        endif
                                                                     else
                                                                        ifeq ($(HB_HOST_CPU),x86_64)
                                                                           # mingw-w64 build
                                                                           HB_COMP_PATH := $(call find_in_path,x86_64-w64-mingw32-gcc)
                                                                           ifneq ($(HB_COMP_PATH),)
                                                                              HB_CCPREFIX := x86_64-w64-mingw32-
                                                                              ifeq ($(HB_CPU),x86)
                                                                                 HB_COMPILER := mingw
                                                                              else
                                                                                 HB_COMPILER := mingw64
                                                                                 HB_CPU := x86_64
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
   else
   ifeq ($(HB_PLATFORM),linux)
      HB_COMP_PATH := $(call find_in_path,wcc386)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := watcom
      else
         HB_COMP_PATH := $(call find_in_path,gcc)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := gcc
         else
            HB_COMP_PATH := $(call find_in_path,suncc)
            ifneq ($(HB_COMP_PATH),)
               HB_COMPILER := sunpro
            else
               HB_COMP_PATH := $(call find_in_path,icc)
               ifneq ($(HB_COMP_PATH),)
                  HB_COMPILER := icc
               endif
            endif
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),bsd)
      HB_COMP_PATH := $(call find_in_path_par,clang,/usr/bin)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := clang
      else
         HB_COMP_PATH := $(call find_in_path_par,gcc,/usr/bin)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := gcc
         endif
      endif
   else
   ifneq ($(filter $(HB_PLATFORM),aix hpux beos qnx cygwin),)
      HB_COMP_PATH := $(call find_in_path,gcc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := gcc
      endif
   else
   ifeq ($(HB_PLATFORM),darwin)
      HB_COMP_PATH := $(call find_in_path,clang)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := clang
      else
         HB_COMP_PATH := $(call find_in_path_par,clang,/Developer/usr/bin/)
         ifneq ($(HB_COMP_PATH),)
            HB_CCPREFIX := /Developer/usr/bin/
            HB_COMPILER := clang
         else
            HB_COMP_PATH := $(call find_in_path,gcc)
            ifneq ($(HB_COMP_PATH),)
               HB_COMPILER := gcc
            else
               HB_COMP_PATH := $(call find_in_path,icc)
               ifneq ($(HB_COMP_PATH),)
                  HB_COMPILER := icc
               endif
            endif
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),sunos)
      HB_COMP_PATH := $(call find_in_path,suncc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := sunpro
      else
         HB_COMP_PATH := $(call find_in_path,gcc)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := gcc
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),dos)
      HB_COMP_PATH := $(call find_in_path,gcc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := djgpp
      else
         HB_COMP_PATH := $(call find_in_path,wcc386)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := watcom
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),os2)
      HB_COMP_PATH := $(call find_in_path,gcc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := gcc
      else
         HB_COMP_PATH := $(call find_in_path,wcc386)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := watcom
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),minix)
      HB_COMP_PATH := $(call find_in_path,clang)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := clang
      else
         HB_COMP_PATH := $(call find_in_path,gcc)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := gcc
         endif
      endif
   else
   ifeq ($(HB_PLATFORM),qnx)
      HB_COMP_PATH := $(call find_in_path,ntox86-gcc)
      ifneq ($(HB_COMP_PATH),)
         HB_COMPILER := gcc
         HB_CCPREFIX := ntox86-
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

   # auto-detect watcom platform by looking at the header path config
   ifeq ($(HB_COMPILER),watcom)
      ifneq ($(call find_in_path_prw,os2.h,$(INCLUDE)),)
         HB_PLATFORM := os2
      else
      ifneq ($(call find_in_path_prw,dirent.h,$(INCLUDE)),)
         HB_PLATFORM := linux
      else
      ifeq ($(call find_in_path_prw,windows.h,$(INCLUDE)),)
         HB_PLATFORM := dos
      endif
      endif
      endif
   endif
endif

ifeq ($(HB_COMPILER_VER),)

   ifeq ($(HB_COMP_PATH_VER_DET),)
      HB_COMP_PATH_VER_DET := $(HB_COMP_PATH)
   endif

   ifneq ($(filter $(HB_COMPILER),clang clang64),)
      ifeq ($(HB_COMP_PATH_VER_DET),)
         HB_COMP_PATH_VER_DET := $(HB_CCPREFIX)clang$(HB_CCSUFFIX)
      endif
      _C_VER := $(shell "$(HB_COMP_PATH_VER_DET)" -v 2>&1)
      ifneq ($(findstring 3.9,$(_C_VER)),)
         HB_COMPILER_VER := 0309
      else
      ifneq ($(findstring 8.0,$(_C_VER)),)
         HB_COMPILER_VER := 0309
      else
      ifneq ($(findstring 7.3,$(_C_VER)),)
         HB_COMPILER_VER := 0308
      else
      ifneq ($(findstring 3.8,$(_C_VER)),)
         HB_COMPILER_VER := 0308
      else
      ifneq ($(findstring 7.0,$(_C_VER)),)
         HB_COMPILER_VER := 0307
      else
      ifneq ($(findstring 3.7,$(_C_VER)),)
         HB_COMPILER_VER := 0307
      else
      ifneq ($(findstring 3.6,$(_C_VER)),)
         HB_COMPILER_VER := 0306
      else
      ifneq ($(findstring 3.5,$(_C_VER)),)
         HB_COMPILER_VER := 0305
      else
         HB_COMPILER_VER := 0304
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      endif
   else
   ifneq ($(filter $(HB_COMPILER),gcc gccarm gccomf mingw mingw64 mingwarm djgpp),)
      ifeq ($(HB_COMP_PATH_VER_DET),)
         HB_COMP_PATH_VER_DET := $(HB_CCPREFIX)gcc$(HB_CCSUFFIX)
      endif
      _C_VER := $(shell "$(HB_COMP_PATH_VER_DET)" -v 2>&1)
      ifneq ($(findstring version 6.1.,$(_C_VER)),)
         HB_COMPILER_VER := 0601
      else
      ifneq ($(findstring version 5.4.,$(_C_VER)),)
         HB_COMPILER_VER := 0504
      else
      ifneq ($(findstring version 5.3.,$(_C_VER)),)
         HB_COMPILER_VER := 0503
      else
      ifneq ($(findstring version 5.2.,$(_C_VER)),)
         HB_COMPILER_VER := 0502
      else
      ifneq ($(findstring version 5.1.,$(_C_VER)),)
         HB_COMPILER_VER := 0501
      else
      ifneq ($(findstring version 4.9.,$(_C_VER)),)
         HB_COMPILER_VER := 0409
      else
      ifneq ($(findstring version 4.8.,$(_C_VER)),)
         HB_COMPILER_VER := 0408
      else
      ifneq ($(findstring version 4.7.,$(_C_VER)),)
         HB_COMPILER_VER := 0407
      else
      ifneq ($(findstring version 4.6.,$(_C_VER)),)
         HB_COMPILER_VER := 0406
      else
      ifneq ($(findstring version 4.5.,$(_C_VER)),)
         HB_COMPILER_VER := 0405
      else
      ifneq ($(findstring version 4.4.,$(_C_VER)),)
         HB_COMPILER_VER := 0404
      else
      ifneq ($(findstring version 4.3.,$(_C_VER)),)
         HB_COMPILER_VER := 0403
      else
         HB_COMPILER_VER := 0304
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
   else
   ifneq ($(filter $(HB_COMPILER),msvc msvc64 msvcia64 msvcarm),)
      _C_VER := $(shell "$(HB_COMP_PATH)" 2>&1)
      ifneq ($(findstring Version 20.,$(_C_VER)),)
         HB_COMPILER_VER := 2000
      else
      ifneq ($(findstring Version 19.,$(_C_VER)),)
         HB_COMPILER_VER := 1900
      else
      ifneq ($(findstring Version 18.,$(_C_VER)),)
         HB_COMPILER_VER := 1800
      else
      ifneq ($(findstring Version 17.,$(_C_VER)),)
         HB_COMPILER_VER := 1700
      else
      ifneq ($(findstring Version 16.,$(_C_VER)),)
         HB_COMPILER_VER := 1600
      else
      ifneq ($(findstring Version 15.,$(_C_VER)),)
         HB_COMPILER_VER := 1500
      else
      ifneq ($(findstring Version 14.,$(_C_VER)),)
         HB_COMPILER_VER := 1400
      else
      ifneq ($(findstring Version 13.,$(_C_VER)),)
         HB_COMPILER_VER := 1300
      else
         HB_COMPILER_VER := 1200
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      endif
   else
   ifneq ($(filter $(HB_COMPILER),pocc pocc64 poccarm),)
      _C_VER := $(shell "$(HB_COMP_PATH_VER_DET)" 2>&1)
      ifneq ($(findstring 8.00.,$(_C_VER)),)
         HB_COMPILER_VER := 0800
      else
      ifneq ($(findstring 7.00.,$(_C_VER)),)
         HB_COMPILER_VER := 0700
      else
      ifneq ($(findstring 6.50.,$(_C_VER)),)
         HB_COMPILER_VER := 0650
      else
      ifneq ($(findstring 6.00.,$(_C_VER)),)
         HB_COMPILER_VER := 0600
      else
      ifneq ($(findstring 5.00.,$(_C_VER)),)
         HB_COMPILER_VER := 0500
      else
         HB_COMPILER_VER := 0450
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

# auto-detect CC values for given platform/compiler
ifneq ($(HB_CC_DET),)
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-mingw)

      HB_COMPILER :=

      ifeq ($(wildcard $(HB_CCPATH)$(HB_CCPREFIX)gcc$(HB_HOST_BIN_EXT)),)
         ifeq ($(HB_CCPATH),)
            ifeq ($(call find_in_path $(HB_CCPREFIX)gcc),)
               HB_CCPREFIX :=
            endif
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      # try to detect MinGW cross-compiler location using some default platform settings
      ifeq ($(HB_CCPATH)$(HB_CCPREFIX),)
         ifneq ($(wildcard /etc/debian_version),)
            HB_CCPREFIX := i586-mingw32msvc-
         else
            ifneq ($(wildcard /etc/gentoo-release),)
               ifneq ($(call find_in_path_par,i386-mingw32msvc-gcc,/opt/xmingw/bin),)
                  HB_CCPATH := /opt/xmingw/
                  HB_CCPREFIX := i386-mingw32msvc-
               else
                  HB_CCPREFIX := i686-mingw32-
               endif
            else
               ifeq ($(HB_PLATFORM),bsd)
                  HB_CCPATH := /usr/local/mingw32/
               else
                  MINGW_OK := $(strip $(foreach d, i386-mingw i486-mingw i586-mingw i686-mingw i386-mingw32 i486-mingw32 i586-mingw32 i686-mingw32, $(if $(wildcard /usr/local/bin/$(d)-gcc),$(d),)))
                  ifneq ($(MINGW_OK),)
                     HB_CCPATH := /usr/local/bin/
                     HB_CCPREFIX := $(MINGW_OK)-
                  endif
               endif
            endif
         endif
      endif

      ifeq ($(wildcard $(HB_CCPATH)$(HB_CCPREFIX)gcc$(HB_HOST_BIN_EXT)),)
         ifeq ($(HB_CCPATH),)
            ifeq ($(call find_in_path $(HB_CCPREFIX)gcc),)
               HB_CCPREFIX :=
            endif
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      # generic detection for mingw cross-compiler
      ifeq ($(HB_CCPATH)$(HB_CCPREFIX),)
         MINGW_BASE_LIST := /usr /usr/local /usr/local/mingw32 /opt/xmingw /opt/cross
         MINGW_PREFIX := $(firstword $(foreach d, $(MINGW_BASE_LIST), $(wildcard $(d)/bin/i?86*-mingw*-gcc$(HB_HOST_BIN_EXT))))
         ifneq ($(MINGW_PREFIX),)
            MINGW_PREFIX := $(MINGW_PREFIX:gcc$(HB_HOST_BIN_EXT)=)
            HB_CCPATH := $(dir $(MINGW_PREFIX))
            HB_CCPREFIX := $(notdir $(MINGW_PREFIX))
         else
            MINGW_PREFIX := $(firstword $(foreach d, $(MINGW_BASE_LIST), $(wildcard $(d)/i?86-mingw*/bin/gcc$(HB_HOST_BIN_EXT))))
            ifneq ($(MINGW_PREFIX),)
               HB_CCPATH := $(dir $(MINGW_PREFIX))
               HB_CCPREFIX :=
            endif
         endif
      endif

      ifneq ($(HB_CCPATH)$(HB_CCPREFIX),)
         HB_COMP_PATH := $(dir $(HB_CCPATH))
         HB_COMPILER := mingw
         export HB_BUILD_3RDEXT := no
         ifneq ($(HB_BUILD_PARTS),all)
            HB_BUILD_PARTS := lib
         endif
      else
         $(error ! Harbour build could not find mingw32 cross-compiler. Please install it, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
      endif

   else
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-mingw64)

      HB_COMPILER :=

      # generic detection for mingw cross-compiler
      ifeq ($(HB_CCPATH)$(HB_CCPREFIX),)
         MINGW_BASE_LIST := /usr /usr/local /usr/local/mingw32 /opt/xmingw /opt/cross
         MINGW_PREFIX := $(firstword $(foreach d, $(MINGW_BASE_LIST), $(wildcard $(d)/bin/amd64-mingw*-gcc$(HB_HOST_BIN_EXT))))
         ifneq ($(MINGW_PREFIX),)
            MINGW_PREFIX := $(MINGW_PREFIX:gcc$(HB_HOST_BIN_EXT)=)
            HB_CCPATH := $(dir $(MINGW_PREFIX))
            HB_CCPREFIX := $(notdir $(MINGW_PREFIX))
         else
            MINGW_PREFIX := $(firstword $(foreach d, $(MINGW_BASE_LIST), $(wildcard $(d)/amd64-mingw*/bin/gcc$(HB_HOST_BIN_EXT))))
            ifneq ($(MINGW_PREFIX),)
               HB_CCPATH := $(dir $(MINGW_PREFIX))
               HB_CCPREFIX :=
            endif
         endif
      endif

      ifneq ($(HB_CCPATH)$(HB_CCPREFIX),)
         HB_COMP_PATH := $(dir $(HB_CCPATH))
         HB_COMPILER := mingw64
         export HB_BUILD_3RDEXT := no
         ifneq ($(HB_BUILD_PARTS),all)
            HB_BUILD_PARTS := lib
         endif
      else
         $(error ! Harbour build could not find mingw-w64 cross-compiler. Please install it, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
      endif

   else
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),wce-mingwarm)

      HB_COMPILER :=

      # Look for known mingw32ce compilers on HB_CCPATH if it's set
      ifneq ($(HB_CCPATH),)
         ifneq ($(call find_in_path_par,arm-mingw32ce-gcc,$(HB_CCPATH)),)
            HB_COMPILER := mingwarm
            HB_CCPREFIX := arm-mingw32ce-
            HB_CCPATH := $(HB_CCPATH)/
         else
            ifneq ($(call find_in_path_par,arm-wince-mingw32ce-gcc,$(HB_CCPATH)),)
               HB_COMPILER := mingwarm
               HB_CCPREFIX := arm-wince-mingw32ce-
               HB_CCPATH := $(HB_CCPATH)/
            else
               ifneq ($(call find_in_path_par,i386-mingw32ce-gcc,$(HB_CCPATH)),)
                  HB_COMPILER := mingw
                  HB_CCPREFIX := i386-mingw32ce-
                  HB_CCPATH := $(HB_CCPATH)/
               else
                  HB_CCPATH :=
                  HB_CCPREFIX :=
               endif
            endif
         endif
      endif

      # If HB_CCPATH not set, or could not be found on the provided PATH,
      # try to detect them in default locations
      ifeq ($(HB_CCPATH),)
         HB_CCPATH := /opt/mingw32ce/bin/
         ifneq ($(call find_in_path_par,arm-mingw32ce-gcc,$(HB_CCPATH)),)
            HB_COMPILER := mingwarm
            HB_CCPREFIX := arm-mingw32ce-
         else
            ifneq ($(call find_in_path_par,arm-wince-mingw32ce-gcc,$(HB_CCPATH)),)
               HB_COMPILER := mingwarm
               HB_CCPREFIX := arm-wince-mingw32ce-
            else
               HB_CCPATH := /opt/x86mingw32ce/bin/
               ifneq ($(call find_in_path_par,i386-mingw32ce-gcc,$(HB_CCPATH)),)
                  HB_COMPILER := mingw
                  HB_CCPREFIX := i386-mingw32ce-
               else
                  HB_CCPATH :=
                  HB_CCPREFIX :=
               endif
            endif
         endif
      endif

      ifneq ($(HB_CCPATH)$(HB_CCPREFIX),)
         HB_COMP_PATH := $(dir $(HB_CCPATH))
         export HB_BUILD_3RDEXT := no
         ifneq ($(HB_BUILD_PARTS),all)
            HB_BUILD_PARTS := lib
         endif
      else
         $(error ! Harbour build could not find cegcc cross-compiler. Please install it to /opt/mingw32ce, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
      endif

   else
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),wce-mingw)

      HB_COMPILER :=

      # Look for known mingw32ce compilers on HB_CCPATH if it's set
      ifneq ($(HB_CCPATH),)
         ifneq ($(call find_in_path_par,i386-mingw32ce-gcc,$(HB_CCPATH)),)
            HB_COMPILER := mingw
            HB_CCPREFIX := i386-mingw32ce-
            HB_CCPATH := $(HB_CCPATH)/
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      # If HB_CCPATH not set, or could not be found on the provided PATH,
      # try to detect them in default locations
      ifeq ($(HB_CCPATH),)
         HB_CCPATH := /opt/x86mingw32ce/bin/
         ifneq ($(call find_in_path_par,i386-mingw32ce-gcc,$(HB_CCPATH)),)
            HB_COMPILER := mingw
            HB_CCPREFIX := i386-mingw32ce-
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      ifneq ($(HB_CCPATH)$(HB_CCPREFIX),)
         HB_COMP_PATH := $(dir $(HB_CCPATH))
         export HB_BUILD_3RDEXT := no
         ifneq ($(HB_BUILD_PARTS),all)
            HB_BUILD_PARTS := lib
         endif
      else
         $(error ! Harbour build could not find cegcc (i386) cross-compiler. Please install it to /opt/mingw32ce, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
      endif

   else
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),dos-djgpp)

      # Look for djgpp compiler on HB_CCPATH if it's set
      ifneq ($(HB_CCPATH),)
         ifneq ($(call find_in_path_par,i586-pc-msdosdjgpp-gcc,$(HB_CCPATH)),)
            HB_COMPILER := djgpp
            HB_CCPREFIX := i586-pc-msdosdjgpp-
            HB_CCPATH := $(HB_CCPATH)/
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      # If HB_CCPATH not set, or could not be found on the provided PATH,
      # try to detect them in default locations
      ifeq ($(HB_CCPATH),)
         HB_CCPATH := /usr/local/i586-pc-msdosdjgpp
         ifneq ($(call find_in_path_par,i586-pc-msdosdjgpp-gcc,$(HB_CCPATH)),)
            HB_COMPILER := djgpp
            HB_CCPREFIX := i586-pc-msdosdjgpp-
         else
            HB_CCPATH :=
            HB_CCPREFIX :=
         endif
      endif

      ifneq ($(HB_CCPATH)$(HB_CCPREFIX),)
         HB_COMP_PATH := $(dir $(HB_CCPATH))
         HB_PLATFORM := dos
         export HB_BUILD_3RDEXT := no
         ifneq ($(HB_BUILD_PARTS),all)
            HB_BUILD_PARTS := lib
         endif
      else
         $(error ! Harbour build could not find djgpp cross-compiler. Please install it to /usr/local/i586-pc-msdosdjgpp, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
      endif
   endif
   endif
   endif
   endif
   endif
endif

ifeq ($(HB_COMPILER_ORI),)
   ifneq ($(HB_COMPILER),)
      HB_COMP_PATH := $(subst $(substpat), ,$(dir $(firstword $(subst $(subst x, ,x),$(substpat),$(HB_COMP_PATH)))))
      HB_COMP_AUTO := (auto-detected$(if $(HB_COMP_PATH),: $(HB_COMP_PATH),)$(if $(HB_CCPREFIX), [$(HB_CCPREFIX)*],)$(if $(HB_CCSUFFIX), [*$(HB_CCSUFFIX)],))
   endif
endif
HB_COMP_VERD := $(if $(HB_COMPILER_VER), (v$(HB_COMPILER_VER)),)

export HB_CCPATH
export HB_CCPREFIX
export HB_CCSUFFIX

ifeq ($(HB_PLATFORM),)
   $(error ! HB_PLATFORM not set, could not auto-detect)
endif
ifeq ($(HB_COMPILER),)
   $(error ! HB_COMPILER not set, could not auto-detect)
endif

export HB_PLATFORM
export HB_COMPILER
export HB_COMPILER_VER
export HB_SHELL

ifneq ($(HB_COMP_PATH),)
   export HB_COMP_PATH_PUB := $(HB_COMP_PATH)
endif

# Always auto-detect bcc location (hack)
ifeq ($(HB_COMP_PATH_PUB),)
   ifeq ($(HB_PLATFORM)-$(HB_COMPILER),win-bcc)
      HB_COMP_PATH := $(call find_in_path_raw,bcc32.exe)
      ifneq ($(HB_COMP_PATH),)
         export HB_COMP_PATH_PUB := $(subst $(substpat), ,$(dir $(firstword $(subst $(subst x, ,x),$(substpat),$(HB_COMP_PATH)))))
      endif
   endif
endif

ifneq ($(filter $(HB_HOST_PLAT),win wce dos os2),)
   HB_HOST_PLAT_UNIX :=
else
   HB_HOST_PLAT_UNIX := yes
endif

export HB_HOST_PLAT_UNIX

ifneq ($(filter $(HB_PLATFORM),win wce dos os2),)
   HB_PLATFORM_UNIX :=
else
   HB_PLATFORM_UNIX := yes
endif

ifeq ($(HB_PLATFORM),beos)
   HB_LD_LIBRARY_PATH = LIBRARY_PATH
else
   HB_LD_LIBRARY_PATH = LD_LIBRARY_PATH
endif

PLAT_COMP := $(HB_PLATFORM)/$(HB_COMPILER)$(subst \,/,$(HB_BUILD_NAME))

OBJ_DIR := obj/$(PLAT_COMP)
BIN_DIR := $(TOP)$(ROOT)bin/$(PLAT_COMP)
LIB_DIR := $(TOP)$(ROOT)lib/$(PLAT_COMP)
ifeq ($(HB_PLATFORM_UNIX),)
   DYN_DIR := $(BIN_DIR)
   IMP_DIR := $(LIB_DIR)
else
   ifeq ($(HB_PLATFORM),cygwin)
      DYN_DIR := $(BIN_DIR)
      IMP_DIR := $(LIB_DIR)
   else
      DYN_DIR := $(LIB_DIR)
      IMP_DIR :=
   endif
   ifeq ($(HB_LD_PATH_SET),)
      ifneq ($(HB_SRC_ROOTPATH),)
         export $(HB_LD_LIBRARY_PATH) := $(HB_SRC_ROOTPATH)lib/$(PLAT_COMP):$($(HB_LD_LIBRARY_PATH))
      else
         export $(HB_LD_LIBRARY_PATH) := $(abspath $(DYN_DIR)):$($(HB_LD_LIBRARY_PATH))
      endif
      export HB_LD_PATH_SET := yes
      ifneq ($($(HB_LD_LIBRARY_PATH)),)
         $(info ! $(HB_LD_LIBRARY_PATH): $($(HB_LD_LIBRARY_PATH)))
      endif
   endif
endif
DYN_PREF :=
# define PKG_DIR only if run from root Makefile
ifeq ($(ROOT),./)
   ifneq ($(HB_SRC_ROOTPATH),)
      PKG_DIR := $(HB_SRC_ROOTPATH)
   else
      PKG_DIR := $(TOP)$(ROOT)
   endif
   PKG_DIR := $(PKG_DIR)pkg/$(PLAT_COMP)
else
   PKG_DIR :=
endif

# Assemble relative path from OBJ_DIR to source.
GRANDP := $(subst $(subst x,x, ),,$(foreach item, $(subst /, ,$(OBJ_DIR)), ../))

# TODO: Set this in <plat>/<comp>.mk (compiler switches may influence it)
ifeq ($(HB_CPU),)
   ifeq ($(HB_PLATFORM),win)
      ifneq ($(filter $(HB_COMPILER),msvc64 mingw64 pocc64),)
         HB_CPU := x86_64
      else
      ifneq ($(filter $(HB_COMPILER),msvcia64 iccia64),)
         HB_CPU := ia64
      else
         HB_CPU := x86
      endif
      endif
   else
   ifeq ($(HB_PLATFORM),wce)
      ifneq ($(filter $(HB_COMPILER),msvcarm mingwarm poccarm),)
         HB_CPU := arm
      else
      ifneq ($(filter $(HB_COMPILER),msvcmips),)
         HB_CPU := mips
      else
      ifneq ($(filter $(HB_COMPILER),msvcsh),)
         HB_CPU := sh
      else
         HB_CPU := x86
      endif
      endif
      endif
   else
   ifneq ($(filter $(HB_PLATFORM),dos os2),)
      HB_CPU := x86
   else
   ifeq ($(HB_PLATFORM),linux)
      HB_CPU := $(HB_HOST_CPU)
      ifneq ($(filter $(HB_USER_CFLAGS),-m64),)
         HB_CPU := x86_64
      else
      ifneq ($(filter $(HB_USER_CFLAGS),-m32),)
         HB_CPU := x86
      endif
      endif
   else
      HB_CPU := $(HB_HOST_CPU)
   endif
   endif
   endif
   endif
endif

ifeq ($(HB_INIT_DONE),)
   ifeq ($(HB_COMPILER),djgpp)
      # NOTE: We do need DJGPP build of GNU Make on Windows
      #       systems. The reason is that this uses special
      #       trick to pass command-lines to other DJGPP tools
      #       (like gcc) to overcome 126 chars MS-DOS command
      #       line length limitation. IOW: mingw32-make.exe
      #       wo not work with DJGPP on Windows hosts.
      #       [vszakats]
      ifeq ($(HB_HOST_PLAT),win)
         ifneq ($(HB_MAKE_PLAT),dos)
            $(warning ! Warning: You should use DJGPP provided MS-DOS GNU Make on Windows hosts)
         endif
      endif
   else
      ifeq ($(HB_HOST_PLAT)-$(HB_MAKE_PLAT),win-dos)
         $(warning ! Warning: You are using MS-DOS GNU Make executable on Windows host.)
         $(warning !          Not recommended combination. Some features will be disabled.)
         $(warning !          Please use the Windows build of GNU Make.)
      endif
   endif
   $(info ! HB_PLATFORM: $(HB_PLATFORM)$(if $(HB_CPU), ($(HB_CPU)),) $(HB_PLAT_AUTO))
   $(info ! HB_COMPILER: $(HB_COMPILER)$(HB_COMP_VERD) $(HB_COMP_AUTO))
endif

export HB_CPU

ifeq ($(HB_HOST_PKGM),)
   ifeq ($(HB_PLATFORM),darwin)
      ifneq ($(wildcard /usr/local/bin/brew),)
         HB_HOST_PKGM += homebrew
      endif
      ifneq ($(wildcard /opt/local/bin/port),)
         HB_HOST_PKGM += macports
      endif
      ifneq ($(wildcard /nix),)
         HB_HOST_PKGM += nix
      endif
      ifneq ($(wildcard /sw/bin/fink),)
         HB_HOST_PKGM += fink
      endif
   else
   ifeq ($(HB_PLATFORM),linux)
      ifneq ($(wildcard /etc/debian_version),)
         HB_HOST_PKGM += deb
      else
      ifneq ($(wildcard /etc/pacman.conf),)
         HB_HOST_PKGM += pacman
      else
      ifneq ($(wildcard /etc/gentoo-release),)
         HB_HOST_PKGM += portage
      else
         HB_HOST_PKGM += rpm
      endif
      endif
      endif
      ifneq ($(wildcard /nix),)
         HB_HOST_PKGM += nix
      endif
   else
   ifeq ($(HB_PLATFORM),bsd)
      ifneq ($(wildcard /etc/pkg),)
         HB_HOST_PKGM += pkg
      else
         HB_HOST_PKGM += ports
      endif
   else
   ifeq ($(HB_PLATFORM),sunos)
      HB_HOST_PKGM += pkg
   else
   ifeq ($(HB_PLATFORM),win)
      ifneq ($(wildcard /etc/pacman.conf),)
         HB_HOST_PKGM += pacman
      endif
   else
   ifeq ($(HB_PLATFORM),cygwin)
      HB_HOST_PKGM += cygwin
   endif
   endif
   endif
   endif
   endif
   endif
endif
export HB_HOST_PKGM

ifeq ($(HB_INIT_DONE),)
   ifneq ($(HB_HOST_PKGM),)
      $(info ! HB_HOST_PKGM: $(HB_HOST_PKGM))
   endif
endif

# Reserve variables for local compiler flags. Makefiles
# should only modify these instead of HB_USER_* variables
# as these can have bad side effects (doubly added values)
# caused by recursive GNU Make runs.
# Notice that even single lib/bin builds will currently
# result in recursive runs, see rule 'descend'. [vszakats]
HB_CFLAGS :=
HB_LDFLAGS :=
HB_AFLAGS :=
HB_PRGFLAGS :=

# C flags passed when compiling to create static lib
HB_CFLAGS_STA :=
# C flags passed when compiling to create dynamic lib
HB_CFLAGS_DYN :=

CFLAGS :=
RCFLAGS :=
ARFLAGS :=
LDFLAGS :=
DFLAGS :=

HB_CROSS_BUILD :=
ifneq ($(HB_HOST_PLAT)$(HB_HOST_CPU),$(HB_PLATFORM)$(HB_CPU))
   ifeq ($(HB_HOST_BIN),)
      # Not required in these combinations: [vszakats]
      # 'Same platform, x86_64 host, x86 target'
      ifneq ($(HB_HOST_PLAT)-$(HB_HOST_CPU)-$(HB_CPU),$(HB_PLATFORM)-x86_64-x86)
         # 'Windows x86 host, MS-DOS target'
         ifneq ($(HB_HOST_PLAT)-$(HB_HOST_CPU)-$(HB_PLATFORM)-$(HB_CPU),win-x86-dos-x86)
            # 'Windows host, Cygwin target'
            ifneq ($(HB_HOST_PLAT)-$(HB_PLATFORM),win-cygwin)
               HB_CROSS_BUILD := yes
               # Try to autosetup
               ifneq ($(HB_SRC_ROOTPATH),)
                  _HB_ROOT_BIN := $(HB_SRC_ROOTPATH)
               else
                  _HB_ROOT_BIN := $(TOP)$(ROOT)
               endif
               HB_HOST_BIN := $(dir $(firstword $(wildcard $(_HB_ROOT_BIN)bin/$(HB_HOST_PLAT)/*/harbour$(HB_HOST_BIN_EXT))))
               ifneq ($(HB_HOST_BIN),)
                  ifeq ($(HB_SRC_ROOTPATH),)
                     HB_HOST_BIN := $(realpath $(HB_HOST_BIN))
                  endif
               else
                  # Look in PATH
                  HB_HOST_BIN := $(dir $(call find_in_path,harbour))
               endif
               ifeq ($(HB_HOST_BIN),)
                  $(warning ! Warning: HB_HOST_BIN not specified. Could not find host native build.)
               else
                  $(info ! HB_HOST_BIN not specified. Automatically set to: $(HB_HOST_BIN))
               endif
            endif
         endif
      endif
      export HB_HOST_BIN
   else
      ifeq ($(HB_INIT_DONE),)
         $(info ! HB_HOST_BIN: $(HB_HOST_BIN))
      endif
      HB_CROSS_BUILD := yes
   endif
   ifeq ($(HB_CROSS_BUILD),yes)
      # Setup platform macros (undefine host, define target)
      HB_PRGFLAGS += -undef:.ARCH.
      ifeq ($(HB_PLATFORM),win)
         HB_PRGFLAGS += -D__PLATFORM__WINDOWS
      else
      ifeq ($(HB_PLATFORM),wce)
         HB_PRGFLAGS += -D__PLATFORM__WINDOWS -D__PLATFORM__WINCE
      else
      ifeq ($(HB_PLATFORM),dos)
         HB_PRGFLAGS += -D__PLATFORM__DOS
      else
      ifeq ($(HB_PLATFORM),os2)
         HB_PRGFLAGS += -D__PLATFORM__OS2
      else
      ifeq ($(HB_PLATFORM),linux)
         HB_PRGFLAGS += -D__PLATFORM__LINUX -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),darwin)
         HB_PRGFLAGS += -D__PLATFORM__DARWIN -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),bsd)
         HB_PRGFLAGS += -D__PLATFORM__BSD -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),sunos)
         HB_PRGFLAGS += -D__PLATFORM__SUNOS -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),hpux)
         HB_PRGFLAGS += -D__PLATFORM__HPUX -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),beos)
         HB_PRGFLAGS += -D__PLATFORM__BEOS -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),qnx)
         HB_PRGFLAGS += -D__PLATFORM__QNX -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),android)
         HB_PRGFLAGS += -D__PLATFORM__ANDROID -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),vxworks)
         HB_PRGFLAGS += -D__PLATFORM__VXWORKS -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),symbian)
         HB_PRGFLAGS += -D__PLATFORM__SYMBIAN -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),cygwin)
         HB_PRGFLAGS += -D__PLATFORM__CYGWIN -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),minix)
         HB_PRGFLAGS += -D__PLATFORM__MINIX -D__PLATFORM__UNIX
      else
      ifeq ($(HB_PLATFORM),aix)
         HB_PRGFLAGS += -D__PLATFORM__AIX -D__PLATFORM__UNIX
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
      endif
      endif
      endif
      ifneq ($(HB_HOST_CPU),$(HB_CPU))
         ifneq ($(filter $(HB_CPU),x86 arm),)
            HB_PRGFLAGS += -D__ARCH32BIT__
         else
         ifneq ($(filter $(HB_CPU),x86_64 ia64),)
            HB_PRGFLAGS += -D__ARCH64BIT__
         endif
         endif
      endif
   endif
endif

include $(TOP)$(ROOT)config/detect.mk

SYSLIBS :=

# Names of portable GT drivers
HB_GT_LIBS := \
   gtcgi \
   gtpca \
   gtstd \

# Add GTs if dependency is available
ifneq ($(HB_HAS_CURSES),)
   HB_GT_LIBS += gtcrs
endif
ifneq ($(HB_HAS_SLANG),)
   HB_GT_LIBS += gtsln
endif
ifneq ($(HB_HAS_X11),)
   HB_GT_LIBS += gtxwc
endif

ifneq ($(HB_PLATFORM),dos)
   HB_PKGNAME := harbour
   ifneq ($(HB_PLATFORM_UNIX),)
      HB_VERSION := $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)
      ifneq ($(HB_VER_STATUS),)
         HB_VERSION := $(HB_VERSION)-$(HB_VER_STATUS)
      endif
      HB_PKGNAME := $(HB_PKGNAME)-$(HB_VERSION)
   else
      HB_VERSION := $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)$(HB_VER_STATUS)
      HB_PKGNAME := $(HB_PKGNAME)-$(HB_VERSION)-$(HB_PLATFORM)-$(HB_COMPILER)
   endif
   HB_PKGNAMI := $(HB_PKGNAME)
else
   # Use short names in MS-DOS
   HB_VERSION := $(HB_VER_MAJOR)$(HB_VER_STATUS_SH)
   HB_PKGNAME := hb$(HB_VERSION)
   # Ugly solution
   ifeq ($(HB_COMPILER),djgpp)
      HB_PKGNAME := $(HB_PKGNAME)dj
   else
      ifeq ($(HB_COMPILER),watcom)
         HB_PKGNAME := $(HB_PKGNAME)wa
      endif
   endif
   HB_PKGNAMI := $(HB_PKGNAME)
endif

export HB_VERSION
export HB_PKGNAME
export HB_PKGNAMI

HB_INSTALL_PREFIX_ORI := $(HB_INSTALL_PREFIX)
ifeq ($(HB_BUILD_PKG),yes)
   ifeq ($(HB_INIT_DONE),)

      ifeq ($(HB_PLATFORM),darwin)
         HB_BUILD_PKG_PREFIX := /usr/local
         HB_SYSLOC := yes
      else
      ifeq ($(HB_PLATFORM),sunos)
         HB_BUILD_PKG_PREFIX := /opt
         HB_SYSLOC := yes
      else
      ifeq ($(HB_PLATFORM),beos)
         HB_BUILD_PKG_PREFIX := /boot/common
         HB_SYSLOC := yes
      else
      ifneq ($(HB_PLATFORM_UNIX),)
         HB_BUILD_PKG_PREFIX := /usr/local
         HB_SYSLOC := yes
      else
         HB_BUILD_PKG_PREFIX := /$(HB_PKGNAME)
      endif
      endif
      endif
      endif

      # HB_TOP              - dir where release packages will be
      #                       created (root of Harbour source tree)
      # HB_INSTALL_PKG_ROOT - dir which has to be packed
      # HB_PKGNAME          - name of the release package
      # HB_INSTALL_PREFIX   - dir where Harbour dirs will be created
      #
      #   <HB_TOP><plat/comp  ><HB_BUILD_PKG_PREFIX>
      #   <HB_INSTALL_PKG_ROOT>
      #   <HB_INSTALL_PREFIX                       >/bin
      #

      ifneq ($(HB_SRC_ROOTPATH),)
         export HB_TOP := $(subst /,$(DIRSEP),$(HB_SRC_ROOTPATH))
         HB_INSTALL_PREFIX := $(PKG_DIR)
      else
         export HB_TOP := $(subst /,$(DIRSEP),$(realpath $(TOP)$(ROOT)))
         HB_INSTALL_PREFIX := $(abspath $(PKG_DIR))
      endif

      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))

      export HB_INSTALL_PKG_ROOT := $(HB_INSTALL_PREFIX)

      HB_INSTALL_BIN :=
      HB_INSTALL_INC :=
      HB_INSTALL_LIB :=
      HB_INSTALL_DYN :=
      HB_INSTALL_DOC :=
      HB_INSTALL_MAN :=
      ifeq ($(HB_PLATFORM),darwin)
         export HB_INSTALL_ETC := $(HB_INSTALL_PREFIX)$(DIRSEP)private$(DIRSEP)etc$(DIRSEP)harbour
      else
         HB_INSTALL_ETC :=
      endif

      HB_INSTALL_PREFIX := $(HB_INSTALL_PREFIX)$(subst /,$(DIRSEP),$(HB_BUILD_PKG_PREFIX))
   endif
else
   # Fill it automatically if not specified
   ifeq ($(HB_INSTALL_PREFIX),)

      ifeq ($(HB_PLATFORM),beos)
         HB_INSTALL_PREFIX := /boot/common
      else
      ifeq ($(HB_PLATFORM_UNIX),)
         # Do nothing. There was logic which set the install dir to
         # the root of the source tree, but in practice it often caused
         # build breakage because of old contrib headers being installed
         # into central header dir and picked up instead of current ones
         # inside contrib directories. Anyways with recent Harbour there
         # no pressing need to use 'install', so I've removed this
         # defaulting logic. [vszakats]
      else
      ifneq ($(PREFIX),)
         HB_INSTALL_PREFIX := $(PREFIX)
      else
      ifneq ($(DESTDIR),)
         HB_INSTALL_PREFIX := $(DESTDIR)
      else
      ifneq ($(HB_HOST_PLAT_UNIX),)
         # Stick to *nix customs. I do not like it, it needs admin.
         ifeq ($(HB_HOST_PLAT),darwin)
            HB_INSTALL_PREFIX := /opt/harbour
         else
            HB_INSTALL_PREFIX := /usr/local
         endif
         # Add postfix for cross builds
         ifneq ($(HB_HOST_PLAT),$(HB_PLATFORM))
            HB_INSTALL_PREFIX := $(HB_INSTALL_PREFIX)/harbour-$(HB_PLATFORM)-$(HB_COMPILER)
         endif
      endif
      endif
      endif
      endif
      endif
   endif

   HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))
endif

export HB_INSTALL_PREFIX

ifeq ($(HB_INIT_DONE),)
   ifneq ($(HB_INSTALL_PREFIX_ORI),$(HB_INSTALL_PREFIX))
      $(info ! HB_INSTALL_PREFIX automatically set to: $(HB_INSTALL_PREFIX))
   endif
   ifeq ($(ROOT),./)
      ifneq ($(wildcard .git),)
         ifneq ($(call find_in_path,git),)
            ifneq ($(shell git diff --name-only --quiet),)
               $(info ! === WARNING: Locally modified source code ===)
            endif
            $(info ! Git revision: $(shell git rev-parse --short HEAD))
         endif
      endif
   endif
   ifneq ($(wildcard $(TOP)$(ROOT).git),)
      ifneq ($(call find_in_path,git),)
         _cmd := git show --summary --format="%h%n%ci%n%an%n%ae" HEAD
         ifneq ($(HB_SHELL),sh)
            _cmd := $(subst %,%%,$(_cmd))
         endif
         $(shell $(_cmd) > $(TOP)$(ROOT)include$(DIRSEP)_repover.txt)
         $(shell git rev-list $(shell git rev-parse --abbrev-ref HEAD) --count >> $(TOP)$(ROOT)include$(DIRSEP)_repover.txt)
      else
         $(info ! WARNING: Git not found in PATH. Version information might not be accurate.)
      endif
   endif
endif

ifeq ($(HB_SYSLOC),)
   ifeq ($(HB_PLATFORM),beos)
      ifneq ($(strip $(foreach dir,/boot/common /boot/system /boot/home/config $(subst :, ,$($(HB_LD_LIBRARY_PATH))),$(findstring |$(dir),|$(HB_INSTALL_PREFIX)))),)
         HB_SYSLOC := yes
      endif
   else
   ifneq ($(HB_PLATFORM_UNIX),)
      ifneq ($(strip $(foreach dir,/usr/local/bin /usr/bin $(subst :, ,$($(HB_LD_LIBRARY_PATH))),$(findstring |$(dir),|$(HB_INSTALL_PREFIX)))),)
         HB_SYSLOC := yes
      endif
   endif
   endif
endif

export HB_SYSLOC

ifneq ($(HB_INSTALL_PREFIX),)

   ifeq ($(HB_BUILD_SHARED),)
      ifneq ($(HB_PLATFORM_UNIX),)
         ifeq ($(HB_SYSLOC),yes)
            export HB_BUILD_SHARED := yes
         endif
      endif
   endif

   LIBSUFFIX :=
   INCSUFFIX :=
   ifeq ($(HB_PLATFORM),beos)
      ifeq ($(HB_SYSLOC),yes)
         LIBSUFFIX := $(DIRSEP)harbour
         INCSUFFIX := $(DIRSEP)harbour
      endif
   else
   ifeq ($(HB_PLATFORM_UNIX),)
      LIBSUFFIX := $(DIRSEP)$(subst /,$(DIRSEP),$(PLAT_COMP))
   else
      LIBSUFFIX :=
      # Use 'lib64' instead of 'lib' for 64-bit targets where lib64 dir exists
      ifneq ($(wildcard $(HB_INSTALL_PREFIX)$(DIRSEP)lib64),)
         ifneq ($(filter $(HB_CPU),x86_64),)
            LIBSUFFIX := 64
         endif
      endif
      # Not perfect, please enhance it.
      ifneq ($(findstring |/usr,|$(HB_INSTALL_PREFIX)),)
         ifeq ($(findstring |/usr/home,|$(HB_INSTALL_PREFIX)),)
            LIBSUFFIX := $(LIBSUFFIX)$(DIRSEP)harbour
            INCSUFFIX := $(DIRSEP)harbour
         endif
      else
         ifneq ($(findstring |/opt,|$(HB_INSTALL_PREFIX)),)
            LIBSUFFIX := $(LIBSUFFIX)$(DIRSEP)harbour
            INCSUFFIX := $(DIRSEP)harbour
         else
            LIBSUFFIX :=
         endif
      endif
   endif
   endif

   # Standard name: BINDIR
   ifeq ($(HB_INSTALL_BIN),)
      export HB_INSTALL_BIN := $(HB_INSTALL_PREFIX)$(DIRSEP)bin
   endif
   # Standard name: LIBDIR
   ifeq ($(HB_INSTALL_LIB),)
      export HB_INSTALL_LIB := $(HB_INSTALL_PREFIX)$(DIRSEP)lib$(LIBSUFFIX)
   endif
   ifeq ($(HB_INSTALL_DYN),)
      ifeq ($(HB_PLATFORM_UNIX),)
         export HB_INSTALL_DYN := $(HB_INSTALL_BIN)
      else
         ifeq ($(HB_PLATFORM),cygwin)
            export HB_INSTALL_DYN := $(HB_INSTALL_BIN)
         else
            export HB_INSTALL_DYN := $(HB_INSTALL_LIB)
         endif
      endif
   endif
   # Standard name: INCLUDEDIR
   ifeq ($(HB_INSTALL_INC),)
      export HB_INSTALL_INC := $(HB_INSTALL_PREFIX)$(DIRSEP)include$(INCSUFFIX)
   endif
   # Standard name: DOCDIR
   ifeq ($(HB_INSTALL_DOC),)
      ifeq ($(HB_PLATFORM_UNIX),)
         export HB_INSTALL_DOC := $(HB_INSTALL_PREFIX)$(DIRSEP)doc
      else
         ifeq ($(HB_SYSLOC),yes)
            export HB_INSTALL_DOC := $(HB_INSTALL_PREFIX)$(DIRSEP)share$(DIRSEP)doc$(DIRSEP)harbour
         else
            export HB_INSTALL_DOC := $(HB_INSTALL_PREFIX)$(DIRSEP)doc
         endif
      endif
   endif
   # Standard name: MANDIR
   ifeq ($(HB_INSTALL_MAN),)
      # Do not set man dir for non-*nix targets
      ifneq ($(HB_PLATFORM_UNIX),)
         ifeq ($(HB_SYSLOC),yes)
            export HB_INSTALL_MAN := $(HB_INSTALL_PREFIX)$(DIRSEP)share$(DIRSEP)man
         endif
      endif
   endif
   # Standard name: ETCDIR
   ifeq ($(HB_INSTALL_ETC),)
      # Do not set doc dir for non-*nix targets
      ifneq ($(HB_PLATFORM_UNIX),)
         ifeq ($(HB_PLATFORM),darwin)
            export HB_INSTALL_ETC := $(HB_INSTALL_PREFIX)$(DIRSEP)private$(DIRSEP)etc$(DIRSEP)harbour
         else
            export HB_INSTALL_ETC := $(HB_INSTALL_PREFIX)$(DIRSEP)etc$(DIRSEP)harbour
         endif
      endif
   endif
   ifeq ($(HB_INSTALL_CONTRIB),)
      ifeq ($(HB_PLATFORM_UNIX),)
         export HB_INSTALL_CONTRIB := $(HB_INSTALL_PREFIX)$(DIRSEP)contrib
      else
         ifneq ($(findstring |/opt/harbour,|$(HB_INSTALL_PREFIX)),)
            export HB_INSTALL_CONTRIB := $(HB_INSTALL_PREFIX)$(DIRSEP)contrib
         else
            export HB_INSTALL_CONTRIB := $(HB_INSTALL_PREFIX)$(DIRSEP)share$(DIRSEP)harbour$(DIRSEP)contrib
         endif
      endif
   endif
else
   # Require HB_INSTALL_PREFIX on non-*nix when install is used,
   # so that obligatory supplement files (like LICENSE.txt) are always
   # copied to install destination.
   ifneq ($(filter install,$(HB_MAKECMDGOALS)),)
      ifeq ($(HB_PLATFORM_UNIX),)
         $(error ! Please set HB_INSTALL_PREFIX and try again. For more information: see README.md)
      endif
   endif
endif

export HB_OBJ_DIR := $(subst /,$(DIRSEP),$(OBJ_DIR))

ifeq ($(HB_HOST_BIN),)
   HB_HOST_BIN_DIR := $(BIN_DIR)
else
   HB_HOST_BIN_DIR := $(HB_HOST_BIN)
endif

ifeq ($(HB_HOST_INC),)
   HB_HOST_INC := $(TOP)$(ROOT)include
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(HB_BUILD_DYN),no)

      HB_IMPLIB_PLOC :=
      HB_DYNLIB_PLOC :=
      HB_DYNLIB_POST :=
      HB_DYNLIB_PEXT :=
      HB_DYNLIB_POSC :=
      HB_DYNLIB_PEXC :=

      ifneq ($(filter $(HB_PLATFORM),win wce cygwin),)

         # harbour-xy[-subtype][.dll|.lib]

         HB_DYNLIB_PLOC := -$(HB_VER_MAJOR)$(HB_VER_MINOR)
         HB_IMPLIB_PLOC := _dll

         ifeq ($(HB_PLATFORM),win)
            ifeq ($(HB_COMPILER),bcc)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-bcc
            else
            ifeq ($(HB_CPU),x86_64)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-x64
            else
            ifeq ($(HB_CPU),ia64)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-ia64
            endif
            endif
            endif
         else
         ifeq ($(HB_PLATFORM),wce)
            HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-wce
            ifeq ($(HB_CPU),arm)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-arm
            else
            ifeq ($(HB_CPU),x86)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-x86
            else
            ifeq ($(HB_CPU),mips)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-mips
            else
            ifeq ($(HB_CPU),sh)
               HB_DYNLIB_PLOC := $(HB_DYNLIB_PLOC)-sh
            endif
            endif
            endif
            endif
         endif
         endif
      else
      ifneq ($(filter $(HB_PLATFORM),dos os2),)
         # harbour[.dll|.???]
      else
         HB_DYN_VERCPT := $(HB_VER_MAJOR).$(HB_VER_MINOR)
         HB_DYN_VER := $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)

         ifeq ($(HB_PLATFORM),darwin)
            # libharbour.2.1.0.dylib
            # libharbour.2.1.dylib ->
            # libharbour.dylib ->
            HB_DYNLIB_POST := .$(HB_DYN_VER)
            HB_DYNLIB_POSC := .$(HB_DYN_VERCPT)
         else
            # libharbour.s?.2.1.0
            # libharbour.s?.2.1 ->
            # libharbour.s? ->
            HB_DYNLIB_PEXT := .$(HB_DYN_VER)
            HB_DYNLIB_PEXC := .$(HB_DYN_VERCPT)
         endif
      endif
      endif

      export HB_DYNLIB_POST
      export HB_DYNLIB_PEXT
      export HB_DYNLIB_POSC
      export HB_DYNLIB_PEXC

      export HB_DYNLIB_BASE := harbour$(HB_DYNLIB_PLOC)
      export HB_IMPLIB_BASE := harbour$(HB_IMPLIB_PLOC)

      ifeq ($(__HB_BUILD_DYN_2ND),yes)
         export HB_DYNLIB_BASE_2ND := harbour2$(HB_DYNLIB_PLOC)
         export HB_IMPLIB_BASE_2ND := harbour2$(HB_IMPLIB_PLOC)
      endif
   endif
endif

CXX :=
CC_DIRSEPFROM :=
CC_DIRSEPTO :=

# export some variables to eliminate repeated setting in recursive calls
export HB_HOST_PLAT
export HB_HOST_CPU
export HB_HOST_BIN_DIR
export HB_HOST_BIN_EXT

# clear these options for an unambiguous Harbour environment
export HARBOUR :=
export HARBOURCMD :=
export CLIPPER :=
export CLIPPERCMD :=

# relevant only on non-*nix hosts where --print-directory is on by default
ifeq ($(findstring w,$(MAKEFLAGS)),)
   MKFLAGS := --no-print-directory
endif

export HB_INIT_DONE := yes

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/global.mk
include $(TOP)$(ROOT)config/globsh.mk

export HB_DYNLIB_EXT := $(DYN_EXT)
export HB_DYNLIB_DIR := $(DYN_DIR)
export HB_DYNLIB_PREF := $(DYN_PREF)

endif # GLOBAL_MK_
