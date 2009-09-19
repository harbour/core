#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# GNU make docs:
#    http://www.gnu.org/software/make/manual/make.html
#    http://www.wanderinghorse.net/computing/make/
#    http://www.jgc.org/feeds/topic-gnumake.xml
#    http://lists.gnu.org/archive/html/help-make/
#    http://make.paulandlesley.org/
# Portable shell programming:
#    http://www.gnu.org/software/autoconf/manual/html_node/Portable-Shell.html
#    http://www.gnu.org/software/bash/manual/bashref.html
#    http://www.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
# GNU coding standards:
#    http://www.gnu.org/prep/standards/standards.html
# GNU Make NEWS:
#    http://cvs.savannah.gnu.org/viewvc/make/NEWS?root=make&view=markup
# ---------------------------------------------------------------

# NOTE: $(realpath/abspath) need GNU Make 3.81 or upper
# NOTE: $(eval) needs GNU Make 3.80 or upper

ifeq ($(GLOBAL_MK_),)
GLOBAL_MK_ := yes

HB_VER_MAJOR     := 2
HB_VER_MINOR     := 0
HB_VER_RELEASE   := 0
HB_VER_STATUS    := beta3
HB_VER_STATUS_SH := b3

# Arbitrary pattern which we do not expect to occur in real-world path names
substpat := !@!@

# This is not strictly necessary, but it does signficantly reduce
# the number of rules that make has to evaluate otherwise, which may give
# a performance boost on a slow system.
.SUFFIXES:

.PHONY: all clean install

need := 3.81
ok := $(filter $(need),$(firstword $(sort $(MAKE_VERSION) $(need))))

ifeq ($(ok),)

$(error ! Error: GNU Make version $(MAKE_VERSION) found, $(need) or upper needed for Harbour)

else

need := 3.81
MAKE_381 := $(filter $(need),$(firstword $(sort $(MAKE_VERSION) $(need))))

find_in_path     = $(strip $(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(PATH))), $(wildcard $(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1)$(HB_HOST_BIN_EXT))))
find_in_path_par = $(strip $(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(2))), $(wildcard $(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1)$(HB_HOST_BIN_EXT))))
find_in_path_raw = $(strip $(foreach dir, $(subst $(PTHSEP), ,$(subst $(subst x, ,x),$(substpat),$(2))), $(wildcard $(subst $(substpat),\ ,$(subst \,/,$(dir)))/$(1))))

define detect_watcom_platform
   ifneq ($(call find_in_path_raw,os2.h,$(INCLUDE)),)
      HB_PLATFORM := os2
   else ifneq ($(call find_in_path_raw,dirent.h,$(INCLUDE)),)
      HB_PLATFORM := linux
   else ifeq ($(call find_in_path_raw,windows.h,$(INCLUDE)),)
      HB_PLATFORM := dos
   endif
endef

define check_host

   ifneq ($(findstring MINGW,$(1)),)
      HB_HOST_PLAT := win
   else ifneq ($(findstring MSys,$(1)),)
      HB_HOST_PLAT := win
   else ifneq ($(findstring Windows,$(1)),)
      HB_HOST_PLAT := win
   else ifneq ($(findstring CYGWIN,$(1)),)
      HB_HOST_PLAT := win
   else ifneq ($(findstring Darwin,$(1)),)
      HB_HOST_PLAT := darwin
   else ifneq ($(findstring darwin,$(1)),)
      HB_HOST_PLAT := darwin
   else ifneq ($(findstring Linux,$(1)),)
      HB_HOST_PLAT := linux
   else ifneq ($(findstring linux,$(1)),)
      HB_HOST_PLAT := linux
   else ifneq ($(findstring HP-UX,$(1)),)
      HB_HOST_PLAT := hpux
   else ifneq ($(findstring hp-ux,$(1)),)
      HB_HOST_PLAT := hpux
   else ifneq ($(findstring SunOS,$(1)),)
      HB_HOST_PLAT := sunos
   else ifneq ($(findstring sunos,$(1)),)
      HB_HOST_PLAT := sunos
   else ifneq ($(findstring BSD,$(1)),)
      HB_HOST_PLAT := bsd
   else ifneq ($(findstring bsd,$(1)),)
      HB_HOST_PLAT := bsd
   else ifneq ($(findstring OS/2,$(1)),)
      HB_HOST_PLAT := os2
   else ifneq ($(findstring MS-DOS,$(1)),)
      HB_HOST_PLAT := dos
   else ifneq ($(findstring msdos,$(1)),)
      HB_HOST_PLAT := dos
   else ifneq ($(findstring beos,$(1)),)
      HB_HOST_PLAT := beos
   else ifneq ($(findstring Haiku,$(1)),)
      HB_HOST_PLAT := beos
   endif

endef

define check_host_cpu

   # TODO: Please fix/extend

   ifneq ($(findstring ppc64,$(1)),)
      HB_HOST_CPU := ppc64
   else ifneq ($(findstring ppc,$(1)),)
      HB_HOST_CPU := ppc
   else ifneq ($(findstring Power,$(1)),)
      HB_HOST_CPU := ppc
   else ifneq ($(findstring arm,$(1)),)
      HB_HOST_CPU := arm
   else ifneq ($(findstring ia64,$(1)),)
      HB_HOST_CPU := ia64
   else ifneq ($(findstring sparc64,$(1)),)
      HB_HOST_CPU := sparc64
   else ifneq ($(findstring sparc,$(1)),)
      HB_HOST_CPU := sparc32
   else ifneq ($(findstring mips,$(1)),)
      HB_HOST_CPU := mips
   else ifneq ($(findstring alpha,$(1)),)
      HB_HOST_CPU := alpha
   else ifneq ($(findstring 9000,$(1)),)
      HB_HOST_CPU := parisc
   else ifneq ($(findstring parisc,$(1)),)
      HB_HOST_CPU := parisc
   else ifneq ($(findstring x86_64,$(1)),)
      HB_HOST_CPU := x86_64
   else ifneq ($(findstring 86,$(1)),)
      HB_HOST_CPU := x86
   else ifneq ($(findstring 64,$(1)),)
      HB_HOST_CPU := x86_64
   else ifneq ($(findstring BePC,$(1)),)
      HB_HOST_CPU := x86
   endif

endef

# Some presets based on HB_BUILD_NAME
ifneq ($(HB_BUILD_NAME),)
   ifeq ($(HB_BUILD_NAME),.r)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= c
   else ifeq ($(HB_BUILD_NAME),.ru)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= c
   else ifeq ($(HB_BUILD_NAME),.rp)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= cpp
   else ifeq ($(HB_BUILD_NAME),.rpu)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= cpp
   else ifeq ($(HB_BUILD_NAME),.d)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= c
   else ifeq ($(HB_BUILD_NAME),.du)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= c
   else ifeq ($(HB_BUILD_NAME),.dp)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= cpp
   else ifeq ($(HB_BUILD_NAME),.dpu)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= cpp
   endif
endif

ifeq ($(HB_INIT_DONE),)
   # Store the original value
   export HB_MAKECMDGOALS := $(MAKECMDGOALS)

   ifeq ($(HB_BUILD_PKG),yes)

      # 'clean' and 'install' are required when building a release package
      ifeq ($(filter clean,$(HB_MAKECMDGOALS)),)
         export HB_BUILD_PKG := no
      else ifeq ($(filter install,$(HB_MAKECMDGOALS)),)
         export HB_BUILD_PKG := no
      else ifneq ($(ROOT),./)
         export HB_BUILD_PKG := no
      endif

      ifeq ($(HB_BUILD_PKG),no)
         $(warning ! Warning: Use 'clean install' from Harbour root directory to create a release package.)
      endif

      # Enforce some basic settings for release packages
      export HB_BUILD_DLL := yes
      export HB_BUILD_IMPLIB := no
      export HB_BUILD_OPTIM := yes
      export HB_BUILD_DEBUG := no
      export HB_BUILD_SHARED := no
   endif

   # Can't build shared tools if we don't create dlls
   ifeq ($(HB_BUILD_DLL),no)
      export HB_BUILD_SHARED := no
   endif
endif

# Make platform detection
ifneq ($(findstring COMMAND,$(SHELL)),)
   HB_MAKE_PLAT := dos
else ifneq ($(findstring sh.exe,$(SHELL)),)
   HB_MAKE_PLAT := win
else ifneq ($(findstring CMD.EXE,$(SHELL)),)
   HB_MAKE_PLAT := os2
else
   HB_MAKE_PLAT := unix
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)

      # Some additional ones to be given a standard name:
      #   HB_DB_DRVEXT                -> -
      #   HB_COMMERCE                 -> ?
      #   HB_BIN_COMPILE              -> HB_BUILD_BIN_DIR
      #   HB_INC_COMPILE              -> - (HB_BUILD_INC_DIR)
      #   HB_DIR_*                    -> HB_LIBDIR_* ? (only used for implib and a few .hbm files)
      #   HB_DLLIBS                   -> (only used in one location, so it's a local matter)
      # Macros:
      #   -DHB_GT_LIB=

      $(info ! Building Harbour $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)$(HB_VER_STATUS) from source - http://www.harbour-project.org)
      $(info ! MAKE: $(MAKE) $(MAKE_VERSION) $(SHELL) $(HB_MAKECMDGOALS) $(MAKEFLAGS) $(if $(MAKESHELL),MAKESHELL: $(MAKESHELL),))
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
         $(info ! HB_USER_AFLAGS: $(HB_USER_DFLAGS))
      endif
      ifneq ($(HB_INSTALL_PREFIX),)
         $(info ! HB_INSTALL_PREFIX: $(HB_INSTALL_PREFIX))
      endif
      ifneq ($(HB_BIN_INSTALL),)
         $(info ! HB_BIN_INSTALL: $(HB_BIN_INSTALL))
      endif
      ifneq ($(HB_LIB_INSTALL),)
         $(info ! HB_LIB_INSTALL: $(HB_LIB_INSTALL))
      endif
      ifneq ($(HB_DYN_INSTALL),)
         $(info ! HB_DYN_INSTALL: $(HB_DYN_INSTALL))
      endif
      ifneq ($(HB_INC_INSTALL),)
         $(info ! HB_INC_INSTALL: $(HB_INC_INSTALL))
      endif
      ifneq ($(HB_DOC_INSTALL),)
         $(info ! HB_DOC_INSTALL: $(HB_DOC_INSTALL))
      endif
      ifneq ($(HB_BUILD_NAME),)
         $(info ! HB_BUILD_NAME: $(HB_BUILD_NAME))
      endif
      ifneq ($(HB_BUILD_PKG),)
         $(info ! HB_BUILD_PKG: $(HB_BUILD_PKG))
      endif
      ifneq ($(HB_BUILD_DLL),)
         $(info ! HB_BUILD_DLL: $(HB_BUILD_DLL))
      endif
      ifneq ($(HB_BUILD_DEBUG),)
         $(info ! HB_BUILD_DEBUG: $(HB_BUILD_DEBUG))
      endif
      ifneq ($(HB_BUILD_OPTIM),)
         $(info ! HB_BUILD_OPTIM: $(HB_BUILD_OPTIM))
      endif
      ifneq ($(HB_BUILD_UNICODE),)
         $(info ! HB_BUILD_UNICODE: $(HB_BUILD_UNICODE))
      endif
      ifneq ($(HB_BUILD_MODE),)
         $(info ! HB_BUILD_MODE: $(HB_BUILD_MODE))
      endif
      ifneq ($(HB_CONTRIBLIBS),)
         $(info ! HB_CONTRIBLIBS: $(HB_CONTRIBLIBS))
      endif
      ifneq ($(HB_CONTRIB_ADDONS),)
         $(info ! HB_CONTRIB_ADDONS: $(HB_CONTRIB_ADDONS))
      endif
      ifneq ($(HB_EXTERNALLIBS),)
         $(info ! HB_EXTERNALLIBS: $(HB_EXTERNALLIBS))
      endif
      ifneq ($(HB_EXTERNAL_ADDONS),)
         $(info ! HB_EXTERNAL_ADDONS: $(HB_EXTERNAL_ADDONS))
      endif
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
   else ifneq ($(OS2_SHELL),)
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
      else ifneq ($(findstring command,$(COMSPEC)),)
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

# NOTE: This can be need if we want to run some internal command which are
#       missing from GNU Make's internal autodetection list. Like 'move' on
#       non-*nix shells. [vszakats]
CMDPREF :=
ifneq ($(HB_SHELL),sh)
   ifneq ($(COMSPEC),)
      CMDPREF := $(COMSPEC) /C
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
   $(eval $(call check_host,$(OSTYPE),))
   ifeq ($(HB_HOST_PLAT),)
      $(eval $(call check_host,$(MACHTYPE),))
      ifeq ($(HB_HOST_PLAT),)
         $(eval $(call check_host,$(OS),))
         ifeq ($(HB_HOST_PLAT),)
            $(eval $(call check_host,$(shell uname -s),))
         endif
      endif
   endif
endif

ifeq ($(HB_HOST_PLAT),)
   ifneq ($(OS2_SHELL),)
      HB_HOST_PLAT := os2
   else ifneq ($(windir),)
      HB_HOST_PLAT := win
   else ifneq ($(WINDIR),)
      HB_HOST_PLAT := win
   else ifeq ($(HB_SHELL),dos)
      HB_HOST_PLAT := dos
   else ifneq ($(HB_PLATFORM),)
      HB_HOST_PLAT := $(HB_PLATFORM)
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
   else ifeq ($(PROCESSOR_ARCHITECTURE),IA64)
      HB_HOST_CPU := ia64
   else
      HB_HOST_CPU := x86
   endif
else
   ifneq ($(filter $(HB_HOST_PLAT),dos os2),)
      HB_HOST_CPU := x86
   else
      $(eval $(call check_host_cpu,$(shell uname -m),))
   endif
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)
      $(info ! HB_HOST_PLAT: $(HB_HOST_PLAT)$(if $(HB_HOST_CPU), ($(HB_HOST_CPU)),)  HB_SHELL: $(HB_SHELL))
   endif
endif

HB_PLAT_AUTO :=
ifeq ($(HB_PLATFORM),)
   HB_PLATFORM := $(HB_HOST_PLAT)
   ifneq ($(HB_COMPILER),)
      ifeq ($(HB_COMPILER),djgpp)
         HB_PLATFORM := dos
      else ifneq ($(filter $(HB_COMPILER),msvcarm msvcmips msvcsh mingwarm poccarm),)
         HB_PLATFORM := wce
      else ifneq ($(filter $(HB_COMPILER),mingw mingw64 msvc msvc64 msvcia64 bcc xcc pocc pocc64),)
         HB_PLATFORM := win
      endif
   endif
   ifneq ($(HB_PLATFORM),)
      HB_PLAT_AUTO := (autodetected)
   endif
endif

HB_COMP_AUTO :=
HB_COMP_PATH :=
ifeq ($(HB_COMPILER),)
   ifneq ($(HB_HOST_PLAT),$(HB_PLATFORM))
      # cross-build section *nix -> win/wce
      ifeq ($(filter $(HB_HOST_PLAT),dos os2),)
         ifeq ($(HB_PLATFORM),win)

            ifeq ($(wildcard $(HB_CCPATH)$(HB_CCPREFIX)gcc),)
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
               ifneq ($(call find_in_path_raw,debian_version,/etc),)
                  HB_CCPREFIX := i586-mingw32msvc-
               else
                  ifneq ($(call find_in_path_raw,gentoo-release,/etc),)
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

            ifeq ($(wildcard $(HB_CCPATH)$(HB_CCPREFIX)gcc),)
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
               MINGW_BASE_LIST := /usr /usr/local /usr/local/mingw32 /opt/xmingw
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
               HB_PLATFORM := win
               export HB_TOOLS_PREF := hbw
               export HB_BUILD_EXTDEF := no
               ifneq ($(HB_BUILD_PART),all)
                  HB_BUILD_PART := lib
               endif
            else
               $(error ! Harbour build could not find mingw32 cross-compiler. Please install it, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
            endif
         else
            ifeq ($(HB_PLATFORM),wce)

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
                  HB_PLATFORM := wce
                  export HB_TOOLS_PREF := hbce
                  export HB_BUILD_EXTDEF := no
                  ifneq ($(HB_BUILD_PART),all)
                     HB_BUILD_PART := lib
                  endif
               else
                  $(error ! Harbour build could not find cegcc cross-compiler. Please install it to /opt/mingw32ce, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
               endif
            else
               ifeq ($(HB_PLATFORM),dos)
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
                     export HB_TOOLS_PREF := hbce
                     export HB_BUILD_EXTDEF := no
                     ifneq ($(HB_BUILD_PART),all)
                        HB_BUILD_PART := lib
                     endif
                  else
                     $(error ! Harbour build could not find djgpp cross-compiler. Please install it to /usr/local/i586-pc-msdosdjgpp, or point HB_CCPATH/HB_CCPREFIX environment variables to it)
                  endif
               endif
            endif
         endif
      endif
   endif
   ifeq ($(HB_COMPILER),)
      ifeq ($(HB_PLATFORM),win)
         HB_COMP_PATH := $(call find_in_path,arm-wince-mingw32ce-gcc)
         ifneq ($(HB_COMP_PATH),)
            HB_COMPILER := mingwarm
            HB_PLATFORM := wce
            HB_CCPREFIX := arm-wince-mingw32ce-
         else
            HB_COMP_PATH := $(call find_in_path,arm-mingw32ce-gcc)
            ifneq ($(HB_COMP_PATH),)
               HB_COMPILER := mingwarm
               HB_PLATFORM := wce
               HB_CCPREFIX := arm-mingw32ce-
            else
               HB_COMP_PATH := $(call find_in_path,i386-mingw32ce-gcc)
               ifneq ($(HB_COMP_PATH),)
                  HB_COMPILER := mingw
                  HB_PLATFORM := wce
                  HB_CCPREFIX := i386-mingw32ce-
               else
                  HB_COMP_PATH := $(call find_in_path,cygstart)
                  ifneq ($(HB_COMP_PATH),)
                     HB_COMPILER := cygwin
                  else
                     HB_COMP_PATH := $(call find_in_path,djasm)
                     ifneq ($(HB_COMP_PATH),)
                        HB_PLATFORM := dos
                        HB_COMPILER := djgpp
                        ifneq ($(HB_HOST_CPU),x86)
                           $(error ! Error: DJGPP cross-builds are only possible on 32-bit Windows hosts)
                        endif
                     else
                        HB_COMP_PATH := $(call find_in_path,gcc)
                        ifneq ($(HB_COMP_PATH),)
                           HB_COMPILER := mingw
                        else
                           HB_COMP_PATH := $(call find_in_path,wpp386)
                           ifneq ($(HB_COMP_PATH),)
                              HB_COMPILER := watcom
                              $(eval $(call detect_watcom_platform))
                           else
                              HB_COMP_PATH := $(call find_in_path,clarm)
                              ifneq ($(HB_COMP_PATH),)
                                 HB_COMPILER := msvcarm
                                 HB_PLATFORM := wce
                                 export HB_VISUALC_VER_PRE80 := yes
                              else
                                 HB_COMP_PATH := $(call find_in_path,armasm)
                                 ifneq ($(HB_COMP_PATH),)
                                    HB_COMPILER := msvcarm
                                    HB_PLATFORM := wce
                                 else
                                    HB_COMP_PATH := $(call find_in_path,idis)
                                    ifneq ($(HB_COMP_PATH),)
                                       HB_COMPILER := iccia64
                                    else
                                       HB_COMP_PATH := $(call find_in_path,icl)
                                       ifneq ($(HB_COMP_PATH),)
                                          HB_COMPILER := icc
                                       else
                                          HB_COMP_PATH := $(call find_in_path,ml64)
                                          ifneq ($(HB_COMP_PATH),)
                                             HB_COMPILER := msvc64
                                          else
                                             HB_COMP_PATH := $(call find_in_path,cl)
                                             ifneq ($(HB_COMP_PATH),)
                                                HB_COMPILER := msvc
                                             else
                                                HB_COMP_PATH := $(call find_in_path,bcc32)
                                                ifneq ($(HB_COMP_PATH),)
                                                   HB_COMPILER := bcc
                                                else
                                                   HB_COMP_PATH := $(call find_in_path,pocc)
                                                   ifneq ($(HB_COMP_PATH),)
                                                      ifneq ($(call find_in_path_raw,coredll.lib,$(LIB)),)
                                                         HB_PLATFORM := wce
                                                         HB_COMPILER := poccarm
                                                      else
                                                         ifneq ($(call find_in_path_raw,dbgeng.lib,$(LIB)),)
                                                            HB_COMPILER := pocc64
                                                         else
                                                            HB_COMPILER := pocc
                                                         endif
                                                      endif
                                                   else
                                                      HB_COMP_PATH := $(call find_in_path,xcc)
                                                      ifneq ($(HB_COMP_PATH),)
                                                         HB_COMPILER := xcc
                                                      else
                                                         HB_COMP_PATH := $(call find_in_path,x86_64-w64-mingw32-gcc)
                                                         ifneq ($(HB_COMP_PATH),)
                                                            HB_COMPILER := mingw64
                                                            HB_CCPREFIX := x86_64-w64-mingw32-
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
            HB_COMP_PATH := $(call find_in_path,wpp386)
            ifneq ($(HB_COMP_PATH),)
               HB_COMPILER := watcom
               $(eval $(call detect_watcom_platform))
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
            ifneq ($(filter $(HB_PLATFORM),hpux bsd),)
               HB_COMP_PATH := $(call find_in_path,gcc)
               ifneq ($(HB_COMP_PATH),)
                  HB_COMPILER := gcc
               endif
            else
               ifeq ($(HB_PLATFORM),darwin)
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
                           HB_COMP_PATH := $(call find_in_path,wpp386)
                           ifneq ($(HB_COMP_PATH),)
                              HB_COMPILER := watcom
                              $(eval $(call detect_watcom_platform))
                           endif
                        endif
                     else
                        ifeq ($(HB_PLATFORM),os2)
                           HB_COMP_PATH := $(call find_in_path,gcc)
                           ifneq ($(HB_COMP_PATH),)
                              HB_COMPILER := gcc
                           else
                              HB_COMP_PATH := $(call find_in_path,wpp386)
                              ifneq ($(HB_COMP_PATH),)
                                 HB_COMPILER := watcom
                                 $(eval $(call detect_watcom_platform))
                              endif
                           endif
                        else
                           ifeq ($(HB_PLATFORM),beos)
                              HB_COMP_PATH := $(call find_in_path,gcc)
                              ifneq ($(HB_COMP_PATH),)
                                 HB_COMPILER := gcc
                              endif
                           endif
                           # add other platforms here
                        endif
                     endif
                  endif
               endif
            endif
         endif
      endif
   endif
   ifneq ($(HB_COMPILER),)
      HB_COMP_PATH := $(subst $(substpat), ,$(dir $(firstword $(subst $(subst x, ,x),$(substpat),$(HB_COMP_PATH)))))
      HB_COMP_AUTO := (autodetected$(if $(HB_COMP_PATH),: $(HB_COMP_PATH),))
   endif
   export HB_CCPATH
   export HB_CCPREFIX
endif

ifeq ($(HB_PLATFORM),)
   $(error ! HB_PLATFORM not set, could not autodetect)
endif
ifeq ($(HB_COMPILER),)
   $(error ! HB_COMPILER not set, could not autodetect)
endif

export HB_PLATFORM
export HB_COMPILER
export HB_SHELL

ifneq ($(filter $(HB_HOST_PLAT),win wce dos os2),)
   HB_HOST_PLAT_UNIX :=
else
   HB_HOST_PLAT_UNIX := yes
endif

ifneq ($(filter $(HB_PLATFORM),win wce dos os2),)
   HB_PLATFORM_UNIX :=
else
   HB_PLATFORM_UNIX := yes
endif

PLAT_COMP := $(HB_PLATFORM)/$(HB_COMPILER)$(subst \,/,$(HB_BUILD_NAME))

OBJ_DIR := obj/$(PLAT_COMP)
BIN_DIR := $(TOP)$(ROOT)bin/$(PLAT_COMP)
LIB_DIR := $(TOP)$(ROOT)lib/$(PLAT_COMP)
ifeq ($(HB_PLATFORM_UNIX),)
   DYN_DIR := $(BIN_DIR)
   IMP_DIR := $(LIB_DIR)
else
   DYN_DIR := $(LIB_DIR)
   IMP_DIR :=
endif
DYN_PREF :=
# define PKG_DIR only if run from root Makefile
ifeq ($(ROOT),./)
   PKG_DIR := $(TOP)$(ROOT)pkg/$(PLAT_COMP)
else
   PKG_DIR :=
endif

# Assemble relative path from OBJ_DIR to source.
GRANDP := $(subst $(subst x,x, ),,$(foreach item, $(subst /, ,$(OBJ_DIR)), ../))

# TODO: Set this in <arch>/<comp>.mk (compiler switches may influence it)
HB_CPU :=
ifeq ($(HB_PLATFORM),win)
   ifneq ($(filter $(HB_COMPILER),msvc64 mingw64 pocc64),)
      HB_CPU := x86_64
   else ifneq ($(filter $(HB_COMPILER),msvcia64 iccia64),)
      HB_CPU := ia64
   else
      HB_CPU := x86
   endif
else
   ifeq ($(HB_PLATFORM),wce)
      ifneq ($(filter $(HB_COMPILER),msvcarm mingwarm poccarm),)
         HB_CPU := arm
      else ifneq ($(filter $(HB_COMPILER),msvcmips),)
         HB_CPU := mips
      else ifneq ($(filter $(HB_COMPILER),msvcsh),)
         HB_CPU := sh
      else
         HB_CPU := x86
      endif
   else
      ifneq ($(filter $(HB_PLATFORM),dos os2),)
         HB_CPU := x86
      else
         HB_CPU := $(HB_HOST_CPU)
      endif
   endif
endif

ifeq ($(HB_INIT_DONE),)
   ifeq ($(HB_COMPILER),djgpp)
      # NOTE: We do need DJGPP build of GNU Make on Windows
      #       systems. The reason is that this uses special
      #       trick to pass command lines to other DJGPP tools
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
   ifneq ($(MAKE_381),)
      $(info ! HB_PLATFORM: $(HB_PLATFORM)$(if $(HB_CPU), ($(HB_CPU)),) $(HB_PLAT_AUTO))
      $(info ! HB_COMPILER: $(HB_COMPILER) $(HB_COMP_AUTO))
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
HB_PRGFLAGS :=

HB_CROSS_BUILD :=
ifneq ($(HB_HOST_PLAT)$(HB_HOST_CPU),$(HB_PLATFORM)$(HB_CPU))
   ifeq ($(HB_BIN_COMPILE),)
      # Not required in these combinations: [vszakats]
      ifneq ($(HB_HOST_PLAT)-$(HB_HOST_CPU)-$(HB_PLATFORM)-$(HB_CPU),win-x86_64-win-x86)
         ifneq ($(HB_HOST_PLAT)-$(HB_HOST_CPU)-$(HB_PLATFORM)-$(HB_CPU),win-x86-dos-x86)
            HB_CROSS_BUILD := yes
            # Try to autosetup
            HB_BIN_COMPILE := $(dir $(firstword $(wildcard $(TOP)$(ROOT)bin/$(HB_HOST_PLAT)/*/harbour$(HB_HOST_BIN_EXT))))
            ifeq ($(HB_BIN_COMPILE),)
               HB_BIN_COMPILE := $(dir $(firstword $(foreach dir,$(subst $(PTHSEP), ,$(PATH)),$(wildcard $(dir)/harbour$(HB_HOST_BIN_EXT)))))
               ifneq ($(HB_BIN_COMPILE),)
                  HB_BIN_COMPILE := $(realpath $(HB_BIN_COMPILE))
               endif
            else
               HB_BIN_COMPILE := $(realpath $(HB_BIN_COMPILE))
            endif
            ifeq ($(HB_BIN_COMPILE),)
               $(warning ! Warning: HB_BIN_COMPILE not specified. Could not find native build.)
            else
               ifneq ($(MAKE_381),)
                  $(info ! HB_BIN_COMPILE not specified. Automatically set to: $(HB_BIN_COMPILE))
               endif
            endif
         endif
      endif
      export HB_BIN_COMPILE
   else
      HB_CROSS_BUILD := yes
   endif
   ifeq ($(HB_CROSS_BUILD),yes)
      # Setup platform macros (undefine host, define target)
      ifeq ($(HB_HOST_PLAT),win)
         HB_PRGFLAGS += -undef:__PLATFORM__WINDOWS
         # We only need this to avoid problems with using Cygwin binaries as native ones.
         HB_PRGFLAGS += -undef:__PLATFORM__UNIX
      else
         ifeq ($(HB_HOST_PLAT),dos)
            HB_PRGFLAGS += -undef:__PLATFORM__DOS
         else
            ifeq ($(HB_HOST_PLAT),os2)
               HB_PRGFLAGS += -undef:__PLATFORM__OS2
            else
               ifeq ($(HB_HOST_PLAT),linux)
                  HB_PRGFLAGS += -undef:__PLATFORM__LINUX -undef:__PLATFORM__UNIX
               endif
            endif
         endif
      endif
      ifeq ($(HB_PLATFORM),win)
         HB_PRGFLAGS += -D__PLATFORM__WINDOWS
         ifeq ($(HB_CPU),x86_64)
            HB_PRGFLAGS += -D__ARCH64BIT__
         else
            ifeq ($(HB_CPU),ia64)
               HB_PRGFLAGS += -D__ARCH64BIT__
            endif
         endif
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
                  endif
               endif
            endif
         endif
      endif
   endif
endif

include $(TOP)$(ROOT)config/detect.mk

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

# Names of valid RDD libraries
HB_RDD_LIBS := \
   rddntx \
   rddnsx \
   rddcdx \
   rddfpt \
   hbsix \
   hbhsx \
   hbusrrdd \
   hbuddall \

# Names of valid RDD subdirectories
HB_RDD_DIRS := \
   dbfntx \
   dbfnsx \
   dbfcdx \
   dbffpt \
   hbsix \
   hsx \
   usrrdd \
   usrrdd/rdds \

ifneq ($(HB_DB_DRVEXT),)
   HB_RDD_LIBS += $(HB_DB_DRVEXT)
   HB_RDD_DIRS += $(HB_DB_DRVEXT)
endif

ifneq ($(HB_PLATFORM),dos)
   HB_VERSION := $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)$(HB_VER_STATUS)
   HB_PKGNAME := harbour-$(HB_VERSION)-$(HB_PLATFORM)-$(HB_COMPILER)
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
HB_INSTALL_PREFIX_TOP := $(subst /,$(DIRSEP),$(realpath $(TOP)$(ROOT)))
ifeq ($(HB_BUILD_PKG),yes)
   ifeq ($(HB_INIT_DONE),)
      export HB_TOP := $(subst /,$(DIRSEP),$(realpath $(TOP)$(ROOT)))
      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(abspath $(PKG_DIR)/$(HB_PKGNAME)))

      HB_BIN_INSTALL :=
      HB_INC_INSTALL :=
      HB_LIB_INSTALL :=
      HB_DYN_INSTALL :=
      HB_DOC_INSTALL :=
   endif
else
   # Fill it automatically if not specified
   ifeq ($(HB_INSTALL_PREFIX),)

      ifeq ($(HB_PLATFORM),beos)
         HB_INSTALL_PREFIX := /boot/common
      else ifeq ($(HB_PLATFORM_UNIX),)
         HB_INSTALL_PREFIX := $(realpath $(TOP)$(ROOT))
      else
         ifneq ($(PREFIX),)
            HB_INSTALL_PREFIX := $(PREFIX)
         else
            ifneq ($(DESTDIR),)
               HB_INSTALL_PREFIX := $(DESTDIR)
            else
               # Stick to *nix customs. I do not like it, it needs admin.
               HB_INSTALL_PREFIX := /usr/local
               # Add postfix for cross builds
               ifneq ($(HB_HOST_PLAT),$(HB_PLATFORM))
                  HB_INSTALL_PREFIX += /harbour-$(HB_PLATFORM)-$(HB_COMPILER)
               endif
            endif
         endif
      endif

      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))
   else
      # TOFIX: HB_INSTALL_PREFIX will have to be duplicated internally to avoid
      #        recursive operation here.

      # Handle simple macros in value
      HB_INSTALL_PREFIX := $(subst {hb_plat},$(HB_PLATFORM),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {hb_comp},$(HB_COMPILER),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {hb_cpu},$(HB_CPU),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {hb_top},$(realpath $(TOP)$(ROOT)),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))
   endif
endif

export HB_INSTALL_PREFIX

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)
      ifneq ($(HB_INSTALL_PREFIX_ORI),$(HB_INSTALL_PREFIX))
         $(info ! HB_INSTALL_PREFIX automatically set to: $(HB_INSTALL_PREFIX))
      endif
      ifeq ($(ROOT),./)
         ifneq ($(call find_in_path,svnversion),)
            $(info ! REVISION: $(shell svnversion .))
         endif
      endif
   endif
endif

HB_SYSLOC :=
ifneq ($(findstring |/usr/local/bin,|$(HB_INSTALL_PREFIX)),)
   HB_SYSLOC := yes
else ifneq ($(findstring |/usr/bin,|$(HB_INSTALL_PREFIX)),)
   HB_SYSLOC := yes
else ifneq ($(findstring |/opt/harbour,|$(HB_INSTALL_PREFIX)),)
   HB_SYSLOC := yes
else ifneq ($(findstring |/opt/bin,|$(HB_INSTALL_PREFIX)),)
   HB_SYSLOC := yes
endif
export HB_SYSLOC

ifneq ($(HB_INSTALL_PREFIX),)

   ifeq ($(HB_BUILD_SHARED),)
      ifeq ($(HB_SYSLOC),yes)
         export HB_BUILD_SHARED := yes
      endif
   endif

   ifeq ($(HB_PLATFORM),beos)
      LIBPOSTFIX := $(DIRSEP)harbour
      INCPOSTFIX := $(DIRSEP)harbour
   else ifeq ($(HB_PLATFORM_UNIX),)
      LIBPOSTFIX := $(DIRSEP)$(subst /,$(DIRSEP),$(PLAT_COMP))
   else
      # Not perfect, please enhance it.
      ifneq ($(findstring |/usr,|$(HB_INSTALL_PREFIX)),)
         ifeq ($(findstring |/usr/home,|$(HB_INSTALL_PREFIX)),)
            LIBPOSTFIX := $(DIRSEP)harbour
            INCPOSTFIX := $(DIRSEP)harbour
         endif
      else
         ifneq ($(findstring |/opt,|$(HB_INSTALL_PREFIX)),)
            LIBPOSTFIX := $(DIRSEP)harbour
            INCPOSTFIX := $(DIRSEP)harbour
         endif
      endif
      # Use 'lib64' instead of 'lib' for 64-bit targets where lib64 dir exists
      ifneq ($(wildcard $(HB_INSTALL_PREFIX)$(DIRSEP)lib64),)
         ifneq ($(filter $(HB_CPU),x86_64),)
            LIBPOSTFIX := 64$(LIBPOSTFIX)
         endif
      endif
   endif

   # Standard name: BINDIR
   ifeq ($(HB_BIN_INSTALL),)
      export HB_BIN_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)bin
   endif
   # Standard name: LIBDIR
   ifeq ($(HB_LIB_INSTALL),)
      export HB_LIB_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)lib$(LIBPOSTFIX)
   endif
   ifeq ($(HB_DYN_INSTALL),)
      ifeq ($(HB_PLATFORM_UNIX),)
         export HB_DYN_INSTALL := $(HB_BIN_INSTALL)
      else
         export HB_DYN_INSTALL := $(HB_LIB_INSTALL)
      endif
   endif
   # Standard name: INCLUDEDIR
   ifeq ($(HB_INC_INSTALL),)
      HB_INC_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)include$(INCPOSTFIX)
      # Do not set include install dir if it's inside the source tree
      # to avoid 'source same as dest' warnings on 'install' copy operation.
      ifneq ($(HB_SHELL),sh)
         ifeq ($(HB_INSTALL_PREFIX),$(HB_INSTALL_PREFIX_TOP))
            HB_INC_INSTALL :=
         endif
      endif
      export HB_INC_INSTALL
   endif
   # Standard name: DOCDIR
   ifeq ($(HB_DOC_INSTALL),)
      # Do not set doc dir for *nix targets
      ifeq ($(HB_PLATFORM_UNIX),)
         ifneq ($(HB_INSTALL_PREFIX),$(HB_INSTALL_PREFIX_TOP))
            export HB_DOC_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)doc
         endif
      endif
   endif
endif

export HB_OBJ_DIR := $(subst /,$(DIRSEP),$(OBJ_DIR))

ifeq ($(HB_BIN_COMPILE),)
   HB_HOST_BIN_DIR := $(BIN_DIR)
else
   HB_HOST_BIN_DIR := $(HB_BIN_COMPILE)
endif

ifeq ($(HB_INC_COMPILE),)
   HB_INC_COMPILE := $(TOP)$(ROOT)include
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(HB_BUILD_DLL),no)

      ifeq ($(HB_PLATFORM_UNIX),)
         HB_DYN_VER := $(HB_VER_MAJOR)$(HB_VER_MINOR)
      else
         HB_DYN_VER := $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE)
      endif

      ifeq ($(HB_PLATFORM),darwin)
         DYNNAME_POST := .$(HB_DYN_VER)
      else
         DYNNAME_POST := -$(HB_DYN_VER)
      endif

      ifeq ($(HB_PLATFORM),win)
         ifeq ($(HB_COMPILER),bcc)
            DYNNAME_POST := $(DYNNAME_POST)-bcc
         else
            ifeq ($(HB_CPU),x86_64)
               DYNNAME_POST := $(DYNNAME_POST)-x64
            else
               ifeq ($(HB_CPU),ia64)
                  DYNNAME_POST := $(DYNNAME_POST)-ia64
               endif
            endif
         endif
      else
         ifeq ($(HB_PLATFORM),wce)
            DYNNAME_POST := $(DYNNAME_POST)-wce
            ifeq ($(HB_CPU),arm)
               DYNNAME_POST := $(DYNNAME_POST)-arm
            else
               ifeq ($(HB_CPU),mips)
                  DYNNAME_POST := $(DYNNAME_POST)-mips
               else
                  ifeq ($(HB_CPU),sh)
                     DYNNAME_POST := $(DYNNAME_POST)-sh
                  endif
               endif
            endif
         else
            ifeq ($(HB_PLATFORM),os2)
               DYNNAME_POST := $(DYNNAME_POST)-os2
            endif
         endif
      endif

      ifeq ($(HB_PLATFORM)-$(HB_COMPILER),dos-watcom)
         HB_DYNLIB_ST := harbour
         HB_DYNLIB_MT := harbourm
      else
         HB_DYNLIB_ST := harbour$(DYNNAME_POST)
         HB_DYNLIB_MT := harbourmt$(DYNNAME_POST)
      endif

      export HB_DYNLIB_ST
      export HB_DYNLIB_MT
   endif
endif

HB_DYN_COMPILE := no

CXX :=

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

endif

endif # GLOBAL_MK_
