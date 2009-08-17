#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# See GNU make docs here:
#    http://www.gnu.org/software/make/manual/make.html
#    http://www.jgc.org/feeds/topic-gnumake.xml
#    http://lists.gnu.org/archive/html/help-make/
# Portable shell programming:
#    http://www.gnu.org/software/autoconf/manual/html_node/Portable-Shell.html
#    http://www.gnu.org/software/bash/manual/bashref.html
#    http://www.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
# GNU Coding standards:
#    http://www.gnu.org/prep/standards/standards.html
# ---------------------------------------------------------------

# GNU Make NEWS:
#    http://cvs.savannah.gnu.org/viewvc/make/NEWS?root=make&view=markup
#
# TOFIX: $(realpath/abspath) need GNU Make 3.81 or upper
# TOFIX: $(eval) needs GNU Make 3.80 or upper
# NOTE: $(error/warning/call/if) need GNU Make 3.78 or upper
# NOTE: $(wordlist/basename/suffix) need GNU Make 3.76 or upper

ifeq ($(GLOBAL_CF_),)
GLOBAL_CF_ := yes

# This isn't strictly necessary, but it does signficantly reduce
# the number of rules that make has to evaluate otherwise, which may give
# a performance boost on a slow system.
.SUFFIXES:

need := 3.70
ok := $(filter $(need),$(firstword $(sort $(MAKE_VERSION) $(need))))

ifeq ($(ok),)

all: ; @echo "! Error: GNU Make version $(MAKE_VERSION) found, $(need) or upper needed for Harbour"

else

need := 3.81
MAKE_381 := $(filter $(need),$(firstword $(sort $(MAKE_VERSION) $(need))))

# Don't indent this subroutine
define find_in_path
$(strip $(foreach dir,$(subst $(PTHSEP), ,$(PATH)),$(wildcard $(dir)/$(1)$(HB_HOST_BIN_EXT))))
endef

define check_host

ifneq ($(findstring MINGW,$(1)),)
   HB_HOST_ARCH := win
else
   ifneq ($(findstring MSys,$(1)),)
      HB_HOST_ARCH := win
   else
      ifneq ($(findstring Windows,$(1)),)
         HB_HOST_ARCH := win
      else
         ifneq ($(findstring CYGWIN,$(1)),)
            HB_HOST_ARCH := win
         else
            ifneq ($(findstring Darwin,$(1)),)
               HB_HOST_ARCH := darwin
            else
               ifneq ($(findstring darwin,$(1)),)
                  HB_HOST_ARCH := darwin
               else
                  ifneq ($(findstring Linux,$(1)),)
                     HB_HOST_ARCH := linux
                  else
                     ifneq ($(findstring linux,$(1)),)
                        HB_HOST_ARCH := linux
                     else
                        ifneq ($(findstring HP-UX,$(1)),)
                           HB_HOST_ARCH := hpux
                        else
                           ifneq ($(findstring hp-ux,$(1)),)
                              HB_HOST_ARCH := hpux
                           else
                              ifneq ($(findstring SunOS,$(1)),)
                                 HB_HOST_ARCH := sunos
                              else
                                 ifneq ($(findstring sunos,$(1)),)
                                    HB_HOST_ARCH := sunos
                                 else
                                    ifneq ($(findstring BSD,$(1)),)
                                       HB_HOST_ARCH := bsd
                                    else
                                       ifneq ($(findstring bsd,$(1)),)
                                          HB_HOST_ARCH := bsd
                                       else
                                          ifneq ($(findstring OS/2,$(1)),)
                                             HB_HOST_ARCH := os2
                                          else
                                             ifneq ($(findstring msdos,$(1)),)
                                                HB_HOST_ARCH := dos
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

endef

# Some presets based on HB_BUILD_NAME
ifneq ($(HB_BUILD_NAME),)
   ifeq ($(HB_BUILD_NAME),.r)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.ru)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.rp)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= cpp
   else
   ifeq ($(HB_BUILD_NAME),.rpu)
      HB_BUILD_DEBUG := no
      HB_BUILD_OPTIM := yes
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= cpp
   else
   ifeq ($(HB_BUILD_NAME),.d)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.du)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= c
   else
   ifeq ($(HB_BUILD_NAME),.dp)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := no
      HB_BUILD_MODE ?= cpp
   else
   ifeq ($(HB_BUILD_NAME),.dpu)
      HB_BUILD_DEBUG := yes
      HB_BUILD_OPTIM := no
      HB_BUILD_UNICODE := yes
      HB_BUILD_MODE ?= cpp
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
   # Store the original value
   export HB_MAKECMDGOALS := $(MAKECMDGOALS)

   ifeq ($(HB_BUILD_PKG),yes)

      # 'clean' and 'install' are required when building a release package
      ifeq ($(findstring clean,$(HB_MAKECMDGOALS)),)
         export HB_BUILD_PKG := no
      else
         ifeq ($(findstring install,$(HB_MAKECMDGOALS)),)
            export HB_BUILD_PKG := no
         else
            ifeq ($(HB_POSTINST),)
               export HB_BUILD_PKG := no
            endif
         endif
      endif

      ifeq ($(HB_BUILD_PKG),no)
         $(warning ! Warning: Use 'clean install' from Harbour root directory to create a release package.)
      endif

      # Enforce some basic setting for release packages
      export HB_BUILD_DLL := yes
      export HB_BUILD_IMPLIB := no
      export HB_BUILD_OPTIM := yes
      export HB_BUILD_DEBUG := no
   endif
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)

      # Some additional ones to be given a standard name:
      #   HB_HOST_BUILD [yes|all|lib] -> HB_BUILD_LIBONLY (rest is redundant and can be controlled by other means)
      #   HB_XBUILD                   -> HB_BUILD_INCDEF
      #   HB_WITHOUT_*                -> HB_HAS_*
      #   HB_REBUILD_PARSER           -> HB_BUILD_PARSER
      #   HB_DB_DRVEXT                -> -
      #   HB_COMMERCE                 -> ?
      #   HB_CRS_LIB                  -> HB_LIB_CURSES
      #   HB_BUILD_VERBOSE            [ OK ]
      #   HB_BIN_COMPILE              -> HB_BUILD_BIN_DIR
      #   HB_INC_COMPILE              -> - (HB_BUILD_INC_DIR)
      #   HB_GPM_MOUSE                -> HB_HAS_GPM
      #   HB_POSTINST                 -> ?
      #   HB_ROOTPOSTINST             -> ?
      #   HB_POSTINSTPARAM            -> ?
      #   HB_GPM_NOICE_DISABLE        -> HB_USER_CFLAGS=-DHB_GPM_NOICE_DISABLE
      #   HB_GT_CRS_BCEHACK           -> HB_USER_CFLAGS=-DHB_GT_CRS_BCEHACK
      #   HB_NCURSES_194              -> HB_USER_CFLAGS=-DHB_NCURSES_194
      # Macros:
      #   -DHB_PCRE_REGEX, -DHB_POSIX_REGEX, -DHB_EXT_ZLIB, -DHB_HAS_GPM

      $(info ! MAKE: $(MAKE) $(MAKE_VERSION) $(HB_MAKECMDGOALS) $(MAKEFLAGS) $(SHELL))
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
      ifneq ($(HB_INSTALL_PREFIX),)
         $(info ! HB_INSTALL_PREFIX: $(HB_INSTALL_PREFIX))
      endif
      ifneq ($(HB_BIN_INSTALL),)
         $(info ! HB_BIN_INSTALL: $(HB_BIN_INSTALL))
      endif
      ifneq ($(HB_LIB_INSTALL),)
         $(info ! HB_LIB_INSTALL: $(HB_LIB_INSTALL))
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
else
   ifeq ($(SHELL),/bin/sh)
      HB_SHELL := sh
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
            endif
         endif
      endif
   endif
endif

# Not needed anymore, can be deleted if everything stays fine [20090812] [vszakats]
# CMDPREF :=
# ifneq ($(HB_SHELL),sh)
#    ifneq ($(COMSPEC),)
#       CMDPREF := $(COMSPEC) /C
#    endif
# endif

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

ifeq ($(HB_HOST_ARCH),)
   $(eval $(call check_host,$(OSTYPE),))
   ifeq ($(HB_HOST_ARCH),)
      $(eval $(call check_host,$(MACHTYPE),))
      ifeq ($(HB_HOST_ARCH),)
         $(eval $(call check_host,$(OS),))
         ifeq ($(HB_HOST_ARCH),)
            $(eval $(call check_host,$(shell uname -s),))
         endif
      endif
   endif
endif

ifeq ($(HB_HOST_ARCH),)
   ifneq ($(OS2_SHELL),)
      HB_HOST_ARCH := os2
   else
      ifneq ($(windir),)
         HB_HOST_ARCH := win
      else
         ifneq ($(WINDIR),)
            HB_HOST_ARCH := win
         else
            ifneq ($(HB_ARCHITECTURE),)
               HB_HOST_ARCH := $(HB_ARCHITECTURE)
            else
               HB_HOST_ARCH := dos
            endif
         endif
      endif
   endif
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)
      $(info ! HB_HOST_ARCH: $(HB_HOST_ARCH)  HB_SHELL: $(HB_SHELL))
   endif
endif

ifneq ($(findstring $(HB_HOST_ARCH),win wce dos os2),)
   HB_HOST_BIN_EXT := .exe
else
   HB_HOST_BIN_EXT :=
endif

ifeq ($(HB_BUILD_VERBOSE),yes)
   ifneq ($(MAKE_381),)
      $(info ! Detected host executable extension: $(HB_HOST_BIN_EXT))
   endif
endif

# Couldn't find a builds of these tools which would fit Harbour respository,
# so these will have to installed by user.
#ifeq ($(HB_SHELL),os2)
#   ifeq ($(call find_in_path,mkdir),)
#      $(error ! Harbour build on OS/2 requires GNU mkdir executable in PATH. See INSTALL for more.)
#   else
#      ifeq ($(call find_in_path,rm),)
#         $(error ! Harbour build on OS/2 requires GNU rm executable in PATH. See INSTALL for more.)
#      endif
#   endif
#endif

ifeq ($(HB_SHELL),sh)
   ECHOQUOTE := "
else
   ECHOQUOTE :=
endif

HB_HOST_CPU :=
ifeq ($(HB_HOST_ARCH),win)
   ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
      HB_HOST_CPU := x86_64
   else
      ifeq ($(PROCESSOR_ARCHITECTURE),IA64)
         HB_HOST_CPU := ia64
      else
         HB_HOST_CPU := x86
      endif
   endif
else
   # TODO: CPU detection for rest of systems.
endif

ifeq ($(HB_BUILD_VERBOSE),yes)
   ifneq ($(MAKE_381),)
      $(info ! Detected host CPU: $(HB_HOST_CPU))
   endif
endif

HB_ARCH_AUTO :=
ifeq ($(HB_ARCHITECTURE),)
   HB_ARCHITECTURE := $(HB_HOST_ARCH)
   ifneq ($(HB_COMPILER),)
      ifeq ($(HB_COMPILER),msvcarm)
         HB_ARCHITECTURE := wce
      else
         ifeq ($(HB_COMPILER),mingwarm)
            HB_ARCHITECTURE := wce
         else
            ifeq ($(HB_COMPILER),poccarm)
               HB_ARCHITECTURE := wce
            else
               ifeq ($(HB_COMPILER),djgpp)
                  HB_ARCHITECTURE := dos
               else
                  ifeq ($(findstring $(HB_COMPILER),mingw mingw64 msvc msvc64 msvcia64 bcc xcc pocc pocc64),)
                     HB_ARCHITECTURE := win
                  endif
               endif
            endif
         endif
      endif
   endif
   ifneq ($(HB_ARCHITECTURE),)
      HB_ARCH_AUTO := (autodetected)
   endif
endif

HB_COMP_AUTO :=
ifeq ($(HB_COMPILER),)
   ifeq ($(HB_ARCHITECTURE),win)
      ifneq ($(call find_in_path,arm-wince-mingw32ce-gcc),)
         HB_COMPILER := mingwarm
         HB_ARCHITECTURE := wce
         HB_CCPREFIX := arm-wince-mingw32ce-
      else
         ifneq ($(call find_in_path,arm-mingw32ce-gcc),)
            HB_COMPILER := mingwarm
            HB_ARCHITECTURE := wce
            HB_CCPREFIX := arm-mingw32ce-
         else
            ifneq ($(call find_in_path,cygstart),)
               HB_COMPILER := cygwin
            else
               ifneq ($(call find_in_path,gcc),)
                  HB_COMPILER := mingw
               else
                  ifneq ($(call find_in_path,wpp386),)
                     HB_COMPILER := watcom
                  else
                     ifneq ($(call find_in_path,ml64),)
                        HB_COMPILER := msvc64
                     else
                        ifneq ($(call find_in_path,icl),)
                           HB_COMPILER := icc
                        else
                           ifneq ($(call find_in_path,cl),)
                              HB_COMPILER := msvc
                           else
                              ifneq ($(call find_in_path,bcc32),)
                                 HB_COMPILER := bcc
                              else
                                 ifneq ($(call find_in_path,pocc),)
                                    HB_COMPILER := pocc
                                 else
                                    ifneq ($(call find_in_path,xcc),)
                                       HB_COMPILER := xcc
                                    else
                                       ifneq ($(call find_in_path,x86_64-w64-mingw32-gcc),)
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
      export HB_CCPREFIX
   else
      ifeq ($(HB_ARCHITECTURE),linux)
         ifneq ($(call find_in_path,wpp386),)
            HB_COMPILER := watcom
         else
            ifneq ($(call find_in_path,gcc),)
               HB_COMPILER := gcc
            endif
         endif
      else
         ifneq ($(findstring $(HB_ARCHITECTURE),darwin hpux bsd),)
            ifneq ($(call find_in_path,gcc),)
               HB_COMPILER := gcc
            endif
         else
            ifeq ($(HB_ARCHITECTURE),sunos)
               ifneq ($(call find_in_path,cc),)
                  HB_COMPILER := sunpro
               else
                  ifneq ($(call find_in_path,gcc),)
                     HB_COMPILER := gcc
                  endif
               endif
            else
               ifeq ($(HB_ARCHITECTURE),dos)
                  ifneq ($(call find_in_path,gcc),)
                     HB_COMPILER := djgpp
                  else
                     ifneq ($(call find_in_path,wpp386),)
                        HB_COMPILER := watcom
                     endif
                  endif
               else
                  ifeq ($(HB_ARCHITECTURE),os2)
                     ifneq ($(call find_in_path,gcc),)
                        HB_COMPILER := gcc
                     else
                        ifneq ($(call find_in_path,wpp386),)
                           HB_COMPILER := watcom
                        endif
                     endif
                  else
                     # add other platforms here
                  endif
               endif
            endif
         endif
      endif
   endif
   ifneq ($(HB_COMPILER),)
      HB_COMP_AUTO := (autodetected)
   endif
endif

ifeq ($(HB_ARCHITECTURE),)
   $(error ! HB_ARCHICTECTURE not set, couldn't autodetect.)
endif
ifeq ($(HB_COMPILER),)
   $(error ! HB_COMPILER not set, couldn't autodetect.)
endif

ifeq ($(HB_INIT_DONE),)
   ifneq ($(MAKE_381),)
      $(info ! HB_ARCHITECTURE: $(HB_ARCHITECTURE) $(HB_ARCH_AUTO))
      $(info ! HB_COMPILER: $(HB_COMPILER) $(HB_COMP_AUTO))
   endif
endif

export HB_ARCHITECTURE
export HB_COMPILER

ARCH_COMP := $(HB_ARCHITECTURE)/$(HB_COMPILER)$(subst \,/,$(HB_BUILD_NAME))

OBJ_DIR := obj/$(ARCH_COMP)
BIN_DIR := $(TOP)$(ROOT)bin/$(ARCH_COMP)
LIB_DIR := $(TOP)$(ROOT)lib/$(ARCH_COMP)
# define PKG_DIR only if run from root Makefile
ifneq ($(HB_POSTINST),)
   PKG_DIR := $(TOP)$(ROOT)pkg/$(ARCH_COMP)
endif

# Assemble relative path from OBJ_DIR to source.
GRANDP := $(subst $(subst x,x, ),,$(foreach item, $(subst /, ,$(OBJ_DIR)), ../))

# TODO: Set this in <arch>/<comp>.mk
HB_CPU :=
ifeq ($(HB_ARCHITECTURE),win)
   ifeq ($(HB_COMPILER),msvc64)
      HB_CPU := x86_64
   else
      ifeq ($(HB_COMPILER),mingw64)
         HB_CPU := x86_64
      else
         ifeq ($(HB_COMPILER),pocc64)
            HB_CPU := x86_64
         else
            ifeq ($(HB_COMPILER),msvcia64)
               HB_CPU := ia64
            else
               ifeq ($(HB_COMPILER),iccia64)
                  HB_CPU := ia64
               else
                  HB_CPU := x86
               endif
            endif
         endif
      endif
   endif
endif

ifeq ($(HB_BUILD_VERBOSE),yes)
   ifneq ($(MAKE_381),)
      $(info ! Detected target CPU: $(HB_CPU))
   endif
endif

ifneq ($(findstring $(HB_ARCHITECTURE),win wce dos os2),)
   HB_OS_UNIX := no
else
   HB_OS_UNIX := yes
endif

# Reserve variables for local compiler flags. Makefiles
# should only modify these instead of HB_USER_* variables
# as these can have bad side effects (doubly added values)
# caused by recursive GNU Make runs.
# Notice that even single lib/bin builds will currently
# result in recursive runs, see rule 'descend'. [vszakats]
HB_CFLAGS :=
HB_PRGFLAGS :=

HB_CROSS_BUILD :=
ifneq ($(HB_HOST_ARCH)$(HB_HOST_CPU),$(HB_ARCHITECTURE)$(HB_CPU))
   ifeq ($(HB_BIN_COMPILE),)
      # Not required in these combinations: [vszakats]
      ifneq ($(HB_HOST_ARCH)-$(HB_HOST_CPU)-$(HB_ARCHITECTURE)-$(HB_CPU),win-x86_64-win-x86)
         ifneq ($(HB_HOST_ARCH)-$(HB_HOST_CPU)-$(HB_ARCHITECTURE)-$(HB_CPU),win-x86-dos-)
            HB_CROSS_BUILD := yes
            # Try to autosetup
            HB_BIN_COMPILE := $(dir $(firstword $(wildcard $(TOP)$(ROOT)bin/$(HB_HOST_ARCH)/*/harbour$(HB_HOST_BIN_EXT))))
            ifeq ($(HB_BIN_COMPILE),)
               HB_BIN_COMPILE := $(dir $(firstword $(foreach dir,$(subst $(PTHSEP), ,$(PATH)),$(wildcard $(dir)/harbour$(HB_HOST_BIN_EXT)))))
               ifneq ($(HB_BIN_COMPILE),)
                  HB_BIN_COMPILE := $(realpath $(HB_BIN_COMPILE))
               endif
            else
               HB_BIN_COMPILE := $(realpath $(HB_BIN_COMPILE))
            endif
            ifeq ($(HB_BIN_COMPILE),)
               $(warning ! Warning: HB_BIN_COMPILE not specified. Couldn't find native build.)
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
      ifeq ($(HB_HOST_ARCH),win)
         HB_PRGFLAGS += -undef:__PLATFORM__WINDOWS
      else
         ifeq ($(HB_HOST_ARCH),dos)
            HB_PRGFLAGS += -undef:__PLATFORM__DOS
         else
            ifeq ($(HB_HOST_ARCH),os2)
               HB_PRGFLAGS += -undef:__PLATFORM__OS2
            else
               ifeq ($(HB_HOST_ARCH),linux)
                  HB_PRGFLAGS += -undef:__PLATFORM__LINUX -undef:__PLATFORM__UNIX
               endif
            endif
         endif
      endif
      ifeq ($(HB_ARCHITECTURE),win)
         HB_PRGFLAGS += -D__PLATFORM__WINDOWS
         ifeq ($(HB_CPU),x86_64)
            HB_PRGFLAGS += -D__ARCH64BIT__
         else
            ifeq ($(HB_CPU),ia64)
               HB_PRGFLAGS += -D__ARCH64BIT__
            endif
         endif
      else
         ifeq ($(HB_ARCHITECTURE),wce)
            HB_PRGFLAGS += -D__PLATFORM__WINDOWS -D__PLATFORM__WINCE
         else
            ifeq ($(HB_ARCHITECTURE),dos)
               HB_PRGFLAGS += -D__PLATFORM__DOS
            else
               ifeq ($(HB_ARCHITECTURE),os2)
                  HB_PRGFLAGS += -D__PLATFORM__OS2
               else
                  ifeq ($(HB_ARCHITECTURE),linux)
                     HB_PRGFLAGS += -D__PLATFORM__LINUX -D__PLATFORM__UNIX
                  endif
               endif
            endif
         endif
      endif
   endif
endif

# Exclude Harbour-wide features prohibiting commercial usage
ifeq ($(HB_COMMERCE),yes)
   export HB_GPM_MOUSE := no
   export HB_WITHOUT_GTSLN := yes
endif

# Detect OpenSSL lib
ifeq ($(HB_HAS_OPENSSL),)
   HB_HAS_OPENSSL := no
   ifneq ($(HB_ARCHITECTURE),dos)
      ifneq ($(HB_COMPILER),watcom)
         ifeq ($(HB_INC_OPENSSL),)
            ifeq ($(HB_XBUILD),)
               HB_INC_OPENSSL := /usr/include /usr/local/ssl/include
            endif
         endif
         HB_INC_OPENSSL := $(strip $(foreach d,$(HB_INC_OPENSSL),$(if $(wildcard $(d)/openssl/ssl.h),$(d),)))
         ifneq ($(HB_INC_OPENSSL),)
            HB_HAS_OPENSSL := yes
            export HB_INC_OPENSSL
         endif
      endif
   endif
   export HB_HAS_OPENSSL
endif

# Detect GPM mouse lib
ifeq ($(HB_GPM_MOUSE),)
   HB_GPM_MOUSE := no
   ifeq ($(HB_INC_GPM),)
      HB_INC_GPM := /usr/include /usr/local/include
   endif
   ifneq ($(strip $(foreach d,$(HB_INC_GPM),$(if $(wildcard $(d)/gpm.h),$(d),))),)
      HB_GPM_MOUSE := yes
   endif
   export HB_GPM_MOUSE
endif

# Names of portable GT drivers
HB_GT_LIBS := \
   gtcgi \
   gtpca \
   gtstd \

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

ifneq ($(HB_HOST_ARCH),dos)
   HB_VERSION := 2.0.0beta2
   HB_PKGNAME := harbour-$(HB_VERSION)-$(HB_ARCHITECTURE)-$(HB_COMPILER)
   HB_PKGNAMI := $(HB_PKGNAME)
else
   # Use short names in MS-DOS
   HB_VERSION := 2b2
   HB_PKGNAME := hb$(HB_VERSION)
   # Ugly solution
   ifeq ($(HB_COMPILER),djgpp)
      HB_PKGNAME += dj
   else
      ifeq ($(HB_COMPILER),watcom)
         HB_PKGNAME += wa
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
      export HB_TOP := $(subst /,$(DIRSEP),$(realpath $(TOP)$(ROOT)))
      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(abspath $(PKG_DIR)/$(HB_PKGNAME)))

      HB_BIN_INSTALL :=
      HB_INC_INSTALL :=
      HB_LIB_INSTALL :=
      HB_DOC_INSTALL :=
   endif
else
   # Fill it automatically if not specified
   ifeq ($(HB_INSTALL_PREFIX),)

      ifeq ($(HB_OS_UNIX),no)
         HB_INSTALL_PREFIX := $(realpath $(TOP)$(ROOT))
      else
         ifneq ($(PREFIX),)
            HB_INSTALL_PREFIX := $(PREFIX)
         else
            ifneq ($(DESTDIR),)
               HB_INSTALL_PREFIX := $(DESTDIR)
            else
               # Stick to *nix customs. I don't like it, it needs admin.
               HB_INSTALL_PREFIX := /usr/local
            endif
         endif
      endif

      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))
   else
      # TOFIX: HB_INSTALL_PREFIX will have to be duplicated internally to avoid
      #        recursive operation here.

      # Handle simple macros in value
      HB_INSTALL_PREFIX := $(subst {HB_ARCH},$(HB_ARCHITECTURE),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {HB_COMP},$(HB_COMPILER),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {HB_CPU},$(HB_CPU),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst {HB_TOP},$(realpath $(TOP)$(ROOT)),$(HB_INSTALL_PREFIX))
      HB_INSTALL_PREFIX := $(subst /,$(DIRSEP),$(HB_INSTALL_PREFIX))
   endif
endif

export HB_INSTALL_PREFIX

ifneq ($(HB_INSTALL_PREFIX_ORI),$(HB_INSTALL_PREFIX))
   ifneq ($(MAKE_381),)
      $(info ! HB_INSTALL_PREFIX set to: $(HB_INSTALL_PREFIX))
   endif
endif

ifneq ($(HB_INSTALL_PREFIX),)

   ifeq ($(HB_OS_UNIX),no)
      LIBPOSTFIX := $(DIRSEP)$(subst /,$(DIRSEP),$(ARCH_COMP))
   else
      # Not perfect, please enhance it.
      ifneq ($(findstring /usr,$(HB_INSTALL_PREFIX)),)
         ifeq ($(findstring /usr/home,$(HB_INSTALL_PREFIX)),)
           LIBPOSTFIX := $(DIRSEP)harbour
           INCPOSTFIX := $(DIRSEP)harbour
         endif
      else
         ifneq ($(findstring /opt,$(HB_INSTALL_PREFIX)),)
            LIBPOSTFIX := $(DIRSEP)harbour
            INCPOSTFIX := $(DIRSEP)harbour
         endif
      endif
   endif

   ifeq ($(HB_BIN_INSTALL),)
      export HB_BIN_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)bin
   endif
   ifeq ($(HB_LIB_INSTALL),)
      export HB_LIB_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)lib$(LIBPOSTFIX)
   endif
   ifeq ($(HB_INC_INSTALL),)
      export HB_INC_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)include$(INCPOSTFIX)
   endif
   ifeq ($(HB_DOC_INSTALL),)
      # Don't set doc dir for *nix targets
      ifeq ($(HB_OS_UNIX),no)
         export HB_DOC_INSTALL := $(HB_INSTALL_PREFIX)$(DIRSEP)doc
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

HB_DYN_COMPILE := no

# export some variables to eliminate repeated setting in recursive calls
export HB_HOST_ARCH
export HB_HOST_CPU
export HB_HOST_BIN_DIR

# clear these options for an unambiguous Harbour enviornment
export HARBOUR :=
export HARBOURCMD :=
export CLIPPER :=
export CLIPPERCMD :=

# relevant only on non-*nix hosts where --print-directory is on by default
ifeq ($(findstring w,$(MAKEFLAGS)),)
   MKFLAGS := --no-print-directory
endif

export HB_INIT_DONE := yes

include $(TOP)$(ROOT)config/$(HB_ARCHITECTURE)/global.mk
include $(TOP)$(ROOT)config/globsh.mk

endif

endif # GLOBAL_CF_
