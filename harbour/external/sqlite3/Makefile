#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := sqlite3

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   sqlite3.c \

# decide if it's supported at all
HB_SUPPORTED := yes
ifeq ($(HB_COMPILER),poccarm)
   HB_SUPPORTED := no
endif
# NOTE: old msvcarm can't cope with some PP directives. [vszakats]
ifeq ($(HB_COMPILER),msvcarm)
   ifneq ($(filter $(HB_COMPILER_VER),1200 1300 1310),)
      HB_SUPPORTED := no
   endif
endif
# NOTE: dos based watcom runs out of memory. [vszakats]
ifeq ($(HB_HOST_PLAT)-$(HB_COMPILER),dos-watcom)
   HB_SUPPORTED := no
endif
# NOTE: disable *nix builds on non-*nix platforms. [vszakats]
ifeq ($(HB_HOST_PLAT_UNIX),)
   ifneq ($(HB_PLATFORM_UNIX),)
      HB_SUPPORTED := no
   endif
endif

ifeq ($(HB_SUPPORTED),yes)

   _DET_DSP_NAME := sqlite3
   _DET_VAR_INC_ := HB_INC_SQLITE3
   _DET_VAR_HAS_ := HB_HAS_SQLITE3
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include
   _DET_INC_LOCL := external/sqlite3
   _DET_INC_HEAD := /sqlite3.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_SQLITE3_LOCAL),)

      ifeq ($(HB_PLATFORM),wce)
         HB_CFLAGS += -D_WIN32_WCE
      endif
      ifeq ($(HB_PLATFORM),dos)
         # DJGPP and OpenWatcom in DOS aren't correctly recognized by SQLite,
         # so we're forcing the next best available option. This will cause missing
         # externals though. [vszakats]
         HB_CFLAGS += -DSQLITE_OS_OTHER
      endif
      ifeq ($(HB_PLATFORM)-$(HB_COMPILER),linux-watcom)
         # Watcom Linux builds cannot use system header files
         HB_CFLAGS += -DSQLITE_OS_OTHER
      endif

      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := unused
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://www.sqlite.org/
# VER    3.7.2
# URL    http://www.sqlite.org/sqlite-amalgamation-3_7_2.zip
# DIFF   sqlite3.dif
#
# MAP    sqlite3.c
# MAP    sqlite3.h
