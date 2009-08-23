#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no
HB_BUILD_MODE := c

ifeq ($(HB_PLATFORM),wce)
   HB_CFLAGS += -D_WIN32_WCE
endif
ifeq ($(HB_PLATFORM),dos)
   # DJGPP and OpenWatcom in DOS aren't correctly recognized by SQLite,
   # so we're forcing the next best available option. This will cause missing
   # externals though. [vszakats]
   HB_CFLAGS += -DSQLITE_OS_OTHER
endif

LIBNAME := sqlite3

ifeq ($(HB_WITH_SQLITE3),)
   ifeq ($(HB_PLATFORM),win)
      HB_WITH_SQLITE3 := yes
   endif
   ifeq ($(HB_PLATFORM),wce)
      HB_WITH_SQLITE3 := yes
   endif
   ifeq ($(HB_PLATFORM),os2)
      HB_WITH_SQLITE3 := yes
   endif
   ifeq ($(HB_PLATFORM),dos)
      HB_WITH_SQLITE3 := yes
   endif
   ifeq ($(HB_COMPILER),mingwarm)
      HB_WITH_SQLITE3 := no
   endif
   ifeq ($(HB_COMPILER),poccarm)
      HB_WITH_SQLITE3 := no
   endif
   # NOTE: dos based watcom runs out of memory. [vszakats]
   ifeq ($(HB_HOST_PLAT)-$(HB_COMPILER),dos-watcom)
      HB_WITH_SQLITE3 := no
   endif
endif

ifeq ($(HB_WITH_SQLITE3),yes)

C_SOURCES := \
   sqlite3.c \

include $(TOP)$(ROOT)config/lib.mk

else
HB_SKIP_REASON := not supported or disabled
include $(TOP)$(ROOT)config/none.mk
endif
