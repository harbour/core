ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbmk2.prg \

RC_SOURCES := \
   $(TOP)$(ROOT)package/harbour.rc

PRG_MAIN := hbmk2.prg

ifeq ($(__HB_MT),no)
   LIBS = $(HB_LIBS_ST_RDD)
else
   LIBS = $(HB_LIBS_MT_RDD)
endif

HB_PRGFLAGS += -u -i$(TOP)
HB_RCFLAGS += -D_HB_INCLUDE_ICON

# Not possible to override default entry
# function cleanly when using plain GNU Make
# to build, so we're using _APPMAIN(), which
# is better than Main()
HB_PRGFLAGS += -DHBMK_USE_APPMAIN

HB_PRGFLAGS += -DHBMK_WITH_BUILTIN_HEADERS_ALL

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),mingw mingw64 gcc clang),)
   HB_USER_CFLAGS := $(filter-out -flto,$(HB_USER_CFLAGS))
   HB_LDFLAGS += -fno-lto
endif
