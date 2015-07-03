ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbmk2.prg \

RC_SOURCES := \
   hbmk2.rc

PRG_MAIN := hbmk2.prg

LIBS = $(HB_LIBS_MT_RDD)

HB_PRGFLAGS += -u -i$(TOP)

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
