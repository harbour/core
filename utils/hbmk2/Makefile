ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

PRG_SOURCES := \
   hbmk2.prg \

RC_SOURCES := \
   hbmk2.rc

PRG_MAIN := hbmk2.prg

LIBS = $(HB_LIBS_MT_RDD)

HB_PRGFLAGS += -u -i$(TOP)

ifneq ($(HB_HAS_GPM),)
   HB_PRGFLAGS += -DHB_HAS_GPM
endif
ifneq ($(HB_HAS_WATT),)
   HB_PRGFLAGS += -DHB_HAS_WATT
endif

# Not possible to override default entry
# function cleanly when using plain GNU Make
# to build, so we're using _APPMAIN(), which
# is better than Main()
HB_PRGFLAGS += -DHBMK_USE_APPMAIN

HB_PRGFLAGS += -DHBMK_WITH_ALL_EMBEDDED_HEADERS

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),mingw mingw64 gcc clang),)
   HB_USER_CFLAGS := $(filter-out -flto,$(HB_USER_CFLAGS))
endif
