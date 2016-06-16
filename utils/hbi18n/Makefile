ROOT := ../../

PRG_SOURCES := \
   hbi18n.prg \

RC_SOURCES := \
   $(TOP)$(ROOT)package/harbour.rc

PRG_MAIN := hbi18n.prg

LIBS = $(HB_LIBS_ST_NORDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),mingw mingw64 gcc clang),)
   HB_LDFLAGS += -fno-lto
endif
