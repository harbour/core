ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbdossrl

#HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   serial.c \

HB_BUILD_DOSSRL := no

ifeq ($(HB_PLATFORM),dos)
   ifeq ($(HB_COMPILER),djgpp)
      HB_BUILD_DOSSRL := yes
   endif
   ifeq ($(HB_COMPILER),watcom)
      HB_BUILD_DOSSRL := yes
   endif
endif

ifeq ($(HB_BUILD_DOSSRL),yes)
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
