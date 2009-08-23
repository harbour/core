#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gttrm

C_SOURCES := \
   gttrm.c \

HB_WITH_GTTRM := yes
ifeq ($(HB_PLATFORM),win)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_PLATFORM),wce)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_PLATFORM),dos)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_PLATFORM),os2)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_COMPILER),cygwin)
   HB_WITH_GTTRM := yes
endif
ifeq ($(HB_COMPILER),djgpp)
   HB_WITH_GTTRM := yes
endif

ifeq ($(HB_WITH_GTTRM),yes)

ifeq ($(HB_GPM_MOUSE),yes)
   HB_CFLAGS += -DHB_HAS_GPM
   ifeq ($(HB_GPM_NOICE_DISABLE),yes)
      HB_CFLAGS += -DHB_GPM_NOICE_DISABLE
   endif
endif

include $(TOP)$(ROOT)config/lib.mk

else
include $(TOP)$(ROOT)config/none.mk
endif
