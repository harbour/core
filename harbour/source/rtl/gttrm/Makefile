#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

LIBNAME := gttrm

C_SOURCES := \
   gttrm.c \

HB_WITH_GTTRM := yes
ifeq ($(HB_ARCHITECTURE),win)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_ARCHITECTURE),wce)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_ARCHITECTURE),dos)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_ARCHITECTURE),os2)
   HB_WITH_GTTRM := no
endif
ifeq ($(HB_COMPILER),cygwin)
   HB_WITH_GTTRM := yes
endif
ifeq ($(HB_COMPILER),djgpp)
   HB_WITH_GTTRM := yes
endif

ifeq ($(HB_WITH_GTTRM),yes)

HB_INC_GPM_OK :=
ifneq ($(HB_GPM_MOUSE),no)
   ifneq ($(HB_COMMERCE),yes)
      HB_INC_GPM := /usr/include /usr/local/include
      HB_INC_GPM_OK += $(foreach d, $(HB_INC_GPM), $(if $(wildcard $(d)/gpm.h),$(d),))
   endif
endif

ifneq ($(strip $(HB_INC_GPM_OK)),)
   HB_USER_CFLAGS += -DHAVE_GPM_H
   ifeq ($(HB_GPM_NOICE_DISABLE),yes)
      HB_USER_CFLAGS += -DHB_GPM_NOICE_DISABLE
   endif
endif

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
