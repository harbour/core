#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbfbird

C_SOURCES := \
   firebird.c \

PRG_SOURCES := \
   tfirebrd.prg \

ifeq ($(filter $(HB_COMPILER),pocc64 poccarm),)

   ifneq ($(HB_INC_FIREBIRD),no)

      ifeq ($(HB_INC_FIREBIRD),)
         ifeq ($(HB_XBUILD),)
            ifneq ($(HB_PLATFORM_UNIX),)
               HB_INC_FIREBIRD := /usr/include /opt/firebird/include
            endif
         endif
      endif

      ifneq ($(HB_INC_FIREBIRD),)
         HB_INC_FIREBIRD_OK := $(strip $(foreach d, $(HB_INC_FIREBIRD), $(if $(wildcard $(d)/ibase.h),$(d),)))

         ifneq ($(HB_INC_FIREBIRD_OK),)

            HB_CFLAGS += $(foreach d,$(HB_INC_FIREBIRD_OK),-I$(d))

            include $(TOP)$(ROOT)config/lib.mk
         else
            HB_SKIP_REASON := component not found
            include $(TOP)$(ROOT)config/none.mk
         endif
      else
         HB_SKIP_REASON := component location not set
         include $(TOP)$(ROOT)config/none.mk
      endif
   else
      HB_SKIP_REASON := deselected
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
