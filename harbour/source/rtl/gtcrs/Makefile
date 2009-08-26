#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtcrs

C_SOURCES := \
   gtcrs.c \

ifneq ($(HB_HAS_CURSES),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_CURSES),-I$(d))

   ifeq ($(HB_GT_CRS_BCEHACK),yes)
      HB_CFLAGS += -DHB_GT_CRS_BCEHACK
   endif
   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
      ifeq ($(HB_GPM_NOICE_DISABLE),yes)
         HB_CFLAGS += -DHB_GPM_NOICE_DISABLE
      endif
   endif
   ifeq ($(HB_NCURSES_194),yes)
      HB_CFLAGS += -DHB_NCURSES_194
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   include $(TOP)$(ROOT)config/none.mk
endif
