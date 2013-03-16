
ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtxwc

C_SOURCES := \
   gtxwc.c \

ifneq ($(HB_HAS_X11),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_X11),-I$(d))

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
