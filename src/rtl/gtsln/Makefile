ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtsln

C_SOURCES := \
   gtsln.c \
   kbsln.c \
   mousesln.c \

# TODO: Add these to lib path list
# LDFLAGS := -L/usr/local/lib $(LDFLAGS)
# LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(LDFLAGS)

ifneq ($(HB_HAS_SLANG),)

   HB_CFLAGS += $(foreach d,$(HB_HAS_SLANG),-I$(d))

   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
