ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gttrm

C_SOURCES := \
   gttrm.c \

HB_SUPPORTED := yes
ifeq ($(HB_PLATFORM),win)
   HB_SUPPORTED := no
endif
ifeq ($(HB_PLATFORM),wce)
   HB_SUPPORTED := no
endif
ifeq ($(HB_PLATFORM),dos)
   HB_SUPPORTED := no
endif
ifeq ($(HB_PLATFORM),os2)
   HB_SUPPORTED := no
endif
ifeq ($(HB_COMPILER),djgpp)
   HB_SUPPORTED := yes
endif
ifeq ($(HB_PLATFORM),vxworks)
   HB_SUPPORTED := no
endif
ifeq ($(HB_PLATFORM),symbian)
   HB_SUPPORTED := no
endif

ifeq ($(HB_SUPPORTED),yes)

   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
