
ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbpmcom

#HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   com.c \

SS_SOURCES := \

HB_BUILD_PMCOM := no

ifeq ($(HB_PLATFORM),dos)
   ifeq ($(HB_COMPILER),djgpp)
      C_SOURCES += irq_g.c
      SX_SOURCES += wrap_g.sx
      HB_BUILD_PMCOM := yes
   endif
   ifeq ($(HB_COMPILER),watcom)
      C_SOURCES += irq_ow.c
      C_SOURCES += wrap_ow.c
      HB_BUILD_PMCOM := yes
   endif
endif

ifeq ($(HB_BUILD_PMCOM),yes)

   HB_CFLAGS += -DDISABLE_TIMING=1
   #HB_CFLAGS += -DDISABLE_PREEMPTING=1
   #HB_CFLAGS += -D_DEBUG=1

   include $(TOP)$(ROOT)config/lib.mk

else
   HB_SKIP_REASON := platform or compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif
