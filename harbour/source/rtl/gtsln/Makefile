#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	gtsln.c \
	kbsln.c \
	mousesln.c \

LIBNAME=gtsln

include $(TOP)$(ROOT)config/lib.cf

# a workaround of a problem with different include directories 
# for slang header files between different unix distributions
ifeq ($(HB_LOCAL_SLN),yes)

CFLAGS := -I/usr/local/include -I/usr/local/include/slang $(CFLAGS)
LDFLAGS := -L/usr/local/lib $(LDFLAGS)

else

CFLAGS := -I/usr/include/slang -I/usr/local/include \
          -I/usr/local/include/slang -I/sw/include -I/opt/local/include $(CFLAGS)
LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(LDFLAGS)

endif


ifeq ($(HB_GPM_MOUSE),yes)
CFLAGS += -DHAVE_GPM_H
ifeq ($(HB_GPM_NOICE_DISABLE),yes)
  CFLAGS += -DHB_GPM_NOICE_DISABLE
endif
endif
