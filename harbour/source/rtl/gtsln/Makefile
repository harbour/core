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
CFLAGS := -I/usr/include/slang -I/usr/local/include \
          -I/usr/local/include/slang -I/sw/include $(CFLAGS)

#LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(LDFLAGS)

ifeq ($(HB_GPM_MOUSE),yes)
CFLAGS += -DHAVE_GPM_H
endif

ifeq ($(HB_MULTI_GT),yes)
CFLAGS += -DHB_MULTI_GT
endif
