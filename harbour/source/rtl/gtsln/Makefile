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
DUMMYA  := $(CFLAGS)
CFLAGS := -I/usr/include/slang -I/usr/local/include \
	  -I/usr/local/include/slang $(DUMMYA)

#DUMMYB  := $(LDFLAGS)
#LDFLAGS := -L/usr/lib/slang -L/usr/local/lib -L/usr/local/lib/slang $(DUMMYB)