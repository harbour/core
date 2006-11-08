#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	pplib.c \
	pptable.c \
	ppcore.c \

LIBNAME=pp

LIBS=\
	common \

include $(TOP)$(ROOT)config/lib.cf

pptable.c : ppgen$(EXE_EXT)
	./ppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q
