#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	pptable.c \
	ppcore.c  \
	pplib.c   \
	pplib2.c  \
	pplib3.c  \

LIBNAME=pp

LIBS=\
	common \

include $(TOP)$(ROOT)config/lib.cf

pptable.c : ppgen$(EXE_EXT)
	./ppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q
