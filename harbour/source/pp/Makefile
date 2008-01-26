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

LIBNAME=hbpp

LIBS=\
	hbcommon \

ifeq ($(HB_PPGEN_PATH),)
   HB_PPGEN_PATH=.
endif

include $(TOP)$(ROOT)config/lib.cf

ifneq ($(HB_PP_RULES),)
pptable.c : $(HB_PP_RULES)
	$(CP) $(subst /,$(DIRSEP),$<) $@
else
pptable.c : hbppgen$(EXE_EXT)
	$(HB_PPGEN_PATH)/hbppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q
endif
