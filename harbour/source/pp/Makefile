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

INSTALL_RULE_LIBRARIES := $(INSTALL_RULE)

ifneq ($(HB_PP_RULES),)
pptable.c : $(HB_PP_RULES)
	$(CP) $(subst /,$(DIRSEP),$<) $@
else
pptable.c : hbppgen$(EXE_EXT)
	$(HB_PPGEN_PATH)/hbppgen$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c -v$(TOP)$(ROOT)include/hbverbld.h
endif

ifneq ($(HB_BIN_INSTALL),)
INSTALL_OBJS = $(ARCH_DIR)/hbppgen$(EXE_EXT)
INSTALL_DIR = $(HB_BIN_INSTALL)
HB_INSTALL_DEF=yes
include $(TOP)$(ROOT)config/install.cf
endif

install::
	$(INSTALL_RULE_LIBRARIES)
