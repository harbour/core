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
	hbnulrtl \

include $(TOP)$(ROOT)config/lib.cf

INSTALL_RULE_LIBRARIES := $(INSTALL_RULE)

HB_VERHEADER=$(TOP)$(ROOT)include/hbverbld.h

vpath hbpp$(EXE_EXT) $(EXE_DIR)
ifneq ($(HB_PP_RULES),)
pptable.c : $(HB_PP_RULES)
	$(CP) $(subst /,$(DIRSEP),$<) $@
else
pptable.c : hbpp$(EXE_EXT)
	$(HB_HOST_BIN_DIR)/hbpp$(HB_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -v$(HB_VERHEADER)
endif

ifneq ($(HB_BIN_INSTALL),)
INSTALL_FILES = $(EXE_DIR)/hbpp$(EXE_EXT)
INSTALL_DIR = $(HB_BIN_INSTALL)
HB_INSTALL_DEF=yes
include $(TOP)$(ROOT)config/install.cf
endif

install::
	$(INSTALL_RULE_LIBRARIES)

clean::
	-$(RM) $(subst /,$(DIRSEP),$(HB_VERHEADER))
	-$(RM) $(subst /,$(DIRSEP),$(EXE_DIR)/hbpp$(EXE_EXT))
