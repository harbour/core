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
	hbnortl \

include $(TOP)$(ROOT)config/lib.cf

ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

INSTALL_RULE_LIBRARIES := $(INSTALL_RULE)

HB_VERHEADER=$(TOP)$(ROOT)include/hbverbld.h

EXE_FILE := $(EXE_DIR)/hbpp$(EXE_EXT)
vpath hbpp$(EXE_EXT) $(EXE_DIR)

pptable.c : hbpp$(EXE_EXT)
	$(HB_HOST_BIN_DIR)/hbpp$(HB_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -v$(HB_VERHEADER)


ifneq ($(HB_BIN_INSTALL),)
INSTALL_FILES = $(EXE_DIR)/hbpp$(EXE_EXT)
INSTALL_DIR = $(HB_BIN_INSTALL)
HB_INSTALL_DEF=yes
include $(TOP)$(ROOT)config/install.cf
endif

install::
	$(INSTALL_RULE_LIBRARIES)

clean::
	-@$(RM) $(subst /,$(DIRSEP),$(HB_VERHEADER))
	-@$(RM) $(subst /,$(DIRSEP),$(EXE_DIR)/hbpp$(EXE_EXT))
	-@$(RM) $(subst /,$(DIRSEP),$(EXE_DIR)/hbpp.tds)

endif
endif
