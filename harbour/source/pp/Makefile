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

# TOFIX: Convert to backslash *when* needed.
HB_VERHEADER=$(TOP)$(ROOT)include/hbverbld.h

ifeq ($(HB_ARCHITECTURE),dos)
   HB_CHANGELOG=../../../../CHANGE~1
else
   HB_CHANGELOG=../../../../ChangeLog
endif

ifneq ($(HB_PP_RULES),)
pptable.c : $(HB_PP_RULES)
	$(CP) $(subst /,$(DIRSEP),$<) $@
else
pptable.c : hbpp$(EXE_EXT)
	$(HB_PPGEN_PATH)/hbpp$(EXE_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(HB_CHANGELOG) -v$(HB_VERHEADER)
endif

ifneq ($(HB_BIN_INSTALL),)
INSTALL_OBJS = $(ARCH_DIR)/hbpp$(EXE_EXT)
INSTALL_DIR = $(HB_BIN_INSTALL)
HB_INSTALL_DEF=yes
include $(TOP)$(ROOT)config/install.cf
endif

install::
	$(INSTALL_RULE_LIBRARIES)

clean::
	$(RM) $(HB_VERHEADER)
