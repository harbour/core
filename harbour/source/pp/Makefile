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

HB_VERHEADER=$(TOP)$(ROOT)include/hbverbld.h

ifeq ($(HB_CHANGELOG),)
   ifeq ($(HB_ARCHITECTURE),dos)
   ifeq ($(HB_COMPILER),watcom)
   HB_CHANGELOG:=change~1
   endif
   endif
ifeq ($(HB_CHANGELOG),)
   HB_CHANGELOG:=$(wildcard $(TOP)$(ROOT)ChangeLog)
ifeq ($(HB_CHANGELOG),)
   HB_CHANGELOG:=$(wildcard $(TOP)$(ROOT)[Cc]hange[Ll]o)
ifeq ($(HB_CHANGELOG),)
   HB_CHANGELOG:=$(wildcard $(TOP)$(ROOT)[Cc]hange~1)
ifeq ($(HB_CHANGELOG),)
   HB_CHANGELOG:=$(wildcard $(TOP)$(ROOT)[Cc]hange~?)
ifeq ($(HB_CHANGELOG),)
   HB_CHANGELOG:=$(wildcard $(TOP)$(ROOT)[Cc]hang~??)
endif
endif
endif
endif
endif
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
	-$(RM) $(subst /,$(DIRSEP),$(HB_VERHEADER))
