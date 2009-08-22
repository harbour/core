#
# $Id$
#

ROOT := ../../

C_SOURCES := \
   pptable.c \
   ppcore.c \
   pplib.c \
   pplib2.c \
   pplib3.c \

LIBNAME := hbpp

LIBS := \
   hbcommon \
   hbnortl \

include $(TOP)$(ROOT)config/lib.mk

ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

INSTALL_RULE_LIB := $(INSTALL_RULE)

HB_VERHEADER := $(TOP)$(ROOT)include/hbverbld.h

BIN_FILE := $(BIN_DIR)/hbpp$(BIN_EXT)

vpath hbpp$(BIN_EXT) $(BIN_DIR)

# NOTE: Leave the extra empty line after the rules.
pptable.c : hbpp$(BIN_EXT)
	$(HB_HOST_BIN_DIR)/hbpp$(HB_HOST_BIN_EXT) $(TOP)$(ROOT)include/hbstdgen.ch -opptable.c -q -c$(TOP)$(ROOT) -v$(HB_VERHEADER)
	-@$(RM) hbpp$(OBJ_EXT)
	$(if $(wildcard hbpp$(OBJ_DYN_POSTFIX)$(OBJ_EXT)),-@$(RM) hbpp$(OBJ_DYN_POSTFIX)$(OBJ_EXT),)


ifneq ($(HB_HOST_BUILD),lib)
   ifneq ($(HB_BIN_INSTALL),)
      INSTALL_FILES := $(BIN_DIR)/hbpp$(BIN_EXT)
      INSTALL_DIR := $(HB_BIN_INSTALL)
      HB_INSTALL_DEF := yes
      include $(TOP)$(ROOT)config/install.mk
   endif
endif

install::
	$(INSTALL_RULE_LIB)

clean::
	$(if $(wildcard $(HB_VERHEADER)), -@$(RM) $(subst /,$(DIRSEP),$(HB_VERHEADER)),)

endif
endif
