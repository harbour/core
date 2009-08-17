#
# $Id$
#

include $(TOP)$(ROOT)config/global.cf

ifneq ($(HB_ARCHITECTURE),)
ifneq ($(HB_COMPILER),)

# Assemble template lib list to help create a few common variations

# (have to use '=' operator here)
HB_LIBS_TPL = \
   hbextern \
   hbdebug \
   $(_HB_VM) \
   hbrtl \
   hblang \
   hbcpage \
   $(HB_GT_LIBS) \
   $(_HB_RDD) \
   hbrtl \
   $(_HB_VM) \
   hbmacro \
   hbcplr \
   hbpp \
   hbcommon

ifeq ($(findstring -DHB_PCRE_REGEX, $(HB_USER_CFLAGS)),)
   ifeq ($(findstring -DHB_POSIX_REGEX, $(HB_USER_CFLAGS)),)
      HB_LIBS_TPL += hbpcre
   endif
endif
ifeq ($(findstring -DHB_EXT_ZLIB, $(HB_USER_CFLAGS)),)
   HB_LIBS_TPL += hbzlib
endif

# Create a few common core lib lists
_HB_RDD := hbrdd $(HB_RDD_LIBS)
_HB_VM := hbvm
HB_LIBS_ST_RDD := $(HB_LIBS_TPL)
_HB_VM := hbvmmt
HB_LIBS_MT_RDD := $(HB_LIBS_TPL)
_HB_RDD := hbnulrdd
_HB_VM := hbvm
HB_LIBS_ST_NORDD := $(HB_LIBS_TPL)
_HB_VM := hbvmmt
HB_LIBS_MT_NORDD := $(HB_LIBS_TPL)

# Cleanup temp vars
HB_LIBS_TPL :=
_HB_RDD :=
_HB_VM :=

include $(TOP)$(ROOT)config/$(HB_ARCHITECTURE)/$(HB_COMPILER).cf
include $(TOP)$(ROOT)config/c.cf
include $(TOP)$(ROOT)config/prg.cf

BIN_NAME :=

ifneq ($(C_MAIN),)
   ifeq ($(BIN_NAME),)
      BIN_NAME := $(C_MAIN:.c=$(BIN_EXT))
   endif
endif

ifneq ($(PRG_MAIN),)
   ifeq ($(BIN_NAME),)
      BIN_NAME := $(PRG_MAIN:.prg=$(BIN_EXT))
   endif
endif

BIN_FILE := $(BIN_DIR)/$(BIN_NAME)

ALL_OBJS := $(ALL_C_OBJS) $(ALL_PRG_OBJS)

first:: dirbase descend

descend:: dirbase
	+@$(MK) $(MKFLAGS) -C $(OBJ_DIR) -f $(GRANDP)Makefile TOP=$(GRANDP) $(BIN_NAME)

vpath $(BIN_NAME) $(BIN_DIR)
$(BIN_NAME) : $(ALL_OBJS)
	$(LD_RULE)

INSTALL_FILES := $(BIN_FILE)
INSTALL_DIR := $(HB_BIN_INSTALL)

include $(TOP)$(ROOT)config/install.cf

endif
endif
