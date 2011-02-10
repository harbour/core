#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

# Assemble template lib list to help create a few common variations

BUILD_SHARED :=
ifeq ($(flavor LIBS),recursive)
   ifeq ($(HB_BUILD_SHARED),yes)
      BUILD_SHARED := yes
   endif
endif

ifeq ($(BUILD_SHARED),yes)
   HB_LIBS_TPL := \
      hbcplr \
      hbdebug \

   ifeq ($(HB_PLATFORM),cygwin)
      HB_LIBS_TPL += hbmainstd
   else
   ifneq ($(filter $(HB_PLATFORM),win wce),)
      ifneq ($(filter $(HB_COMPILER),mingw mingw64 mingwarm),)
         HB_LIBS_TPL += hbmainstd
      else
      ifeq ($(HB_COMPILER),watcom)
         HB_LDFLAGS += FILE $(LIB_DIR)/hbmainstd.lib
      else
         HB_LIBS_TPL += hbmainstd hbmainwin
      endif
      endif
   else
   ifeq ($(HB_PLATFORM),os2)
      ifeq ($(HB_COMPILER),watcom)
         HB_LDFLAGS += FILE $(LIB_DIR)/hbmainstd.lib
      else
         HB_LIBS_TPL += hbmainstd
      endif
   endif
   endif
   endif

   HB_LIBS_ST_RDD := $(HB_LIBS_TPL) $(HB_DYNLIB_ST)
   HB_LIBS_MT_RDD := $(HB_LIBS_TPL) $(HB_DYNLIB_ST)
   HB_LIBS_ST_NORDD := $(HB_LIBS_ST_RDD)
   HB_LIBS_MT_NORDD := $(HB_LIBS_ST_RDD)

   HB_LIBS_TPL :=
else
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

   ifneq ($(HB_HAS_PCRE_LOCAL),)
      HB_LIBS_TPL += hbpcre
   endif
   ifneq ($(HB_HAS_ZLIB_LOCAL),)
      HB_LIBS_TPL += hbzlib
   endif

   # Create a few common core lib lists
   _HB_RDD := \
      hbrdd \
      rddntx \
      rddnsx \
      rddcdx \
      rddfpt \
      hbsix \
      hbhsx \
      hbusrrdd \
      hbuddall

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

endif

HB_LINKING_RTL :=
HB_LINKING_VMMT :=

ifneq ($(filter hbrtl,$(LIBS)),)
   HB_LINKING_RTL := yes
   ifneq ($(filter hbvmmt,$(LIBS)),)
      HB_LINKING_VMMT := yes
   endif
endif

-include $(TOP)$(ROOT)config/$(HB_PLATFORM)/libs.mk

ifneq ($(HB_PLATFORM_UNIX),)
   ifeq ($(BUILD_SHARED),yes)
      SYSLIBS :=
      SYSLIBPATHS :=
   endif
endif

LIBS := $(HB_USER_LIBS) $(LIBS)

include $(TOP)$(ROOT)config/$(HB_PLATFORM)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk
include $(TOP)$(ROOT)config/res.mk

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
ifneq ($(RC),)
   ALL_OBJS += $(ALL_RC_OBJS)
endif

first:: dirbase descend

descend:: dirbase
	+@$(MK) $(MKFLAGS) -C $(OBJ_DIR) -f $(GRANDP)Makefile TOP=$(GRANDP) $(BIN_NAME)

vpath $(BIN_NAME) $(BIN_DIR)
$(BIN_NAME) : $(ALL_OBJS)
	$(LD_RULE)

INSTALL_FILES := $(BIN_FILE)
INSTALL_DIR := $(HB_INSTALL_BIN)
include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_BIN := $(INSTALL_RULE)

ifneq ($(INSTALL_RULE_BIN),)

install:: first
	$(INSTALL_RULE_BIN)

endif

endif
endif
