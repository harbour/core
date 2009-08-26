#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

HB_DYN_LIBS := \
   hbcommon \
   hbpp \
   hbrtl \
   hbmacro \
   hblang \
   hbcpage \
   hbpcre \
   hbzlib \
   hbextern \
   hbrdd \
   rddntx \
   rddnsx \
   rddcdx \
   rddfpt \
   hbsix \
   hbhsx \
   hbusrrdd \
   gtcgi \
   gtpca \
   gtstd \
   gtwvt \
   gtgui \
   gtwin \
   gtos2 \
   gttrm \
   gtcrs \
   gtsln \
   gtxwc \
   hbvm \
   hbvmmt \
   hbmaindllh

# Added only for hbpp
-include $(TOP)$(ROOT)config/$(HB_PLATFORM)/libs.mk
include $(TOP)$(ROOT)config/$(HB_PLATFORM)/$(HB_COMPILER).mk
include $(TOP)$(ROOT)config/c.mk
include $(TOP)$(ROOT)config/prg.mk

LIB_NAME := $(LIB_PREF)$(LIBNAME)$(LIB_EXT)

LIB_FILE := $(LIB_DIR)/$(LIB_NAME)

ALL_OBJS := $(ALL_C_OBJS) $(ALL_PRG_OBJS)

first:: dirbase descend

descend:: dirbase
	+@$(MK) $(MKFLAGS) -C $(OBJ_DIR) -f $(GRANDP)Makefile TOP=$(GRANDP) $(LIB_NAME)

vpath $(LIB_NAME) $(LIB_DIR)
$(LIB_NAME) : $(ALL_OBJS)
	$(AR_RULE)

INSTALL_FILES := $(LIB_FILE)
INSTALL_DIR := $(HB_LIB_INSTALL)

include $(TOP)$(ROOT)config/install.mk

endif
endif
