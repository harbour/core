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

ifneq ($(HB_HAS_PCRE_LOCAL),)
   HB_DYN_LIBS += hbpcre
endif
ifneq ($(HB_HAS_ZLIB_LOCAL),)
   HB_DYN_LIBS += hbzlib
endif

# Added only for hbpp
-include $(TOP)$(ROOT)config/$(HB_PLATFORM)/libs.mk

ifneq ($(__HB_BUILD_NOSYSLIB),)
   SYSLIBS := $(filter-out $(__HB_BUILD_NOSYSLIB),$(SYSLIBS))
endif

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
INSTALL_DIR := $(HB_INSTALL_LIB)

include $(TOP)$(ROOT)config/instsh.mk
INSTALL_RULE_LIBS := $(INSTALL_RULE)

ifneq ($(INSTALL_RULE_LIBS),)

install:: first
	$(INSTALL_RULE_LIBS)

endif


endif
endif
