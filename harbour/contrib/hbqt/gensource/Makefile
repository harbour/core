#
# $Id$
#

ROOT = ../../

LIBNAME=hbqt

ifeq ($(HB_WITH_QT),)
HB_WITH_QT=yes
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITH_QT=no
endif
ifeq ($(HB_ARCHITECTURE),os2)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),mingw64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),owatcom)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),bcc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),pocc64)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),poccce)
HB_WITH_QT=no
endif
ifeq ($(HB_COMPILER),poccarm)
HB_WITH_QT=no
endif
endif

ifeq ($(HB_WITH_QT),yes)

ifeq ($(HB_INC_QT),)
ifeq ($(HB_XBUILD),)
HB_INC_QT = /usr/include/qt4 /Developer/qt/include
endif
endif

HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))

ifneq ($(strip $(HB_INC_QT_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d))

include Makefile_gen

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
else
include $(TOP)$(ROOT)config/none.cf
endif
