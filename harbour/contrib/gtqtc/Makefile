#
# $Id$
#

ROOT = ../../

LIBNAME=gtqtc

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
ifeq ($(HB_COMPILER),watcom)
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
ifeq ($(HB_COMPILER),poccarm)
HB_WITH_QT=no
endif
endif

ifeq ($(HB_WITH_QT),yes)

ifeq ($(HB_INC_QT),)
ifeq ($(HB_XBUILD),)
HB_INC_QT = /usr/include/qt4 /usr/lib/qt4/include /usr/include /Developer/qt/include
endif
endif

HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))
ifeq ($(HB_ARCHITECTURE),darwin)
ifeq ($(strip $(HB_INC_QT_OK)),)
HB_INC_QT_OK = $(if $(wildcard /Library/Frameworks/QtCore.framework/Versions/4/Headers/QtCore),spec,)
ifeq ($(HB_INC_QT_OK),spec)
HB_USER_CFLAGS += -I/Library/Frameworks/QtCore.framework/Headers
HB_USER_CFLAGS += -I/Library/Frameworks/QtGui.framework/Headers
HB_USER_CFLAGS += -I/Library/Frameworks/QtNetwork.framework/Headers
HB_USER_CFLAGS += -I/Library/Frameworks/QtWebKit.framework/Headers
endif
endif
endif

ifneq ($(strip $(HB_INC_QT_OK)),)

ifneq ($(HB_QT_STATIC),)
LIBNAME=gtqtcs
HB_USER_CFLAGS += -DQT_NODLL
endif

ifneq ($(strip $(HB_INC_QT_OK)),spec)
HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d) -I$(d)/Qt -I$(d)/QtCore -I$(d)/QtGui -I$(d)/QtNetwork -I$(d)/QtWebKit)
endif

CPP_SOURCES=\
	gtqtc.cpp \
	moc_gtqtc.cpp \

C_HEADERS=\
	gtqtc.h \

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
