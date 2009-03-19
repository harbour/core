#
# $Id$
#

ROOT = ../../

LIBNAME=hbqt

ifneq ($(HB_ARCHITECTURE),dos)
ifneq ($(HB_ARCHITECTURE),os2)

ifeq ($(HB_INC_QT),)
ifeq ($(HB_XBUILD),)
HB_INC_QT = /usr/include/qt4 /Developer/qt/include
endif
endif

HB_INC_QT_OK += $(foreach d, $(HB_INC_QT), $(if $(wildcard $(d)/Qt/qglobal.h),$(d),))

ifneq ($(strip $(HB_INC_QT_OK)),)

HB_USER_CFLAGS += $(foreach d, $(HB_INC_QT_OK), -I$(d))

CPP_SOURCES=\
	hbqt_base.cpp \
	hbqt_qdialog.cpp \
	hbqt_qabstractbutton.cpp \
	hbqt_qabstractitemview.cpp \
	hbqt_qabstractprintdialog.cpp \
	hbqt_qwidget.cpp \

C_HEADERS=\
	hbqt.h \

#PRG_HEADERS=\
#	hbqt.ch \
#	hbqtextern.ch \

#PRG_SOURCES=\
#	xxx.prg \

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
else
include $(TOP)$(ROOT)config/none.cf
endif
