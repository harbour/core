#
# $Id$
#

ROOT = ../../

PRG_HEADERS = \

C_SOURCES = \
	hbziparc.c \
	hbcomprs.c \

CPP_SOURCES = \
	stdafx.cpp \
	ziparchive.cpp \
	zipautobuffer.cpp \
	zipcentraldir.cpp \
	zipcomp.cpp \
	zipcompatibility.cpp \
	zipexception.cpp \
	zipfile.cpp  \
	zipfileheader.cpp \
	zipmemfile.cpp \
	zipnew.cpp \
	zipplatformcomm.cpp \
	zipstorage.cpp \
	ZipString.cpp

ifeq ($(HB_ARCHITECTURE),linux)
CPP_SOURCES += \
	ZipPathComponent_lnx.cpp \
	ZipPlatform_lnx.cpp \

else
CPP_SOURCES += \
	ZipPathComponent_win.cpp \
	ZipPlatform_win.cpp \

endif

PRG_SOURCES= \

LIBNAME=hbziparch

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
