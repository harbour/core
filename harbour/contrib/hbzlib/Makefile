#
# $Id$
#

ROOT = ../../

PRG_HEADERS = \


C_SOURCES = \
	zip.c \
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
	../../Linux/ZipPathComponent.cpp \
	../../Linux/ZipPlatform.cpp \

else
CPP_SOURCES += \
	zippathcomponent.cpp \
	zipplatform.cpp \

endif


PRG_SOURCES= \

LIBNAME=ziparchive

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

CFLAGS := $(CFLAGS) -I../../include -DZLIB_DLL 
ifeq ($(HB_ARCHITECTURE),w32)
CFLAGS += -DWIN32 -DASSERT
endif

install::
	$(INSTALL_RULE_HEADERS)
