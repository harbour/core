#
# $Id$
#

ROOT = ../../

PRG_HEADERS = \

C_SOURCES = \
	hbziparc.c \
	hbcomprs.c \

CPP_SOURCES = \
	hbzipnew.cpp \
	hbzipcom.cpp \
	\
	Aes.cpp \
	BaseLibCompressor.cpp \
	Bzip2Compressor.cpp \
	DeflateCompressor.cpp \
	DirEnumerator.cpp \
	FileFilter.cpp \
	Hmac.cpp \
	RandomPool.cpp \
	Sha1.cpp \
	Wildcard.cpp \
	ZipAesCryptograph.cpp \
	ZipArchive.cpp \
	ZipAutoBuffer.cpp \
	ZipCentralDir.cpp \
	ZipCompatibility.cpp \
	ZipCompressor.cpp \
	ZipCrc32Cryptograph.cpp \
	ZipCryptograph.cpp \
	ZipException.cpp \
	ZipExtraData.cpp \
	ZipExtraField.cpp \
#	ZipFile_mfc.cpp \
	ZipFile_stl.cpp \
	ZipFileHeader.cpp \
	ZipMemFile.cpp \
	ZipPlatformComm.cpp \
	ZipStorage.cpp \
	ZipString.cpp \

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

ifeq ($(HB_ARCHITECTURE),linux)
CFLAGS := $(CFLAGS) -DZIP_ARCHIVE_LNX
endif

include $(TOP)$(ROOT)config/lib.cf
