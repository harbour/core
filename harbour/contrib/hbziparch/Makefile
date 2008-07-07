#
# $Id$
#

ROOT = ../../

LIBNAME=hbziparch

ifeq ($(HB_WITH_ZIPARCH),)
ifeq ($(HB_ARCHITECTURE),w32)
   HB_WITH_ZIPARCH=yes
endif
ifeq ($(HB_ARCHITECTURE),linux)
   HB_WITH_ZIPARCH=yes
endif
endif

ifeq ($(HB_WITH_ZIPARCH),yes)

PRG_HEADERS = \

C_SOURCES = \
	hbziparc.c \
	hbxdirec.c \

CPP_SOURCES = \
	hbzipnew.cpp \
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
	ZipFile_stl.cpp \
	ZipFileHeader.cpp \
	ZipMemFile.cpp \
	ZipPlatformComm.cpp \
	ZipStorage.cpp \
	ZipString.cpp \

#	ZipFile_mfc.cpp \

ifeq ($(HB_ARCHITECTURE),w32)
CPP_SOURCES += \
	ZipPathComponent_win.cpp \
	ZipPlatform_win.cpp \

else
CPP_SOURCES += \
	ZipPathComponent_lnx.cpp \
	ZipPlatform_lnx.cpp \

endif

PRG_SOURCES= \

ifeq ($(HB_ARCHITECTURE),linux)
C_USR += -DZIP_ARCHIVE_LNX
endif

include $(TOP)$(ROOT)config/lib.cf

else
include $(TOP)$(ROOT)config/none.cf
endif
