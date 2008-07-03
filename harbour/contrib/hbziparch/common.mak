#
# $Id$
#

LIBNAME = $(LIBPREF)hbziparch

LIB_PATH = $(LIB_DIR)$(LIBNAME)$(LIBEXT)

LIB_OBJS = \
    $(OBJ_DIR)hbziparc$(OBJEXT) \
    $(OBJ_DIR)hbxdirec$(OBJEXT) \
    \
    $(OBJ_DIR)hbzipnew$(OBJEXT) \
    \
    $(OBJ_DIR)Aes$(OBJEXT) \
    $(OBJ_DIR)BaseLibCompressor$(OBJEXT) \
    $(OBJ_DIR)Bzip2Compressor$(OBJEXT) \
    $(OBJ_DIR)DeflateCompressor$(OBJEXT) \
    $(OBJ_DIR)DirEnumerator$(OBJEXT) \
    $(OBJ_DIR)FileFilter$(OBJEXT) \
    $(OBJ_DIR)Hmac$(OBJEXT) \
    $(OBJ_DIR)RandomPool$(OBJEXT) \
    $(OBJ_DIR)Sha1$(OBJEXT) \
    $(OBJ_DIR)Wildcard$(OBJEXT) \
    $(OBJ_DIR)ZipAesCryptograph$(OBJEXT) \
    $(OBJ_DIR)ZipArchive$(OBJEXT) \
    $(OBJ_DIR)ZipAutoBuffer$(OBJEXT) \
    $(OBJ_DIR)ZipCentralDir$(OBJEXT) \
    $(OBJ_DIR)ZipCompatibility$(OBJEXT) \
    $(OBJ_DIR)ZipCompressor$(OBJEXT) \
    $(OBJ_DIR)ZipCrc32Cryptograph$(OBJEXT) \
    $(OBJ_DIR)ZipCryptograph$(OBJEXT) \
    $(OBJ_DIR)ZipException$(OBJEXT) \
    $(OBJ_DIR)ZipExtraData$(OBJEXT) \
    $(OBJ_DIR)ZipExtraField$(OBJEXT) \
#   $(OBJ_DIR)ZipFile_mfc$(OBJEXT) \
    $(OBJ_DIR)ZipFile_stl$(OBJEXT) \
    $(OBJ_DIR)ZipFileHeader$(OBJEXT) \
    $(OBJ_DIR)ZipMemFile$(OBJEXT) \
#   $(OBJ_DIR)ZipPathComponent_lnx$(OBJEXT) \
    $(OBJ_DIR)ZipPathComponent_win$(OBJEXT) \
#   $(OBJ_DIR)ZipPlatform_lnx$(OBJEXT) \
    $(OBJ_DIR)ZipPlatform_win$(OBJEXT) \
    $(OBJ_DIR)ZipPlatformComm$(OBJEXT) \
    $(OBJ_DIR)ZipStorage$(OBJEXT) \
    $(OBJ_DIR)ZipString$(OBJEXT) \

all: \
    $(LIB_PATH) \
