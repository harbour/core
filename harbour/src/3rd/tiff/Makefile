#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := tiff

HB_BUILD_WARN := no
HB_BUILD_MODE := c
HB_BUILD_UNICODE := no

C_SOURCES := \
   auxtif.c \
   codec.c \
   color.c \
   compress.c \
   dirinfo.c \
   dumpmode.c \
   error.c \
   extensio.c \
   fax3.c \
   fax3sm.c \
   getimage.c \
   jbig.c \
   jpeg.c \
   jpeg_12.c \
   luv.c \
   lzma.c \
   lzw.c \
   next.c \
   ojpeg.c \
   packbits.c \
   pixarlog.c \
   predict.c \
   print.c \
   strip.c \
   swab.c \
   thunder.c \
   tile.c \
   version.c \
   warning.c \

C_SOURCES += \
   close.c \
   dir.c \
   dirread.c \
   dirwrite.c \
   flush.c \
   open.c \
   read.c \
   write.c \

#  tif_ojpeg.c \

ifeq ($(HB_PLATFORM),win)
#  C_SOURCES += win32.c
#  HB_CFLAGS += -DUSE_WIN32_FILEIO
else ifeq ($(HB_PLATFORM_UNIX),yes)
   C_SOURCES += unix.c
endif

ifeq ($(filter $(HB_COMPILER),<>),)

   _DET_DSP_NAME := tiff
   _DET_VAR_INC_ := HB_INC_TIFF
   _DET_VAR_HAS_ := HB_HAS_TIFF
   _DET_FLT_PLAT :=
   _DET_FLT_COMP :=
   _DET_INC_DEFP := /usr/include
   _DET_INC_LOCL := src/3rd/tiff
   _DET_INC_HEAD := /tiff.h
   include $(TOP)$(ROOT)config/detfun.mk

   ifneq ($(HB_HAS_TIFF_LOCAL),)

      _DET_DSP_NAME := jpeg
      _DET_VAR_INC_ := HB_INC_JPEG
      _DET_VAR_HAS_ := HB_HAS_JPEG
      _DET_FLT_PLAT :=
      _DET_FLT_COMP :=
      _DET_INC_DEFP := /usr/include
      _DET_INC_LOCL := src/3rd/jpeg
      _DET_INC_HEAD := /jpeglib.h
      include $(TOP)$(ROOT)config/detfun.mk

      ifneq ($(HB_HAS_JPEG),)
         HB_CFLAGS += -DJPEG_SUPPORT
         HB_CFLAGS += $(foreach d,$(HB_HAS_JPEG),-I$(d))
      endif

      ifneq ($(HB_HAS_ZLIB),)
         HB_CFLAGS += -DZIP_SUPPORT
         HB_CFLAGS += $(foreach d,$(HB_HAS_ZLIB),-I$(d))
      endif

      include $(TOP)$(ROOT)config/lib.mk
   else
      HB_SKIP_REASON := unused
      include $(TOP)$(ROOT)config/none.mk
   endif
else
   HB_SKIP_REASON := compiler not supported
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://www.libtiff.org/
# VER    4.0.2
# URL    http://download.osgeo.org/libtiff/tiff-4.0.2.zip
# DIFF   tiff.dif
#
# MAP    COPYRIGHT                LICENSE
# MAP    libtiff/tif_aux.c        auxtif.c
# MAP    libtiff/tif_close.c      close.c
# MAP    libtiff/tif_codec.c      codec.c
# MAP    libtiff/tif_color.c      color.c
# MAP    libtiff/tif_compress.c   compress.c
# MAP    libtiff/tif_dir.c        dir.c
# MAP    libtiff/tif_dirinfo.c    dirinfo.c
# MAP    libtiff/tif_dirread.c    dirread.c
# MAP    libtiff/tif_dirwrite.c   dirwrite.c
# MAP    libtiff/tif_dumpmode.c   dumpmode.c
# MAP    libtiff/tif_error.c      error.c
# MAP    libtiff/tif_extension.c  extensio.c
# MAP    libtiff/tif_fax3.c       fax3.c
# MAP    libtiff/tif_fax3sm.c     fax3sm.c
# MAP    libtiff/tif_flush.c      flush.c
# MAP    libtiff/tif_getimage.c   getimage.c
# MAP    libtiff/tif_jbig.c       jbig.c
# MAP    libtiff/tif_jpeg.c       jpeg.c
# MAP    libtiff/tif_jpeg_12.c    jpeg_12.c
# MAP    libtiff/tif_luv.c        luv.c
# MAP    libtiff/tif_lzma.c       lzma.c
# MAP    libtiff/tif_lzw.c        lzw.c
# MAP    libtiff/tif_next.c       next.c
# MAP    libtiff/tif_ojpeg.c      ojpeg.c
# MAP    libtiff/tif_open.c       open.c
# MAP    libtiff/tif_packbits.c   packbits.c
# MAP    libtiff/tif_pixarlog.c   pixarlog.c
# MAP    libtiff/tif_predict.c    predict.c
# MAP    libtiff/tif_print.c      print.c
# MAP    libtiff/tif_read.c       read.c
# MAP    libtiff/tif_strip.c      strip.c
# MAP    libtiff/tif_swab.c       swab.c
# MAP    libtiff/tif_thunder.c    thunder.c
# MAP    libtiff/tif_tile.c       tile.c
# MAP    libtiff/tif_unix.c       unix.c
# MAP    libtiff/tif_version.c    version.c
# MAP    libtiff/tif_warning.c    warning.c
# MAP    libtiff/tif_win32.c      win32.c
# MAP    libtiff/tif_write.c      write.c
# MAP    libtiff/tif_zip.c        zip.c
# MAP    libtiff/t4.h             t4.h
# MAP    libtiff/tiff.h           tiff.h
# MAP    libtiff/tiffio.h         tiffio.h
# MAP    libtiff/tiffiop.h        tiffiop.h
# MAP    libtiff/tiffvers.h       tiffvers.h
# MAP    libtiff/tif_dir.h        dir.h
# MAP    libtiff/tif_fax3.h       fax3.h
# MAP    libtiff/tif_predict.h    predict.h
# MAP    libtiff/uvcode.h         uvcode.h
# MAP    libtiff/tiffconf.h.in    tiffconf.h
