#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := xhb

C_SOURCES := \
   bkgtsks.c \
   cstructc.c \
   datesxhb.c \
   dbf2txt.c \
   dbgfxc.c \
   filestat.c \
   fparse.c \
   freadlin.c \
   hbcomprs.c \
   hbcrypt.c \
   hboutdbg.c \
   hbserv.c \
   hbsyslog.c \
   hbxml.c \
   txtline.c \
   xhbarr.c \
   xhbat.c \
   xhbcopyf.c \
   xhbenum.c \
   xhberrc.c \
   xhbfunc.c \
   xhbis.c \
   xhbmsgs.c \
   xhbqself.c \
   xhbsave.c \
   xhbscr.c \
   xhbtrim.c \
   xhbwith.c \
   xstrdel.c \

C_HEADERS := \
   hbcompat.h \
   xhb.h \

PRG_SOURCES := \
   arrayblk.prg \
   cstruct.prg \
   dbgfx.prg \
   decode.prg \
   dirrec.prg \
   dumpvar.prg \
   hblog.prg \
   hblognet.prg \
   hbstruct.prg \
   regexrpl.prg \
   sprintf.prg \
   stream.prg \
   traceprg.prg \
   trpc.prg \
   trpccli.prg \
   ttable.prg \
   txml.prg \
   xcstr.prg \
   xdbmodst.prg \
   xhbcomp.prg \
   xhberr.prg \
   xhbmt.prg \
   xhbver.prg \
   xthrow.prg \
   \
   hjwindow.prg \
   hterrsys.prg \
   htjlist.prg \
   htmutil.prg \
   tcgi.prg \
   tedit.prg \
   tfile.prg \
   tframe.prg \
   thtm.prg \

PRG_HEADERS := \
   hbcompat.ch \
   xhb.ch \

# force redetection
HB_HAS_ZLIB :=

_DET_DSP_NAME := zlib
_DET_VAR_INC_ := HB_INC_ZLIB
_DET_VAR_HAS_ := HB_HAS_ZLIB
_DET_FLT_PLAT :=
_DET_FLT_COMP :=
_DET_INC_DEFP := /usr/include
ifneq ($(filter $(HB_PLATFORM),win wce os2 dos),)
   _DET_INC_DEFP += $(TOP)$(ROOT)external/zlib
endif
_DET_INC_HEAD := /zlib.h

include $(TOP)$(ROOT)config/detfun.mk

ifneq ($(HB_HAS_ZLIB),)
   HB_CFLAGS += -DHB_HAS_ZLIB $(foreach d,$(HB_HAS_ZLIB),-I$(d))

   include $(TOP)$(ROOT)config/header.mk
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := $(_DET_RES_TEXT)
   include $(TOP)$(ROOT)config/none.mk
endif
