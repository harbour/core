#
# $Id$
#

ROOT := ../../

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

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

install::
	$(INSTALL_RULE_HEADERS)
