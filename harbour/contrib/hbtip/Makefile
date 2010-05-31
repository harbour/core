#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbtip

C_SOURCES := \
   encb64c.c \
   encurlc.c \
   utils.c \

PRG_SOURCES := \
   cgi.prg \
   client.prg \
   credent.prg \
   encb64.prg \
   encoder.prg \
   encqp.prg \
   encurl.prg \
   ftpcli.prg \
   httpcli.prg \
   log.prg \
   mail.prg \
   popcli.prg \
   sendmail.prg \
   sessid.prg \
   smtpcli.prg \
   thtml.prg \
   url.prg \

PRG_HEADERS := \
   thtml.ch \
   tip.ch \

include $(TOP)$(ROOT)config/header.mk
include $(TOP)$(ROOT)config/lib.mk

ifneq ($(HB_HAS_OPENSSL),)
   DIRS := hbtipssl
   include $(TOP)$(ROOT)config/dir.mk
endif
