#
# $Id$
#

ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../
vpath %.prg ../

LIBNAME := hbtipssl

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
   smtpcli.prg   \
   thtml.prg \
   url.prg \

HB_PRGFLAGS += -DHB_HAS_OPENSSL
HB_INC_DEPEND := -I$(TOP)$(ROOT)contrib/hbssl

include $(TOP)$(ROOT)config/lib.mk
