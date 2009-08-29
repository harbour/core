#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbtip

C_SOURCES := \
   encmthd.c \
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

PRG_HEADERS := \
   thtml.ch \
   tip.ch \

ifneq ($(HB_PLATFORM),dos)

   include $(TOP)$(ROOT)config/header.mk
   INSTALL_RULE_HEADERS := $(INSTALL_RULE)
   include $(TOP)$(ROOT)config/lib.mk

   ifneq ($(HB_HAS_OPENSSL),)
      DIRS := hbtipssl
      include $(TOP)$(ROOT)config/dir.mk
   endif

   install::
	$(INSTALL_RULE_HEADERS)

else
   include $(TOP)$(ROOT)config/none.mk
endif
