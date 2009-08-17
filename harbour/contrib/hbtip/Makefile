#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbtip

ifneq ($(HB_ARCHITECTURE),dos)

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

include $(TOP)$(ROOT)config/header.mk
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.mk

DIRS :=
ifeq ($(HB_HAS_OPENSSL),yes)
   DIRS += hbtipssl
endif

ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
endif

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.mk
endif
