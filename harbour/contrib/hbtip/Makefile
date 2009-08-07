#
# $Id$
#

ROOT = ../../

LIBNAME=hbtip

ifneq ($(HB_ARCHITECTURE),dos)

C_SOURCES = \
	encmthd.c \
	utils.c \

PRG_SOURCES= \
	cgi.prg \
	client.prg \
	credent.prg \
	encb64.prg \
	encoder.prg \
	encqp.prg \
	encurl.prg \
	ftpcln.prg \
	httpcln.prg \
	log.prg \
	mail.prg \
	popcln.prg \
	sendmail.prg \
	sessid.prg \
	smtpcln.prg	\
	thtml.prg \
	url.prg \

PRG_HEADERS= \
	thtml.ch \
	tip.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
