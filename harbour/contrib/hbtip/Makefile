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
	popcln.prg \
	credent.prg \
	encoder.prg \
	encb64.prg \
	encqp.prg \
	encurl.prg \
	url.prg \
	client.prg \
	ftpcln.prg \
	smtpcln.prg	\
	httpcln.prg \
	mail.prg \
	cgi.prg \
	thtml.prg \
	sendmail.prg \
	sessid.prg \

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
