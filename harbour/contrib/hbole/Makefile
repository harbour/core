#
# $Id$
#

ROOT = ../../

LIBNAME=hbole

ifeq ($(HB_WITH_HBOLE),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_HBOLE=yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_HBOLE=yes
endif
endif

ifeq ($(HB_WITH_HBOLE),yes)

C_SOURCES=\
	oleinit.c \
	olecore.c \

C_HEADERS = \
	hbwinole.h \

PRG_SOURCES=\
	oleauto.prg \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
