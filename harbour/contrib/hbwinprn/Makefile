#
# $Id: Makefile 10777 2009-04-03 13:47:54Z vszakats $
#

ROOT = ../../

LIBNAME=hbwinprn

ifeq ($(HB_WITH_HBWINPRN),)
ifeq ($(HB_ARCHITECTURE),win)
HB_WITH_HBWINPRN=yes
endif
ifeq ($(HB_ARCHITECTURE),wce)
HB_WITH_HBWINPRN=yes
endif
endif

ifeq ($(HB_WITH_HBWINPRN),yes)

C_SOURCES = \
	win_prn1.c \
	win_prn2.c \

PRG_SOURCES = \
	win_tprn.prg \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
