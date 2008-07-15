#
# $Id$
#

ROOT = ../../

LIBNAME=hbsqlit3

ifeq ($(HB_WITHOUT_SQLIT3),)
ifeq ($(HB_ARCHITECTURE),dos)
HB_WITHOUT_SQLIT3=yes
endif
ifeq ($(HB_ARCHITECTURE),bsd)
HB_WITHOUT_SQLIT3=yes
endif
ifeq ($(HB_ARCHITECTURE),hpux)
HB_WITHOUT_SQLIT3=yes
endif
endif

ifneq ($(HB_WITHOUT_SQLIT3),yes)
C_SOURCES =\
	hbsqlit3.c \

PRG_HEADERS=\
	hbsqlit3.ch \

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)

else
include $(TOP)$(ROOT)config/none.cf
endif
