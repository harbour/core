#
# $Id$
#

ROOT = ../../

YACC_FLAGS = -p hb_comp

YACC_SOURCE=macro.y

YACC_HEADERS=\
	hbmacro.h \
	hbsetup.h \
	hbpcode.h \
	hbdefs.h \

#NOTE: You can pass additional parameters that control the speed/size
# ratio of generated flex scanner. These parameters are:
# -Cf   - fastest/biggest
# -CF
# -C    - in between
# -Cm
# -Ce
# -Cem  - slowest/smallest
# see Flex documentation for full set of switches
LEX_FLAGS = -Phb_comp -C

LEX_SOURCE=macro.l

LEX_HEADERS=\
	hbsetup.h \
	hberrors.h \
	hbdefs.h \
	hbmacro.h \

C_SOURCES=macro.c \

LIBNAME=macro

include $(TOP)$(ROOT)config/lib.cf
