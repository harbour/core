#
# $Id$
#

ROOT := ../../

# NOTE: You can pass additional parameters that control the speed/size
#       ratio of generated flex scanner. These parameters are:
#
#       -Cf   - fastest/biggest
#       -CF
#       -C    - in between
#       -Cm
#       -Ce
#       -Cem  - slowest/smallest
#
#       see Flex documentation for full set of switches

#LEX_FLAGS := -Phb_macro -C
#LEX_SOURCE := macro.l
#LEX_HEADERS := \
#   hbsetup.h \
#   hberrors.h \
#   hbdefs.h \
#   hbmacro.h \

#YACC_FLAGS := -p hb_macro
YACC_SOURCE := macro.y
YACC_DEPEND := macrolex.c
YACC_HEADERS := \
   hbmacro.h \
   hbsetup.h \
   hbpcode.h \
   hbdefs.h \

C_SOURCES := \
   macroa.c \
   macrob.c \
   macrolex.c \

LIBNAME := hbmacro

include $(TOP)$(ROOT)config/lib.cf
