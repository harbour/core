#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	gtcrs.c \
	mousecrs.c \
	kbdcrs.c \

PRG_SOURCES=\
	keymap.prg \
	charmap.prg

PRG_HEADERS=\
	eterm.map \
	linux.map \
	debug.map \

LIBNAME=gtcrs

include $(TOP)$(ROOT)config/lib.cf

# a workaround of a problem with different include directories 
# for slang curses files between different unix distributions
ifeq ($(HB_NCURSES_FINK),yes)
  CFLAGS += -I/sw/include -I/sw/include/ncurses
else
  ifeq ($(HB_NCURSES_194),yes)
    CFLAGS += -I/usr/include/ncur194
    CFLAGS += -DHB_NCURSES_194
  else
    CFLAGS += -I/usr/include/ncurses
    ifeq ($(HB_GT_CRS_BCEHACK),yes)
      CFLAGS += -DHB_GT_CRS_BCEHACK
    endif
  endif
endif

ifeq ($(HB_GPM_MOUSE),yes)
  CFLAGS += -DHAVE_GPM_H
endif

ifeq ($(HB_MULTI_GT),yes)
  CFLAGS += -DHB_MULTI_GT
endif
