#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
	C_MAIN := mainstd.c mainwin.c
else
	C_MAIN := main.c
endif

C_SOURCES=\
	arrays.c     \
	arrayshb.c   \
	asort.c      \
	break.c      \
	classes.c    \
	cmdarg.c     \
	debug.c      \
	dynsym.c     \
	codebloc.c   \
	estack.c     \
	eval.c       \
	extend.c     \
	fm.c         \
	garbage.c    \
	hvm.c        \
	initexit.c   \
	initsymb.c   \
	itemapi.c    \
	macro.c      \
	$(C_MAIN)    \
	memvars.c    \
	memvclip.c   \
	pcount.c     \
	proc.c       \
	pvalue.c     \
	runner.c     \

PRG_SOURCES=\
	harbinit.prg \

LIBNAME=vm

include $(TOP)$(ROOT)config/lib.cf
