#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),w32)
	ifeq ($(HB_COMPILER),mingw32)
		C_MAIN = mainwin.c
		DIRS = mainstd
	else
		C_MAIN = mainstd.c mainwin.c
	endif
else
	ifeq ($(HB_ARCHITECTURE),os2)
		C_MAIN = mainstd.c mainpm.c
	else
		C_MAIN = main.c
	endif
endif

C_SOURCES=\
	arrays.c     \
	arrayshb.c   \
	asort.c      \
	break.c      \
	classes.c    \
	cmdarg.c     \
	debug.c      \
	dynlibhb.c   \
	dynsym.c     \
	codebloc.c   \
	estack.c     \
	eval.c       \
	evalhb.c     \
	extend.c     \
	fm.c         \
	garbage.c    \
	hashes.c     \
	hashfunc.c   \
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
ifneq ($(DIRS),)
	include $(TOP)$(ROOT)config/dir.cf
endif
