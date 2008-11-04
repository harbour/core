#
# $Id$
#

vpath %.c ../
vpath %.prg ../

ROOT = ../../../

ifeq ($(HB_ARCHITECTURE),w32)
	ifeq ($(HB_COMPILER),mingw)
		C_MAIN = mainwin.c
	else
		ifeq ($(HB_COMPILER),mingwce)
			C_MAIN = mainwin.c
		else
			C_MAIN = mainstd.c mainwin.c
		endif
	endif
else
	ifeq ($(HB_ARCHITECTURE),os2)
		C_MAIN = mainstd.c
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
	debugold.c   \
	dynlibhb.c   \
	dynsym.c     \
	codebloc.c   \
	estack.c     \
	eval.c       \
	evalhb.c     \
	extend.c     \
	extrap.c     \
	fm.c         \
	fmhb.c       \
	garbage.c    \
	hashes.c     \
	hashfunc.c   \
	hbmem.c      \
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
	set.c        \
	thread.c     \
	vm.c         \

PRG_SOURCES=\
	harbinit.prg \

LIBNAME=hbvmmt

ifeq ($(C_USR),$(subst -DHB_MT_VM,,$(C_USR)))
   C_USR += -DHB_MT_VM
endif

include $(TOP)$(ROOT)config/lib.cf
