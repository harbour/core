#
# $Id$
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),win)
	ifeq ($(HB_COMPILER),mingw)
		C_MAIN = mainwin.c
		DIRS = mainstd
	else
		ifeq ($(HB_COMPILER),mingwce)
			C_MAIN = mainwin.c
			DIRS = mainstd mainwin
		else
			C_MAIN = mainstd.c mainwin.c
			DIRS = mainstd mainwin
		endif
	endif
else
	ifeq ($(HB_ARCHITECTURE),os2)
		C_MAIN = mainstd.c
	else
		C_MAIN = main.c
	endif
endif

ifeq ($(HB_ARCHITECTURE),win)
	ifeq ($(HB_COMPILER),bcc)
		DIRS += maindllh
	endif
	ifeq ($(HB_COMPILER),msvc)
		DIRS += maindllh
	endif
	ifeq ($(HB_COMPILER),msvc64)
		DIRS += maindllh
	endif
	ifeq ($(HB_COMPILER),msvcia64)
		DIRS += maindllh
	endif
	ifeq ($(HB_COMPILER),owatcom)
		DIRS += maindllh
	endif
endif

C_SOURCES=\
	hvm.c \
	arrays.c \
	arrayshb.c \
	asort.c \
	break.c \
	classes.c \
	cmdarg.c \
	debug.c \
	dynlibhb.c \
	dynsym.c \
	codebloc.c \
	estack.c \
	eval.c \
	evalhb.c \
	extend.c \
	extrap.c \
	fm.c \
	garbage.c \
	hashes.c \
	hashfunc.c \
	initexit.c \
	initsymb.c \
	itemapi.c \
	macro.c \
	$(C_MAIN) \
	memvars.c \
	memvclip.c \
	pcount.c \
	proc.c \
	pvalue.c \
	runner.c \
	set.c \
	thread.c \
	vm.c \

PRG_SOURCES=\
	harbinit.prg \

LIBNAME=hbvm

DIRS+= fm

HB_USER_CFLAGS:=$(subst -DHB_MT_VM,,$(HB_USER_CFLAGS))
ifneq ($(HB_ARCHITECTURE),dos)
   DIRS+= vmmt fmmt
endif

include $(TOP)$(ROOT)config/lib.cf
ifneq ($(DIRS),)
	include $(TOP)$(ROOT)config/dir.cf
endif
