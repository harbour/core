#
# $Id$
#

vpath %.c ../
vpath %.prg ../

ROOT := ../../../

include $(TOP)$(ROOT)config/global.cf

ifeq ($(HB_ARCHITECTURE),win)
   ifeq ($(HB_COMPILER),mingw)
      C_MAIN := mainwin.c
   else
      C_MAIN := mainstd.c mainwin.c
   endif
else
   ifeq ($(HB_ARCHITECTURE),wce)
      C_MAIN := mainwin.c
   else
      ifeq ($(HB_ARCHITECTURE),os2)
         C_MAIN := mainstd.c
      else
         C_MAIN := main.c
      endif
   endif
endif

ifeq ($(HB_HVM_ALL),yes)

C_SOURCES := \
	hvmall.c \

else

C_SOURCES := \
	hvm.c \
	arrays.c \
	classes.c \
	dynsym.c \
	codebloc.c \
	estack.c \
	extend.c \
	fm.c \
	garbage.c \
	hashes.c \
	itemapi.c \
	macro.c \
	memvars.c \
	set.c \
	thread.c \

endif

C_SOURCES += \
	arrayshb.c \
	asort.c \
	break.c \
	cmdarg.c \
	debug.c \
	dynlibhb.c \
	eval.c \
	evalhb.c \
	extrap.c \
	hashfunc.c \
	initexit.c \
	initsymb.c \
	memvclip.c \
	pcount.c \
	pvalue.c \
	proc.c \
	runner.c \
	vm.c \
	$(C_MAIN) \

PRG_SOURCES := \
	harbinit.prg \

LIBNAME := hbvmmt

ifeq ($(HB_USER_CFLAGS),$(subst -DHB_MT_VM,,$(HB_USER_CFLAGS)))
   HB_USER_CFLAGS += -DHB_MT_VM
endif

include $(TOP)$(ROOT)config/lib.cf
