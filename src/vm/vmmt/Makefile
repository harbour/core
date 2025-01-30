ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

vpath %.c ../
vpath %.prg ../

ifeq ($(HB_PLATFORM),win)
   ifneq ($(filter $(HB_COMPILER),mingw mingw64),)
      C_MAIN := mainwin.c
   else
      C_MAIN := mainstd.c mainwin.c
   endif
else
   ifeq ($(HB_PLATFORM),cygwin)
      C_MAIN := mainwin.c
   else
      ifeq ($(HB_PLATFORM),wce)
         C_MAIN := mainwin.c
      else
         ifeq ($(HB_PLATFORM),os2)
            C_MAIN := mainstd.c
         else
            C_MAIN := main.c
         endif
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
   strapi.c \
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
   pbyref.c \
   pcount.c \
   pvalue.c \
   pvaluehb.c \
   proc.c \
   procaddr.c \
   runner.c \
   vm.c \
   $(C_MAIN) \

PRG_SOURCES := \
   harbinit.prg \

LIBNAME := hbvmmt

HB_CFLAGS += -DHB_MT_VM

include $(TOP)$(ROOT)config/lib.mk
