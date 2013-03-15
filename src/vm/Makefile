#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

ifeq ($(HB_HVM_ALL),)

   HB_HVM_ALL := yes
   ifeq ($(HB_COMPILER),watcom)
      ifeq ($(HB_BUILD_MODE),cpp)
         HB_HVM_ALL := no
      endif
   endif
   ifeq ($(HB_COMPILER),bcc)
      HB_HVM_ALL := no
   endif
   ifeq ($(HB_COMPILER),pocc)
      HB_HVM_ALL := no
   endif
   ifeq ($(HB_COMPILER),pocc64)
      HB_HVM_ALL := no
   endif
   ifeq ($(HB_COMPILER),poccarm)
      HB_HVM_ALL := no
   endif
   ifeq ($(HB_COMPILER),xcc)
      HB_HVM_ALL := no
   endif
   ifeq ($(HB_COMPILER),dmc)
      HB_HVM_ALL := no
   endif
   export HB_HVM_ALL
endif

DIRS :=

ifeq ($(HB_PLATFORM),win)
   ifneq ($(filter $(HB_COMPILER),mingw mingw64),)
      C_MAIN := mainwin.c
   else
      C_MAIN := mainstd.c mainwin.c
   endif
   DIRS := mainstd mainwin maindllh maindllp
else
   ifeq ($(HB_PLATFORM),cygwin)
      C_MAIN := mainstd.c
      DIRS := mainstd maindllh maindllp
   else
   ifeq ($(HB_PLATFORM),wce)
      C_MAIN := mainwin.c
      DIRS := mainstd mainwin maindllh maindllp
   else
      ifeq ($(HB_PLATFORM),os2)
         C_MAIN := mainstd.c
         DIRS := mainstd maindllh
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

LIBNAME := hbvm

ifneq ($(HB_MT),yes)
ifeq ($(HB_PLATFORM),dos)
ifneq ($(HB_COMPILER),djgpp)
ifneq ($(HB_COMPILER),watcom)
   HB_MT := no
endif
endif
endif
endif
ifneq ($(HB_MT),no)
   DIRS += vmmt
endif

include $(TOP)$(ROOT)config/lib.mk
ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
endif
