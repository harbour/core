#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no

C_SOURCES := \
   chartabs.c \
   pcrecomp.c \
   pcreconf.c \
   pcredfa.c \
   pcreexec.c \
   pcrefinf.c \
   pcreget.c \
   pcreglob.c \
   pcreinfo.c \
   pcremktb.c \
   pcrenewl.c \
   pcreoutf.c \
   pcrerefc.c \
   pcrestud.c \
   pcretabs.c \
   pcretryf.c \
   pcreucd.c \
   pcrever.c \
   pcrevutf.c \
   pcrexcls.c \

LIBNAME := hbpcre

ifeq ($(HB_WITH_PCRE),)
   ifneq ($(filter $(HB_PLATFORM),win wce os2 dos),)
      HB_WITH_PCRE := yes
   endif
endif

ifeq ($(HB_WITH_PCRE),yes)

   # workaround for problems in sunpro x86 PIC builds exploited by hbpcre library code
   ifeq ($(HB_COMPILER),sunpro)
      ifeq ($(HB_PLATFORM),sunos)
         ifeq ($(findstring sparc,$(shell isalist)),)
            HB_CFLAGS += -xbuiltin=%none
         endif
      else
         HB_CFLAGS += -xbuiltin=%none
      endif
   endif

   # suppress bcc warnings
   ifeq ($(HB_COMPILER),bcc)
      HB_CFLAGS += -w-use -w-csu -w-aus -w-sig
   endif

   HB_CFLAGS += -DHAVE_CONFIG_H

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := not supported or disabled
   include $(TOP)$(ROOT)config/none.mk
endif
