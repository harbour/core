#
# $Id$
#

ROOT := ../../

HB_BUILD_WARN := no

C_SOURCES := \
   _hbpcreg.c \
   chartabs.c \
   pcrecomp.c \
   pcreconf.c \
   pcredfa.c \
   pcreexec.c \
   pcrefinf.c \
   pcreget.c \
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

include $(TOP)$(ROOT)config/lib.mk
