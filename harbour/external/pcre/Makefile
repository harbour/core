#
# $Id$
#

ROOT := ../../

include $(TOP)$(ROOT)config/global.mk

# URL: http://sourceforge.net/projects/pcre/files/pcre/8.02/pcre-8.02.zip/download

HB_BUILD_WARN := no
HB_BUILD_MODE := c

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

ifneq ($(HB_HAS_PCRE_LOCAL),)

   HB_CFLAGS += -DHAVE_STDINT_H=0
   # only needed for win/wce, for other platforms this is noop
   HB_CFLAGS_STA := -DPCRE_STATIC

   ifneq ($(filter $(HB_COMPILER),bcc msvc msvc64 msvcia64 icc iccia64 djgpp),)
      HB_CFLAGS += -DHAVE_INTTYPES_H=0
   endif

   HB_CFLAGS += -DHAVE_CONFIG_H

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

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := unused
   include $(TOP)$(ROOT)config/none.mk
endif

# Use with 'hbrun <root>/config/ren_sfn.prg [F|T]'
#
# RENAME LICENCE    LICENCE
# RENAME config.h   config.h.generic
# RENAME pcre.h     pcre.h.generic
# RENAME pcreinal.h pcre_internal.h
# RENAME ucp.h      ucp.h
# RENAME chartabs.c pcre_chartables.c.dist
# RENAME pcrecomp.c pcre_compile.c
# RENAME pcreconf.c pcre_config.c
# RENAME pcredfa.c  pcre_dfa_exec.c
# RENAME pcreexec.c pcre_exec.c
# RENAME pcrefinf.c pcre_fullinfo.c
# RENAME pcreget.c  pcre_get.c
# RENAME pcreglob.c pcre_globals.c
# RENAME pcreinfo.c pcre_info.c
# RENAME pcremktb.c pcre_maketables.c
# RENAME pcrenewl.c pcre_newline.c
# RENAME pcreoutf.c pcre_ord2utf8.c
# RENAME pcreprni.h pcre_printint.src
# RENAME pcrerefc.c pcre_refcount.c
# RENAME pcrestud.c pcre_study.c
# RENAME pcretabs.c pcre_tables.c
# RENAME pcretryf.c pcre_try_flipped.c
# RENAME pcreucd.c  pcre_ucd.c
# RENAME pcrevutf.c pcre_valid_utf8.c
# RENAME pcrever.c  pcre_version.c
# RENAME pcrexcls.c pcre_xclass.c
