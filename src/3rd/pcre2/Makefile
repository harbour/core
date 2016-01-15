ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   chartabs.c \
   p2autopo.c \
   p2compil.c \
   p2config.c \
   p2contex.c \
   p2dfamat.c \
   p2error.c \
   p2jicomp.c \
   p2maktab.c \
   p2match.c \
   p2matchd.c \
   p2newlin.c \
   p2ord2ut.c \
   p2patter.c \
   p2serial.c \
   p2string.c \
   p2study.c \
   p2substi.c \
   p2substr.c \
   p2tables.c \
   p2ucd.c \
   p2validu.c \
   p2xclass.c \

LIBNAME := hbpcre2

ifneq ($(HB_HAS_PCRE2_LOCAL),)

   HB_CFLAGS += -DPCRE2_CODE_UNIT_WIDTH=8
   # only needed for win/wce, for other platforms this is noop
   HB_CFLAGS_STA := -DPCRE2_STATIC

   ifneq ($(filter $(HB_COMPILER),clang gcc mingw mingw64 sunpro),)
      ifeq ($(filter $(HB_PLATFORM)-$(HB_COMPILER),win-clang),)
         HB_CFLAGS += -DHAVE_INTTYPES_H
      endif
   endif

   ifneq ($(filter $(HB_COMPILER),mingw mingw64 msvc mvc64)$(filter $(HB_PLATFORM),darwin linux),)
      HB_CFLAGS += -DSUPPORT_JIT
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

# ORIGIN http://pcre.org/
# URL    https://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre2-10.21.tar.bz2
# DIFF   pcre2.dif
#
# MAP    LICENCE                               LICENSE.txt
# MAP    src/config.h.generic                  config.h
# MAP    src/pcre2.h.generic                   pcre2.h
# MAP    src/pcre2_internal.h                  pcre2ina.h
# MAP    src/pcre2_intmodedep.h                pcre2mod.h
# MAP    src/pcre2_ucp.h                       pcre2ucp.h
# MAP    src/pcre2_chartables.c.dist           chartabs.c
# MAP    src/pcre2_auto_possess.c              p2autopo.c
# MAP    src/pcre2_compile.c                   p2compil.c
# MAP    src/pcre2_config.c                    p2config.c
# MAP    src/pcre2_context.c                   p2contex.c
# MAP    src/pcre2_dfa_match.c                 p2dfamat.c
# MAP    src/pcre2_error.c                     p2error.c
# MAP    src/pcre2_jit_compile.c               p2jicomp.c
# MAP    src/pcre2_jit_match.c                 p2jimat.c
# MAP    src/pcre2_jit_misc.c                  p2jimisc.c
# MAP    src/pcre2_maketables.c                p2maktab.c
# MAP    src/pcre2_match.c                     p2match.c
# MAP    src/pcre2_match_data.c                p2matchd.c
# MAP    src/pcre2_newline.c                   p2newlin.c
# MAP    src/pcre2_ord2utf.c                   p2ord2ut.c
# MAP    src/pcre2_pattern_info.c              p2patter.c
# MAP    src/pcre2_serialize.c                 p2serial.c
# MAP    src/pcre2_string_utils.c              p2string.c
# MAP    src/pcre2_study.c                     p2study.c
# MAP    src/pcre2_substitute.c                p2substi.c
# MAP    src/pcre2_substring.c                 p2substr.c
# MAP    src/pcre2_tables.c                    p2tables.c
# MAP    src/pcre2_ucd.c                       p2ucd.c
# MAP    src/pcre2_valid_utf.c                 p2validu.c
# MAP    src/pcre2_xclass.c                    p2xclass.c
# MAP    src/sljit/sljitConfig.h               sjconf.h
# MAP    src/sljit/sljitConfigInternal.h       sjconfi.h
# MAP    src/sljit/sljitExecAllocator.c        sjexeca.c
# MAP    src/sljit/sljitLir.c                  sjlir.c
# MAP    src/sljit/sljitLir.h                  sjlir.h
# MAP    src/sljit/sljitNativeARM_32.c         sjarm32.c
# MAP    src/sljit/sljitNativeARM_64.c         sjarm64.c
# MAP    src/sljit/sljitNativeARM_T2_32.c      sjarmt2.c
# MAP    src/sljit/sljitNativeMIPS_32.c        sjmips32.c
# MAP    src/sljit/sljitNativeMIPS_64.c        sjmips64.c
# MAP    src/sljit/sljitNativeMIPS_common.c    sjmipsc.c
# MAP    src/sljit/sljitNativePPC_32.c         sjppc32.c
# MAP    src/sljit/sljitNativePPC_64.c         sjppc64.c
# MAP    src/sljit/sljitNativePPC_common.c     sjppcc.c
# MAP    src/sljit/sljitNativeSPARC_32.c       sjspar32.c
# MAP    src/sljit/sljitNativeSPARC_common.c   sjsparc.c
# MAP    src/sljit/sljitNativeTILEGX-encoder.c sjtilenc.c
# MAP    src/sljit/sljitNativeTILEGX_64.c      sjtil64.c
# MAP    src/sljit/sljitNativeX86_32.c         sjx8632.c
# MAP    src/sljit/sljitNativeX86_64.c         sjx8664.c
# MAP    src/sljit/sljitNativeX86_common.c     sjx86c.c
# MAP    src/sljit/sljitUtils.c                sjutils.c
