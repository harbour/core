/*
 * $Id$
 */

#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ >= 1360 ) && ( __BORLANDC__ < 1400 ) ) /* for BCC 5.5 */
#define XRABPLY_TYPE64 __int64
#define XV64(v) ((xply_word) v ## ui64)
#endif

/* Define to 1 if you have the <dlfcn.h> header file. */
/* #undef HAVE_DLFCN_H */

/* Define to 1 if you have the `free' function. */
#define HAVE_FREE 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the `malloc' function. */
#define HAVE_MALLOC 1

/* Define to 1 if you have the `memchr' function. */
#define HAVE_MEMCHR 1

/* Define to 1 if you have the `memcmp' function. */
#define HAVE_MEMCMP 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define to 1 if you have the `realloc' function. */
#define HAVE_REALLOC 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strlen' function. */
#define HAVE_STRLEN 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#if defined( __BORLANDC__ ) 
#undef HAVE_UNISTD_H
#elif defined( _MSC_VER )
#undef HAVE_UNISTD_H
#else
#define HAVE_UNISTD_H 1
#endif
 
/* Name of package */
#define PACKAGE "libxdiff"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "libxdiff"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "libxdiff 0.23"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "libxdiff"

/* Define to the version of this package. */
#define PACKAGE_VERSION "0.23"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "0.23"

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #undef WORDS_BIGENDIAN */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */

#if defined( __GNUC__ )
#  define inline __inline__
#elif defined( _MSC_VER )
#  define inline _inline
#else
#  define inline
#endif

#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to empty if the keyword `volatile' does not work. Warning: valid
   code using `volatile' can become incorrect without. Disable with care. */
/* #undef volatile */
