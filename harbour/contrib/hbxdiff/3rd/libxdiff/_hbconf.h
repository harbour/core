/*
 * $Id$
 */

#if ! defined( HB_CONFIG_H )
#define HB_CONFIG_H

/* Name of package */
#if ! defined( PACKAGE )
#  define PACKAGE             "libxdiff"
#endif /* #if !defined(PACKAGE) */

/* Define to the address where bugs reports for this package should be sent. */
#if ! defined( PACKAGE_BUGREPORT )
#  define PACKAGE_BUGREPORT   ""
#endif /* #if !defined(PACKAGE_BUGREPORT) */

/* Define to the full name of this package. */
#if ! defined( PACKAGE_NAME )
#  define PACKAGE_NAME        "libxdiff"
#endif /* #if !defined(PACKAGE_NAME) */

/* Define to the full name and version of this package. */
#if ! defined( PACKAGE_STRING )
#define PACKAGE_STRING        "libxdiff 0.23"
#endif /* #if !defined(PACKAGE_STRING) */

/* Define to the one symbol short name of this package. */
#if ! defined( PACKAGE_TARNAME )
#define PACKAGE_TARNAME       "libxdiff"
#endif /* #if !defined(PACKAGE_TARNAME) */

/* Define to the version of this package. */
#if ! defined( PACKAGE_VERSION )
#define PACKAGE_VERSION       "0.23"
#endif /* #if !defined(PACKAGE_VERSION) */

#include "hbdefs.h"
#include "hb_io.h"        /* include unistd.h */

/* Define to 1 if you have the `memchr'&Co function. */
#define HAVE_MEMCHR           1
#define HAVE_MEMCMP           1
#define HAVE_MEMCPY           1
#define HAVE_MEMSET           1
#define HAVE_STRLEN           1

/*
 * #define XRABPLY_TYPE64        __int64
 * #define XV64( v ) ( ( xply_word ) v##UI64 )
 */

/* for BCC 5.5 */
#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ >= 1360 ) && ( __BORLANDC__ < 1400 ) )
#  define XRABPLY_TYPE64        __int64
#  define XV64( v ) ( ( xply_word ) v##ui64 )
#endif

/* 
 * Define to 1 if your processor stores words with the most significant byte
 * first (like Motorola and SPARC, unlike Intel and VAX).
 */

/* #undef WORDS_BIGENDIAN */

/*
 * #if defined( HB_LITTLE_ENDIAN )
 * #  undef WORDS_BIGENDIAN
 * #elif defined( HB_BIG_ENDIAN )
 * #  define WORDS_BIGENDIAN 1
 * #else
 * #  error Unsupported machine byte order (endian).
 * #endif
 */

/* Define to empty if `const' does not conform to ANSI C. */

/* #undef const */

/* Define to `__inline__' or `__inline' if that's what the C compiler
 * calls it, or to nothing if 'inline' is not supported under any name. 
 */

#define inline _HB_INLINE_

/* 
 * Define to empty if the keyword `volatile' does not work. Warning: valid
 * code using `volatile' can become incorrect without. Disable with care.
 */

/* #undef volatile */

#endif /* HB_CONFIG_H */
