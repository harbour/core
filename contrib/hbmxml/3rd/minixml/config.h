/*
 * "$Id: config.h.in 408 2010-09-19 05:26:46Z mike $"
 *
 * Configuration file for Mini-XML, a small XML-like file parsing library.
 *
 * Copyright 2003-2010 by Michael R Sweet.
 *
 * These coded instructions, statements, and computer programs are the
 * property of Michael R Sweet and are protected by Federal copyright
 * law.  Distribution and use rights are outlined in the file "COPYING"
 * which should have been included with this file.  If this file is
 * missing or damaged, see the license at:
 *
 *     http://www.minixml.org/
 */

/*
 * Include necessary headers...
 */

#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_WARNINGS

#include "hbdefs.h"
#include "hb_io.h"

#include "hbapi.h"
#define HAVE_SNPRINTF 1
#undef snprintf
#define snprintf hb_snprintf

#define HAVE_VSNPRINTF 1
#undef vsnprintf
#define vsnprintf hb_vsnprintf

#if defined ( _MSC_VER )
#define close      _close
#define open       _open
#define read       _read
/* #define snprintf   _snprintf */
#define strdup     _strdup
/* #define vsnprintf  _vsnprintf */
#define write      _write
#endif

/*
 * Version number...
 */

#define MXML_VERSION "Mini-XML v2.7"


/*
 * Inline function support...
 */

#define inline _HB_INLINE_


/*
 * Long long support...
 */

#ifndef __BORLANDC__
#define HAVE_LONG_LONG 1
#endif


/*
 * Do we have the snprintf() and vsnprintf() functions?
 */

/* #define HAVE_SNPRINTF 1 */
/* #define HAVE_VSNPRINTF 1 */


/*
 * Do we have the strXXX() functions?
 */

/* #define HAVE_STRDUP 1 */


/*
 * Do we have threading support?
 */

/* #undef HAVE_PTHREAD_H */


/*
 * Define prototypes for string functions as needed...
 */

#  ifdef __cplusplus
extern "C" {
#  endif /* __cplusplus */

#  ifndef HAVE_STRDUP
extern char	*_mxml_strdup(const char *);
#    if defined( strdup )
#      undef strdup
#    endif
#    define strdup _mxml_strdup
#  endif /* !HAVE_STRDUP */

extern char	*_mxml_strdupf(const char *, ...);
extern char	*_mxml_vstrdupf(const char *, va_list);

#  ifndef HAVE_SNPRINTF
extern int	_mxml_snprintf(char *, size_t, const char *, ...);
#    if defined( snprintf )
#      undef snprintf
#    endif
#    define snprintf _mxml_snprintf
#  endif /* !HAVE_SNPRINTF */

#  ifndef HAVE_VSNPRINTF
extern int	_mxml_vsnprintf(char *, size_t, const char *, va_list);
#    if defined( vsnprintf )
#      undef vsnprintf
#    endif
#    define vsnprintf _mxml_vsnprintf
#  endif /* !HAVE_VSNPRINTF */

#  ifdef __cplusplus
}
#  endif /* __cplusplus */
