#ifndef _HBCONF_H
#define _HBCONF_H

#include "hbapi.h"
#include "hbarc4.h"

#if defined( HB_FORCE_ARC4RANDOM )
#  define HAVE_ARC4RANDOM_BUF
#  define arc4random_buf hb_arc4random_buf
#endif

#if defined( HB_OS_WIN )
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  undef WIN32_LEAN_AND_MEAN
#  include "hbwinuni.h"
#endif
#if defined( HB_OS_WIN_CE ) && ! defined( _WINCE )
#  define _WINCE
#endif

/* 1234 = LITLE_ENDIAN, 4321 = BIG_ENDIAN */
#if defined( HB_LITTLE_ENDIAN )
#  define BYTEORDER 1234
#elif defined( HB_BIG_ENDIAN )
#  define BYTEORDER 4321
#else
#  error Unsupported machine byte order (endian).
#endif

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to specify how much context to retain around the current parse
   point. */
#define XML_CONTEXT_BYTES 1024

/* Define to make parameter entity parsing functionality available. */
#define XML_DTD 1

/* Define to make XML Namespaces functionality available. */
#define XML_NS 1

#endif /* _HBCONF_H */
