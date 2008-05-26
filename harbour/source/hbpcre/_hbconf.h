/*
 * $Id$
 */

#ifndef HB_CONFIG_H
#define HB_CONFIG_H

#define PCRE_STATIC

#if defined( _MSC_VER )
   #pragma warning( push, 0 )
#endif

#if defined( __BORLANDC__ )
   #pragma warn -use
   #pragma warn -csu
   #pragma warn -aus
#endif

#include "config.h"

#endif
