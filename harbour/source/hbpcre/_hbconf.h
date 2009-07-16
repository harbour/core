/*
 * $Id$
 */

#ifndef HB_CONFIG_H
#define HB_CONFIG_H

#if !defined( HB_DYNLIB )
   #define PCRE_STATIC
#endif

#if defined( _MSC_VER )
   #pragma warning( push, 1 )
#endif

#if defined( __BORLANDC__ )
   #pragma warn -use
   #pragma warn -csu
   #pragma warn -aus
   #pragma warn -sig
#endif

#include "config.h"

#endif
