/*
 * Harbour Project source code:
 *
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_REGEX_H_
#define HB_REGEX_H_

#include "hbapi.h"

#if defined( _HB_REGEX_INTERNAL_ )

#if defined( HB_HAS_PCRE )
#  include <pcre.h>
#  undef HB_POSIX_REGEX
#elif defined( HB_OS_UNIX )
#  include <sys/types.h>
#  include <regex.h>
#  define HB_POSIX_REGEX
#elif defined( __BORLANDC__ )
#  include <pcreposi.h>
#  define HB_POSIX_REGEX
#else
#  error pcre component required, but not available
#endif

typedef struct
{
   HB_BOOL     fFree;
   int         iFlags;
   int         iEFlags;
#if defined( HB_HAS_PCRE )
   pcre        * re_pcre;
#elif defined( HB_POSIX_REGEX )
   regex_t     reg;
#endif
} HB_REGEX;
typedef HB_REGEX * PHB_REGEX;

#if defined( HB_HAS_PCRE )
   #define HB_REGMATCH              int
   #define HB_REGMATCH_SIZE( n )    ( ( n ) * 3 )
   #define HB_REGMATCH_SO( p, n )   ( p )[ ( n ) * 2 ]
   #define HB_REGMATCH_EO( p, n )   ( p )[ ( n ) * 2 + 1 ]
#elif defined( HB_POSIX_REGEX )
   #define HB_REGMATCH              regmatch_t
   #define HB_REGMATCH_SIZE( n )    ( n )
   #define HB_REGMATCH_SO( p, n )   ( p )[ n ].rm_so
   #define HB_REGMATCH_EO( p, n )   ( p )[ n ].rm_eo
#else
   #define HB_REGMATCH              int
   #define HB_REGMATCH_SIZE( n )    ( ( n ) * 2 )
   #define HB_REGMATCH_SO( p, n )   ( p )[ ( n ) * 2 ]
   #define HB_REGMATCH_EO( p, n )   ( p )[ ( n ) * 2 + 1 ]
#endif

typedef void ( * HB_REG_FREE )( PHB_REGEX );
typedef int  ( * HB_REG_COMP )( PHB_REGEX, const char * );
typedef int  ( * HB_REG_EXEC )( PHB_REGEX, const char *, HB_SIZE, int, HB_REGMATCH * );

extern void hb_regexInit( HB_REG_FREE pFree, HB_REG_COMP pComp, HB_REG_EXEC pExec );
extern HB_BOOL hb_regexIs( PHB_ITEM pItem );

#ifndef REG_EXTENDED
#  define REG_EXTENDED  0x00
#endif
#ifndef REG_NOSUB
#  define REG_NOSUB     0x00
#endif

#else

typedef void * PHB_REGEX;

#endif /* _HB_REGEX_INTERNAL_ */

#define HBREG_ICASE     0x01
#define HBREG_NEWLINE   0x02
#define HBREG_NOTBOL    0x04
#define HBREG_NOTEOL    0x08
#define HBREG_EXTENDED  0x10
#define HBREG_NOSUB     0x20
#define HBREG_DOTALL    0x40

#ifndef REGEX_MAX_GROUPS
#  define REGEX_MAX_GROUPS 16
#endif

HB_EXTERN_BEGIN

extern HB_EXPORT PHB_REGEX hb_regexCompile( const char * szRegEx, HB_SIZE nLen, int iFlags );
extern HB_EXPORT PHB_REGEX hb_regexGet( PHB_ITEM pRegExItm, int iFlags );
extern HB_EXPORT void      hb_regexFree( PHB_REGEX pRegEx );
extern HB_EXPORT HB_BOOL   hb_regexMatch( PHB_REGEX pRegEx, const char * szString, HB_SIZE nLen, HB_BOOL fFull );

HB_EXTERN_END

#endif /* HB_REGEX_H_ */
