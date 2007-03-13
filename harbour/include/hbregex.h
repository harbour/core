/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * 
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#if defined( __BORLANDC__ )
#  if __BORLANDC__ >= 0x550 && !defined( HB_PCRE_REGEX_BCC )
#     define HB_PCRE_REGEX_BCC
#  endif
#elif defined( OS_UNIX_COMPATIBLE ) && !defined( __WATCOMC__ )
#  if !defined( HB_POSIX_REGEX )
#     define HB_POSIX_REGEX
#  endif
#endif

#if defined( HB_PCRE_REGEX_BCC )
#  include <pcre.h>
#  include <pcreposi.h>
#  if !defined( HB_PCRE_REGEX )
#     define HB_PCRE_REGEX
#  endif
#elif defined( HB_PCRE_REGEX )
#  include <pcre/pcre.h>
#  include <pcre/pcreposix.h>
#elif defined( HB_POSIX_REGEX )
#  include <regex.h>
#else
#  undef _HB_REGEX_INTERNAL_
#endif

#endif /* _HB_REGEX_INTERNAL_ */

#if defined( _HB_REGEX_INTERNAL_ )

typedef struct
{
   regex_t     reg;
   regmatch_t  aMatches[1];
   BOOL        fFree;
   int         iCFlags;
   int         iEFlags;
} HB_REGEX;
typedef HB_REGEX * PHB_REGEX;

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

#ifndef REGEX_MAX_GROUPS
#  define REGEX_MAX_GROUPS 16
#endif

HB_EXTERN_BEGIN

extern HB_EXPORT PHB_REGEX hb_regexCompile( const char *szRegEx, ULONG ulLen, int iFlags );
extern HB_EXPORT PHB_REGEX hb_regexGet( PHB_ITEM pRegExItm, int iFlags );
extern HB_EXPORT void      hb_regexFree( PHB_REGEX pRegEx );
extern HB_EXPORT BOOL      hb_regexMatch( PHB_REGEX pRegEx, const char *szString, BOOL fFull );

HB_EXTERN_END

#endif /* HB_REGEX_H_ */
