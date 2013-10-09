/*
 * Harbour Project source code:
 * String matching functions
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbregex.h"

HB_BOOL hb_strMatchRegExp( const char * szString, const char * szPattern )
{
   PHB_REGEX pRegEx;

   HB_TRACE( HB_TR_DEBUG, ( "hb_strMatchRegExp(%s, %s)", szString, szPattern ) );

   pRegEx = hb_regexCompile( szPattern, strlen( szPattern ), HBREG_EXTENDED );
   if( pRegEx )
   {
      HB_BOOL fMatch;
      fMatch = hb_regexMatch( pRegEx, szString, strlen( szString ), HB_TRUE );
      hb_regexFree( pRegEx );
      return fMatch;
   }
   else
      return hb_strMatchWildExact( szString, szPattern );
}

/*
 * WildMatch( cPattern, cValue [, lExact] ) compares
 * cValue with cPattern, cPattern * may contain wildcard characters (?*)
 * When lExact is TRUE then it will check if whole cValue is covered by
 * cPattern else it will check if cPattern is a prefix of cValue
 */

/* NOTE: This function is compatible with sx_WildMatch(), except when
         the pattern is an empty string where hb_WildMatch() returns
         .T., while sx_WildMatch() returns .F. [vszakats] */

HB_FUNC( HB_WILDMATCH )
{
   const char * szPattern = hb_parc( 1 ),
              * szText = hb_parc( 2 );

   hb_retl( szText && szPattern &&
            ( hb_parl( 3 ) ? hb_strMatchWildExact( szText, szPattern ) :
                             hb_strMatchWild( szText, szPattern ) ) );
}

HB_FUNC( HB_WILDMATCHI )
{
   const char * szPattern = hb_parc( 1 ),
              * szText = hb_parc( 2 );

   hb_retl( szText && szPattern &&
            hb_strMatchCaseWildExact( szText, szPattern ) );
}

HB_FUNC( HB_FILEMATCH )
{
   const char * szText = hb_parc( 1 ),
              * szPattern = hb_parc( 2 );

   hb_retl( szText && szPattern &&
            hb_strMatchFile( szText, szPattern ) );
}
