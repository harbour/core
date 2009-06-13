/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * String matching functions
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbapi.h"
#include "hbregex.h"

#if defined( HB_OS_UNIX ) && !defined( HB_NO_FNMATCH )
#  include <fnmatch.h>
#endif

#define HB_MAX_WILDPATTERN     256

BOOL hb_strMatchWild( const char *szString, const char *szPattern )
{
   BOOL fMatch = TRUE, fAny = FALSE;
   ULONG pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
         ulBufSize = HB_MAX_WILDPATTERN;
   ULONG * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
         ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = strlen( szString );
   ulSize = strlen( szPattern );
   while( i < ulSize )
   {
      if( szPattern[i] == '*' )
      {
         fAny = TRUE;
         i++;
      }
      else if( j < ulLen && ( szPattern[i] == '?' || szPattern[i] == szString[j] ) )
      {
         if( fAny )
         {
            if( ulAny >= ulBufSize )
            {
               if( ( ulBufSize <<= 1 ) == ( HB_MAX_WILDPATTERN << 1 ) )
               {
                  ulAnyPosP = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
               }
               else
               {
                  ulAnyPosP = ( ULONG * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( ULONG ) );
               }
            }
            ulAnyPosP[ulAny] = i;
            ulAnyPosV[ulAny] = j;
            ulAny++;
            fAny = FALSE;
         }
         j++;
         i++;
      }
      else if( fAny && j < ulLen )
      {
         j++;
      }
      else if( ulAny > 0 )
      {
         ulAny--;
         i = ulAnyPosP[ulAny];
         j = ulAnyPosV[ulAny] + 1;
         fAny = TRUE;
      }
      else
      {
         fMatch = FALSE;
         break;
      }
   }
   if( ulBufSize > HB_MAX_WILDPATTERN )
   {
      hb_xfree( ulAnyPosP );
      hb_xfree( ulAnyPosV );
   }
   return fMatch;
}

BOOL hb_strMatchWildExact( const char *szString, const char *szPattern )
{
   BOOL fMatch = TRUE, fAny = FALSE;
   ULONG pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
         ulBufSize = HB_MAX_WILDPATTERN;
   ULONG * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
         ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = strlen( szString );
   ulSize = strlen( szPattern );
   while( i < ulSize || ( j < ulLen && !fAny ) )
   {
      if( i < ulSize && szPattern[i] == '*' )
      {
         fAny = TRUE;
         i++;
      }
      else if( j < ulLen && i < ulSize &&
               ( szPattern[i] == '?' || szPattern[i] == szString[j] ) )
      {
         if( fAny )
         {
            if( ulAny >= ulBufSize )
            {
               if( ( ulBufSize <<= 1 ) == ( HB_MAX_WILDPATTERN << 1 ) )
               {
                  ulAnyPosP = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
               }
               else
               {
                  ulAnyPosP = ( ULONG * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( ULONG ) );
               }
            }
            ulAnyPosP[ulAny] = i;
            ulAnyPosV[ulAny] = j;
            ulAny++;
            fAny = FALSE;
         }
         j++;
         i++;
      }
      else if( fAny && j < ulLen )
      {
         j++;
      }
      else if( ulAny > 0 )
      {
         ulAny--;
         i = ulAnyPosP[ulAny];
         j = ulAnyPosV[ulAny] + 1;
         fAny = TRUE;
      }
      else
      {
         fMatch = FALSE;
         break;
      }
   }
   if( ulBufSize > HB_MAX_WILDPATTERN )
   {
      hb_xfree( ulAnyPosP );
      hb_xfree( ulAnyPosV );
   }
   return fMatch;
}

BOOL hb_strMatchCaseWildExact( const char *szString, const char *szPattern )
{
   BOOL fMatch = TRUE, fAny = FALSE;
   ULONG pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
         ulBufSize = HB_MAX_WILDPATTERN;
   ULONG * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
         ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = strlen( szString );
   ulSize = strlen( szPattern );
   while ( i < ulSize || ( j < ulLen && !fAny ) )
   {
      if( i < ulSize && szPattern[i] == '*' )
      {
         fAny = TRUE;
         i++;
      }
      else if( j < ulLen && i < ulSize &&
               ( szPattern[i] == '?' ||
                 hb_charUpper( szPattern[i] ) == hb_charUpper( szString[j] ) ) )
      {
         if( fAny )
         {
            if( ulAny >= ulBufSize )
            {
               if( ( ulBufSize <<= 1 ) == ( HB_MAX_WILDPATTERN << 1 ) )
               {
                  ulAnyPosP = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xgrab( ulBufSize * sizeof( ULONG ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( ULONG ) );
               }
               else
               {
                  ulAnyPosP = ( ULONG * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( ULONG ) );
                  ulAnyPosV = ( ULONG * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( ULONG ) );
               }
            }
            ulAnyPosP[ulAny] = i;
            ulAnyPosV[ulAny] = j;
            ulAny++;
            fAny = FALSE;
         }
         j++;
         i++;
      }
      else if( fAny && j < ulLen )
      {
         j++;
      }
      else if( ulAny > 0 )
      {
         ulAny--;
         i = ulAnyPosP[ulAny];
         j = ulAnyPosV[ulAny] + 1;
         fAny = TRUE;
      }
      else
      {
         fMatch = FALSE;
         break;
      }
   }
   if( ulBufSize > HB_MAX_WILDPATTERN )
   {
      hb_xfree( ulAnyPosP );
      hb_xfree( ulAnyPosV );
   }
   return fMatch;
}

BOOL hb_strMatchRegExp( const char * szString, const char * szPattern )
{
   PHB_REGEX pRegEx;

   HB_TRACE(HB_TR_DEBUG, ("hb_strMatchRegExp(%s, %s)", szString, szPattern));

   pRegEx = hb_regexCompile( szPattern, strlen( szPattern ), HBREG_EXTENDED );
   if( pRegEx )
   {
      BOOL fMatch;
      fMatch = hb_regexMatch( pRegEx, szString, strlen( szString ), TRUE );
      hb_regexFree( pRegEx );
      return fMatch;
   }
   else
      return hb_strMatchWildExact( szString, szPattern );
}

BOOL hb_strMatchFile( const char * szString, const char * szPattern )
{
#if defined( HB_OS_UNIX )
#  if defined( HB_NO_FNMATCH )
   return hb_strMatchWildExact( szString, szPattern );
#  else
   return fnmatch( szPattern, szString, FNM_PERIOD | FNM_PATHNAME ) == 0;
#  endif
#else
   return hb_strMatchCaseWildExact( szString, szPattern );
#endif
}

/*
 * WildMatch( cPattern, cValue [, lExact] ) compares
 * cValue with cPattern, cPattern * may contain wildcard characters (?*)
 * When lExact is TRUE then it will check if whole cValue is covered by
 * cPattern else it will check if cPatern is a prefix of cValue
 */

/* NOTE: This function is compatible with sx_WildMatch(), except when
         the pattern is an empty string where hb_WildMatch() returns
         .T., while sx_WildMatch() returns .F. [vszakats] */

HB_FUNC( HB_WILDMATCH )
{
   hb_retl( ( ! HB_ISCHAR( 1 ) || ! HB_ISCHAR( 2 ) ) ? FALSE :
            hb_parl( 3 ) ? hb_strMatchWildExact( hb_parc( 2 ), hb_parc( 1 ) ) :
                           hb_strMatchWild( hb_parc( 2 ), hb_parc( 1 ) ) );
}

HB_FUNC( HB_WILDMATCHI )
{
   hb_retl( hb_strMatchCaseWildExact( hb_parcx( 2 ), hb_parcx( 1 ) ) );
}

HB_FUNC( HB_FILEMATCH )
{
   hb_retl( ( ! HB_ISCHAR( 1 ) || ! HB_ISCHAR( 2 ) ) ? FALSE :
            hb_strMatchFile( hb_parc( 1 ), hb_parc( 2 ) ) );
}
