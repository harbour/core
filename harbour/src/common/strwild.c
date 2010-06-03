/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    wildcards / file match functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#if defined( HB_OS_UNIX ) && !defined( HB_NO_FNMATCH )
#  include <fnmatch.h>
#endif

#define HB_MAX_WILDPATTERN     256

HB_BOOL hb_strMatchWild( const char *szString, const char *szPattern )
{
   HB_BOOL fMatch = HB_TRUE, fAny = HB_FALSE;
   HB_SIZE pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
           ulBufSize = HB_MAX_WILDPATTERN;
   HB_SIZE * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
           ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = ( HB_SIZE ) strlen( szString );
   ulSize = ( HB_SIZE ) strlen( szPattern );
   while( i < ulSize )
   {
      if( szPattern[i] == '*' )
      {
         fAny = HB_TRUE;
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
                  ulAnyPosP = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
               }
               else
               {
                  ulAnyPosP = ( HB_SIZE * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( HB_SIZE ) );
               }
            }
            ulAnyPosP[ ulAny ] = i;
            ulAnyPosV[ ulAny ] = j;
            ulAny++;
            fAny = HB_FALSE;
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
         i = ulAnyPosP[ ulAny ];
         j = ulAnyPosV[ ulAny ] + 1;
         fAny = HB_TRUE;
      }
      else
      {
         fMatch = HB_FALSE;
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

HB_BOOL hb_strMatchWildExact( const char *szString, const char *szPattern )
{
   HB_BOOL fMatch = HB_TRUE, fAny = HB_FALSE;
   HB_SIZE pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
           ulBufSize = HB_MAX_WILDPATTERN;
   HB_SIZE * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
           ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = ( HB_SIZE ) strlen( szString );
   ulSize = ( HB_SIZE ) strlen( szPattern );
   while( i < ulSize || ( j < ulLen && !fAny ) )
   {
      if( i < ulSize && szPattern[i] == '*' )
      {
         fAny = HB_TRUE;
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
                  ulAnyPosP = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
               }
               else
               {
                  ulAnyPosP = ( HB_SIZE * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( HB_SIZE ) );
               }
            }
            ulAnyPosP[ ulAny ] = i;
            ulAnyPosV[ ulAny ] = j;
            ulAny++;
            fAny = HB_FALSE;
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
         i = ulAnyPosP[ ulAny ];
         j = ulAnyPosV[ ulAny ] + 1;
         fAny = HB_TRUE;
      }
      else
      {
         fMatch = HB_FALSE;
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

HB_BOOL hb_strMatchCaseWildExact( const char *szString, const char *szPattern )
{
   HB_BOOL fMatch = HB_TRUE, fAny = HB_FALSE;
   HB_SIZE pulBufPosP[ HB_MAX_WILDPATTERN ], pulBufPosV[ HB_MAX_WILDPATTERN ],
           ulBufSize = HB_MAX_WILDPATTERN;
   HB_SIZE * ulAnyPosP = pulBufPosP, * ulAnyPosV = pulBufPosV,
           ulSize, ulLen, ulAny, i, j;

   i = j = ulAny = 0;
   ulLen = ( HB_SIZE ) strlen( szString );
   ulSize = ( HB_SIZE ) strlen( szPattern );
   while ( i < ulSize || ( j < ulLen && !fAny ) )
   {
      if( i < ulSize && szPattern[i] == '*' )
      {
         fAny = HB_TRUE;
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
                  ulAnyPosP = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xgrab( ulBufSize * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosP, pulBufPosP, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                  memcpy( ulAnyPosV, pulBufPosV, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
               }
               else
               {
                  ulAnyPosP = ( HB_SIZE * ) hb_xrealloc( ulAnyPosP, ulBufSize * sizeof( HB_SIZE ) );
                  ulAnyPosV = ( HB_SIZE * ) hb_xrealloc( ulAnyPosV, ulBufSize * sizeof( HB_SIZE ) );
               }
            }
            ulAnyPosP[ ulAny ] = i;
            ulAnyPosV[ ulAny ] = j;
            ulAny++;
            fAny = HB_FALSE;
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
         i = ulAnyPosP[ ulAny ];
         j = ulAnyPosV[ ulAny ] + 1;
         fAny = HB_TRUE;
      }
      else
      {
         fMatch = HB_FALSE;
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

HB_BOOL hb_strMatchFile( const char * szString, const char * szPattern )
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
