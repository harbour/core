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
#include "hbapicdp.h"

#if defined( HB_OS_UNIX ) && !defined( HB_NO_FNMATCH )
#  include <fnmatch.h>
#endif

#define HB_MAX_WILDPATTERN     256

static HB_BOOL hb_strMatchWildRaw( const char *szString, const char *szPattern,
                                   HB_BOOL fExact, HB_BOOL fCase )
{
   HB_BOOL fMatch = HB_TRUE, fAny = HB_FALSE;
   HB_SIZE pnBufPosP[ HB_MAX_WILDPATTERN ], pnBufPosV[ HB_MAX_WILDPATTERN ],
           nBufSize = HB_MAX_WILDPATTERN;
   HB_SIZE * nAnyPosP = pnBufPosP, * nAnyPosV = pnBufPosV,
           nSize, nLen, nAny, nPosP, nPosV;

   nPosP = nPosV = nAny = 0;
   nLen = strlen( szString );
   nSize = strlen( szPattern );
   while( nPosP < nSize || ( fExact && !fAny && nPosV < nLen ) )
   {
      if( nPosP < nSize && szPattern[ nPosP ] == '*' )
      {
         fAny = HB_TRUE;
         nPosP++;
      }
      else if( nPosV < nLen && nPosP < nSize &&
               ( szPattern[ nPosP ] == '?' ||
                 ( ! fCase ? szPattern[ nPosP ] == szString[ nPosV ] :
                   ( hb_charUpper( szPattern[ nPosP ] ) ==
                     hb_charUpper( szString[ nPosV ] ) ) ) ) )
      {
         if( fAny )
         {
            if( nAny >= nBufSize )
            {
               if( ( nBufSize <<= 1 ) == ( HB_MAX_WILDPATTERN << 1 ) )
               {
                  nAnyPosP = ( HB_SIZE * ) hb_xgrab( nBufSize * sizeof( HB_SIZE ) );
                  nAnyPosV = ( HB_SIZE * ) hb_xgrab( nBufSize * sizeof( HB_SIZE ) );
                  memcpy( nAnyPosP, pnBufPosP, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                  memcpy( nAnyPosV, pnBufPosV, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
               }
               else
               {
                  nAnyPosP = ( HB_SIZE * ) hb_xrealloc( nAnyPosP, nBufSize * sizeof( HB_SIZE ) );
                  nAnyPosV = ( HB_SIZE * ) hb_xrealloc( nAnyPosV, nBufSize * sizeof( HB_SIZE ) );
               }
            }
            nAnyPosP[ nAny ] = nPosP;
            nAnyPosV[ nAny ] = nPosV;
            nAny++;
            fAny = HB_FALSE;
         }
         nPosV++;
         nPosP++;
      }
      else if( fAny && nPosV < nLen )
      {
         nPosV++;
      }
      else if( nAny > 0 )
      {
         nAny--;
         nPosP = nAnyPosP[ nAny ];
         nPosV = nAnyPosV[ nAny ] + 1;
         fAny = HB_TRUE;
      }
      else
      {
         fMatch = HB_FALSE;
         break;
      }
   }
   if( nBufSize > HB_MAX_WILDPATTERN )
   {
      hb_xfree( nAnyPosP );
      hb_xfree( nAnyPosV );
   }
   return fMatch;
}

static HB_BOOL hb_strMatchWildCDP( const char *szString, const char *szPattern,
                                   HB_BOOL fExact, HB_BOOL fCase,
                                   PHB_CODEPAGE cdp )
{
   HB_BOOL fMatch = HB_TRUE, fAny = HB_FALSE;
   HB_SIZE pnBufPosP[ HB_MAX_WILDPATTERN ], pnBufPosV[ HB_MAX_WILDPATTERN ],
           nBufSize = HB_MAX_WILDPATTERN;
   HB_SIZE * nAnyPosP = pnBufPosP, * nAnyPosV = pnBufPosV,
           nSize, nLen, nAny, nPosP, nPosV;

   nPosP = nPosV = nAny = 0;
   nLen = strlen( szString );
   nSize = strlen( szPattern );
   while( nPosP < nSize || ( fExact && !fAny && nPosV < nLen ) )
   {
      if( nPosP < nSize && szPattern[ nPosP ] == '*' )
      {
         fAny = HB_TRUE;
         nPosP++;
         continue;
      }

      if( nPosV < nLen && nPosP < nSize )
      {
         HB_SIZE nPP = nPosP, nPV = nPosV;

         if( szPattern[ nPosP ] == '?' )
         {
            nPosP++;
            nPosV += hb_cdpTextPos( cdp, szString + nPosV, nLen - nPosV, 1 );
         }
         else if( fCase )
         {
            if( !hb_cdpCharCaseEq( cdp, szString, nLen, &nPosV,
                                        szPattern, nSize, &nPosP ) )
            {
               nPosV = nPV;
               nPosP = nPP;
            }
         }
         else
         {
            if( !hb_cdpCharEq( cdp, szString, nLen, &nPosV,
                                    szPattern, nSize, &nPosP ) )
            {
               nPosV = nPV;
               nPosP = nPP;
            }
         }
         if( nPP != nPosP )
         {
            if( fAny )
            {
               if( nAny >= nBufSize )
               {
                  if( ( nBufSize <<= 1 ) == ( HB_MAX_WILDPATTERN << 1 ) )
                  {
                     nAnyPosP = ( HB_SIZE * ) hb_xgrab( nBufSize * sizeof( HB_SIZE ) );
                     nAnyPosV = ( HB_SIZE * ) hb_xgrab( nBufSize * sizeof( HB_SIZE ) );
                     memcpy( nAnyPosP, pnBufPosP, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                     memcpy( nAnyPosV, pnBufPosV, HB_MAX_WILDPATTERN * sizeof( HB_SIZE ) );
                  }
                  else
                  {
                     nAnyPosP = ( HB_SIZE * ) hb_xrealloc( nAnyPosP, nBufSize * sizeof( HB_SIZE ) );
                     nAnyPosV = ( HB_SIZE * ) hb_xrealloc( nAnyPosV, nBufSize * sizeof( HB_SIZE ) );
                  }
               }
               nAnyPosP[ nAny ] = nPP;
               nAnyPosV[ nAny ] = nPosV;
               nAny++;
               fAny = HB_FALSE;
            }
            continue;
         }
      }

      if( fAny && nPosV < nLen )
      {
         nPosV += hb_cdpTextPos( cdp, szString + nPosV, nLen - nPosV, 1 );
      }
      else if( nAny > 0 )
      {
         nAny--;
         nPosP = nAnyPosP[ nAny ];
         nPosV = nAnyPosV[ nAny ];
         fAny = HB_TRUE;
      }
      else
      {
         fMatch = HB_FALSE;
         break;
      }
   }
   if( nBufSize > HB_MAX_WILDPATTERN )
   {
      hb_xfree( nAnyPosP );
      hb_xfree( nAnyPosV );
   }
   return fMatch;
}

HB_BOOL hb_strMatchWild( const char *szString, const char *szPattern )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp && HB_CDP_ISCHARIDX( cdp ) )
      return hb_strMatchWildCDP( szString, szPattern, HB_FALSE, HB_FALSE, cdp );
   else
      return hb_strMatchWildRaw( szString, szPattern, HB_FALSE, HB_FALSE );
}

HB_BOOL hb_strMatchWildExact( const char *szString, const char *szPattern )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp && HB_CDP_ISCHARIDX( cdp ) )
      return hb_strMatchWildCDP( szString, szPattern, HB_TRUE, HB_FALSE, cdp );
   else
      return hb_strMatchWildRaw( szString, szPattern, HB_TRUE, HB_FALSE );
}

HB_BOOL hb_strMatchCaseWildExact( const char *szString, const char *szPattern )
{
   PHB_CODEPAGE cdp = hb_vmCDP();

   if( cdp && HB_CDP_ISCHARIDX( cdp ) )
      return hb_strMatchWildCDP( szString, szPattern, HB_TRUE, HB_TRUE, cdp );
   else
      return hb_strMatchWildRaw( szString, szPattern, HB_TRUE, HB_TRUE );
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
