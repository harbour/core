/*
 * Harbour Project source code:
 *    CT3 Number and bit manipulation functions:
 *       CToN(), NToC()
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "ct.h"

#if HB_VMLONG_MAX == INT32_MAX
#  define HB_CT3_STRICT32
#endif

HB_FUNC( CTON )
{
   const char * szNumber = hb_parc( 1 );
   int iBase = hb_parnidef( 2, 10 );

   if( szNumber && iBase >= 2 && iBase <= 36 )
   {
      HB_MAXUINT nValue = 0, nMax;
#ifdef HB_CT3_STRICT32
      nMax = UINT32_MAX;
#else
      HB_BOOL fStrict = HB_ISLOG( 3 );
      if( fStrict )
         nMax = UINT32_MAX;
      else
         nMax = UINT64_MAX;
#endif

      for( ;; )
      {
         int iDigit = ( HB_UCHAR ) *szNumber++;
         if( iDigit >= '0' && iDigit <= '9' )
            iDigit -= '0';
         else if( iDigit >= 'A' && iDigit <= 'Z' )
            iDigit -= 'A' - 10;
         else if( iDigit >= 'a' && iDigit <= 'z' )
            iDigit -= 'a' - 10;
         else
            break;
         if( iDigit >= iBase )
            break;
         if( nValue > ( nMax - iDigit ) / iBase )
         {
            nValue = 0;
            break;
         }
         nValue = nValue * iBase + iDigit;
      }

#ifdef HB_CT3_STRICT32
      /* test shows that this is exact CT3 behavior */
      if( ( HB_I32 ) nValue >= 0 || hb_parl( 3 ) )
         hb_retnl( ( HB_I32 ) nValue );
      else
         hb_retnd( ( HB_U32 ) nValue );
#else
      if( fStrict )
      {
         if( hb_parl( 3 ) )
            hb_retnint( ( HB_I32 ) nValue );
         else
            hb_retnint( ( HB_U32 ) nValue );
      }
      else if( ( HB_MAXINT ) nValue < 0 )
         hb_retnd( ( double ) nValue );
      else
         hb_retnint( nValue );
#endif
   }
   else
      hb_retni( 0 );
}

HB_FUNC( NTOC )
{
   char szBuffer[ 256 ], * pszResult = NULL;
   HB_MAXINT nValue = 0;
   int iBase = hb_parnidef( 2, 10 ), iLen = hb_parni( 3 ), i;

   if( iLen < 0 || iLen > ( int ) sizeof( szBuffer ) )
      iLen = sizeof( szBuffer );

   if( iBase >= 2 && iBase <= 36 && ct_numParam( 1, &nValue ) )
   {
      HB_MAXUINT uValue = ( HB_MAXUINT ) nValue;

      i = iLen == 0 ? ( int ) sizeof( szBuffer ) : iLen;
      do
      {
         if( --i < 0 )
            break;
         else
         {
            int iDigit = uValue % iBase;
            uValue /= iBase;
            iDigit += iDigit < 10 ? '0' : ( 'A' - 10 );
            szBuffer[ i ] = ( char ) iDigit;
         }
      }
      while( uValue != 0 );

      if( i >= 0 )
      {
         if( iLen == 0 )
            iLen = sizeof( szBuffer ) - i;
         else
         {
            const char * szPad = hb_parc( 4 );
            char cPad = szPad ? szPad[ 0 ] : ( char ) hb_parnidef( 4, ' ' );

            while( i > 0 )
               szBuffer[ --i ] = cPad;
         }
         pszResult = &szBuffer[ i ];
      }
   }
   if( pszResult == NULL )
   {
      if( iLen == 0 )
         iLen = 1;
      memset( szBuffer, '*', iLen );
      pszResult = szBuffer;
   }
   hb_retclen( pszResult, iLen );
}
