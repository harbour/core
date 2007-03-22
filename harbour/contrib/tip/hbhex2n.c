/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xHarbour compatible functions: NUMTOHEX, HEXTONUM, STRTOHEX, HEXTOSTR
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
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
#include "hbapierr.h"

static HB_ULONG hb_hextonum( char * szHex )
{
   HB_ULONG ulNum = 0;
   int iDigit;

   while( * szHex == ' ' ) szHex++;
   while( * szHex )
   {
      iDigit = * szHex;
      if ( iDigit >= '0' && iDigit <= '9' )
         iDigit -= '0';
      else if ( iDigit >= 'A' && iDigit <= 'F' )
         iDigit -= 'A' - 10;
      else if ( iDigit >= 'a' && iDigit <= 'f' )
         iDigit -= 'a' - 10;
      else
      {
         ulNum = 0;
         break;
      }
      ulNum = ( ulNum << 4 ) + iDigit;
      szHex++;
   }
   return ulNum;
}

HB_FUNC( NUMTOHEX )
{
   HB_ULONG ulNum;
   char     ret[ 33 ];
   int      iDigit, iLen, iDefaultLen;

   if( ISNUM( 2 ) )
   {
      iLen = hb_parni( 2 );
      iLen = iLen < 1 ? 1 : ( iLen > 32 ? 32 : iLen );
      iDefaultLen = 0;
   }
   else
   {
      iLen = 32;
      iDefaultLen = 1;
   }
                    
   if( ISNUM( 1 ) )
      ulNum = hb_parnint( 1 );
   else if( ISPOINTER( 1 ) )
      ulNum = ( HB_PTRDIFF ) hb_parptr( 1 );
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   ret[ iLen ] = '\0';
   do
   {
      iDigit = ( int ) ( ulNum & 0x0F );
      ret[ --iLen ] = iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ); 
      ulNum >>= 4;
   }
   while( iDefaultLen ? ulNum > 0 : iLen > 0 );

   hb_retc( &ret[ iLen ] );
}

HB_FUNC( HEXTONUM )
{
   if( ISCHAR( 1 ) )
      hb_retnint( hb_hextonum( hb_parc( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( STRTOHEX )
{
   char * cStr = hb_parc( 1 );
   ULONG ulLen = hb_parclen( 1 );

   if( !cStr )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
   else if( ulLen == 0 )
      hb_retc( NULL );
   else
   {
      char * cSep = hb_parc( 2 ), * cOutBuf, * c;
      ULONG ulLenSep = hb_parclen( 2 ), ul;

      c = cOutBuf = ( char * ) hb_xgrab( ulLen * 2 + ( ulLen - 1 ) * ulLenSep + 1 );
      for( ul = 0; ul < ulLen; ul++ )
      {
         if( ulLenSep && ul )
         {
            memcpy( c, cSep, ulLenSep );
            c += ulLenSep;
         }
         c[ 0 ] = ( cStr[ ul ] >> 4 ) & 0x0F;
         c[ 0 ] += c[ 0 ] < 10 ? '0' : 'A' - 10;
         c[ 1 ] = cStr[ ul ] & 0x0F;
         c[ 1 ] += c[ 1 ] < 10 ? '0' : 'A' - 10;
         c += 2;
      }
      hb_retclen_buffer( cOutBuf, c - cOutBuf );
   }
}

HB_FUNC( HEXTOSTR )
{
   char * cStr = hb_parc( 1 );

   if( cStr )
   {
      ULONG ulLen = hb_parclen( 1 ), ulPos = 0, ul;
      char * cOutBuf = ( char * ) hb_xgrab( ( ulLen >> 1 ) + 1 ), c;
      int iByte = 0, iFirst = 0;

      for( ul = 0; ul < ulLen; ul++ )
      {
         c = *cStr++;
         if( c >= '0' && c <= '9' )
            iByte = ( iByte << 4 ) + c - '0';
         else if( c >= 'A' && c <= 'F' )
            iByte = ( iByte << 4 ) + c - 'A' + 10;
         else if( c >= 'a' && c <= 'f' )
            iByte = ( iByte << 4 ) + c - 'a' + 10;
         else 
           continue;
         iFirst ^= 1;
         if( !iFirst )
         {
           cOutBuf[ ulPos++ ] = ( char ) iByte;
           iByte = 0;
         }
      }
      hb_retclen_buffer( cOutBuf, ulPos );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
}
