/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HB_NUMTOHEX()
 *    HB_HEXTONUM()
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

HB_FUNC( HB_HEXTONUM )
{
   const char * szHex = hb_parc( 1 );

   if( szHex )
   {
      HB_MAXUINT ulNum = 0;

      while( *szHex == ' ' )
         szHex++;
      while( *szHex )
      {
         int iDigit;
         char c = *szHex++;
         if( c >= '0' && c <= '9' )
            iDigit = c - '0';
         else if( c >= 'A' && c <= 'F' )
            iDigit = c - ( 'A' - 10 );
         else if( c >= 'a' && c <= 'f' )
            iDigit = c - ( 'a' - 10 );
         else
         {
            ulNum = 0;
            break;
         }
         ulNum = ( ulNum << 4 ) + iDigit;
      }
      hb_retnint( ulNum );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_NUMTOHEX )
{
   HB_MAXUINT ulNum;
   int        iLen;
   HB_BOOL    fDefaultLen;
   char       ret[ 33 ];

   if( HB_ISNUM( 2 ) )
   {
      iLen = hb_parni( 2 );
      iLen = ( iLen < 1 ) ? 1 : ( ( iLen > 32 ) ? 32 : iLen );
      fDefaultLen = 0;
   }
   else
   {
      iLen = 32;
      fDefaultLen = 1;
   }

   if( HB_ISNUM( 1 ) )
      ulNum = hb_parnint( 1 );
   else if( HB_ISPOINTER( 1 ) )
      ulNum = ( HB_PTRDIFF ) hb_parptr( 1 );
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   ret[ iLen ] = '\0';
   do
   {
      int iDigit = ( int ) ( ulNum & 0x0F );
      ret[ --iLen ] = ( char ) ( iDigit + ( iDigit < 10 ? '0' : 'A' - 10 ) );
      ulNum >>= 4;
   }
   while( fDefaultLen ? ulNum != 0 : iLen != 0 );

   hb_retc( &ret[ iLen ] );
}

HB_FUNC( HB_STRTOHEX )
{
   const char * szStr = hb_parc( 1 ), * szSep = "";
   HB_SIZE nStr, nSep = 0;

   if( hb_pcount() > 1 )
   {
      szSep = hb_parc( 2 );
      nSep = hb_parclen( 2 );
   }

   if( ! szStr || ! szSep )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   nStr = hb_parclen( 1 );
   if( nStr )
   {
      HB_SIZE nDest = ( nStr << 1 ) + ( nStr - 1 ) * nSep;
      char * szDest, * szPtr;

      szPtr = szDest = ( char * ) hb_xgrab( nDest + 1 );
      do
      {
         HB_UCHAR uc = ( HB_UCHAR ) *szStr++, ud;
         ud = uc >> 4;
         *szPtr++ = ud + ( ud < 10 ? '0' : 'A' - 10 );
         ud = uc & 0x0F;
         *szPtr++ = ud + ( ud < 10 ? '0' : 'A' - 10 );
         if( --nStr && nSep )
         {
            memcpy( szPtr, szSep, nSep );
            szPtr += nSep;
         }
      }
      while( nStr );
      hb_retclen_buffer( szDest, nDest );
   }
   else
      hb_retc_null();
}

HB_FUNC( HB_HEXTOSTR )
{
   const char * szStr = hb_parc( 1 );
   HB_SIZE nStr;

   if( ! szStr )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   nStr = hb_parclen( 1 );
   if( nStr > 1 )
   {
      HB_SIZE nDest, ul;
      const char * szPtr;
      char * szDest;

      szPtr = szStr;
      ul = nStr;
      nDest = 0;
      do
      {
         char c = *szPtr++;
         if( ( c >= '0' && c <= '9' ) ||
             ( c >= 'A' && c <= 'F' ) ||
             ( c >= 'a' && c <= 'f' ) )
            ++nDest;
      }
      while( --ul );

      nDest >>= 1;
      if( nDest )
      {
         int iVal = 0x10;

         szDest = ( char * ) hb_xgrab( nDest + 1 );
         /* ul = 0; see above stop condition */
         do
         {
            char c = *szStr++;
            if( c >= '0' && c <= '9' )
               iVal += c - '0';
            else if( c >= 'A' && c <= 'F' )
               iVal += c - ( 'A' - 10 );
            else if( c >= 'a' && c <= 'f' )
               iVal += c - ( 'a' - 10 );
            else
               continue;

            if( iVal & 0x100 )
            {
               szDest[ ul++ ] = ( char ) iVal & 0xff;
               iVal = 0x1;
            }
            iVal <<= 4;
         }
         while( --nStr );

         hb_retclen_buffer( szDest, nDest );
         return;
      }
   }

   hb_retc_null();
}
