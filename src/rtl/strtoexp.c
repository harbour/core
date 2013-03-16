/*
 * Harbour Project source code:
 * hb_StrToExp() function - convert string to valid macrocompiler expression
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

HB_FUNC( HB_STRTOEXP )
{
   const char * pszString = hb_parc( 1 );

   if( pszString )
   {
      HB_SIZE nLen = hb_parclen( 1 ), nRet, n, nQ = 0;
      int iType = 0;
      char ch, * pDst, * pszResult;

      for( n = 0; n < nLen; ++n )
      {
         switch( pszString[ n ] )
         {
            case '\\':
               ++nQ;
               break;
            case '"':
               ++nQ;
               iType |= 1;
               break;
            case '\'':
               iType |= 2;
               break;
            case ']':
               iType |= 4;
               break;
            case '\r':
            case '\n':
               iType |= 7;
               ++nQ;
               break;
            case '\0':
               iType |= 7;
               nQ += 3;
               break;
         }
      }
      if( iType == 7 || hb_parl( 2 ) )
      {
         nRet = nLen + 3 + nQ;
         pDst = pszResult = ( char * ) hb_xgrab( nRet + 1 );
         *pDst++ = 'e';
         *pDst++ = '"';
         for( n = 0; n < nLen; ++n )
         {
            ch = pszString[ n ];
            switch( ch )
            {
               case '\r':
                  *pDst++ = '\\';
                  *pDst++ = 'r';
                  break;
               case '\n':
                  *pDst++ = '\\';
                  *pDst++ = 'n';
                  break;
               case '\0':
                  *pDst++ = '\\';
                  *pDst++ = '0';
                  *pDst++ = '0' + ( ch >> 3 );
                  *pDst++ = '0' + ( ch & 7 );
                  break;
               case '\\':
               case '"':
                  *pDst++ = '\\';
               default:
                  *pDst++ = ch;
                  break;
            }
         }
         *pDst++ = '"';
      }
      else
      {
         nRet = nLen + 2;
         pDst = pszResult = ( char * ) hb_xgrab( nRet + 1 );
         if( ( iType & 1 ) == 0 )
            *pDst++ = ch = '"';
         else if( ( iType & 2 ) == 0 )
            *pDst++ = ch = '\'';
         else
         {
            *pDst++ = '[';
            ch = ']';
         }
         memcpy( pDst, pszString, nLen );
         pDst += nLen;
         *pDst++ = ch;
      }
      *pDst = '\0';
      hb_retclen_buffer( pszResult, nRet );
   }
}
