/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_VALTOSTR() function
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
#include "hbapiitm.h"

#ifdef HB_EXTENSION

HB_FUNC( HB_VALTOSTR )
{
   ULONG ulLen;
   BOOL bFreeReq;
   char * buffer = hb_itemString( hb_param( 1, HB_IT_ANY ), &ulLen, &bFreeReq );

   if( bFreeReq )
      hb_retclen_buffer( buffer, ulLen );
   else
      hb_retclen( buffer, ulLen );
}

HB_FUNC( HB_STRTOEXP )
{
   char * pszString = hb_parc( 1 );
   if( pszString )
   {
      ULONG ulLen = hb_parclen( 1 ), ulRet, ul;
      int iType = 0, iQ = 0;
      char ch, * pDst, * pszResult;

      for( ul = 0; ul < ulLen; ++ul )
      {
         if( pszString[ ul ] == '"' )
         {
            ++iQ;
            iType |= 1;
         }
         else if( pszString[ ul ] == '\\' )
            ++iQ;
         else if( pszString[ ul ] == '\'' )
            iType |= 2;
         else if( pszString[ ul ] == ']' )
            iType |= 4;
         else if( pszString[ ul ] == '\r' ||
                  pszString[ ul ] == '\n' ||
                  pszString[ ul ] == '\0' )
         {
            iType |= 7;
            iQ += 3;
         }
      }
      if( iType == 7 )
      {
         ulRet = ulLen + 3 + iQ;
         pDst = pszResult = ( char * ) hb_xgrab( ulRet + 1 );
         *pDst++ = 'e';
         *pDst++ = '"';
         for( ul = 0; ul < ulLen; ++ul )
         {
            ch = pszString[ ul ];
            if( ch == '"' )
            {
               *pDst++ = '\\';
               *pDst++ = '"';
            }
            else if( ch == '\\' )
            {
               *pDst++ = '\\';
               *pDst++ = '\\';
            }
            else if( ch == '\r' || ch == '\n' || ch == '\0' )
            {
               *pDst++ = '\\';
               *pDst++ = '0';
               *pDst++ = '0' + ( ch >> 3 );
               *pDst++ = '0' + ( ch & 7 );
            }
            else
               *pDst++ = ch;
         }
         *pDst++ = '"';
      }
      else
      {
         ulRet = ulLen + 2;
         pDst = pszResult = ( char * ) hb_xgrab( ulRet + 1 );
         if( ( iType & 1 ) == 0 )
            *pDst++ = ch = '"';
         else if( ( iType & 2 ) == 0 )
            *pDst++ = ch = '\'';
         else
         {
            *pDst++ = '[';
            ch = ']';
         }
         memcpy( pDst, pszString, ulLen );
         pDst += ulLen;
         *pDst++ = ch;
      }
      *pDst = '\0';
      hb_retclen_buffer( pszResult, ulRet );
   }
}

#endif

