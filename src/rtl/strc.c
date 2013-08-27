/*
 * Harbour Project source code:
 * hb_StrDecodEscape() - decode string with \ escape sequences
 * hb_StrCDecode() - decode string using C compiler rules
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
#include "hbapiitm.h"
#include "hbapierr.h"

/* hb_StrDecodEscape( <cEscSeqStr> ) -> <cStr>
 * decode string with \ escape sequences
 */
HB_FUNC( HB_STRDECODESCAPE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      HB_SIZE nLen = hb_itemGetCLen( pText );
      if( nLen > 0 )
      {
         char * str = ( char * ) hb_xgrab( nLen + 1 );
         hb_xmemcpy( str, hb_itemGetCPtr( pText ), nLen + 1 );
         hb_strRemEscSeq( str, &nLen );
         hb_retclen_buffer( str, nLen );
      }
      else
         hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_StrCDecode( <cStr> [, @<lCont> ] ) -> <cResult> | NIL
 * decode string using C compiler rules
 * if second parameter <lCont> is passed by reference then it allows
 * to decode multiline strings. In such case <lCont> is set to .T.
 * if string ends with unclosed "" quoting.
 * Function returns decoded string or NIL on syntax error.
 */
HB_FUNC( HB_STRCDECODE )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      HB_SIZE nLen = hb_itemGetCLen( pText );
      HB_BOOL fCont = hb_parl( 2 );
      if( nLen > 0 )
      {
         const char * pszSrc = hb_itemGetCPtr( pText );
         char * pszDst = ( char * ) hb_xgrab( nLen + 1 );
         HB_SIZE nDst = 0, n;

         for( ;; )
         {
            if( ! fCont )
            {
               while( nLen && HB_ISSPACE( *pszSrc ) )
               {
                  ++pszSrc;
                  --nLen;
               }
               if( nLen && *pszSrc == '"' )
               {
                  ++pszSrc;
                  --nLen;
                  fCont = HB_TRUE;
               }
            }
            if( ! fCont || ! nLen )
               break;

            n = 0;
            while( n < nLen )
            {
               char c = pszSrc[ n ];
               if( c == '"' )
               {
                  fCont = HB_FALSE;
                  break;
               }
               pszDst[ nDst + n ] = c;
               if( ++n < nLen && c == '\\' )
               {
                  pszDst[ nDst + n ] = pszSrc[ n ];
                  ++n;
               }
            }
            if( n > 0 )
            {
               pszSrc += n;
               nLen -= n;
               hb_strRemEscSeq( pszDst + nDst, &n );
               nDst += n;
            }
            if( ! fCont )
            {
               ++pszSrc;
               --nLen;
            }
         }
         if( nLen == 0 && ( ! fCont || HB_ISBYREF( 2 ) ) )
         {
            hb_retclen_buffer( pszDst, nDst );
            hb_storl( fCont, 2 );
         }
         else
            hb_xfree( pszDst );
      }
      else if( fCont )
         hb_itemReturn( pText );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
