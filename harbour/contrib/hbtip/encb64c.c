/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( __TIP_BASE64_ENCODE )
{
   const char * cData = hb_parc( 1 );
   char * cRet;
   HB_ISIZ nLen = hb_parclen( 1 );
   HB_ISIZ nPos = 0, nPosRet = 0, nPosBlock = 0;
   HB_SIZE nFinalLen;
   HB_ISIZ nLineCount = 0;
   HB_UCHAR cElem, cElem1;
   HB_BOOL bExcept;

   if( ! cData )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL,
         HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   if( ! nLen )
   {
      hb_retc_null();
      return;
   }

   bExcept = hb_parl( 2 );

   /* we know exactly the renturned length. */
   nFinalLen = ( HB_SIZE ) ( ( nLen / 3 + 2 ) * 4 );
   /* add line termination padding, CRLF each 76 output bytes */
   nFinalLen += ( nFinalLen / 72 + 1 ) * 2;
   cRet = ( char * ) hb_xgrab( nFinalLen );

   while( nPos < nLen )
   {
      cElem = ( HB_UCHAR ) cData[ nPos ];
      /* NOT using trailing 0 here as some buggy 3dparty func
         will create strings without trailing 0. */

      nPosBlock++;

      switch( nPosBlock )
      {
         case 1:
            cElem = cElem >> 2;
            break;
         case 2:
            cElem1 = nPos < nLen - 1 ? ( HB_UCHAR ) cData[ nPos + 1 ] : 0;
            cElem = ( ( cElem & 0x3 ) << 4 ) | ( cElem1 >> 4 );
            nPos++;
            break;
         case 3:
            cElem1 = nPos < nLen - 1 ? ( HB_UCHAR ) cData[ nPos + 1 ] : 0;
            cElem = ( ( cElem & 0xF ) << 2 ) | ( cElem1 >> 6 );
            nPos++;
            break;
         case 4:
            cElem = cElem & 0x3f;
            nPos++;
            nPosBlock = 0;
            break;
      }

      if( cElem < 26 )
         cRet[ nPosRet++ ] = cElem + 'A';
      else if( cElem < 52 )
         cRet[ nPosRet++ ] = ( cElem - 26 ) + 'a';
      else if( cElem < 62 )
         cRet[ nPosRet++ ] = ( cElem - 52 ) + '0';
      else if( cElem == 62 )
         cRet[ nPosRet++ ] = '+';
      else
         cRet[ nPosRet++ ] = '/';

      if( ! bExcept )
      {
         ++nLineCount;
         /* RFC says to add a CRLF each 76 chars, but is pretty unclear about
            the fact of this 76 chars counting CRLF or not. Common
            practice is to limit line size to 72 chars */
         if( nLineCount == 72 )
         {
            cRet[ nPosRet++ ] = '\r';
            cRet[ nPosRet++ ] = '\n';
            nLineCount = 0;
         }
      }
   }

   switch( nPos % 3 )
   {
      case 1:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
      case 2:
         cRet[ nPosRet++ ] = '=';
         /* fallthrough */
   }

   /* RFC is not explicit, but CRLF SHOULD be added at bottom
      during encoding phase */
   if( ! bExcept )
   {
      cRet[ nPosRet++ ] = '\r';
      cRet[ nPosRet++ ] = '\n';
   }

   /* this function also adds a zero */
   hb_retclen_buffer( cRet, nPosRet );
}
