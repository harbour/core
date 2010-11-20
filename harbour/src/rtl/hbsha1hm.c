/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HMAC-SHA1 Harbour wrappers.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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

#include "sha1hmac.h"

HB_FUNC( HB_HMAC_SHA1 )
{
   PHB_ITEM pKey = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );

   if( pKey && pBuffer )
   {
      char * buffer = hb_itemGetC( pBuffer );
      unsigned char mac[ HMAC_SHA1_DIGEST_LENGTH ];
      HMAC_SHA1_CTX ctx;

      hb_HMAC_SHA1_Init( &ctx );
      #if HB_SIZE_MAX > UINT_MAX
      {
         HB_SIZE nCount = hb_itemGetCLen( pBuffer );
         HB_SIZE nDone = 0;

         while( nCount )
         {
            unsigned int uiChunk;

            if( nCount > ( HB_SIZE ) UINT_MAX )
            {
               uiChunk = UINT_MAX;
               nCount -= ( HB_SIZE ) uiChunk;
            }
            else
            {
               uiChunk = ( unsigned int ) nCount;
               nCount = 0;
            }

            hb_HMAC_SHA1_UpdateKey( &ctx, buffer + nDone, uiChunk );

            nDone += ( HB_SIZE ) uiChunk;
         }
      }
      #else
         hb_HMAC_SHA1_UpdateKey( &ctx, buffer, hb_itemGetCLen( pBuffer ) );
      #endif
      hb_HMAC_SHA1_EndKey( &ctx );

      hb_itemFreeC( buffer );

      buffer = hb_itemGetC( pKey );

      hb_HMAC_SHA1_StartMessage( &ctx );
      #if HB_SIZE_MAX > UINT_MAX
      {
         HB_SIZE nCount = hb_itemGetCLen( pKey );
         HB_SIZE nDone = 0;

         while( nCount )
         {
            unsigned int uiChunk;

            if( nCount > ( HB_SIZE ) UINT_MAX )
            {
               uiChunk = UINT_MAX;
               nCount -= ( HB_SIZE ) uiChunk;
            }
            else
            {
               uiChunk = ( unsigned int ) nCount;
               nCount = 0;
            }

            hb_HMAC_SHA1_UpdateMessage( &ctx, buffer + nDone, uiChunk );

            nDone += ( HB_SIZE ) uiChunk;
         }
      }
      #else
         hb_HMAC_SHA1_UpdateMessage( &ctx, buffer, hb_itemGetCLen( pKey ) );
      #endif
      hb_HMAC_SHA1_EndMessage( mac, &ctx );
      hb_HMAC_SHA1_Done( &ctx );

      hb_itemFreeC( buffer );

      if( ! hb_parl( 3 ) )
      {
         char hex[ ( sizeof( mac ) * 2 ) + 1 ];
         hb_strtohex( ( char * ) mac, sizeof( mac ), hex );
         hb_retclen( hex, HB_SIZEOFARRAY( hex ) - 1 );
      }
      else
         hb_retclen( ( char * ) mac, sizeof( mac ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
