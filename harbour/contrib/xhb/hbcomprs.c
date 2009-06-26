/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Compression related functions
 *
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
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbzlib.h"

/*******************************************************************
* Giancarlo Niccolai:
* Calculates the minimum length of destination buffer
*/

int s_hb_compress_error;

ULONG hb_destBuflen( ULONG srclen )
{
   ULONG ret = srclen;

   ret += ret / 100*15 + 12;
   if ( srclen % 100 != 0 ) ret+=15;
   return ret;
}

/****** COMPRESSOR WRAPPER *****
*  HB_COMPRESS( cSource [, nSourceLen ] ) --> cDest
*  HB_COMPRESS( nComprFactor, cSource [,nSourceLen ] ) --> cDest
*  HB_COMPRESS( cSource, nSourceLen, @cDest, @nDestLen ) --> nError
*  HB_COMPRESS( nComprFactor, cSource, nSourceLen, @cDest, @nDestLen ) --> nError
*/

HB_FUNC( HB_COMPRESS )
{
   const char *cSource;
   char *cDest;
   ULONG ulSrclen, ulDstlen, ulBufLen;
   PHB_ITEM pSource, pDest =NULL, pDestLen = NULL;
   int nCompFactor, iFirst;
   int cerr;

   if ( HB_ISNUM(1) )
   {
      nCompFactor = hb_parni( 1 );
      iFirst = 1;
   }
   else
   {
      nCompFactor = Z_DEFAULT_COMPRESSION;
      iFirst = 0;
   }

   pSource = hb_param( iFirst + 1, HB_IT_STRING );

   if( pSource == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError(1) );
      return;
   }

   cSource = hb_itemGetCPtr( pSource ); 
   if (hb_pcount() > iFirst + 1 )
   {
      ulSrclen = (ULONG) hb_parnl( iFirst + 2 );
   }
   else
   {
      ulSrclen = hb_itemGetCLen( pSource ); 
   }

   /* Allocation mode: user provided or allocated here */
   if ( hb_pcount() == iFirst + 4 )
   {
      pDest = hb_param( iFirst + 3, HB_IT_BYREF);
      pDestLen = hb_param( iFirst + 4, HB_IT_BYREF);
      ulDstlen = hb_parnl( iFirst + 4 );
      cDest = NULL;
      ulBufLen = 0;
      if( pDest && pDestLen && ulDstlen > 0 )
      {
         hb_itemGetWriteCL( pDest, &cDest, &ulBufLen );
      }
      if( cDest == NULL || ulBufLen < ulDstlen )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError(1) );
         return;
      }
   }
   else
   {
      ulDstlen = hb_destBuflen( ulSrclen );
      cDest = (char *) hb_xgrab( ulDstlen + 1 );
   }

   cerr = compress2( ( Bytef * ) cDest, &ulDstlen, ( const Bytef * ) cSource, ulSrclen, nCompFactor );

   if ( cerr != Z_OK )
   {
      if ( pDest != NULL )
      {
         hb_retni( cerr );
      }
      else
      {
         hb_xfree( cDest );
         hb_ret();
      }
   }
   else
   {
      if (pDestLen != NULL )
      {
         hb_stornl( iFirst + 4, ( LONG ) ulDstlen );
         hb_retni( Z_OK );
      }
      else
      {
         hb_retclenAdopt( cDest, ulDstlen );
      }
   }
   s_hb_compress_error = cerr;
}


/****** DECOMPRESSOR WRAPPER *****
*  HB_UNCOMPRESS( nDestLen, cSource [, nSourceLen ] ) --> cDest
*  HB_UNCOMPRESS( nDestLen, cSource, nSourceLen, @cDest ) --> nError
*/

HB_FUNC( HB_UNCOMPRESS )
{
   const char *cSource;
   char *cDest;
   ULONG ulSrclen, ulDstlen, ulBufLen;
   PHB_ITEM pSource, pDest;
   int cerr;

   pSource = hb_param( 2, HB_IT_STRING );

   if( ! HB_ISNUM(1) || pSource == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError(1) );
      return;
   }

   cSource = hb_itemGetCPtr( pSource ); 
   ulDstlen = (ULONG) hb_parnl( 1 );
   if (hb_pcount() > 2 )
   {
      ulSrclen = (ULONG) hb_parnl( 3 );
   }
   else
   {
      ulSrclen = hb_itemGetCLen( pSource ); 
   }

   /* Allocation mode: user provided or allocated here */
   if ( hb_pcount() == 4 )
   {
      pDest = hb_param( 4, HB_IT_BYREF);
      cDest = NULL;
      ulBufLen = 0;
      if( pDest )
      {
         hb_itemGetWriteCL( pDest, &cDest, &ulBufLen );
      }
      if( cDest == NULL || ulBufLen < ulDstlen )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError(1) );
         return;
      }
   }
   else
   {
      cDest = (char *) hb_xgrab( ulDstlen + 1 );
   }

   cerr = uncompress( ( Bytef * ) cDest, &ulDstlen, ( const Bytef * ) cSource, ulSrclen );

   if ( cerr != Z_OK )
   {
      if ( hb_pcount() == 4 )
      {
         hb_retni( cerr );
      }
      else
      {
         hb_xfree( cDest );
         hb_ret();
      }
   }
   else
   {
      if ( hb_pcount() == 4 )
      {
         hb_retni( Z_OK );
      }
      else
      {
         hb_retclenAdopt( cDest, ulDstlen );
      }
   }
   s_hb_compress_error = cerr;
}

/*********************************
* HB_COMPRESSERROR() --> nError
*/
HB_FUNC( HB_COMPRESSERROR )
{
   hb_retni( s_hb_compress_error );
}

/*********************************
* HB_COMPRESSERRORDESC( nErrorCode ) --> cDesc
*/

HB_FUNC( HB_COMPRESSERRORDESC )
{
   hb_retcAdopt( hb_strdup( zError( hb_parni( 1 ) ) ) );
}

/*******************************
* HB_COMPRESSBUFLEN( nSrcLen ) -->nDestLen
*/

HB_FUNC( HB_COMPRESSBUFLEN )
{
   hb_retnl( (LONG) hb_destBuflen( hb_parni(1) ) );
}

