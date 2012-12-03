/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_ATI() function
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999-2009 Viktor Szakats (harbour syenar.net)
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

static HB_SIZE hb_strAtI( PHB_CODEPAGE cdp, const char * szSub, HB_SIZE nSubLen, const char * szText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strAtI(%p, %s, %" HB_PFS "u, %s, %" HB_PFS "u)", cdp, szSub, nSubLen, szText, nLen ) );

   if( nSubLen > 0 && nLen >= nSubLen )
   {
      HB_SIZE nPos = 0, nIndex = 0;
      do
      {
         HB_SIZE nSubPos = 0, nPrev = nPos;
         if( hb_cdpCharCaseEq( cdp, szText, nLen, &nPos, szSub, nSubLen, &nSubPos ) )
         {
            HB_SIZE nBack = nPos;
            do
            {
               if( nSubPos >= nSubLen )
                  return ( HB_CDP_ISCHARIDX( cdp ) ? nIndex : nPrev ) + 1;
            }
            while( hb_cdpCharCaseEq( cdp, szText, nLen, &nPos, szSub, nSubLen, &nSubPos ) );
            nPos = nBack;
         }
         ++nIndex;
      }
      while( nPos < nLen );
   }

   return 0;
}

HB_FUNC( HB_ATI )
{
   PHB_ITEM pSub  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pText = hb_param( 2, HB_IT_STRING );

   if( pText && pSub )
   {
      PHB_CODEPAGE cdp         = hb_vmCDP();
      const char * pszText     = hb_itemGetCPtr( pText );
      HB_SIZE      nTextLength = hb_itemGetCLen( pText );
      HB_SIZE      nStart      = hb_parns( 3 );
      HB_SIZE      nFrom, nTo, nPos = 0;

      if( nStart <= 1 )
         nStart = nFrom = 0;
      else if( HB_CDP_ISCHARIDX( cdp ) )
         nFrom = hb_cdpTextPos( cdp, pszText, nTextLength, --nStart );
      else
         nFrom = --nStart;

      if( nFrom < nTextLength )
      {
         pszText     += nFrom;
         nTextLength -= nFrom;
         if( HB_ISNUM( 4 ) )
         {
            nTo = hb_parns( 4 );
            if( nTo <= nStart )
               nTo = 0;
            else
            {
               nTo -= nStart;
               if( HB_CDP_ISCHARIDX( cdp ) )
                  nTo = hb_cdpTextPos( cdp, pszText, nTextLength, nTo );
               if( nTo > nTextLength )
                  nTo = nTextLength;
            }
         }
         else
            nTo = nTextLength;

         if( nTo > 0 )
         {
            nPos = hb_strAtI( cdp, hb_itemGetCPtr( pSub ), hb_itemGetCLen( pSub ),
                              pszText, nTo );
            if( nPos > 0 )
               nPos += HB_CDP_ISCHARIDX( cdp ) ? nStart : nFrom;
         }
      }
      hb_retns( nPos );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1108, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
