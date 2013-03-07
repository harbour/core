/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    hb_UTF8ToStrBox()
 *
 * Copyright 2009-2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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
#include "hbapierr.h"
#include "hbapicdp.h"
#include "hbapigt.h"

HB_FUNC( HB_UTF8TOSTRBOX )
{
   const char * szString = hb_parc( 1 );

   if( szString )
   {
      HB_SIZE nLen = hb_parclen( 1 ), nDest = 0;
      char * szDest = NULL;

      if( nLen )
      {
         PHB_CODEPAGE cdp = hb_gtBoxCP();

         if( cdp )
         {
            if( hb_cdpIsUTF8( cdp ) )
            {
               hb_itemReturn( hb_param( 1, HB_IT_STRING ) );
               return;
            }
            else
            {
               szString = hb_parc( 1 );
               nDest = hb_cdpUTF8AsStrLen( cdp, szString, nLen, 0 );
               szDest = ( char * ) hb_xgrab( nDest + 1 );
               hb_cdpUTF8ToStr( cdp, szString, nLen, szDest, nDest + 1 );
            }
         }
      }

      if( szDest )
         hb_retclen_buffer( szDest, nDest );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
