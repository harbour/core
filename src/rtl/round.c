/*
 * Harbour Project source code:
 * Round(), Int() functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *    Int()
 *
 * Copyright 2003 Vicente Aranzana <varanzana@gruposp.com>
 * hb_numRound()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( INT )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber )
   {
      if( HB_IS_NUMINT( pNumber ) )
         hb_itemReturn( pNumber );
      else
      {
         int iWidth;

         hb_itemGetNLen( pNumber, &iWidth, NULL );
         hb_retnlen( hb_numInt( hb_itemGetND( pNumber ) ), iWidth, 0 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1090, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( ROUND )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber && HB_ISNUM( 2 ) )
   {
      int iDec = hb_parni( 2 );

#ifdef HB_CLP_STRICT
      /* In CA-Cl*pper Round() always returns double item, what in some
       * applications may be important due to different formatting rules
       * when SET FIXED is ON [druzus]
       */
      hb_retndlen( hb_numRound( hb_itemGetND( pNumber ), iDec ), 0, HB_MAX( iDec, 0 ) );
#else
      if( iDec == 0 && HB_IS_NUMINT( pNumber ) )
         hb_retnint( hb_itemGetNInt( pNumber ) );
      else
         hb_retnlen( hb_numRound( hb_itemGetND( pNumber ), iDec ), 0, HB_MAX( iDec, 0 ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1094, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
