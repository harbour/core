/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour extended GT functions
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( HB_SETDISPCP )
{
   if ( ISCHAR(1) )
   {
      if ( hb_pcount() == 2 && ISLOG(2) )
         hb_gtSetDispCP( hb_parc( 1 ), NULL, hb_parl( 2 ) );
      else
         hb_gtSetDispCP( hb_parc( 1 ), hb_parc( 2 ), hb_parl( 3 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1089, NULL, "HB_SETDISPCP", HB_ERR_ARGS_BASEPARAMS );

   hb_ret();  /* return NIL */
}

HB_FUNC( HB_SETKEYCP )
{
   if ( ISCHAR(1) )
   {
      hb_gtSetKeyCP( hb_parc( 1 ), hb_parc( 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1089, NULL, "HB_SETKEYCP", HB_ERR_ARGS_BASEPARAMS );

   hb_ret();  /* return NIL */
}

HB_FUNC( HB_SETTERMCP )
{
   if ( ISCHAR(1) )
   {
      if ( hb_pcount() == 2 && ISLOG(2) )
      {
         hb_gtSetDispCP( hb_parc( 1 ), NULL, hb_parl( 2 ) );
         hb_gtSetKeyCP( hb_parc( 1 ), NULL );
      }
      else
      {
         hb_gtSetDispCP( hb_parc( 1 ), hb_parc( 2 ), hb_parl( 3 ) );
         hb_gtSetKeyCP( hb_parc( 1 ), hb_parc( 2 ) );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1089, NULL, "HB_SETTERMCP", HB_ERR_ARGS_BASEPARAMS );

   hb_ret();  /* return NIL */
}

HB_FUNC( HB_GTINFO )
{
   if( ISNUM( 1 ) )
   {
      HB_GT_INFO gtInfo;

      gtInfo.pNewVal = hb_param( 2, HB_IT_ANY );
      gtInfo.pResult = NULL;

      hb_gtInfo( hb_parni( 1 ), &gtInfo );
      if( gtInfo.pResult )
         hb_itemRelease( hb_itemReturn( gtInfo.pResult ) );
      else
         hb_ret();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "HB_GTINFO", HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_GTVERSION )
{
   hb_retc( hb_gtVersion( hb_parni( 1 ) ) );
}

HB_FUNC( HB_GTALERT )
{
   int iClrNorm = ISCHAR( 3 ) ? hb_gtColorToN( hb_parc( 3 ) ) : hb_parni( 3 ),
       iClrHigh = ISCHAR( 4 ) ? hb_gtColorToN( hb_parc( 4 ) ) : hb_parni( 4 );

   hb_retni( hb_gtAlert( hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ),
                         iClrNorm, iClrHigh, hb_parnd( 5 ) ) );
}

HB_FUNC( HB_GFXPRIMITIVE )
{
   PHB_ITEM pType   = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pTop    = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 5, HB_IT_NUMERIC );
   PHB_ITEM pColor  = hb_param( 6, HB_IT_NUMERIC );

   hb_retni( hb_gtGfxPrimitive( hb_itemGetNI(pType),
                                hb_itemGetNI(pTop), hb_itemGetNI(pLeft),
                                hb_itemGetNI(pBottom), hb_itemGetNI(pRight),
                                hb_itemGetNI(pColor) ) );
}

HB_FUNC( HB_GFXTEXT )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   char *cText      = hb_parc(3);
   PHB_ITEM pColor  = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pSize   = hb_param( 5, HB_IT_NUMERIC );
   PHB_ITEM pWidth  = hb_param( 6, HB_IT_NUMERIC );

   hb_gtGfxText( hb_itemGetNI(pTop),
                 hb_itemGetNI(pLeft), cText, hb_itemGetNI(pColor),
                 hb_itemGetNI(pSize), hb_itemGetNI(pWidth) );
}
