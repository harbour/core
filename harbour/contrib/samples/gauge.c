/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Gauge functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
#include "hbapigt.h"

// Box array definitions
#define B_TOP           1
#define B_LEFT          2
#define B_BOTTOM        3
#define B_RIGHT         4
#define B_BACKCOLOR     5
#define B_BARCOLOR      6
#define B_DISPLAYNUM    8
#define B_BARCHAR       7
#define B_PERCENT       9
#define B_LEN           B_PERCENT

#define B_BOXLINES      "ÚÄ¿³ÙÄÀ³"

static void hb_gaugeUpdate( PHB_ITEM pArray, float fPercent );

/* GaugeNew( <nRowTop>, <nColumnTop>, <nRowBottom>, <nColumnBottom>,
      [<cBackgroundColor>],
      [<cGaugeColor>],
      [<cGaugeCharacter>] ) --> aGauge
*/
HB_FUNC( GAUGENEW )
{
   PHB_ITEM pReturn = hb_itemArrayNew( B_LEN );   /* Create array */
   PHB_ITEM pItem;

   pItem = hb_itemPutNL( NULL, ( ISNUM( B_TOP ) ? hb_parni( B_TOP ) : 0 ) );
   hb_itemArrayPut( pReturn, B_TOP, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutNL( NULL, ( ISNUM( B_LEFT ) ? hb_parni( B_LEFT ) : 0 ) );
   hb_itemArrayPut( pReturn, B_LEFT, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutNL( NULL,
              ( ISNUM( B_BOTTOM ) ?
                 ( hb_parni( B_BOTTOM ) < hb_parni( B_TOP ) + 2 ?
                     hb_parni( B_TOP ) + 2 : hb_parni( B_BOTTOM ) ) : 0 ) );
   hb_itemArrayPut( pReturn, B_BOTTOM, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutNL( NULL,
              ( ISNUM( B_RIGHT ) ?
                 ( hb_parni( B_RIGHT ) < hb_parni( B_LEFT ) + 4 ?
                    hb_parni( B_LEFT ) + 4 : hb_parni( B_RIGHT ) ) : 0 ) );
   hb_itemArrayPut( pReturn, B_RIGHT, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutC( NULL, ( ISCHAR( B_BACKCOLOR ) ? hb_parc( B_BACKCOLOR ) : "W/N" ) );
   hb_itemArrayPut( pReturn, B_BACKCOLOR, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutC( NULL, ( ISCHAR( B_BARCOLOR ) ? hb_parc( B_BARCOLOR ) : "W+/N" ) );
   hb_itemArrayPut( pReturn, B_BARCOLOR, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutL( NULL, !( ISNUM( B_RIGHT ) && 
                                 ISNUM( B_LEFT ) && 
                                 ( hb_parni( B_RIGHT ) < hb_parni( B_LEFT ) + 9 ) ) );
   hb_itemArrayPut( pReturn, B_DISPLAYNUM, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutC( NULL, ( ISCHAR( B_BARCHAR ) ? hb_parc( B_BARCHAR ) : ( char * ) '\xdb') );
   hb_itemArrayPut( pReturn, B_BARCHAR, pItem );
   hb_itemRelease( pItem );

   pItem = hb_itemPutNL( NULL, 0 );
   hb_itemArrayPut( pReturn, B_PERCENT, pItem );
   hb_itemRelease( pItem );

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}

/* GaugeDisplay( aGauge ) --> aGauge
*/
HB_FUNC( GAUGEDISPLAY )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      int iCenter = ( ( hb_arrayGetNL( pArray, B_RIGHT ) - hb_arrayGetNL( pArray, B_LEFT ) ) / 2 ) + 1;
      char szOldColor[ CLR_STRLEN ];
      char * szStr = "        ";

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_arrayGetCPtr( pArray, B_BACKCOLOR ) );

      hb_gtBox( (SHORT)  hb_arrayGetNL( pArray, B_TOP ),
                (SHORT)  hb_arrayGetNL( pArray, B_LEFT ),
                (SHORT)  hb_arrayGetNL( pArray, B_BOTTOM ),
                (SHORT)  hb_arrayGetNL( pArray, B_RIGHT ),
                (BYTE *) szStr );

      hb_gtBox( (SHORT)  hb_arrayGetNL( pArray, B_TOP ),
                (SHORT)  hb_arrayGetNL( pArray, B_LEFT ),
                (SHORT)  hb_arrayGetNL( pArray, B_BOTTOM ),
                (SHORT)  hb_arrayGetNL( pArray, B_RIGHT ),
                (BYTE *) B_BOXLINES );

      if( hb_arrayGetL( pArray, B_DISPLAYNUM ) )
         hb_gtWriteAt( (USHORT) hb_arrayGetNL( pArray, B_TOP ),
                       iCenter, ( BYTE * ) "[      ]", 8 );

      hb_gtSetColorStr( szOldColor );

      hb_gaugeUpdate( pArray, (float) hb_arrayGetNL( pArray, B_PERCENT ) );

      hb_itemReturn( pArray );
   }
}

/* GaugeUpdate( aGauge, nPercent ) --> aGauge
*/
HB_FUNC( GAUGEUPDATE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      hb_gaugeUpdate( pArray, ISNUM( 2 ) ? (float) hb_parnd( 2 ) : 0 );

      hb_itemReturn( pArray );
   }
}

static void hb_gaugeUpdate( PHB_ITEM pArray, float fPercent )
{
   int iCenter = ( ( hb_arrayGetNL( pArray, B_RIGHT ) - hb_arrayGetNL( pArray, B_LEFT ) ) / 2 ) + 1;
   int iRatio = hb_arrayGetNL( pArray, B_RIGHT ) - hb_arrayGetNL( pArray, B_LEFT ) - 1;
   int iRow;
   int iCols;
   int iMax;
   char szOldColor[ CLR_STRLEN ];
   char * szStr = "        ";
   char szPct[ 4 ];

   hb_gtGetColorStr( szOldColor );
   hb_gtSetColorStr( hb_arrayGetCPtr( pArray, B_BARCOLOR ) );

   fPercent = ( fPercent < 0 ? 0 : ( fPercent > 1 ? 1 : fPercent ) );
   iCols    = (int) (fPercent * iRatio);

   if( hb_arrayGetL( pArray, B_DISPLAYNUM ) )
   {
//    sprintf( szPct, "%3.0f\%", fPercent * 100 );
      sprintf( szPct, "%3.0f%%", fPercent * 100 );
      hb_gtWriteAt( (USHORT) hb_arrayGetNL( pArray, B_TOP ),
                    (USHORT) iCenter + 2, (BYTE *) szPct, 4 );
   }

   hb_gtBox( hb_arrayGetNL( pArray, B_TOP ) + 1, hb_arrayGetNL( pArray, B_LEFT ) + 1,
             hb_arrayGetNL( pArray, B_BOTTOM ) - 1, hb_arrayGetNL( pArray, B_RIGHT ) - 1,
             ( BYTE * ) szStr );

   iMax = hb_arrayGetNL( pArray, B_BOTTOM ) - hb_arrayGetNL( pArray, B_TOP ) - 1;
   for( iRow = 1; iRow <= iMax; iRow++ )
   {
      hb_gtRepChar( (USHORT) (iRow + hb_arrayGetNL( pArray, B_TOP )),
                    (USHORT) (hb_arrayGetNL( pArray, B_LEFT ) + 1),
                    ( BYTE ) * hb_arrayGetCPtr( pArray, B_BARCHAR ), iCols );
   }

   hb_gtSetColorStr( szOldColor );
}
