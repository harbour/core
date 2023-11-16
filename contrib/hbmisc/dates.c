/*
 * Additional date functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 * Copyright 1999 Jon Berg <jmberg@pnh10.med.navy.mil> (DateTime())
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include <time.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbdate.h"

static const int s_daysinmonth[ 12 ] =
   { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static HB_BOOL hb_isleapyear( int iYear )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_isleapyear(%d)", iYear ) );

   return ( iYear % 4 == 0 && iYear % 100 != 0 ) || ( iYear % 400 == 0 );
}

static int hb_daysinmonth( int iYear, int iMonth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_daysinmonth(%d, %d)", iYear, iMonth ) );

   if( iMonth > 0 && iMonth < 13 )
      return s_daysinmonth[ iMonth - 1 ] +
             ( ( iMonth == 2 && hb_isleapyear( iYear ) ) ? 1 : 0 );
   else
      return 0;
}

static int hb_doy( int iYear, int iMonth, int iDay )
{
   int i;
   int iDoy = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_doy(%d, %d, %d)", iYear, iMonth, iDay ) );

   for( i = 1; i < iMonth; ++i )
      iDoy += hb_daysinmonth( iYear, i );

   return iDoy + iDay;
}

static int hb_woy( long lDate, HB_BOOL fISO )
{
   int iYear, iMonth, iDay;

   HB_TRACE( HB_TR_DEBUG, ( "hb_woy(%ld, %d)", lDate, ( int ) fISO ) );

   hb_dateDecode( lDate, &iYear, &iMonth, &iDay );

   if( fISO )
      hb_dateDecode( lDate + 3 - ( hb_dateDOW( iYear, iMonth, iDay ) + 5 ) % 7,
                     &iYear, &iMonth, &iDay );

   return ( hb_doy( iYear, iMonth, iDay ) - 1 ) / 7 + 1;
}

HB_FUNC( AMONTHS )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 12 );  /* Create array */
   int      i;

   for( i = 0; i < 12; ++i )
      hb_arraySetC( pReturn, i + 1, hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + i ) );

   hb_itemReturnRelease( pReturn );
}

HB_FUNC( ADAYS )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 7 );  /* Create array */
   int      i;

   for( i = 0; i < 7; ++i )
      hb_arraySetC( pReturn, i + 1, hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + i ) );

   hb_itemReturnRelease( pReturn );
}

HB_FUNC( ISLEAPYEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      hb_retl( hb_isleapyear( iYear ) );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( HBMISC_DAYSINMONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      hb_retni( hb_daysinmonth( iYear, iMonth ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WOY )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   hb_retni( pDate == NULL ? 0 :
             hb_woy( hb_itemGetDL( pDate ), hb_parldef( 2, HB_TRUE ) ) );
}
