/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Sample date functions rewritten in C
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

/* MDY( <dDate> ) --> "month dd, yyyy"
*/
HB_FUNC( MDY )
{
   long lYear, lMonth, lDay;
   char szDate[ 9 ];
   char szFormatted[ 11 ];
   char * szReturn;
   int iBufferLen;
   int iLen;

   hb_dateDecode( hb_parnl( 1 ), &lYear, &lMonth, &lDay );
   hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, "MM/DD/YYYY" );

   iLen = strlen( hb_dateCMonth( lMonth ) );

   iBufferLen = iLen + ( hb_set.hb_set_century ? 9 : 7 );
   szReturn = ( char * ) hb_xgrab( iBufferLen );

   memset( szReturn, ' ', iBufferLen + 1 );
   memcpy( szReturn, hb_dateCMonth( lMonth ), iLen );
   memcpy( szReturn + iLen + 1, szFormatted + 3, 2 );
   szReturn[ iLen + 3 ] = ',';
   memcpy( szReturn + iLen + 5, szFormatted + 6 + ( hb_set.hb_set_century ? 0 : 2 ), 2 + ( hb_set.hb_set_century ? 2 : 0 ) );

   hb_retclen( szReturn, iBufferLen );
   hb_xfree( szReturn );
}

/* DMY( <dDate> ) --> "dd month yyyy" (european date format)
*/
HB_FUNC( DMY )
{
   long lYear, lMonth, lDay;
   char szDate[ 9 ];
   char szFormatted[ 11 ];
   char * szReturn;
   int iBufferLen;
   int iLen;

   hb_dateDecode( hb_parnl( 1 ), &lYear, &lMonth, &lDay );
   hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, "MM/DD/YYYY" );

   iLen = strlen( hb_dateCMonth( lMonth ) );

   iBufferLen = iLen + ( hb_set.hb_set_century ? 9 : 7 );
   szReturn = ( char * ) hb_xgrab( iBufferLen );

   memset( szReturn, ' ', iBufferLen );
   memcpy( szReturn, szFormatted + 3, 2 );
   memcpy( szReturn + 3, hb_dateCMonth( lMonth ), iLen );
   memcpy( szReturn + iLen + 4, szFormatted + 6 + ( hb_set.hb_set_century ? 0 : 2 ), 2 + ( hb_set.hb_set_century ? 2 : 0 ) );

   hb_retclen( szReturn, iBufferLen );
   hb_xfree( szReturn );
}

/* DATEASAGE( <dDate> ) --> <nYears>
*/
HB_FUNC( DATEASAGE )
{
   long lAge = 0;
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;
      long lThisYear, lThisMonth, lThisDay;

      hb_dateToday( &lThisYear, &lThisMonth, &lThisDay );
      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );
      
      if( lThisYear > lYear )
      {
         lAge = lThisYear - lYear;
      
         if( lThisMonth < lMonth || ( lThisMonth == lMonth && lThisDay < lDay ) )
            --lAge;
      }
   }

   hb_retnl( lAge );
}

/* ADDMONTH( <dDate>, <nMonths> ) --> <dNewDate>
   NOTE: Caller must validate dNewDate.
*/
HB_FUNC( ADDMONTH )
{
   long lMonths = hb_parnl( 2 );
   long lYear, lMonth, lDay;

   int iLimit, iMonthAdd, iYearAdd;
   long lNew;

   hb_dateDecode( hb_parnl( 1 ), &lYear, &lMonth, &lDay );

   iLimit = 12 - lMonth + 1;
   iYearAdd = lMonths / 12;
   lMonths %= 12;

   if( lMonths >= iLimit )
      iYearAdd++;

   lYear += iYearAdd;

   iMonthAdd = lMonths % 12;
   lMonth    = ( lMonth + iMonthAdd ) % 12;

   if( lMonth == 0 )
      lMonth = 12;

   lNew = hb_dateEncode( lYear, lMonth, lDay );

   if( !lNew )
   {
      lMonth = ( lMonth + 1 ) % 12;
      lDay = 1;
      lYear = lYear + ( ( lMonth + 1 ) / 12 );
   }

   hb_retd( lYear, lMonth, lDay );
}

/* DATEASARRAY( <dDate> ) --> { Year, Month, Day }
   NOTE: Returns an empty array if <dDate> is not a valid date.
*/
HB_FUNC( DATEASARRAY )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 3 );     /* Create array */
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      PHB_ITEM pItem;
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      pItem = hb_itemPutNL( NULL, lYear );
      hb_itemArrayPut( pReturn, 1, pItem );
      hb_itemRelease( pItem );

      pItem = hb_itemPutNL( NULL, lMonth );
      hb_itemArrayPut( pReturn, 2, pItem );
      hb_itemRelease( pItem );

      pItem = hb_itemPutNL( NULL, lDay );
      hb_itemArrayPut( pReturn, 3, pItem );
      hb_itemRelease( pItem );
   }

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}

/* ARRAYASDATE( { <Year>, <Month>, <Day> } ) --> <dDate>
*/
HB_FUNC( ARRAYASDATE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_retd( hb_arrayGetNL( pArray, 1 ), hb_arrayGetNL( pArray, 2 ), hb_arrayGetNL( pArray, 3 ) );
   else
      hb_retdl( 0 );
}

/* DATEISLEAP( <dDate> ) --> <lIsLeapYear>
*/
HB_FUNC( DATEISLEAP )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      hb_retl( ( lYear % 4 == 0 && lYear % 100 != 0 ) || ( lYear % 400 == 0 ) );
   }
   else
      hb_retl( FALSE );
}

/* NTOD( <nMonth>, <nDay>, <nYear> ) --> <dDate>
*/
HB_FUNC( NTOD )
{
   hb_retd( hb_parnl( 3 ), hb_parnl( 1 ), hb_parnl( 2 ) );
}
