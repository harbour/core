/*
 * $Id$
 *
   Harbour Project source code

   Copyright(C) 1999 by Jose Lalin.
   http://www.Harbour-Project.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   You can contact me at: dezac@corevia.com
  
   The following functions are Copyright 1999 Jose Lalin <dezac@corevia.com>:
      hb_isleapyear(), hb_daysinmonth(), hb_doy(), hb_wom(), hb_woy()
      HB_AMONTHS(), HB_ADAYS(), HB_ISLEAPYEAR(), HB_DAYSINMONTH(), 
      HB_EOM(), HB_BOM(), HB_WOM(), HB_DOY(), HB_WOY(), HB_EOY(), HB_BOY()
 */

#include <extend.h>
#include <errorapi.h>
#include <init.h>
#include <itemapi.h>
#include <ctype.h>
#include <time.h>

HARBOUR HB_ADAYS(void);
HARBOUR HB_AMONTHS(void);
HARBOUR HB_BOM(void);
HARBOUR HB_BOY(void);
HARBOUR HB_DAYSINMONTH(void);
HARBOUR HB_DOY(void);
HARBOUR HB_EOM(void);
HARBOUR HB_EOY(void);
HARBOUR HB_ISLEAPYEAR(void);
HARBOUR HB_WOM(void);
HARBOUR HB_WOY(void);

HB_INIT_SYMBOLS_BEGIN( Dates2__InitSymbols )
{ "ADAYS",         FS_PUBLIC, HB_ADAYS         , 0 },
{ "AMONTHS",       FS_PUBLIC, HB_AMONTHS       , 0 },
{ "BOM",           FS_PUBLIC, HB_BOM           , 0 },
{ "BOY",           FS_PUBLIC, HB_BOY           , 0 },
{ "DAYSINMONTH",   FS_PUBLIC, HB_DAYSINMONTH   , 0 },
{ "DOY",           FS_PUBLIC, HB_DOY           , 0 },
{ "EOM",           FS_PUBLIC, HB_EOM           , 0 },
{ "EOY",           FS_PUBLIC, HB_EOY           , 0 },
{ "ISLEAPYEAR",    FS_PUBLIC, HB_ISLEAPYEAR    , 0 },
{ "WOM",           FS_PUBLIC, HB_WOM           , 0 },
{ "WOY",           FS_PUBLIC, HB_WOY           , 0 }
HB_INIT_SYMBOLS_END( Dates2__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup Dates2__InitSymbols
#endif


/* In dates.c module */
void hb_dateDecode( long julian, long * plDay, long * plMonth, long * plYear );
long hb_dow( long d, long m, long y );

/* In msgxxx.c modules */
extern char *hb_monthsname[];
extern char *hb_daysname[];

static int hb__daysinmonth[ 12 ] =
{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

int hb_isleapyear( long lYear )
{
return (( lYear % 4 == 0 && lYear % 100 != 0 ) || lYear % 400 == 0 )?1:0;
}

long hb_daysinmonth( long lMonth, long lYear )
{
   int i = hb_isleapyear( lYear );

   if( lMonth > 0 && lMonth < 13 )
     return hb__daysinmonth[ lMonth-1 ] + ((i&&lMonth == 2)?1:0);

   return 0;
}

long hb_doy( long lDay, long lMonth, long lYear )
{
   int i;
   int iDoy = 0;

   for( i = 1; i < lMonth; i++ )
      iDoy += hb_daysinmonth( i, lYear );
   iDoy += lDay;

   return iDoy;
}

long hb_wom( long lDay, long lMonth, long lYear )
{
   int iWom = lDay + hb_dow( 1, lMonth, lYear) - 1;

   if( iWom > 0 )
      return ( iWom - hb_dow( lDay, lMonth, lYear ) ) / 7 + 1 ;
   else
      return 0;
}

long hb_woy( long lDay, long lMonth, long lYear, int iIso )
{
   int iWeek, n;

   lDay = hb_doy( lDay, lMonth, lYear );
   n = ( ( ( 1 - (iIso?1:0) ) % 7 ) ) - 1;
   lDay += (n>0)?1:0;
   iWeek = lDay / 7;
   if( iIso )
      iWeek += (n<4)?1:0;
   else
      ++iWeek;

   return iWeek;
}

HARBOUR HB_AMONTHS( void )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 12 );    /* Create array */
   PHB_ITEM pString;
   int i;

   for( i = 0; i < 12; i++ )
   {
      pString = hb_itemNew( NULL );
      hb_itemPutC( pString, hb_monthsname[ i ] );
      hb_itemArrayPut( pReturn, i+1, pString );
      hb_itemRelease ( pString );
   }
   hb_itemReturn ( pReturn );
   hb_itemRelease( pReturn );
}

HARBOUR HB_ADAYS( void )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 7 );    /* Create array */
   PHB_ITEM pString;
   int i;

   for( i = 0; i < 7; i++ )
   {
      pString = hb_itemNew( NULL );
      hb_itemPutC( pString, hb_daysname[ i ] );
      hb_itemArrayPut( pReturn, i+1, pString );
      hb_itemRelease ( pString );
   }
   hb_itemReturn ( pReturn );
   hb_itemRelease( pReturn );
}

HARBOUR HB_ISLEAPYEAR( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retl( hb_isleapyear( lYear ) );
   }
   else
   {
      hb_errorRT_BASE(EG_ARG, 9999, "Argument error", "ISLEAPYEAR");
   }
}

HARBOUR HB_DAYSINMONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retni( hb_daysinmonth( lMonth, lYear ) );
   }
   else
      hb_retni( 0 );
}

HARBOUR HB_EOM( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;
   char szDateFormat[ 9 ];

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      lDay = hb_daysinmonth( lMonth, lYear );
      sprintf( szDateFormat, "%04i%02i%02i", (int) lYear, (int) lMonth, (int) lDay );
      hb_retds( szDateFormat );
   }
   else
      hb_retds( "" );
}

HARBOUR HB_BOM( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;
   char szDateFormat[ 9 ];

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      sprintf( szDateFormat, "%04i%02i%02i", (int) lYear, (int) lMonth, 1 );
      hb_retds( szDateFormat );
   }
   else
      hb_retds( "" );
}

HARBOUR HB_WOM( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retni( hb_wom( lDay, lMonth, lYear ) );
   }
   else
      hb_retni( 0 );
}

HARBOUR HB_DOY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retni( hb_doy( lDay, lMonth, lYear ) );
   }
   else
      hb_retni( 0 );
}

HARBOUR HB_WOY( void )
// Return the nWeek of the year (1 - 52, 0 - 52 if ISO)
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;
   int iIso = 1;

   if( pDate )
   {
      PHB_ITEM pIso = hb_param( 2, IT_LOGICAL );
      if( pIso )
         iIso = pIso->item.asLogical.value;

      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      hb_retni( hb_woy( lDay, lMonth, lYear, iIso ) );
   }
   else
      hb_retni( 0 );
}

HARBOUR HB_EOY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;
   char szDateFormat[ 9 ];

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      sprintf( szDateFormat, "%04i%02i%02i", (int) lYear, 12, 31 );
      hb_retds( szDateFormat );
   }
   else
      hb_retds( "" );
}

HARBOUR HB_BOY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );
   long lDay, lMonth, lYear;
   char szDateFormat[ 9 ];

   if( pDate )
   {
      hb_dateDecode( pDate->item.asDate.value, &lDay, &lMonth, &lYear );
      sprintf( szDateFormat, "%04i%02i%02i", (int) lYear, 1, 1 );
      hb_retds( szDateFormat );
   }
   else
      hb_retds( "" );
}
