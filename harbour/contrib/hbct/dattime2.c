/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions, part II: - ADDMONTH()
 *                                       - CTODOW()
 *                                       - CTOMONTH()
 *                                       - DAYSINMONTH()
 *                                       - DAYSTOMONTH()
 *                                       - DMY()
 *                                       - DOY()
 *                                       - ISLEAP()
 *                                       - LASTDAYOM()
 *                                       - MDY()
 *                                       - NTOCDOW()
 *                                       - NTOCMONTH()
 *                                       - QUARTER()
 *                                       - WEEK()
 *
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru>
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
#include "hbapilng.h"
#include "hbdate.h"
#include "hbset.h"


static HB_BOOL ct_isleap( int iYear )
{
   return iYear != 0 && ( ( ( iYear & 3 ) == 0 && iYear % 100 != 0 ) ||
                          iYear % 400 == 0 );
}

static int ct_daysinmonth( int iMonth, HB_BOOL bLeap )
{
   if( iMonth == 2 )
      return bLeap ? 29 : 28;
   else if( iMonth == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11 )
      return 30;
   else
      return 31;
}

static int ct_daystomonth( int iMonth, HB_BOOL bLeap )
{
   static const int iMonthes[] =
               { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

   return ( ( iMonth < 1 && iMonth > 12 ) ? 0 : iMonthes[iMonth - 1] +
            ( ( bLeap && iMonth > 2 ) ? 1 : 0 ) );
}

static int ct_doy( long lDate )
{
   int iYear, iMonth, iDay;
   long lFirst;

   hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   lFirst = hb_dateEncode( iYear, 1, 1 );
   return ( int ) ( lDate - lFirst + 1 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CTODOW()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      convert name of day of the week to its ordinal number
 *  $SYNTAX$
 *      CTODOW (<cName>) -> nOrdinal
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      CTODOW() is compatible with CT3's CTODOW().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      NTOCDOW()
 *  $END$
 */
HB_FUNC( CTODOW )
{
   HB_SIZE nLen = hb_parclen( 1 );
   int iDow = 0;

   if( nLen )
   {
      const char *szParam = hb_parc( 1 );

      for( iDow = 7; iDow > 0; iDow-- )
      {
         const char * szDow = hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + iDow - 1 );
         if( hb_strnicmp( szDow, szParam, nLen ) == 0 )
            break;
      }
   }

   hb_retnl( iDow );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CTOMONTH()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      convert name of month to its ordinal number
 *  $SYNTAX$
 *      CTOMONTH (<cName>) -> nOrdinal
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      CTOMONTH() is compatible with CT3's CTOMONTH().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      NTOCMONTH()
 *  $END$
 */
HB_FUNC( CTOMONTH )
{
   HB_SIZE nLen = hb_parclen( 1 );
   int iMonth = 0;

   if( nLen )
   {
      const char *szParam = hb_parc( 1 );
      for( iMonth = 12; iMonth > 0; iMonth-- )
      {
         const char * szMonth = hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 );
         if( hb_strnicmp( szMonth, szParam, nLen ) == 0 )
            break;
      }
   }

   hb_retnl( iMonth );
}


/*  $DOC$
 *  $FUNCNAME$
 *      DMY()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns the date as a string in DD Month YY format
 *  $SYNTAX$
 *      DMY ([<dDate>][, <lMode>]) -> cDateString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      Returns the date as a string in DD Month YY format. If lmode
 *      is TRUE, a "." is inserted after the DD
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      DMY() is compatible with CT3's DMY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      MDY()
 *  $END$
 */
HB_FUNC( DMY )
{
   int iYear, iMonth, iDay;
   HB_BOOL bMode = HB_FALSE;

   if( HB_ISDATETIME( 1 ) )
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      hb_dateToday( &iYear, &iMonth, &iDay );

   if( HB_ISLOG( 2 ) )
      bMode = hb_parl( 2 );

   if( iMonth >= 1 && iMonth <= 12 )
   {
      const char *szMonth = hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      int iMonLen = ( int ) strlen( szMonth );
      int iLen = 0, iBufLen = iMonLen + 10;
      char *szMDY = ( char * ) hb_xgrab( iBufLen );

      if( iDay < 10 )
      {
         szMDY[iLen] = ( char ) iDay + 0x30;
         iLen++;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }

      if( bMode )
      {
         szMDY[iLen] = '.';
         iLen++;
      }
      szMDY[iLen] = ' ';
      iLen++;

      hb_strncpy( szMDY + iLen, szMonth, iBufLen - iLen - 1 );
      iLen += iMonLen;
      szMDY[iLen] = ' ';
      iLen++;

      if( hb_setGetCentury() )
      {
         hb_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      hb_retclen( szMDY, iLen );
      hb_xfree( szMDY );
   }
   else
      hb_retc_null();
}


/*  $DOC$
 *  $FUNCNAME$
 *      MDY()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns the date as a string in Month DD, YY or Month DD, YYYY
 *  $SYNTAX$
 *      MDY ([<dDate>]) -> cDateString
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      Returns the date as a string in Month DD, YY or Month DD, YYYY
 *      If dDate is NULL, the system date is used
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      MDY() is compatible with CT3's MDY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      DMY()
 *  $END$
 */
HB_FUNC( MDY )
{
   int iYear, iMonth, iDay;

   if( HB_ISDATETIME( 1 ) )
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      hb_dateToday( &iYear, &iMonth, &iDay );

   if( iMonth >= 1 && iMonth <= 12 )
   {
      const char *szMonth = hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 );
      int iLen = ( int ) strlen( szMonth );
      int iBufLen = iLen + 9;
      char *szMDY = ( char * ) hb_xgrab( iBufLen );

      hb_strncpy( szMDY, szMonth, iBufLen - 1 );
      szMDY[iLen++] = ' ';
      if( iDay < 10 )
      {
         szMDY[iLen] = ( char ) iDay + 0x30;
         iLen++;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iDay );
         iLen += 2;
      }
      szMDY[iLen++] = ' ';

      if( hb_setGetCentury() )
      {
         hb_snprintf( szMDY + iLen, 5, "%04d", iYear );
         iLen += 4;
      }
      else
      {
         hb_snprintf( szMDY + iLen, 3, "%02d", iYear % 100 );
         iLen += 2;
      }

      hb_retclen( szMDY, iLen );
      hb_xfree( szMDY );
   }
   else
      hb_retc_null();
}


/*  $DOC$
 *  $FUNCNAME$
 *      ADDMONTH()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      add months to a date
 *  $SYNTAX$
 *      ADDMONTH ([<dDate>,] <nMonths>) -> dShiftedDate
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      ADDMONTH() is compatible with CT3's ADDMOTH().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( ADDMONTH )
{
   long lJulian, lMillisec = 0;
   int iYear, iMonth, iDay, iNum, iDays;
   HB_BOOL fTimeStamp = HB_FALSE;

   if( HB_ISNUM( 1 ) )
   {
      iNum = hb_parni( 1 );
      hb_dateToday( &iYear, &iMonth, &iDay );
   }
   else
   {
      if( HB_ISTIMESTAMP( 1 ) )
      {
         fTimeStamp = HB_TRUE;
         hb_partdt( &lJulian, &lMillisec, 1 );
         hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
      }
      else if( HB_ISDATE( 1 ) )
         hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
      else
         hb_dateToday( &iYear, &iMonth, &iDay );
      iNum = hb_parni( 2 );
   }

   iMonth += iNum;
   while( iMonth <= 0 )
   {
      iMonth += 12;
      iYear--;
   }
   while( iMonth > 12 )
   {
      iMonth -= 12;
      iYear++;
   }

   iDays = ct_daysinmonth( iMonth, ct_isleap( iYear ) );
   if( iDay > iDays )
   {
      iDay = iDays;
   }

   lJulian = hb_dateEncode( iYear, iMonth, iDay );
   if( fTimeStamp )
      hb_rettdt( lJulian, lMillisec );
   else
      hb_retdl( lJulian );
}


/*  $DOC$
 *  $FUNCNAME$
 *      DOY()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Determines the day of the year for a specific date
 *  $SYNTAX$
 *      DMY ([<dDate>]) -> nDayOfYear
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      Determines the day of the year for a specific date
 *      if dDate is invalid, returns 0
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      DOY() is compatible with CT3's DOY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */
HB_FUNC( DOY )
{
   long lDate;

   if( HB_ISDATETIME( 1 ) )
      lDate = hb_pardl( 1 );
   else
   {
      int iYear, iMonth, iDay;

      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }

   hb_retni( ct_doy( lDate ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      ISLEAP()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      determines of year of date is a leap year
 *  $SYNTAX$
 *      ISLEAP ([<dDate>]) -> lIsLeap
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      ISLEAP() is compatible with CT3's ISLEAP().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */
HB_FUNC( ISLEAP )
{
   int iYear, iMonth, iDay;

   if( HB_ISDATETIME( 1 ) )
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      hb_dateToday( &iYear, &iMonth, &iDay );

   hb_retl( ct_isleap( iYear ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      DAYSTOMONTH()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Total number of days from first of Jan to beginning of nMonth.
 *  $SYNTAX$
 *      DAYSTOMONTH (<nMonth>, <lLeapYear>) -> nDaysToMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      lLeap is FALSE for a non-leap year but TRUE if it is. If so and nMonth
 *      is greater than 2, ndays is incremented
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      DAYSTOMONTH() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      DAYSINMONTH()
 *  $END$
 */
HB_FUNC( DAYSTOMONTH )
{
   int iMonth = hb_parni( 1 );
   HB_BOOL bLeap = hb_parl( 2 );

   hb_retni( ct_daystomonth( iMonth, bLeap ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      DAYSINMONTH()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns the number of days in month
 *  $SYNTAX$
 *      DAYSINMONTH (<nMonth>, <lLeapYear>) -> nDaysInMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      DAYSINMONTH() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      DAYSTOMONTH()
 *  $END$
 */
HB_FUNC( DAYSINMONTH )
{
   int iMonth = hb_parni( 1 );
   HB_BOOL bLeap = hb_parl( 2 );

   hb_retni( ct_daysinmonth( iMonth, bLeap ) );

}


/*  $DOC$
 *  $FUNCNAME$
 *      QUARTER()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns a number equal to the quarter in which a date falls
 *  $SYNTAX$
 *      QUARTER ([<dDate>]) -> nQuarter
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      Returns a number equal to the quarter in which ddate
 *      falls. If ddate  is empty, the system date is employed.
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      QUARTER() is compatible with CT3's QUARTER().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */
HB_FUNC( QUARTER )
{
   int iYear, iMonth, iDay;

   if( HB_ISDATETIME( 1 ) )
      hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
   else
      hb_dateToday( &iYear, &iMonth, &iDay );

   hb_retni( ( iMonth + 2 ) / 3 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      LASTDAYOM()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns the the number of days in the month.
 *  $SYNTAX$
 *      LASTDAYOM ([<dDate|nMonth>]) -> nDaysInMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      <dDate|nMonth> can be a date or a month number. If empty uses the
 *      system date.  If nMonth is a 2, lastdayom() will not know if it
 *      is a leap year or not. If dDate is invalid, returns 0
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      LASTDAYOM() is compatible with CT3's LASTDAYOM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      EOM()
 *  $END$
 */
HB_FUNC( LASTDAYOM )
{
   HB_BOOL bLeap = HB_FALSE;
   int iYear, iMonth, iDay;

   if( HB_ISNUM( 1 ) )
      iMonth = hb_parni( 1 );
   else
   {
      if( HB_ISDATETIME( 1 ) )
         hb_dateDecode( hb_pardl( 1 ), &iYear, &iMonth, &iDay );
      else
         hb_dateToday( &iYear, &iMonth, &iDay );

      bLeap = ct_isleap( iYear );
   }

   hb_retni( ( iMonth && ( iMonth <= 12 ) ? ct_daysinmonth( iMonth, bLeap ) : 0 ) );

}


/*  $DOC$
 *  $FUNCNAME$
 *      NTOCDOW()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      (num of day) -> day name
 *  $SYNTAX$
 *      NTOCDOW (<nDay>) -> cDay
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NTOCDOW() is compatible with CT3's NTOCDOW().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      CTODOW()
 *  $END$
 */
HB_FUNC( NTOCDOW )
{
   hb_retc( hb_dateCDOW( hb_parni( 1 ) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NTOCMONTH()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      (num of month ) -> Month Name
 *  $SYNTAX$
 *      NTOCMONTH (<nMonth>) -> cMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NTOCMONTH() is compatible with CT3's NTOCMONTH().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *      CTOMONTH()
 *  $END$
 */
HB_FUNC( NTOCMONTH )
{
   hb_retc( hb_dateCMonth( hb_parni( 1 ) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      WEEK()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Returns the calendar week a number
 *  $SYNTAX$
 *      WEEK ([<dDate>][, <lSWN>]) -> nWeek
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      Returns the calendar week a number. If no date is specified,
 *      the system date is used. An empty date viz ctod("  /  /  ")
 *      returns 0.
 *      If <lSWN> is .T., week() will calculate the "simple week number", defined by
 *         - week #1 starts on January, 1st
 *         - week #(n+1) starts seven days after start of week #n
 *      If <lSWN> is .F. (default), the ISO8601 week number, defined by
 *         - weeks start on mondays
 *         - week #1 is the one that includes January, 4
 *      will be calculated
 *      TODO: add further documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      WEEK() is compatible with CT3's WEEK().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is dattime2.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */
HB_FUNC( WEEK )
{
   int iYear, iMonth, iDay, iWeek;
   long lDate;
   HB_BOOL bSWN = hb_parl( 2 );

   if( HB_ISDATETIME( 1 ) )
   {
      lDate = hb_pardl( 1 );
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
   }
   else
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }

   if( !lDate )
   {
      iWeek = 0;
   }
   else if( bSWN )
   {
      int iDays = ct_daystomonth( iMonth, ct_isleap( iYear ) ) + iDay;
      int iPart = ( iDays % 7 );

      iWeek = iDays / 7;
      if( iPart > 0 )
         iWeek++;
   }
   else
   {
      long lDate2;

      if( hb_setGetCPtr( HB_SET_DATEFORMAT ) && ( hb_setGetCPtr( HB_SET_DATEFORMAT )[0] == 'd' ||
                                                  hb_setGetCPtr( HB_SET_DATEFORMAT )[0] == 'D' ) )
         lDate2 = lDate + 3 - ( hb_dateDOW( iYear, iMonth, iDay ) + 5 ) % 7;
      else
         lDate2 = lDate + 4 - hb_dateDOW( iYear, iMonth, iDay );

      iWeek = ( ct_doy( lDate2 ) - 1 ) / 7 + 1;
   }

   hb_retni( iWeek );
}
