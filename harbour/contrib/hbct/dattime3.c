/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions:
 *       WAITPERIOD(), TIMEVALID(), SETTIME(), SETDATE()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbdate.h"

#if defined( HB_OS_LINUX )
   /* stime exists only in SVr4, SVID, X/OPEN and Linux */
#   ifndef _SVID_SOURCE
#      define _SVID_SOURCE
#   endif
#endif
#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#include <time.h>


/*  $DOC$
 *  $FUNCNAME$
 *      WAITPERIOD()
 *  $CATEGORY$
 *      HBCT date and time functions
 *  $ONELINER$
 *      Pauses a specified time in increments of 1/100 seconds
 *  $SYNTAX$
 *      WAITPERIOD([<nDelay>]) --> lNotElapsed
 *  $ARGUMENTS$
 *      <nDelay>  Designates the waiting period at initialization in
 *      1/100ths of seconds.  Values from 1 to 8, 640, 000 (one day) are
 *      possible.
 *  $RETURNS$
 *      WAITPERIOD() returns .T. , if the time span designated at initialization
 *      has not elapsed.
 *  $DESCRIPTION$
 *      This function sets a time span for a xHarbour DO WHILE loop to run.
 *      The function must initialize prior to the loop, since you must specify
 *      the <nDelay> parameter in 1/100th seconds.  Subsequently, the function
 *      can be implemented without a parameter for additional loop conditions.
 *      It returns .T., as long as the designated time span has not yet run out.
 *
 *      Note
 *
 *      The function notes the status of the internal timer at
 *      initialization.  From that point on, the initialization should always
 *      precede the respective DO WHILE; otherwise, the time delay is
 *      incorrect.  The passing of midnight (the time resets to the 0 value)
 *      is taken into account.
 *  $EXAMPLES$
 *      Run a loop for 5 seconds:
 *
 *      WAITPERIOD(500)               // Initialization, 5 seconds
 *      DO WHILE <cond1> .AND. <cond2> .AND. WAITPERIOD()
 *        *...
 *      ENDDO
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      WAITPERIOD() is Clipper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

/* TODO: make it MT safe */
static double s_dTimeSet = 0;
static double s_dTimeCounter = 0;

HB_FUNC( WAITPERIOD )
{
   double d = hb_dateSeconds();

   if( hb_pcount() > 0 )
   {
      s_dTimeSet = d;
      s_dTimeCounter = d + hb_parnd( 1 )  / 100.0;
   }

   if( d < s_dTimeSet )
      d += 86400.0;

   hb_retl( d < s_dTimeCounter );
}

static HB_BOOL _hb_timeValid( const char * szTime, HB_SIZE nLen, int * piDecode )
{
   HB_BOOL fValid = HB_FALSE;

   if( nLen == 2 || nLen == 5 || nLen == 8 || nLen == 11 )
   {
      static const int s_iMax[] = { 23, 59, 59, 99 };
      int i, iVal;
      HB_SIZE ul;

      fValid = HB_TRUE;
      for( ul = 0; fValid && ul < nLen; ++ul )
      {
         fValid = ul % 3 == 2 ? szTime[ul] == ':' :
                  ( szTime[ul] >= '0' && szTime[ul] <= '9' );
      }
      for( ul = 0, i = 0; fValid && ul < nLen; ul += 3, ++i )
      {
         iVal = 10 * ( szTime[ul] - '0' ) + ( szTime[ul + 1] - '0' );
         fValid = iVal <= s_iMax[i];
         if( piDecode )
            piDecode[i] = iVal;
      }
   }

   return fValid;
}

/*  $DOC$
 *  $FUNCNAME$
 *      TIMEVALID()
 *  $CATEGORY$
 *      HBCT Date and Time Functions
 *  $ONELINER$
 *      Determines whether a specIFied time is valid
 *  $SYNTAX$
 *      TIMEVALID(<cTime>) --> lValid
 *  $ARGUMENTS$
 *      <cTime>  Designates a character string that contains the time to
 *      test.
 *  $RETURNS$
 *      TIMEVALID() RETURNs .T. when <cTime> is a valid time; or .F. when
 *      <cTime> is an invalid time.
 *  $DESCRIPTION$
 *      With input that requires time manipulation, writing your own UDF to
 *      check time inputs was unavoidable up to now.  TIMEVALID() permits
 *      Complete checking of a time designation.  You can use this FUNCTION
 *      effectively with a VALID clause within a READ mask.
 *
 *      Note
 *
 *      Note the format for time designations.  There must always be
 *      two digits for hours, minutes, seconds, and hundredths; otherwise,
 *      the time it is regarded as invalid.  Valid examples are "12",
 *      "12:59", "12:59:59", and "12:59:59:99".  By contrast, invalid
 *      examples are "24", "12:60", or "12:1", and/or "12:".  IF you work
 *      with time strings that are not completely filled and that you need to
 *      check with TIMEVALID(), then they must be TRIMmed prior to the use of
 *      TIMEVALID() (see following Examples).
 *  $EXAMPLES$
 *      Using the VALID clause with TRIM, all valid times are
 *      accepted, even IF no seconds or minutes are specIFied:
 *
 *      cBegin  :=  SPACE(11)
 *      @ 5, 10 SAY "Please input time for beginning work:";
 *      GET cBegin VALID TIMEVALID(TRIM(cBegin))
 *      READ
 *
 *      Using a VALID clause without TRIM, hours and minutes must be
 *      specified, so that TIMEVALID() can confirm a valid time:
 *
 *      cBegin  :=  SPACE(5)
 *      @ 5, 10 SAY "Please input time for beginning work:";
 *      GET cBegin VALID TIMEVALID(cBegin)
 *      READ
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Cl*pper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.c, library is libct.
 *  $SEEALSO$
 *      SETTIME()
 *  $END$
 */

HB_FUNC( TIMEVALID )
{
   hb_retl( _hb_timeValid( hb_parc( 1 ), hb_parclen( 1 ), NULL ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      SETTIME()
 *  $CATEGORY$
 *      HBCT Date and Time Functions
 *  $ONELINER$
 *      Sets the system clock
 *  $SYNTAX$
 *      SETTIME(<cTime>, [<lMode>]) --> lSet
 *  $ARGUMENTS$
 *      <cTime>  Designates a character string that contains the time that
 *      is to become the system time.
 *
 *      <lMode>  Designates whether the time should also be set in the
 *      CMOS-RAM of an AT.  The default is do not write to CMOS-RAM. Note that in
 *      Windows plataform this adjust is automatic, therefore this parameter is
 *      without efect.
 *  $RETURNS$
 *      The FUNCTION RETURNs .T. when the time is set successfully.
 *  $DESCRIPTION$
 *      When you use this FUNCTION to convert the time into the system time from
 *      within your xHarbour application, all files acquire this time with
 *      each write procedure.
 *  $EXAMPLES$
 *      Set the system time in each case; but the hardware clock only
 *      on an AT:
 *
 *      cNewTime  :=  "10:20:00"
 *      IF ISAT()
 *        SETTIME(cNewTime, .T.)
 *      ELSE
 *        SETTIME(cNewTime)
 *      ENDIF
 *
 *      Or, more compactly:
 *
 *      SETTIME(cNewTime, ISAT())
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Cl*pper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.c, library is libct.
 *  $SEEALSO$
 *      SETDATE(),TIMEVALID()
 *  $END$
 */

HB_FUNC( SETTIME )
{
   HB_BOOL fResult = HB_FALSE;
   int iTime[4];

   iTime[0] = iTime[1] = iTime[2] = iTime[3] = 0;
   if( _hb_timeValid( hb_parc( 1 ), hb_parclen( 1 ), iTime ) )
   {
#if defined( HB_OS_WIN )
      SYSTEMTIME st;
      GetLocalTime( &st );
      st.wHour         = ( WORD ) iTime[0];
      st.wMinute       = ( WORD ) iTime[1];
      st.wSecond       = ( WORD ) iTime[2];
      st.wMilliseconds = ( WORD ) iTime[3] * 10;
      fResult = SetLocalTime( &st );
#elif defined( HB_OS_LINUX ) && !defined( __WATCOMC__ )
/* stime exists only in SVr4, SVID, X/OPEN and Linux */
      HB_ULONG lNewTime;
      time_t tm;

      lNewTime = iTime[0] * 3600 + iTime[1] * 60 + iTime[2];
      tm = time( NULL );
      tm += lNewTime - ( tm % 86400 );
      fResult = stime( &tm ) == 0;
#endif
   }

   hb_retl( fResult );
}


/*  $DOC$
 *  $FUNCNAME$
 *      SETDATE()
 *  $CATEGORY$
 *      HBCT Date and Time Functions
 *  $ONELINER$
 *      Sets the system date
 *  $SYNTAX$
 *      SETDATE(<dDate>, [<lMode>]) --> lSet
 *  $ARGUMENTS$
 *      <dDate>  Designates which date to use to set the system date.
 *
 *      <lMode>  Designates whether the date should also be set in the CMOS-
 *      RAM of an AT.  The default is do not write (.F.). Note that in Windows
 *      plataform this adjust is automatic, therefore this parameter is without
 *      efect.
 *  $RETURNS$
 *      SETDATE() RETURNs .T. when the date is successfully set.
 *  $DESCRIPTION$
 *      When you use this FUNCTION to set the system date from within your
 *      xHarbour application, all files acquire this date with each write
 *      procedure.
 *  $EXAMPLES$
 *      Set the system date in each case; but the hardware clock only
 *      on an AT:
 *
 *      dNewDate  :=  CTOD("07/30/91")
 *      IF ISAT()
 *         SETDATE(dNewDate, .T.)
 *      ELSE
 *         SETDATE(dNewDate)
 *      ENDIF
 *
 *      Or, more compactly:
 *
 *      SETDATE(dNewDate, ISAT())
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Cl*pper Tools compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is dattime3.c, library is libct.
 *  $SEEALSO$
 *      SETTIME()
 *  $END$
 */

HB_FUNC( SETDATE )
{
   HB_BOOL fResult = HB_FALSE;
   long lDate = hb_pardl( 1 );

   if( lDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      if( iYear >= 1970 )
      {
#if defined( HB_OS_WIN )
         SYSTEMTIME st;
         GetLocalTime( &st );
         st.wYear      = ( WORD ) iYear;
         st.wMonth     = ( WORD ) iMonth;
         st.wDay       = ( WORD ) iDay;
         st.wDayOfWeek = ( WORD ) hb_dateJulianDOW( lDate );
         fResult = SetLocalTime( &st );
#elif defined( HB_OS_LINUX ) && !defined( __WATCOMC__ )
/* stime exists only in SVr4, SVID, X/OPEN and Linux */
         long lNewDate;
         time_t tm;

         lNewDate = lDate - hb_dateEncode( 1970, 1, 1 );
         tm = time( NULL );
         tm = lNewDate * 86400 + ( tm % 86400 );
         fResult = stime( &tm ) == 0;
#endif
      }
   }

   hb_retl( fResult );
}
