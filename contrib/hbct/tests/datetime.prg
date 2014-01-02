/*
 * Harbour Project source code:
 *   Program to test functions to mimic Clipper Tools III date and time functions.
 *   (C) Alan Secker 2002 <alansecker@globalnet.co.uk>
 *
 *   small changes: Martin Vogel <vogel@inttec.de> 2003
 *
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

#ifdef __HARBOUR__
#require "hbct"
#endif

#include "inkey.ch"
#include "setcurs.ch"

#ifdef __HARBOUR__
#include "hbextlng.ch"
#endif

PROCEDURE Main( cLang )

   LOCAL nChoice := 1
   LOCAL farr := { ;
      "AddMonth( dDate )     Add a month to dDate      ", ;
      "BoM()                 Beginning of month        ", ;
      "BoQ()                 Returns first date of qtr ", ;
      "BoY()                 Beginning of year         ", ;
      "CToDoW()              Day name to day number    ", ;
      "CToMonth()            Month name to number      ", ;
      "DaysInMonth()         number of days in xMonth  ", ;
      "DaysToMonth( dDate )  Returns num days to month ", ;
      "DMY( dDate, lMode )   date as DD month YY       ", ;
      "DoY( dDate )          Returns day of the year   ", ;
      "EoM( dDate )          Returns last day of month ", ;
      "EoQ( dDate )          Returns last date of qtr  ", ;
      "EoY( dDate )          Returns last day of year  ", ;
      "IsLeap( dDate )       Returns .T. if leap year  ", ;
      "LastDayOM( dDate )    Returns num days in month ", ;
      "MDY( dDate )          Returns stg Month DD, YY  ", ;
      "NToCDoW( nDay )       Returns name of day       ", ;
      "NToCMonth( nMth )     Returns name of month     ", ;
      "Quarter( date )       Returns qtr number of date", ;
      "SToD( ansi date )     Returns Harbour date      ", ;
      "Week( dDate, lSWN )   Returns numbef of week    ", }

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

#ifdef __HARBOUR__
   hb_langSelect( iif( HB_ISSTRING( cLang ), cLang, "en" ) )
#endif

   SET CURSOR OFF
   SET CONFIRM ON
   SET ESCAPE ON

   DO WHILE .T.

      CLS

      @ 2, 5 TO 24, 66
      nChoice := AChoice( 3, 7, 23, 65, farr,,, nChoice )

      CLS

      DO CASE
      CASE nChoice ==  0 ; EXIT
      CASE nChoice ==  1 ; addmtest()
      CASE nChoice ==  2 ; bomtest()
      CASE nChoice ==  3 ; boqtest()
      CASE nChoice ==  4 ; boytest()
      CASE nChoice ==  5 ; ctodowtest()
      CASE nChoice ==  6 ; ctomonthtest()
      CASE nChoice ==  7 ; dInMonthtest()
      CASE nChoice ==  8 ; d2month()
      CASE nChoice ==  9 ; dmytest()
      CASE nChoice == 10 ; doytest()
      CASE nChoice == 11 ; eomtest()
      CASE nChoice == 12 ; eoqtest()
      CASE nChoice == 13 ; eoytest()
      CASE nChoice == 14 ; isleaptest()
      CASE nChoice == 15 ; lastdayomtest()
      CASE nChoice == 16 ; mdytest()
      CASE nChoice == 17 ; ntocdowtest()
      CASE nChoice == 18 ; ntocmthtest()
      CASE nChoice == 19 ; qtrtest()
      CASE nChoice == 20 ; stodtest()
      CASE nChoice == 21 ; weektest()
      ENDCASE
   ENDDO

   RETURN

STATIC PROCEDURE addmtest()

   // AddMonth( dDate, nMonths )
   // ===========================
   // This version will only accept an nMonths value of from 0 to 70

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()
   LOCAL mnth    := 0

   DO WHILE .T.
      @  5, 10 SAY "AddMonth( dDate ) returns the date incremented by"
      @  6, 10 SAY "the number of days in months."
      @  7, 10 SAY "Insert a date"
      @  8, 10 SAY "How many months"

      @  7, 60 GET dDate
      @  8, 60 GET mnth PICTURE "99"

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 39 SAY "The returned date is " + DToC( AddMonth( dDate, mnth ) )

      Inkey( 0 )
   ENDDO

   RETURN

STATIC PROCEDURE bomtest()

   // BoM( dDate )
   // =============

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "BoM( dDate ) returns the date of the first day of dDate"
      @  6, 10 SAY "If dDate is 2002-10-15 (that's in yyyy-mm-dd), BoM()"
      @  7, 10 SAY "should return 2002-10-01. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The returned date is " + DToC( BoM( dDate ) )

      Inkey( 0 )
   ENDDO

   RETURN

STATIC PROCEDURE boqtest()

   // BoM( dDate )
   // =============

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "BoQ( dDate ) returns the date of the first day of the"
      @  6, 10 SAY "quarter in which dDate is situated. If dDate is"
      @  7, 10 SAY "2002-10-15 (that's in yyyy-mm-dd), BoQ() should return"
      @  8, 10 SAY "2002-09-01. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The returned date is " + DToC( BoQ( dDate ) )

      Inkey( 0 )
   ENDDO

   RETURN

STATIC PROCEDURE boytest()

   // BoY( dDate )
   // =============

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()
   LOCAL nkey

   DO WHILE .T.
      @  5, 10 SAY "BoY( dDate ) returns the date of the first day of dDate"
      @  6, 10 SAY "If dDate is 2002-10-15 (that's in yyyy-mm-dd), BoY()"
      @  7, 10 SAY "should return 2002-01-01. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The returned date is " + DToC( BoY( dDate ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE ctodowtest()

   // CToDoW( cDow )
   // ===============
   // Convert name of day of the week to its ordinal number
   // if cDow is invalid, returns 0

   LOCAL GetList := {}
   LOCAL cDow    := Space( 9 )
   LOCAL nkey

   DO WHILE .T.
      @  5, 10 SAY "CToDoW( dDate ) receives the name of a day of the week and "
      @  6, 10 SAY "returns a number representing its position in the week"
      @  7, 10 SAY "Sunday returns 1. Test it, Insert a day"

      @  9, 10 GET cDow

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The day number is " + hb_ntos( CToDoW( Upper( AllTrim( cDow ) ) ) )

      Inkey( 0 )

      cDow := Space( 9 )
   ENDDO

   RETURN

STATIC PROCEDURE ctomonthtest()

   // CToMonth( cDom )
   // =================
   // Convert the name of a month to its ordinal number. If cDom is
   // invalid, CToMonth() Returns 0.

   LOCAL GetList := {}
   LOCAL cDom    := Space( 9 )

   DO WHILE .T.
      @  5, 10 SAY "CToMonth( cDom ) receives the name of a month and returns"
      @  6, 10 SAY "a number representing its position in the month. April"
      @  7, 10 SAY "returns 4. Test it, Insert a month"

      @  9, 10 GET cDom PICTURE "!!!!!!!!!"

      READ

      cDom := AllTrim( cDom )

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The day number is " + hb_ntos( CToMonth( cDom ) )

      Inkey( 0 )

      cDom := Space( 9 )
   ENDDO

   RETURN

STATIC PROCEDURE dInMonthtest()

   // DaysInMonth( xDate, lleap )
   // ============================
   // Returns the number of days in nMonth, either whose name as a
   // string or month number is passed.

   LOCAL GetList := {}
   LOCAL cMonth  := Space( 9 )
   LOCAL nMonth
   LOCAL cLeap   := Space( 1 )

   DO WHILE .T.
      @  5, 10 SAY "DaysInMonth() receives either the number of a month or"
      @  6, 10 SAY "its name and returns the number of days in the month. "
      @  7, 10 SAY "April returns 30. Test it. Insert a month number"
      @  8, 10 SAY "Is it a leap year?"

      @  7, 60 GET cMonth PICTURE "XXXXXXXXX"
      @  8, 60 GET cLeap PICTURE "Y"

      READ

      cMonth := Upper( RTrim( cMonth ) )
      nMonth := Val( cMonth )

      DO CASE
      CASE ValType( cMonth ) == "C" .AND. nMonth == 0
         nMonth := CToMonth( cMonth )
      CASE nMonth <= 1 .OR. nMonth > 12
         LOOP
      ENDCASE

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 10, 40 SAY "The day number is " + ;
         hb_ntos( DaysInMonth( nMonth, IsAffirm( cLeap ) ) )

      Inkey( 0 )

      cMonth := Space( 9 )
      cLeap  := Space( 1 )
   ENDDO

   RETURN

STATIC PROCEDURE d2month()

   // DaysToMonth()
   // ==============
   // Total number days from first of Jan to beginning of nMonth.
   // lLeap is .F. for a non-leap year but .T. if it is. If so and
   // nMonth is greater than 2, ndays is incremented.

   LOCAL GetList := {}
   LOCAL cMonth  := Space( 2 )
   LOCAL cLeap   := Space( 1 )
   LOCAL nMonth

   DO WHILE .T.
      @  5, 10 SAY "DaysToMonth() receives the number a month and returns"
      @  6, 10 SAY "the number of days in the year up to that month. March"
      @  7, 10 SAY "returns 59 or 60 in a leap year. Test it."
      @  8, 10 SAY "Insert a month number"
      @  9, 10 SAY "Leap year Y/N?"

      @  8, 33 GET cMonth PICTURE "99"
      @  9, 33 GET cLeap PICTURE "Y"

      READ

      nMonth := Val( cMonth )

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The day number is " + ;
         hb_ntos( DaysToMonth( nMonth, IsAffirm( cLeap ) ) )

      Inkey( 0 )

      cMonth := Space( 2 )
      cLeap  := Space( 1 )
   ENDDO

   RETURN

STATIC PROCEDURE dmytest()

   // DMY( dDate, lMode )
   // ====================
   // Returns the date as a string in DD Month YY format. If lmode
   // is .T., a "." is inserted after the DD
   // This version does not observe the Nations module.

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()
   LOCAL cMode   := Space( 1 )

   DO WHILE .T.
      @  5, 10 SAY "DMY() receives a date and logical lmode. If lMode is"
      @  6, 10 SAY "is either missing or .F., DMY() returns the date as"
      @  7, 10 SAY "a string in DD Month YY format. If lMode is .T., a"
      @  8, 10 SAY "full stop or perod '.' is inserted after DD. Test it"
      @  9, 10 SAY "Insert a date"
      @ 10, 10 SAY "inssert a full stop Y/N?"

      @  9, 36 GET dDate
      @ 10, 45 GET cMode PICTURE "Y"

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 12, 10 SAY "The date string returned is " + ;
         LTrim( DMY( dDate, IsAffirm( cMode ) ) )

      Inkey( 0 )

      dDate := hb_SToD()
      cMode := Space( 1 )
   ENDDO

   RETURN

STATIC PROCEDURE doytest()

   // DoY( dDate )
   // =============
   // Determines the day of the year for a specific date
   // if dDate is invalid, returns 0

   LOCAL GetList := {}
   LOCAL dDate   := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "DoY( dDate ) returns the day of the year for the"
      @  6, 10 SAY "date passed. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The day of the date entered is " + ;
         hb_ntos( DoY( dDate ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE eomtest()

   // dBom := EoM( dDate )
   // =====================
   // Returns the last date in the month of the month appearing in
   // date.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "EoM( dDate ) returns the last date in the month of the"
      @  6, 10 SAY "month appearing in dDate. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The last date in the month is " + ;
         LTrim( DToC( EoM( dDate ) ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE eoqtest()

   // dret := EoQ( dDate )
   // =====================
   // Returns the last date in the quarter in which dDate falls.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "EoQ( dDate ) returns the last date in the quarter in"
      @  6, 10 SAY "which dDate falls. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The last date in the month is " + ;
         LTrim( DToC( EoQ( dDate ) ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE eoytest()

   // dEoY := EoY( dDate )
   // =====================
   // Returns the last date in the year of the year appearing in
   // date.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "EoY( dDate ) returns the last date in the year of the"
      @  6, 10 SAY "year appearing in dDate. Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The last date in the year is " + ;
         LTrim( DToC( EoY( dDate ) ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE isleaptest()

   // lRet := IsLeap( dDate )
   // ========================
   // if dDate is a leap year, lRet is .T., otherwise .F..
   // Leap years are exactly divisible by 4 and 1,000 but not 100.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "IsLeap( dDate ) returns TRUE if dDate is a leap year"
      @  6, 10 SAY "Test it, Insert a date"

      @  9, 10 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The result is " + ;
         PadR( iif( IsLeap( dDate ), "TRUE", "FALSE" ), 5 )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE lastdayomtest()

   // ndays := LastDayOM( xDate )
   // ============================
   // Returns the the number of days in the month.
   // xDate can be a date or a month number. If empty uses the
   // system date.
   // If xDate is invalid, returns 0

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()
   LOCAL cMth  := "  "

   DO WHILE .T.
      @  5, 10 SAY "LastDayOM( xDate ) returns the number of days in the"
      @  6, 10 SAY "month appearing in date. Or, if only a month number"
      @  7, 10 SAY "is passed, in that month. Test it. "
      @  9, 10 SAY "Insert a Date (or)"
      @ 10, 10 SAY "a month"

      @  9, 30 GET dDate
      @ 10, 38 GET cMth PICTURE "99" // VALID Val( cMth ) < 12

      READ

      IF Empty( dDate )
         dDate := Val( cMth )
      ENDIF

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 12, 10 SAY "The number of days in the month is " + ;
         hb_ntos( LastDayOM( dDate ) )

      Inkey( 0 )

      dDate := hb_SToD()
      cMth  := "  "
   ENDDO

   RETURN

STATIC PROCEDURE mdytest()

   // cDate := MDY( dDate )
   // ======================
   // Returns the date as a string in "Month DD, YY" or "Month DD, YYYY"
   // If dDate is NULL, the system date is used
   // This version does not observe the Nations module.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "MDY() receives a date. mdy returns the date as"
      @  6, 10 SAY "a string in month DD YY format. Test it"
      @  8, 10 SAY "Insert a date"

      @  8, 30 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The date string returned is " + ;
         LTrim( MDY( dDate ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE ntocdowtest()

   // cDay := NToCDoW( nDayNum )
   // ===========================
   // NToCDoW() receives the number of a day and returns its
   // name as a string. This version does not observe the Nations
   // module.

   LOCAL GetList := {}
   LOCAL cDay := Space( 2 )

   DO WHILE .T.
      @  5, 10 SAY "NToCDoW( n ) returns the name of the day number n"
      @  6, 10 SAY "Test it, Insert a day number"

      @  6, 60 GET cDay

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The day selected is " + ;
         PadR( NToCDoW( Val( cDay ) ), 10 )

      Inkey( 0 )

      cDay := Space( 2 )
   ENDDO

   RETURN

STATIC PROCEDURE ntocmthtest()

   // cMonth := NToCMonth( nMonthNum )
   // =================================
   // NToCMonth() receives the number of a month and returns its
   // name as a string. This version does not observe the Nations
   // module.

   LOCAL GetList := {}
   LOCAL cMonth := "  "

   DO WHILE .T.
      @  5, 10 SAY "NToCMonth( n ) returns the name of the month number n"
      @  6, 10 SAY "Test it, Insert a month number"

      @  6, 60 GET cMonth

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 11, 10 SAY "The month selected is " + ;
         PadR( NToCMonth( Val( cMonth ) ), 10 )

      Inkey( 0 )

      cMonth := "  "
   ENDDO

   RETURN

STATIC PROCEDURE qtrtest()

   // nqtr := Quarter( dDate )
   // =========================
   // Returns the quarter as a number. If no date is specified,
   // the system date is used.

   LOCAL GetList := {}
   LOCAL dDate  := hb_SToD()

   DO WHILE .T.
      @  5, 10 SAY "Quarter( dDate ) returns the number of the quarter"
      @  6, 10 SAY "Test it, Insert a date"

      @  6, 52 GET dDate

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @  8, 10 SAY "The quarter number is " + ;
         hb_ntos( Quarter( dDate ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN

STATIC PROCEDURE stodtest()

   // SToD( ansi-date )
   // ==================
   // Returns a Harbour format date. If Ansi date is invalid, a
   // null date is returned.

   LOCAL GetList := {}
   LOCAL dDate
   LOCAL cAnsidate := Space( 8 )

   DO WHILE .T.
      @  5, 10 SAY "SToD( ansi-date ) receives an ANSI date string and"
      @  6, 10 SAY "returns a Harbour format date"
      @  8, 10 SAY "Enter an ANSI date string in the form YYYYMMDD"

      @  8, 57 GET cAnsidate PICTURE "999999999" VALID chkansi( cAnsidate )

      READ

      dDate := SToD( cAnsidate )

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @ 10, 10 SAY "The Harbour format date is " + ;
         PadR( LTrim( DToC( dDate ) ), 10 )

      Inkey( 0 )

      cAnsidate := Space( 8 )
   ENDDO

   RETURN

STATIC FUNCTION chkansi( cAnsidate )

   LOCAL nYear  := Val( Left( cAnsidate, 4 ) )
   LOCAL nMonth := Val( SubStr( cAnsidate, 5, 2 ) )
   LOCAL nDay   := Val( Right( cAnsidate, 2 ) )

   RETURN ;
      nYear >= 1 .AND. ;
      nMonth >= 1 .AND. nMonth <= 12 .AND. ;
      nDay >= 0 .AND. nDay <= 31

STATIC PROCEDURE weektest()

   // nWeek := Week( dDate, lSWN )
   // =============================
   // Returns the calendar week as a number. If no date is specified,
   // the system date is used. An empty date viz hb_SToD()
   // returns 0.

   LOCAL GetList := {}
   LOCAL dDate := hb_SToD()
   LOCAL cMode := Space( 1 )

   DO WHILE .T.
      @  5, 10 SAY "Week( dDate, lSWN ) returns the calendar number of the week"
      @  6, 10 SAY "if lSWN == .T., the simple week number is returned"
      @  7, 10 SAY "if lSWN == .F. (default), the ISO8601 week number is returned"
      @  8, 10 SAY "Test it, Insert a date and <lSWN>"

      @  9, 52 GET dDate
      @ 10, 61 GET cMode PICTURE "Y"

      READ

      IF LastKey() == K_ESC
         EXIT
      ENDIF

      @  8, 10 SAY "The week number is " + ;
         hb_ntos( Week( dDate, IsAffirm( cMode ) ) )

      Inkey( 0 )

      dDate := hb_SToD()
   ENDDO

   RETURN
