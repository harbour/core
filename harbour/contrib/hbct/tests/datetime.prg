/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Program to test functions to mimic Clipper Tools III date & time functions.
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


#include "inkey.ch"
#include "setcurs.ch"

// TODO: add language module request(s) and an achoice to select different lang modules

//:

PROCEDURE Main()

   //:
   LOCAL cScr
   LOCAL nchoice
   LOCAL c    := .T.
   LOCAL farr := { "addmonth( ddate )  Add a month to ddate     ", ;
      "bom()              Beginning of month       ", ;
      "boq()              Returns first date of qtr", ;
      "boy()              Beginning of year        ", ;
      "ctodow()           Day name to day number   ", ;
      "ctomonth()         Month name to number     ", ;
      "daysInMonth()      number of days in xMonth ", ;
      "daystomonth(ddate) Returns num days TO month", ;
      "dmy( ddate, lmode) date as DD month YY      ", ;
      "doy( ddate )       Returns day of the year  ", ;
      "eom( ddate )       Returns last day of month", ;
      "eoq( ddate )       Returns last date of qtr ", ;
      "eoy( ddate )       Returns last day of year ", ;
      "isleap( ddate )    Returns .T. if leap year ", ;
      "lastdayom( ddate ) Returns num days in month", ;
      "mdy( dDate )       Returns stg Month DD, YY ", ;
      "ntocdow( nDay )    Returns name of day      ", ;
      "ntocmonth( nMth )  Returns name of month    ", ;
      "quarter(date)      Returns qtr number of date", ;
      "stod( ansi date)    Returns Clipper date     ", ;
      "week( ddate, lSWN ) Returns numbef of week   ", }

   SET DATE ANSI
   SET CENTURY ON

   cls

   DO WHILE c
      cScr := SaveScreen( 4, 5, 21, 66 )

      @  4, 5 TO 21, 66
      nchoice := AChoice( 5, 7, 20, 65, farr )   //, ,1)

      IF Empty( nchoice )
         RETURN
      ENDIF

      @ 0, 0 CLEAR TO MaxRow(), MaxCol()

      DO CASE
      CASE nchoice == 1
         addmtest()
      CASE nchoice == 2
         bomtest()
      CASE nchoice == 3
         boqtest()
      CASE nchoice == 4
         boytest()
      CASE nchoice == 5
         ctodowtest()
      CASE nchoice == 6
         ctomonthtest()
      CASE nchoice == 7
         dInMonthtest()
      CASE nchoice == 8
         d2month()
      CASE nchoice == 9
         dmytest()
      CASE nchoice == 10
         doytest()
      CASE nchoice == 11
         eomtest()
      CASE nchoice == 12
         eoqtest()
      CASE nchoice == 13
         eoytest()
      CASE nchoice == 14
         isleaptest()
      CASE nchoice == 15
         lastdayomtest()
      CASE nchoice == 16
         mdytest()
      CASE nchoice == 17
         ntocdowtest()
      CASE nchoice == 18
         ntocmthtest()
      CASE nchoice == 19
         qtrtest()
      CASE nchoice == 20
         stodtest()
      CASE nchoice == 21
         weektest()
      ENDCASE

      RestScreen( 4, 5, 21, 66, cScr )
   ENDDO

   RETURN

//:

FUNCTION addmtest()

   //:
   //: addmonth(ddate, nMonths)
   //: =========================
   //: This version will only accept an nMonths value of from 0 to 70

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL c       := .T.
   LOCAL mnth    := 0
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "addmonth(ddate) returns the date incremented by"
      @ 6, 10 SAY "the number of days in months."
      @ 7, 10 SAY "Insert a date"
      @ 8, 10 SAY "How many months"
      @ 7, 60 GET ddate
      @ 8, 60 GET mnth PICTURE "99"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 39 SAY "The returned date is " + DToC( addmonth( ddate, mnth ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION bomtest()

   //:
   //: bom( ddate )
   //: =============

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL c       := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "bom(ddate) returns the date of the first day of ddate"
      @ 6, 10 SAY "If ddate is 15/10/2002(that's in dd/mm/yyy), bom()"
      @ 7, 10 SAY "should return 01/10/2002. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The returned date is " + DToC( bom( ddate ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION boqtest()

   //:
   //: bom( ddate )
   //: =============

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL c       := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "boq(ddate) returns the date of the first day of the"
      @ 6, 10 SAY "quarter in which ddate is situated. If ddate is"
      @ 7, 10 SAY "15/10/2002 (that's in dd/mm/yyy), BOQ() should return"
      @ 8, 10 SAY "01/09/2002. Test it, Insert a date"

      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The returned date is " + DToC( boq( ddate ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION boytest()

   //:
   //: boy( ddate )
   //: =============

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL c       := .T.
   LOCAL nkey

   DO WHILE c
      @ 5, 10 SAY "boy(ddate) returns the date of the first day of ddate"
      @ 6, 10 SAY "If ddate is 15/10/2002(that's in dd/mm/yyy), boy()"
      @ 7, 10 SAY "should return 01/01/2002. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The returned date is " + DToC( boy( ddate ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate   := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION ctodowtest()

   //:
   //: ctodow( cDow )
   //: ==============
   //: Convert name of day of the week to its ordinal number
   //: if cDow is invalid, returns 0
   //: English day names only.

   LOCAL getlist := {}
   LOCAL cDow    := Space( 9 )
   LOCAL nkey
   LOCAL c       := .T.

   DO WHILE c
      @ 5, 10 SAY "ctodow(ddate) receives the name of a day of the week and "
      @ 6, 10 SAY "returns a number representing its position in the week"
      @ 7, 10 SAY "Sunday returns 1. Test it, Insert a day"
      @ 9, 10 GET cDow

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The day number is " + Str( ctodow( Upper( AllTrim( cDow ) ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cDow := Space( 9 )

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION ctomonthtest()

   //:
   //: ctomonth(cDom )
   //: ===============
   //: Convert the name of a month to its ordinal number. If cMonth is
   //: invalid, ctomonth() Returns 0. English month names only

   LOCAL getlist := {}
   LOCAL cDom    := Space( 9 )
   LOCAL c       := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "ctomonth(ddate) receives the name of a month and returns"
      @ 6, 10 SAY "a number representing its position in the month. April"
      @ 7, 10 SAY "returns 4. Test it, Insert a month"
      @ 9, 10 GET cDom PICTURE "!!!!!!!!!"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      cDom := AllTrim( cDom )

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The day number is " + Str( ctomonth( cDom ) )
         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cDom := Space( 9 )
      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION dInMonthtest()

   //:
   //: daysInMonth( xDate, lleap )
   //: ============================
   //: Returns the number of days in nMonth, either whose name as a
   //: string or month number is passed. English Month names only.

   LOCAL getlist := {}
   LOCAL cMonth  := Space( 9 )
   LOCAL c       := .T.
   LOCAL nMonth
   LOCAL nKey
   LOCAL cLeap   := "N"
   LOCAL lleap   := .F.

   DO WHILE c
      @ 5, 10 SAY "daysInmonth() receives either the number of a month or"
      @ 6, 10 SAY "its name and returns the number of days in the month. "
      @ 7, 10 SAY "April returns 30. Test it. Insert a month number"
      @ 8, 10 SAY "Is it a leap year?"
      @ 7, 60 GET cMonth PICTURE "XXXXXXXXX"
      @ 8, 60 GET cleap PICTURE "Y"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      cMonth := Upper( RTrim( cMonth ) )
      nMonth := Val( cMonth )

      DO CASE
      CASE ValType( cMonth ) == "C" .AND. nmonth == 0
         nMonth := ctomonth( cMonth )

      CASE nMonth == 0  .OR. ;
            nMonth  > 12

         LOOP
      ENDCASE

      IF cLeap == "Y"
         lleap := .T.
      ENDIF

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 10, 40 SAY "The day number is " + ;
            LTrim( Str( daysInMonth( nMonth, lLeap ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cMonth := Space( 9 )
         cLeap  := "N"

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION d2month()

   //:
   //: daystomonth()
   //: =============
   //: Total number days from first of Jan to beginning of nMonth.
   //: lLeap is .F. for a non-leap year but .T. if it is. If so and
   //: nMonth is greater than 2, ndays is incremented.

   LOCAL getlist := {}
   LOCAL cMonth  := Space( 2 )
   LOCAL cLeap   := "N"
   LOCAL c       := .T.
   LOCAL nMonth
   LOCAL lLeap
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "daystomonth() receives the number a month and returns"
      @ 6, 10 SAY "the number of days in the year up to that month. March"
      @ 7, 10 SAY "returns 59 or 60 in a leap year. Test it."
      @ 8, 10 SAY "Insert a month number"
      @ 9, 10 SAY "Leap year Y/N?"
      @ 8, 33 GET cMonth PICTURE "99"
      @ 9, 33 GET cLeap PICTURE "Y"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nMonth := Val( cMonth )

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         IF cLeap == "Y"
            lLeap := .T.
         ENDIF

         @ 11, 10 SAY "The day number is " +  ;
            LTrim( Str( daystomonth( nMonth, lLeap ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cMonth := Space( 2 )
         cLeap  := "N"

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION dmytest()

   //:
   //: dmy( ddate, lmode)
   //: ===================
   //: Returns the date as a string in DD Month YY format. If lmode
   //: is .T., a "." is inserted after the DD
   //: This version does not observe the Nations module. English
   //: only.

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL cMode   := Space( 1 )
   LOCAL lmode
   LOCAL c       := .T.
   LOCAL nKey

   DO WHILE c
      @  5, 10 SAY "dmy() receives a date and logical lmode. If lmode is"
      @  6, 10 SAY "is either missing or FALSE, dmy returns the date as"
      @  7, 10 SAY "a string in DD Month YY format. If lmode is TRUE, a"
      @  8, 10 SAY "full stop or perod '.' is inserted after DD. Test it"
      @  9, 10 SAY "Insert a date"
      @ 10, 10 SAY "inssert a full stop Y/N?"
      @  9, 36 GET ddate PICTURE "@D   /  /    "
      @ 10, 45 GET cMode PICTURE "Y"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         iif( cMode == "Y", lMode := .T. , lMode := .F. )

         @ 12, 10 SAY "The date string returned is " + ;
            LTrim( dmy( ddate, lmode ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()
         cMode := Space( 1 )

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION doytest()

   //:
   //: doy( ddate )
   //: =============
   //: Determines the day of the year for a specific date
   //: if dDate is invalid, returns 0

   LOCAL getlist := {}
   LOCAL ddate   := SToD()
   LOCAL c       := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "doy(ddate) returns the day of the year for the"
      @ 6, 10 SAY "date passed. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE
         @ 11, 10 SAY "The day of the date entered is " + ;
            LTrim( Str( doy( ddate ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION eomtest()

   //:
   //: dBom := eom( ddate )
   //: =====================
   //: Returns the last date in the month of the month appearing in
   //: date.

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "eom(ddate) returns the last date in the month of the"
      @ 6, 10 SAY "month appearing in ddate. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The last date in the month is " + ;
            LTrim( DToC( eom( ddate ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION eoqtest()

   //:
   //: dret := eoq( ddate )
   //: =====================
   //: Returns the last date in the quarter in which ddate falls.

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "EOQ(ddate) returns the last date in the quarter in"
      @ 6, 10 SAY "which ddate falls. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The last date in the month is " + ;
            LTrim( DToC( eoq( ddate ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION eoytest()

   //:
   //: dEoy := eoy( ddate )
   //: =====================
   //: Returns the last date in the year of the year appearing in
   //: date.

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "eoy(ddate) returns the last date in the year of the"
      @ 6, 10 SAY "year appearing in ddate. Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The last date in the year is " + ;
            LTrim( DToC( eoy( ddate ) ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION isleaptest()

   //:
   //:  lRet :=  isleap( ddate )
   //:  ========================
   //:  if ddate is a leap year, lRet is .T., otherwise .F..
   //:  Leap years are exactly divisible by 4 and 1,000 but not 100.

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL cResult
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "isleap(ddate) returns TRUE if ddate is a leap year"
      @ 6, 10 SAY "Test it, Insert a date"
      @ 9, 10 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         IF isleap( ddate )
            cResult := "TRUE "
         ELSE
            cResult := "FALSE"
         ENDIF

         @ 11, 10 SAY "The result is " + cResult

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION lastdayomtest()

   //:
   //:   ndays := lastdayom( xDate )
   //:   ===========================
   //: Returns the the number of days in the month.
   //: xDate can be a date or a month number. If empty uses the
   //: system date.
   //: If xDate is invalid, returns 0

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL nKey
   LOCAL cMth  := "  "

   DO WHILE c
      @  5, 10 SAY "lastdayom(xDate) returns the number of days in the"
      @  6, 10 SAY "month appearing in date. Or, if only a month number"
      @  7, 10 SAY "is passed, in that month. Test it. "
      @  9, 10 SAY "Insert a date(or)"
      @ 10, 10 SAY "a month"

      @  9, 30 GET ddate
      @ 10, 38 GET cMth PICTURE "99"       //valid val(cmth) < 12

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         iif( Empty( ddate ), ddate := Val( cMth ), ddate )

         @ 12, 10 SAY "The number of days in the month is " + ;
            LTrim( Str( lastdayom( ddate ) ) )
         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()
         cMth  := "  "

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION mdytest()

   //:
   //:  cDate := mdy( dDate )
   //:  ======================
   //: Returns the date as a string in "Month DD, YY" or "Month DD, YYYY"
   //: If dDate is NULL, the system date is used
   //: This version does not observe the Nations module. English only.

   LOCAL getlist := {}
   LOCAL ddate := SToD()
   LOCAL c     := .T.
   LOCAL nKey

   DO WHILE c
      @  5, 10 SAY "mdy() receives a date. mdy returns the date as"
      @  6, 10 SAY "a string in month DD YY format.  Test it"
      @  8, 10 SAY "Insert a date"
      @  8, 30 GET ddate PICTURE "@D   /  /  "

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The date string returned is " + ;
            LTrim( mdy( ddate ) )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION ntocdowtest()

   //:
   //:  cDay :=  ntocdow( nDayNum )
   //:  ==================================
   //:  ntocdow() receives the number of a day and returns its
   //:  name as a string. This version does not observe the Nations
   //:  module. English only.

   LOCAL getlist := {}
   LOCAL cDay := "  "
   LOCAL c      := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "ntocdow(n) returns the name of the day number n"
      @ 6, 10 SAY "Test it, Insert a day number"
      @ 6, 60 GET cDay

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The day selected is " + ;
            PadR( ntocdow( Val( cDay ) ), 10 )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cDay := "  "

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION ntocmthtest()

   //:
   //:  cMonth :=  ntocmonth( nMonthNum )
   //:  ==================================
   //:  ntocmonth() receives the number of a month and returns its
   //:  name as a string. This version does not observe the Nations
   //:  module. English only.

   LOCAL getlist := {}
   LOCAL cMonth := "  "
   LOCAL c      := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "ntocmonth(n) returns the name of the month number n"
      @ 6, 10 SAY "Test it, Insert a month number"
      @ 6, 60 GET cMonth

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @ 11, 10 SAY "The month selected is " + ;
            PadR( ntocmonth( Val( cMonth ) ), 10 )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cMonth := "  "

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION qtrtest()

   //:
   //:  nqtr := quarter( ddate )
   //:  ===========================
   //:  Returns the quarter as a number. If no date is specified,
   //:  the system date is used.

   LOCAL getlist := {}
   LOCAL ddate  := SToD()
   LOCAL c      := .T.
   LOCAL nKey

   DO WHILE c
      @ 5, 10 SAY "quarter(ddate) returns the number of the quarter"
      @ 6, 10 SAY "Test it, Insert a date"
      @ 6, 52 GET ddate

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @  8, 10 SAY "The quarter number is " + ;
            PadR( LTrim( Str( quarter( ddate ) ) ), 10 )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate  := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL

//:

FUNCTION stodtest()

   //:
   //:  stod( ansi-date)
   //:  ================
   //:  Returns a Clipper format date. If Ansi date is invalid, a
   //:  null date is returned.

   LOCAL getlist   := {}
   LOCAL ddate
   LOCAL cAnsidate := Space( 8 )
   LOCAL c         := .T.
   LOCAL nKey

   DO WHILE c
      @  5, 10 SAY "stod( ansi-date ) receives an ANSI date string and"
      @  6, 10 SAY "returns a Clipper format date"
      @  8, 10 SAY "Enter an ANSI date string in the form YYYYMMDD"
      @  8, 57 GET cAnsidate PICTURE "999999999" VALID chkansi( cAnsidate )

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         ddate := SToD( cAnsidate )

         @ 10, 10 SAY "The Clipper format date is " + ;
            PadR( LTrim( DToC( ddate ) ), 10 )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         cAnsidate := Space( 8 )

      ENDIF

   ENDDO

   RETURN NIL

//:

FUNCTION chkansi( cAnsidate )

   //:
   LOCAL nYear
   LOCAL nMonth
   LOCAL nDay
   LOCAL lretval := .T.

   nYear  := Val( Left( cAnsidate, 4 ) )
   nMonth := Val( SubStr( cAnsidate, 5, 2 ) )
   nDay   := Val( Right( cAnsidate, 2 ) )

   DO CASE

   CASE nYear < 1
      lretval := .F.
   CASE nMonth < 1 .OR. nMonth > 12
      lretval := .F.
   CASE nday < 0 .OR. nday > 31
      lretval := .F.
   ENDCASE

   RETURN lretval

//:

FUNCTION weektest()

   //:
   //:  nWeek := week( ddate, lSWN )
   //:  =============================
   //:  Returns the calendar week as a number. If no date is specified,
   //:  the system date is used. An empty date viz stod()
   //:  returns 0.

   LOCAL getlist := {}
   LOCAL ddate  := SToD()
   LOCAL c      := .T.
   LOCAL nKey
   LOCAL cMode := Space( 1 )

   DO WHILE c
      @ 5, 10 SAY "week(ddate, lSWN) returns the calendar number of the week"
      @ 6, 10 SAY "if lSWN == .T., the simple week number is returned"
      @ 7, 10 SAY "if lSWN == .F.(default), the ISO8601 week number is returned"

      @ 8, 10 SAY "Test it, Insert a date and <lSWN>"
      @ 9, 52 GET ddate
      @ 10, 61 GET cMode PICTURE "Y"

      SET CONFIRM ON
      SET ESCAPE ON
      READ
      SET ESCAPE OFF
      SET CONFIRM ON

      nKey := LastKey()

      IF nKey == K_ESC
         c := .F.
      ELSE

         @  8, 10 SAY "The week number is " + ;
            PadR( LTrim( Str( week( ddate, cMode == "Y" ) ) ), 10 )

         SET CURSOR OFF
         Inkey( 0 )
         SET CURSOR ON

         ddate  := SToD()

      ENDIF

   ENDDO

   @ 0, 0 clear

   RETURN NIL
