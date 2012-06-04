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


*:--------------------------------------------------------------------
PROCEDURE main ()
*:--------------------------------------------------------------------
local cScr
local nchoice
local c    := .T.
local farr := { "addmonth ( ddate )  Add a month to ddate     ", ;
                "bom ()              Beginning of month       ", ;
                "boq ()              Returns first date of qtr", ;
                "boy ()              Beginning of year        ", ;
                "ctodow ()           Day name to day number   ", ;
                "ctomonth ()         Month name to number     ", ;
                "daysInMonth ()      number of days in xMonth ", ;
                "daystomonth (ddate) Returns num days TO month", ;
                "dmy ( ddate, lmode) date as DD month YY      ", ;
                "doy ( ddate )       Returns day of the year  ", ;
                "eom ( ddate )       Returns last day of month", ;
                "eoq ( ddate )       Returns last date of qtr ", ;
                "eoy ( ddate )       Returns last day of year ", ;
                "isleap ( ddate )    Returns .T. if leap year ", ;
                "lastdayom ( ddate ) Returns num days in month", ;
                "mdy ( dDate )       Returns stg Month DD, YY ", ;
                "ntocdow ( nDay )    Returns name of day      ", ;
                "ntocmonth ( nMth )  Returns name of month    ", ;
                "quarter (date)      Returns qtr number of date", ;
                "stod( ansi date)    Returns Clipper date     ", ;
                "week( ddate, lSWN ) Returns numbef of week   ", }

   set date british
   set century on

   cls

   do while c
      cScr := savescreen ( 4, 5, 21,66 )

      @  4, 5 to 21, 66
      nchoice := achoice ( 5, 7,  20,  65, farr )   //, ,1)

      if empty ( nchoice )
         return
      endif

      @ 0, 0 clear to maxrow(), maxcol()

      do case
         case nchoice == 1
              addmtest()
         case nchoice == 2
              bomtest ()
         case nchoice == 3
              boqtest ()
         case nchoice == 4
              boytest ()
         case nchoice == 5
              ctodowtest ()
         case nchoice == 6
              ctomonthtest ()
         case nchoice == 7
              dInMonthtest ()
         case nchoice == 8
              d2month ()
         case nchoice == 9
              dmytest ()
         case nchoice == 10
              doytest ()
         case nchoice == 11
              eomtest ()
         case nchoice == 12
              eoqtest ()
         case nchoice == 13
              eoytest ()
         case nchoice == 14
              isleaptest ()
         case nchoice == 15
              lastdayomtest ()
         case nchoice == 16
              mdytest ()
         case nchoice == 17
              ntocdowtest ()
         case nchoice == 18
              ntocmthtest ()
         case nchoice == 19
              qtrtest ()
         case nchoice == 20
              stodtest()
         case nchoice == 21
              weektest ()
      endcase

      restscreen ( 4, 5, 21, 66, cScr)
   enddo

   return

*:--------------------------------------------------------------------
FUNCTION addmtest()
*:--------------------------------------------------------------------
*: addmonth (ddate, nMonths)
*: =========================
*: This version will only accept an nMonths value of from 0 to 70

local getlist := {}
local ddate   := ctod ("  /  /    ")
local c       := .T.
local mnth    := 0
local nKey

do while c
   @ 5, 10 say "addmonth (ddate) returns the date incremented by"
   @ 6, 10 say "the number of days in months."
   @ 7, 10 say "Insert a date"
   @ 8, 10 say "How many months"
   @ 7, 60 get ddate
   @ 8, 60 get mnth picture "99"

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else

      @ 11, 39 say "The returned date is " + dtoc ( addmonth (ddate, mnth) )

      set cursor off
      inkey(0)
      set cursor on

   endif

 enddo

 @ 0, 0 clear

 return NIL

*:--------------------------------------------------------------------
FUNCTION bomtest()
*:--------------------------------------------------------------------
*: bom ( ddate )
*: =============

local getlist := {}
local ddate   := ctod ("  /  /    ")
local c       := .T.
local nKey

do while c
   @ 5, 10 say "bom (ddate) returns the date of the first day of ddate"
   @ 6, 10 say "If ddate is 15/10/2002 (that's in dd/mm/yyy), bom ()"
   @ 7, 10 say "should return 01/10/2002. Test it, Insert a date"
   @ 9, 10 get ddate

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      @ 11, 10 say "The returned date is " + dtoc ( bom (ddate) )

      set cursor off
      inkey(0)
      set cursor on

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:--------------------------------------------------------------------
FUNCTION boqtest ()
*:--------------------------------------------------------------------
*: bom ( ddate )
*: =============

local getlist := {}
local ddate   := ctod ("  /  /    ")
local c       := .T.
local nKey

   do while c
      @ 5, 10 say "boq (ddate) returns the date of the first day of the"
      @ 6, 10 say "quarter in which ddate is situated. If ddate is"
      @ 7, 10 say "15/10/2002  (that's in dd/mm/yyy), BOQ () should return"
      @ 8, 10 say "01/09/2002. Test it, Insert a date"

      @ 9, 10 get ddate

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else
         @ 11, 10 say "The returned date is " + dtoc ( boq (ddate) )

         set cursor off
         inkey(0)
         set cursor on

      endif

    enddo

    @ 0, 0 clear

    return NIL


*:--------------------------------------------------------------------
FUNCTION boytest ()
*:--------------------------------------------------------------------
*: boy ( ddate )
*: =============

local getlist := {}
local ddate   := ctod ("  /  /    ")
local c       := .T.
local nkey

do while c
   @ 5, 10 say "boy (ddate) returns the date of the first day of ddate"
   @ 6, 10 say "If ddate is 15/10/2002 (that's in dd/mm/yyy), boy ()"
   @ 7, 10 say "should return 01/01/2002. Test it, Insert a date"
   @ 9, 10 get ddate

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      @ 11, 10 say "The returned date is " + dtoc ( boy (ddate) )

      set cursor off
      inkey(0)
      set cursor on

      ddate   := ctod ("  /  /    ")

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION ctodowtest ()
*:----------------------------------------------------------------
*: ctodow ( cDow)
*: ==============
*: Convert name of day of the week to its ordinal number
*: if cDow is invalid, returns 0
*: English day names only.

local getlist := {}
local cDow    := space (9)
local nkey
local c       := .T.

do while c
   @ 5, 10 say "ctodow (ddate) receives the name of a day of the week and "
   @ 6, 10 say "returns a number representing its position in the week"
   @ 7, 10 say "Sunday returns 1. Test it, Insert a day"
   @ 9, 10 get cDow

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      @ 11, 10 say "The day number is " + str ( ctodow ( upper (alltrim (cDow))) )

      set cursor off
      inkey(0)
      set cursor on

      cDow := space (9)

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION ctomonthtest ()
*:----------------------------------------------------------------
*: ctomonth (cDom )
*: ===============
*: Convert the name of a month to its ordinal number. If cMonth is
*: invalid, ctomonth() Returns 0. English month names only

local getlist := {}
local cDom    := space (9)
local c       := .T.
local nKey

do while c
   @ 5, 10 say "ctomonth (ddate) receives the name of a month and returns"
   @ 6, 10 say "a number representing its position in the month. April"
   @ 7, 10 say "returns 4. Test it, Insert a month"
   @ 9, 10 get cDom picture "!!!!!!!!!"

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   cDom := alltrim ( cDom )

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      @ 11, 10 say "The day number is " + str ( ctomonth ( cDom) )
      set cursor off
      inkey(0)
      set cursor on

      cDom := space (9)
   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION dInMonthtest ()
*:----------------------------------------------------------------
*: daysInMonth ( xDate, lleap )
*: ============================
*: Returns the number of days in nMonth, either whose name as a
*: string or month number is passed. English Month names only.

local getlist := {}
local cMonth  := space (9)
local c       := .T.
local nMonth
local nKey
local cLeap   := "N"
local lleap   := .F.

   do while c
      @ 5, 10 say "daysInmonth () receives either the number of a month or"
      @ 6, 10 say "its name and returns the number of days in the month. "
      @ 7, 10 say "April returns 30. Test it. Insert a month number"
      @ 8, 10 say "Is it a leap year?"
      @ 7, 60 get cMonth picture "XXXXXXXXX"
      @ 8, 60 get cleap picture "Y"

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      cMonth := upper (rtrim (cMonth))
      nMonth := val (cMonth)

      do case
         case valtype ( cMonth ) == "C" .and. nmonth == 0
              nMonth := ctomonth ( cMonth)

         case nMonth == 0  .or. ;
              nMonth  > 12

              loop
      endcase

     if cLeap == "Y"
         lleap := .T.
      endif

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         @ 10, 40 say "The day number is " + ;
                                   ltrim (str ( daysInMonth ( nMonth, lLeap )))

         set cursor off
         inkey(0)
         set cursor on

         cMonth := space (9)
         cLeap  := "N"

      endif

    enddo

    @ 0, 0 clear

    return NIL


*:----------------------------------------------------------------
FUNCTION d2month ()
*:----------------------------------------------------------------
*: daystomonth()
*: =============
*: Total number days from first of Jan to beginning of nMonth.
*: lLeap is .F. for a non-leap year but .T. if it is. If so and
*: nMonth is greater than 2, ndays is incremented.

local getlist := {}
local cMonth  := space (2)
local cLeap   := "N"
local c       := .T.
local nMonth
local lLeap
local nKey

do while c
   @ 5, 10 say "daystomonth () receives the number a month and returns"
   @ 6, 10 say "the number of days in the year up to that month. March"
   @ 7, 10 say "returns 59 or 60 in a leap year. Test it."
   @ 8, 10 say "Insert a month number"
   @ 9, 10 say "Leap year Y/N?"
   @ 8, 33 get cMonth picture "99"
   @ 9, 33 get cLeap picture "Y"

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nMonth := val ( cMonth )

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      if cLeap == "Y"
         lLeap := .T.
      endif

      @ 11, 10 say "The day number is " +  ;
                             ltrim (str ( daystomonth ( nMonth, lLeap )))

      set cursor off
      inkey(0)
      set cursor on

      cMonth := space (2)
      cLeap  := "N"

   endif

 enddo

 @ 0, 0 clear

 return NIL

*:----------------------------------------------------------------
FUNCTION dmytest ()
*:----------------------------------------------------------------
*: dmy ( ddate, lmode)
*: ===================
*: Returns the date as a string in DD Month YY format. If lmode
*: is .T., a "." is inserted after the DD
*: This version does not observe the Nations module. English
*: only.

local getlist := {}
local ddate   := ctod ("  /  /    ")
local cMode   := space (1)
local lmode
local c       := .T.
local nKey

   do while c
      @  5, 10 say "dmy() receives a date and logical lmode. If lmode is"
      @  6, 10 say "is either missing or FALSE, dmy returns the date as"
      @  7, 10 say "a string in DD Month YY format. If lmode is TRUE, a"
      @  8, 10 say "full stop or perod '.' is inserted after DD. Test it"
      @  9, 10 say "Insert a date"
      @ 10, 10 say "inssert a full stop Y/N?"
      @  9, 36 get ddate picture "@D   /  /    "
      @ 10, 45 get cMode picture "Y"

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else
         if (cMode == "Y", lMode := .T., lMode := .F.)

         @ 12, 10 say "The date string returned is " + ;
                           ltrim (dmy ( ddate, lmode ))

         set cursor off
         inkey(0)
         set cursor on

         ddate := ctod ("  /  /    ")
         cMode := space (1)

      endif

    enddo

    @ 0, 0 clear

    return NIL

*:----------------------------------------------------------------
FUNCTION doytest ()
*:----------------------------------------------------------------
*: doy ( ddate )
*: =============
*: Determines the day of the year for a specific date
*: if dDate is invalid, returns 0

local getlist := {}
local ddate   := ctod ("  /  /    ")
local c       := .T.
local nKey

do while c
   @ 5, 10 say "doy (ddate) returns the day of the year for the"
   @ 6, 10 say "date passed. Test it, Insert a date"
   @ 9, 10 get ddate

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else
      @ 11, 10 say "The day of the date entered is " + ;
                         ltrim ( str (doy ( ddate )))

      set cursor off
      inkey(0)
      set cursor on

      ddate := ctod ("  /  /    ")

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION eomtest ()
*:----------------------------------------------------------------
*: dBom := eom ( ddate )
*: =====================
*: Returns the last date in the month of the month appearing in
*: date.

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local nKey

do while c
   @ 5, 10 say "eom (ddate) returns the last date in the month of the"
   @ 6, 10 say "month appearing in ddate. Test it, Insert a date"
   @ 9, 10 get ddate

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else

      @ 11, 10 say "The last date in the month is " + ;
                             ltrim ( dtoc (eom ( ddate )))

      set cursor off
      inkey(0)
      set cursor on

      ddate := ctod ("  /  /    ")

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION eoqtest ()
*:----------------------------------------------------------------
*: dret := eoq ( ddate )
*: =====================
*: Returns the last date in the quarter in which ddate falls.

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local nKey

   do while c
      @ 5, 10 say "EOQ (ddate) returns the last date in the quarter in"
      @ 6, 10 say "which ddate falls. Test it, Insert a date"
      @ 9, 10 get ddate

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         @ 11, 10 say "The last date in the month is " + ;
                                ltrim ( dtoc (eoq ( ddate )))

         set cursor off
         inkey(0)
         set cursor on

         ddate := ctod ("  /  /    ")

      endif

    enddo

    @ 0, 0 clear

    return NIL


*:----------------------------------------------------------------
FUNCTION eoytest ()
*:----------------------------------------------------------------
*: dEoy := eoy ( ddate )
*: =====================
*: Returns the last date in the year of the year appearing in
*: date.

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local nKey

   do while c
      @ 5, 10 say "eoy (ddate) returns the last date in the year of the"
      @ 6, 10 say "year appearing in ddate. Test it, Insert a date"
      @ 9, 10 get ddate

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         @ 11, 10 say "The last date in the year is " + ;
                                ltrim ( dtoc (eoy ( ddate )))

         set cursor off
         inkey(0)
         set cursor on

         ddate := ctod ("  /  /    ")

      endif

   enddo

   @ 0, 0 clear

   return NIL


*:----------------------------------------------------------------
FUNCTION isleaptest ()
*:----------------------------------------------------------------
*:  lRet :=  isleap ( ddate )
*:  ========================
*:  if ddate is a leap year, lRet is .T., otherwise .F..
*:  Leap years are exactly divisible by 4 and 1,000 but not 100.

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local cResult
local nKey

   do while c
      @ 5, 10 say "isleap (ddate) returns TRUE if ddate is a leap year"
      @ 6, 10 say "Test it, Insert a date"
      @ 9, 10 get ddate

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         if isleap ( ddate )
            cResult := "TRUE "
         else
            cResult := "FALSE"
         endif

         @ 11, 10 say "The result is " + cResult

         set cursor off
         inkey(0)
         set cursor on

         ddate := ctod ("  /  /    ")

      endif

    enddo

    @ 0, 0 clear

    return NIL

*:----------------------------------------------------------------
FUNCTION lastdayomtest ()
*:----------------------------------------------------------------
*:   ndays := lastdayom ( xDate )
*:--------------------------------------------------------------
*: Returns the the number of days in the month.
*: xDate can be a date or a month number. If empty uses the
*: system date.
*: If xDate is invalid, returns 0

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local nKey
local cMth  := "  "

 do while c
    @  5, 10 say "lastdayom (xDate) returns the number of days in the"
    @  6, 10 say "month appearing in date. Or, if only a month number"
    @  7, 10 say "is passed, in that month. Test it. "
    @  9, 10 say "Insert a date (or)"
    @ 10, 10 say "a month"

    @  9, 30 get ddate
    @ 10, 38 get cMth picture "99"       //valid val (cmth) < 12

    set confirm on
    set escape on
    read
    set escape off
    set confirm on

    nKey := lastkey()

    if nKey == K_ESC
       c := .F.
    else

       if (empty (ddate), ddate := val (cMth), ddate)

       @ 12, 10 say "The number of days in the month is " + ;
                     ltrim (str (lastdayom ( ddate )))
       set cursor off
       inkey(0)
       set cursor on

       ddate := ctod ("  /  /    ")
       cMth  := "  "

    endif

 enddo

 @ 0, 0 clear

 return NIL

*:----------------------------------------------------------------
FUNCTION mdytest ()
*:----------------------------------------------------------------
*:  cDate := mdy ( dDate )
*:  ======================
*: Returns the date as a string in "Month DD, YY" or "Month DD, YYYY"
*: If dDate is NULL, the system date is used
*: This version does not observe the Nations module. English only.

local getlist := {}
local ddate := ctod ("  /  /    ")
local c     := .T.
local nKey

  do while c
     @  5, 10 say "mdy() receives a date. mdy returns the date as"
     @  6, 10 say "a string in month DD YY format.  Test it"
     @  8, 10 say "Insert a date"
     @  8, 30 get ddate picture "@D   /  /  "

     set confirm on
     set escape on
     read
     set escape off
     set confirm on

     nKey := lastkey()

     if nKey == K_ESC
        c := .F.
     else

        @ 11, 10 say "The date string returned is " + ;
                                ltrim (mdy ( ddate ))

        set cursor off
        inkey(0)
        set cursor on

        ddate := ctod ("  /  /    ")

     endif

  enddo

  @ 0, 0 clear

  return NIL


*:----------------------------------------------------------------
FUNCTION ntocdowtest ()
*:----------------------------------------------------------------
*:  cDay :=  ntocdow ( nDayNum )
*:  ==================================
*:  ntocdow() receives the number of a day and returns its
*:  name as a string. This version does not observe the Nations
*:  module. English only.

local getlist := {}
local cDay := "  "
local c      := .T.
local nKey

do while c
   @ 5, 10 say "ntocdow(n) returns the name of the day number n"
   @ 6, 10 say "Test it, Insert a day number"
   @ 6, 60 get cDay

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else

      @ 11, 10 say "The day selected is " + ;
                        padr (ntocdow ( val (cDay) ), 10)

      set cursor off
      inkey(0)
      set cursor on

      cDay := "  "

   endif

 enddo

 @ 0, 0 clear

 return NIL


*:----------------------------------------------------------------
FUNCTION ntocmthtest ()
*:----------------------------------------------------------------
*:  cMonth :=  ntocmonth ( nMonthNum )
*:  ==================================
*:  ntocmonth() receives the number of a month and returns its
*:  name as a string. This version does not observe the Nations
*:  module. English only.

local getlist := {}
local cMonth := "  "
local c      := .T.
local nKey

do while c
   @ 5, 10 say "ntocmonth(n) returns the name of the month number n"
   @ 6, 10 say "Test it, Insert a month number"
   @ 6, 60 get cMonth

   set confirm on
   set escape on
   read
   set escape off
   set confirm on

   nKey := lastkey()

   if nKey == K_ESC
      c := .F.
   else

      @ 11, 10 say "The month selected is " + ;
                        padr (ntocmonth ( val (cMonth) ), 10)

      set cursor off
      inkey(0)
      set cursor on

      cMonth := "  "

   endif

 enddo

 @ 0, 0 clear

 return NIL

*:----------------------------------------------------------------
FUNCTION qtrtest ()
*:----------------------------------------------------------------
*:  nqtr := quarter ( ddate )
*:  ===========================
*:  Returns the quarter as a number. If no date is specified,
*:  the system date is used.

local getlist := {}
local ddate  := ctod ("  /  /    ")
local c      := .T.
local nKey

   do while c
      @ 5, 10 say "quarter (ddate) returns the number of the quarter"
      @ 6, 10 say "Test it, Insert a date"
      @ 6, 52 get ddate

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         @  8, 10 say "The quarter number is " + ;
                           padr (ltrim ( str ( quarter ( ddate ))), 10)

         set cursor off
         inkey(0)
         set cursor on

         ddate  := ctod ("  /  /    ")

      endif

    enddo

    @ 0, 0 clear

    return NIL


*:----------------------------------------------------------------
FUNCTION stodtest()
*:----------------------------------------------------------------
*:  stod ( ansi-date)
*:  ================
*:  Returns a Clipper format date. If Ansi date is invalid, a
*:  null date is returned.

local getlist   := {}
local ddate
local cAnsidate := space(8)
local c         := .T.
local nKey

   do while c
      @  5, 10 say "stod ( ansi-date ) receives an ANSI date string and"
      @  6, 10 say "returns a Clipper format date"
      @  8, 10 say "Enter an ANSI date string in the form YYYYMMDD"
      @  8, 57 get cAnsidate picture "999999999" valid chkansi (cAnsidate)

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         ddate := stod ( cAnsidate )

         @ 10, 10 say "The Clipper format date is " + ;
                           padr (ltrim ( dtoc ( ddate )), 10)

         set cursor off
         inkey(0)
         set cursor on

         cAnsidate := space(8)

      endif

   enddo
   return NIL


*:----------------------------------------------------------------
FUNCTION chkansi (cAnsidate)
*:----------------------------------------------------------------
local nYear
local nMonth
local nDay
local lretval := .T.

   nYear  := val (  left ( cAnsidate, 4))
   nMonth := val (substr ( cAnsidate, 5, 2))
   nDay   := val ( right ( cAnsidate, 2))

   do case

      case nYear < 1
           lretval := .F.
      case nMonth < 1 .or. nMonth > 12
           lretval := .F.
      case nday < 0 .or. nday > 31
           lretval := .F.
   endcase

   return lretval


*:----------------------------------------------------------------
FUNCTION weektest ()
*:----------------------------------------------------------------
*:  nWeek := week ( ddate, lSWN )
*:  =============================
*:  Returns the calendar week as a number. If no date is specified,
*:  the system date is used. An empty date viz ctod("  /  /  ")
*:  returns 0.

local getlist := {}
local ddate  := ctod ("  /  /    ")
local c      := .T.
local nKey
local cMode := space(1)

   do while c
      @ 5, 10 say "week (ddate, lSWN) returns the calendar number of the week"
      @ 6, 10 say "if lSWN == .T., the simple week number is returned"
      @ 7, 10 say "if lSWN == .F.(default), the ISO8601 week number is returned"

      @ 8, 10 say "Test it, Insert a date and <lSWN>"
      @ 9, 52 get ddate
      @ 10, 61 get cMode picture "Y"

      set confirm on
      set escape on
      read
      set escape off
      set confirm on

      nKey := lastkey()

      if nKey == K_ESC
         c := .F.
      else

         @  8, 10 say "The week number is " + ;
                           padr (ltrim ( str (week ( ddate, cMode=="Y"))), 10)

         set cursor off
         inkey(0)
         set cursor on

         ddate  := ctod ("  /  /    ")

      endif

    enddo

    @ 0, 0 clear

    return NIL
