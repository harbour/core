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
 * Copyright 2002 Alan Secker <alansecker@globalnet.co.uk>
 * Copyright 2003 Martin Vogel <vogel@inttec.de>: Enhancements, internationalization, documentation headers
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


#include "set.ch"
#include "hblang.ch"


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

FUNCTION addmonth ( ddate, nmth )
local nDay
// local ndays
// local sev
// local dnew
// local dEnd
// local nOldday
local nMonth
local nYear
local nLDOM

//   if nmth > 70 
//      return ctod ("  /  /    ")
//   endif
//
//   nOldday := day (ddate)
//   ndays   := nmth * 30
//
//   dnew    := ddate + ndays
//
//   nMonth  := month ( dnew )
//   nNyear  := year  ( dnew )
//
//   dEnd    := ctod (str (nOldday) + "/" + str(nMonth) + "/" + str (nNyear))
//
//   return dEnd  

   if !(valtype (ddate) $ "DN")
      return ctod("")
   endif

   if valtype (ddate) == "N"
      nmth := ddate
      ddate := date()
   endif

   nmth = int (nmth)

   nDay = day (ddate)
   nMonth = month (ddate)
   nYear = year (ddate)

   nMonth += nmth

   if nMonth <= 0
      do while nMonth <= 0
	 nMonth += 12
	 nYear--
      enddo
   endif

   if nMonth > 12
      do while nMonth > 12
	 nMonth -= 12
	 nYear++
      enddo
   endif

   // correction for different end of months
   if nDay > (nLDOM := lastdayom (nMonth))
     nDay := nLDOM
   endif

   ddate := stod (strzero (nYear, 4) + strzero (nMonth, 2) + strzero (nDay, 2))
   return (ddate)

  
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

FUNCTION ctodow ( cDow )
//local cWeek  := "SUNMONTUEWEDTHUFRISAT "
//local nWk    := len (cWeek)
//local cMatch := left (upper ( Alltrim (cDow)), 3)
//local n 
//local nDay   := 0
//
//   for n = 1 to nWk step 3
//        if RTRIM (substr (cWeek, n, 3)) == cMatch
//           nDay := INT (((n-1) / 3) + 1)
//           exit
//        endif
//   next
//
//   return nDay

local nOrdinal := 0
local bExact

  if valtype (cDow) != "C"
     return (0)
  endif
  
  bExact = set (_SET_EXACT, .F.)
  cDow = upper (alltrim (cDow))

  do while nOrdinal < 7
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_DAY + nOrdinal))) = cDow
	set (_SET_EXACT, bExact)
	return (nOrdinal+1)
     endif
     nOrdinal++
  enddo

  set (_SET_EXACT, bExact)
  return (0)


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

FUNCTION ctomonth ( cDom )
//local cMonth := "JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC "
//local nMnth  := len (cMonth)
//local cMatch := left (upper ( Alltrim (cDom)), 3)
//local n 
//local nDay   := 0

local nOrdinal := 0
local bExact

//   for n = 1 to nMnth step 3
//        if RTRIM (substr (cMonth, n, 3)) == cMatch
//           nDay := INT (((n-1) / 3) + 1)
//           exit
//        endif
//   next
//
//   return nDay

  if valtype (cDom) != "C"
     return (0)
  endif
  
  bExact = set (_SET_EXACT, .F.)
  cDom = upper (alltrim (cDom))

  do while nOrdinal < 12
     if upper (alltrim (hb_langmessage (HB_LANG_ITEM_BASE_MONTH + nOrdinal))) = cDom
	set (_SET_EXACT, bExact)
	return (nOrdinal+1)
     endif
     nOrdinal++
  enddo

  set (_SET_EXACT, bExact)
  return (0)


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

FUNCTION daysInmonth ( nMonth, lLeap )

local nday := 0

  do case
     case nMonth == 2
          if lLeap == .T.
             nday := 29
          else
             nday := 28
          endif

     case nMonth == 4 .or. nMonth == 6 .or. ;
          nMonth == 9 .or. nMonth == 11
          nday := 30
     otherwise 
          nday := 31
  endcase

  return nday

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

FUNCTION daystomonth ( nMonth, lLeap )

local ndays := 0

   if valtype(lLeap) != "L"
      lLeap := .F.
   endif

   do case
      case nMonth == 2
           ndays :=  31     // + Jan 31
      case nMonth == 3     
           ndays :=  59     // + Feb 28
      case nMonth == 4
           ndays :=  90     // + Mar 31
      case nMonth == 5
           ndays := 120     // + Apr 30
      case nMonth == 6
           ndays := 151     // + May 31
      case nMonth == 7
           ndays := 181     // + Jun 30
      case nMonth == 8
           ndays := 212     // + Jul 31
      case nMonth == 9
           ndays := 243     // + Aug 31
      case nMonth == 10
           ndays := 273     // + Sep 30
      case nMonth == 11
           ndays := 304     // + Oct 31
      case nMonth == 12
           ndays := 334     // + Nov 30
   endcase
  
   if (lLeap,  ndays ++, )

   return ndays


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

FUNCTION dmy ( ddate, lmode )

//local nMonth  := month (dDate)
//local nDay    :=   day (dDate)
//local nYear   :=  year (dDate)
local nMonth, nDay, nYear 

local cPeriod := ""
local cDate
local cMonth 

local cYear
local lSetCentury := __SETCENTURY()

   if valtype (ddate) != "D"
      ddate := date ()
   endif

   nMonth  := month (ddate)
   nDay    :=   day (ddate)
   nYear   :=  year (ddate)

   cMonth := ntocmonth ( nMonth )
   cYear := str (nYear, iif (lSetCentury, 4, 2))

   cPeriod := if (lmode == .T., ".", "")


   cDate := ltrim ( str ( nDay )) + cPeriod + " " + cMonth + " " + ;
            ltrim ( cYear )
//            ltrim ( str ( nYear ))
   return cDate


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

FUNCTION doy ( dDate )

local lleap   := .F.
local nMonth  := month (dDate)
local nDay    := day (dDate)
local numdays := 0

   if valtype ( dDate ) != "D"
      dDate := date()
   endif

   if empty (dDate)
      return 0
   endif

   lLeap := isleap (dDate)
   numdays := daystomonth ( nMonth, lleap ) + nDay

   return numdays


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
   
FUNCTION isleap ( ddate )

local nYear
local nMmyr
local nCyYr
local nQdYr
local lRetval

   if empty ( ddate )
     ddate := date ()
   endif

   nYear  := year (ddate)
   nCyYr  := nYear / 400
//   nMmyr  := nyear /1000
   nMmyr  := nYear /100
   nQdYr  := nYear / 4
   
   do case
      case int (nCyYr) == nCyYr
           lRetVal := .T.

      case int (nMmyr) == nMmyr
           lRetVal := .F.
           
      case int (nQdYr) == nQdYr
           lRetVal := .T.

      otherwise
           lRetVal := .F.
   endcase
   
   return lRetVal
   
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

FUNCTION lastdayom ( xDate )

local nMonth := 0
local nDays  := 0
local lleap  := .F.

     do case
        case empty ( xDate)
             nMonth := month ( date() )

        case valtype ( xDate ) == "D"
             nMonth  := month (xdate)
             lleap := isleap ( xdate)

        case valtype (xDate ) == "N"
             if xdate > 12
                nmonth := 0
             else
                nMonth := xDate
             endif
     endcase

     if nmonth != 0
        ndays := daysInmonth ( nMonth, lleap )
     endif

     return ndays

 
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

FUNCTION mdy ( dDate )

local nMonth
local nDay
local nYear
local cDate
local cMonth 

local lSetCentury := __SETCENTURY()
local cYear

//   default dDate to date()
   if valtype (ddate) != "D"
      ddate := date ()
   endif

   nMonth  := month (dDate)
   nDay    :=   day (dDate)
   nYear   :=  year (dDate)
   cMonth  := ntocmonth ( nMonth )

   cYear := str (nYear, iif (lSetCentury, 4, 2))

   cDate := cMonth + " " + ;
            ltrim ( str ( nDay )) + " " + ;
            ltrim ( cYear )
   //            ltrim ( str ( nYear ))
   
   return cDate


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

FUNCTION ntocdow ( nDay )
   
local cDay := ""
   
   if nDay >= 1 .AND. nDay <= 7
     cDay := hb_langmessage (HB_LANG_ITEM_BASE_DAY + (nDay-1))
   endif

   return cDay


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

FUNCTION ntocmonth ( nMonthNum )
   
local cMonth := ""
   
   if nMonthNum >= 1 .AND. nMonthNum <= 12
     cMonth := hb_langmessage (HB_LANG_ITEM_BASE_MONTH + (nMonthNum-1))
   endif

//  do case
//     case nMonthNum == 1
//          cMonth := "January"
//     case nMonthNum == 2
//          cMonth := "February"
//     case nMonthNum == 3
//          cMonth := "March"
//     case nMonthNum == 4
//          cMonth := "April"
//     case nMonthNum == 5
//          cMonth := "May"
//     case nMonthNum == 6
//          cMonth := "June"
//     case nMonthNum == 7
//          cMonth := "July"
//     case nMonthNum == 8
//          cMonth := "August"
//     case nMonthNum == 9
//          cMonth := "September"
//     case nMonthNum == 10
//          cMonth := "October"
//    case nMonthNum == 11
//          cMonth := "November"
//     case nMonthNum == 12
//          cMonth := "December"
//  endcase

    return cMonth


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

FUNCTION quarter ( ddate )

local nmonth
local nretmonth

   if empty (ddate)
      ddate := date()
   endif

   nmonth    := month (ddate)
//   nretmonth := int (( nmonth / 3 ) + 0.67 )
   nretmonth := int ((nmonth + 2) / 3 )

   return nretmonth



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

FUNCTION week ( dDate, lSWN )

local nMonth
local nDays
local nDay
local nYear
local nWeek
local nPart
local dDate2
// local nleap

//       do case
//	  case valtype (dDate) == "D" .and. empty ( dDate)
//             return nDays
//	     
//	  case empty (dDate)
//             dDate := date()
//       endcase

     if valtype (dDate) == "D" .and. empty (dDate)
	return 0
     endif

     if empty (dDate)
	dDate := date()
     endif

     nMonth  := month (dDate)
     nDay    :=   day (dDate)
     nYear   :=  year (dDate)

     if valtype (lSWN) != "L"
	lSWN := .F.
     endif

     if lSWN
       // simple week number

       //     nleap := if (isleap (dDate), 1, nleap)
       //     ndays := daystomonth ( nMonth, nleap ) + nday
       nDays := daystomonth ( nMonth, isleap (dDate)) + nDay
       
       nPart := nDays % 7
       nWeek := INT (nDays / 7)
       
       nWeek := INT (if ( nPart > 0, ++ nWeek, nWeek))

     else
	// ISO8601 week number
	dDate2 := dDate + 3 - ((dow(dDate)+5) % 7)
	nWeek := 1 + int ((dDate2 - boy (dDate2)) / 7) 
	
     endif

     return nWeek






