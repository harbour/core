/*
 * $Id$
 */

// Testing Harbour dates management.

#include "set.ch"

function Main()

   local dDate, dDate2, cMask, cDate

   OUTSTD (hb_eol(),  "Testing Harbour dates management on", DATE())

   TestCentury()

   OUTSTD (hb_eol(), "")
   OUTSTD (hb_eol(),  "dDate := CToD( '02/04/1999' ) =>", dDate := CToD( "02/04/1999" ))

   OUTSTD (hb_eol(),  "ValType( dDate ) =", ValType( dDate ))

   OUTSTD (hb_eol(),  "Day( dDate ) =", Day( dDate ))
   OUTSTD (hb_eol(),  "Month( dDate ) =", Month( dDate ))
   OUTSTD (hb_eol(),  "Year( dDate ) =", Year( dDate ), hb_eol())

   OUTSTD (hb_eol(),  "dDate + 5 =", dDate2 := dDate + 5)
   OUTSTD (hb_eol(),  "dDate - 5 =", dDate - 5, hb_eol() )

   OUTSTD (hb_eol(),  "dDate2 - dDate =", dDate2 - dDate)

   OUTSTD (hb_eol(), "")
   OUTSTD (hb_eol(), dDate, DTOS (dDate))

   OUTSTD (hb_eol(), "19990429", STOD ("19990429"))

   OUTSTD (hb_eol(), "")
   SET (_SET_EPOCH, 1950)
   cMask := "dd/mm/yyyy"
   cDate := "02/04/49"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (hb_eol(), cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

   OUTSTD (hb_eol(), "")
   cMask := "mm/dd/yyyy"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (hb_eol(), cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

   OUTSTD (hb_eol(), "")
   cMask := "yyyy/mm/dd"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (hb_eol(), cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))
   OUTSTD (hb_eol(), "")
   OUTSTD (hb_eol(), "49/02/04", cMask, CTOD ("49/02/04"))

   TestCentury(hb_eol())

   OUTSTD (hb_eol(), "")
   cMask := "yyyy/dd/mm"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (hb_eol(), cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))
   OUTSTD (hb_eol(), "")
   OUTSTD (hb_eol(), "49/02/04", cMask, CTOD ("49/02/04"))

   OUTSTD (hb_eol(), "")
   cMask := "ddd/mmm/yy"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (hb_eol(), cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

   return nil

procedure TestCentury()
   OUTSTD (hb_eol(), "")
   OUTSTD (hb_eol(), __SETCENTURY ())
   __SETCENTURY ("ON")
   OUTSTD (__SETCENTURY ())
   __SETCENTURY ("OFF")
   OUTSTD (__SETCENTURY ())
   __SETCENTURY ("GIBBERISH")
   OUTSTD (__SETCENTURY ())
   __SETCENTURY (.T.)
   OUTSTD (__SETCENTURY ())
   __SETCENTURY (5)
   OUTSTD (__SETCENTURY ())
   return
