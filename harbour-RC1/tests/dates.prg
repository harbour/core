//
// $Id$
//

// Testing Harbour dates management.

#include "set.ch"

function Main()
local cNewLine := HB_OSNewLine()

   local dDate, dDate2, cMask, cDate

   OUTSTD (cNewLine,  "Testing Harbour dates management on", DATE())

   TestCentury(cNewLine)

   OUTSTD (cNewLine, "")
   OUTSTD (cNewLine,  "dDate = CToD( '02/04/1999' ) =>", dDate := CToD( "02/04/1999" ))

   OUTSTD (cNewLine,  "ValType( dDate ) =", ValType( dDate ))

   OUTSTD (cNewLine,  "Day( dDate ) =", Day( dDate ))
   OUTSTD (cNewLine,  "Month( dDate ) =", Month( dDate ))
   OUTSTD (cNewLine,  "Year( dDate ) =", Year( dDate ), cNewLine)

   OUTSTD (cNewLine,  "dDate + 5 =", dDate2 := dDate + 5)
   OUTSTD (cNewLine,  "dDate - 5 =", dDate - 5, cNewLine )

   OUTSTD (cNewLine,  "dDate2 - dDate =", dDate2 - dDate)

   OUTSTD (cNewLine, "")
   OUTSTD (cNewLine, dDate, DTOS (dDate))

   OUTSTD (cNewLine, "19990429", STOD ("19990429"))

   OUTSTD (cNewLine, "")
   SET (_SET_EPOCH, 1950)
   cMask := "dd/mm/yyyy"
   cDate := "02/04/49"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (cNewLine, cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

   OUTSTD (cNewLine, "")
   cMask := "mm/dd/yyyy"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (cNewLine, cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

   OUTSTD (cNewLine, "")
   cMask := "yyyy/mm/dd"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (cNewLine, cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))
   OUTSTD (cNewLine, "")
   OUTSTD (cNewLine, "49/02/04", cMask, CTOD ("49/02/04"))

   TestCentury(cNewLine)

   OUTSTD (cNewLine, "")
   cMask := "yyyy/dd/mm"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (cNewLine, cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))
   OUTSTD (cNewLine, "")
   OUTSTD (cNewLine, "49/02/04", cMask, CTOD ("49/02/04"))

   OUTSTD (cNewLine, "")
   cMask := "ddd/mmm/yy"
   SET (_SET_DATEFORMAT, cMask)
   dDate := CTOD (cDate)
   OUTSTD (cNewLine, cDate, cMask, dDate, DTOS (dDate), DTOC (dDate))

return nil

procedure TestCentury(cNewLine)
   OUTSTD (cNewLine, "")
   OUTSTD (cNewLine, __SETCENTURY ())
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
