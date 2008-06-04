//
// $Id$
//

/* gtstd test */

func Main()
  local n

  PosNow()
  ?? "Output test. First line, no newlines."

  ? "Press a key to continue: "
  ?? inkey(0)

  ? "This is row " + alltrim(str(row()))
  
  @ 7, 30 say "@ 7,30"
  @ 7, 10 say "@ 7,10"
  @ 7, 60 say "@ 7,60"
  @ 7, 75 say "9876543210"
  @ 6, 10 say "@ 6,10.."
  PosNow()

  ?
  ? "Scroll test: pre = "
  PosNow()
   // scroll(0,0,maxrow(),maxcol(),-3,0)
  ?? " post = "
  PosNow()

  ?
  ? "Press key to test CLS"
  inkey(0)
  CLS

  PosNow()

  ?
  ? "Press key to test for n := 100 to 120 ; tone(n, 1) ; next"
  inkey(0)
  for n := 100 to 120 ; tone(n, 1) ; next  

  ? "Done.."
  ? "Testing long string via QOUT. 50 characters follow here: 98765432109876543210987654321098765432109876543210"
  ? "Done.. testing end of screen scroll"

  for n := 1 to 25
    ? "This line is on row "
    ?? alltrim(str(row()))
    inkey(0)
  next

  return NIL

func PosNow()
  ?? "[" + alltrim(str(row())) + "," + alltrim(str(col())) + "]"
  return NIL

