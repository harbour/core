/*
 * $Id$
 */

/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   Simplest demo program to show the difference of MainCoord Mode and
   Standard Mode of GTWVW.
*/


proc main
  setcolor("N/W")

  WVW_SetMainCoord(.F.)    //Standard Mode
  fillscreen()

  WVW_SetMainCoord(.T.)    //MainCoord Mode
  fillscreen()
return

proc fillscreen()
local i,j
  scroll()
  WVW_nOpenWindow("Win2", 10,10,19,69)
  WVW_nOpenWindow("Win3", 15,15,22,75)
  devpos(0,0)
  ?? "I'm gonna fill this (" + alltrim(str(maxrow()+1)) + "x" + alltrim(str(maxcol()+1)) + ") screen"
  devpos(1,0)
  for i := 1 to maxrow()-1
     for j := 0 to maxcol()
        ?? alltrim(str(j % 10,0))
     next
  next
  ?? "Done. Press ESC to exit."
  do while inkey(0)!=27
  enddo
  WVW_lCloseWindow()
  WVW_lCloseWindow()
return
