/*
   At Id: 9807a3584fe7990bb602500fd242bd46f586b300
   has a RTE and visual glitch while playing arrows
   up and down and change y1 or y2 to not integer
   value and number of items less/equal then/to
   number of rows in window.
*/

PROCEDURE Main()

   LOCAL x1
   LOCAL x2
   LOCAL y1
   LOCAL y2

   CLS
   x1 := 24
   x2 := 35
   y1 := 7.5
   y2 := 9
   DispBox( y1, x1, y2 + 1, x2 + 1,, "W+/B,N/BG" )
   AChoice( y1 + 1, x1 + 1, y2, x2, ;
      { "menu 1", "menu 2", "menu 3" }, ;
      { .T., .F., .T. } )

   RETURN
