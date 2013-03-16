
#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   // Thanks to Jim Gale for helping me understand the basics
   LOCAL i, ar[ 3, 26 ], aBlocks[ 3 ], aHeadings, nElem := 1, bGetFunc, cRet
   // set up 2 dimensional array ar[]
   FOR i := 1 TO 26
      ar[ 1, i ] := i                          //  1  ->  26  Numeric
      ar[ 2, i ] := Chr( Asc( "A" ) + i - 1 )  // "A" -> "Z"  Character
      ar[ 3, i ] := Chr( Asc( "Z" ) - i + 1 )  // "Z" -> "A"  Character
   NEXT
   // Set Up aHeadings[] for column headings
   aHeadings  := { "Numbers", "Letters", "Reverse" }
   // Set Up Blocks Describing Individual Elements in Array ar[]
   aBlocks[ 1 ] := {|| Str( ar[ 1, nElem ], 2 ) }  // to prevent default 10 spaces
   aBlocks[ 2 ] := {|| ar[ 2, nElem ] }
   aBlocks[ 3 ] := {|| ar[ 3, nElem ] }
   // Set up TestGet() as bGetFunc
   bGetFunc   := {| b, ar, nDim, nElem | TestGet( b, ar, nDim, nElem ) }

   SET SCOREBOARD OFF
   SetColor( "W/N" )
   CLS
   @ 21, 4 SAY "Use Cursor Keys To Move Between Fields, <F7> = Delete Row, <F8> = Add Row"
   @ 22, 7 SAY "<ESC> = Quit Array Edit, <Enter> or <Any Other Key> Edits Element"
   SetColor( "N/W, W/N, , , W/N" )
   cRet := ft_ArEdit( 3, 5, 18, 75, ar, @nElem, aHeadings, aBlocks, bGetFunc )
   SetColor( "W/N" )
   CLS
   ? cRet
   ? "LastKey() = ESC:", LastKey() == K_ESC

   RETURN

FUNCTION TestGet( b, ar, nDim, nElem )

   LOCAL GetList   := {}
   LOCAL nRow      := Row()
   LOCAL nCol      := Col()
   LOCAL cSaveScrn := SaveScreen( 21, 0, 22, MaxCol() )
   LOCAL cOldColor := SetColor( "W/N" )

   @ 21, 0 CLEAR TO 22, MaxCol()
   @ 21, 29 SAY "Editing Array Element"
   SetColor( cOldColor )
   DO CASE
   CASE nDim == 1
      @ nRow, nCol GET ar[ 1, nElem ] PICTURE "99"
      READ
      b:refreshAll()
   CASE nDim == 2
      @ nRow, nCol GET ar[ 2, nElem ] PICTURE "!"
      READ
      b:refreshAll()
   CASE nDim == 3
      @ nRow, nCol GET ar[ 3, nElem ] PICTURE "!"
      READ
      b:refreshAll()
   ENDCASE
   RestScreen( 21, 0, 22, MaxCol(), cSaveScrn )
   @ nRow, nCol SAY ""

   RETURN .T.
