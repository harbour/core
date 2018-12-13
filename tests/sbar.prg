/*
 * ScrollBar class test
 *
 * Example donated by Diego Pego,
 * modified by Alejandro de Garate
 */

/* UTF-8 */

#include "directry.ch"
#include "achoice.ch"
#include "inkey.ch"

#ifdef __HARBOUR__
   #define B_THIN  hb_UTF8ToStrBox( "█▀███▄██" )
#else
   #define B_THIN  ( Chr( 219 ) + Chr( 223 ) + Chr( 219 ) + Chr( 219 ) + ;
                     Chr( 219 ) + Chr( 220 ) + Chr( 219 ) + Chr( 219 ) )
#endif

PROCEDURE Main()

   InitScrlBar()

   RETURN

FUNCTION InitScrlBar()

   LOCAL tmpFileList, i

   LOCAL aFileList := {}
   LOCAL filesScroll

   CLS
   SetBlink( .F. )
#ifdef __HARBOUR__
   @  0,  0, MaxRow(), MaxCol() BOX Replicate( hb_UTF8ToStrBox( "▓" ), 9 ) COLOR "GR+/W*"
#else
   @  0,  0, MaxRow(), MaxCol() BOX Replicate( Chr( 178 ), 9 ) COLOR "GR+/W*"
#endif
   @  4, 28 SAY "            Directory            " COLOR "W+/B"
   @  5, 28, 15, 60 BOX B_THIN + " " COLOR "W/W*"

   // get the current folder files to display on the aChoice menu
   tmpFileList := Directory()

   FOR i := 1 TO Len( tmpFileList )
      AAdd( aFileList, tmpFileList[ i ][ F_NAME ] )
   NEXT

   filesScroll := ScrollBar( 6, 14, 60, NIL, 1 )

   filesScroll:total := Len( aFileList )

   filesScroll:colorSpec := "W+/W, W+/W"
   SET COLOR TO "N/W*, W+/B,,,W/N"

   filesScroll:display()

   i := AChoice( 6, 29, 14, 59, aFileList, , {| modo | updateFilesScroll( modo, aFileList, filesScroll ) } )

   @ MaxRow() - 1, 0 SAY iif( i < 1, "", aFileList[ i ] ) COLOR "N/W*"
   SET COLOR TO
   @ MaxRow(), 0

   RETURN 0

// function used to update scrollbar

STATIC FUNCTION updateFilesScroll( modo, aFileList, filesScroll )

   LOCAL newPos, valRet := AC_CONT   // Default to continue
   LOCAL nLastKey := LastKey()

   newPos := filesScroll:current

   DO CASE
   CASE nLastKey == K_CTRL_PGUP
      newPos := 1
   CASE nLastKey == K_CTRL_PGDN
      newPos := filesScroll:total
   CASE nLastKey == K_CTRL_HOME
      newPos := newPos - ( filesScroll:barLength + 1 )
   CASE nLastKey == K_CTRL_END
      newPos := newPos + ( filesScroll:barLength + 1 )
   CASE nLastKey == K_PGUP
      newPos := newPos - ( filesScroll:barLength + 1 )
   CASE nLastKey == K_PGDN
      newPos := newPos + ( filesScroll:barLength + 1 )
   CASE nLastKey == K_UP
      newPos--
   CASE nLastKey == K_DOWN
      newPos++
   CASE modo == AC_EXCEPT
      DO CASE
      CASE nLastKey == K_ENTER
         valRet := AC_SELECT
      CASE nLastKey == K_ESC
         valRet := AC_ABORT
      OTHERWISE
         valRet := AC_GOTO
      ENDCASE
   ENDCASE

   IF newPos < 1
      newPos := 1
   ELSEIF newPos >= filesScroll:total
      newPos := filesScroll:total
   ENDIF

   filesScroll:current := newPos
   filesScroll:update()

   RETURN valRet
