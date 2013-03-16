
#require "hbnf"

#include "inkey.ch"

PROCEDURE Main( cInFile )

   LOCAL nKey
   LOCAL nNColor   := 7
   LOCAL nHColor   := 15
   LOCAL nColSkip  := 5
   LOCAL nRMargin  := 132
   LOCAL cExitKeys := PadR( "AABBC", 12 )
   LOCAL lBrowse   := .F.
   LOCAL nStart    := 1
   LOCAL nBuffSize := 4096
   LOCAL GetList := {}

   LOCAL aExitKeys
   LOCAL tmp

   CLS

   hb_default( @cInFile, __FILE__ )

   cInFile := PadR( cInFile, 128 )

   @ 0,  0 SAY "ENTER FILENAME: "   GET cInFile   PICTURE "@S30"
   @ 1,  0 SAY "    FOREGROUND: "   GET nNColor   PICTURE "999"
   @ 2,  0 SAY "     HIGHLIGHT: "   GET nHColor   PICTURE "999"
   @ 3,  0 SAY "     EXIT KEYS: "   GET cEXitKeys
   @ 4,  0 SAY "   BUFFER SIZE: "   GET nBUffSize PICTURE "9999"
   @ 1, 40 SAY "COLUMN INCREMENT: " GET nCOlSkip  PICTURE "999"
   @ 2, 40 SAY "   MAX LINE SIZE: " GET nRMargin  PICTURE "999"
   @ 3, 40 SAY "     BROWSE MODE? " GET lBRowse   PICTURE "Y"

   READ

   /*
    * REMEMBER A WINDOW WILL BE ONE SIZE LESS AND GREATER THAN THE PASSED COORD.'S
    *
    * THE 9TH PARAMETER CONTAINS THE KEYS THAT THE ROUTINE WILL TERMINATE ON
    *
    */

   aExitKeys := {}
   FOR EACH tmp IN cExitKeys
      AAdd( aExitKeys, hb_keyCode( tmp ) )
   NEXT

   AAdd( aExitKeys, K_F3 )

   @ 4, 9 TO 11, 71

   ft_DFSetup( AllTrim( cInFile ), 5, 10, 10, 70, nStart, ;
      nNColor, nHColor, aExitKeys, ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   nKey := ft_DispFile()

   ft_DFClose()

   @ 20, 0 SAY "Key pressed was: " + "[" + hb_keyChar( nKey ) + "] (" + hb_ntos( nKey ) + ")"

   RETURN
