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

   cInFile := PadR( hb_defaultValue( cInFile, __FILE__ ), 128 )

   @ 0,  0 SAY "Enter filename:"   GET cInFile   PICTURE "@S30"
   @ 1,  0 SAY "    Foreground:"   GET nNColor   PICTURE "999"
   @ 2,  0 SAY "     Highlight:"   GET nHColor   PICTURE "999"
   @ 3,  0 SAY "     Exit keys:"   GET cEXitKeys
   @ 4,  0 SAY "   Buffer size:"   GET nBUffSize PICTURE "9999"
   @ 1, 40 SAY "Column increment:" GET nCOlSkip  PICTURE "999"
   @ 2, 40 SAY "   Max line size:" GET nRMargin  PICTURE "999"
   @ 3, 40 SAY "     Browse mode?" GET lBRowse   PICTURE "Y"

   READ

   /* Remember a window will be one size less and greater than the passed coordinates.
      The 9th parameter contains the keys that the routine will terminate on. */

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

   @ MaxRow() - 4, 0 SAY "Key pressed was: " + "[" + hb_keyChar( nKey ) + "] (" + hb_ntos( nKey ) + ")"

   RETURN
