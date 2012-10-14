/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL cInFile   := __FILE__
   LOCAL CKEY
   LOCAL NNCOLOR   := 7
   LOCAL NHCOLOR   := 15
   LOCAL NCOLSKIP  := 5
   LOCAL NRMARGIN  := 132
   LOCAL CEXITKEYS := "AABBC       "
   LOCAL LBROWSE   := .F.
   LOCAL NSTART    := 1
   LOCAL NBUFFSIZE := 4096
   LOCAL GetList := {}

   @ 0, 0 CLEAR

   @ 0, 0  SAY "ENTER FILENAME: "   GET CINFILE
   @ 1, 0  SAY "    FOREGROUND: "   GET NNCOLOR   PICTURE "999"
   @ 2, 0  SAY "     HIGHLIGHT: "   GET NHCOLOR   PICTURE "999"
   @ 3, 0  SAY "     EXIT KEYS: "   GET CEXITKEYS
   @ 4, 0  SAY "   BUFFER SIZE: "   GET NBUFFSIZE PICTURE "9999"
   @ 1, 40 SAY "COLUMN INCREMENT: " GET NCOLSKIP  PICTURE "999"
   @ 2, 40 SAY "   MAX LINE SIZE: " GET NRMARGIN  PICTURE "999"
   @ 3, 40 SAY "     BROWSE MODE? " GET LBROWSE   PICTURE "Y"

   READ

   /*
    * REMEMBER A WINDOW WILL BE ONE SIZE LESS AND GREATER THAN THE PASSED COORD.'S
    *
    * THE 9TH PARAMETER CONTAINS THE KEYS THAT THE ROUTINE WILL TERMINATE ON
    * AND THE hb_BChar(143) represents the F3 key.
    *
    */

   @ 4, 9 TO 11, 71

   FT_DFSETUP( cInFile, 5, 10, 10, 70, nStart, ;
      nNColor, nHColor, cExitKeys + hb_BChar( 143 ), ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   cKey := FT_DISPFILE()

   FT_DFCLOSE()

   @ 20, 0 SAY "Key pressed was: " + "[" + cKey + "]"

   RETURN
