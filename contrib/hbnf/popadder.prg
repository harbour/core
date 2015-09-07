/* This is an original work by Keith A. Wire and is placed in the public domain.

      Rev 1.4   03 Mar 1994 19:47:22   GLENN
   Author made some enhancements and modifications.

      Rev 1.3   19 Jan 1993 19:52:52   GLENN
   Removed reference to K_SPACE, as this has been defined in Clipper
   5.2's inkey.ch.

      Rev 1.2   17 Aug 1991 15:44:30   GLENN
   Don Caton fixed some spelling errors in the doc

      Rev 1.1   15 Aug 1991 23:04:12   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   14 Jun 1991 17:37:54   GLENN
   Initial revision.
 */

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "achoice.ch"

// Set up manifest constants to access the window colors in the array t_aWinColor
#define W_BORDER 1
#define W_ACCENT 2
#define W_PROMPT 3
#define W_SCREEN 4
#define W_TITLE  5
#define W_VARIAB 6
#define W_CURR   NIL

// Set up manifest constants to access the Standard screen colors in the array
// t_aStdColor
#define STD_ACCENT   1
#define STD_ERROR    2
#define STD_PROMPT   3
#define STD_SCREEN   4
#define STD_TITLE    5
#define STD_VARIABLE 6
#define STD_BORDER   7

// Instead of using STATIC variables for these I'm using a LOCAL array
//   and passing aAdder[] all over the place. Don't let this confuse
//   you. I wrote the Adder using the variable names and now let the
//   PreProcessor do all the work.
#define nTotal     aAdder[ 1 ]
#define nNumTotal  aAdder[ 2 ]
#define nSavTotal  aAdder[ 3 ]
#define cTotPict   aAdder[ 4 ]
#define lClAdder   aAdder[ 5 ]
#define lDecSet    aAdder[ 6 ]
#define nDecDigit  aAdder[ 7 ]
#define nMaxDeci   aAdder[ 8 ]
#define lMultDiv   aAdder[ 9 ]
#define nAddMode   aAdder[ 10 ]
#define lSubRtn    aAdder[ 11 ]
#define lTotalOk   aAdder[ 12 ]
#define lAddError  aAdder[ 13 ]
#define lTape      aAdder[ 14 ]
#define lNewNum    aAdder[ 15 ]
#define nSavSubTot aAdder[ 16 ]
#define lDivError  aAdder[ 17 ]
#define aTrans     aAdder[ 18 ]
#define nTopOS     aAdder[ 19 ]
#define nLeftOS    aAdder[ 20 ]
#define nAddSpace  aAdder[ 21 ]
#define nTapeSpace aAdder[ 22 ]
#define cTapeScr   aAdder[ 23 ]

// I still use a few of STATICs, but most are set to NIL when quiting
THREAD STATIC t_lAdderOpen := .F.
THREAD STATIC t_aKeys
THREAD STATIC t_aWindow
THREAD STATIC t_nWinColor
THREAD STATIC t_aWinColor
THREAD STATIC t_aStdColor

// Pop Up Calculator with Tape Display
PROCEDURE ft_Adder()

   LOCAL nOldDecim, cMoveTotSubTot, cTotal, nKey
   LOCAL oGet        := GetActive()
   LOCAL nOldCurs
   LOCAL nOldRow     := Row()
   LOCAL nOldCol     := Col()
   LOCAL bOldF10
   LOCAL nOldLastKey := hb_keyLast()
   LOCAL lShowRight  := .T.
   LOCAL aAdder      := Array( 23 )
   LOCAL tmp, tmp1

   LOCAL lAC_exit_ok

   IF t_lAdderOpen  // Must prevent recursive calls
      RETURN
   ENDIF
   t_lAdderOpen := .T.

   nOldCurs := SetCursor( SC_NONE )
   bOldF10  := SetKey( K_F10, NIL )

   aTrans     := { "                  0.00 C " }
   nOldDecim  := Set( _SET_DECIMALS, 9 )
   cTotPict   := "999999999999999.99"
   cTapeScr   := ""
   nTotal     := nNumTotal := nSavTotal := nDecDigit := 0
   nMaxDeci   := 2    // Initial # of decimals
   nSavSubTot := 0
   lNewNum    := .F.
   nAddMode   := 1    // Start in ADD mode
   lMultDiv   := .F.  // Start in ADD mode
   lClAdder   := .F.  // Clear adder flag
   lDecSet    := .F.  // Decimal ? - keyboard routine
   lSubRtn    := lTotalOk := lTape := lAddError := lDivError := .F.

   nTopOS     := Int( ( MaxRow() - 24 ) / 2 )  // Using the TopOffSet and LeftOffSet
   nLeftOS    := Int( ( MaxCol() - 79 ) / 2 )  // the Adder will always be centered
   nAddSpace  := iif( lShowRight, 40, 0 ) + nLeftOS
   nTapeSpace := iif( lShowRight, 0, 40 ) + nLeftOS

   // Set Up the STATIC variables
   t_aKeys     := {}
   t_aWindow   := {}
   t_nWinColor := 0

   _ftAddScreen( aAdder )

   hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nTotal, cTotPict ) )

   DO WHILE .T.
      nKey := _ftInkey()
      DO CASE
      CASE hb_keyChar( nKey ) $ "1234567890."
         _ftProcessNumb( aAdder, nKey )
      CASE nKey == hb_keyCode( "+" )
         _ftAddSub( aAdder, nKey )
      CASE nKey == hb_keyCode( "-" )
         _ftAddSub( aAdder, nKey )
      CASE nKey == hb_keyCode( "*" )
         _ftMultDiv( aAdder, nKey )
      CASE nKey == hb_keyCode( "/" )
         _ftMultDiv( aAdder, nKey )
      CASE nKey == K_ENTER            // <RTN> Total or Subtotal
         _ftAddTotal( aAdder )
      CASE nKey == K_ESC              // <ESC> Quit
         EXIT
      CASE hb_keyChar( nKey ) $ "Dd"  // <D> Change number of decimal places
         _ftChangeDec( aAdder )
      CASE hb_keyChar( nKey ) $ "Tt"  // <T> Display Tape
         _ftDisplayTape( aAdder, nKey )
      CASE hb_keyChar( nKey ) $ "Mm"  // <M> Move Adder
         IF lTape
            RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
         ENDIF
         DO CASE
         CASE __XSaveGetChar( SaveScreen( 6 + nTopOS, 26 + nAddSpace, 6 + nTopOS, 27 + nAddSpace ), 0 ) == " "
            cMoveTotSubTot := " "
         CASE __XSaveGetChar( SaveScreen( 6 + nTopOS, 19 + nAddSpace, 6 + nTopOS, 20 + nAddSpace ), 0 ) == "S"
            cMoveTotSubTot := "S"
         OTHERWISE
            cMoveTotSubTot := "T"
         ENDCASE
         tmp := SaveScreen( 4 + nTopOS, 8 + nAddSpace, 4 + nTopOS, 25 + nAddSpace )
         cTotal := ""
         FOR tmp1 := 0 TO 16
            cTotal += __XSaveGetChar( tmp, tmp1 )
         NEXT
         _ftPopWin()                     // Remove Adder
         lShowRight := ! lShowRight
         nAddSpace  := iif( lShowRight, 40, 0 ) + nLeftOS
         nTapeSpace := iif( lShowRight, 0, 40 ) + nLeftOS
         _ftAddScreen( aAdder )
         _ftDispTotal( aAdder )
         IF lTape
            lTape := .F.
            _ftDisplayTape( aAdder, nKey )
         ENDIF
         hb_DispOutAt( 4 + nTopOS, 8 + nAddSpace, cTotal )
         IF ! Empty( cMoveTotSubTot )
            _ftSetWinColor( W_CURR, W_SCREEN )
            hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, ;
               iif( cMoveTotSubTot == "T", "   <TOTAL>", "<SUBTOTAL>" ) )
            _ftSetWinColor( W_CURR, W_PROMPT )
         ENDIF
      CASE hb_keyChar( nKey ) $ "Ss" .AND. lTape  // <S> Scroll tape display
         IF Len( aTrans ) > 16           // We need to scroll
            hb_DispOutAt( 21 + nTopOS, 8 + nTapeSpace, " " + /* LOW-ASCII "↑↓" */ Chr( 24 ) + Chr( 25 ) + "-SCROLL  <ESC>-QUIT ", "GR+/W" )
            SetColor( "N/W,W+/N" )
            lAC_exit_ok := .F.
            AChoice( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, aTrans, .T., ;
               {| nMode, cur_elem, rel_pos | _ftAdderTapeUDF( nMode, cur_elem, rel_pos, @lAC_exit_ok ) }, Len( aTrans ), 20 )
            hb_DispBox( 21 + nTopOS, 8 + nTapeSpace, 21 + nTopOS, 30 + nTapeSpace, HB_B_SINGLE_UNI, "R+/W" )
            _ftSetWinColor( W_CURR, W_PROMPT )
            CLEAR TYPEAHEAD
         ELSE
            _ftError( "there are " + iif( Len( aTrans ) > 0, "only " + hb_ntos( Len( aTrans ) ), "no" ) + ;
               " transactions entered so far." + ;
               " No need to scroll!" )
         ENDIF
      CASE nKey == K_DEL                 // Delete - Clear adder
         _ftClearAdder( aAdder )
      CASE nKey == K_F1                  // <F1> Help
         _ftAddHelp()
      CASE nKey == K_F10                 // <F10> Quit - Return total
         IF lTotalOk                     // Did they finish the calculation
            IF oGet != NIL .AND. oGet:type == "N"
               oGet:varPut( nSavTotal )
               EXIT
            ELSE
               _ftError( "but I can not return the total from the " + ;
                  "adder to this variable. You must quit the adder using" + ;
                  " the <ESC> key and then enter the total manually." )
            ENDIF
         ELSE
            _ftError( "the calculation is not finished yet! You must have" + ;
               " a TOTAL before you can return it to the program." )
         ENDIF
      ENDCASE
   ENDDO

   Set( _SET_DECIMALS, nOldDecim )
   SetCursor( nOldCurs )
   IF lTape
      RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
   ENDIF
   _ftPopWin()
   SetPos( nOldRow, nOldCol )
   _ftSetLastKey( nOldLastKey )
   SetKey( K_F10, bOldF10 )
   t_lAdderOpen := .F.            // Reset the recursive flag

   // Reset the STATICs to NIL
   t_aKeys := t_aWindow := t_aWinColor := t_aStdColor := NIL

   RETURN

// Display the Adder
STATIC PROCEDURE _ftAddScreen( aAdder )

   LOCAL nCol, i

   _ftPushWin( 2 + nTopOS, 2 + nAddSpace, 22 + nTopOS, 30 + nAddSpace, "   Adder   ", ;
      "<F1> for Help",, HB_B_DOUBLE_UNI + " " )
   nCol := 5 + nAddSpace
   FOR EACH i IN { ;
      "      ┌───┐ ┌───┐ ┌───┐", ;
      "      │   │ │   │ │   │", ;
      "      └───┘ └───┘ └───┘", ;
      "┌───┐ ┌───┐ ┌───┐ ┌───┐", ;
      "│   │ │   │ │   │ │   │", ;
      "└───┘ └───┘ └───┘ │   │", ;
      "┌───┐ ┌───┐ ┌───┐ │   │", ;
      "│   │ │   │ │   │ │   │", ;
      "└───┘ └───┘ └───┘ └───┘", ;
      "┌───┐ ┌───┐ ┌───┐ ┌───┐", ;
      "│   │ │   │ │   │ │   │", ;
      "└───┘ └───┘ └───┘ │   │", ;
      "┌─────────┐ ┌───┐ │   │", ;
      "│         │ │   │ │   │", ;
      "└─────────┘ └───┘ └───┘" }
      hb_DispOutAt( 6 + nTopOS + i:__enumIndex(), nCol, hb_UTF8ToStr( i ) )
   NEXT
   _ftSetWinColor( W_CURR, W_TITLE )
   nCol := 7 + nAddSpace
   hb_DispOutAt( 11 + nTopOS, nCol, "7" )
   hb_DispOutAt( 14 + nTopOS, nCol, "4" )
   hb_DispOutAt( 17 + nTopOS, nCol, "1" )
   nCol := 13 + nAddSpace
   hb_DispOutAt(  8 + nTopOS, nCol, "/" )
   hb_DispOutAt( 11 + nTopOS, nCol, "8" )
   hb_DispOutAt( 14 + nTopOS, nCol, "5" )
   hb_DispOutAt( 17 + nTopOS, nCol, "2" )
   nCol := 19 + nAddSpace
   hb_DispOutAt(  8 + nTopOS, nCol, "X" )
   hb_DispOutAt( 11 + nTopOS, nCol, "9" )
   hb_DispOutAt( 14 + nTopOS, nCol, "6" )
   hb_DispOutAt( 17 + nTopOS, nCol, "3" )
   hb_DispOutAt( 20 + nTopOS, nCol, "." )
   hb_DispOutAt( 20 + nTopOS, 10 + nAddSpace, "0" )
   nCol := 25 + nAddSpace
   hb_DispOutAt(  8 + nTopOS, nCol, "-" )
   hb_DispOutAt( 13 + nTopOS, nCol, "+" )
   hb_DispOutAt( 18 + nTopOS, nCol, "=" )
   hb_DispOutAt( 19 + nTopOS, nCol, Chr( 4 ) /* LOW-ASCII "♦" */ )
   _ftSetWinColor( W_CURR, W_PROMPT )
   hb_DispBox( 3 + nTopOS, 6 + nAddSpace, 5 + nTopOS, 27 + nAddSpace, HB_B_DOUBLE_UNI + " " )

   RETURN

// Change the decimal position in the display
STATIC PROCEDURE _ftChangeDec( aAdder )

   LOCAL nNumDec := nMaxDeci := _ftQuest( "How many decimals do you want to display?", 0, "9", {| oGet | _ftValDeci( oGet ) } )

   cTotPict := Right( _ftStuffComma( Stuff( "9999999999999999999", 19 - nNumDec, 1, "." ) ), 19 )
   cTotPict := iif( nNumDec == 2 .OR. nNumDec == 6, " " + Right( cTotPict, 18 ), cTotPict )

   IF lSubRtn
      _ftDispTotal( aAdder )
   ELSE
      _ftDispSubTot( aAdder )
   ENDIF

   RETURN

// Display total number to Adder Window
STATIC PROCEDURE _ftDispTotal( aAdder )

   IF nTotal > Val( StrTran( cTotPict, "," ) )
      hb_DispOutAt( 4 + nTopOS, 8 + nAddSpace, "****  ERROR  **** " )
      _ftError( "that number is to big to display! I believe the answer was " + ;
         _ftStuffComma( hb_ntos( nTotal ) ) + "." )
      lAddError := .T.
      _ftUpdateTrans( aAdder, .T. )
      _ftClearAdder( aAdder )
      nTotal := nNumTotal := 0
      lAddError := .F.
   ELSE
      hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nTotal, cTotPict ) )
   ENDIF

   RETURN

// Display subtotal number
STATIC PROCEDURE _ftDispSubTot( aAdder )

   IF nNumTotal > Val( StrTran( cTotPict, "," ) )
      hb_DispOutAt( 4 + nTopOS, 8 + nAddSpace, "****  ERROR  **** " )
      _ftError( "that number is to big to display! I believe the answer was " + ;
         _ftStuffComma( hb_ntos( nNumTotal ) ) + "." )
      lAddError := .T.
      _ftUpdateTrans( aAdder, .T., nNumTotal )
      _ftClearAdder( aAdder )
      nTotal := nNumTotal := 0
      lAddError := .F.
   ELSE
      hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nNumTotal, cTotPict ) )
   ENDIF

   RETURN

// Act on NUMBER key pressed
STATIC PROCEDURE _ftProcessNumb( aAdder, nKey )

   LOCAL nNum

   _ftEraseTotSubTot( aAdder )
   lTotalOk  := .F.
   lClAdder  := .F.               // Reset the Clear flag
   lAddError := .F.               // Reset adder error flag

   IF nKey == hb_keyCode( "." )   // Period (.) decimal point
      IF lDecSet                  // Has decimal already been set
         Tone( 800, 1 )
      ELSE
         lDecSet := .T.
      ENDIF
   ELSE                           // It must be a number input
      lNewNum := .T.
      nNum := nKey - hb_keyCode( "0" )
      IF lDecSet                  // Decimal set
         IF nDecDigit < nMaxDeci  // Check how many decimals are allowed
            ++nDecDigit
            nNumTotal += nNum / ( 10 ^ nDecDigit )
         ENDIF
      ELSE
         nNumTotal := nNumTotal * 10 + nNum
      ENDIF
   ENDIF

   _ftDispSubTot( aAdder )

   RETURN

// Enter key - SUBTOTAL/TOTAL
STATIC PROCEDURE _ftAddTotal( aAdder )

   _ftEraseTotSubTot( aAdder )
   lDecSet   := .F.
   nDecDigit :=  0
   lClAdder  := .F.               // Reset the Clear flag
   IF lSubRtn                     // If this was the second time they
      IF ! lMultDiv
         _ftSetWinColor( W_CURR, W_SCREEN )
         hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "   <TOTAL>" )
         _ftSetWinColor( W_CURR, W_PROMPT )
         _ftUpdateTrans( aAdder, .T. )
         _ftDispTotal( aAdder )
         lSubRtn   := .F.         // pressed the total key reset everyting
         nSavTotal := nTotal
         nTotal    := 0
         lTotalOk  := .T.
      ENDIF
   ELSE                           // This was the first time they pressed
      IF ! lMultDiv .AND. LastKey() == K_ENTER // total key
         lSubRtn := .T.
      ENDIF
      IF _ftRoundIt( nTotal, nMaxDeci ) != 0 .OR. _ftRoundIt( nNumTotal, nMaxDeci ) != 0
         IF ! lMultDiv
            _ftSetWinColor( W_CURR, W_SCREEN )
            hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "<SUBTOTAL>" )
            _ftSetWinColor( W_CURR, W_PROMPT )
         ENDIF
         IF _ftRoundIt( nNumTotal, nMaxDeci ) != 0
            lSubRtn := .F.
            _ftUpdateTrans( aAdder, .F., nNumTotal )
         ENDIF
         IF ! lMultDiv
            lSubRtn := .T.        // total key
         ENDIF
         DO CASE
         CASE nAddMode == 1 ; nTotal += nNumTotal
         CASE nAddMode == 2 ; nTotal -= nNumTotal
         CASE nAddMode == 3 ; nTotal *= nNumTotal
         CASE nAddMode == 4 ; nTotal := _ftDivide( aAdder, nTotal, nNumTotal )
            IF lDivError
               _ftError( "you can't divide by ZERO!" )
               lDivError := .F.
            ENDIF
         ENDCASE
      ENDIF
      _ftDispTotal( aAdder )
      IF lMultDiv                 // This was a multiply or divide
         _ftSetWinColor( W_CURR, W_SCREEN )
         hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "   <TOTAL>" )
         _ftSetWinColor( W_CURR, W_PROMPT )
         lSubRtn := .F.           // pressed total so key reset everything
         IF ! lTotalOk            // If you haven't printed total DO-IT
            lTotalOk := .T.
            _ftUpdateTrans( aAdder, .F. )
         ENDIF
         nNumTotal := 0
         nSavTotal := nTotal
         nTotal    := 0
      ELSEIF ! lTotalOk           // If you haven't printed total DO-IT
         _ftUpdateTrans( aAdder, .F. )
         nNumTotal := 0
      ENDIF
   ENDIF

   RETURN

// Process + or - keypress
STATIC PROCEDURE _ftAddSub( aAdder, nKey )

   lMultDiv  := .F.
   _ftEraseTotSubTot( aAdder )
   lTotalOk  := .F.
   lDecSet   := .F.
   nDecDigit := 0
   lSubRtn   := .F.
   // They pressed the + or - key to process the previous total
   IF _ftRoundIt( nNumTotal, nMaxDeci ) == 0 .AND. _ftRoundIt( nTotal, nMaxDeci ) == 0
      nNumTotal := nSavTotal
      lNewNum := .T.
   ENDIF
   DO CASE
   CASE nKey == hb_keyCode( "+" )  // Add
      nAddMode := 1
      IF ! lNewNum                 // They pressed + again to add the same
         nNumTotal := nSavSubTot   // number without re-entering
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     += nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal      // Save this number in case they just press + or -
      nNumTotal  := 0
   CASE nKey == hb_keyCode( "-" )  // Subtract
      nAddMode := 2
      IF ! lNewNum                 // They pressed + again to add the same
         nNumTotal := nSavSubTot   // number without re-entering
         lNewNum   := .T.
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     -= nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal      // Save this number in case they just press + or -
      nNumTotal  := 0
   ENDCASE

   _ftDispTotal( aAdder )

   RETURN

// Process * or / keypress
STATIC PROCEDURE _ftMultDiv( aAdder, nKey )

   lMultDiv  := .T.
   _ftEraseTotSubTot( aAdder )
   lTotalOk  := .F.
   lDecSet   := .F.
   nDecDigit := 0
   lSubRtn   := .F.
   // They pressed the + or - key to process the previous total
   IF _ftRoundIt( nNumTotal, nMaxDeci ) == 0 .AND. _ftRoundIt( nTotal, nMaxDeci ) == 0
      nNumTotal := nSavTotal
   ENDIF
   // Get the first number of the product or division
   IF _ftRoundIt( nTotal, nMaxDeci ) == 0
      DO CASE
      CASE nKey == hb_keyCode( "*" )  // Setup mode
         nAddMode := 3
         _ftUpdateTrans( aAdder, .F., nNumTotal )
      CASE nKey == hb_keyCode( "/" )
         nAddMode := 4
         _ftUpdateTrans( aAdder, .F., nNumTotal )
      ENDCASE
      nTotal    := nNumTotal
      nNumTotal := 0
   ELSE
      DO CASE
      CASE nKey == hb_keyCode( "*" )  // Multiply
         nAddMode  := 3
         _ftUpdateTrans( aAdder, .F., nNumTotal )
         nTotal    := nTotal * nNumTotal
         nNumTotal := 0
      CASE nKey == hb_keyCode( "/" )  // Divide
         nAddMode := 4
         _ftUpdateTrans( aAdder, .F., nNumTotal )
         nTotal := _ftDivide( aAdder, nTotal, nNumTotal )
         IF lDivError
            _ftError( "you can't divide by ZERO!" )
            lDivError := .F.
         ENDIF
         nNumTotal := 0
      ENDCASE
   ENDIF

   _ftDispTotal( aAdder )

   RETURN

// Help window
STATIC PROCEDURE _ftAddHelp()

   _ftPushMessage( ;
      "This Adder works like a desk top calculator. You may add, " + ;
      "subtract, multiply, or divide. " + hb_eol() + hb_eol() + ;
      "When adding or subtracting, the first entry is entered " + ;
      "into the accumulator and each successive entry is " + ;
      "subtotaled. When you press <Enter> the SubTotal is also " + ;
      "shown on the tape. The second time you press <Enter> the " + ;
      "adder is Totaled. When multiplying or dividing the " + ;
      "<Enter> is a Total the first time pressed." + hb_eol() + hb_eol() + ;
      "Hot Keys:" + hb_eol() + ;
      "         <D>ecimals - change # of decimals" + hb_eol() + ;
      "         <M>ove     - the Adder from right to left" + hb_eol() + ;
      "         <T>ape     - turn Tape Display On or Off" + hb_eol() + ;
      "         <S>croll   - the tape display" + hb_eol() + hb_eol() + ;
      "         <Del> ---+-- 1st Clear entry" + hb_eol() + ;
      "                  +-- 2nd Clear Adder" + hb_eol() + ;
      "         <Esc>      - Quit" + hb_eol() + ;
      "         <F10>      - return a <TOTAL> to the active get", ;
      "ADDER HELP", "press any key to continue..." )

   RETURN

// Clear entry / Clear Adder
STATIC PROCEDURE _ftClearAdder( aAdder )

   _ftEraseTotSubTot( aAdder )
   lDecSet   := .F.
   nDecDigit := 0
   IF lClAdder         // If it has alredy been pressed once
      nTotal    := 0   // then we are clearing the total
      nSavTotal := 0
      _ftUpdateTrans( aAdder, .F. )
      lClAdder  := .F.
      _ftDispTotal( aAdder )
   ELSE
      nNumTotal := 0   // Just clearing the last entry
      lClAdder  := .T.
      _ftDispSubTot( aAdder )
   ENDIF

   RETURN

// Update transactions array
STATIC PROCEDURE _ftUpdateTrans( aAdder, lTypeTotal, nAmount )

   LOCAL lUseTotal := ( nAmount == NIL )

   hb_default( @nAmount, 0 )

   IF lClAdder  // Clear the adder (they pressed <DEL> twice
      AAdd( aTrans, Str( 0, 22, nMaxDeci ) + " C" )
      IF lTape  // If there is a tape Show Clear
         _ftDisplayTape( aAdder )
      ENDIF
      RETURN
   ENDIF

   IF lTypeTotal  // Update from total
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )
      aTrans[ Len( aTrans ) ] := _ftStuffComma( ATail( aTrans ), .T. ) + " *" + ;
         iif( lAddError, "ER", "" )

   ELSE           // Update from nNumTotal
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )

      aTrans[ Len( aTrans ) ] := _ftStuffComma( ATail( aTrans ), .T. ) + ;
         iif( lSubRtn, " S", iif( nAddMode == 1, " +", iif( nAddMode == 2, " -", ;
         iif( lTotalOk, " =", iif( nAddMode == 3, " X", " /" ) ) ) ) ) + iif( lAddError, "ER", "" )
   ENDIF

   IF lTape
      _ftDisplayTape( aAdder )
   ENDIF

   RETURN

// Clear the <TOTAL> and <SUBTOTAL> from Adder
STATIC PROCEDURE _ftEraseTotSubTot( aAdder )

   _ftSetWinColor( W_CURR, W_SCREEN )
   hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, Space( 10 ) )
   _ftSetWinColor( W_CURR, W_PROMPT )

   RETURN

// Adder Rounding function
STATIC FUNCTION _ftRoundIt( nNumber, nPlaces )

   hb_default( @nPlaces, 0 )

   RETURN iif( nNumber < 0, -1.0, 1.0 ) * ;
      Int( Abs( nNumber ) * 10 ^ nPlaces + 0.50 + 10 ^ -12 ) / 10 ^ nPlaces

// Check divide by zero not allowed
STATIC FUNCTION _ftDivide( aAdder, nNumerator, nDenominator )

   IF nDenominator == 0
      lDivError := .T.
      RETURN 0
   ENDIF

   lDivError := .F.

   RETURN nNumerator / nDenominator

// Validate the number of decimals
STATIC FUNCTION _ftValDeci( oGet )

   IF oGet:VarGet() > 8
      _ftError( "no more than 8 decimal places please!" )
      RETURN .F.
   ENDIF

   RETURN .T.

// Display the Tape
STATIC PROCEDURE _ftDisplayTape( aAdder, nKey )

   LOCAL nDispTape, nTopTape := 1

   IF hb_keyChar( nKey ) $ "Tt" .AND. lTape  // Stop displaying tape
      lTape := .F.
      RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
      RETURN
   ENDIF
   IF lTape                  // Are we in the display mode
      SetColor( "N/W" )
      hb_Scroll( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, 1 )
      IF Len( aTrans ) > 0   // Any transactions been entered yet?
         hb_DispOutAt( 20 + nTopOS, 7 + nTapeSpace, ATail( aTrans ) )
      ENDIF
      _ftSetWinColor( W_CURR, W_PROMPT )
   ELSE                      // Start displaying tape
      lTape := .T.
      SetColor( "N/W" )
      cTapeScr := SaveScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace )
      hb_Shadow( 4 + nTopOS, 6 + nTapeSpace, 21 + nTopOS, 33 + nTapeSpace )
      hb_DispBox( 4 + nTopOS, 6 + nTapeSpace, 21 + nTopOS, 33 + nTapeSpace, HB_B_SINGLE_UNI + " ", "R+/W" )
      hb_DispOutAt( 4 + nTopOS, 17 + nTapeSpace, " TAPE ", "GR+/W" )
      SetColor( "N/W" )
      IF Len( aTrans ) > 15
         nTopTape := Len( aTrans ) - 15
      ENDIF
      FOR nDispTape := Len( aTrans ) TO nTopTape STEP -1
         hb_DispOutAt( 20 + nDispTape - Len( aTrans ) + nTopOS, 7 + nTapeSpace, aTrans[ nDispTape ] )
      NEXT
   ENDIF
   _ftSetWinColor( W_CURR, W_PROMPT )

   RETURN

// Sets the LastKey() value to value of nLastKey
// NOTE: I use this in most of my Pop-Up routines to reset the
//       original value of LastKey() when quitting.
STATIC PROCEDURE _ftSetLastKey( nLastKey )

   _ftPushKeys()
   hb_keySetLast( nLastKey )
   _ftPopKeys()

   RETURN

// Push any keys in the Keyboard buffer on the array t_aKeys[]
// NOTE: Save any keys in the buffer. for FAST typists <g>.
STATIC PROCEDURE _ftPushKeys()

   LOCAL nKey

   DO WHILE ( nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) ) != 0
      AAdd( t_aKeys, nKey )
   ENDDO

   RETURN

// Restore the keyboard with any keystrokes that were saved with _ftPushKeys
STATIC PROCEDURE _ftPopKeys()

   IF Len( t_aKeys ) > 0
      hb_keyPut( t_aKeys )
      t_aKeys := {}
   ENDIF

   RETURN

// Display a message on the screen in a window
STATIC PROCEDURE _ftPushMessage( cMessage, cTitle, cBotTitle )

   LOCAL cOldColor   := SetColor()
   LOCAL nOldLastkey := hb_keyLast()
   LOCAL nOldRow     := Row()
   LOCAL nOldCol     := Col()
   LOCAL nOldCurs    := SetCursor( SC_NONE )
   LOCAL nWinColor   := W_CURR

   LOCAL nMessLen  := Len( cMessage )
   LOCAL nWide     := iif( nMessLen > 72, 72, iif( nMessLen < 12, 12, nMessLen ) )
   LOCAL nNumRows  := MLCount( cMessage, nWide )

   LOCAL nTop    := Int( ( MaxRow() - nNumRows ) / 2 )  // Center it on the screen
   LOCAL nLeft   := Int( ( MaxCol() - nWide ) / 2 ) - 3
   LOCAL nBottom := nTop + nNumRows + 2
   LOCAL nRight  := nLeft + nWide + 4

   _ftPushWin( nTop, nLeft, nBottom, nRight, cTitle, cBotTitle, nWinColor )
   _ftDispMessage( cMessage, nTop + 1, nLeft + 2, nBottom - 1, nRight - 2 )
   _ftInkey()
   _ftPopWin()

   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   _ftSetLastKey( nOldLastKey )

   RETURN

// Push a Question Box on the Screen
// NOTE: This function will work for all Data Types
STATIC FUNCTION _ftQuest( cMessage, xVarVal, cPict, bValid )

   LOCAL oNewGet
   LOCAL cVarType := ValType( xVarVal )
   LOCAL nVarLen  := ;
      iif( cVarType == "C", Len( xVarVal ), ;
      iif( cVarType == "D", 10, ;
      iif( cVarType == "L", 1, ;
      iif( cVarType == "N", iif( cPict == NIL, 9, Len( cPict ) ), 0 ) ) ) )
   LOCAL nOldLastKey := hb_keyLast()

   LOCAL nOldRow   := Row()
   LOCAL nOldCol   := Col()
   LOCAL nOldCurs  := SetCursor( SC_NONE )
   LOCAL cOldColor := SetColor()

   LOCAL nMessLen := Len( cMessage ) + nVarLen + 1
   LOCAL nWide    := iif( nMessLen > 66, 66, iif( nMessLen < 12, 12, nMessLen ) )

   LOCAL nNumMessRow    := MLCount( cMessage, nWide )
   LOCAL nLenLastRow    := Len( RTrim( MemoLine( cMessage, nWide, nNumMessRow ) ) )
   LOCAL lGetOnNextLine := ( nLenLastRow + nVarLen ) > nWide
   LOCAL nNumRows       := nNumMessRow + iif( lGetOnNextLine, 1, 0 )

   // Center it in the screen
   LOCAL nTop    := Int( ( MaxRow() - nNumRows ) / 2 )
   LOCAL nBottom := nTop + nNumRows + 1
   LOCAL nLeft   := Int( ( MaxCol() - nWide ) / 2 ) - 4
   LOCAL nRight  := nLeft + nWide + 4

   _ftPushWin( nTop, nLeft, nBottom, nRight, "QUESTION ?", iif( HB_ISSTRING( xVarVal ) ;
      .AND. nVarLen > nWide, /* LOW-ASCII "←" */ Chr( 27 ) + " scroll " + Chr( 26 ) /* LOW-ASCII "→" */, NIL ) )
   _ftDispMessage( cMessage, nTop + 1, nLeft + 2, nBottom - 1, nRight - 2 )

   oNewGet := GetNew( ;
      iif( lGetOnNextLine, Row() + 1, Row() ), ;
      iif( lGetOnNextLine, nLeft + 2, Col() + 1 ), ;
      {| x | iif( PCount() > 0, xVarVal := x, xVarVal ) }, ;
      "xVarVal" )

   // If the input line is character and wider than window SCROLL
   IF lGetOnNextLine .AND. HB_ISSTRING( xVarVal ) .AND. nVarLen > nWide
      oNewGet:Picture := "@S" + hb_ntos( nWide ) + iif( cPict == NIL, "", " " + cPict )
   ENDIF

   IF cPict != NIL           // Use the picture they passed
      oNewGet:Picture := cPict
   ELSE                      // Else setup default pictures
      DO CASE
      CASE HB_ISDATE( xVarVal )    ; oNewGet:Picture := "9999-99-99"
      CASE HB_ISLOGICAL( xVarVal ) ; oNewGet:Picture := "Y"
      CASE HB_ISNUMERIC( xVarVal ) ; oNewGet:Picture := "999999.99"  // Guess that they are inputting dollars
      ENDCASE
   ENDIF

   oNewGet:PostBlock := iif( bValid == NIL, NIL, bValid )
   oNewGet:Display()

   SetCursor( SC_NORMAL )
   ReadModal( { oNewGet } )  // without reissuing the gets

   _ftPopWin()

   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   _ftSetLastKey( nOldLastKey )

   RETURN xVarVal

// User function for AChoice() when scrolling tape
STATIC FUNCTION _ftAdderTapeUDF( mode, cur_elem, rel_pos, /* @ */ lAC_exit_ok )

   HB_SYMBOL_UNUSED( cur_elem )
   HB_SYMBOL_UNUSED( rel_pos )

   IF mode == AC_EXCEPT
      SWITCH LastKey()
      CASE K_ESC
         hb_keyPut( { K_CTRL_PGDN, K_ENTER } )  // Go to last item
         lAC_exit_ok := .T.
         // Fall through
      CASE K_CTRL_PGDN
         RETURN AC_CONT
      OTHERWISE
         IF lAC_exit_ok
            lAC_exit_ok := .F.
            RETURN AC_ABORT
         ENDIF
      ENDSWITCH
   ENDIF

   RETURN AC_CONT

// Display an ERROR message in a window
STATIC PROCEDURE _ftError( cMessage )

   LOCAL nTop, nLeft, nBot, nRight
   LOCAL cErrorScr, nMessLen, nWide, nNumRows

   LOCAL nOldLastKey := hb_keyLast()
   LOCAL nOldRow := Row()
   LOCAL nOldCol := Col()
   LOCAL nOldCurs := SetCursor( SC_NONE )
   LOCAL cOldColor := _ftSetScrColor( STD_ERROR )

   cMessage := "I'm sorry but, " + cMessage
   nMessLen := Len( cMessage )
   nWide    := iif( nMessLen > 66, 66, iif( nMessLen < 12, 12, nMessLen ) )
   nNumRows := MLCount( cMessage, nWide )
   nTop     := Int( ( MaxRow() - nNumRows ) / 2 )  // Center it in the screen
   nBot     := nTop + 3 + nNumRows
   nLeft    := Int( ( MaxCol() - nWide ) / 2 ) - 2
   nRight   := nLeft + nWide + 4

   cErrorScr := SaveScreen( nTop, nLeft, nBot + 1, nRight + 2 )
   hb_Shadow( nTop, nLeft, nBot, nRight )
   hb_DispBox( nTop, nLeft, nBot, nRight, HB_B_SINGLE_UNI + " " )
   hb_DispOutAt( nTop, nLeft + Int( nWide / 2 ) - 1, " ERROR " )
   hb_DispOutAt( nBot - 1, nLeft + Int( nWide - 28 ) / 2 + 3, "Press any key to continue..." )
   _ftDispMessage( cMessage, nTop + 1, nLeft + 3, nBot - 2, nRight - 3 )
   Tone( 70, 5 )
   _ftInkey()
   RestScreen( nTop, nLeft, nBot + 1, nRight + 2, cErrorScr )
   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   _ftSetLastKey( nOldLastKey )

   RETURN

// Stuff a Comma in a string
STATIC FUNCTION _ftStuffComma( cStrToStuff, lTrimStuffedStr )

   LOCAL nDecPosit, x

   IF !( "." $ cStrToStuff )
      cStrToStuff := Stuff( cStrToStuff, iif( ;
         "C" $ cStrToStuff .OR. "E" $ cStrToStuff .OR. "+" $ cStrToStuff .OR. ;
         "-" $ cStrToStuff .OR. "X" $ cStrToStuff .OR. "*" $ cStrToStuff .OR. ;
         Chr( 4 ) /* LOW-ASCII "♦" */ $ cStrToStuff .OR. "/" $ cStrToStuff .OR. "=" $ cStrToStuff, ;
         Len( cStrToStuff ) - 1, Len( cStrToStuff ) + 1 ), 0, "." )

      IF cStrToStuff == " " .OR. cStrToStuff == "0"
         cStrToStuff := ""
      ENDIF
   ENDIF

   nDecPosit := At( ".", cStrToStuff )

   IF Len( Left( LTrim( StrTran( cStrToStuff, "-" ) ), ;
      At( ".", LTrim( StrTran( cStrToStuff, "-" ) ) ) - 1 ) ) > 3
      IF hb_defaultValue( lTrimStuffedStr, .F. )  // Do we trim the number each time we insert a comma
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff ) STEP -4
            cStrToStuff := SubStr( Stuff( cStrToStuff, x, 0, "," ), 2 )
         NEXT
      ELSE
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff ) STEP -3
            cStrToStuff := Stuff( cStrToStuff, x, 0, "," )
         NEXT
      ENDIF
   ENDIF

   RETURN cStrToStuff

// Set the standard screen colors to the color requested.
// See Also: _ftSetWinColor()
STATIC FUNCTION _ftSetScrColor( nStd, nEnh, nBord, nBack, nUnsel )

   IF Empty( t_aWinColor )
      _ftInitColors()
   ENDIF

   hb_default( @nEnh, 8 )

   RETURN SetColor( ;
      t_aStdColor[ hb_defaultValue( nStd, 8 ) ] + "," + ;
      t_aStdColor[ nEnh ] + "," + ;
      t_aStdColor[ hb_defaultValue( nBord, 8 ) ] + "," + ;
      t_aStdColor[ hb_defaultValue( nBack, 8 ) ] + "," + ;
      t_aStdColor[ hb_defaultValue( nUnsel, nEnh ) ] )

// Push a new window on the screen in the position t,l,b,r
// and if cTitle is not NIL print the title for the window
// in centered in the top line of the box. Similarly do
// the same for cBotTitle. If nWinColor==NIL get the next
// window color and use it for all the colors. If
// cTypeBord==NIL use the single line border, else use the
// one they requested. Push the window coordinates, the
// color number, the SaveScreen() value, and whether they
// picked the window color they wanted to use. If
// lAutoWindow=.F. then the window color was incremented
// and we will will restore the color number when we pop
// the window off.
STATIC PROCEDURE _ftPushWin( t, l, b, r, cTitle, cBotTitle, nWinColor )

   LOCAL lAutoWindow := ( nWinColor == NIL )

   nWinColor := iif( nWinColor == NIL, _ftNextWinColor(), nWinColor )
   AAdd( t_aWindow, { t, l, b, r, nWinColor, SaveScreen( t, l, b + 1, r + 2 ), lAutoWindow } )
   hb_Shadow( t, l, b, r )
   _ftSetWinColor( nWinColor, W_BORDER )
   hb_DispBox( t, l, b, r, HB_B_SINGLE_UNI + " " )

   IF cTitle != NIL
      _ftSetWinColor( nWinColor, W_TITLE )
      _ftWinTitle( cTitle )
   ENDIF

   IF cBotTitle != NIL
      _ftSetWinColor( nWinColor, W_TITLE )
      _ftWinTitle( cBotTitle, "bot" )
   ENDIF

   _ftSetWinColor( nWinColor, W_SCREEN, W_VARIAB )
   hb_Scroll( t + 1, l + 1, b - 1, r - 1 )

   RETURN

// Pop the currently active window off the screen by restoring
// it from the t_aWindow Array and if they pushed a new window
// automatically selecting the color we will roll back the
// current window setting using _ftLastWinColor() and reset
// the color to the color setting when window was pushed.
STATIC PROCEDURE _ftPopWin()

   LOCAL nNumWindow := Len( t_aWindow )

   RestScreen( t_aWindow[ nNumWindow, 1 ], t_aWindow[ nNumWindow, 2 ], ;
      t_aWindow[ nNumWindow, 3 ] + 1, t_aWindow[ nNumWindow, 4 ] + 2, ;
      t_aWindow[ nNumWindow, 6 ] )

   IF t_aWindow[ nNumWindow, 7 ]
      _ftLastWinColor()
   ENDIF

   ASize( t_aWindow, Len( t_aWindow ) - 1 )

   IF Empty( t_aWindow )
      _ftSetScrColor( STD_SCREEN, STD_VARIABLE )
   ELSE
      _ftSetWinColor( W_CURR, W_SCREEN, W_VARIAB )
   ENDIF

   RETURN

// Set the Color to the Window Colors requested
// See Also: _ftSetScrColor()
// NOTE: If the window number is not passed use the currently
//       active window number nWinColor.
STATIC FUNCTION _ftSetWinColor( nWin, nStd, nEnh, nBord, nBack, nUnsel )

   hb_default( @nWin, t_nWinColor )
   hb_default( @nEnh, 7 )

   RETURN SetColor( ;
      t_aWinColor[ hb_defaultValue( nStd, 7 ), nWin ] + "," + ;
      t_aWinColor[ nEnh, nWin ] + "," + ;
      t_aWinColor[ hb_defaultValue( nBord, 7 ), nWin ] + "," + ;
      t_aWinColor[ hb_defaultValue( nBack, 7 ), nWin ] + "," + ;
      t_aWinColor[ hb_defaultValue( nUnsel, nEnh ), nWin ] )

// Decrement the active window color number and return the current value
// NOTE: If we are already on window #1 restart count by using # 4.
STATIC FUNCTION _ftLastWinColor()
   RETURN t_nWinColor := iif( t_nWinColor == 1, 4, t_nWinColor - 1 )

// Increment the active window color number and return the current value
// NOTE: If we are already on window #4 restart count by using # 1.
STATIC FUNCTION _ftNextWinColor()

   IF Empty( t_aWinColor )
      _ftInitColors()
   ENDIF

   RETURN t_nWinColor := iif( t_nWinColor < 4, t_nWinColor + 1, 1 )

// Print the top or bottom titles on the border of the currently active window
STATIC PROCEDURE _ftWinTitle( cTheTitle, cTopOrBot )

   LOCAL nCurWin  := Len( t_aWindow )
   LOCAL nLenTitle := Len( cTheTitle )

   hb_DispOutAt( t_aWindow[ nCurWin, iif( cTopOrBot == NIL, 1, 3 ) ], ( t_aWindow[ nCurWin, 4 ] - ;
      t_aWindow[ nCurWin, 2 ] - nLenTitle ) / 2 + t_aWindow[ nCurWin, 2 ], " " + cTheTitle + " " )

   RETURN

// Initilize the colors for the Adder
STATIC PROCEDURE _ftInitColors()

   t_aWinColor := { ;
      { "GR+/BG", "GR+/G", "B+/RB",  "G+/R"  }, ;
      { "R+/N",   "W+/RB", "W+/BG",  "GR+/B" }, ;
      { "GR+/N",  "GR+/N", "GR+/N",  "GR+/N" }, ;
      { "B/BG",   "BG+/G", "W+/RB",  "BG+/R" }, ;
      { "W+/BG",  "W+/G",  "GR+/RB", "W+/R"  }, ;
      { "GR+/B",  "GR+/R", "R+/B",   "W+/BG" }, ;
      { "N/N",    "N/N",   "N/N",    "N/N"   } }

   t_aStdColor := { ;
      "BG+*/RB", ;
      "GR+/R", ;
      "GR+/N", ;
      "W/B", ;
      "GR+/N", ;
      "GR+/GR", ;
      { ;
      "W+/B",  "W/B",   "G+/B", "R+/B", ;
      "GR+/B", "BG+/B", "B+/B", "G+/B" }, ;
      "N/N" }

   RETURN

// Returns the number of spaces on the Left side of the String
STATIC FUNCTION _ftCountLeft( cString )
   RETURN Len( cString ) - Len( LTrim( cString ) )

STATIC FUNCTION _ftInkey()

   LOCAL nKey, nKeyStd, bBlock

   DO WHILE .T.
      nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

      IF ( bBlock := SetKey( nKey ) ) != NIL .OR. ;
         ( bBlock := SetKey( nKeyStd ) ) != NIL
         Eval( bBlock, ProcName(), ProcLine(), "nKey" )
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN nKeyStd

STATIC PROCEDURE _ftDispMessage( cMessage, nT, nL, nB, nR )

   _ftPushKeys()
   hb_keyPut( { K_CTRL_PGDN, K_CTRL_W } )
   MemoEdit( cMessage, nT, nL, nB, nR, .F.,, nR - nL + 1 )
   _ftPopKeys()

   RETURN
