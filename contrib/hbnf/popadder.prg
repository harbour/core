/*
 * Author....: Keith A. Wire (docs included)
 * CIS ID....: 73760,2427
 *
 * This is an original work by Keith A. Wire and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   03 Mar 1994 19:47:22   GLENN
 * Author made some enhancements and modifications.
 *
 *    Rev 1.3   19 Jan 1993 19:52:52   GLENN
 * Removed reference to K_SPACE, as this has been defined in Clipper
 * 5.2's inkey.ch.
 *
 *    Rev 1.2   17 Aug 1991 15:44:30   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.1   15 Aug 1991 23:04:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 17:37:54   GLENN
 * Initial revision.
 *
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

#define FT_B_DOUBLE  HB_B_DOUBLE_UNI + " "
#define FT_B_SINGLE  HB_B_SINGLE_UNI + " "

// Instead of using STATIC variables for these I'm using a LOCAL array
//   and passing aAdder[] all over the place.... Don't let this confuse
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

// I still use a few of STATICS, but most are set to NIL when quiting...
THREAD STATIC t_lAdderOpen := .F.
THREAD STATIC t_aKeys
THREAD STATIC t_aWindow
THREAD STATIC t_nWinColor
THREAD STATIC t_aWinColor
THREAD STATIC t_aStdColor

// Pop Up Adder / Calculator with Tape Display
// NOTE: To make ft_Adder() pop up from any wait state in your
//       application just insert the line:
//         SET KEY K_ALT_A TO {|| ft_Adder() }
//       at the top of your application

PROCEDURE ft_Adder()

   LOCAL nOldDecim, cMoveTotSubTot, cTotal, lDone, nKey
   LOCAL oGet        := GetActive()
   LOCAL nOldCurs    := SetCursor( SC_NONE )
   LOCAL nOldRow     := Row()
   LOCAL nOldCol     := Col()
   LOCAL bOldF10     := SetKey( K_F10, NIL )
   LOCAL nOldLastKey := LastKey()
   LOCAL lShowRight  := .T.
   LOCAL aAdder      := Array( 23 )
   LOCAL tmp, tmp1

   LOCAL lAC_exit_ok

   // Must prevent recursive calls

   IF t_lAdderOpen
      RETURN
   ELSE
      t_lAdderOpen := .T.
   ENDIF

   aTrans       := { "                  0.00 C " }
   nOldDecim    := Set( _SET_DECIMALS, 9 )
   cTotPict     := "999999999999999.99"
   cTapeScr     := ""
   nTotal       := nNumTotal := nSavTotal := nDecDigit := 0
   lDone        := .F.                   // Loop flag
   nMaxDeci     := 2                     // Initial # of decimals
   nSavSubTot   := 0
   lNewNum      := .F.
   nAddMode     := 1                     // Start in ADD mode
   lMultDiv     := .F.                   // Start in ADD mode
   lClAdder     := .F.                   // Clear adder flag
   lDecSet      := .F.                   // Decimal ? - keyboard routine
   lSubRtn      := lTotalOk := lTape := lAddError := lDivError := .F.

   nTopOS       := Int( ( MaxRow() - 24 ) / 2 )  // Using the TopOffSet and LeftOffSet
   nLeftOS      := Int( ( MaxCol() - 79 ) / 2 )  // the Adder will always be centered
   nAddSpace    := iif( lShowRight, 40, 0 ) + nLeftOS
   nTapeSpace   := iif( lShowRight, 0, 40 ) + nLeftOS

   // Set Up the STATIC variables
   t_aKeys      := {}
   t_aWindow    := {}
   t_nWinColor  := 0

   _ftAddScreen( aAdder )

   // Set the decimals to 2 and display a cleared adder
   _ftChangeDec( aAdder, 2 )
   hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nTotal, cTotPict ) )

   DO WHILE ! lDone                      // Input key and test loop
      nKey := _ftInkey( 0, "nKey" )
      DO CASE
      CASE hb_keyChar( nKey ) $ "1234567890."
         _ftProcessNumb( aAdder, nKey )
      CASE nKey == hb_keyCode( "+" )    // <+> sign
         _ftAddSub( aAdder, nKey )
      CASE nKey == hb_keyCode( "-" )    // <-> sign
         _ftAddSub( aAdder, nKey )
      CASE nKey == hb_keyCode( "*" )    // <*> sign
         _ftMultDiv( aAdder, nKey )
      CASE nKey == hb_keyCode( "/" )    // </> sign
         _ftMultDiv( aAdder, nKey )
      CASE nKey == K_ENTER              // <RTN> Total or Subtotal
         _ftAddTotal( aAdder )
      CASE nKey == K_ESC                // <ESC> Quit
         Set( _SET_DECIMALS, nOldDecim )
         SetCursor( nOldCurs )
         IF lTape
            RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
         ENDIF
         _ftPopWin()
         SetPos( nOldRow, nOldCol )
         _ftSetLastKey( nOldLastKey )
         SetKey( K_F10, bOldF10 )
         t_lAdderOpen := .F.               // Reset the recursive flag
         lDone      := .T.
      CASE nKey == hb_keyCode( "D" ) .OR. nKey == hb_keyCode( "d" )  // <D> Change number of decimal places
         _ftChangeDec( aAdder )
      CASE nKey == hb_keyCode( "T" ) .OR. nKey == hb_keyCode( "t" )  // <T> Display Tape
         _ftDisplayTape( aAdder, nKey )
      CASE nKey == hb_keyCode( "M" ) .OR. nKey == hb_keyCode( "m" )  // <M> Move Adder
         IF lTape
            RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
         ENDIF
         IF !( __XSaveGetChar( SaveScreen( 6 + nTopOS, 26 + nAddSpace, 6 + nTopOS, 27 + nAddSpace ), 0 ) == " " )
            IF __XSaveGetChar( SaveScreen( 6 + nTopOS, 19 + nAddSpace, 6 + nTopOS, 20 + nAddSpace ), 0 ) == "S"
               cMoveTotSubTot := "S"
            ELSE
               cMoveTotSubTot := "T"
            ENDIF
         ELSE
            cMoveTotSubTot := " "
         ENDIF
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
            hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, iif( cMoveTotSubTot == "T", "   <TOTAL>", ;
               "<SUBTOTAL>" ) )
            _ftSetWinColor( W_CURR, W_PROMPT )
         ENDIF
      CASE ( nKey == hb_keyCode( "S" ) .OR. nKey == hb_keyCode( "s" ) ) .AND. lTape  // <S> Scroll tape display
         IF Len( aTrans ) > 16           // We need to scroll
            SetColor( "GR+/W" )
            hb_DispOutAt( 21 + nTopOS, 8 + nTapeSpace, " " + /* LOW-ASCII "↑↓" */ Chr( 24 ) + Chr( 25 ) + "-SCROLL  <ESC>-QUIT " )
            SetColor( "N/W,W+/N" )
            lAC_exit_ok := .F.
            AChoice( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, aTrans, .T., ;
               {| nMode, cur_elem, rel_pos | _ftAdderTapeUDF( nMode, cur_elem, rel_pos, @lAC_exit_ok ) }, Len( aTrans ), 20 )
            SetColor( "R+/W" )
            hb_DispBox( 21 + nTopOS, 8 + nTapeSpace, 21 + nTopOS, 30 + nTapeSpace, HB_B_SINGLE_UNI )
            _ftSetWinColor( W_CURR, W_PROMPT )
            CLEAR TYPEAHEAD
         ELSE
            _ftError( "there are " + iif( Len( aTrans ) > 0, "only " + ;
               hb_ntos( Len( aTrans ) ), "no" ) + ;
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
               Set( _SET_DECIMALS, nOldDecim )
               SetCursor( nOldCurs )
               IF lTape
                  RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
               ENDIF
               _ftPopWin()
               SetPos( nOldRow, nOldCol )
               _ftSetLastKey( nOldLastKey )
               SetKey( K_F10, bOldF10 )
               oGet:VARPUT( nSavTotal )
               t_lAdderOpen := .F.           // Reset the recursive flag
               lDone        := .T.
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

   // Reset the STATICS to NIL
   t_aKeys := t_aWindow := t_aWinColor := t_aStdColor := NIL

   RETURN

// Display the Adder

STATIC PROCEDURE _ftAddScreen( aAdder )

   LOCAL nCol

   _ftPushWin( 2 + nTopOS, 2 + nAddSpace, 22 + nTopOS, 30 + nAddSpace, "   Adder   ", ;
      "<F-1> for Help",, FT_B_DOUBLE )
   nCol := 5 + nAddSpace
   hb_DispOutAt(  7 + nTopOS, nCol, hb_UTF8ToStr( "      ┌───┐ ┌───┐ ┌───┐" ) )
   hb_DispOutAt(  8 + nTopOS, nCol, hb_UTF8ToStr( "      │   │ │   │ │   │" ) )
   hb_DispOutAt(  9 + nTopOS, nCol, hb_UTF8ToStr( "      └───┘ └───┘ └───┘" ) )
   hb_DispOutAt( 10 + nTopOS, nCol, hb_UTF8ToStr( "┌───┐ ┌───┐ ┌───┐ ┌───┐" ) )
   hb_DispOutAt( 11 + nTopOS, nCol, hb_UTF8ToStr( "│   │ │   │ │   │ │   │" ) )
   hb_DispOutAt( 12 + nTopOS, nCol, hb_UTF8ToStr( "└───┘ └───┘ └───┘ │   │" ) )
   hb_DispOutAt( 13 + nTopOS, nCol, hb_UTF8ToStr( "┌───┐ ┌───┐ ┌───┐ │   │" ) )
   hb_DispOutAt( 14 + nTopOS, nCol, hb_UTF8ToStr( "│   │ │   │ │   │ │   │" ) )
   hb_DispOutAt( 15 + nTopOS, nCol, hb_UTF8ToStr( "└───┘ └───┘ └───┘ └───┘" ) )
   hb_DispOutAt( 16 + nTopOS, nCol, hb_UTF8ToStr( "┌───┐ ┌───┐ ┌───┐ ┌───┐" ) )
   hb_DispOutAt( 17 + nTopOS, nCol, hb_UTF8ToStr( "│   │ │   │ │   │ │   │" ) )
   hb_DispOutAt( 18 + nTopOS, nCol, hb_UTF8ToStr( "└───┘ └───┘ └───┘ │   │" ) )
   hb_DispOutAt( 19 + nTopOS, nCol, hb_UTF8ToStr( "┌─────────┐ ┌───┐ │   │" ) )
   hb_DispOutAt( 20 + nTopOS, nCol, hb_UTF8ToStr( "│         │ │   │ │   │" ) )
   hb_DispOutAt( 21 + nTopOS, nCol, hb_UTF8ToStr( "└─────────┘ └───┘ └───┘" ) )
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
   hb_DispBox( 3 + nTopOS, 6 + nAddSpace, 5 + nTopOS, 27 + nAddSpace, FT_B_DOUBLE )

   RETURN

// Change the decimal position in the display

STATIC PROCEDURE _ftChangeDec( aAdder, nNumDec )

   LOCAL cDefTotPict  := "9999999999999999999"

   IF nNumDec == NIL
      nNumDec := 0

      nNumDec := _ftQuest( "How many decimals do you want to display?", ;
         nNumDec, "9", {| oGet | _ftValDeci( oGet ) } )

      cTotPict := _ftPosRepl( cDefTotPict, ".", 19 - Abs( nNumDec ) )

      cTotPict := Right( _ftStuffComma( cTotPict ), 19 )
      cTotPict := iif( nNumDec == 2 .OR. nNumDec == 6, " " + Right( cTotPict, 18 ), cTotPict )

      nMaxDeci := nNumDec

      IF lSubRtn
         _ftDispTotal( aAdder )
      ELSE
         _ftDispSubTot( aAdder )
      ENDIF

   ENDIF

   RETURN

// Display total number to Adder Window

STATIC PROCEDURE _ftDispTotal( aAdder )

   LOCAL cTotStr

   IF nTotal > Val( _ftCharRem( ",", cTotPict ) )
      cTotStr   := _ftStuffComma( hb_ntos( nTotal ) )
      hb_DispOutAt( 4 + nTopOS, 8 + nAddSpace, "****  ERROR  **** " )
      _ftError( "that number is to big to display! I believe the answer was " + ;
         cTotStr + "." )
      lAddError := .T.
      _ftUpdateTrans( aAdder, .T., NIL )
      _ftClearAdder( aAdder )
      nTotal    := 0
      nNumTotal := 0
      lAddError := .F.
   ELSE
      hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nTotal, cTotPict ) )
   ENDIF

   RETURN

// Display subtotal number

STATIC PROCEDURE _ftDispSubTot( aAdder )

   LOCAL cStotStr

   IF nNumTotal > Val( _ftCharRem( ",", cTotPict ) )
      cStotStr  := _ftStuffComma( hb_ntos( nNumTotal ) )
      hb_DispOutAt( 4 + nTopOS, 8 + nAddSpace, "****  ERROR  **** " )
      _ftError( "that number is to big to display! I believe the answer was " + ;
         cStotStr + "." )
      lAddError := .T.
      _ftUpdateTrans( aAdder, .T., nNumTotal )
      _ftClearAdder( aAdder )
      nTotal    := 0
      nNumTotal := 0
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
   lClAdder  := .F.                      // Reset the Clear flag
   lAddError := .F.                      // Reset adder error flag

   IF nKey == hb_keyCode( "." )          // Period (.) decimal point
      IF lDecSet                         // Has decimal already been set
         Tone( 800, 1 )
      ELSE
         lDecSet := .T.
      ENDIF
   ELSE                                  // It must be a number input
      lNewNum := .T.
      nNum := nKey - hb_keyCode( "0" )
      IF lDecSet                         // Decimal set
         IF nDecDigit < nMaxDeci         // Check how many decimals are allowed
            ++nDecDigit
            nNumTotal += nNum / ( 10 ^ nDecDigit )
         ENDIF
      ELSE
         nNumTotal := nNumTotal * 10 + nNum
      ENDIF
   ENDIF

   _ftDispSubTot( aAdder )

   RETURN

// Enter key - SUBTOTAL\TOTAL

STATIC PROCEDURE _ftAddTotal( aAdder )

   _ftEraseTotSubTot( aAdder )
   lDecSet   := .F.
   nDecDigit :=  0
   lClAdder  := .F.                      // Reset the Clear flag
   IF lSubRtn                            // If this was the second time they
      IF ! lMultDiv
         _ftSetWinColor( W_CURR, W_SCREEN )
         hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "   <TOTAL>" )
         _ftSetWinColor( W_CURR, W_PROMPT )
         _ftUpdateTrans( aAdder, .T., NIL )
         _ftDispTotal( aAdder )
         lSubRtn   := .F.                  // pressed the total key reset everyting
         nSavTotal := nTotal
         nTotal    := 0
         lTotalOk  := .T.
      ENDIF
   ELSE                                  // This was the first time they pressed
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
            lSubRtn := .T.                  // total key
         ENDIF
         DO CASE
         CASE nAddMode == 1                  // Add
            nTotal += nNumTotal
         CASE nAddMode == 2              // Subtract
            nTotal -= nNumTotal
         CASE nAddMode == 3              // Multiply
            nTotal *= nNumTotal
         CASE nAddMode == 4              // Divide
            nTotal := _ftDivide( aAdder, nTotal, nNumTotal )
            IF lDivError
               _ftError( "you can't divide by ZERO!" )
               lDivError := .F.
            ENDIF
         ENDCASE
      ENDIF
      _ftDispTotal( aAdder )
      IF lMultDiv                         // This was a multiply or divide
         _ftSetWinColor( W_CURR, W_SCREEN )
         hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "   <TOTAL>" )
         _ftSetWinColor( W_CURR, W_PROMPT )
         lSubRtn := .F.                    // pressed total so key reset everything
         IF ! lTotalOk                     // If you haven't printed total DO-IT
            lTotalOk := .T.
            _ftUpdateTrans( aAdder, .F., NIL )
         ENDIF
         nNumTotal := 0
         nSavTotal := nTotal
         nTotal    := 0
      ELSE
         IF ! lTotalOk                     // If you haven't printed total DO-IT
            _ftUpdateTrans( aAdder, .F., NIL )
            nNumTotal := 0
         ENDIF
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
      lNewNum   := .T.
   ENDIF
   DO CASE
   CASE nKey == hb_keyCode( "+" )          // Add
      nAddMode := 1
      IF ! lNewNum                         // They pressed + again to add the same
         nNumTotal := nSavSubTot           // number without re-entering
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     += nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal   // Save this number in case they just press + or -
      nNumTotal  := 0
   CASE nKey == hb_keyCode( "-" )          // Subtract
      nAddMode := 2
      IF ! lNewNum                         // They pressed + again to add the same
         nNumTotal := nSavSubTot           // number without re-entering
         lNewNum   := .T.
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     -= nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal   // Save this number in case they just press + or -
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
      CASE nKey == hb_keyCode( "*" )       // Setup mode
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
      CASE nKey == hb_keyCode( "*" )       // Multiply
         nAddMode  := 3
         _ftUpdateTrans( aAdder, .F., nNumTotal )
         nTotal    := nTotal * nNumTotal
         nNumTotal := 0
      CASE nKey == hb_keyCode( "/" )       // Divide
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

   LOCAL cMess := "This Adder works like a desk top calculator. You may add," + ;
      " subtract, multiply, or divide. "           + hb_eol() + hb_eol() + ;
      "When adding or subtracting, the first entry is entered "  + ;
      "into the accumulator and each sucessive entry is "        + ;
      "subtotaled. When you press <ENTER> the SubTotal is also " + ;
      "shown on the tape. The second time you press <ENTER> the " + ;
      "adder is Totaled. When multiplying or dividing the "      + ;
      "<ENTER> is a Total the first time pressed." + hb_eol() + hb_eol() + ;
      "Hot Keys:"                                           + hb_eol() + ;
      "         <D>ecimals - change # of decimals"          + hb_eol() + ;
      "         <M>ove     - the Adder from right to left"  + hb_eol() + ;
      "         <T>ape     - turn Tape Display On or Off"   + hb_eol() + ;
      "         <S>croll   - the tape display"              + hb_eol() + hb_eol() + ;
      "         <DEL> ---+-- 1st Clear entry"               + hb_eol() + ;
      "                  +-- 2nd Clear ADDER"               + hb_eol() + ;
      "         <ESC>      - Quit"                          + hb_eol() + ;
      "         <F10>      - return a <TOTAL> to the active get"

   _ftPushMessage( cMess, .T., "ADDER HELP", "press any key to continue...", ;
      "QUIET" )

   RETURN

// Clear entry / Clear Adder

STATIC PROCEDURE _ftClearAdder( aAdder )

   _ftEraseTotSubTot( aAdder )
   lDecSet   := .F.
   nDecDigit := 0
   IF lClAdder                           // If it has alredy been pressed once
      nTotal    := 0                      // then we are clearing the total
      nSavTotal := 0
      _ftUpdateTrans( aAdder, .F., NIL )
      lClAdder  := .F.
      _ftDispTotal( aAdder )
   ELSE
      nNumTotal := 0                      // Just clearing the last entry
      lClAdder  := .T.
      _ftDispSubTot( aAdder )
   ENDIF

   RETURN

// Update transactions array

STATIC PROCEDURE _ftUpdateTrans( aAdder, lTypeTotal, nAmount )

   LOCAL lUseTotal := ( nAmount == NIL )

   __defaultNIL( @nAmount, 0 )

   IF lClAdder                     // Clear the adder (they pressed <DEL> twice
      AAdd( aTrans, Str( 0, 22, nMaxDeci ) + " C" )
      IF lTape                            // If there is a tape Show Clear
         _ftDisplayTape( aAdder )
      ENDIF
      RETURN
   ENDIF

   IF lTypeTotal                         // If lTypeTotal == .T. Update from total
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )
      aTrans[ Len( aTrans ) ] := _ftStuffComma( aTrans[ Len( aTrans ) ], .T. ) + " *" + ;
         iif( lAddError, "ER", "" )

   ELSE                            // If lTypeTotal=.F. Update from nNumTotal
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )

      aTrans[ Len( aTrans ) ] := _ftStuffComma( aTrans[ Len( aTrans ) ], .T. ) + ;
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
   hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "          " )
   _ftSetWinColor( W_CURR, W_PROMPT )

   RETURN

// Adder Rounding function

STATIC FUNCTION _ftRoundIt( nNumber, nPlaces )

   __defaultNIL( @nPlaces, 0 )

   RETURN iif( nNumber < 0.0, -1.0, 1.0 ) * ;
      Int( Abs( nNumber ) * 10 ^ nPlaces + 0.50 + 10 ^ -12 ) / 10 ^ nPlaces

// Check divide by zero not allowed

STATIC FUNCTION _ftDivide( aAdder, nNumerator, nDenominator )

   IF nDenominator == 0.0
      lDivError := .T.
      RETU 0
   ELSE
      lDivError := .F.
   ENDIF

   RETURN nNumerator / nDenominator

// Validate the number of decimals

STATIC FUNCTION _ftValDeci( oGet )

   LOCAL lRtnValue := .T.

   IF oGet:VarGet() > 8
      _ftError( "no more than 8 decimal places please!" )
      lRtnValue := .F.
   ENDIF

   RETURN lRtnValue

// Display the Tape

STATIC PROCEDURE _ftDisplayTape( aAdder, nKey )

   LOCAL nDispTape, nTopTape := 1

   IF ( nKey == hb_keyCode( "T" ) .OR. nKey == hb_keyCode( "t" ) ) .AND. lTape  // Stop displaying tape
      lTape := .F.
      RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
      RETURN
   ENDIF
   IF lTape                              // Are we in the display mode
      SetColor( "N/W" )
      hb_Scroll( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, 1 )
      IF Len( aTrans ) > 0               // Any transactions been entered yet?
         hb_DispOutAt( 20 + nTopOS, 7 + nTapeSpace, aTrans[ Len( aTrans ) ] )
      ENDIF
      _ftSetWinColor( W_CURR, W_PROMPT )
   ELSE                                  // Start displaying tape
      lTape := .T.
      SetColor( "N/W" )
      cTapeScr := SaveScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace )
      hb_Shadow( 4 + nTopOS, 6 + nTapeSpace, 21 + nTopOS, 33 + nTapeSpace )
      hb_DispBox( 4 + nTopOS, 6 + nTapeSpace, 21 + nTopOS, 33 + nTapeSpace, FT_B_SINGLE, "R+/W" )
      SetColor( "GR+/W" )
      hb_DispOutAt( 4 + nTopOS, 17 + nTapeSpace, " TAPE " )
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
// NOTE: Save any keys in the buffer... for FAST typists <g>.

STATIC PROCEDURE _ftPushKeys()

   LOCAL nKey

   DO WHILE ( nKey := Inkey() ) != 0
      AAdd( t_aKeys, nKey )
   ENDDO

   RETURN

// Restore the keyboard with any keystrokes that were saved with _ftPushKeys

STATIC PROCEDURE _ftPopKeys()

   IF ! Empty( t_aKeys )
      hb_keyPut( t_aKeys )
   ENDIF
   t_aKeys := {}

   RETURN

// Display a message on the screen in a window
// See Also: _ftPopMessage()

STATIC PROCEDURE _ftPushMessage( cMessage, lWait, cTitle, cBotTitle, xQuiet, nTop )

   LOCAL nMessLen, nNumRows, nWide, nLeft, nBottom, nRight, cOldDevic
   LOCAL lOldPrint
   LOCAL cOldColor   := SetColor()
   LOCAL nOldLastkey := LastKey()
   LOCAL nOldRow     := Row()
   LOCAL nOldCol     := Col()
   LOCAL nOldCurs    := SetCursor( SC_NONE )
   LOCAL nWinColor   := W_CURR

   cOldDevic := Set( _SET_DEVICE, "SCREEN" )
   lOldPrint := Set( _SET_PRINTER, .F. )
   nMessLen  := Len( cMessage )
   nWide     := iif( nMessLen > 72, 72, iif( nMessLen < 12, 12, nMessLen ) )
   nNumRows  := MLCount( cMessage, nWide )

   // If they didn't say what the top row is, Center it on the screen
   __defaultNIL( @nTop, Int( ( MaxRow() - nNumRows ) / 2 ) )

   nBottom   := nTop + nNumRows + 2
   nLeft     := Int( ( MaxCol() - nWide ) / 2 ) - 3
   nRight    := nLeft + nWide + 4

   __defaultNIL( @lWait, .F. )

   _ftPushWin( nTop, nLeft, nBottom, nRight, cTitle, cBotTitle, nWinColor )
   _ftDispMessage( cMessage, nTop + 1, nLeft + 2, nBottom - 1, nRight - 2 )

   IF xQuiet == NIL
      Tone( 800, 1 )
   ENDIF
   IF lWait
      _ftInkey( 0, "nKey" )
      _ftPopMessage()
   ENDIF

   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   Set( _SET_DEVICE, cOldDevic )
   Set( _SET_PRINTER, lOldPrint )
   _ftSetLastKey( nOldLastKey )

   RETURN

// Pop off the Message Box
// See Also: _ftPushMessage()

STATIC PROCEDURE _ftPopMessage()

   _ftPopWin()

   RETURN

// Push a Question Box on the Screen
// NOTE: This function will work for all Data Types

STATIC FUNCTION _ftQuest( cMessage, xVarVal, cPict, bValid, lNoESC, nWinColor, nTop )

   LOCAL nOldRow, nOldCol, cOldColor, nMessLen, nWide, nNumRows, nBottom, nLeft
   LOCAL nRight, oNewGet, nNumMessRow, nLenLastRow, lGetOnNextLine, nOldCurs
   LOCAL cVarType := ValType( xVarVal )
   LOCAL nVarLen  := ;
      iif( cVarType == "C", Len( xVarVal ), ;
      iif( cVarType == "D", 10, ;
      iif( cVarType == "L", 1, ;
      iif( cVarType == "N", iif( cPict == NIL, 9, Len( cPict ) ), 0 ) ) ) )
   LOCAL nOldLastKey := LastKey()
   LOCAL cOldDevice  := Set( _SET_DEVICE, "SCREEN" )
   LOCAL lOldPrint   := Set( _SET_PRINTER, .F. )

   nOldRow   := Row()
   nOldCol   := Col()
   nOldCurs  := SetCursor( SC_NONE )
   cOldColor := SetColor()

   __defaultNIL( @lNoESC, .F. )

   nMessLen  := Len( cMessage ) + nVarLen + 1
   nWide     := iif( nMessLen > 66, 66, iif( nMessLen < 12, 12, nMessLen ) )

   nNumMessRow    := MLCount( cMessage, nWide )
   nLenLastRow    := Len( RTrim( MemoLine( cMessage, nWide, nNumMessRow ) ) )
   lGetOnNextLine := ( nLenLastRow + nVarLen ) > nWide
   nNumRows       := nNumMessRow + iif( lGetOnNextLine, 1, 0 )

   // Center it in the screen
   nTop        := iif( nTop == NIL, Int( ( MaxRow() - nNumRows ) / 2 ), nTop )
   nBottom     := nTop + nNumRows + 1
   nLeft       := Int( ( MaxCol() - nWide ) / 2 ) - 4
   nRight      := nLeft + nWide + 4

   _ftPushWin( nTop, nLeft, nBottom, nRight, "QUESTION ?", iif( HB_ISSTRING( xVarVal ) ;
      .AND. nVarLen > nWide, /* LOW-ASCII "←" */ Chr( 27 ) + " scroll " + Chr( 26 ) /* LOW-ASCII "→" */, NIL ), nWinColor )
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

   IF cPict != NIL                       // Use the picture they passed
      oNewGet:Picture := cPict
   ELSE                                  // Else setup default pictures
      DO CASE
      CASE HB_ISDATE( xVarVal )
         oNewGet:Picture := "9999-99-99"
      CASE HB_ISLOGICAL( xVarVal )
         oNewGet:Picture := "Y"
      CASE HB_ISNUMERIC( xVarVal )
         oNewGet:Picture := "999999.99"  // Guess that they are inputting dollars
      ENDCASE
   ENDIF

   oNewGet:PostBlock := iif( bValid == NIL, NIL, bValid )

   oNewGet:Display()

   SetCursor( SC_NORMAL )
   DO WHILE .T.                          // Loop so we can check for <ESC>
      // without reissuing the gets
      ReadModal( { oNewGet } )
      IF LastKey() == K_ESC .AND. lNoESC  // They pressed <ESC>
         _ftError( "you cannot Abort! Please enter an answer." )
      ELSE
         EXIT
      ENDIF

   ENDDO

   _ftPopWin()

   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   Set( _SET_DEVICE, cOldDevice )
   Set( _SET_PRINTER, lOldPrint )
   _ftSetLastKey( nOldLastKey )

   RETURN xVarVal

// User function for AChoice() when scrolling tape

STATIC FUNCTION _ftAdderTapeUDF( mode, cur_elem, rel_pos, /* @ */ lAC_exit_ok )

   LOCAL nKey, nRtnVal

   HB_SYMBOL_UNUSED( cur_elem )
   HB_SYMBOL_UNUSED( rel_pos )

   IF mode == AC_EXCEPT
      nKey := LastKey()
      DO CASE
      CASE nKey == K_CTRL_PGDN
         nRtnVal := AC_CONT
      CASE nKey == K_ESC
         hb_keyPut( { K_CTRL_PGDN, K_ENTER } )  // Go to last item
         lAC_exit_ok := .T.
         nRtnVal := AC_CONT
      CASE lAC_exit_ok
         nRtnVal := AC_ABORT
         lAC_exit_ok := .F.
      OTHERWISE
         nRtnVal := AC_CONT
      ENDCASE
   ELSE
      nRtnVal := AC_CONT
   ENDIF

   RETURN nRtnVal

// Display an ERROR message in a window

STATIC PROCEDURE _ftError( cMessage, xDontReset )

   LOCAL nOldRow, nOldCol, nOldCurs, nTop, nLeft, nBot, nRight, cOldColor
   LOCAL nOldLastKey, cErrorScr, nMessLen, nWide, nNumRows
   LOCAL cOldDevic, lOldPrint
   LOCAL lResetLKey := ( xDontReset == NIL )

   nOldLastKey := LastKey()
   nOldRow  := Row()
   nOldCol  := Col()
   nOldCurs := SetCursor( SC_NONE )
   cOldColor := _ftSetSCRColor( STD_ERROR )
   cOldDevic := Set( _SET_DEVICE, "SCREEN" )
   lOldPrint := Set( _SET_PRINTER, .F. )
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
   hb_DispBox( nTop, nLeft, nBot, nRight, FT_B_SINGLE )
   hb_DispOutAt( nTop, nLeft + Int( nWide / 2 ) - 1, " ERROR " )
   hb_DispOutAt( nBot - 1, nLeft + Int( nWide - 28 ) / 2 + 3, "Press any key to continue..." )
   _ftDispMessage( cMessage, nTop + 1, nLeft + 3, nBot - 2, nRight - 3 )
   Tone( 70, 5 )
   _ftInkey( 0, "nKey" )
   RestScreen( nTop, nLeft, nBot + 1, nRight + 2, cErrorScr )
   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )

   IF lResetLKey
      _ftSetLastKey( nOldLastKey )
   ENDIF

   Set( _SET_DEVICE, cOldDevic )
   Set( _SET_PRINTER, lOldPrint )

   RETURN

// Stuff a Comma in a string

STATIC FUNCTION _ftStuffComma( cStrToStuff, lTrimStuffedStr )

   LOCAL nDecPosit, x

   __defaultNIL( @lTrimStuffedStr, .F. )

   IF !( "." $ cStrToStuff )
      cStrToStuff := _ftPosIns( cStrToStuff, ".", iif( "C" $ cStrToStuff .OR. ;
         "E" $ cStrToStuff .OR. "+" $ cStrToStuff .OR. "-" $ cStrToStuff ;
         .OR. "X" $ cStrToStuff .OR. "*" $ cStrToStuff .OR. ;
         Chr( 4 ) /* LOW-ASCII "♦" */ $ cStrToStuff .OR. ;
         "/" $ cStrToStuff .OR. "=" $ cStrToStuff, ;
         Len( cStrToStuff ) - 1, Len( cStrToStuff ) + 1 ) )

      IF cStrToStuff == " " .OR. cStrToStuff == "0"
         cStrToStuff := SubStr( cStrToStuff, 2 )
      ENDIF

   ENDIF
   nDecPosit := At( ".", cStrToStuff )

   IF Len( Left( LTrim( _ftCharRem( "-", cStrToStuff ) ), ;
      At( ".", LTrim( _ftCharRem( "-", cStrToStuff ) ) ) - 1 ) ) > 3
      IF lTrimStuffedStr    // Do we trim the number each time we insert a comma
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff, " " ) STEP -4
            cStrToStuff := SubStr( _ftPosIns( cStrToStuff, ",", x ), 2 )
         NEXT
      ELSE
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff, " " ) STEP -3
            cStrToStuff := _ftPosIns( cStrToStuff, ",", x )
         NEXT
      ENDIF
   ENDIF

   RETURN cStrToStuff

// Set the standard screen colors to the color requested.
// See Also: _ftSetWinColor()

STATIC FUNCTION _ftSetSCRColor( nStd, nEnh, nBord, nBack, nUnsel )

   IF Empty( t_aWinColor )
      _ftInitColors()
   ENDIF

   __defaultNIL( @nStd, 8 )
   __defaultNIL( @nEnh, 8 )
   __defaultNIL( @nBord, 8 )
   __defaultNIL( @nBack, 8 )
   __defaultNIL( @nUnsel, nEnh )

   RETURN SetColor( ;
      t_aStdColor[ nStd ] + "," + ;
      t_aStdColor[ nEnh ] + "," + ;
      t_aStdColor[ nBord ] + "," + ;
      t_aStdColor[ nBack ] + "," + ;
      t_aStdColor[ nUnsel ] )

// NOTE: Push a new window on the screen in the position t,l,b,r
//       and if cTitle is not NIL print the title for the window
//       in centered in the top line of the box. Similarly do
//       the same for cBotTitle. If nWinColor==NIL get the next
//       window color and use it for all the colors. If
//       cTypeBord==NIL use the single line border, else use the
//       one they requested. Push the window coordinates, the
//       color number, the SaveScreen() value, and whether they
//       picked the window color they wanted to use. If
//       lAutoWindow=.F. then the window color was incremented
//       and we will will restore the color number when we pop
//       the window off.
//          nWinColor DEFAULT == _ftNextWinColor()

STATIC PROCEDURE _ftPushWin( t, l, b, r, cTitle, cBotTitle, nWinColor )

   LOCAL lAutoWindow := ( nWinColor == NIL )

   nWinColor := iif( nWinColor == NIL, _ftNextWinColor(), nWinColor )
   AAdd( t_aWindow, { t, l, b, r, nWinColor, SaveScreen( t, l, b + 1, r + 2 ), lAutoWindow } )
   hb_Shadow( t, l, b, r )
   _ftSetWinColor( nWinColor, W_BORDER )
   hb_DispBox( t, l, b, r, FT_B_SINGLE )

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

// Pop a Window off the screen
// NOTE: Pop the currently active window off the screen by restoring
//       it from the t_aWindow Array and if they pushed a new window
//       automatically selecting the color we will roll back the
//       current window setting using _ftLastWinColor() and reset
//       the color to the color setting when window was pushed.

STATIC PROCEDURE _ftPopWin()

   LOCAL nNumWindow := Len( t_aWindow )

   RestScreen( t_aWindow[ nNumWindow, 1 ], t_aWindow[ nNumWindow, 2 ], ;
      t_aWindow[ nNumWindow, 3 ] + 1, t_aWindow[ nNumWindow, 4 ] + 2, ;
      t_aWindow[ nNumWindow, 6 ] )

   IF t_aWindow[ nNumWindow, 7 ]
      _ftLastWinColor()
   ENDIF

   ASize( t_aWindow, Len( t_aWindow ) - 1 )

   IF ! Empty( t_aWindow )
      _ftSetWinColor( W_CURR, W_SCREEN, W_VARIAB )
   ELSE
      _ftSetSCRColor( STD_SCREEN, STD_VARIABLE )
   ENDIF

   RETURN

// Set the Color to the Window Colors requested
// See Also: _ftSetSCRColor()
// NOTE: If the window number is not passed use the currently
//       active window number nWinColor.

STATIC FUNCTION _ftSetWinColor( nWin, nStd, nEnh, nBord, nBack, nUnsel )

   __defaultNIL( @nWin, t_nWinColor )
   __defaultNIL( @nStd, 7 )
   __defaultNIL( @nEnh, 7 )
   __defaultNIL( @nBord, 7 )
   __defaultNIL( @nBack, 7 )
   __defaultNIL( @nUnsel, nEnh )

   RETURN SetColor( ;
      t_aWinColor[ nStd, nWin ] + "," + ;
      t_aWinColor[ nEnh, nWin ] + "," + ;
      t_aWinColor[ nBord, nWin ] + "," + ;
      t_aWinColor[ nBack, nWin ] + "," + ;
      t_aWinColor[ nUnsel, nWin ] )

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

// Print the top or bottom titles on the border of the currently
// active window.

STATIC PROCEDURE _ftWinTitle( cTheTitle, cTopOrBot )

   LOCAL nCurWin  := Len( t_aWindow )
   LOCAL nLenTitle := Len( cTheTitle )

   hb_DispOutAt( t_aWindow[ nCurWin, iif( cTopOrBot == NIL, 1, 3 ) ], ( t_aWindow[ nCurWin, 4 ] - ;
      t_aWindow[ nCurWin, 2 ] - nLenTitle ) / 2 + t_aWindow[ nCurWin, 2 ], " " + cTheTitle + " " )

   RETURN

// Initilize the colors for the Adder

STATIC PROCEDURE _ftInitColors()

   t_aWinColor := { ;
      { "GR+/BG", "GR+/G", "B+/RB",  "G+/R" }, ;
      { "R+/N",   "W+/RB", "W+/BG",  "GR+/B" }, ;
      { "GR+/N",  "GR+/N", "GR+/N",  "GR+/N" }, ;
      {  "B/BG",  "BG+/G", "W+/RB",  "BG+/R" }, ;
      { "W+/BG",  "W+/G",  "GR+/RB", "W+/R" }, ;
      { "GR+/B",  "GR+/R", "R+/B",   "W+/BG" }, ;
      {  "N/N",   "N/N",   "N/N",    "N/N" } }

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

// Replace the Character at nPosit in cString with cChar

STATIC FUNCTION _ftPosRepl( cString, cChar, nPosit )
   RETURN StrTran( cString, "9", cChar, nPosit, 1 ) + ""

// Removes all occurances of cChar from cString.

STATIC FUNCTION _ftCharRem( cChar, cString )
   RETURN StrTran( cString, cChar )

// Returns the number of spaces on the Left side of the String

STATIC FUNCTION _ftCountLeft( cString )
   RETURN Len( cString ) - Len( LTrim( cString ) )

// Insert the Character cChar in cString at position nPosit

STATIC FUNCTION _ftPosIns( cString, cChar, nPosit )
   RETURN Left( cString, nPosit - 1 ) + cChar + SubStr( cString, nPosit )

STATIC FUNCTION _ftInkey( nSecs, cVar )

   LOCAL nVar

   DO WHILE .T.
      IF SetKey( nVar := Inkey( nSecs ) ) != NIL
         Eval( SetKey( nVar ), ProcName(), ProcLine(), cVar )
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN nVar

STATIC PROCEDURE _ftDispMessage( cMessage, nT, nL, nB, nR )

   _ftPushKeys()
   hb_keyPut( { K_CTRL_PGDN, K_CTRL_W } )
   MemoEdit( cMessage, nT, nL, nB, nR, .F., NIL, nR - nL + 1 )
   _ftPopKeys()

   RETURN
