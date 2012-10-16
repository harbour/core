/*
 * $Id$
 */

/*
 * Author....: Keith A. Wire
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

#define nTotTran Len( aTrans )

#command DISPMESSAGE <mess>,<t>,<l>,<b>,<r> => ;
      _ftPushKeys(); hb_keyPut( { K_CTRL_PGDN, K_CTRL_W } ) ;;
      MemoEdit( <mess>, <t>, <l>, <b>, <r>, .F., NIL, ( <r> ) - ( <l> ) + 1 ) ;;
      _ftPopKeys()

/* This INKEY UDC was posted by Don Caton on NanForum... Thanks Don <g> */
#command FT_INKEY [ <secs> ] TO <var>                                     ;
      =>                                                                  ;
      WHILE .T.                                                          ;;
         <var> := Inkey( [ <secs> ] )                                    ;;
         IF SetKey( <var> ) != NIL                                       ;;
            Eval( SetKey( <var> ), ProcName(), ProcLine(), #<var> )      ;;
         ELSE                                                            ;;
            EXIT                                                         ;;
         END                                                             ;;
      END

// Instead of using STATIC variables for these I'm using a LOCAL array
//   and passing aAdder[] all over the place.... Don't let this confuse
//   you. I wrote the Adder using the variable names & now let the
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

/*+- Function ---------------------------------------------------------------+
  |         Name: FT_Adder()            Docs: Keith A. Wire                  |
  |  Description: Pop Up Adder / Calculator with Tape Display                |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:18:40am            Time updated:  11:18:40am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  |        Notes: To make FT_Adder() pop up from any wait state in your      |
  |             : application just insert the line:                          |
  |             :   SET KEY K_ALT_A  TO FT_Adder                             |
  |             : at the top of your application                             |
  +--------------------------------------------------------------------------+
*/

FUNCTION FT_Adder()

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
      RETURN NIL
   ELSE
      t_lAdderOpen := .T.
   ENDIF

   aTrans       := { "                  0.00 C " }
   nOldDecim    := Set( _SET_DECIMALS, 9 )
   cTotPict     := "999999999999999.99"
   cTapeScr     := ""
   nTotal       := nNumTotal := nSavTotal := nDecDigit := 0
   lDone        := .F.                   // Loop flag
   nKey         := 0
   nMaxDeci     := 2                     // Initial # of decimals
   nSavSubTot   := 0
   lNewNum      := .F.
   nAddMode     := 1                     // Start in ADD mode
   lMultDiv     := .F.                   // Start in ADD mode
   lClAdder     := .F.                   // Clear adder flag
   lDecSet      := .F.                   // Decimal ? - keyboard routine
   lSubRtn      := lTotalOk := lTape := lAddError := lDivError := .F.

   nTopOS       := Int( ( MaxRow() - 24 ) / 2 )  // Using the TopOffSet & LeftOffSet
   nLeftOS      := Int( ( MaxCol() - 79 ) / 2 )  // the Adder will always be centered
   nAddSpace    := iif( lShowRight, 40, 0 ) + nLeftOS
   nTapeSpace   := iif( lShowRight, 0, 40 ) + nLeftOS

   // Set Up the STATIC variables
   t_aKeys      := {}
   t_aWindow    := {}
   t_nWinColor  := 0

   _ftAddScreen( aAdder )

   // Set the decimals to 2 & display a cleared adder
   _ftChangeDec( aAdder, 2 )
   hb_DispOutAt( 4 + nTopOS, 7 + nAddSpace, Transform( nTotal, cTotPict ) )

   DO WHILE ! lDone                      // Input key & test loop
      FT_INKEY 0 TO nKey
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
         IF !Empty( cMoveTotSubTot )
            _ftSetWinColor( W_CURR, W_SCREEN )
            hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, iif( cMoveTotSubTot == "T", "   <TOTAL>", ;
               "<SUBTOTAL>" ) )
            _ftSetWinColor( W_CURR, W_PROMPT )
         ENDIF
      CASE ( nKey == hb_keyCode( "S" ) .OR. nKey == hb_keyCode( "s" ) ) .AND. lTape  // <S> Scroll tape display
         IF nTotTran > 16                  // We need to scroll
            SetColor( "GR+/W" )
            hb_DispOutAt( 21 + nTopOS, 8 + nTapeSpace, " " + /* LOW-ASCII "↑↓" */ Chr( 24 ) + Chr( 25 ) + "-SCROLL  <ESC>-QUIT " )
            SetColor( "N/W,W+/N" )
            lAC_exit_ok := .F.
            AChoice( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, aTrans, .T., ;
               {| nMode, cur_elem, rel_pos | _ftAdderTapeUDF( nMode, cur_elem, rel_pos, @lAC_exit_ok ) }, nTotTran, 20 )
            SetColor( "R+/W" )
            hb_DispBox( 21 + nTopOS, 8 + nTapeSpace, 21 + nTopOS, 30 + nTapeSpace, HB_B_SINGLE_UNI )
            _ftSetWinColor( W_CURR, W_PROMPT )
            CLEAR TYPEAHEAD
         ELSE
            _ftError( "there are " + iif( nTotTran > 0, "only " + ;
               LTrim( Str( nTotTran, 3, 0 ) ), "no" ) + ;
               " transactions entered so far." + ;
               " No need to scroll!" )
         ENDIF
      CASE nKey == K_DEL                // Delete - Clear adder
         _ftClearAdder( aAdder )
      CASE nKey == K_F1                 // <F1> Help
         _ftAddHelp()
      CASE nKey == K_F10                // <F10> Quit - Return total
         IF lTotalOk                     // Did they finish the calculation
            IF oGet != NIL .AND. oGet:TYPE == "N"
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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftAddScreen()        Docs: Keith A. Wire                  |
  |  Description: Display the Adder                                          |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:24:29am            Time updated:  11:24:29am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftAddScreen( aAdder )

   LOCAL nCol

   _ftPushWin( 2 + nTopOS, 2 + nAddSpace, 22 + nTopOS, 30 + nAddSpace, "   Adder   ", ;
      "<F-1> for Help", , FT_B_DOUBLE )
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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftChangeDec()        Docs: Keith A. Wire                  |
  |  Description: Change the decimal position in the display                 |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:25:17am            Time updated:  11:25:17am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nNumDec                                                    |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftChangeDec( aAdder, nNumDec )

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftDispTotal()        Docs: Keith A. Wire                  |
  |  Description: Display total number to Adder Window                       |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:25:58am            Time updated:  11:25:58am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftDispTotal( aAdder )

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftDispSubTot()       Docs: Keith A. Wire                  |
  |  Description: Display subtotal number                                    |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:26:31am            Time updated:  11:26:31am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftDispSubTot( aAdder )

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftProcessNumb()      Docs: Keith A. Wire                  |
  |  Description: Act on NUMBER key pressed                                  |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 11:38:34am            Time updated:  11:38:34am            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nKey                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftProcessNumb( aAdder, nKey )

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
            nDecDigit := ++nDecDigit
            nNumTotal := nNumTotal + nNum / ( 10 ** nDecDigit )
         ENDIF
      ELSE
         nNumTotal := nNumTotal * 10 + nNum
      ENDIF
   ENDIF

   _ftDispSubTot( aAdder )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftAddTotal()         Docs: Keith A. Wire                  |
  |  Description: Enter key - SUBTOTAL\TOTAL                                 |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:05:29pm            Time updated:  12:05:29pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftAddTotal( aAdder )

   _ftEraseTotSubTot( aAdder )
   lDecSet   := .F.
   nDecDigit :=  0
   lClAdder  := .F.                      // Reset the Clear flag
   IF lSubRtn                            // If this was the second time they
      IF !lMultDiv
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
         IF !lMultDiv
            lSubRtn := .T.                  // total key
         ENDIF
         IF nAddMode == 1                  // Add
            nTotal := nTotal + nNumTotal
         ELSEIF nAddMode == 2              // Subtract
            nTotal := nTotal - nNumTotal
         ELSEIF nAddMode == 3              // Multiply
            nTotal := nTotal * nNumTotal
         ELSEIF nAddMode == 4              // Divide
            nTotal := _ftDivide( aAdder, nTotal, nNumTotal )
            IF lDivError
               _ftError( "you can't divide by ZERO!" )
               lDivError := .F.
            ENDIF
         ENDIF
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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftAddSub()           Docs: Keith A. Wire                  |
  |  Description: Process + or - keypress                                    |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:06:13pm            Time updated:  12:06:13pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nKey                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftAddSub( aAdder, nKey )

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
   IF nKey == hb_keyCode( "+" )                 // Add
      nAddMode := 1
      IF !lNewNum                         // They pressed + again to add the same
         nNumTotal := nSavSubTot           // number without re-entering
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     := nTotal + nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal   // Save this number in case they just press + or -
      nNumTotal  := 0
   ELSEIF nKey == hb_keyCode( "-" )          // Subtract
      nAddMode := 2
      IF !lNewNum                         // They pressed + again to add the same
         nNumTotal := nSavSubTot           // number without re-entering
         lNewNum   := .T.
      ENDIF
      _ftUpdateTrans( aAdder, .F., nNumTotal )
      nTotal     := nTotal - nNumTotal
      lNewNum    := .F.
      nSavSubTot := nNumTotal   // Save this number in case they just press + or -
      nNumTotal  := 0
   ENDIF

   _ftDispTotal( aAdder )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftMultDiv()          Docs: Keith A. Wire                  |
  |  Description: Process * or / keypress                                    |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:06:43pm            Time updated:  12:06:43pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nKey                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftMultDiv( aAdder, nKey )

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
      IF nKey == hb_keyCode( "*" )            // Setup mode
         nAddMode := 3
         _ftUpdateTrans( aAdder, .F., nNumTotal )
      ELSEIF nKey == hb_keyCode( "/" )
         nAddMode := 4
         _ftUpdateTrans( aAdder, .F., nNumTotal )
      ENDIF
      nTotal    := nNumTotal
      nNumTotal := 0
   ELSE
      IF nKey == hb_keyCode( "*" )           // Multiply
         nAddMode  := 3
         _ftUpdateTrans( aAdder, .F., nNumTotal )
         nTotal    := nTotal * nNumTotal
         nNumTotal := 0
      ELSEIF nKey == hb_keyCode( "/" )       // Divide
         nAddMode := 4
         _ftUpdateTrans( aAdder, .F., nNumTotal )
         nTotal := _ftDivide( aAdder, nTotal, nNumTotal )
         IF lDivError
            _ftError( "you can't divide by ZERO!" )
            lDivError := .F.
         ENDIF
         nNumTotal := 0
      ENDIF
   ENDIF

   _ftDispTotal( aAdder )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftAddHelp            Docs: Keith A. Wire                  |
  |  Description: Help window                                                |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:07:07pm            Time updated:  12:07:07pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftAddHelp

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftClearAdder()       Docs: Keith A. Wire                  |
  |  Description: Clear entry / Clear Adder                                  |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:07:33pm            Time updated:  12:07:33pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftClearAdder( aAdder )

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftUpdateTrans()      Docs: Keith A. Wire                  |
  |  Description: Update transactions array                                  |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:07:55pm            Time updated:  12:07:55pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : lTypeTotal                                                 |
  |             : nAmount                                                    |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftUpdateTrans( aAdder, lTypeTotal, nAmount )

   LOCAL lUseTotal := ( nAmount == NIL )

   nAmount := iif( nAmount == NIL, 0, nAmount )
   IF lClAdder                     // Clear the adder (they pressed <DEL> twice
      AAdd( aTrans, Str( 0, 22, nMaxDeci ) + " C" )
      IF lTape                            // If there is a tape Show Clear
         _ftDisplayTape( aAdder )
      ENDIF
      RETU NIL
   ENDIF

   IF lTypeTotal                         // If lTypeTotal == .T. Update from total
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )
      aTrans[ nTotTran ] := _ftStuffComma( aTrans[ nTotTran ], .T. ) + " *" + ;
         iif( lAddError, "ER", "" )

   ELSE                            // If lTypeTotal=.F. Update from nNumTotal
      AAdd( aTrans, Str( iif( lUseTotal, nTotal, nAmount ), 22, nMaxDeci ) )

      aTrans[ nTotTran ] := _ftStuffComma( aTrans[ nTotTran ], .T. ) + ;
         iif( lSubRtn, " S", iif( nAddMode == 1, " +", iif( nAddMode == 2, " -", ;
         iif( lTotalOk, " =", iif( nAddMode == 3, " X", " /" ) ) ) ) ) + iif( lAddError, "ER", "" )

   ENDIF

   IF lTape
      _ftDisplayTape( aAdder )
   ENDIF

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftEraseTotSubTot()   Docs: Keith A. Wire                  |
  |  Description: Clear the <TOTAL> & <SUBTOTAL> from Adder                  |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:08:14pm            Time updated:  12:08:14pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftEraseTotSubTot( aAdder )

   _ftSetWinColor( W_CURR, W_SCREEN )
   hb_DispOutAt( 6 + nTopOS, 18 + nAddSpace, "          " )
   _ftSetWinColor( W_CURR, W_PROMPT )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftRoundIt()          Docs: Keith A. Wire                  |
  |  Description: Adder Rounding function                                    |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:09:00pm            Time updated:  12:09:00pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: nNumber                                                    |
  |             : nPlaces                                                    |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftRoundIt( nNumber, nPlaces )

   nPlaces := iif( nPlaces == NIL, 0, nPlaces )

   RETURN iif( nNumber < 0.0, - 1.0, 1.0 ) * ;
      Int( Abs( nNumber ) * 10 ^ nPlaces + 0.50 + 10 ^ - 12 ) / 10 ^ nPlaces

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftDivide()           Docs: Keith A. Wire                  |
  |  Description: Check divide by zero not allowed                           |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:10:41pm            Time updated:  12:10:41pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nNumerator                                                 |
  |             : nDenominator                                               |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftDivide( aAdder, nNumerator, nDenominator )

   IF nDenominator == 0.0
      lDivError := .T.
      RETU 0
   ELSE
      lDivError := .F.
   ENDIF

   RETURN nNumerator / nDenominator

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftValDeci()          Docs: Keith A. Wire                  |
  |  Description: Validate the number of decimals                            |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:10:56pm            Time updated:  12:10:56pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: oGet                                                       |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftValDeci( oGet )

   LOCAL lRtnValue := .T.

   IF oGet:VarGet() > 8
      _ftError( "no more than 8 decimal places please!" )
      lRtnValue := .F.
   ENDIF

   RETURN lRtnValue

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftDisplayTape()      Docs: Keith A. Wire                  |
  |  Description: Display the Tape                                           |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:11:28pm            Time updated:  12:11:28pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: aAdder                                                     |
  |             : nKey                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftDisplayTape( aAdder, nKey )

   LOCAL nDispTape, nTopTape := 1

   IF ( nKey == hb_keyCode( "T" ) .OR. nKey == hb_keyCode( "t" ) ) .AND. lTape  // Stop displaying tape
      lTape := .F.
      RestScreen( 4 + nTopOS, 6 + nTapeSpace, 22 + nTopOS, 35 + nTapeSpace, cTapeScr )
      RETURN NIL
   ENDIF
   IF lTape                              // Are we in the display mode
      SetColor( "N/W" )
      hb_Scroll( 5 + nTopOS, 7 + nTapeSpace, 20 + nTopOS, 32 + nTapeSpace, 1 )
      IF nTotTran > 0                       // Any transactions been entered yet?
         hb_DispOutAt( 20 + nTopOS, 7 + nTapeSpace, aTrans[ nTotTran ] )
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
      IF nTotTran > 15
         nTopTape := nTotTran - 15
      ENDIF
      FOR nDispTape := nTotTran TO nTopTape STEP -1
         hb_DispOutAt( 20 + nDispTape - nTotTran + nTopOS, 7 + nTapeSpace, aTrans[ nDispTape ] )
      NEXT
   ENDIF
   _ftSetWinColor( W_CURR, W_PROMPT )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftSetLastKey()       Docs: Keith A. Wire                  |
  |  Description: Sets the LASTKEY() value to value of nLastKey              |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:12:00pm            Time updated:  12:12:00pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: nLastKey                                                   |
  | Return Value: NIL                                                        |
  |        Notes: I use this in most of my Pop-Up routines to reset the      |
  |             : original value of LASTKEY() when quitting.                 |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftSetLastKey( nLastKey )

   _ftPushKeys()
   hb_keyPut( nLastKey )
   Inkey()
   _ftPopKeys()

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPushKeys           Docs: Keith A. Wire                  |
  |  Description: Push any keys in the Keyboard buffer on the array t_aKeys[]|
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:16:09pm            Time updated:  12:16:09pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  |        Notes: Save any keys in the buffer... for FAST typists <g>.       |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPushKeys

   DO WHILE NextKey() != 0
      AAdd( t_aKeys, Inkey() )
   ENDDO

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPopKeys            Docs: Keith A. Wire                  |
  |  Description: Restore the keyboard with any keystrokes that were saved   |
  |             :   with _ftPushKeys                                         |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:17:58pm            Time updated:  12:17:58pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPopKeys

   IF ! Empty( t_aKeys )
      hb_keyPut( t_aKeys )
   ENDIF
   t_aKeys := {}

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPushMessage()      Docs: Keith A. Wire                  |
  |  Description: Display a message on the screen in a window                |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:18:53pm            Time updated:  12:18:53pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cMessage                                                   |
  |             : lWait                                                      |
  |             : cTitle                                                     |
  |             : cBotTitle                                                  |
  |             : xQuiet                                                     |
  |             : nTop                                                       |
  | Return Value: NIL                                                        |
  |     See Also: _ftPopMessage                                              |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPushMessage( cMessage, lWait, cTitle, cBotTitle, xQuiet, nTop )

   LOCAL nMessLen, nNumRows, nWide, nLeft, nBottom, nRight, nKey, cOldDevic
   LOCAL lOldPrint
   LOCAL cOldColor   := SetColor()
   LOCAL nOldLastkey := LastKey()
   LOCAL nOldRow     := Row()
   LOCAL nOldCol     := Col()
   LOCAL nOldCurs    := SetCursor( SC_NONE )
   LOCAL nWinColor   := iif( nWinColor == NIL, W_CURR, nWinColor )

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
   lWait     := iif( lWait == NIL, .F., lWait )

   _ftPushWin( nTop, nLeft, nBottom, nRight, cTitle, cBotTitle, nWinColor )
   DISPMESSAGE cMessage, nTop + 1, nLeft + 2, nBottom - 1, nRight - 2

   IF xQuiet == NIL
      Tone( 800, 1 )
   ENDIF
   IF lWait
      FT_INKEY 0 TO nKey
      _ftPopMessage()
   ENDIF

   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )
   Set( _SET_DEVICE, cOldDevic )
   Set( _SET_PRINTER, lOldPrint )
   _ftSetLastKey( nOldLastKey )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPopMessage         Docs: Keith A. Wire                  |
  |  Description: Pop off the Message Box                                    |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:24:22pm            Time updated:  12:24:22pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  |     See Also: _ftPushMessage()                                           |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPopMessage

   _ftPopWin()

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftQuest()            Docs: Keith A. Wire                  |
  |  Description: Push a Question Box on the Screen                          |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:25:32pm            Time updated:  12:25:32pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cMessage                                                   |
  |             : xVarVal                                                    |
  |             : cPict                                                      |
  |             : bValid                                                     |
  |             : lNoESC                                                     |
  |             : nWinColor                                                  |
  |             : nTop                                                       |
  | Return Value: xVarVal                                                    |
  |        Notes: This function will work for all Data Types                 |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftQuest( cMessage, xVarVal, cPict, bValid, lNoESC, nWinColor, nTop )

   LOCAL nOldRow, nOldCol, cOldColor, nMessLen, nWide, nNumRows, nBottom, nLeft
   LOCAL nRight, oNewGet, nNumMessRow, nLenLastRow, lGetOnNextLine, nOldCurs
   LOCAL cVarType := ValType( xVarVal )
   LOCAL nVarLen  := iif( cVarType == "C", Len( xVarVal ),;
                     iif( cVarType == "D", 8, ;
                     iif( cVarType == "L", 1, ;
                     iif( cVarType == "N", iif( cPict == NIL, 9, Len( cPict ) ), 0 ) ) ) )
   LOCAL nOldLastKey := LastKey()
   LOCAL cOldDevice  := Set( _SET_DEVICE, "SCREEN" )
   LOCAL lOldPrint   := Set( _SET_PRINTER, .F. )

   nOldRow   := Row()
   nOldCol   := Col()
   nOldCurs  := SetCursor( SC_NONE )
   cOldColor := SetColor()
   lNoESC    := iif( lNoESC == NIL, .F., lNoESC )

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
   DISPMESSAGE cMessage, nTop + 1, nLeft + 2, nBottom - 1, nRight - 2

   oNewGet := GetNew( iif( lGetOnNextLine,Row() + 1,Row() ), ;
      iif( lGetOnNextLine, nLeft + 2, Col() + 1 ), ;
      {| x | iif( PCount() > 0, xVarVal := x, xVarVal ) }, ;
      "xVarVal" )

   // If the input line is character & wider than window SCROLL
   IF lGetOnNextLine .AND. HB_ISSTRING( xVarVal ) .AND. nVarLen > nWide
      oNewGet:Picture   := "@S" + LTrim( Str( nWide, 4, 0 ) ) + iif( cPict == NIL, "", " " + cPict )
   ENDIF

   IF cPict != NIL                       // Use the picture they passed
      oNewGet:Picture   := cPict
   ELSE                                  // Else setup default pictures
      IF HB_ISDATE( xVarVal )
         oNewGet:Picture   := "99/99/99"
      ELSEIF HB_ISLOGICAL( xVarVal )
         oNewGet:Picture   := "Y"
      ELSEIF HB_ISNUMERIC( xVarVal )
         oNewGet:Picture   := "999999.99"  // Guess that they are inputting dollars
      ENDIF
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
   Set( _SET_DEVICE,  cOldDevice )
   Set( _SET_PRINTER, lOldPrint )
   _ftSetLastKey( nOldLastKey )

   RETURN xVarVal

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftAdderTapeUDF()    Docs: Keith A. Wire                   |
  |  Description: User function for ACHOICE() when scrolling tape            |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:26:44pm            Time updated:  12:26:44pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: mode                                                       |
  |             : cur_elem                                                   |
  |             : rel_pos                                                    |
  | Return Value: nRtnVal                                                    |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftAdderTapeUDF( mode, cur_elem, rel_pos, /* @ */ lAC_exit_ok )

   LOCAL nKey, nRtnVal

   HB_SYMBOL_UNUSED( cur_elem )
   HB_SYMBOL_UNUSED( rel_pos )

   DO CASE
   CASE mode == AC_EXCEPT
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
   OTHERWISE
      nRtnVal := AC_CONT
   ENDCASE

   RETURN nRtnVal

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftError()            Docs: Keith A. Wire                  |
  |  Description: Display an ERROR message in a window                       |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:27:43pm            Time updated:  12:27:43pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cMessage                                                   |
  |             : xDontReset                                                 |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftError( cMessage, xDontReset )

   LOCAL nOldRow, nOldCol, nOldCurs, nTop, nLeft, nBot, nRight, cOldColor
   LOCAL nOldLastKey, cErrorScr, nMessLen, nWide, nNumRows, nKey
   LOCAL cOldDevic, lOldPrint
   LOCAL lResetLKey := iif( xDontReset == NIL, .T., .F. )

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
   DISPMESSAGE cMessage, nTop + 1, nLeft + 3, nBot - 2, nRight - 3
   Tone( 70, 5 )
   FT_INKEY 0 TO nKey
   RestScreen( nTop, nLeft, nBot + 1, nRight + 2, cErrorScr )
   SetCursor( nOldCurs )
   SetColor( cOldColor )
   SetPos( nOldRow, nOldCol )

   IF lResetLKey
      _ftSetLastKey( nOldLastKey )
   ENDIF

   Set( _SET_DEVICE, cOldDevic )
   Set( _SET_PRINTER, lOldPrint )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftStuffComma()       Docs: Keith A. Wire                  |
  |  Description: Stuff a Comma in a string                                  |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:28:19pm            Time updated:  12:28:19pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cStrToStuff                                                |
  |             : lTrimStuffedStr                                            |
  | Return Value: cStrToStuff                                                |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftStuffComma( cStrToStuff, lTrimStuffedStr )

   LOCAL nDecPosit, x

   lTrimStuffedStr := iif( lTrimStuffedStr == NIL, .F., lTrimStuffedStr )
   IF !( "." $ cStrToStuff )
      cStrToStuff := _ftPosIns( cStrToStuff, ".", iif( "C" $ cStrToStuff .OR. ;
         "E" $ cStrToStuff .OR. "+" $ cStrToStuff .OR. "-" $ cStrToStuff ;
         .OR. "X" $ cStrToStuff .OR. "*" $ cStrToStuff .OR. ;
         "" $ cStrToStuff .OR. "/" $ cStrToStuff .OR. "=" $ cStrToStuff, ;
         Len( cStrToStuff ) - 1, Len( cStrToStuff ) + 1 ) )

      IF cStrToStuff == " " .OR. cStrToStuff == "0"
         cStrToStuff := SubStr( cStrToStuff, 2 )
      ENDIF

   ENDIF
   nDecPosit := At( ".", cStrToStuff )

   IF Len( Left( LTrim(_ftCharRem("-",cStrToStuff ) ), ;
         At( ".", LTrim( _ftCharRem("-",cStrToStuff ) ) ) - 1 ) ) > 3
      IF lTrimStuffedStr    // Do we trim the number each time we insert a comma
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff, " " ) STEP -4
            cStrToStuff := SubStr( _ftPosIns( cStrToStuff,",",x ), 2 )
         NEXT
      ELSE
         FOR x := nDecPosit - 3 TO 2 + _ftCountLeft( cStrToStuff, " " ) STEP -3
            cStrToStuff := _ftPosIns( cStrToStuff, ",", x )
         NEXT
      ENDIF
   ENDIF

   RETURN cStrToStuff

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftSetSCRColor()      Docs: Keith A. Wire                  |
  |  Description: Set the standard screen colors to the color requested.     |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:28:48pm            Time updated:  12:28:48pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: nStd                                                       |
  |             : nEnh                                                       |
  |             : nBord                                                      |
  |             : nBack                                                      |
  |             : nUnsel                                                     |
  |     See Also: _ftSetWinColor()                                           |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftSetSCRColor( nStd, nEnh, nBord, nBack, nUnsel )

   IF Empty( t_aWinColor )
      _ftInitColors()
   ENDIF

   nStd  := iif( nStd   == NIL, 8, nStd )
   nEnh  := iif( nEnh   == NIL, 8, nEnh )
   nBord := iif( nBord  == NIL, 8, nBord )
   nBack := iif( nBack  == NIL, 8, nBack )
   nUnsel := iif( nUnsel == NIL, nEnh, nUnsel )

   RETURN SetColor( ;
      t_aStdColor[ nStd ] + "," + ;
      t_aStdColor[ nEnh ] + "," + ;
      t_aStdColor[ nBord ] + "," + ;
      t_aStdColor[ nBack ] + "," + ;
      t_aStdColor[ nUnsel ] )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPushWin()          Docs: Keith A. Wire                  |
  |  Description: Push a new window on the screen                            |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:34:38pm            Time updated:  12:34:38pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: t                                                          |
  |             : l                                                          |
  |             : b                                                          |
  |             : r                                                          |
  |             : cTitle                                                     |
  |             : cBotTitle                                                  |
  |             : nWinColor                                                  |
  | Return Value: NIL                                                        |
  |     See Also:                                                            |
  |        Notes: Push a new window on the screen in the position t,l,b,r    |
  |             :   and if cTitle is not NIL print the title for the window  |
  |             :   in centered in the top line of the box. Similarly do     |
  |             :   the same for cBotTitle. If nWinColor==NIL get the next   |
  |             :   window color and use it for all the colors. If           |
  |             :   cTypeBord==NIL use the single line border, else use the  |
  |             :   one they requested. Push the window coordinates, the     |
  |             :   color number, the SAVESCREEN() value, and whether they   |
  |             :   picked the window color they wanted to use. If           |
  |             :   lAutoWindow=.F. then the window color was incremented    |
  |             :   and we will will restore the color number when we pop    |
  |             :   the window off.                                          |
  |             :                                                            |
  |             :      nWinColor DEFAULT == _ftNextWinColor()                |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPushWin( t, l, b, r, cTitle, cBotTitle, nWinColor )

   LOCAL lAutoWindow := nWinColor == NIL

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPopWin             Docs: Keith A. Wire                  |
  |  Description: Pop a Window off the screen                                |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 12:52:34pm            Time updated:  12:52:34pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  |        Notes: Pop the currently active window off the screen by restoring|
  |             :   it from the t_aWindow Array and if they pushed a new window|
  |             :   automatically selecting the color we will roll back the  |
  |             :   current window setting using _ftLastWinColor() and reset |
  |             :   the color to the color setting when window was pushed.   |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPopWin

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

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftSetWinColor()      Docs: Keith A. Wire                  |
  |  Description: Set the Color to the Window Colors requested               |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:37:32pm            Time updated:  01:37:32pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: nWin                                                       |
  |             : nStd                                                       |
  |             : nEnh                                                       |
  |             : nBord                                                      |
  |             : nBack                                                      |
  |             : nUnsel                                                     |
  |     See Also: _ftSetSCRColor()                                           |
  |        Notes: If the window number is not passed use the currently active|
  |             :   window number nWinColor.                                 |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftSetWinColor( nWin, nStd, nEnh, nBord, nBack, nUnsel )

   nWin   := iif( nWin   == NIL, t_nWinColor, nWin )
   nStd   := iif( nStd   == NIL, 7, nStd )
   nEnh   := iif( nEnh   == NIL, 7, nEnh )
   nBord  := iif( nBord  == NIL, 7, nBord )
   nBack  := iif( nBack  == NIL, 7, nBack )
   nUnsel := iif( nUnsel == NIL, nEnh, nUnsel )

   RETURN SetColor( ;
      t_aWinColor[ nStd, nWin ] + "," + ;
      t_aWinColor[ nEnh, nWin ] + "," + ;
      t_aWinColor[ nBord, nWin ] + "," + ;
      t_aWinColor[ nBack, nWin ] + "," + ;
      t_aWinColor[ nUnsel, nWin ] )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftLastWinColor       Docs: Keith A. Wire                  |
  |  Description: Decrement the active window color number and return the    |
  |             :   current value                                            |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:49:19pm            Time updated:  01:49:19pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  |        Notes: If we are already on window #1 restart count by using # 4. |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftLastWinColor()

   RETURN t_nWinColor := iif( t_nWinColor == 1, 4, t_nWinColor - 1 )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftNextWinColor       Docs: Keith A. Wire                  |
  |  Description: Increment the active window color number and return the    |
  |             :   current value                                            |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:51:12pm            Time updated:  01:51:12pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  |        Notes: If we are already on window #4 restart count by using # 1. |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftNextWinColor()

   IF Empty( t_aWinColor )
      _ftInitColors()
   ENDIF

   RETURN t_nWinColor := ( iif( t_nWinColor < 4, t_nWinColor + 1, 1 ) )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftWinTitle()         Docs: Keith A. Wire                  |
  |  Description: Print the top or bottom titles on the border of the        |
  |             :   currently active window.                                 |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:52:29pm            Time updated:  01:52:29pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cTheTitle                                                  |
  |             : cTopOrBot                                                  |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftWinTitle( cTheTitle, cTopOrBot )

   LOCAL nCurWin  := Len( t_aWindow )
   LOCAL nLenTitle := Len( cTheTitle )

   hb_DispOutAt( t_aWindow[ nCurWin, iif( cTopOrBot == NIL, 1, 3 ) ], ( t_aWindow[ nCurWin, 4 ] - ;
      t_aWindow[ nCurWin, 2 ] - nLenTitle ) / 2 + t_aWindow[ nCurWin, 2 ], " " + cTheTitle + " " )

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftInitColors         Docs: Keith A. Wire                  |
  |  Description: Initilize the colors for the Adder                         |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 02:59:58pm            Time updated:  02:59:58pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: None                                                       |
  | Return Value: NIL                                                        |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftInitColors

   t_aWinColor := { ;
      { "GR+/BG","GR+/G", "B+/RB", "G+/R" }                        ,;
      { "R+/N",   "W+/RB", "W+/BG", "GR+/B" }                      ,;
      { "GR+/N", "GR+/N", "GR+/N", "GR+/N" }                       ,;
      {  "B/BG", "BG+/G", "W+/RB", "BG+/R" }                       ,;
      { "W+/BG", "W+/G", "GR+/RB", "W+/R" }                        ,;
      { "GR+/B", "GR+/R", "R+/B",  "W+/BG" }                       ,;
      {  "N/N",   "N/N",  "N/N",   "N/N" }   }

   t_aStdColor := { ;
      "BG+*/RB"                                                    ,;
      "GR+/R"                                                      ,;
      "GR+/N"                                                      ,;
      "W/B"                                                        ,;
      "GR+/N"                                                      ,;
      "GR+/GR"                                                     ,;
      { "W+/B",  "W/B", "G+/B", "R+/B"                             ,;
      "GR+/B", "BG+/B", "B+/B", "G+/B" }                           ,;
      "N/N" }

   RETURN NIL

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPosRepl()          Docs: Keith A. Wire                  |
  |  Description: Replace the Character at nPosit in cString with cChar      |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:44:21pm            Time updated:  01:44:21pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cString                                                    |
  |             : cChar                                                      |
  |             : nPosit                                                     |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPosRepl( cString, cChar, nPosit )

   RETURN StrTran( cString, "9", cChar, nPosit, 1 ) + ""

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftCharRem()          Docs: Keith A. Wire                  |
  |  Description: Removes all occurances of cChar from cString.              |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:45:41pm            Time updated:  01:45:41pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cChar                                                      |
  |             : cString                                                    |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftCharRem( cChar, cString )

   RETURN StrTran( cString, cChar )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftCountLeft()        Docs: Keith A. Wire                  |
  |  Description: Returns the number of spaces on the Left side of the String|
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:47:00pm            Time updated:  01:47:00pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cString                                                    |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftCountLeft( cString )

   RETURN Len( cString ) - Len( LTrim( cString ) )

/*+- Function ---------------------------------------------------------------+
  |         Name: _ftPosIns()           Docs: Keith A. Wire                  |
  |  Description: Insert the Character cChar in cString at position nPosit   |
  |       Author: Keith A. Wire                                              |
  | Date created: 10-03-93              Date updated:  10-03-93              |
  | Time created: 01:48:30pm            Time updated:  01:48:30pm            |
  |    Copyright: None - Public Domain                                       |
  +--------------------------------------------------------------------------+
  |    Arguments: cString                                                    |
  |             : cChar                                                      |
  |             : nPosit                                                     |
  +--------------------------------------------------------------------------+
*/

STATIC FUNCTION _ftPosIns( cString, cChar, nPosit )

   RETURN Left( cString, nPosit - 1 ) + cChar + SubStr( cString, nPosit )
