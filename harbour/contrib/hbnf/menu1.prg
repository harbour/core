/*
 * $Id$
 */

/*
 * Author....: Paul Ferrara
 * CIS ID....: 76702,556
 *
 * This is an original work by Paul Ferrara and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:40   GLENN
 * Nanforum Toolkit
 *
 */

/*
     For the sample program:

     Compile with "/n /dFT_TEST" SWITCHES AND LINK.

     PASS "MONO" OR "MONO" AS A COMMAND LINE PARAMETER TO FORCE MONO MODE.

     PASS "NOSNOW" OR "NOSNOW" AS A COMMAND LINE PARAMETER ON A CGA.

     PASS "VGA" OR "VGA" AS A COMMAND LINE PARAMETER FOR 50-LINE MODE.
 */

#include "achoice.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define DISABLE     0
#define ENABLE      1

THREAD STATIC t_aChoices := {}
THREAD STATIC t_aValidKeys := {}
THREAD STATIC t_nHPos
THREAD STATIC t_nVPos
THREAD STATIC t_nMaxRow
THREAD STATIC t_nMaxCol

// BEGINNING OF DEMO PROGRAM
#ifdef FT_TEST
// DUMMY PROCEDURE NAME SO "CCMDLINE" WILL BE LOCAL

PROCEDURE Main( cCmdLine )

   LOCAL sDosScrn, nDosRow, nDosCol, lColor

   // my approach to color variables
   // see colorchg.arc on NANFORUM
   LOCAL cNormN
   LOCAL cWindN
   LOCAL cErrH
   LOCAL cErrN

   // options on menu bar
   LOCAL aColors
   LOCAL aBar     := { " ENTER/EDIT ", " REPORTS ", " DISPLAY ", " MAINTENANCE ", " QUIT " }
   LOCAL aOptions[ Len( aBar ) ]
   AEval( aBar, {| x, i | HB_SYMBOL_UNUSED( x ), aOptions[ i ] := { {}, {}, {} } } )

   cCmdLine := iif( cCmdLine == NIL, "", cCmdLine )

   lColor := iif( "MONO" $ Upper( cCmdLine ), .F. , IsColor() )

   // Border, Box, Bar, Current, Unselected
   aColors := iif( lColor, { "W+/G", "N/G", "N/G", "N/W", "N+/G" }, ;
      { "W+/N", "W+/N", "W/N", "N/W", "W/N" } )

   FT_FILL( aOptions[ 1 ], "A. Execute A Dummy Procedure"        , {|| fubar() }, .T. )
   FT_FILL( aOptions[ 1 ], "B. Enter Daily Charge/Credit Slips"  , {|| .T. }, .T. )
   FT_FILL( aOptions[ 1 ], "C. Enter Payments On Accounts"       , {|| .T. }, .F. )
   FT_FILL( aOptions[ 1 ], "D. Edit Daily Transactions"          , {|| .T. }, .T. )
   FT_FILL( aOptions[ 1 ], "E. Enter/Update Member File"         , {|| .T. }, .T. )
   FT_FILL( aOptions[ 1 ], "F. Update Code File"                 , {|| .T. }, .F. )
   FT_FILL( aOptions[ 1 ], "G. Add/Update Auto Charge File"      , {|| .T. }, .T. )
   FT_FILL( aOptions[ 1 ], "H. Post All Transactions To A/R File", {|| .T. }, .T. )
   FT_FILL( aOptions[ 1 ], "I. Increment Next Posting Date"      , {|| .T. }, .T. )

   FT_FILL( aOptions[ 2 ], "A. Print Member List"                , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "B. Print Active Auto Charges"        , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "C. Print Edit List"                  , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "D. Print Pro-Usage Report"           , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "E. Print A/R Transaction Report"     , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "F. Aging Report Preparation"         , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "G. Add Interest Charges"             , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "H. Print Aging Report"               , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "I. Print Monthly Statements"         , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "J. Print Mailing Labels"             , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "K. Print Transaction Totals"         , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "L. Print Transaction Codes File"     , {|| .T. }, .T. )
   FT_FILL( aOptions[ 2 ], "M. Print No-Activity List"           , {|| .T. }, .T. )

   FT_FILL( aOptions[ 3 ], "A. Transaction Totals Display"       , {|| .T. }, .T. )
   FT_FILL( aOptions[ 3 ], "B. Display Invoice Totals"           , {|| .T. }, .T. )
   FT_FILL( aOptions[ 3 ], "C. Accounts Receivable Display"      , {|| .T. }, .T. )

   FT_FILL( aOptions[ 4 ], "A. Backup Database Files"            , {|| .T. }, .T. )
   FT_FILL( aOptions[ 4 ], "B. Reindex Database Files"           , {|| .T. }, .T. )
   FT_FILL( aOptions[ 4 ], "C. Set System Parameters"            , {|| .T. }, .T. )
   FT_FILL( aOptions[ 4 ], "D. This EXITs Too"                   , {|| .F. }, .T. )

   FT_FILL( aOptions[ 5 ], "A. Does Nothing"                     , {|| .T. }, .T. )
   FT_FILL( aOptions[ 5 ], "B. Exit To DOS"                      , {|| .F. }, .T. )

   // main routine starts here
   SET SCOREBOARD OFF

   cNormN := iif( lColor, "N/G" , "W/N"  )
   cWindN := iif( lColor, "W/B" , "W/N"  )
   cErrH  := iif( lColor, "W+/R", "W+/N" )
   cErrN  := iif( lColor, "W/R" , "W/N"  )

   SAVE SCREEN TO sDosScrn
   nDosRow := Row()
   nDosCol := Col()
   SetColor( "w/n" )
   CLS
   NoSnow( "NOSNOW" $ Upper( cCmdLine ) )
   IF "VGA" $ Upper( cCmdLine )
      SetMode( 50, 80 )
   ENDIF
   t_nMaxRow := MaxRow()
   SetBlink( .F. )
   SetColor( cWindN + "*" )
   CLS
   SetColor( cNormN )
   @ t_nMaxRow, 0
   @ t_nMaxRow, 0 SAY hb_UTF8ToStr( " FT_MENU1 1.0 │ " )
   @ t_nMaxRow, 16 SAY "WRITTEN BY PAUL FERRARA [76702,556] FOR NANFORUM.LIB"
   @ t_nMaxRow, 69 SAY hb_UTF8ToStr( "│ " ) + DToC( Date() )

   SetColor( cErrH )
   @ t_nMaxRow - 11, 23, t_nMaxRow - 3, 56 BOX hb_UTF8ToStr( "┌─┐│┘─└│ " )
   @ t_nMaxRow - 9, 23 SAY hb_UTF8ToStr( "├────────────────────────────────┤" )
   SetColor( cErrN )
   @ t_nMaxRow - 10, 33 SAY "Navigation Keys"
   @ t_nMaxRow - 8, 25 SAY "LeftArrow   RightArrow   Alt-E"
   @ t_nMaxRow - 7, 25 SAY "Home        End          Alt-R"
   @ t_nMaxRow - 6, 25 SAY "Tab         Shift-Tab    Alt-D"
   @ t_nMaxRow - 5, 25 SAY "PgUp        PgDn         Alt-M"
   @ t_nMaxRow - 4, 25 SAY "Enter       ESCape       Alt-Q"
   SetColor( cNormN )

   FT_MENU1( aBar, aOptions, aColors )

   SetColor( "W/N" )
   SetCursor( SC_NORMAL )
   SetBlink( .T. )
   IF "VGA" $ Upper( cCmdLine )
      SetMode( 25, 80 )
   ENDIF
   RESTORE SCREEN FROM sDosScrn
   SetPos( nDosRow, nDosCol )
   QUIT

FUNCTION fubar()

   LOCAL OldColor := SetColor( "W/N" )

   CLS
   QOut( "Press Any Key" )
   Inkey( 0 )
   SetColor( OldColor )

   RETURN .T.

#endif

// end of demo program

FUNCTION FT_MENU1( aBar, aOptions, aColors, nTopRow, lShadow )

   LOCAL nTtlUsed
   LOCAL sMainScrn, lCancMode, lLooping := .T.

   // column position for each item on the menu bar
   LOCAL aBarCol[ Len( aBar ) ]

   // inkey code for each item on menu bar
   LOCAL aBarKeys[ Len( aBar ) ]

   // inkey codes for A - Z
   LOCAL aKeyCodes := { 286, 304, 302, 288, 274, 289, 290, 291, 279, ;
      292, 293, 294, 306, 305, 280, 281, 272, 275, ;
      287, 276, 278, 303, 273, 301, 277, 300 }

   // Len() of widest array element for for each pulldown menu
   LOCAL aBarWidth[ Len( aBar ) ]

   // starting column for each box
   LOCAL aBoxLoc[ Len( aBar ) ]

   // last selection for each element
   LOCAL aLastSel[ Len( aBar ) ]

   // color memvars
   LOCAL cBorder  := aColors[ 1 ]
   LOCAL cBox     := aColors[ 2 ]
   LOCAL cBar     := aColors[ 3 ]
   LOCAL cCurrent := aColors[ 4 ]
   LOCAL cUnSelec := aColors[ 5 ]

   t_nMaxRow := MaxRow()
   t_nMaxCol := MaxCol()

   // row for menu bar
   nTopRow := iif( nTopRow == NIL, 0, nTopRow )

   AFill( aLastSel, 1 )
   t_aChoices := aOptions

   // this is the routine that calculates the position of each item
   // on the menu bar.

   aBarCol[1] := 0
   nTtlUsed := Len( aBar[ 1 ] ) + 1
   AEval( aBar, ;
      {| x, i | HB_SYMBOL_UNUSED( x ), aBarcol[ i ] := nTtlUsed, nTtlUsed += ( Len( aBar[ i ] ) + 1 ) }, ;
      2, Len( aBar ) - 1 )

   // calculates widest element for each pulldown menu
   // see below for _ftWidest()
   AFill( aBarWidth, 1 )
   AEval( t_aChoices, {| x, i | HB_SYMBOL_UNUSED( x ), _ftWidest( @i, t_aChoices, @aBarWidth ) } )

   // box location for each pulldown menu
   // see below for _ftLocat()
   AEval( t_aChoices, {| x, i | HB_SYMBOL_UNUSED( x ), _ftLocat( i, aBarCol, aBarWidth, @aBoxLoc, t_nMaxCol ) } )

   // valid keys for each pulldown menu
   // see below for _ftValKeys()
   AEval( t_aChoices, {| x, i | HB_SYMBOL_UNUSED( x ), AAdd( t_aValidKeys, "" ), ;
      _ftValKeys( i, t_aChoices, @t_aValidKeys ) } )

   // display the menu bar
   SetColor( cBar )
   @ nTopRow, 0
   AEval( aBar, {| x, i | HB_SYMBOL_UNUSED( x ), DevPos( nTopRow, aBarCol[ i ] ), DevOut( aBar[ i ] ) } )

   // store inkey code for each item on menu bar to aBarKeys
   AEval( aBarKeys, {| x, i | HB_SYMBOL_UNUSED( x ), aBarKeys[ i ] := ;
      aKeyCodes[ Asc( Upper( LTrim( aBar[ i ] ) ) ) - Asc( "@" ) ] } )

   // disable Alt-C and Alt-D
   lCancMode := SetCancel( .F. )
   AltD( DISABLE )

   // main menu loop
   SAVE SCREEN TO sMainScrn
   // which menu and which menu item
   t_nHPos := 1
   t_nVPos := 1
   DO WHILE lLooping
      RESTORE SCREEN FROM sMainScrn
      SetColor( cCurrent )
      @ nTopRow, aBarCol[ t_nHPos ] SAY aBar[ t_nHPos ]
      IF lShadow == NIL .OR. lShadow
         FT_SHADOW( nTopRow + 1, aBoxLoc[ t_nHPos ], Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 3 + aBoxLoc[ t_nHPos ] )
      ENDIF
      SetColor( cBorder )
      @ nTopRow + 1, aBoxLoc[ t_nHPos ], Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 3 + aBoxLoc[ t_nHPos ] BOX hb_UTF8ToStr( "╔═╗║╝═╚║ " )
      SetColor( cBox + "," + cCurrent + ",,," + cUnselec )
      t_nVPos := AChoice( nTopRow + 2, aBoxLoc[ t_nHPos ] + 2, Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 1 + aBoxLoc[ t_nHPos ], t_aChoices[ t_nHPos, 1 ], t_aChoices[ t_nHPos, 3 ], "__ftAcUdf", aLastSel[ t_nHPos ] )
      DO CASE
      CASE LastKey() == K_RIGHT .OR. LastKey() == K_TAB
         t_nHPos := iif( t_nHPos == Len( t_aChoices ), 1, t_nHPos + 1 )
      CASE LastKey() == K_LEFT .OR. LastKey() == K_SH_TAB
         t_nHPos := iif( t_nHPos == 1, Len( t_aChoices ), t_nHPos - 1 )
      CASE LastKey() == K_ESC
         lLooping := _ftBailOut( cBorder, cBox )
      CASE LastKey() == K_HOME
         t_nHPos := 1
      CASE LastKey() == K_END
         t_nHPos := Len( t_aChoices )
      CASE LastKey() == K_ENTER
         aLastSel[ t_nHPos ] := t_nVPos
         IF t_aChoices[ t_nHPos, 2, t_nVPos ] != NIL
            SetCancel( lCancMode )
            AltD( ENABLE )
            lLooping := Eval( t_aChoices[ t_nHPos, 2, t_nVPos ] )
            AltD( DISABLE )
            SetCancel( .F. )
         ENDIF
      CASE AScan( aBarKeys, LastKey() ) > 0
         t_nHPos := AScan( aBarKeys, LastKey() )
      ENDCASE
   ENDDO
   SetCancel( lCancMode )
   AltD( ENABLE )
   RESTORE SCREEN FROM sMainScrn

   RETURN NIL

// ACHOICE() user function
FUNCTION __ftAcUdf( nMode )

   LOCAL nRtnVal := AC_CONT
   DO CASE
   CASE nMode == AC_HITTOP
      hb_keyPut( K_CTRL_END )
   CASE nMode == AC_HITBOTTOM
      hb_keyPut( K_CTRL_HOME )
   CASE nMode == AC_EXCEPT
      IF Upper( hb_keyChar( LastKey() ) ) $ t_aValidKeys[ t_nHPos ]
         IF t_aChoices[ t_nHPos, 3, At( Upper( hb_keyChar( LastKey() ) ), t_aValidKeys[ t_nHPos ] ) ]
            hb_keyPut( K_ENTER )
            nRtnVal := AC_GOTO
         ENDIF
      ELSE
         nRtnVal := AC_SELECT
      ENDIF
   ENDCASE

   RETURN nRtnVal

STATIC FUNCTION _ftWidest( i, t_aChoices, aBarWidth )

   AEval( t_aChoices[ i, 1 ], {| a, b | HB_SYMBOL_UNUSED( a ), aBarWidth[ i ] := ;
      Max( aBarWidth[ i ], Len( t_aChoices[ i, 1, b ] ) ) } )

   RETURN NIL

STATIC FUNCTION _ftLocat( i, aBarCol, aBarWidth, aBoxLoc, t_nMaxCol )

   aBoxLoc[ i ] := iif( aBarCol[ i ] + aBarWidth[ i ] + 4 > t_nMaxCol + 1, ;
      t_nMaxCol - 3 - aBarWidth[ i ], aBarCol[ i ] )

   RETURN NIL

STATIC FUNCTION _ftBailOut( cBorder, cBox )

   LOCAL cOldColor, sOldScreen, nKeyPress, nOldCursor

   nOldCursor := SetCursor( SC_NONE )
   sOldScreen := SaveScreen( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55 )
   cOldColor := SetColor( cBorder )
   FT_SHADOW( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55 )
   @ t_nMaxRow / 2 - 1, 24, t_nMaxRow/2 + 2, 55 BOX hb_UTF8ToStr( "╔═╗║╝═╚║ " )
   SetColor( cBox )
   @ t_nMaxRow / 2,  26 SAY "Press ESCape To Confirm Exit"
   @ t_nMaxRow / 2 + 1, 27 SAY "Or Any Other Key To Resume"
   nKeyPress := Inkey( 0 )
   SetColor( cOldColor )
   RestScreen( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55, sOldScreen )
   SetCursor( nOldCursor )

   RETURN !( nKeyPress == K_ESC )

STATIC FUNCTION _ftValKeys( nNum, t_aChoices, t_aValidKeys )

   AEval( t_aChoices[ nNum, 1 ], {| x | t_aValidKeys[ nNum ] += Left( x, 1 ) } )

   RETURN NIL

FUNCTION FT_FILL( aArray, cMenuOption, bBlock, lAvailable )

   AAdd( aArray[ 1 ], cMenuOption )
   AAdd( aArray[ 2 ], bBlock )
   AAdd( aArray[ 3 ], lAvailable )

   RETURN NIL
