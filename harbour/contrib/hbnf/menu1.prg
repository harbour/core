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

FUNCTION ft_Menu1( aBar, aOptions, aColors, nTopRow, lShadow )

   LOCAL nTtlUsed
   LOCAL sMainScrn, lCancMode, lLooping := .T.

   // column position for each item on the menu bar
   LOCAL aBarCol[ Len( aBar ) ]

   // inkey code for each item on menu bar
   LOCAL aBarKeys[ Len( aBar ) ]

   // inkey codes for A - Z
   LOCAL aKeyCodes := { ;
      K_ALT_A, ;
      K_ALT_B, ;
      K_ALT_C, ;
      K_ALT_D, ;
      K_ALT_E, ;
      K_ALT_F, ;
      K_ALT_G, ;
      K_ALT_H, ;
      K_ALT_I, ;
      K_ALT_J, ;
      K_ALT_K, ;
      K_ALT_L, ;
      K_ALT_M, ;
      K_ALT_N, ;
      K_ALT_O, ;
      K_ALT_P, ;
      K_ALT_Q, ;
      K_ALT_R, ;
      K_ALT_S, ;
      K_ALT_T, ;
      K_ALT_U, ;
      K_ALT_V, ;
      K_ALT_W, ;
      K_ALT_X, ;
      K_ALT_Y, ;
      K_ALT_Z }

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
   __defaultNIL( @nTopRow, 0 )

   AFill( aLastSel, 1 )
   t_aChoices := aOptions

   // this is the routine that calculates the position of each item
   // on the menu bar.

   aBarCol[ 1 ] := 0
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
   hb_Scroll( nTopRow, 0, nTopRow )
   AEval( aBar, {| x, i | HB_SYMBOL_UNUSED( x ), hb_DispOutAt( nTopRow, aBarCol[ i ], aBar[ i ] ) } )

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
      hb_DispOutAt( nTopRow, aBarCol[ t_nHPos ], aBar[ t_nHPos ] )
      IF lShadow == NIL .OR. lShadow
         hb_Shadow( nTopRow + 1, aBoxLoc[ t_nHPos ], Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 3 + aBoxLoc[ t_nHPos ] )
      ENDIF
      hb_DispBox( nTopRow + 1, aBoxLoc[ t_nHPos ], Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 3 + aBoxLoc[ t_nHPos ], hb_UTF8ToStrBox( "╔═╗║╝═╚║ " ), cBorder )
      SetColor( cBox + "," + cCurrent + ",,," + cUnselec )
      t_nVPos := AChoice( nTopRow + 2, aBoxLoc[ t_nHPos ] + 2, Len( t_aChoices[ t_nHPos, 1 ] ) + nTopRow + 2, aBarWidth[ t_nHPos ] + 1 + aBoxLoc[ t_nHPos ], t_aChoices[ t_nHPos, 1 ], t_aChoices[ t_nHPos, 3 ], {| nMode | __ftAcUdf( nMode ) }, aLastSel[ t_nHPos ] )
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

// AChoice() user function

STATIC FUNCTION __ftAcUdf( nMode )

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

   LOCAL sOldScreen, nKeyPress, nOldCursor

   nOldCursor := SetCursor( SC_NONE )
   sOldScreen := SaveScreen( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55 )
   hb_Shadow( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55 )
   hb_DispBox( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55, hb_UTF8ToStrBox( "╔═╗║╝═╚║ " ), cBorder )
   hb_DispOutAt( t_nMaxRow / 2, 26, "Press ESCape To Confirm Exit", cBox )
   hb_DispOutAt( t_nMaxRow / 2 + 1, 27, "Or Any Other Key To Resume", cBox )
   nKeyPress := Inkey( 0 )
   RestScreen( t_nMaxRow / 2 - 1, 24, t_nMaxRow / 2 + 2, 55, sOldScreen )
   SetCursor( nOldCursor )

   RETURN nKeyPress != K_ESC

STATIC FUNCTION _ftValKeys( nNum, t_aChoices, t_aValidKeys )

   AEval( t_aChoices[ nNum, 1 ], {| x | t_aValidKeys[ nNum ] += Left( x, 1 ) } )

   RETURN NIL

FUNCTION ft_Fill( aArray, cMenuOption, bBlock, lAvailable )

   AAdd( aArray[ 1 ], cMenuOption )
   AAdd( aArray[ 2 ], bBlock )
   AAdd( aArray[ 3 ], lAvailable )

   RETURN NIL
