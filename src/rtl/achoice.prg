/*
 * AChoice() function
 *
 * Released to Public Domain by Peter Townsend <cephas@tpgi.com.au>
 *
 */

#pragma -gc0

#include "achoice.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define INRANGE( xLo, xVal, xHi )  ( xVal >= xLo .AND. xVal <= xHi )
#define BETWEEN( xLo, xVal, xHi )  Min( Max( xLo, xVal ), xHi )

/* NOTE: Extension: Harbour supports codeblocks and function pointers
         as the xSelect parameter (both when supplied as is, or as an
         array of values). [vszakats] */

FUNCTION AChoice( nTop, nLeft, nBottom, nRight, acItems, xSelect, xUserFunc, nPos, nHiLiteRow )

   LOCAL nNumCols           // Number of columns in the window
   LOCAL nNumRows           // Number of rows in the window
   LOCAL nRowsClr           // Number of rows to clear
   LOCAL alSelect           // Select permission
   LOCAL nNewPos   := 0     // The next item to be selected
   LOCAL lFinished          // Is processing finished?
   LOCAL nKey      := 0     // The keystroke to be processed
   LOCAL nKeyStd   := 0
   LOCAL nMode              // The current operating mode
   LOCAL nAtTop             // The number of the item at the top
   LOCAL nItems    := 0     // The number of items
   LOCAL nGap               // The number of lines between top and current lines

   // Block used to search for items
   LOCAL lUserFunc          // Is a user function to be used?
   LOCAL nUserFunc          // Return value from user function
   LOCAL nSaveCsr
   LOCAL nFrstItem := 0
   LOCAL nLastItem := 0

   LOCAL bAction
   LOCAL cKey
   LOCAL nAux

   nTop := Int( hb_defaultValue( nTop, 0 ) )
   nBottom := Int( hb_defaultValue( nBottom, 0 ) )

   hb_default( @nLeft, 0 )
   hb_default( @nRight, 0 )

   IF nRight > MaxCol()
      nRight := MaxCol()
   ENDIF

   IF nBottom > MaxRow()
      nBottom := MaxRow()
   ENDIF

   IF ! HB_ISARRAY( acItems ) .OR. Len( acItems ) == 0
      SetPos( nTop, nRight + 1 )
      RETURN 0
   ENDIF

   nSaveCsr := SetCursor( SC_NONE )

   ColorSelect( CLR_STANDARD )

   /* NOTE: Undocumented parameter passing handled. AChoice()
            is called in such way in rldialg.prg from RL tool
            supplied with Clipper 5.x. 6th parameter is the
            user function and 7th parameter is zero (empty I
            suppose). [vszakats] */
   IF Empty( xUserFunc ) .AND. ValType( xSelect ) $ "CBS"
      xUserFunc := xSelect
      xSelect := NIL
   ENDIF

   lUserFunc := ! Empty( xUserFunc ) .AND. ValType( xUserFunc ) $ "CBS"

   IF ! HB_ISARRAY( xSelect ) .AND. ! HB_ISLOGICAL( xSelect )
      xSelect := .T.               // Array or logical, what is selectable
   ENDIF

   hb_default( @nPos, 1 )          // The number of the selected item
   hb_default( @nHiLiteRow, 0 )    // The row to be highlighted

   nNumCols := nRight - nLeft + 1
   nNumRows := nBottom - nTop + 1

   IF HB_ISARRAY( xSelect )
      alSelect := xSelect
   ELSE
      alSelect := Array( Len( acItems ) )
      AFill( alSelect, xSelect )
   ENDIF

   IF ( nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, alSelect, acItems ) ) == AC_NOITEM
      nPos := 0
   ENDIF

   // Ensure hilighted item can be selected
   nPos := BETWEEN( nFrstItem, nPos, nLastItem )

   // Force hilighted row to be valid
   nHiLiteRow := BETWEEN( 0, nHiLiteRow, nNumRows - 1 )

   // Force the topmost item to be a valid index of the array
   nAtTop := BETWEEN( 1, Max( 1, nPos - nHiLiteRow ), nItems )

   // Ensure as much of the selection area as possible is covered
   IF ( nAtTop + nNumRows - 1 ) > nItems
      nAtTop := Max( 1, nItems - nNumrows + 1 )
   ENDIF

   DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, nItems )

   lFinished := ( nMode == AC_NOITEM )
   IF lFinished .AND. lUserFunc
      Do( xUserFunc, nMode, nPos, nPos - nAtTop )
   ENDIF
   DO WHILE ! lFinished

      IF nMode != AC_EXCEPT .AND. nMode != AC_NOITEM
         nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )
         nMode := AC_IDLE
      ENDIF

      DO CASE
      CASE ( bAction := SetKey( nKey ) ) != NIL .OR. ;
           ( bAction := SetKey( nKeyStd ) ) != NIL

         Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
         IF NextKey() == 0
            hb_keySetLast( 255 )
            nKey := nKeyStd := 0
         ENDIF

         nRowsClr := Min( nNumRows, nItems )

         IF ( nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, alSelect, acItems ) ) == AC_NOITEM
            nPos := 0
            nAtTop := Max( 1, nPos - nNumRows + 1 )
         ELSE
            DO WHILE nPos < nLastItem .AND. ! Ach_Select( alSelect, nPos )
               nPos++
            ENDDO

            IF nPos > nLastItem
               nPos := BETWEEN( nFrstItem, nPos, nLastItem )
            ENDIF

            nAtTop := Min( nAtTop, nPos )

            IF nAtTop + nNumRows - 1 > nItems
               nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems - nNumRows + 1 )
            ENDIF

            IF nAtTop < 1
               nAtTop := 1
            ENDIF
         ENDIF

         DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems, nRowsClr )

      CASE ( nKeyStd == K_ESC .OR. nMode == AC_NOITEM ) .AND. ! lUserFunc

         IF nPos != 0
            DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
         ENDIF

         nMode     := AC_IDLE
         nPos      := 0
         lFinished := .T.

      CASE nKeyStd == K_LDBLCLK .OR. nKeyStd == K_LBUTTONDOWN
         nAux := HitTest( nTop, nLeft, nBottom, nRight, MRow(), MCol() )
         IF nAux != 0 .AND. ( nNewPos := nAtTop + nAux - 1 ) <= nItems
            IF Ach_Select( alSelect, nNewPos )
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
               IF nKeyStd == K_LDBLCLK
                  hb_keyIns( K_ENTER )
               ENDIF
            ENDIF
         ENDIF

#ifdef HB_CLP_STRICT
      CASE nKeyStd == K_UP
#else
      CASE nKeyStd == K_UP .OR. nKeyStd == K_MWFORWARD
#endif

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            nNewPos := nPos - 1
            DO WHILE ! Ach_Select( alSelect, nNewPos )
               nNewPos--
            ENDDO
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ELSE
               DispBegin()
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               hb_Scroll( nTop, nLeft, nBottom, nRight, nNewPos - ( nAtTop + nNumRows - 1 ) )
               nAtTop := nNewPos
               nPos   := Max( nPos, nAtTop + nNumRows - 1 )
               DO WHILE nPos > nNewPos
                  IF nTop + nPos - nAtTop <= nBottom
                     DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
                  ENDIF
                  nPos--
               ENDDO
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
               DispEnd()
            ENDIF
         ENDIF

#ifdef HB_CLP_STRICT
      CASE nKeyStd == K_DOWN
#else
      CASE nKeyStd == K_DOWN .OR. nKeyStd == K_MWBACKWARD
#endif

         // Find the next selectable item to display
         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < Min( nPos, nItems - nNumRows + 1 )
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE

            nNewPos := nPos + 1

            DO WHILE ! Ach_Select( alSelect, nNewPos )
               nNewPos++
            ENDDO

            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ELSE
               DispBegin()
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               hb_Scroll( nTop, nLeft, nBottom, nRight, nNewPos - ( nAtTop + nNumRows - 1 ) )
               nAtTop := nNewPos - nNumRows + 1
               nPos   := Max( nPos, nAtTop )
               DO WHILE nPos < nNewPos
                  DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
                  nPos++
               ENDDO
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
               DispEnd()
            ENDIF
         ENDIF

      CASE nKeyStd == K_CTRL_PGUP .OR. ( nKeyStd == K_HOME .AND. ! lUserFunc )

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            nPos   := nFrstItem
            nAtTop := nPos
            DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
         ENDIF

      CASE nKeyStd == K_CTRL_PGDN .OR. ( nKeyStd == K_END .AND. ! lUserFunc )

         IF nPos == nLastItem
            IF nAtTop == Min( nLastItem, nItems - Min( nItems, nNumRows ) + 1 )
               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nLastItem, nItems - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nLastItem
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ELSE
               nPos   := nLastItem
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ENDIF

      CASE nKeyStd == K_CTRL_HOME

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            nNewPos := nAtTop
            DO WHILE ! Ach_Select( alSelect, nNewPos )
               nNewPos++
            ENDDO
            IF nNewPos != nPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ENDIF
         ENDIF

      CASE nKeyStd == K_CTRL_END

         IF nPos == nLastItem
            IF nAtTop == Min( nPos, nItems - Min( nItems, nNumRows ) + 1 ) .OR. nPos == nItems
               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            nNewPos := Min( nAtTop + nNumRows - 1, nItems )
            DO WHILE ! Ach_Select( alSelect, nNewPos )
               nNewPos--
            ENDDO
            IF nNewPos != nPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ENDIF
         ENDIF

      CASE nKeyStd == K_PGUP

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nFrstItem, nAtTop + nNumRows - 1 )
               // On same page as nFrstItem
               nPos   := nFrstItem
               nAtTop := Max( nPos - nNumRows + 1, 1 )
            ELSE
               IF ( nPos - nNumRows + 1 ) < nFrstItem
                  nPos   := nFrstItem
                  nAtTop := nFrstItem
               ELSE
                  nPos   := Max( nFrstItem, nPos - nNumRows + 1 )
                  nAtTop := Max( 1, nAtTop - nNumRows + 1 )
                  DO WHILE nPos > nFrstItem .AND. ! Ach_Select( alSelect, nPos )
                     nPos--
                     nAtTop--
                  ENDDO
                  nAtTop := Max( 1, nAtTop )
                  IF nAtTop < nNumRows .AND. nPos < nNumRows
                     nPos := nNumRows
                     nAtTop := 1
                  ENDIF
               ENDIF
            ENDIF
            DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
         ENDIF

      CASE nKeyStd == K_PGDN

         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < Min( nPos, nItems - nNumRows + 1 )
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               // On the same page as nLastItem
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nLastItem
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ELSE
               nGap := nPos - nAtTop
               nPos := Min( nLastItem, nPos + nNumRows - 1 )
               IF ( nPos + nNumRows - 1 ) > nLastItem
                  // On the last page
                  nAtTop := nLastItem - nNumRows + 1
                  nPos   := Min( nLastItem, nAtTop + nGap )
               ELSE
                  // Not on the last page
                  nAtTop := nPos - nGap
               ENDIF
               // Make sure that the item is selectable
               DO WHILE nPos < nLastItem .AND. ! Ach_Select( alSelect, nPos )
                  nPos++
                  nAtTop++
               ENDDO
               // Don't leave blank space on the page
               DO WHILE ( nAtTop + nNumRows - 1 ) > nItems
                  nAtTop--
               ENDDO
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ENDIF

      CASE nKeyStd == K_ENTER .AND. ! lUserFunc

         IF nPos != 0
            DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
         ENDIF

         nMode     := AC_IDLE
         lFinished := .T.

      CASE nKeyStd == K_RIGHT .AND. ! lUserFunc

         IF nPos != 0
            DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
         ENDIF

         nPos      := 0
         lFinished := .T.

      CASE nKeyStd == K_LEFT .AND. ! lUserFunc

         IF nPos != 0
            DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
         ENDIF

         nPos      := 0
         lFinished := .T.

      CASE ( ! lUserFunc .OR. nMode == AC_EXCEPT ) .AND. ;
           ! ( cKey := Upper( hb_keyChar( nKeyStd ) ) ) == ""

         // Find next selectable item
         FOR nNewPos := nPos + 1 TO nItems
            IF Ach_Select( alSelect, nNewPos ) .AND. hb_LeftEqI( acItems[ nNewPos ], cKey )
               EXIT
            ENDIF
         NEXT
         IF nNewPos == nItems + 1
            FOR nNewPos := 1 TO nPos - 1
               IF Ach_Select( alSelect, nNewPos ) .AND. hb_LeftEqI( acItems[ nNewPos ], cKey )
                  EXIT
               ENDIF
            NEXT
         ENDIF

         IF nNewPos != nPos
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               // On same page
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .F., nNumCols )
               nPos := nNewPos
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, Ach_Select( alSelect, nPos ), .T., nNumCols )
            ELSE
               // On different page
               nPos   := nNewPos
               nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems )
               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems )
            ENDIF
         ENDIF

         nMode := AC_IDLE

      CASE nMode == AC_EXCEPT

         // Handle keypresses which don't translate to characters
         nMode := AC_IDLE

      CASE nMode != AC_NOITEM

         nMode := iif( nKeyStd == 0, AC_IDLE, AC_EXCEPT )

      ENDCASE

      IF lUserFunc

         IF HB_ISNUMERIC( nUserFunc := Do( xUserFunc, nMode, nPos, nPos - nAtTop ) )

            SWITCH nUserFunc
            CASE AC_ABORT
            CASE AC_REDRAW  /* QUESTION: Is this correct? */
               IF nPos != 0
                  DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
               ENDIF
               lFinished := .T.
               nPos      := 0
               EXIT
            CASE AC_SELECT
               IF nPos != 0
                  DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
               ENDIF
               lFinished := .T.
               EXIT
            CASE AC_CONT
               // Do nothing
               nMode := AC_IDLE
               EXIT
            CASE AC_GOTO
               // Do nothing. The next keystroke won't be read and
               // this keystroke will be processed as a goto.
               nMode := AC_EXCEPT
               EXIT
            ENDSWITCH

            IF nPos > 0 .AND. nMode != AC_EXCEPT

#if 0
               /* TOVERIFY: Disabled nRowsClr in DispPage() call.
                  Please verify it, I do not know why it was added but
                  it breaks code which adds dynamically new acItems positions */
               nRowsClr := Min( nNumRows, nItems )
#endif
               IF ( nMode := Ach_Limits( @nFrstItem, @nLastItem, @nItems, alSelect, acItems ) ) == AC_NOITEM
                  nPos := 0
                  nAtTop := Max( 1, nPos - nNumRows + 1 )
               ELSE
                  DO WHILE nPos < nLastItem .AND. ! Ach_Select( alSelect, nPos )
                     nPos++
                  ENDDO

                  IF nPos > nLastItem
                     nPos := BETWEEN( nFrstItem, nPos, nLastItem )
                  ENDIF

                  nAtTop := Min( nAtTop, nPos )

                  IF nAtTop + nNumRows - 1 > nItems
                     nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems - nNumRows + 1 )
                  ENDIF

                  IF nAtTop < 1
                     nAtTop := 1
                  ENDIF
               ENDIF

               DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nItems /*, nRowsClr */ )
            ENDIF
         ELSE
            IF nPos != 0
               DispLine( acItems[ nPos ], nTop + nPos - nAtTop, nLeft, .T., .F., nNumCols )
            ENDIF
            nPos      := 0
            lFinished := .T.
         ENDIF
      ENDIF
   ENDDO

   SetCursor( nSaveCsr )

   RETURN nPos

STATIC FUNCTION HitTest( nTop, nLeft, nBottom, nRight, mRow, mCol )

   IF mCol >= nLeft .AND. ;
      mCol <= nRight .AND. ;
      mRow >= nTop .AND. ;
      mRow <= nBottom
      RETURN mRow - nTop + 1
   ENDIF

   RETURN 0

STATIC PROCEDURE DispPage( acItems, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop, nArrLen, nRowsClr )

   LOCAL nCntr
   LOCAL nRow       // Screen row
   LOCAL nIndex     // Array index

   hb_default( @nRowsClr, nArrLen )

   DispBegin()

   FOR nCntr := 1 TO Min( nNumRows, nRowsClr )

      nRow   := nTop + nCntr - 1
      nIndex := nCntr + nAtTop - 1

      IF INRANGE( 1, nIndex, nArrLen )
         DispLine( acItems[ nIndex ], nRow, nLeft, Ach_Select( alSelect, nIndex ), nIndex == nPos, nRight - nLeft + 1 )
      ELSE
         ColorSelect( CLR_STANDARD )
         hb_DispOutAt( nRow, nLeft, Space( nRight - nLeft + 1 ) )
      ENDIF
   NEXT

   DispEnd()

   RETURN

STATIC PROCEDURE DispLine( cLine, nRow, nCol, lSelect, lHiLite, nNumCols )

   ColorSelect( iif( lSelect .AND. HB_ISSTRING( cLine ), ;
      iif( lHiLite, CLR_ENHANCED, CLR_STANDARD ), CLR_UNSELECTED ) )

   hb_DispOutAt( nRow, nCol, iif( HB_ISSTRING( cLine ), hb_UPadR( cLine, nNumCols ), Space( nNumCols ) ) )
   IF lHiLite
      SetPos( nRow, nCol )
   ENDIF

   ColorSelect( CLR_STANDARD )

   RETURN

STATIC FUNCTION Ach_Limits( /* @ */ nFrstItem, /* @ */ nLastItem, /* @ */ nItems, alSelect, acItems )

   LOCAL nCntr

   nFrstItem := nLastItem := nItems := 0

   FOR nCntr := 1 TO Len( acItems )
      IF HB_ISSTRING( acItems[ nCntr ] ) .AND. ! HB_ISNULL( acItems[ nCntr ] )
         nItems++
         IF Ach_Select( alSelect, nCntr )
            IF nFrstItem == 0
               nFrstItem := nLastItem := nCntr
            ELSE
               nLastItem := nItems
            ENDIF
         ENDIF
      ELSE
         EXIT
      ENDIF
   NEXT

   IF nFrstItem == 0
      nLastItem := nItems
      RETURN AC_NOITEM
   ENDIF

   RETURN AC_IDLE

STATIC FUNCTION Ach_Select( alSelect, nPos )

   LOCAL sel

   IF nPos >= 1 .AND. nPos <= Len( alSelect )
      sel := alSelect[ nPos ]
      DO CASE
      CASE HB_ISEVALITEM( sel )
         sel := Eval( sel )
      CASE HB_ISSTRING( sel ) .AND. ! Empty( sel )
         sel := Eval( hb_macroBlock( sel ) )
      ENDCASE
      IF HB_ISLOGICAL( sel )
         RETURN sel
      ENDIF
   ENDIF

   RETURN .T.
