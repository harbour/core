/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ACHOICE() function
 *
 * Released to Public Domain by Peter Townsend <cephas@tpgi.com.au>
 * www - http://www.harbour-project.org
 *
 */

#include "achoice.ch"
#include "color.ch"
#include "common.ch"
#include "inkey.ch"
#include "set.ch"
#include "setcurs.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      ACHOICE()
 *  $CATEGORY$
 *      Array
 *
 *  $ONELINER$
 *      Allows selection of an element from an array
 *
 *  $SYNTAX$
 *      ACHOICE(<nTop>, <nLeft>, <nBottom>, <nRight>,
 *         <acMenuItems>,
 *         [<alSelectableItems> | <lSelectableItems>],
 *         [<cUserFunction> | <bUserBlock>],
 *         [<nInitialItem>],
 *         [<nWindowRow>]) --> nPosition
 *
 *  $ARGUMENTS$
 *      nTop           - topmost row used to display array (default 0)
 *      nLeft          - leftmost row used to display array (default 0)
 *      nBottom        - bottommost row used to display array (default MAXROW())
 *      nRight         - rightmost row used to display array (default MAXCOL())
 *      acMenuItems    - the character array of items from which to select
 *      alSelectableItems - an array of items, either logical or character,
 *                          which is used to determine if a particular item
 *                          may be selected.  If the type of a given item is
 *                          character, it is macro evaluated, and the result
 *                          is expected to be a logical.  A value of .T. means
 *                          that the item may be selected, .F. that it may not.
 *                          (See next argument: lSelectableItems)
 *      lSelectableItems  - a logical value which is used to apply to all
 *                          items in acMenuItems.  If .T., all items may be
 *                          selected; if .F., none may be selected.
 *                          (See previous argument: alSelectableItems)
 *                          Default .T.
 *      cUserFunction     - the name of a function to be called which may
 *                          effect special processing of keystrokes.  It is
 *                          specified without parentheses or parameters.
 *                          When it is called, it will be supplied with the
 *                          parameters: nMode, nCurElement, and nRowPos.
 *                          Default NIL.
 *      bUserBlock     - a codeblock to be called which may
 *                          effect special processing of keystrokes. It
 *                          should be specified in the form
 *                          {|nMode, nCurElemenet, nRowPos| ;
 *                                 MyFunc(nMode, nCurElemenet, nRowPos) }.
 *                          Default NIL.
 *      nInitialItem   - the number of the element to be highlighted as
 *                          the current item when the array is initally
 *                          displayed.  1 origin.  Default 1.
 *      nWindowRow     - the number of the window row on which the initial
 *                          item is to be displayed. 0 origin.  Default 0.
 *
 *  $RETURNS$
 *      nPosition  - the number of the item to be selected, or 0 if the
 *                   selection was aborted.
 *
 *  $DESCRIPTION$
 *      Allows selection of an element from an array.
 *      Please see standard Clipper documentation for ACHOICE for
 *      additional detail.
 *
 *  $EXAMPLES$
 *      aItems := { "One", "Two", "Three" }
 *      nChoice := ACHOICE( 10, 10, 20, 20, aItems )
 *      IF nChoice == 0
 *          ? "You did not choose an item"
 *      ELSE
 *          ? "You chose element " + LTRIM( STR( nChoice ) )
 *          ?? " which has a value of " + aItems[ nChoice ]
 *      ENDIF
 *
 *  $SEEALSO$
 * 
 *  $END$
 */

#define INRANGE( xLo, xVal, xHi )       ( xVal >= xLo .AND. xVal <= xHi )
#define BETWEEN( xLo, xVal, xHi )       Min( Max( xLo, xVal ), xHi )

FUNCTION AChoice( nTop, nLeft, nBottom, nRight, acItems, xSelect, xUserFunc, nPos, nHiLiteRow )

   LOCAL nNumCols                          // Number of columns in the window
   LOCAL nNumRows                          // Number of rows in the window
   LOCAL acCopy    := {}                   // A padded copy of the items
   LOCAL alSelect                          // Select permission
   LOCAL nNewPos   := 0                    // The next item to be selected
   LOCAL lFinished := .F.                  // Is processing finished?
   LOCAL nKey      := 0                    // The keystroke to be processed
   LOCAL nMode     := AC_IDLE              // The current operating mode
   LOCAL nAtTop    := 1                    // The number of the item at the top
   LOCAL nAtBtm    := 1                    // The number of the item at the bottom
   LOCAL nItems                            // The number of items
   LOCAL nGap      := 0                    // The number of lines between top and current lines
                                           // Block used to search for items
   LOCAL lUserFunc                         // Is a user function to be used?
   LOCAL nUserFunc := 0                    // Return value from user function
   LOCAL nSaveCsr  := SetCursor( SC_NONE )
   LOCAL nFrstItem := 0
   LOCAL nLastItem := 0
   LOCAL nCntr
   LOCAL bAction
   LOCAL cKey

   ColorSelect( CLR_STANDARD )

   lUserFunc := !Empty( xUserFunc ) .AND. ValType( xUserFunc ) $ "CB"

   DEFAULT nTop    TO 0                    // The topmost row of the window
   DEFAULT nLeft   TO 0                    // The leftmost column of the window
   DEFAULT nBottom TO MaxRow() + 1         // The bottommost row of the windows
   DEFAULT nRight  TO MaxCol() + 1         // The rightmost column of the window

   DEFAULT acItems TO {}                   // The items from which to choose
   DEFAULT xSelect TO .T.                  // Array or logical, what is selectable
   DEFAULT nPos TO 1                       // The number of the selected item
   DEFAULT nHiLiteRow TO 0                 // The row to be highlighted

   nNumCols := nRight - nLeft + 1
   nNumRows := nBottom - nTop + 1
   AEval( acItems, {| x | iif( ISCHARACTER( x ), AAdd( acCopy, PadR( x, nNumCols ) ), .F. ) } )
   nItems := Len( acCopy )

   alSelect := Array( nItems )
   IF ISARRAY( xSelect )
      AFill( alSelect, .T. )
      FOR nCntr := 1 TO Len( xSelect )
         IF nCntr <= nItems
            IF ISCHARACTER( xSelect[ nCntr ] )
               IF Empty( xSelect[ nCntr ] )
                  lFinished := .T.
                  nPos      := 0
               ELSE
/* TODO: When macro evaluation will work, this should be commented out:
               alSelect[ nCntr ] := &( xSelect[ nCntr ] )
*/
               ENDIF
            ELSE
               alSelect[ nCntr ] := xSelect[ nCntr ]
            ENDIF
         ELSE
            nCntr := Len( xSelect ) + 1
         ENDIF
      NEXT
   ELSE
      AFill( alSelect, xSelect )
   ENDIF

   IF !lFinished

      nFrstItem := AScan( alSelect, .T. )  // First valid item

      IF nFrstItem == 0
         nLastItem := 0
         nPos      := 0
         nMode     := AC_NOITEM
      ELSE
         nMode     := AC_IDLE
         nLastItem := nItems               // Last valid item
         DO WHILE !alSelect[ nLastItem ]
            nLastItem--
         ENDDO
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

      DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )

   ENDIF

   DO WHILE !lFinished

      IF nMode != AC_GOTO .AND. nMode != AC_NOITEM
         nKey  := Inkey( 0 )
         nMode := AC_IDLE
      ENDIF

      DO CASE
      CASE ( bAction := SetKey( nKey ) ) != NIL

         Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
         IF Empty( NextKey() )
            KEYBOARD Chr( 255 )
            Inkey()
            nKey := 0
         ENDIF

      CASE ( nKey == K_ESC .OR. nMode == AC_NOITEM ) .AND. !lUserFunc

         nMode     := AC_ABORT
         nPos      := 0
         lFinished := .T.

      CASE nKey == K_UP

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            nNewPos := nPos - 1
            DO WHILE !alSelect[ nNewPos ]
               nNewPos --
            ENDDO
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ELSE
               DispBegin()
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               Scroll( nTop, nLeft, nBottom, nRight, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos
               nPos   := Max( nPos, nAtTop + nNumRows - 1 )
               DO WHILE nPos > nNewPos
                  DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
                  nPos --
               ENDDO
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
               DispEnd()
            ENDIF
         ENDIF

      CASE nKey == K_DOWN

         // Find the next selectable item to display
         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < Min( nPos, nItems - nNumRows + 1 )
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE

            nNewPos := nPos + 1

            DO WHILE !alSelect[ nNewPos ]
               nNewPos ++
            ENDDO

            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ELSE
               DispBegin()
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               Scroll( nTop, nLeft, nBottom, nRight, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos - nNumRows + 1
               nPos   := Max( nPos, nAtTop )
               DO WHILE nPos < nNewPos
                  DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
                  nPos ++
               ENDDO
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
               DispEnd()
            ENDIF

         ENDIF

      CASE nKey == K_CTRL_PGUP .OR. ( nKey == K_HOME .AND. !lUserFunc )

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            nPos   := nFrstItem
            nAtTop := nPos
            DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
         ENDIF

      CASE nKey == K_CTRL_PGDN .OR. ( nKey == K_END .AND. !lUserFunc )

         IF nPos == nLastItem
            IF nAtTop == Min( nLastItem, nItems - nNumRows + 1 )
               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nLastItem, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ELSE
               nPos   := nLastItem
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ENDIF

      CASE nKey == K_CTRL_HOME

         IF nPos == nFrstItem
            IF nAtTop == Max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            ELSE
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            nNewPos := nAtTop
            DO WHILE !alSelect[ nNewPos ]
               nNewPos++
            ENDDO
            IF nNewPos != nPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ENDIF
         ENDIF

      CASE nKey == K_CTRL_END

         IF nPos == nLastItem
            IF nAtTop == Min( nPos, nItems - nNumRows + 1 )
               nMode := AC_HITBOTTOM
            ELSE
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            nNewPos := nAtTop + nNumRows - 1
            DO WHILE !alSelect[ nNewPos ]
               nNewPos--
            ENDDO
            IF nNewPos != nPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ENDIF
         ENDIF

      CASE nKey == K_PGUP

         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > Max( 1, nPos - nNumRows + 1 )
               nAtTop := Max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
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
                  DO WHILE nPos > nFrstItem .AND. !alSelect[ nPos ]
                     nPos--
                     nAtTop--
                  ENDDO
                  nAtTop := Max( 1, nAtTop )
               ENDIF
            ENDIF
            DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
         ENDIF

      CASE nKey == K_PGDN

         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < Min( nPos, nItems - nNumRows + 1 )
               nAtTop := Min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ELSE
            IF INRANGE( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               // On the same page as nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
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
               DO WHILE nPos < nLastItem .AND. !alSelect[ nPos ]
                  nPos++
                  nAtTop++
               ENDDO
               // Don't leave blank space on the page
               DO WHILE ( nAtTop + nNumRows - 1 ) > nItems
                  nAtTop--
               ENDDO
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ENDIF

      CASE nKey == K_ENTER .AND. !lUserFunc

         nMode     := AC_SELECT
         lFinished := .T.

      CASE nKey == K_RIGHT .AND. !lUserFunc

         nPos      := 0
         lFinished := .T.

      CASE nKey == K_LEFT .AND. !lUserFunc

         nPos      := 0
         lFinished := .T.

      CASE INRANGE( 32, nKey, 255 ) .AND. ( !lUserFunc .OR. nMode == AC_GOTO )

         cKey := Upper( Chr( nKey ) )

         // Find next selectable item
         FOR nNewPos := nPos + 1 TO nItems
            IF alSelect[ nNewPos ] .AND. Left( acCopy[ nNewPos ], 1 ) == cKey
               EXIT
            ENDIF
         NEXT
         IF nNewPos == nItems + 1
            FOR nNewPos := 1 TO nPos - 1
               IF alSelect[ nNewPos ] .AND. Left( acCopy[ nNewPos ], 1 ) == cKey
                  EXIT
               ENDIF
            NEXT
         ENDIF

         IF nNewPos != nPos
            IF INRANGE( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               // On same page
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLeft, alSelect[ nPos ], .T. )
            ELSE
               // On different page
               nPos   := nNewPos
               nAtTop := BETWEEN( 1, nPos - nNumRows + 1, nItems )
               DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )
            ENDIF
         ENDIF

         nMode := AC_IDLE

      CASE nMode == AC_GOTO

         // Garbage collect gotos which aren't valid ASCII characters
         nMode := AC_IDLE

      OTHERWISE

         IF nKey == 0  // No keystroke
            nMode := AC_IDLE
         ELSE
            nMode := AC_EXCEPT
         ENDIF

      ENDCASE

      IF lUserFunc

         nUserFunc := Do( xUserFunc, nMode, nPos, nPos - nAtTop )
         // DISPVAR nUserFunc

         DO CASE
         CASE nUserFunc == AC_ABORT
            lFinished := .T.
            nPos      := 0
         CASE nUserFunc == AC_SELECT
            lFinished := .T.
         CASE nUserFunc == AC_CONT
            // Do nothing
         CASE nUserFunc == AC_GOTO
            // Do nothing.  The next keystroke won't be read and
            // this keystroke will be processed as a goto.
            nMode := AC_GOTO
         ENDCASE

      ENDIF

   ENDDO

   SetCursor( nSaveCsr )

   RETURN nPos

STATIC PROCEDURE DispPage( acCopy, alSelect, nTop, nLeft, nRight, nNumRows, nPos, nAtTop )

   LOCAL nCntr
   LOCAL nRow                              // Screen row
   LOCAL nIndex                            // Array index
   LOCAL nSaveRow := Row()                 // Position at start of routine
   LOCAL nSaveCol := Col()                 // Position at start of routine
   LOCAL nArrLen  := Len( acCopy )

   DispBegin()

   FOR nCntr := 1 TO nNumRows

      nRow   := nTop + nCntr - 1
      nIndex := nCntr + nAtTop - 1

      IF INRANGE( 1, nIndex, nArrLen )
         DispLine( acCopy[ nIndex ], nRow, nLeft, alSelect[ nIndex ], nIndex == nPos )
      ELSE
         ColorSelect( CLR_STANDARD )
         DispOutAt( nRow, nLeft, Space( nRight - nLeft + 1 ) )
      ENDIF
   NEXT

   DispEnd()

   SetPos( nSaveRow, nSaveCol )

   RETURN

STATIC PROCEDURE DispLine( cLine, nRow, nCol, lSelect, lHiLite )

   ColorSelect( iif( lSelect, ;
                iif( lHiLite, CLR_ENHANCED, CLR_STANDARD ), CLR_UNSELECTED ) )

   DispOutAt( nRow, nCol, cLine )

   ColorSelect( CLR_STANDARD )

   RETURN

