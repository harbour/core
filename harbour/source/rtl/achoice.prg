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

#include "inkey.ch"
#include "setcurs.ch"
#include "set.ch"
#include "achoice.ch"
#include "common.ch"
#include "color.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      ACHOICE()
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
 *      nTop              - topmost row used to display array (default 0)
 *      nLeft             - leftmost row used to display array (default 0)
 *      nBottom           - bottommost row used to display array (default MAXROW())
 *      nRight            - rightmost row used to display array (default MAXCOL())
 *      acMenuItems       - the character array of items from which to select
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
 *      bUserBlock        - a codeblock to be called which may
 *                          effect special processing of keystrokes. It
 *                          should be specified in the form
 *                          {|nMode, nCurElemenet, nRowPos| ;
 *                                 MyFunc(nMode, nCurElemenet, nRowPos) }.
 *                          Default NIL.
 *      nInitialItem      - the number of the element to be highlighted as
 *                          the current item when the array is initally
 *                          displayed.  1 origin.  Default 1.
 *      nWindowRow        - the number of the window row on which the initial
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
 *      ACHOICE() - as supplied in Clipper
 *
 *  $END$
 */

function achoice( nTop, nLft, nBtm, nRyt, acItems, xSelect, xUserFunc, nPos, nHiLytRow )

   local nNumCols                          // Number of columns in the window
   local nNumRows                          // Number of rows in the window
   local acCopy    := {}                   // A padded copy of the items
   local alSelect                          // Select permission
   local nNewPos   := 0                    // The next item to be selected
   local lFinished := .F.                  // Is processing finished?
   local nKey      := 0                    // The keystroke to be processed
   local nMode     := AC_IDLE              // The current operating mode
   local nAtTop    := 1                    // The number of the item at the top
   local nAtBtm    := 1                    // The number of the item at the bottom
   local nItems                            // The number of items
   local nGap      := 0                    // The number of lines between top and current lines
                                           // Block used to search for items
   local lUserFunc                         // Is a user function to be used?
   local nUserFunc := 0                    // Return value from user function
   local nSaveCsr  := setcursor( SC_NONE )
   local nFrstItem := 0
   local nLastItem := 0
   local nCntr
   local bAction
   local cKey

   ColorSelect( CLR_STANDARD )

   lUserFunc := !empty( xUserFunc ) .AND. ValType( xUserFunc ) $ "CB"

   DEFAULT nTop to 0                       // The topmost row of the window
   DEFAULT nLft to 0                       // The leftmost column of the window
   DEFAULT nBtm to maxrow() + 1            // The bottommost row of the windows
   DEFAULT nRyt to maxcol() + 1            // The rightmost column of the window
   DEFAULT acItems to {}                   // The items from which to choose
   DEFAULT xSelect to .T.                  // Array or logical, what is selectable
   DEFAULT nPos to 1                       // The number of the selected item
   DEFAULT nHiLytRow to 0                  // The row to be highlighted

   nNumCols := nRyt - nLft + 1
   nNumRows := nBtm - nTop + 1
   aeval( acItems, { | x, n | if( ISCHARACTER( x ), aadd( acCopy, padr( x, nNumCols ) ), .F. ) } )
   nItems := len( acCopy )

   alSelect := array( nItems )
   IF ISARRAY( xSelect )
      afill( alSelect, .T. )
      for nCntr := 1 to len( xSelect )
         IF nCntr <= nItems
            IF ISCHARACTER( xSelect[ nCntr ] )
               IF empty( xSelect[ nCntr ] )
                  lFinished := .T.
                  nPos      := 0
               else
/* TODO: When macro evaluation will work, this should be commented out:
               alSelect[ nCntr ] := &( xSelect[ nCntr ] )
*/
               endif
            else
               alSelect[ nCntr ] := xSelect[ nCntr ]
            endif
         else
            nCntr := len( xSelect ) + 1
         endif
      next
   else
      afill( alSelect, xSelect )
   endif

   IF !lFinished

      nFrstItem := ascan( alSelect, .T. )  // First valid item

      IF nFrstItem == 0
         nLastItem := 0
         nPos      := 0
         nMode     := AC_NOITEM
      else
         nMode     := AC_IDLE
         nLastItem := nItems               // Last valid item
         do while ( !alSelect[ nLastItem ] )
            nLastItem --
         enddo
      endif

      // Ensure hilighted item can be selected
      nPos := Between( nFrstItem, nPos, nLastItem )

      // Force hilighted row to be valid
      nHiLytRow := Between( 0, nHiLytRow, nNumRows - 1 )

      // Force the topmost item to be a valid index of the array
      nAtTop := Between( 1, max( 1, nPos - nHiLytRow ), nItems )

      // Ensure as much of the selection area as possible is covered
      if ( nAtTop + nNumRows - 1 ) > nItems
         nAtTop := max( 1, nItems - nNumrows + 1 )
      endif

      DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )

   endif

   do while ( !lFinished )

      IF nMode != AC_GOTO .and. nMode != AC_NOITEM
         nKey  := Inkey( 0 )
         nMode := AC_IDLE
      endif

      do case

      case ( bAction := setkey( nKey ) ) <> NIL

         eval( bAction, procname( 1 ), procline( 1 ), "" )
         if empty( nextkey() )
            keyboard chr( 255 )
            inkey()
            nKey := 0
         endif

      case ( ( nKey == K_ESC ) .or. ( nMode == AC_NOITEM ) ) .and. ( !lUserFunc )
         nMode     := AC_ABORT
         nPos      := 0
         lFinished := .T.

      case nKey == K_UP
         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > max( 1, nPos - nNumRows + 1 )
               nAtTop := max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            nNewPos := nPos - 1
            do while !alSelect[ nNewPos ]
               nNewPos --
            enddo
            IF InRange( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            else
               dispbegin()
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               scroll( nTop, nLft, nBtm, nRyt, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos
               nPos   := max( nPos, nAtTop + nNumRows - 1 )
               do while nPos > nNewPos
                  DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
                  nPos --
               enddo
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
               dispend()
            endif
         endif

      case nKey == K_DOWN
         * Find the next selectable item to display
         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < min( nPos, nItems - nNumRows + 1 )
               nAtTop := min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else

            nNewPos := nPos + 1

            do while !alSelect[ nNewPos ]
               nNewPos ++
            enddo

            IF InRange( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            else
               dispbegin()
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               scroll( nTop, nLft, nBtm, nRyt, ( nNewPos - ( nAtTop + nNumRows - 1 ) ) )
               nAtTop := nNewPos - nNumRows + 1
               nPos   := max( nPos, nAtTop )
               do while nPos < nNewPos
                  DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
                  nPos ++
               enddo
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
               dispend()
            endif

         endif

      case nKey == K_CTRL_PGUP .or. ( nKey == K_HOME .and. !lUserFunc )
         IF nPos == nFrstItem
            IF nAtTop == max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            else
               nAtTop := max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            nPos   := nFrstItem
            nAtTop := nPos
            DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
         endif

      case nKey == K_CTRL_PGDN .or. ( nKey == K_END .and. !lUserFunc )
         IF nPos == nLastItem
            IF nAtTop == min( nLastItem, nItems - nNumRows + 1 )
               nMode := AC_HITBOTTOM
            else
               nAtTop := min( nLastItem, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            IF InRange( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            else
               nPos   := nLastItem
               nAtTop := max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         endif

      case nKey == K_CTRL_HOME
         IF nPos == nFrstItem
            IF nAtTop == max( 1, nPos - nNumRows + 1 )
               nMode := AC_HITTOP
            else
               nAtTop := max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            nNewPos := nAtTop
            do while !alSelect[ nNewPos ]
               nNewPos ++
            enddo
            IF nNewPos != nPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            endif
         endif

      case nKey == K_CTRL_END
         IF nPos == nLastItem
            IF nAtTop == min( nPos, nItems - nNumRows + 1 )
               nMode := AC_HITBOTTOM
            else
               nAtTop := min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            nNewPos := nAtTop + nNumRows - 1
            do while !alSelect[ nNewPos ]
               nNewPos --
            enddo
            IF nNewPos != nPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            endif
         endif

      case nKey == K_PGUP
         IF nPos == nFrstItem
            nMode := AC_HITTOP
            IF nAtTop > max( 1, nPos - nNumRows + 1 )
               nAtTop := max( 1, nPos - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            IF InRange( nAtTop, nFrstItem, nAtTop + nNumRows - 1 )
               * On same page as nFrstItem
               nPos   := nFrstItem
               nAtTop := max( nPos - nNumRows + 1, 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            else
               if ( nPos - nNumRows + 1 ) < nFrstItem
                  nPos   := nFrstItem
                  nAtTop := nFrstItem
               else
                  nPos   := max( nFrstItem, nPos - nNumRows + 1 )
                  nAtTop := max( 1, nAtTop - nNumRows + 1 )
                  do while ( nPos > nFrstItem ) .and. ( !alSelect[ nPos ] )
                     nPos --
                     nAtTop --
                  enddo
                  nAtTop := max( 1, nAtTop )
               endif
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         endif

      case nKey == K_PGDN
         IF nPos == nLastItem
            nMode := AC_HITBOTTOM
            IF nAtTop < min( nPos, nItems - nNumRows + 1 )
               nAtTop := min( nPos, nItems - nNumRows + 1 )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         else
            IF InRange( nAtTop, nLastItem, nAtTop + nNumRows - 1 )
               * On the same page as nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nLastItem
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            else
               nGap := nPos - nAtTop
               nPos := min( nLastItem, nPos + nNumRows - 1 )
               if ( nPos + nNumRows - 1 ) > nLastItem
                  * On the last page
                  nAtTop := nLastItem - nNumRows + 1
                  nPos   := min( nLastItem, nAtTop + nGap )
               else
                  * Not on the last page
                  nAtTop := nPos - nGap
               endif
               * Make sure that the item is selectable
               do while ( nPos < nLastItem ) .and. ( !alSelect[ nPos ] )
                  nPos ++
                  nAtTop ++
               enddo
               * Don't leave blank space on the page
               do while ( nAtTop + nNumRows - 1 ) > nItems
                  nAtTop --
               enddo
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         endif

      case ( nKey == K_ENTER ) .and. ( !lUserFunc )
         nMode     := AC_SELECT
         lFinished := .T.

      case ( nKey == K_RIGHT ) .and. ( !lUserFunc )
         nPos      := 0
         lFinished := .T.

      case ( nKey == K_LEFT ) .and. ( !lUserFunc )
         nPos      := 0
         lFinished := .T.

      case InRange( 32, nKey, 255 ) .and. ( ( !lUserFunc ) .or. ( nMode == AC_GOTO ) )

         cKey := upper( chr( nKey ) )

         * Find next selectable item
         FOR nNewPos := nPos + 1 TO nItems
              IF alSelect[ nNewPos ] .AND. left( acCopy[ nNewPos ], 1 ) == cKey
                   EXIT
              ENDIF
         NEXT
         IF nNewPos == nItems + 1
              FOR nNewPos := 1 TO nPos - 1
                   IF alSelect[ nNewPos ] .AND. left( acCopy[ nNewPos ], 1 ) == cKey
                        EXIT
                   ENDIF
              NEXT
         ENDIF

         IF nNewPos != nPos
            IF InRange( nAtTop, nNewPos, nAtTop + nNumRows - 1 )
               * On same page
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .F. )
               nPos := nNewPos
               DispLine( acCopy[ nPos ], nTop + ( nPos - nAtTop ), nLft, alSelect[ nPos ], .T. )
            else
               * On different page
               nPos   := nNewPos
               nAtTop := Between( 1, nPos - nNumRows + 1, nItems )
               DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )
            endif
         endif

         nMode := AC_IDLE

      case ( nMode == AC_GOTO )
         * Garbage collect gotos which aren't valid ASCII characters
         nMode := AC_IDLE

      otherwise
         IF nKey == 0  // No keystroke
            nMode := AC_IDLE
         else
            nMode := AC_EXCEPT
         endif

      endcase

      IF lUserFunc
         nUserFunc := do( xUserFunc, nMode, nPos, nPos - nAtTop )
         // DISPVAR nUserFunc
         do case
         case nUserFunc == AC_ABORT
            lFinished := .T.
            nPos      := 0
         case nUserFunc == AC_SELECT
            lFinished := .T.
         case nUserFunc == AC_CONT
            * Do nothing
         case nUserFunc == AC_GOTO
            * Do nothing.  The next keystroke won't be read and
            * this keystroke will be processed as a goto.
            nMode := AC_GOTO
         endcase
      endif

   enddo

   setcursor( nSaveCsr )

   return nPos

static function DispPage( acCopy, alSelect, nTop, nLft, nNumRows, nPos, nAtTop )

   local nCntr
   local nRow                              // Screen row
   local nIndex                            // Array index
   local nSaveRow := row()                 // Position at start of routine
   local nSaveCol := col()                 // Position at start of routine
   local nArrLen  := len( acCopy )

   dispbegin()
   for nCntr := 1 to nNumRows
      nRow   := nTop + nCntr - 1
      nIndex := nCntr + nAtTop - 1
      IF InRange( 1, nIndex, nArrLen )
         DispLine( acCopy[ nIndex ], nRow, nLft, alSelect[ nIndex ], nIndex == nPos )
      else
         ColorSelect( CLR_STANDARD )
         SetPos( nRow, nLft )
         DispOut( space( len( acCopy[ 1 ] ) ) )
      endif
   next
   dispend()

   setpos( nSaveRow, nSaveCol )

   return NIL

static function DispLine( cLine, nRow, nCol, lSelect, lHiLyt )

   ColorSelect( if( lSelect, ;
                if( lHiLyt, CLR_ENHANCED, CLR_STANDARD ), CLR_UNSELECTED ) )

   SetPos( nRow, nCol )
   DispOut( cLine )

   ColorSelect( CLR_STANDARD )

   return NIL

static function InRange( xLo, xVal, xHi )
   return ( xVal >= xLo ) .and. ;
          ( xVal <= xHi )

static function Between( xLo, xVal, xHi )
   return min( max( xLo, xVal ), xHi )

