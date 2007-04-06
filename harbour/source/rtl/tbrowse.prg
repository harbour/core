/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBrowse Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

 /*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000, '01, '02 Maurilio Longo <maurilio.longo@libero.it>
 * Cursor movement handling, stabilization loop, multi-line headers and footers support
 * ::PageUp(), ::PageDown(), ::Down(), ::Up(), ::GoBottom(), ::GoTop(), ::Stabilize()
 * ::GotoXY(), ::DispCell(), ::WriteMLineText(), ::RedrawHeaders(),
 * ::SetFrozenCols(), ::SetColumnWidth()
 *
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell(nColumn, nColor)
 *
 */


/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible.
         ADDITION: Same goes for DevPos(), always use SetPod() instead.
         [vszakats] */

/* TODO: :firstScrCol() --> nScreenCol
         Determines screen column where the first table column is displayed.
         Xbase++ compatible method */

/* TODO: :viewArea() --> aViewArea
         Determines the coordinates for the data area of a TBrowse object.
         Xbase++ compatible method */

#include "common.ch"
#include "hbclass.ch"
#include "color.ch"
#include "error.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "button.ch"
#include "tbrowse.ch"

#define _DISPCELL_CLR_UNSELECTED        1
#define _DISPCELL_CLR_SELECTED          2

CLASS TBrowse

   DATA aColumns              // Array to hold all browse columns
   DATA autoLite              // Logical value to control highlighting
   DATA cargo                 // User-definable variable
   DATA colorSpec             // Color table for the TBrowse display
   DATA colPos                // Current cursor column position
   DATA colSep                // Column separator character
   DATA footSep               // Footing separator character
   DATA goBottomBlock         // Code block executed by TBrowse:goBottom()
   DATA goTopBlock            // Code block executed by TBrowse:goTop()
   DATA headSep               // Heading separator character
   DATA hitBottom             // Indicates the end of available data
   DATA hitTop                // Indicates the beginning of available data
   DATA leftVisible           // Indicates position of leftmost unfrozen column in display
   DATA nBottom               // Bottom row number for the TBrowse display
   DATA nLeft                 // Leftmost column for the TBrowse display
   DATA nRight                // Rightmost column for the TBrowse display
   DATA nTop                  // Top row number for the TBrowse display
   DATA rightVisible          // Indicates position of rightmost unfrozen column in display
   DATA rowCount              // Number of visible data rows in the TBrowse display
   DATA rowPos                // Current cursor row position
   ACCESS skipBlock INLINE ::bSkipBlock
   ASSIGN skipBlock( b ) INLINE ::SetSkipBlock( b )
   DATA stable                // Indicates if the TBrowse object is stable

#ifdef HB_COMPAT_C53
   DATA nRow                  // Row number for the actual cell
   DATA nCol                  // Col number for the actual cell
   DATA aKeys
   DATA mColpos, mRowPos, message
#endif

   ACCESS freeze INLINE ::nFrozenCols                       // Number of columns to freeze/frozen
   ASSIGN freeze( nHowMany ) INLINE ::SetFrozenCols( nHowMany ), ::nFrozenCols

   METHOD New( nTop, nLeft, nBottom, nRight )               // Constructor
   METHOD Down()                                            // Moves the cursor down one row
   METHOD End()                                             // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()                                        // Repositions the data source to the bottom of file
   METHOD GoTop()                                           // Repositions the data source to the top of file
   METHOD Home()                                            // Moves the cursor to the leftmost visible data column
   MESSAGE Left() METHOD _Left()                            // Moves the cursor left one column
   METHOD PageDown()                                        // Repositions the data source downward
   METHOD PageUp()                                          // Repositions the data source upward
   METHOD PanEnd()                                          // Moves the cursor to the rightmost data column
   METHOD PanHome()                                         // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()                                         // Pans left without changing the cursor position
   METHOD PanRight()                                        // Pans right without changing the cursor position
   MESSAGE Right() METHOD _Right()                          // Moves the cursor right one column
   METHOD Up()                                              // Moves the cursor up one row

   METHOD AddColumn( oCol )
   METHOD DelColumn( nPos )                                 // Delete a column object from a browse
   METHOD InsColumn( nPos, oCol )                           // Insert a column object in a browse
   METHOD GetColumn( nColumn )                              // Gets a specific TBColumn object
   METHOD SetColumn( nColumn, oCol )                        // Replaces one TBColumn object with another
   METHOD ColWidth( nColumn )                               // Returns the display width of a particular column
   METHOD ColCount() INLINE ::nColumns
   METHOD ColorRect()                                       // Alters the color of a rectangular group of cells
   METHOD Configure( nMode )                                // Reconfigures the internal settings of the TBrowse object
                                                            // nMode is an undocumented parameter in CA-Cl*pper
   METHOD DeHilite()                                        // Dehighlights the current cell
   METHOD ForceStable()                                     // Performs a full stabilization
   METHOD Hilite()                                          // Highlights the current cell
   METHOD Invalidate()                                      // Forces entire redraw during next stabilization
   METHOD RefreshAll()                                      // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent() INLINE;                          // Causes the current row to be refilled and repainted on next stabilize
          If( ! Empty( ::aRedraw ), ::aRedraw[ ::RowPos ] := .T., ), ::stable := .F., Self
   METHOD Stabilize()                                       // Performs incremental stabilization

#ifdef HB_COMPAT_C53
   METHOD SetKey( nKey, bBlock )
   METHOD ApplyKey( nKey )
   METHOD InitKeys( Self )
   METHOD TApplyKey( nKey, o )
   METHOD HitTest( nMouseRow, nMouseCol )
   METHOD SetStyle( nMode, lSetting )
#endif

   PROTECTED:     /* P R O T E C T E D */

   METHOD MGotoYX( nRow, nCol )           // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                          // _M_GotoXY because this method will mostly be called to handle mouse requests

   HIDDEN:         /* H I D D E N */

   METHOD PosCursor()                     // Positions the cursor to the beginning of the call, used only when autolite==.F.
   METHOD LeftDetermine()                 // Determine leftmost unfrozen column in display
   METHOD DispCell( nRow, nCol, nMode )   // Displays a single cell and returns cell type as a single letter like Valtype()
   METHOD HowManyCol()                    // Counts how many cols can be displayed
   METHOD RedrawHeaders()                 // Repaints TBrowse Headers
   METHOD Moved()                         // Every time a movement key is issued I need to reset certain properties
                                          // of TBrowse, I do these settings inside this method

   METHOD WriteMLineText(cStr, nPadLen, lHeader, cColor) // Writes a multi-line text where ";" is a line break, lHeader
                                                         // is .T. if it is a header and not a footer
   METHOD SetFrozenCols( nHowMany )       // Handles freezing of columns
   METHOD SetColumnWidth( oCol )          // Calcs width of given column
   METHOD SetSkipBlock( b )               // Sets ::skipBlock()

   DATA aRect                             // The rectangle specified with ColorRect()
   DATA aRectColor                        // The color positions to use in the rectangle specified with ColorRect()
   DATA aRedraw                           // Array of logical items indicating, is appropriate row need to be redraw
   DATA aColsWidth                        // Array with width of TBrowse's columns
   DATA lHeaders                          // Internal variable which indicates whether there are column footers to paint
   DATA lFooters                          // Internal variable which indicates whether there are column footers to paint
   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA nColsWidth                        // Total width of visible columns plus ColSep
   DATA nColsVisible                      // Number of columns that fit on the browse width
   DATA lHitTop                           // Internal Top/Bottom reached flag
   DATA lHitBottom
   DATA nRecsToSkip                       // Recs to skip on next Stabilize()
   DATA nNewRowPos                        // Next position of data source (after first phase of stabilization)
   DATA nLastRetrieved                    // Position, relative to first row, of last retrieved row (with an Eval(::bSkipBlock, n))

   DATA nHeaderHeight                     // How many lines is highest Header/Footer and so how many lines of
   DATA nFooterHeight                     // screen space I have to reserve
   DATA nFrozenWidth                      // How many screen column are not available on the left side of TBrowse display
                                          // > 0 only when there are frozen columns
   DATA bSkipBlock                        // Code block used to reposition data source
   DATA nFrozenCols                       // Number of frozen columns on left side of TBrowse
   DATA nColumns                          // Number of columns added to TBrowse
   DATA lNeverDisplayed                   // .T. if TBrowse has never been stabilized()
#ifdef HB_COMPAT_C53
   DATA rect
   DATA aVisibleCols
   DATA aSetStyle
#endif
ENDCLASS


METHOD New( nTop, nLeft, nBottom, nRight ) CLASS TBrowse

   default  nTop    to 0
   default  nLeft   to 0
   default  nBottom to MaxRow()
   default  nRight  to MaxCol()

   ::aColumns        := {}
   ::aColsWidth      := {}
   ::AutoLite        := .T.
   ::leftVisible     := 1
   ::ColPos          := 1
   ::HitBottom       := .F.
   ::HitTop          := .F.
   ::lHitTop         := .F.
   ::lHitBottom      := .F.
   ::ColorSpec       := SetColor()
   ::ColSep          := " "
   ::FootSep         := ""
   ::HeadSep         := ""
   ::RowPos          := 1
   ::nNewRowPos      := 1
   ::bSkipBlock      := {|| NIL }
   ::stable          := .F.
   ::nLastRetrieved  := 1
   ::nRecsToSkip     := 0
   ::aRedraw         := {}
   ::lHeaders        := .F.
   ::lFooters        := .F.
   ::lRedrawFrame    := .T.
   ::aRect           := {}
   ::aRectColor      := {}
   ::nColsWidth      := 0
   ::nColsVisible    := 0
   ::nHeaderHeight   := 1
   ::nFooterHeight   := 1
   ::nFrozenWidth    := 0
   ::nFrozenCols     := 0
   ::nColumns        := 0
   ::lNeverDisplayed := .T.

   ::nTop            := nTop
   ::nLeft           := nLeft
   ::nBottom         := nBottom
   ::nRight          := nRight

   #ifdef HB_COMPAT_C53

      ::mColPos         := 0
      ::mRowPos         := 0
      ::rect            := { nTop, nLeft, nBottom, nRight }
      ::aVisibleCols    := {}
      ::message         := ''
      ::nRow            := 0
      ::nCol            := 0
      ::aSetStyle       := Array( TBR_CUSTOM - 1 )

      ::aSetStyle[ TBR_APPEND ]    := .f.
      ::aSetStyle[ TBR_APPENDING ] := .f.
      ::aSetStyle[ TBR_MODIFY ]    := .f.
      ::aSetStyle[ TBR_MOVE ]      := .f.
      ::aSetStyle[ TBR_SIZE ]      := .f.

   #endif

return Self


METHOD Invalidate() CLASS TBrowse

   AFill( ::aRedraw, .T. )
   ::stable := .F.
   ::lRedrawFrame := .T.

return Self


METHOD RefreshAll() CLASS TBrowse

   AFill( ::aRedraw, .T. )
   ::stable := .F.

return Self


METHOD Configure( nMode ) CLASS TBrowse

   local n, nHeight
   local nLeft, nRight

   ::lHeaders := .F.
   ::lFooters := .F.
   ::lRedrawFrame := .T.

   // Are there column headers to paint ?
   for n := 1 to ::nColumns
      if ! Empty( ::aColumns[n]:Heading )
         ::lHeaders := .T.
         exit
      endif
   next

   // Are there column footers to paint ?
   for n := 1 to ::nColumns
      if ! Empty( ::aColumns[n]:Footing )
         ::lFooters := .T.
         exit
      endif
   next

   ::nHeaderHeight := 1
   ::nFooterHeight := 1

   // Find out highest header and footer
   for n := 1 to ::nColumns

      if ::lHeaders .AND. ! Empty( ::aColumns[n]:Heading )
         nHeight := Len( ::aColumns[n]:Heading ) - Len( StrTran( ::aColumns[n]:Heading, ";" ) ) + 1

         if nHeight > ::nHeaderHeight
            ::nHeaderHeight := nHeight
         endif

      endif

      if ::lFooters .AND. ! Empty( ::aColumns[n]:Footing )
         nHeight := Len( ::aColumns[n]:Footing ) - Len( StrTran( ::aColumns[n]:Footing, ";" ) ) + 1

         if nHeight > ::nFooterHeight
            ::nFooterHeight := nHeight
         endif

      endif
   next

   IF nMode == NIL
      for n := 1 to ::nColumns
         ::aColsWidth[n] := ::SetColumnWidth( ::aColumns[n] )
      next
   ENDIF

   // 20/nov/2000 - maurilio.longo@libero.it
   // If I add (or remove) header or footer (separator) I have to change number
   // of available rows
   ::RowCount := ::nBottom - ::nTop + 1 - ;
                  iif( ::lHeaders, ::nHeaderHeight + iif( Empty( ::HeadSep ), 0, 1 ), 0 ) - ;
                  iif( ::lFooters, ::nFooterHeight + iif( Empty( ::FootSep ), 0, 1 ), 0 )

   if Len( ::aRedraw ) <> ::RowCount
      ASize( ::aRedraw, ::RowCount )
   endif

   ::Invalidate()

   // Force re-evaluation of space occupied by frozen columns
   if ::freeze > 0
      ::SetFrozenCols( ::freeze )
   endif

   #ifdef HB_COMPAT_C53

      nLeft := ::nLeft
      nRight := ::nRight
      ::rect := { ::nTop + ::nHeaderHeight + If( Empty( ::HeadSep ), 0, 1 ), ::nLeft, ::nBottom - ::nFooterHeight - If( Empty( ::FootSep ), 0, 1 ), ::nRight }
      For n := nLeft To nRight
         AAdd( ::aVisibleCols, n )
      Next

   #endif

return Self


// Adds a TBColumn object to the TBrowse object
METHOD AddColumn( oCol ) CLASS TBrowse

   AAdd( ::aColumns, oCol )
   AAdd( ::aColsWidth, ::SetColumnWidth( oCol ) )
   ::nColumns++
   ::Configure( 2 )

return Self


// Insert a column object in a browse
METHOD InsColumn( nPos, oCol )

   ASize( ::aColumns, ++::nColumns )
   AIns( ::aColumns, nPos )
   ASize( ::aColsWidth, ::nColumns )
   AIns( ::aColsWidth, nPos )

   ::aColumns[ nPos ] := oCol
   ::aColsWidth[ nPos ] := ::SetColumnWidth( oCol )
   ::Configure( 2 )

return oCol


// Gets a specific TBColumn object
METHOD GetColumn( nColumn )

return iif( 0 < nColumn .AND. nColumn <= ::nColumns, ::aColumns[ nColumn ], NIL )


// Replaces one TBColumn object with another
METHOD SetColumn( nColumn, oCol )

   if 0 < nColumn .AND. nColumn <= ::nColumns
       ::aColumns[ nColumn ] := oCol
       ::aColsWidth[ nColumn ] := ::SetColumnWidth( oCol )
       ::Configure( 2 )
   endif

return oCol


// Returns the display width of a particular column
METHOD ColWidth( nColumn )

return iif( 0 < nColumn .AND. nColumn <= ::nColumns, ::aColsWidth[ nColumn ], 0 )


METHOD DelColumn( nPos ) CLASS TBrowse

   local oCol := ::aColumns[ nPos ]

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, --::nColumns )
   ADel( ::aColsWidth, nPos )
   ASize( ::aColsWidth, ::nColumns )

   ::Configure( 2 )

return oCol


METHOD SetFrozenCols( nHowMany ) CLASS TBrowse

   LOCAL nCol
   LOCAL nWidth := ::nRight - ::nLeft + 1    // Visible width of the browse

   ::nFrozenCols := nHowMany
   // Space inside TBrowse window reserved for frozen columns
   ::nFrozenWidth := 0

   // If I've never displayed this TBrowse before I cannot calc occupied space since
   // columns:width is not yet set, ::Stabilize() will call me later
   if ! ::lNeverDisplayed

      if nHowMany > 0
         for nCol := 1 to nHowMany
            ::nFrozenWidth += ::aColsWidth[ nCol ]
            if nCol < ::nColumns
               ::nFrozenWidth += iif( ::aColumns[ nCol + 1 ]:ColSep != NIL,;
                                      Len( ::aColumns[ nCol + 1 ]:ColSep ),;
                                      Len( ::ColSep ) )
            endif
         next
      endif

      for nCol := 1 to ::nColumns
         if nHowMany > 0
            // If there are columns which are larger than TBrowse display width minus
            // frozen columns reserved space, shrihnk them to fit
            if ::nFrozenWidth + ::aColsWidth[ nCol ] > nWidth
               ::aColsWidth[ nCol ] := nWidth - ::nFrozenWidth
            endif

         else
            // Reset column widths
            ::aColsWidth[ nCol ] := ::SetColumnWidth( ::aColumns[ nCol ] )
         endif
      next
   endif

return nHowMany


METHOD SetColumnWidth( oCol ) CLASS TBrowse

   LOCAL xRes, cType, nTokenPos := 0, nL, cHeading
   LOCAL nWidthMax := ::nRight - ::nLeft + 1    // Visible width of TBrowse
   LOCAL nWidth := 0, nColWidth := 0, nLen := 0


   // if oCol has :Width property set I use it
   if oCol:Width <> nil
      nWidth := Min( oCol:Width, nWidthMax )

   else
      if ISBLOCK( oCol:block )

         cType := Valtype(xRes := Eval( oCol:block ) )

         do case
            case cType == "N"
               nLen := Len( Str( xRes ) )

            case cType == "L"
               nLen := 1

            case cType == "C"
               nLen := Len( xRes )

            case cType == "D"
               nLen := Len( DToC( xRes ) )

            otherwise
               nLen := 0

         endcase

         cHeading := oCol:Heading
         while ( nL := Len( hb_TokenPtr( @cHeading, @nTokenPos, ";" ) ) ) > 0
            nColWidth := Max( nL, nColWidth )
         enddo
      endif

      nWidth:= Min( iif( nColwidth > nLen, nColwidth, nLen ), nWidthMax )

   endif

return nWidth


METHOD Down() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := 1

return Self


METHOD Up() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := -1

return Self


METHOD End() CLASS TBrowse

   ::Moved()

   if ::ColPos < ::rightVisible
      ::ColPos := ::rightVisible
      ::lRedrawFrame := .T.
      ::RefreshCurrent()
   endif

return Self


METHOD GoBottom() CLASS TBrowse

   local nToTop

   ::Moved()

   Eval( ::goBottomBlock )
   // Skip back from last record as many records as TBrowse can hold
   nToTop := Abs( Eval( ::bSkipBlock, - ( ::RowCount - 1 ) ) )
   // From top of TBrowse new row position is nToTop + 1 records away
   ::nNewRowPos := nToTop + 1
   // Last read record is first record inside TBrowse
   ::nLastRetrieved := 1
   ::RefreshAll()

return Self


METHOD GoTop() CLASS TBrowse

   ::Moved()

   Eval( ::goTopBlock )
   ::nLastRetrieved := 1
   ::nNewRowPos := 1
   ::RefreshAll()

return Self


METHOD Home() CLASS TBrowse

   ::Moved()

   if ::ColPos != ::leftVisible
      ::ColPos := ::leftVisible
      ::lRedrawFrame := .T.
      ::RefreshCurrent()
   endif

return Self


METHOD _Right() CLASS TBrowse

   ::Moved()

   if ::ColPos < ::rightVisible
      ::ColPos++
   else
      if ::ColPos < ::nColumns
         ::rightVisible++
         ::leftVisible := ::LeftDetermine()
         ::ColPos++
         ::lRedrawFrame := .T.
         ::RefreshAll()
      endif
   endif

return Self


METHOD _Left() CLASS TBrowse

   local leftVis := ::leftVisible

   ::Moved()

   if ::ColPos > ::leftVisible
      ::ColPos--
   else
      if ::ColPos <= Max( ::leftVisible, ::nFrozenCols ) .AND. ::ColPos > 1
         while leftVis == ::leftVisible
            ::rightVisible--
            ::leftVisible := ::LeftDetermine()
         end
         ::ColPos--
         ::lRedrawFrame := .T.
         ::RefreshAll()
      endif
   endif

return Self


METHOD LeftDetermine() CLASS TBrowse

   local nWidthMax := ::nRight - ::nLeft + 1  // Visible width of the browse
   local nWidth := ::nFrozenWidth
   local nCol

   nCol := ::rightVisible
   while nWidth < nWidthMax .and. nCol > ::nFrozenCols

      nWidth += ::aColsWidth[ nCol ] +;
                iif( ::aColumns[ nCol ]:ColSep != NIL,;
                     Len( ::aColumns[ nCol ]:ColSep ),;
                     Len( ::ColSep ) )

      if nWidth < nWidthMax
         nCol--
      endif
   enddo

return Min( nCol + 1, ::nColumns )


METHOD PageDown() CLASS TBrowse

   ::Moved()

   ::nRecsToSkip := ( ::RowCount - ::RowPos ) + ::RowCount

return Self


METHOD PageUp() CLASS TBrowse

   ::Moved()

   ::nRecsToSkip := - ( ( ::RowPos - 1 ) + ::RowCount )

return Self


METHOD PanEnd() CLASS TBrowse

   ::Moved()

   if ::ColPos < ::nColumns
      if ::rightVisible < ::nColumns
         ::rightVisible := ::nColumns
         ::leftVisible := ::LeftDetermine()
         ::ColPos := ::rightVisible
         ::lRedrawFrame := .T.
         ::RefreshAll()
      else
         ::ColPos := ::rightVisible
         ::RefreshCurrent()
      endif
   endif

return Self


METHOD PanHome() CLASS TBrowse

   ::Moved()

   if ::ColPos > 1
      if ::leftVisible > ::nFrozenCols + 1
         ::leftVisible := ::nFrozenCols + 1
         ::ColPos := 1
         ::RefreshAll()
         ::lRedrawFrame := .T.
      else
         ::ColPos := 1
         ::RefreshCurrent()
      endif
   endif

return Self


METHOD PanLeft() CLASS TBrowse

   local n := ::ColPos - ::leftVisible
   local leftVis := ::leftVisible

   ::Moved()

   if ::leftVisible > ::nFrozenCols + 1
      while leftVis == ::leftVisible
         ::rightVisible--
         ::leftVisible := ::LeftDetermine()
      end
      ::ColPos := Min( ::leftVisible + n, ::rightVisible )
      ::lRedrawFrame := .T.
      ::RefreshAll()
   endif

return Self


METHOD PanRight() CLASS TBrowse

   local n := ::ColPos - ::leftVisible

   ::Moved()

   if ::rightVisible < ::nColumns
      ::rightVisible++
      ::leftVisible := ::LeftDetermine()
      ::ColPos := Min( ::leftVisible + n, ::rightVisible )
      ::lRedrawFrame := .T.
      ::RefreshAll()
   endif

return Self


METHOD DeHilite() CLASS TBrowse

   local nRow := ::nTop + ::RowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. ! ::lHeaders, 0, 1 ) - 1
   local cType

   SetPos( nRow, ::aColumns[ ::ColPos ]:ColPos )

   cType := ::DispCell( ::RowPos, ::ColPos, _DISPCELL_CLR_UNSELECTED )

   SetPos(nRow, ::aColumns[ ::ColPos ]:ColPos + iif( cType == "L", ::aColsWidth[::ColPos] / 2, 0 ) )

return Self


METHOD ForceStable() CLASS TBrowse

   while !::Stabilize()
   end

return Self


METHOD Hilite() CLASS TBrowse

   local nRow, nCol
   local cType

   nRow := ::nTop + ::RowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. ! ::lHeaders, 0, 1 ) - 1
   nCol := ::aColumns[ ::ColPos ]:ColPos

   // Start of cell
   SetPos( nRow, nCol )

   cType := ::DispCell( ::RowPos, ::ColPos, _DISPCELL_CLR_SELECTED )
   nCol  += iif( cType == "L", ::aColsWidth[::ColPos] / 2, 0 )

   // Put cursor back on first char of cell value
   SetPos( nRow, nCol )

   #ifdef HB_COMPAT_C53
      ::nRow := nRow
      ::nCol := nCol
   #endif

return Self


METHOD PosCursor() CLASS TBrowse

   local nRow := ::nTop + ::RowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. ! ::lHeaders, 0, 1 ) - 1
   local nCol
   local cType := ValType( Eval( ::aColumns[ ::ColPos ]:block ) )

   nCol := ::aColumns[ ::ColPos ]:ColPos + iif( cType == "L", ::aColsWidth[::ColPos] / 2, 0 )

   // Put cursor on first char of cell value
   SetPos( nRow, nCol )

   #ifdef HB_COMPAT_C53
      ::nRow := nRow
      ::nCol := nCol
   #endif

return Self


// Calculate how many columns fit on the browse width including ColSeps
METHOD HowManyCol() CLASS TBrowse

   local nToAdd
   local nWidth := ::nRight - ::nLeft + 1

   // They were locals, so now I need to clear them (should fix this)
   ::nColsWidth := 0
   ::nColsVisible := 0

   if ::nFrozenCols > 0
      if ::leftVisible <= ::nFrozenCols
         ::leftVisible := ::nFrozenCols + 1
      endif

      ::nColsVisible := 0
      while ::nColsVisible < ::nFrozenCols

         nToAdd := ::aColsWidth[ ::nColsVisible + 1 ]

         if ::nColsVisible >= 1 .and. ::nColsVisible < ::nColumns
            nToAdd += iif( ::aColumns[ ::nColsVisible + 1 ]:ColSep != NIL,;
                           Len( ::aColumns[ ::nColsVisible + 1 ]:ColSep ),;
                           Len( ::ColSep ) )
         endif

         if ::nColsWidth + nToAdd > nWidth
            exit
         endif

         ::nColsWidth += nToAdd
         ::nColsVisible++
      enddo


      if ::nColsWidth > nWidth
         /* NOTE: Why do I change frozen columns here? */
         ::freeze := 0
         ::nColsWidth := 0
      endif
   endif

   ::nColsVisible := ::leftVisible - 1

   while ::nColsVisible < ::nColumns

      nToAdd := ::aColsWidth[ ::nColsVisible + 1 ]

      if ::nColsVisible >= ::leftVisible .or. ::nFrozenCols > 0
         nToAdd += iif( ::aColumns[ ::nColsVisible + 1 ]:ColSep != NIL,;
                        Len( ::aColumns[ ::nColsVisible + 1 ]:ColSep ),;
                        Len( ::ColSep ) )
      endif

      if ::nColsWidth + nToAdd > nWidth
         exit
      endif

      ::nColsWidth += nToAdd
      ::nColsVisible++
   enddo

   ::rightVisible := ::nColsVisible

return Self


// Gets TBrowse width and width of displayed columns plus colsep
METHOD RedrawHeaders() CLASS TBrowse

   local n, nTPos, nBPos
   local cBlankBox := Space(9)
   local nScreenRowT
   local nScreenRowB
   local nLCS             // Len(ColSep)
   local nWidth := ::nRight - ::nLeft + 1

   if ::lHeaders          // Drawing headers

      // Clear area of screen occupied by headers
      DispBox( ::nTop, ::nLeft, ::nTop + ::nHeaderHeight - 1, ::nRight, cBlankBox, ::ColorSpec )

      // Set cursor at first field start of description
      SetPos( ::nTop, ::nLeft + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Heading, ::aColsWidth[ n ], .T., hb_ColorIndex( ::ColorSpec, ::aColumns[ n ]:defColor[ _DISPCELL_CLR_UNSELECTED ] - 1 ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[n + 1]:ColSep != NIL, Len( ::aColumns[n + 1]:ColSep ), Len( ::ColSep ) ) )
         endif
      next
   endif

   if ! Empty( ::HeadSep ) .AND. ::lHeaders //Draw horizontal heading separator line
      DispOutAt( ( nScreenRowT := ::nTop + ::nHeaderHeight ), ::nLeft,;
                Replicate( Right( ::HeadSep, 1 ), nWidth ), ::ColorSpec )
   else
      nScreenRowT := NIL
   endif

   if ! Empty( ::FootSep ) .AND. ::lFooters //Draw horizontal footing separator line
      DispOutAt( ( nScreenRowB := ::nBottom - ::nFooterHeight ), ::nLeft,;
                Replicate( Right( ::FootSep, 1 ), nWidth ), ::ColorSpec )
   else
      nScreenRowB := NIL
   endif

   nTPos := nBPos := ::nLeft + (( nWidth - ::nColsWidth ) / 2 )

   // Draw headin/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
      if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
         n := ::leftVisible
      endif

      if n < ::rightVisible

         nLCS := iif( ::aColumns[n + 1]:ColSep != NIL, Len( ::aColumns[n + 1]:ColSep ), Len( ::ColSep ) )

         if nScreenRowT != NIL
            DispOutAt( nScreenRowT, ( nTPos += ::aColsWidth[ n ] ), Left( ::HeadSep, nLCS ), ::ColorSpec )
            nTPos += nLCS
         endif

         if nScreenRowB != NIL
            DispOutAt( nScreenRowB, ( nBPos += ::aColsWidth[ n ] ), Left( ::FootSep, nLCS ), ::ColorSpec )
            nBPos += nLCS
         endif

      endif
   next

   if ::lFooters                // Drawing footers

      // Clear area of screen occupied by footers
      DispBox( ::nBottom - ::nFooterHeight + 1, ::nLeft, ::nBottom, ::nRight, cBlankBox, ::ColorSpec )

      // Set cursor at first field start of description
      SetPos( ::nBottom, ::nLeft + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Footing, ::aColsWidth[ n ], .F., hb_ColorIndex( ::ColorSpec, ::aColumns[ n ]:defColor[ _DISPCELL_CLR_UNSELECTED ] - 1 ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[n + 1]:ColSep != NIL, Len( ::aColumns[n + 1]:ColSep ), Len( ::ColSep ) ) )
         endif
      next
   endif

return Self


METHOD Stabilize() CLASS TBrowse

   local nRow, n
   local nWidth := ::nRight - ::nLeft + 1 // Visible width of the browse
   local oStartCol, oEndCol
   local lDisplay                      // Is there something to show inside current cell?
   local nRecsSkipped                  // How many records do I really skipped?
   local nFirstRow                     // Where is on screen first row of TBrowse?
   local nOldCursor                    // Current shape of cursor (which I remove before stabilization)


   // I need to set columns width If TBrowse was never displayed before
   if ::lNeverDisplayed
      ::Configure( 0 )
      //AEVal(::aColumns, {|oCol| ::SetColumnWidth(oCol)} )

      // NOTE: It must be before call to ::SetFrozenCols() since this call
      //       tests this iVar value, and I set it to .F. since I'm going to display TBrowse
      //       for first time
      ::lNeverDisplayed := .F.

      // Force re-evaluation of frozen space since I could not calc it before
      // being columns width not set
      if ::freeze > 0
         ::SetFrozenCols( ::freeze )
      endif
   endif

   nOldCursor := SetCursor( SC_NONE )

   if ::lRedrawFrame
      // How may columns fit on TBrowse width?
      ::HowManyCol()
      ::RedrawHeaders()

      // Now that browser frame has been redrawn we don't need to redraw it unless
      // displayed columns change
      ::lRedrawFrame := .F.

   else
      oStartCol := ::aColumns[ iif( ::rightVisible != 0, ::rightVisible, 1 ) ]
      oEndCol := ::aColumns[ iif( ::nFrozenCols > 0, 1, ::leftVisible ) ]
      ::nColsWidth := iif( oStartCol != NIL, oStartCol:ColPos, 0 ) + ;
        iif( oStartCol != NIL, ::aColsWidth[ iif( ::rightVisible != 0, ::rightVisible, 1 ) ], 0 ) - oEndCol:ColPos

   endif

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::stable state to .T.
   if !::stable

      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      // If I have a requested movement still to handle
      if ::nRecsToSkip <> 0

         // If I'm not under cursor (maybe I've interrupted an ongoing stabilization) I have to set data source to cursor position
         if ::nLastRetrieved <> ::nNewRowPos
            Eval( ::bSkipBlock, ::nNewRowPos - ::nLastRetrieved )
            ::nLastRetrieved := ::nNewRowPos
         endif

         nRecsSkipped := Eval( ::bSkipBlock, ::nRecsToSkip )

         // I've tried to move past top or bottom margin
         if nRecsSkipped == 0

            if ::nRecsToSkip > 0
               ::lHitBottom := .T.

            elseif ::nRecsToSkip < 0
               ::lHitTop := .T.

            // else ::nRecsToSkip == 0
               //
            endif

         elseif nRecsSkipped == ::nRecsToSkip

            // If after movement I'm still inside present TBrowse
            if ( ::nNewRowPos + nRecsSkipped >= 1 ) .AND. ( ::nNewRowPos + nRecsSkipped <= ::RowCount )
               ::nNewRowPos += nRecsSkipped
               ::nLastRetrieved := ::nNewRowPos
               // This is needed since present TBrowse has no cache, so I need to repaint current row
               // rereading it from data source and to force rereading from data source I have to mark
               // row as invalid
               ::aRedraw[::nNewRowPos] := .T.

            else
               // It was K_PGDN or K_PGUP
               if Abs( nRecsSkipped ) >= ::RowCount

                  // K_PGDN
                  if nRecsSkipped > 0
                     ::nLastRetrieved := ::RowCount

                  else // K_PGUP
                     ::nLastRetrieved := 1

                  endif
                  ::RefreshAll()

               else // K_DN or K_UP

                  // Where does really start first TBrowse row?
                  nFirstRow := ::nTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. !::lHeaders, 0, 1 )

                  // I'm at top or bottom of TBrowse so I can scroll
                  if ::nNewRowPos == ::RowCount
                     Scroll( nFirstRow + nRecsSkipped - 1, ::nLeft, nFirstRow + ::RowCount - 1, ::nRight, nRecsSkipped )
                     ::nLastRetrieved := ::RowCount

                  else
                     Scroll( nFirstRow, ::nLeft, nFirstRow + ::RowCount + nRecsSkipped, ::nRight, nRecsSkipped )
                     ::nLastRetrieved := 1

                  endif

                  // I've scrolled on screen rows, now I need to scroll ::aRedraw array as well!
                  if nRecsSkipped > 0
                     ADel( ::aRedraw, 1 )
                  else
                     AIns( ::aRedraw, 1 )
                  endif

                  ::aRedraw[::nNewRowPos] := .T.
               endif
            endif

         else // I couldn't move as far as requested

            // I need to refresh all rows if I go past current top or bottom row
            if ( ::nNewRowPos + nRecsSkipped < 1 ) .OR. ( ::nNewRowPos + nRecsSkipped > ::RowCount )
               // don't go past boundaries
               ::nNewRowPos := iif( nRecsSkipped > 0, ::RowCount, 1 )
               ::RefreshAll()

            else
               ::nNewRowPos += nRecsSkipped
               ::aRedraw[::nNewRowPos] := .T.

            endif

            ::nLastRetrieved := ::nNewRowPos

         endif

         // Data source moved, so next time I won't enter this stage of stabilization
         ::nRecsToSkip := 0

         // Exit first stage of stabilization
         SetCursor( nOldCursor )
         return .F.

      endif

      // Data source is alredy at correct record number, now we need
      // to repaint browser accordingly.
      for nRow := 1 to ::RowCount

         // if there is a row to repaint
         if ::aRedraw[nRow]

            DispOutAt( ::nTop + nRow + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. !::lHeaders, 0, 1 ) - 1, ::nLeft,;
                      Space( ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )

            for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible

               if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
                  n := ::leftVisible
               endif

               if nRow == 1
                  ::aColumns[ n ]:ColPos := Col()
               endif

               // NOTE: If my TBrowse has 20 rows but I have only 3 recs, clipper clears
               //       remaining 17 rows in a single operation, I will, instead, try to skip
               //       17 times. Should be made more clever.
               if nRow <> ::nLastRetrieved
                  if lDisplay := Eval( ::bSkipBlock, nRow - ::nLastRetrieved ) == ( nRow - ::nLastRetrieved )
                     ::nLastRetrieved := nRow
                  endif
               else
                  lDisplay := .T.
               endif

               if lDisplay
                  ::DispCell( nRow, n, _DISPCELL_CLR_UNSELECTED )
               else
                  // Clear cell
                  DispOut( Space( ::aColsWidth[ n ] ), hb_ColorIndex( ::ColorSpec, ::aColumns[ n ]:defColor[ _DISPCELL_CLR_UNSELECTED ] - 1 ) )
               endif

               if n < ::rightVisible
                  if ::aColumns[ n + 1 ]:ColSep != NIL
                     DispOut( ::aColumns[ n + 1 ]:ColSep, ::ColorSpec )

                  elseif ::ColSep != NIL
                     DispOut( ::ColSep, ::ColorSpec )

                  endif
               endif
            next

            DispOut( Space( Int( Round( ( nWidth - ::nColsWidth ) / 2, 0 ) ) ), ::ColorSpec )

            // doesn't need to be redrawn
            ::aRedraw[nRow] := .F.

            // Exit incremental row stabilization
            SetCursor( nOldCursor )
            return .F.
         endif

      next

      // If I reach this point I've repainted all rows so I can set ::stable state
      if nRow > ::RowCount

         // If I have fewer records than available TBrowse rows, cursor cannot be lower than
         // last record (note ::lHitBottom is set only during a movement)
         if ::nLastRetrieved < ::nNewRowPos
            ::nNewRowPos := ::nLastRetrieved
         endif

         // If I'm not already under cursor I have to set data source to cursor position
         if ::nLastRetrieved <> ::nNewRowPos
            Eval( ::bSkipBlock, ::nNewRowPos - ::nLastRetrieved )
            ::nLastRetrieved := ::nNewRowPos
         endif

         // new cursor position
         ::RowPos := ::nNewRowPos

         ::HitTop := ::lHitTop
         ::HitBottom := ::lHitBottom

         if ::AutoLite
            ::Hilite()
         else
            ::PosCursor()
         endif
         SetCursor( nOldCursor )
         ::stable := .T.

         return .T.
      endif

   else
      /* NOTE: DBU relies upon current cell being reHilited() even if already stable */
      if ::AutoLite
         ::Hilite()
      else
         ::PosCursor()
      endif
      SetCursor( nOldCursor )
      return .T.

   endif

return .F.


// Movement keys cause TBrowse to become unstable.
METHOD Moved() CLASS TBrowse

   // No need to Dehilite() current cell more than once
   if ::stable
      // Internal flags used to set ::HitTop/Bottom during next stabilization
      ::lHitTop := .F.
      ::lHitBottom := .F.

      ::DeHilite()

      ::stable := .F.
   endif

return Self

/* NOTE: Incompatibility: In C5x this function will refresh the
         screen if a non-empty aRect was passed. It does this
         without skipping/reloading all the records, so I suspect
         some internal buffering. For now I left out this feature,
         so caller has to refresh manually. [vszakats] */

METHOD ColorRect( aRect, aRectColor ) CLASS TBrowse

   IF ISARRAY( aRect ) .AND. ISARRAY( aRectColor )
      ::aRect       := aRect
      ::aRectColor  := aRectColor
   ENDIF

return Self


METHOD DispCell( nRow, nCol, nMode ) CLASS TBrowse

   LOCAL oCol   := ::aColumns[ nCol ]
   LOCAL nWidth := ::aColsWidth[ nCol ]
   LOCAL ftmp   := Eval( oCol:block )
   LOCAL cType  := ValType( ftmp )
   LOCAL cPict  := iif( Empty( oCol:Picture ), "", oCol:Picture )

   LOCAL tmp

   LOCAL cColor

   IF ! Empty( ::aRect ) .AND. ;
      nCol >= ::aRect[ 2 ] .AND. ;
      nCol <= ::aRect[ 4 ] .AND. ;
      nRow >= ::aRect[ 1 ] .AND. ;
      nRow <= ::aRect[ 3 ] .AND. ;
      ! Empty( ::aRectColor )
      cColor := hb_ColorIndex( ::ColorSpec, ::aRectColor[ nMode ] - 1 )
   ELSE
      cColor := hb_ColorIndex( ::ColorSpec, iif( oCol:ColorBlock == NIL, oCol:defColor[ nMode ], Eval( oCol:ColorBlock, ftmp )[ nMode ] ) - 1 )
   ENDIF

   do case
   case cType $ "CM"
      DispOut( PadR( Transform( ftmp, cPict ), nWidth ), cColor )

   case cType == "N"
      DispOut( PadL( Transform( ftmp, cPict ), nWidth ), cColor )

   case cType == "D"
      cPict := iif( cPict == "", "@D", cPict )
      DispOut( PadR( Transform( ftmp, cPict ), nWidth ), cColor )

   case cType == "L"
      tmp := PadC( "X", nWidth )
      DispOut( Space( Len( tmp ) - Len( LTrim( tmp ) ) ), cColor )
      DispOut( iif( ftmp, "T", "F" ), cColor )
      DispOut( Space( Len( tmp ) - Len( RTrim( tmp ) ) ), cColor )

   otherwise
      DispOut( Space( nWidth ), cColor )

   endcase

return cType


#ifdef HB_COMPAT_C53

METHOD ApplyKey( nKey )  CLASS TBrowse

return ::TApplyKey( nKey, self )


METHOD InitKeys( o ) CLASS TBROWSE

   // ; Assigned codeblock receives two parameters: {|oTBrowse, nKey| <action> }

   Default o:aKeys to {{ K_DOWN       , {|Ob| Ob:Down(), 0 }},;
                       { K_END        , {|Ob| Ob:End(), 0 }},;
                       { K_CTRL_PGDN  , {|Ob| Ob:GoBottom(), 0 }},;
                       { K_CTRL_PGUP  , {|Ob| Ob:GoTop(), 0 }},;
                       { K_HOME       , {|Ob| Ob:Home(), 0 }},;
                       { K_LEFT       , {|Ob| Ob:Left(), 0 }},;
                       { K_PGDN       , {|Ob| Ob:PageDown(), 0 }},;
                       { K_PGUP       , {|Ob| Ob:PageUp(), 0 }},;
                       { K_CTRL_END   , {|Ob| Ob:PanEnd(), 0 }},;
                       { K_CTRL_HOME  , {|Ob| Ob:PanHome(), 0 }},;
                       { K_CTRL_LEFT  , {|Ob| Ob:PanLeft(), 0 }},;
                       { K_CTRL_RIGHT , {|Ob| Ob:PanRight(), 0 }},;
                       { K_RIGHT      , {|Ob| Ob:Right(), 0 }},;
                       { K_UP         , {|Ob| Ob:Up(),0 }},;
                       { K_ESC        , {|| -1 }},;
                       { K_LBUTTONDOWN, {|Ob| tbmouse(ob, MRow(), MCol()) }}}
return o


METHOD SetKey( nKey, bBlock ) CLASS TBrowse

   local bReturn,nPos

   ::InitKeys( self )

   if ( nPos := ascan( ::aKeys, {|x| x[1] == nkey } ) ) == 0
      if ( ISBLOCK( bBlock ) )
         bReturn := bBlock
         aadd( ::aKeys, { nKey, bBlock } )
      endif
      bReturn:=bBlock

   elseif ( ISBLOCK( bBlock ) )
      ::aKeys[npos][2] := bBlock
      bReturn := bBlock

   elseif PCOUNT() == 1
      bReturn := ::aKeys[npos][2]

   elseif ( bReturn := ::aKeys[ nPos ][ 2 ], PCount() == 2 .AND. ;
           ISNIL( bBlock ) .AND. nKey != 0 )
      adel( ::aKeys, nPos )
      asize( ::aKeys, Len( ::aKeys ) - 1 )
   endif

return bReturn


METHOD TApplyKey( nKey, oBrowse ) CLASS tBrowse

   local bBlock := oBrowse:setkey( nKey ), nReturn := TBR_CONTINUE  // 0

   default bBlock to oBrowse:setkey( 0 )

   if ( ISNIL( bBlock ) )
      nReturn := TBR_EXCEPTION  // 1
   else
      nReturn := eval( bBlock, oBrowse, nKey )
   endif

return nReturn
#endif


// NOTE: Not tested, could be broken
METHOD MGotoYX( nRow, nCol ) CLASS TBrowse

   local nColsLen, nI, nNewRow

   // Am I inside TBrowse display area ?
   if nRow > ::nTop .AND. nRow < ::nBottom .AND. ;
      nCol > ::nLeft .AND. nCol < ::nRight

      // if not stable force repositioning of data source; maybe this is not first Stabilize() call after
      // TBrowse became unstable, but we need to call Stabilize() al least one time before moving again to be sure
      // data source is under cursor position
      if ! ::stable
         ::Stabilize()

      else
         ::Moved()

      endif

      // Set new row position
      nNewRow := nRow - ::nTop + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::HeadSep ) .OR. ! ::lHeaders, 0, 1 ) - 1
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      ::Stabilize()

      // Now move to column under nCol
      nColsLen := 0
      // NOTE: I don't think it is correct, have to look up docs
      nI := iif( ::nFrozenCols > 0, ::nFrozenCols, ::leftVisible )

      while nColsLen < nCol .AND. nI < ::rightVisible

         nColsLen += ::aColsWidth[nI]
         if nI >= 1 .AND. nI < ::nColumns
            nColsLen += iif( ::aColumns[nI]:ColSep != NIL, Len( ::aColumns[nI]:ColSep ), Len( ::ColSep ) )
         endif

         nI++

      enddo

      ::ColPos := nI

      // Force redraw of current row with new cell position
      ::RefreshCurrent()

   endif

return Self


METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor ) CLASS TBrowse

   local n
   local nCol := Col()
   local nRow := Row()

   // Do I have to write an header or a footer?
   if lHeader

      // Simple case, write header as usual
      if ::nHeaderHeight == 1
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         for n := ::nHeaderHeight to 1 step -1
            SetPos( nRow + n - 1, nCol )
            DispOut( PadR( hb_TokenGet( @cStr, n, ";" ), nPadLen ), cColor )
         next

         SetPos( nRow, nCol + nPadLen )

      endif

   // footer
   else

      // Simple case, write footer as usual
      if ::nFooterHeight == 1
         DispOut( PadR( cStr, nPadLen ), cColor )

      else
         for n := 0 to ( ::nFooterHeight - 1 )
            SetPos( nRow - n, nCol )
            DispOut( PadR( hb_TokenGet( @cStr, ::nFooterHeight - n, ";" ), nPadLen ), cColor )
         next

         SetPos( nRow, nCol + nPadLen )

      endif

   endif

return Self

METHOD SetSkipBlock( b )
   local oErr

   if ISBLOCK( b )
      ::bSkipBlock := b
   elseif b != NIL
      oErr := ErrorNew()
      oErr:Args          := { b }
      oErr:CanSubstitute := .T.
      oErr:severity      := ES_ERROR
      oErr:genCode       := EG_ARG
      oErr:subSystem     := "TBROWSE"
      oErr:SubCode       := 1001
      oErr:Description   := "Argument error"
      Eval( ErrorBlock(), oErr )
   endif

return ::bSkipBlock

function TBrowseNew( nTop, nLeft, nBottom, nRight )

return TBrowse():New( nTop, nLeft, nBottom, nRight )


#ifdef HB_COMPAT_C53
function TBMOUSE( oBrowse, nMouseRow, nMouseCol )
   local n

   if ( oBrowse:hittest( nMouseRow, nMouseCol ) == -5121 )

      n := oBrowse:mRowPos - oBrowse:rowPos

      do while ( n < 0 )
         n++
         oBrowse:up()
      enddo

      do while ( n > 0 )
         n--
         oBrowse:down()
      enddo

      n := oBrowse:mColPos - oBrowse:colPos

      do while ( n < 0 )
         n++
         oBrowse:left()
      enddo

      do while ( n > 0 )
         n--
         oBrowse:right()
      enddo

      return 0
   endif

   return 1

Method hitTest( mRow, mCol ) CLASS TBROWSE
  local i
  ::mRowPos := ::rowPos
  ::mColPos := ::colPos

  if mRow < ::rect[1] .or. mRow > ::rect[3]
     return HTNOWHERE
  endif

  if mCol < ::rect[2] .or. mCol > ::rect[4]
     return HTNOWHERE
  endif

  ::mRowPos := mRow - ::rect[1] + 1
  for i := 1 to len( ::aVisibleCols )
      if ::aVisibleCols[i] > mcol
         exit
      endif
  next

  ::mColPos := ::aVisibleCols[i]

return HTCELL

METHOD SetStyle( nMode, lSetting ) CLASS TBROWSE
  LOCAL lRet := .F.

  IF nMode > LEN( ::aSetStyle )
     ASize( ::aSetStyle, nMode )
     ::aSetStyle[ nMode ] := .F.
  ENDIF

  lRet := ::aSetStyle[ nMode ]

  IF ISLOGICAL( lSetting )
     ::aSetStyle[ nMode ] := lSetting
  ENDIF

RETURN lRet
#endif
