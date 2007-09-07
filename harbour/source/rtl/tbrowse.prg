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
 * Copyright 2000-2002 Maurilio Longo <maurilio.longo@libero.it>
 * Cursor movement handling, stabilization loop, multi-line headers and footers support
 * ::PageUp(), ::PageDown(), ::Down(), ::Up(), ::GoBottom(), ::GoTop(), ::Stabilize()
 * ::GotoXY(), ::DispCell(), ::WriteMLineText(), ::RedrawHeaders(),
 * ::freeze(), ::SetColumnWidth()
 *
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell( nColumn, nColor )
 *
 */


/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible.
         ADDITION: Same goes for DevPos(), always use SetPos() instead.
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

/* TBColumn info constants */
#define TBCI_OBJ                1    // Object TBColumn
#define TBCI_TYPE               2    // Type of Data in Column
#define TBCI_WIDTH              3    // Column Width
#define TBCI_HEADING            4    // Column Headings
#define TBCI_FOOTING            5    // Column Footings
#define TBCI_PICT               6    // Column Picture
#define TBCI_WIDTHCELL          7    // Width of the Cell
#define TBCI_COLSEP             8    // Column Seperator
#define TBCI_SEPWIDTH           9    // Width of the Separator
#define TBCI_DEFCOLOR           10   // Array with index of color
#define TBCI_SETWIDTH           11   // If True, only SetFrozen can change TBCI_WIDTH
#define TBCI_LCOLSEP            12   // Should column separator be drawn
#define TBCI_SCRCOLPOS          13   // Temporary column position on screen
								
#define TBC_CLR_STANDARD        1    // first index value to set unselected data color.
#define TBC_CLR_ENHANCED        2    // second index value to set selected data color.
#ifdef HB_COMPAT_C53
#define TBC_CLR_HEADING         3    // third index value to set heading color.
#define TBC_CLR_FOOTING         4    // fourth index value to set footing color.
#else
#define TBC_CLR_HEADING         TBC_CLR_STANDARD
#define TBC_CLR_FOOTING         TBC_CLR_STANDARD
#endif

/* NOTE: In CA-Cl*pper TBROWSE class does not inherit from any other classes
         and there is no public class function like TBrowse(). There is 
         in XPP though. */ 

CREATE CLASS TBrowse

   EXPORT:

   VAR cargo                                                // User-definable variable
   VAR leftVisible      INIT 0 READONLY                     // Indicates position of leftmost unfrozen column in display
   VAR rightVisible     INIT 0 READONLY                     // Indicates position of rightmost unfrozen column in display
   VAR rowCount                READONLY                     // Number of visible data rows in the TBrowse display

   METHOD down()                                            // Moves the cursor down one row
   METHOD end()                                             // Moves the cursor to the rightmost visible data column
   METHOD goBottom()                                        // Repositions the data source to the bottom of file
   METHOD goTop()                                           // Repositions the data source to the top of file
   METHOD home()                                            // Moves the cursor to the leftmost visible data column
   METHOD left()                                            // Moves the cursor left one column
   METHOD pageDown()                                        // Repositions the data source downward
   METHOD pageUp()                                          // Repositions the data source upward
   METHOD panEnd()                                          // Moves the cursor to the rightmost data column
   METHOD panHome()                                         // Moves the cursor to the leftmost visible data column
   METHOD panLeft()                                         // Pans left without changing the cursor position
   METHOD panRight()                                        // Pans right without changing the cursor position
   METHOD right()                                           // Moves the cursor right one column
   METHOD up()                                              // Moves the cursor up one row

   METHOD addColumn( oCol )
   METHOD delColumn( nPos )                                 // Delete a column object from a browse
   METHOD insColumn( nPos, oCol )                           // Insert a column object in a browse
   METHOD getColumn( nColumn )                              // Gets a specific TBColumn object
   METHOD setColumn( nColumn, oCol )                        // Replaces one TBColumn object with another
   METHOD colWidth( nColumn )                               // Returns the display width of a particular column
   METHOD colCount()
   METHOD colorRect()                                       // Alters the color of a rectangular group of cells
   /* NOTE: nMode is an undocumented Harbour parameter. Should not be used by app code. */
   METHOD configure( nMode )                                // Reconfigures the internal settings of the TBrowse object
   METHOD deHilite()                                        // Dehighlights the current cell
   METHOD forceStable()                                     // Performs a full stabilization
   METHOD hilite()                                          // Highlights the current cell
   METHOD invalidate()                                      // Forces entire redraw during next stabilization
   METHOD refreshAll()                                      // Causes all data to be recalculated during the next stabilize
   METHOD refreshCurrent()                                  // Causes the current row to be refilled and repainted on next stabilize
   METHOD stabilize()                                       // Performs incremental stabilization
#ifdef HB_COMPAT_C53
   METHOD setKey( nKey, bBlock )
   METHOD applyKey( nKey )
   METHOD TApplyKey( nKey, o )                              /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD hitTest( nMouseRow, nMouseCol )
   METHOD setStyle( nStyle, lNewValue )
#endif
#ifdef HB_COMPAT_XPP
   MESSAGE _Left() METHOD Left()
   MESSAGE _Right() METHOD Right()
   MESSAGE _End() METHOD End()
#endif

   METHOD autoLite( lAutoLite ) SETGET
   METHOD nBottom( nBottom ) SETGET
   METHOD nLeft( nLeft ) SETGET
   METHOD nRight( nRight ) SETGET
   METHOD nTop( nTop ) SETGET
   METHOD colorSpec( cColorSpec ) SETGET
   METHOD colSep( cColSep ) SETGET
   METHOD footSep( cFootSep ) SETGET
   METHOD headSep( cHeadSep ) SETGET
   METHOD colPos( nColPos ) SETGET
   METHOD goBottomBlock( bBlock ) SETGET
   METHOD goTopBlock( bBlock ) SETGET
   METHOD hitBottom( lHitBottom ) SETGET
   METHOD hitTop( lHitTop ) SETGET
   METHOD rowPos( nRowPos ) SETGET
   METHOD stable( lStable ) SETGET
   METHOD freeze( nFrozenCols ) SETGET
   METHOD skipBlock( bSkipBlock ) SETGET
#ifdef HB_COMPAT_C53                                   
   METHOD nRow( nRow ) SETGET                               /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD nCol( nCol ) SETGET                               /* NOTE: Undocumented CA-Cl*pper 5.3 method. */
   METHOD mRowPos( nMRowPos ) SETGET
   METHOD mColPos( nMColPos ) SETGET
   METHOD message( cMessage ) SETGET
#endif

   METHOD New( nTop, nLeft, nBottom, nRight )               /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR lAutoLite        INIT .T.                            // Logical value to control highlighting
   VAR n_Top            INIT 0                              // Top row number for the TBrowse display
   VAR n_Left           INIT 0                              // Leftmost column for the TBrowse display
   VAR n_Bottom         INIT 0                              // Bottom row number for the TBrowse display
   VAR n_Right          INIT 0                              // Rightmost column for the TBrowse display
   VAR cColorSpec       INIT SetColor()                     // Color table for the TBrowse display
   VAR cColSep          INIT " "                            // Column separator character
   VAR cFootSep         INIT ""                             // Footing separator character
   VAR cHeadSep         INIT ""                             // Heading separator character
   VAR nColPos          INIT 1                              // Current cursor column position
   VAR bGoBottomBlock   INIT {|| NIL }                      // Code block executed by TBrowse:goBottom()
   VAR bGoTopBlock      INIT {|| NIL }                      // Code block executed by TBrowse:goTop()
   VAR lHitBottom       INIT .F.                            // Indicates the end of available data
   VAR lHitTop          INIT .F.                            // Indicates the beginning of available data
   VAR nRowPos          INIT 1                              // Current cursor row position
   VAR lStable          INIT .F.                            // Indicates if the TBrowse object is stable
   VAR aRect            INIT {}                             // The rectangle specified with ColorRect()
   VAR aRectColor       INIT {}                             // The color positions to use in the rectangle specified with ColorRect()
   VAR aRedraw          INIT {}                             // Array of logical items indicating, is appropriate row need to be redraw
   VAR aColumns         INIT {}                             // Array to hold all browse columns
   VAR aColsWidth       INIT {}                             // Array with width of TBrowse's columns
   VAR aColsPos         INIT {}                             // Array with position of TBrowse's columns
   VAR lHeaders         INIT .F.                            // Internal variable which indicates whether there are column footers to paint
   VAR lFooters         INIT .F.                            // Internal variable which indicates whether there are column footers to paint
   VAR lRedrawFrame     INIT .T.                            // True if I need to redraw Headers/Footers
   VAR nColsWidth       INIT 0                              // Total width of visible columns plus ColSep
   VAR nColsVisible     INIT 0                              // Number of columns that fit on the browse width
   VAR lHitTop          INIT .F.                            // Internal Top reached flag
   VAR lHitBottom       INIT .F.                            // Internal Bottom reached flag
   VAR nRecsToSkip      INIT 0                              // Recs to skip on next Stabilize()
   VAR nNewRowPos       INIT 1                              // Next position of data source (after first phase of stabilization)
   VAR nLastRetrieved   INIT 1                              // Position, relative to first row, of last retrieved row (with an Eval(::bSkipBlock, n))
   VAR nHeaderHeight    INIT 1                              // How many lines is highest Header/Footer and so how many lines of
   VAR nFooterHeight    INIT 1                              // screen space I have to reserve
   VAR nFrozenWidth     INIT 0                              // How many screen column are not available on the left side of TBrowse display. > 0 only when there are frozen columns
   VAR bSkipBlock       INIT {|| NIL }                      // Code block used to reposition data source
   VAR nFrozenCols      INIT 0                              // Number of frozen columns on left side of TBrowse
   VAR nColumns         INIT 0                              // Number of columns added to TBrowse
   VAR lNeverDisplayed  INIT .T.                            // .T. if TBrowse has never been stabilized()
#ifdef HB_COMPAT_C53                                        
   VAR n_Row            INIT 0                              // Row number for the actual cell
   VAR n_Col            INIT 0                              // Col number for the actual cell
   VAR nMRowPos         INIT 0
   VAR nMColPos         INIT 0
   VAR cMessage         INIT ""
   VAR aKeys
   VAR rect
   VAR aVisibleCols     INIT {}
   VAR aSetStyle        INIT { .F., .F., .F., .F., .F. }    /* TBR_APPEND, TBR_APPENDING, TBR_MODIFY, TBR_MOVE, TBR_SIZE */
#endif

   METHOD PosCursor()                                       // Positions the cursor to the beginning of the call, used only when autolite==.F.
   METHOD LeftDetermine()                                   // Determine leftmost unfrozen column in display
   METHOD DispCell( nRow, nCol, nMode )                     // Displays a single cell and returns cell type as a single letter like Valtype()
   METHOD HowManyCol()                                      // Counts how many cols can be displayed
   METHOD RedrawHeaders()                                   // Repaints TBrowse Headers
   METHOD Moved()                                           // Every time a movement key is issued I need to reset certain properties of TBrowse, I do these settings inside this method
   METHOD WriteMLineText( cStr, nPadLen, lHeader, cColor )  // Writes a multi-line text where ";" is a line break, lHeader is .T. if it is a header and not a footer
   METHOD SetColumnWidth( oCol )                            // Calcs width of given column
   METHOD MGotoYX( nRow, nCol )                             // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                                            // _M_GotoXY because this method will mostly be called to handle mouse requests
ENDCLASS

/* -------------------------------------------- */

METHOD invalidate() CLASS TBrowse

   AFill( ::aRedraw, .T. )
   ::lStable := .F.
   ::lRedrawFrame := .T.

   return Self

METHOD refreshAll() CLASS TBrowse

   AFill( ::aRedraw, .T. )
   ::lStable := .F.

   return Self

METHOD refreshCurrent() CLASS TBrowse

   if ! Empty( ::aRedraw ) .and. ::nRowPos > 0
      ::aRedraw[ ::nRowPos ] := .T.
   endif

   ::lStable := .F.

   return Self

METHOD configure( nMode ) CLASS TBrowse

   local n
   local nHeight
   local nLeft
   local nRight

   ::lHeaders := .F.
   ::lFooters := .F.
   ::lRedrawFrame := .T.

   // Are there column headers to paint ?
   for n := 1 to ::nColumns
      if ! Empty( ::aColumns[ n ]:Heading )
         ::lHeaders := .T.
         exit
      endif
   next

   // Are there column footers to paint ?
   for n := 1 to ::nColumns
      if ! Empty( ::aColumns[ n ]:Footing )
         ::lFooters := .T.
         exit
      endif
   next

   ::nHeaderHeight := 1
   ::nFooterHeight := 1

   // Find out highest header and footer
   for n := 1 to ::nColumns

      if ::lHeaders .and. ! Empty( ::aColumns[ n ]:Heading )
         nHeight := Len( ::aColumns[ n ]:Heading ) - Len( StrTran( ::aColumns[ n ]:Heading, ";" ) ) + 1

         if nHeight > ::nHeaderHeight
            ::nHeaderHeight := nHeight
         endif

      endif

      if ::lFooters .and. ! Empty( ::aColumns[ n ]:Footing )
         nHeight := Len( ::aColumns[ n ]:Footing ) - Len( StrTran( ::aColumns[ n ]:Footing, ";" ) ) + 1

         if nHeight > ::nFooterHeight
            ::nFooterHeight := nHeight
         endif

      endif
   next

   if nMode == NIL
      for n := 1 to ::nColumns
         ::aColsWidth[ n ] := ::SetColumnWidth( ::aColumns[ n ] )
      next
   endif

   // 20/nov/2000 - maurilio.longo@libero.it
   // If I add (or remove) header or footer (separator) I have to change number
   // of available rows
   ::RowCount := ::n_Bottom - ::n_Top + 1 - ;
                  iif( ::lHeaders, ::nHeaderHeight + iif( Empty( ::cHeadSep ), 0, 1 ), 0 ) - ;
                  iif( ::lFooters, ::nFooterHeight + iif( Empty( ::cFootSep ), 0, 1 ), 0 )

   if Len( ::aRedraw ) != ::RowCount
      ASize( ::aRedraw, ::RowCount )
   endif

   ::Invalidate()

   // Force re-evaluation of space occupied by frozen columns
   if ::nFrozenCols > 0
      ::freeze := ::nFrozenCols
   endif

   #ifdef HB_COMPAT_C53

      nLeft := ::n_Left
      nRight := ::n_Right
      ::rect := { ::n_Top + ::nHeaderHeight + iif( Empty( ::cHeadSep ), 0, 1 ), ::n_Left, ::n_Bottom - ::nFooterHeight - iif( Empty( ::cFootSep ), 0, 1 ), ::n_Right }
      for n := nLeft To nRight
         AAdd( ::aVisibleCols, n )
      next

   #endif

   return Self

// Adds a TBColumn object to the TBrowse object
METHOD addColumn( oCol ) CLASS TBrowse

   ::Moved()

   ::nColumns++

   AAdd( ::aColumns, oCol )
   AAdd( ::aColsWidth, ::SetColumnWidth( oCol ) )
   AAdd( ::aColsPos, 0 )

   if ::nColumns == 1
      ::leftVisible := 1
   endif

   ::Configure( 2 )
   ::HowManyCol()

   return Self

// Insert a column object in a browse
METHOD insColumn( nPos, oCol ) CLASS TBrowse

   if nPos >= 1

      ::Moved()
      
      if nPos > ::nColumns

         ::nColumns++

         AAdd( ::aColumns, oCol )
         AAdd( ::aColsWidth, ::SetColumnWidth( oCol ) )
         AAdd( ::aColsPos, 0 )

      else

         ::nColumns++
         
         ASize( ::aColumns, ::nColumns )
         AIns( ::aColumns, nPos )
         ASize( ::aColsWidth, ::nColumns )
         AIns( ::aColsWidth, nPos )
         ASize( ::aColsPos, ::nColumns )
         AIns( ::aColsPos, nPos )

         ::aColumns[ nPos ] := oCol
         ::aColsWidth[ nPos ] := ::SetColumnWidth( oCol )
         ::aColsPos[ nPos ] := 0

      endif
   
      ::Configure( 2 )
      ::HowManyCol()

   endif

   return oCol

// Replaces one TBColumn object with another
METHOD setColumn( nPos, oCol ) CLASS TBrowse

   if nPos >= 1 .and. nPos <= ::nColumns

      ::Moved()

      ::aColumns[ nPos ] := oCol
      ::aColsWidth[ nPos ] := ::SetColumnWidth( oCol )
      ::aColsPos[ nPos ] := 0

      ::Configure( 2 )
      ::HowManyCol()

   endif

   return oCol

METHOD delColumn( nPos ) CLASS TBrowse

   local oCol := ::aColumns[ nPos ] /* NOTE: To keep CA-Cl*pper compatible runtime error generation. [vszakats] */

   ::Moved()

   /* Need to adjust variables in case last column is deleted. */

   if nPos == ::nColPos .or. ;
      nPos == ::nColumns .or.;
      ::nColPos == ::nColumns .or. ;
      ::rightVisible == ::nColumns

      if ::leftVisible == ::rightVisible .and. ::leftVisible > 1
         ::leftVisible--
      endif

      ::rightVisible--

      if ::nColPos == ::nColumns
         ::nColpos--
      endif

   endif

   ::nColumns--

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, ::nColumns )
   ADel( ::aColsWidth, nPos )
   ASize( ::aColsWidth, ::nColumns )
   ADel( ::aColsPos, nPos )
   ASize( ::aColsPos, ::nColumns )

   if ::nColumns < ::nFrozenCols
      ::nFrozenCols := 0
   endif

   ::Configure( 2 )
   ::HowManyCol()

   return oCol

// Gets a specific TBColumn object
METHOD getColumn( nColumn ) CLASS TBrowse
   return iif( nColumn > 0 .and. nColumn <= ::nColumns, ::aColumns[ nColumn ], NIL )

// Returns the display width of a particular column
METHOD colWidth( nColumn ) CLASS TBrowse
   return iif( nColumn > 0 .and. nColumn <= ::nColumns, ::aColsWidth[ nColumn ], 0 )

METHOD colCount() CLASS TBrowse
   return ::nColumns

METHOD freeze( nFrozenCols ) CLASS TBrowse

   local nCol
   local nWidth

   if ISNUMBER( nFrozenCols )
   
      if nFrozenCols >= 0 .and. nFrozenCols <= ::nColumns

         nWidth := ::n_Right - ::n_Left + 1    // Visible width of the browse
         
         ::nFrozenCols := nFrozenCols
         // Space inside TBrowse window reserved for frozen columns
         ::nFrozenWidth := 0
         
         // If I've never displayed this TBrowse before I cannot calc occupied space since
         // columns:width is not yet set, ::Stabilize() will call me later
         if ! ::lNeverDisplayed
         
            if nFrozenCols > 0
               for nCol := 1 to nFrozenCols
                  ::nFrozenWidth += ::aColsWidth[ nCol ]
                  if nCol < ::nColumns
                     ::nFrozenWidth += iif( ::aColumns[ nCol + 1 ]:ColSep != NIL,;
                                            Len( ::aColumns[ nCol + 1 ]:ColSep ),;
                                            Len( ::cColSep ) )
                  endif
               next
            endif
         
            for nCol := 1 to ::nColumns
               if nFrozenCols > 0
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
      else
         return nFrozenCols
      endif
   endif

   return ::nFrozenCols

METHOD down() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip++

   return Self

METHOD up() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip--

   return Self

METHOD end() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::rightVisible
      ::nColPos := ::rightVisible
      // !
      ::lRedrawFrame := .T.
      ::refreshCurrent()
   endif

   return Self

METHOD goBottom() CLASS TBrowse

   local nToTop

   ::Moved() // !

   Eval( ::bGoBottomBlock )

   // Skip back from last record as many records as TBrowse can hold
   nToTop := Abs( Eval( ::bSkipBlock, - ( ::RowCount - 1 ) ) )

// ::nRecsToSkip := ::RowCount - 1

   // From top of TBrowse new row position is nToTop + 1 records away
   ::nNewRowPos := nToTop + 1

   // Last read record is first record inside TBrowse
   ::nLastRetrieved := 1
// ::nRowPos := 1
   ::refreshAll()

   return Self

METHOD goTop() CLASS TBrowse

   ::Moved() // !

   Eval( ::bGoTopBlock )
   ::nLastRetrieved := 1
   ::nNewRowPos := 1
   ::refreshAll()

   return Self

METHOD home() CLASS TBrowse

   ::Moved()

   if ::nColPos != ::leftVisible
      ::nColPos := ::leftVisible
      ::invalidate()
//    ::lRedrawFrame := .T.
//    ::refreshCurrent()
   endif

   return Self

METHOD right() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::rightVisible
      ::nColPos++
   else
      if ::nColPos < ::nColumns
         ::rightVisible++
         ::leftVisible := ::leftDetermine()
         ::nColPos++
         ::invalidate()
//       ::lRedrawFrame := .T.
//       ::refreshAll()
      else
         /* 09/08/2004 - <maurilio.longo@libero.it>
                         In a ! ::lStable state clipper moves ::nColPos past ::ColCount or
                         before column 1, so here and on _Left(), Home(), End(), PanEnd()
                         PanHome() methods I let it go "out of bounds",
                         PerformStabilization() gives ::nColPos a correct value */
         ::nColPos++
      endif
   endif

   return Self

METHOD left() CLASS TBrowse

   local leftVis

   ::Moved()

   if ::nColPos > ::leftVisible .or.;
      ( ::nColPos <= ::nFrozenCols + 1 .and. ::nColPos > 1 )
      ::nColPos--
   else
      if ::nColPos <= Max( ::leftVisible, ::nFrozenCols ) .and. ::nColPos > 1
         leftVis := ::leftVisible
         do while leftVis == ::leftVisible
            ::rightVisible--
            ::leftVisible := ::LeftDetermine()
         enddo
         ::nColPos--
         ::Invalidate()
//       ::lRedrawFrame := .T.
//       ::RefreshAll()
      else
         ::nColPos-- // Can go "out of bounds", here we behave like clipper.
      endif
   endif

   return Self

METHOD leftDetermine() CLASS TBrowse

   local nWidthMax := ::n_Right - ::n_Left + 1  // Visible width of the browse
   local nWidth := ::nFrozenWidth
   local nCol

   nCol := ::rightVisible
   do while nWidth < nWidthMax .and. nCol > ::nFrozenCols

      nWidth += ::aColsWidth[ nCol ] +;
                iif( ::aColumns[ nCol ]:ColSep != NIL,;
                     Len( ::aColumns[ nCol ]:ColSep ),;
                     Len( ::cColSep ) )

      if nWidth < nWidthMax
         nCol--
      endif
   enddo

   return Min( nCol + 1, ::nColumns )

METHOD pageDown() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := ( ::RowCount - ::nRowPos ) + ::RowCount

   return Self

METHOD pageUp() CLASS TBrowse

   ::Moved()
   ::nRecsToSkip := - ( ( ::nRowPos - 1 ) + ::RowCount )

   return Self

METHOD panEnd() CLASS TBrowse

   ::Moved()

   if ::nColPos < ::nColumns
      if ::rightVisible < ::nColumns
         ::rightVisible := ::nColumns
         ::leftVisible := ::LeftDetermine()
         ::nColPos := ::rightVisible
         ::Invalidate()
//       ::lRedrawFrame := .T.
//       ::RefreshAll()
      else
         ::nColPos := ::rightVisible
         /* 18/10/2005 - <maurilio.longo@libero.it>
                         This is wrong, should not reload datasource. But ::Invalidate()
                         forces a full repaint, which is overkill. Maybe just a ::aRedraw[ x ] := .T. */
         ::RefreshCurrent()
      endif
   else
      ::nColPos := ::nColumns // Can go "out of bounds", here we behave like clipper
   endif

   return Self

METHOD panHome() CLASS TBrowse

   ::Moved()

   if ::nColPos > 1
      if ::leftVisible > ::nFrozenCols + 1
         ::leftVisible := ::nFrozenCols + 1
         ::nColPos := 1
         ::Invalidate()
//       ::RefreshAll()
//       ::lRedrawFrame := .T.
      else
         ::nColPos := 1
         ::RefreshCurrent()
      endif
   else
      ::nColPos := 1 // Can go "out of bounds", here we behave like clipper
   endif

   return Self

METHOD panLeft() CLASS TBrowse

   local leftVis

   ::Moved()

   if ::leftVisible > ::nFrozenCols + 1
      leftVis := ::leftVisible
      /* While space left available by columns exiting to the right side of tbrowse
         is not enough to contain a new column to the left (::leftVisible doesn't change) */
      do while leftVis == ::leftVisible
         ::rightVisible--
         ::leftVisible := ::LeftDetermine()
      enddo

      /* Since panel "shifts" to the right, ::nColPos could end up "out of" the
         right side of tbrowse, so, change it to ::rightvisible if this happens */
      ::nColPos := Min( ::nColPos, ::rightVisible )
      ::Invalidate()
//    ::lRedrawFrame := .T.
//    ::RefreshAll()
   endif

   return Self

METHOD panRight() CLASS TBrowse

   local leftVis

   ::Moved()

   if ::rightVisible < ::nColumns
      leftVis := ::leftVisible
      do while leftVis == ::leftVisible
         ::rightVisible++
         ::leftVisible := ::LeftDetermine()
      enddo
      ::nColPos := Min( ::nColPos, ::rightVisible )
      ::Invalidate()
//    ::lRedrawFrame := .T.
//    ::RefreshAll()
   endif

   return Self

METHOD forceStable() CLASS TBrowse

   do while !::Stabilize()
   enddo

   return Self

METHOD deHilite() CLASS TBrowse

   local nRow
   local cType

   if ::nColPos > 0 .and. ::nColPos <= Len( ::aColumns )

      nRow := ::n_Top + ::nRowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
      
      SetPos( nRow, ::aColsPos[ ::nColPos ] )
      
      cType := ::DispCell( ::nRowPos, ::nColPos, TBC_CLR_STANDARD )
      
      SetPos( nRow, ::aColsPos[ ::nColPos ] + iif( cType == "L", ::aColsWidth[ ::nColPos ] / 2, 0 ) )

   endif

   return Self

METHOD hilite() CLASS TBrowse

   local nRow
   local nCol
   local cType

   if ::nColPos > 0 .and. ::nColPos <= Len( ::aColumns )

      nRow := ::n_Top + ::nRowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
      nCol := ::aColsPos[ ::nColPos ]
      
      // Start of cell
      SetPos( nRow, nCol )
      
      cType := ::DispCell( ::nRowPos, ::nColPos, TBC_CLR_ENHANCED )
      nCol  += iif( cType == "L", ::aColsWidth[ ::nColPos ] / 2, 0 )
      
      // Put cursor back on first char of cell value
      SetPos( nRow, nCol )
      
      #ifdef HB_COMPAT_C53
         ::n_Row := nRow
         ::n_Col := nCol
      #endif

   endif

   return Self

METHOD stabilize() CLASS TBrowse

   local nRow, n
   local nWidth := ::n_Right - ::n_Left + 1 // Visible width of the browse
   local lDisplay                      // Is there something to show inside current cell?
   local nRecsSkipped                  // How many records do I really skipped?
   local nFirstRow                     // Where is on screen first row of TBrowse?
   local nOldCursor                    // Current shape of cursor (which I remove before stabilization)

   local oStartCol, oEndCol
   local nStartCol, nEndCol

   /* First, since ::nColPos can go "out of bounds" we need
      to put 1 <= ::nColpos <= ::nColumns
      And we need to do this before calling ::Configure() which
      needs a ::nColPos "inside bounds" */
   ::nColPos := Max( Min( ::nColPos, ::nColumns ), 1 )

   // I need to set columns width If TBrowse was never displayed before
   if ::lNeverDisplayed
      ::Configure( 0 )
      //AEval( ::aColumns, {| oCol | ::SetColumnWidth( oCol ) } )

      // NOTE: It must be before call to ::freeze assigment since this call
      //       tests this iVar value, and I set it to .F. since I'm going to display TBrowse
      //       for first time
      ::lNeverDisplayed := .F.

      // Force re-evaluation of frozen space since I could not calc it before
      // being columns width not set
      if ::nFrozenCols > 0
         ::freeze := ::nFrozenCols
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
      nStartCol := ::aColsPos[ iif( ::rightVisible != 0, ::rightVisible, 1 ) ]
      oEndCol := ::aColumns[ iif( ::nFrozenCols > 0, 1, ::leftVisible ) ]
      nEndCol := ::aColsPos[ iif( ::nFrozenCols > 0, 1, ::leftVisible ) ]
      ::nColsWidth := iif( oStartCol != NIL, nStartCol, 0 ) + ;
                      iif( oStartCol != NIL, ::aColsWidth[ iif( ::rightVisible != 0, ::rightVisible, 1 ) ], 0 ) - nEndCol
   endif

   // From this point there is stabilization of rows which is made up of three phases
   // 1st repositioning of data source
   // 2nd redrawing of rows, after each row we exit stabilization loop with .F.
   // 3rd if all rows have been redrawn we set ::lStable state to .T.
   if !::lStable

      // NOTE: I can enter here because of a movement key or a ::RefreshAll():ForceStable() call

      // If I have a requested movement still to handle
      if ::nRecsToSkip != 0

         // If I'm not under cursor (maybe I've interrupted an ongoing stabilization) I have to set data source to cursor position
         if ::nLastRetrieved != ::nNewRowPos
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
            if ( ::nNewRowPos + nRecsSkipped >= 1 ) .and. ( ::nNewRowPos + nRecsSkipped <= ::RowCount )
               ::nNewRowPos += nRecsSkipped
               ::nLastRetrieved := ::nNewRowPos
               // This is needed since present TBrowse has no cache, so I need to repaint current row
               // rereading it from data source and to force rereading from data source I have to mark
               // row as invalid
               ::aRedraw[ ::nNewRowPos ] := .T.

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
                  nFirstRow := ::n_Top + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. !::lHeaders, 0, 1 )

                  // I'm at top or bottom of TBrowse so I can scroll
                  if ::nNewRowPos == ::RowCount
                     Scroll( nFirstRow + nRecsSkipped - 1, ::n_Left, nFirstRow + ::RowCount - 1, ::n_Right, nRecsSkipped )
                     ::nLastRetrieved := ::RowCount

                  else
                     Scroll( nFirstRow, ::n_Left, nFirstRow + ::RowCount + nRecsSkipped, ::n_Right, nRecsSkipped )
                     ::nLastRetrieved := 1

                  endif

                  // I've scrolled on screen rows, now I need to scroll ::aRedraw array as well!
                  if nRecsSkipped > 0
                     ADel( ::aRedraw, 1 )
                  else
                     AIns( ::aRedraw, 1 )
                  endif

                  ::aRedraw[ ::nNewRowPos ] := .T.
               endif
            endif

         else // I couldn't move as far as requested

            // I need to refresh all rows if I go past current top or bottom row
            if ( ::nNewRowPos + nRecsSkipped < 1 ) .or. ( ::nNewRowPos + nRecsSkipped > ::RowCount )
               // don't go past boundaries
               ::nNewRowPos := iif( nRecsSkipped > 0, ::RowCount, 1 )
               ::RefreshAll()

            else
               ::nNewRowPos += nRecsSkipped
               ::aRedraw[ ::nNewRowPos ] := .T.

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
         if ::aRedraw[ nRow ]

            DispOutAt( ::n_Top + nRow + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. !::lHeaders, 0, 1 ) - 1, ::n_Left,;
                      Space( ( nWidth - ::nColsWidth ) / 2 ), ::cColorSpec )

            for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible

               if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
                  n := ::leftVisible
               endif

               if nRow == 1
                  ::aColsPos[ n ] := Col()
               endif

               // NOTE: If my TBrowse has 20 rows but I have only 3 recs, clipper clears
               //       remaining 17 rows in a single operation, I will, instead, try to skip
               //       17 times. Should be made more clever.
               if nRow != ::nLastRetrieved
                  if lDisplay := Eval( ::bSkipBlock, nRow - ::nLastRetrieved ) == ( nRow - ::nLastRetrieved )
                     ::nLastRetrieved := nRow
                  endif
               else
                  lDisplay := .T.
               endif

               if lDisplay
                  ::DispCell( nRow, n, TBC_CLR_STANDARD )
               else
                  // Clear cell
                  DispOut( Space( ::aColsWidth[ n ] ), tbr_GetColor( ::cColorSpec, ::aColumns[ n ]:defColor, TBC_CLR_STANDARD ) )
               endif

               if n < ::rightVisible
                  if ::aColumns[ n + 1 ]:ColSep != NIL
                     DispOut( ::aColumns[ n + 1 ]:ColSep, ::cColorSpec )

                  elseif ::cColSep != NIL
                     DispOut( ::cColSep, ::cColorSpec )

                  endif
               endif
            next

            DispOut( Space( Int( Round( ( nWidth - ::nColsWidth ) / 2, 0 ) ) ), ::cColorSpec )

            // doesn't need to be redrawn
            ::aRedraw[ nRow ] := .F.

            // Exit incremental row stabilization
            SetCursor( nOldCursor )
            return .F.
         endif

      next

      // If I reach this point I've repainted all rows so I can set ::lStable state
      if nRow > ::RowCount

         // If I have fewer records than available TBrowse rows, cursor cannot be lower than
         // last record (note ::lHitBottom is set only during a movement)
         if ::nLastRetrieved < ::nNewRowPos
            ::nNewRowPos := ::nLastRetrieved
         endif

         // If I'm not already under cursor I have to set data source to cursor position
         if ::nLastRetrieved != ::nNewRowPos
            Eval( ::bSkipBlock, ::nNewRowPos - ::nLastRetrieved )
            ::nLastRetrieved := ::nNewRowPos
         endif

         // new cursor position
         ::nRowPos := ::nNewRowPos

//       ::HitTop := ::lHitTop
//       ::HitBottom := ::lHitBottom

         if ::lAutoLite
            ::Hilite()
         else
            ::PosCursor()
         endif
         SetCursor( nOldCursor )
         ::lStable := .T.

         return .T.
      endif

   else
      /* NOTE: DBU relies upon current cell being reHilited() even if already stable */
      if ::lAutoLite
         ::Hilite()
      else
         ::PosCursor()
      endif
      SetCursor( nOldCursor )
      return .T.

   endif

   return .F.

/* NOTE: Incompatibility: In C5x this function will refresh the
         screen if a non-empty aRect was passed. It does this
         without skipping/reloading all the records, so I suspect
         some internal buffering. For now I left out this feature,
         so caller has to refresh manually. [vszakats] */

METHOD colorRect( aRect, aRectColor ) CLASS TBrowse

   if ISARRAY( aRect ) .and. ISARRAY( aRectColor )
      ::aRect       := aRect
      ::aRectColor  := aRectColor
   endif

   return Self

/* -------------------------------------------- */

METHOD PosCursor() CLASS TBrowse

   local nRow := ::n_Top + ::nRowPos + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
   local nCol
   local cType := ValType( Eval( ::aColumns[ ::nColPos ]:block ) )

   nCol := ::aColsPos[ ::nColPos ] + iif( cType == "L", ::aColsWidth[ ::nColPos ] / 2, 0 )

   // Put cursor on first char of cell value
   SetPos( nRow, nCol )

   #ifdef HB_COMPAT_C53
      ::n_Row := nRow
      ::n_Col := nCol
   #endif

   return Self

// Calculate how many columns fit on the browse width including ColSeps
METHOD HowManyCol() CLASS TBrowse

   local nToAdd
   local nWidth := ::n_Right - ::n_Left + 1

   local nColsVisible
   local nColsWidth

#ifdef COMMENT

   local saveColsWidth
   local tryLeftVisible
   local nLeftCol
   local oErr
   
   nColsWidth   := 0
   nColsVisible := 0

   if ::nFrozenCols > 0
      nColsVisible := 0
      do while nColsVisible < ::nFrozenCols .and. nColsVisible < ::nColumns
         nToAdd := ::aColsWidth[ nColsVisible + 1 ]

         if nColsVisible >= 1 .and. nColsVisible < ::nColumns .and. ::aColsWidth[ nColsVisible ] > 0
            nToAdd += iif( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL, Len( ::aColumns[ nColsVisible + 1 ]:ColSep ), Len( ::cColSep ) )
         endif

         if nColsWidth + nToAdd > nWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++
      enddo

      if nColsWidth + nToAdd > nWidth .and. nColsVisible < ::nFrozenCols
         /* NOTE: Why do I change frozen columns here? */
         ::freeze       := 0
         ::nColsWidth   := 0
         ::rightVisible := nColsVisible
         ::nColsVisible := nColsVisible
         return Self
      endif

      if ::leftVisible <= ::nFrozenCols
         ::leftVisible := ::nFrozenCols + 1
      endif

   endif

   // ;

   // BDj notes:
   // Cannot assume that ::leftVisible is correct
   // (eg. if ::nColPos was assigned ::rightVisible+1)
   // Must do the following in a loop repeatedly until:
   // (0) ::nColPos <= ::nFrozenCols (assume ::nColPos > 0)
   // or
   // (1) ::leftVisible <= ::nColPos <= ::rightVisible
   // or
   // (2) the above conditions are impossible (runtime error)

   saveColsWidth  := nColsWidth
   tryLeftVisible := ::leftVisible

   // ::nColPos is to the left of leftVisible
   if ::nFrozenCols == 0 .and. tryLeftVisible > ::nColPos
      tryLeftVisible := ::nColPos
   endif

   do while .T.

      nColsVisible := Max( 0, tryLeftVisible - 1 )

      do while nColsVisible < ::nColumns
         // which column is displayed to the left of next col?
         if ::nFrozenCols > 0 .and. nColsVisible + 1 == tryLeftVisible
            nLeftCol := ::nFrozenCols
         else
            nLeftCol := nColsVisible
         endif

         nToAdd := ::aColsWidth[ nColsVisible + 1 ]

         // next, we must check against nLeftCol, not nColsVisible:
         if ( nColsVisible >= tryLeftVisible .or. ::nFrozenCols > 0 ) .and.;
            ( nLeftCol > 0 ) .and.;
            ::aColsWidth[ nLeftCol ] > 0

            nToAdd += iif( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL, Len( ::aColumns[ nColsVisible + 1 ]:ColSep ), Len( ::cColSep ) )
         endif

         if nColsWidth + nToAdd > nWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++
      enddo

      // check: is ::nColPos fit within these calculated cols?
      if ::nColPos <= ::nFrozenCols .or.;
         ( tryLeftVisible <= ::nColPos .and. ::nColPos <= nColsVisible )
         exit
      endif

      // not ok. can retry?
      if tryLeftVisible == ::nColumns
         // cannot fit ::nColPos into display, generate Error TBROWSE
         oErr := ErrorNew()
         oErr:severity    := ES_ERROR
         oErr:genCode     := EG_LIMIT
         oErr:subSystem   := "TBROWSE"
         oErr:subCode     := 0
         oErr:description := "Width limit exceeded"
         oErr:canRetry    := .F.
         oErr:canDefault  := .F.
         oErr:fileName    := ""
         oErr:osCode      := 0
         Eval( ErrorBlock(), oErr )
      endif

      // retry until ::nColPos fit into display
      tryLeftVisible++
      nColsWidth := saveColsWidth

   enddo

   ::leftVisible  := Max( 1, tryLeftVisible )
   ::rightVisible := Max( 1, nColsVisible )
   ::nColsVisible := Max( 1, nColsVisible )
   ::nColsWidth   := nColsWidth

#endif

   nColsWidth := 0
   nColsVisible := 0
   
   if ::nFrozenCols > 0

      if ::leftVisible <= ::nFrozenCols
         ::leftVisible := ::nFrozenCols + 1
      endif

      do while nColsVisible < ::nFrozenCols

         nToAdd := ::aColsWidth[ nColsVisible + 1 ]

         if nColsVisible >= 1 .and. nColsVisible < ::nColumns
            nToAdd += iif( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL,;
                           Len( ::aColumns[ nColsVisible + 1 ]:ColSep ),;
                           Len( ::cColSep ) )
         endif

         if nColsWidth + nToAdd > nWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++
      enddo

      if nColsWidth > nWidth
         // NOTE: Why do I change frozen columns here?
         ::freeze := 0
         nColsWidth := 0
      endif
   endif

   nColsVisible := Max( ::leftVisible - 1, 0 )

   do while nColsVisible < ::nColumns

      nToAdd := ::aColsWidth[ nColsVisible + 1 ]

      if nColsVisible >= ::leftVisible .or. ::nFrozenCols > 0
         nToAdd += iif( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL, Len( ::aColumns[ nColsVisible + 1 ]:ColSep ), Len( ::cColSep ) )
      endif

      if nColsWidth + nToAdd > nWidth
         exit
      endif

      nColsWidth += nToAdd
      nColsVisible++
   enddo

   ::nColsVisible := Max( 1, nColsVisible )
   ::nColsWidth   := nColsWidth
   ::rightVisible := ::nColsVisible

   return Self

// Movement keys cause TBrowse to become unstable.
METHOD Moved() CLASS TBrowse

   // Internal flags used to set ::lHitTop/::lHitBottom during next stabilization
   ::lHitTop         := .F.
   ::lHitBottom      := .F.

   // No need to DeHilite() current cell more than once
   if ::lStable
      if ::lAutoLite
         ::DeHilite()
      else
         ::PosCursor()
      endif
      ::lStable := .F.
   endif

   return Self

METHOD DispCell( nRow, nCol, nMode ) CLASS TBrowse

   local oCol   := ::aColumns[ nCol ]
   local nWidth := ::aColsWidth[ nCol ]
   local ftmp   := Eval( oCol:block )
   local cType  := ValType( ftmp )
   local cPict  := iif( Empty( oCol:Picture ), "", oCol:Picture )

   local tmp

   local cColor

   if ! Empty( ::aRect ) .and. ;
      nCol >= ::aRect[ 2 ] .and. ;
      nCol <= ::aRect[ 4 ] .and. ;
      nRow >= ::aRect[ 1 ] .and. ;
      nRow <= ::aRect[ 3 ] .and. ;
      ! Empty( ::aRectColor )
      cColor := tbr_GetColor( ::cColorSpec, ::aRectColor, nMode )
   else
      cColor := tbr_GetColor( ::cColorSpec, iif( oCol:ColorBlock == NIL, oCol:defColor, Eval( oCol:ColorBlock, ftmp ) ), nMode )
   endif

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
         // Headers are aligned to bottom
         cStr := Replicate( ";", ::nHeaderHeight - hb_TokenCount( cStr, ";" ) + 1 ) + cStr

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
         for n := 0 to ::nFooterHeight - 1
            SetPos( nRow - n, nCol )
            DispOut( PadR( hb_TokenGet( @cStr, ::nFooterHeight - n, ";" ), nPadLen ), cColor )
         next

         SetPos( nRow, nCol + nPadLen )
      endif

   endif

   return Self

METHOD SetColumnWidth( oCol ) CLASS TBrowse

   local nWidth
   local xValue
   local cString
   local nTokenPos
   local nLen

   // if oCol has :Width property set I use it
   if oCol:width != NIL

      nWidth := oCol:width

   elseif ISBLOCK( oCol:block )

      xValue := Eval( oCol:block )
      nWidth := tbr_CalcWidth( xValue, ValType( xValue ), oCol:picture )

      cString := oCol:heading
      do while ( nLen := Len( hb_TokenPtr( @cString, @nTokenPos, ";" ) ) ) > 0
         nWidth := Max( nLen, nWidth )
      enddo

      cString := oCol:footing
      do while ( nLen := Len( hb_TokenPtr( @cString, @nTokenPos, ";" ) ) ) > 0
         nWidth := Max( nLen, nWidth )
      enddo

   else

      nWidth := 0

   endif

   return Min( nWidth, ::n_Right - ::n_Left + 1 )

// Gets TBrowse width and width of displayed columns plus colsep
METHOD redrawHeaders() CLASS TBrowse

   local n, nTPos, nBPos
   local cBlankBox := Space(9)
   local nScreenRowT
   local nScreenRowB
   local nLCS             // Len( ColSep )
   local nWidth := ::n_Right - ::n_Left + 1

   if ::lHeaders          // Drawing headers

      // Clear area of screen occupied by headers
      DispBox( ::n_Top, ::n_Left, ::n_Top + ::nHeaderHeight - 1, ::n_Right, cBlankBox, ::cColorSpec )

      // Set cursor at first field start of description
      SetPos( ::n_Top, ::n_Left + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Heading, ::aColsWidth[ n ], .T., tbr_GetColor( ::cColorSpec, ::aColumns[ n ]:defColor, TBC_CLR_HEADING ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[ n + 1 ]:ColSep != NIL, Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::cColSep ) ) )
         endif
      next
   endif

   if ! Empty( ::cHeadSep ) .and. ::lHeaders // Draw horizontal heading separator line
      DispOutAt( ( nScreenRowT := ::n_Top + ::nHeaderHeight ), ::n_Left,;
                Replicate( Right( ::cHeadSep, 1 ), nWidth ), ::cColorSpec )
   else
      nScreenRowT := NIL
   endif

   if ! Empty( ::cFootSep ) .and. ::lFooters // Draw horizontal footing separator line
      DispOutAt( ( nScreenRowB := ::n_Bottom - ::nFooterHeight ), ::n_Left,;
                Replicate( Right( ::cFootSep, 1 ), nWidth ), ::cColorSpec )
   else
      nScreenRowB := NIL
   endif

   nTPos := nBPos := ::n_Left + ( ( nWidth - ::nColsWidth ) / 2 )

   // Draw headin/footing column separator
   for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
      if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
         n := ::leftVisible
      endif

      if n < ::rightVisible

         nLCS := iif( ::aColumns[ n + 1 ]:ColSep != NIL, Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::cColSep ) )

         if nScreenRowT != NIL
            DispOutAt( nScreenRowT, ( nTPos += ::aColsWidth[ n ] ), Left( ::cHeadSep, nLCS ), ::cColorSpec )
            nTPos += nLCS
         endif

         if nScreenRowB != NIL
            DispOutAt( nScreenRowB, ( nBPos += ::aColsWidth[ n ] ), Left( ::cFootSep, nLCS ), ::cColorSpec )
            nBPos += nLCS
         endif

      endif
   next

   if ::lFooters                // Drawing footers

      // Clear area of screen occupied by footers
      DispBox( ::n_Bottom - ::nFooterHeight + 1, ::n_Left, ::n_Bottom, ::n_Right, cBlankBox, ::cColorSpec )

      // Set cursor at first field start of description
      SetPos( ::n_Bottom, ::n_Left + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Footing, ::aColsWidth[ n ], .F., tbr_GetColor( ::cColorSpec, ::aColumns[ n ]:defColor, TBC_CLR_FOOTING ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[ n + 1 ]:ColSep != NIL, Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::cColSep ) ) )
         endif
      next
   endif

   return Self

// NOTE: Not tested, could be broken
METHOD MGotoYX( nRow, nCol ) CLASS TBrowse

   local nColsLen
   local nI
   local nNewRow

   // Am I inside TBrowse display area ?
   if nRow > ::n_Top .and. nRow < ::n_Bottom .and. ;
      nCol > ::n_Left .and. nCol < ::n_Right

      // if not stable force repositioning of data source; maybe this is not first Stabilize() call after
      // TBrowse became unstable, but we need to call Stabilize() al least one time before moving again to be sure
      // data source is under cursor position
      if ! ::lStable
         ::Stabilize()
      else
         ::Moved()
      endif

      // Set new row position
      nNewRow := nRow - ::n_Top + iif( ::lHeaders, ::nHeaderHeight, 0 ) + iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      ::Stabilize()

      // Now move to column under nCol
      nColsLen := 0
      // NOTE: I don't think it is correct, have to look up docs
      nI := iif( ::nFrozenCols > 0, ::nFrozenCols, ::leftVisible )

      do while nColsLen < nCol .and. nI < ::rightVisible

         nColsLen += ::aColsWidth[ nI ]
         if nI >= 1 .and. nI < ::nColumns
            nColsLen += iif( ::aColumns[ nI ]:ColSep != NIL, Len( ::aColumns[ nI ]:ColSep ), Len( ::cColSep ) )
         endif

         nI++

      enddo

      ::nColPos := nI

      // Force redraw of current row with new cell position
      ::RefreshCurrent()

   endif

   return Self

/* -------------------------------------------- */

METHOD autoLite( lAutoLite ) CLASS TBrowse

   if ISLOGICAL( lAutoLite )
      ::lAutoLite := lAutoLite
   endif

   return ::lAutoLite

METHOD nTop( nTop ) CLASS TBrowse

   if nTop != NIL
      #ifdef HB_COMPAT_C53
         ::n_Top := _eInstVar( Self, "NTOP", nTop, "N", 1001 )
      #else
         ::n_Top := _eInstVar( Self, "NTOP", nTop, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::Configure( 2 )
   endif

   return ::n_Top

METHOD nLeft( nLeft ) CLASS TBrowse

   if nLeft != NIL
      #ifdef HB_COMPAT_C53
         ::n_Left := _eInstVar( Self, "NLEFT", nLeft, "N", 1001 )
      #else
         ::n_Left := _eInstVar( Self, "NLEFT", nLeft, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::Configure( 2 )
   endif

   return ::n_Left

METHOD nBottom( nBottom ) CLASS TBrowse

   if nBottom != NIL
      ::n_Bottom := _eInstVar( Self, "NBOTTOM", nBottom, "N", 1001, {| o, x | x >= o:nTop } )
      ::Configure( 2 )
   endif

   return ::n_Bottom

METHOD nRight( nRight ) CLASS TBrowse

   if nRight != NIL
      ::n_Right := _eInstVar( Self, "NRIGHT", nRight, "N", 1001, {| o, x | x >= o:nLeft } )
      ::Configure( 2 )
   endif

   return ::n_Right

METHOD colorSpec( cColorSpec ) CLASS TBrowse

   if cColorSpec != NIL
      ::cColorSpec := _eInstVar( Self, "COLORSPEC", cColorSpec, "C", 1001 )
   endif

   return ::cColorSpec

METHOD colSep( cColSep ) CLASS TBrowse

   if cColSep != NIL
      ::cColSep := _eInstVar( Self, "COLSEP", cColSep, "C", 1001 )
   endif

   return ::cColSep

METHOD footSep( cFootSep ) CLASS TBrowse

   if cFootSep != NIL
      ::cFootSep := _eInstVar( Self, "FOOTSEP", cFootSep, "C", 1001 )
   endif

   return ::cFootSep

METHOD headSep( cHeadSep ) CLASS TBrowse

   if cHeadSep != NIL
      ::cHeadSep := _eInstVar( Self, "HEADSEP", cHeadSep, "C", 1001 )
   endif

   return ::cHeadSep

/* NOTE: CA-Cl*pper has a bug where negative nColPos value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD colPos( nColPos ) CLASS TBrowse

   if PCount() > 0
      if ISNUMBER( nColPos )
         ::nColPos := nColPos
      else
         ::nColPos := 0
      endif
   endif

   return ::nColPos

/* NOTE: CA-Cl*pper has a bug where negative nRowPos value will be translated to 16bit unsigned int, 
         so the behaviour will be different in this case. [vszakats] */

METHOD rowPos( nRowPos ) CLASS TBrowse

   if PCount() > 0
      if ISNUMBER( nRowPos )
         ::nRowPos := iif( nRowPos < 1 .or. nRowPos > ::RowCount, ::RowCount, nRowPos )
         return nRowPos
      else
         ::nRowPos := ::RowCount
         return 0
      endif
   endif

   return ::nRowPos

METHOD goBottomBlock( bBlock ) CLASS TBrowse

   if bBlock != NIL
      /* NOTE: In CA-Cl*pper the string is: "GOBOTTOMBL" */
      ::bGoBottomBlock := _eInstVar( Self, "GOBOTTOMBLOCK", bBlock, "B", 1001 )
   endif

   return ::bGoBottomBlock

METHOD goTopBlock( bBlock ) CLASS TBrowse

   if bBlock != NIL
      ::bGoTopBlock := _eInstVar( Self, "GOTOPBLOCK", bBlock, "B", 1001 )
   endif

   return ::bGoTopBlock

METHOD hitBottom( lHitBottom ) CLASS TBrowse

   if PCount() > 0
      if ISLOGICAL( lHitBottom )
         ::lHitBottom := lHitBottom
      else
         return .T.
      endif
   endif

   return ::lHitBottom

METHOD hitTop( lHitTop ) CLASS TBrowse

   if PCount() > 0
      if ISLOGICAL( lHitTop )
         ::lHitTop := lHitTop
      else
         return .T.
      endif
   endif

   return ::lHitTop

METHOD stable( lStable ) CLASS TBrowse

   if PCount() > 0
      if ISLOGICAL( lStable )
         ::lStable := lStable
      else
         return .T.
      endif
   endif

   return ::lStable

METHOD skipBlock( bSkipBlock ) CLASS TBrowse

   if bSkipBlock != NIL
      ::bSkipBlock := _eInstVar( Self, "SKIPBLOCK", bSkipBlock, "B", 1001 )
   endif

   return ::bSkipBlock

#ifdef HB_COMPAT_C53

METHOD ApplyKey( nKey ) CLASS TBrowse

   return ::TApplyKey( nKey, self )

METHOD SetKey( nKey, bBlock ) CLASS TBrowse

   local bReturn
   local nPos

   // ; Assigned codeblock receives two parameters: {| oTBrowse, nKey | <action> }

   if ::aKeys == NIL
      ::aKeys := { { K_DOWN       , {| oB | oB:Down()    , TBR_CONTINUE } },;
                   { K_END        , {| oB | oB:End()     , TBR_CONTINUE } },;
                   { K_CTRL_PGDN  , {| oB | oB:GoBottom(), TBR_CONTINUE } },;
                   { K_CTRL_PGUP  , {| oB | oB:GoTop()   , TBR_CONTINUE } },;
                   { K_HOME       , {| oB | oB:Home()    , TBR_CONTINUE } },;
                   { K_LEFT       , {| oB | oB:Left()    , TBR_CONTINUE } },;
                   { K_PGDN       , {| oB | oB:PageDown(), TBR_CONTINUE } },;
                   { K_PGUP       , {| oB | oB:PageUp()  , TBR_CONTINUE } },;
                   { K_CTRL_END   , {| oB | oB:PanEnd()  , TBR_CONTINUE } },;
                   { K_CTRL_HOME  , {| oB | oB:PanHome() , TBR_CONTINUE } },;
                   { K_CTRL_LEFT  , {| oB | oB:PanLeft() , TBR_CONTINUE } },;
                   { K_CTRL_RIGHT , {| oB | oB:PanRight(), TBR_CONTINUE } },;
                   { K_RIGHT      , {| oB | oB:Right()   , TBR_CONTINUE } },;
                   { K_UP         , {| oB | oB:Up()      , TBR_CONTINUE } },;
                   { K_ESC        , {|    | TBR_EXIT } },;
                   { K_LBUTTONDOWN, {| oB | TBMouse( oB, MRow(), MCol() ) } } }

      #ifdef HB_EXTENSION
         AAdd( ::aKeys, { K_MWFORWARD  , {| oB | oB:Up()      , TBR_CONTINUE } } )
         AAdd( ::aKeys, { K_MWBACKWARD , {| oB | oB:Down()    , TBR_CONTINUE } } )
      #endif
   endif

   if ( nPos := AScan( ::aKeys, {| x | x[ 1 ] == nKey } ) ) == 0
      if ISBLOCK( bBlock )
         AAdd( ::aKeys, { nKey, bBlock } )
      endif
      bReturn := bBlock

   elseif ISBLOCK( bBlock )
      ::aKeys[ nPos ][ 2 ] := bBlock
      bReturn := bBlock

   elseif PCount() == 1
      bReturn := ::aKeys[ nPos ][ 2 ]

   elseif ( bReturn := ::aKeys[ nPos ][ 2 ], PCount() == 2 .and. ;
          bBlock == NIL .and. nKey != 0 )

      ADel( ::aKeys, nPos )
      ASize( ::aKeys, Len( ::aKeys ) - 1 )
   endif

   return bReturn

METHOD TApplyKey( nKey, oBrowse ) CLASS TBrowse

   local bBlock := oBrowse:setKey( nKey )

   DEFAULT bBlock TO oBrowse:setKey( 0 )

   if bBlock == NIL
      return TBR_EXCEPTION
   endif

   return Eval( bBlock, oBrowse, nKey )

METHOD hitTest( mRow, mCol ) CLASS TBrowse
   local i

   ::nmRowPos := ::nRowPos
   ::nmColPos := ::nColPos
  
   if mRow < ::rect[ 1 ] .or. mRow > ::rect[ 3 ]
      return HTNOWHERE
   endif
  
   if mCol < ::rect[ 2 ] .or. mCol > ::rect[ 4 ]
      return HTNOWHERE
   endif
  
   ::nmRowPos := mRow - ::rect[ 1 ] + 1
   for i := 1 to len( ::aVisibleCols )
       if ::aVisibleCols[ i ] > mcol
          exit
       endif
   next
  
   ::mColPos := ::aVisibleCols[ i ]

   return HTCELL

METHOD nRow() CLASS TBrowse
   return ::n_Row

METHOD nCol() CLASS TBrowse
   return ::n_Col

METHOD mRowPos() CLASS TBrowse
   return ::nmRowPos

METHOD mColPos() CLASS TBrowse
   return ::nmColPos

METHOD message( cMessage ) CLASS TBrowse

   if cMessage != NIL
      ::cMessage := _eInstVar( Self, "MESSAGE", cMessage, "C", 1001 )
   endif

   return ::cMessage

METHOD setStyle( nStyle, lNewValue ) CLASS TBrowse

   /* NOTE: CA-Cl*pper 5.3 does no checks on the value of nStyle, so in case 
            it is zero or non-numeric, a regular RTE will happen. [vszakats] */
  
   if nStyle > Len( ::aSetStyle ) .and. nStyle <= 4096 /* Some reasonable limit for maximum number of styles */
      ASize( ::aSetStyle, nStyle )
   endif
  
   if ISLOGICAL( lNewValue )
      ::aSetStyle[ nStyle ] := lNewValue
   endif

   return ::aSetStyle[ nStyle ]

FUNCTION TBMouse( oBrowse, nMouseRow, nMouseCol )
   local n

   if oBrowse:hitTest( nMouseRow, nMouseCol ) == HTCELL

      n := oBrowse:mRowPos - oBrowse:nRowPos

      do while n < 0
         n++
         oBrowse:up():forceStable()
      enddo

      do while n > 0
         n--
         oBrowse:down():forceStable()
      enddo

      n := oBrowse:mColPos - oBrowse:nColPos
      if n < oBrowse:leftVisible - oBrowse:colPos .and. oBrowse:freeze + 1 < oBrowse:leftVisible
         n += oBrowse:freeze + 1 - oBrowse:leftVisible // hidden columns
      endif

      do while n < 0
         n++
         oBrowse:left()
      enddo

      do while n > 0
         n--
         oBrowse:right()
      enddo

      return TBR_CONTINUE
   endif

   return TBR_EXCEPTION

#endif

/* -------------------------------------------- */

METHOD New( nTop, nLeft, nBottom, nRight ) CLASS TBrowse

   DEFAULT nTop    TO 0
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO MaxRow()
   DEFAULT nRight  TO MaxCol()

   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight

   #ifdef HB_COMPAT_C53
      ::rect := { ::n_Top, ::n_Left, ::n_Bottom, ::n_Right }
   #endif

   return Self

FUNCTION TBrowseNew( nTop, nLeft, nBottom, nRight )
   return TBrowse():New( nTop, nLeft, nBottom, nRight )

/* -------------------------------------------- */

/* NOTE: nMode can be 1/2 or 1/2/3/4 when compiled with HB_COMPAT_C53 (default) [vszakats] */
STATIC FUNCTION tbr_GetColor( cColorSpec, aDefColor, nMode )
   return hb_ColorIndex( cColorSpec, iif( ISARRAY( aDefColor ) .and. nMode <= Len( aDefColor ), aDefColor, { 1, 2, 1, 1 } )[ nMode ] - 1 )

STATIC FUNCTION tbr_CalcWidth( xValue, cType, cPicture )

   do case
   case cType $ "CM" ; return Len( iif( Empty( cPicture ), xValue        , Transform( xValue, cPicture ) ) )
   case cType == "N" ; return Len( iif( Empty( cPicture ), Str( xValue ) , Transform( xValue, cPicture ) ) )
   case cType == "D" ; return Len( iif( Empty( cPicture ), DToC( xValue ), Transform( xValue, cPicture ) ) )
   case cType == "L" ; return 1
   endcase

   return 0
