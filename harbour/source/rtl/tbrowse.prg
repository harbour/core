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
 * Copyright 2007 Viktor Szakats <harbour.01 syenar.hu>
 *    tbr_CookColor(), tbr_GetColor()
 *
 * See doc/license.txt for licensing terms.
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

/* NOTE: These TBColumn properties are _not_ cached inside TBrowse:
         :picture, :block, :colorBlock */

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"
#include "common.ch"
#include "error.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "tbrowse.ch"

/* TBColumn info constants */
#define TBCI_OBJ                1    // Object TBColumn
#define TBCI_WIDTH              2    // Column Width
#define TBCI_WIDTHCELL          3    // Width of the Cell
#define TBCI_HEADING            4    // Column Headings
#define TBCI_FOOTING            5    // Column Footings
#define TBCI_COLSEP             6    // Column Seperator
#define TBCI_SEPWIDTH           7    // Width of the Separator
#define TBCI_DEFCOLOR           8    // Array with index of color
#define TBCI_SETWIDTH           9    // If True, only SetFrozen can change TBCI_WIDTH
#define TBCI_LCOLSEP            10   // Should column separator be drawn
#define TBCI_SCRCOLPOS          11   // Temporary column position on screen

//#define TBCI_COLOBJECT       1   // column object
//#define TBCI_CELLWIDTH       2   // width of the cell
//#define TBCI_COLWIDTH        3   // width of the column
//#define TBCI_SCRCELLPOS      4   // cell position on screen
//#define TBCI_SCRCOLPOS       5   // column position on screen
//#define TBCI_SEPWIDTH        6   // width of the separator

#define TBC_CLR_STANDARD        1    // first index value to set unselected data color.
#define TBC_CLR_ENHANCED        2    // second index value to set selected data color.
#ifdef HB_COMPAT_C53
#define TBC_CLR_HEADING         3    // third index value to set heading color.
#define TBC_CLR_FOOTING         4    // fourth index value to set footing color.
#define TBC_CLR_MAX_            4
#else
#define TBC_CLR_HEADING         TBC_CLR_STANDARD
#define TBC_CLR_FOOTING         TBC_CLR_STANDARD
#define TBC_CLR_MAX_            2
#endif

/* NOTE: In CA-Cl*pper TBROWSE class does not inherit from any other classes
         and there is no public class function like TBrowse(). There is 
         in XPP though. */ 

#if defined(HB_C52_STRICT) && !defined(HB_COMPAT_XPP)
CREATE CLASS TBrowse STATIC
#else
CREATE CLASS TBrowse
#endif

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
   METHOD hitTest( nMRow, nMCol )
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
   METHOD border( cBorder ) SETGET
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
   VAR cBorder                                              // Character value defining characters drawn around object (C5.3)
   VAR cColorSpec                                           // Color table for the TBrowse display
   VAR aColorSpec                                           // Color table for the TBrowse display (preprocessed)
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
   VAR aColsInfo        INIT {}                             // Array with column data
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
   VAR lHiLited         INIT .F.
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

   METHOD InitColumn( oCol, lAddColumn )
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
#ifdef HB_COMPAT_C53
   local nLeft
   local nRight
#endif

   // ; Fill the column info array
/*
   local xVal

   if nMode == 2

      for n := 1 to ::nColumns
      
         xVal := Eval( ::aColumns[ n ]:block )
      
         aCol[ TBCI_HEADING   ] := ::aColumns[ n ]:heading
         aCol[ TBCI_FOOTING   ] := ::aColumns[ n ]:footing
         aCol[ TBCI_WIDTH     ] := ::SetColumnWidth( ::aColumns[ n ] )
         aCol[ TBCI_WIDTHCELL ] := Min( aCol[ TBCI_WIDTH ], tbr_CalcWidth( xVal, ValType( xValue ), ::aColumns[ n ]:picture ) )
         aCol[ TBCI_COLSEP    ] := iif( aCol[ TBCI_OBJ ]:ColSep != NIL, aCol[ TBCI_OBJ ]:ColSep, ::ColSep )
         aCol[ TBCI_DEFCOLOR  ] := tbr_DefColor( ::aColumns[ n ]:defColor, ::aColorSpec )
         aCol[ TBCI_SEPWIDTH  ] := Len( aCol[ TBCI_COLSEP ] )
         aCol[ TBCI_LCOLSEP   ] := aCol[ TBCI_WIDTH ] > 0
         aCol[ TBCI_COLSEP    ] := iif( aCol[ TBCI_OBJ ]:ColSep != NIL, aCol[ TBCI_OBJ ]:ColSep, ::ColSep )
      next
   endif
*/
   // ;

   ::lHeaders := .F.
   ::lFooters := .F.
   ::lRedrawFrame := .T.

   if nMode == 2 .AND. ::nColumns == 1
      ::leftVisible := 1
   endif
   if ::nColumns < ::nFrozenCols
      ::nFrozenCols := 0
   endif

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

      // ...

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

   if nMode == 2
      ::HowManyCol()
   endif

   #ifdef HB_COMPAT_C53

      nLeft := ::n_Left
      nRight := ::n_Right

      ::rect := { ::n_Top    + iif( ::lHeaders, ::nHeaderHeight + iif( Empty( ::cHeadSep ), 0, 1 ), 0 ),;
                  ::n_Left,;
                  ::n_Bottom - iif( ::lFooters, ::nFooterHeight - iif( Empty( ::cFootSep ), 0, 1 ), 0 ),;
                  ::n_Right }

      for n := nLeft To nRight
         AAdd( ::aVisibleCols, n )
      next

   #endif

   return Self

// Adds a TBColumn object to the TBrowse object
METHOD addColumn( oCol ) CLASS TBrowse

   ::Moved() /* TOFIX: This logic should go inside ::configure() */

   ::nColumns++

   AAdd( ::aColumns, oCol )
   AAdd( ::aColsWidth, ::SetColumnWidth( oCol ) )
   AAdd( ::aColsPos, 0 )
   AAdd( ::aColsInfo, ::InitColumn( oCol, .T. ) )

   ::Configure( 2 )

   return Self

// Insert a column object in a browse
METHOD insColumn( nPos, oCol ) CLASS TBrowse

   if nPos >= 1

      ::Moved() /* TOFIX: This logic should go inside ::configure() */
      
      if nPos > ::nColumns

         /* NOTE: CA-Cl*pper doesn't do this, but crashes instead. */

         ::nColumns++

         AAdd( ::aColumns, oCol )
         AAdd( ::aColsWidth, ::SetColumnWidth( oCol ) )
         AAdd( ::aColsPos, 0 )
         AAdd( ::aColsInfo, ::InitColumn( oCol, .F. ) )

      else

         ::nColumns++
         
         ASize( ::aColumns, ::nColumns )
         AIns( ::aColumns, nPos )
         ASize( ::aColsWidth, ::nColumns )
         AIns( ::aColsWidth, nPos )
         ASize( ::aColsPos, ::nColumns )
         AIns( ::aColsPos, nPos )
         ASize( ::aColsInfo, ::nColumns )
         AIns( ::aColsInfo, ::InitColumn( oCol, .F. ) )

         ::aColumns[ nPos ] := oCol
         ::aColsWidth[ nPos ] := ::SetColumnWidth( oCol )
         ::aColsPos[ nPos ] := 0
         ::aColsInfo[ nPos ] := ::InitColumn( oCol, .F. )

      endif
   
      ::Configure( 2 )

   endif

   return oCol

// Replaces one TBColumn object with another
METHOD setColumn( nPos, oCol ) CLASS TBrowse

   LOCAL oOldCol

   /* NOTE: CA-Cl*pper doesn't check this, but crashes instead. */

   if nPos >= 1 .and. nPos <= ::nColumns

      ::Moved() /* TOFIX: This logic should go inside ::configure() */

      oOldCol := ::aColumns[ nPos ]

      ::aColumns[ nPos ] := oCol
      ::aColsWidth[ nPos ] := ::SetColumnWidth( oCol )
      ::aColsPos[ nPos ] := 0
      ::aColsInfo[ nPos ] := ::InitColumn( oCol, .F. )

      ::Configure( 2 )

   endif

   /* NOTE: CA-Cl*pper 5.2 NG says this will return the previously set 
            column, but it's returning Self instead. In C5.3 this bug 
            was fixed and it works as expected (except when wrong
            parameter is passed, when it returns NIL). [vszakats] */
#ifdef HB_C52_STRICT
   return Self
#else
   return oOldCol
#endif

METHOD delColumn( nPos ) CLASS TBrowse

   local oCol := ::aColumns[ nPos ] /* NOTE: To keep CA-Cl*pper compatible runtime error generation. [vszakats] */

   ::Moved() /* TOFIX: This logic should go inside ::configure() */

   /* Need to adjust variables in case last column is deleted. */

   /* TOFIX: This logic should go inside ::configure() */

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
   ADel( ::aColsInfo, nPos )
   ASize( ::aColsInfo, ::nColumns )

   ::Configure( 2 )

   return oCol

// Gets a specific TBColumn object
METHOD getColumn( nColumn ) CLASS TBrowse
   return iif( nColumn > 0 .and. nColumn <= ::nColumns, ::aColumns[ nColumn ], NIL )

// Returns the display width of a particular column
METHOD colWidth( nColumn ) CLASS TBrowse
   return iif( nColumn > 0 .and. nColumn <= ::nColumns, ::aColsWidth[ nColumn ], 0 )

METHOD colCount() CLASS TBrowse
   return Len( ::aColumns )

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

   DO WHILE !::Stabilize()
   ENDDO

   RETURN Self

METHOD deHilite() CLASS TBrowse

   local nRow
   LOCAL nCol

   IF ::rowPos < 1 .OR. ::rowPos > ::rowCount
      ::rowPos := 0
   ELSEIF ::nColPos > 0 .AND. ::nColPos <= Len( ::aColumns )

      nRow := ::n_Top +; // TOFIX
              ::nRowPos +;
              iif( ::lHeaders, ::nHeaderHeight, 0 ) +;
              iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
      nCol := ::aColsPos[ ::nColPos ] // TOFIX
      
      SetPos( nRow, nCol ) // TOFIX
      nCol += ::DispCell( ::nRowPos, ::nColPos, TBC_CLR_STANDARD )
      SetPos( nRow, nCol )
   ENDIF

   ::lHiLited := .F.

   RETURN Self

METHOD hilite() CLASS TBrowse

   LOCAL nRow
   LOCAL nCol

   IF ::rowPos < 1 .OR. ::rowPos > ::rowCount
      ::rowPos := 0
   ELSEIF ::nColPos >= 1 .AND. ::nColPos <= Len( ::aColumns )

      nRow := ::n_Top +; // TOFIX
              ::nRowPos +;
              iif( ::lHeaders, ::nHeaderHeight, 0 ) +;
              iif( Empty( ::cHeadSep ) .or. ! ::lHeaders, 0, 1 ) - 1
      nCol := ::aColsPos[ ::nColPos ] // TOFIX
      
      SetPos( nRow, nCol )
      nCol += ::DispCell( ::nRowPos, ::nColPos, TBC_CLR_ENHANCED )
      SetPos( nRow, nCol )

      ::lHiLited := .T.
   ENDIF
      
   #ifdef HB_COMPAT_C53
      ::n_Row := nRow
      ::n_Col := nCol
   #endif

   RETURN Self

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

      if !Empty( ::cBorder )
         /* NOTE: Intentionally the external version of coordinate messages. */
         DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cBorder, ::cColorSpec[ 1 ] )
      endif

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
                      Space( ( nWidth - ::nColsWidth ) / 2 ), ::aColorSpec[ 1 ] )

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
                  DispOut( Space( ::aColsWidth[ n ] ), tbr_GetColor( ::aColorSpec, ::aColumns[ n ]:defColor, TBC_CLR_STANDARD ) )
               endif

               if n < ::rightVisible
                  if ::aColumns[ n + 1 ]:ColSep != NIL
                     DispOut( ::aColumns[ n + 1 ]:ColSep, ::aColorSpec[ 1 ] )

                  elseif ::cColSep != NIL
                     DispOut( ::cColSep, ::aColorSpec[ 1 ] )

                  endif
               endif
            next

            DispOut( Space( Int( Round( ( nWidth - ::nColsWidth ) / 2, 0 ) ) ), ::aColorSpec[ 1 ] )

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

METHOD InitColumn( oCol, lAddColumn ) CLASS TBrowse

   IF !lAddColumn .AND. ISOBJECT( oCol ) .AND. ISBLOCK( oCol:block )
      RETURN {;
         oCol                          ,; // TBCI_OBJ      
         ::SetColumnWidth( oCol )      ,; // TBCI_WIDTH    
         0                             ,; // TBCI_WIDTHCELL
         ""                            ,; // TBCI_HEADING  
         ""                            ,; // TBCI_FOOTING  
         ""                            ,; // TBCI_COLSEP   
         0                             ,; // TBCI_SEPWIDTH 
         oCol:defColor                 ,; // TBCI_DEFCOLOR
         .F.                           ,; // TBCI_SETWIDTH 
         .T.                           ,; // TBCI_LCOLSEP
         0                             }  // TBCI_SCRCOLPOS
   ENDIF

   RETURN {;
      oCol                          ,; // TBCI_OBJ      
      0                             ,; // TBCI_WIDTH    
      0                             ,; // TBCI_WIDTHCELL
      ""                            ,; // TBCI_HEADING  
      ""                            ,; // TBCI_FOOTING  
      ""                            ,; // TBCI_COLSEP   
      0                             ,; // TBCI_SEPWIDTH 
      {}                            ,; // TBCI_DEFCOLOR
      .F.                           ,; // TBCI_SETWIDTH 
      .T.                           ,; // TBCI_LCOLSEP  
      0                             }  // TBCI_SCRCOLPOS

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

   LOCAL oCol     := ::aColumns[ nCol ]    // TOFIX
   LOCAL nWidth   := ::aColsWidth[ nCol ]  // TOFIX
   LOCAL ftmp     := Eval( oCol:block )
   LOCAL cType    := ValType( ftmp )
   LOCAL cPicture := oCol:Picture
   LOCAL nSkip    := 0
   LOCAL aDefColor
   LOCAL cColor

   IF !ISCHARACTER( cPicture )
      cPicture := ""
   ENDIF

   IF ! Empty( ::aRect ) .AND. ; // TOFIX: aRect validation ?
      nCol >= ::aRect[ 2 ] .AND. ;
      nCol <= ::aRect[ 4 ] .AND. ;
      nRow >= ::aRect[ 1 ] .AND. ;
      nRow <= ::aRect[ 3 ] .AND. ;
      ! Empty( ::aRectColor ) // TOFIX: ISEMPTY ?
      cColor := tbr_GetColor( ::aColorSpec, ::aRectColor, nMode )
   ELSE
      /* NOTE: Not very optimal that we're evaluating this block all the time. 
               But CA-Cl*pper always has a block here, and there is no other way 
               to tell if the code in it is NIL (the default) or something valuable. 
               [vszakats] */
      aDefColor := Eval( oCol:colorBlock, ftmp )
      cColor := tbr_GetColor( ::aColorSpec, iif( ISARRAY( aDefColor ), aDefColor, oCol:defColor ), nMode ) // TOFIX: ISARRAY ?
   ENDIF

   SWITCH cType
   CASE "C"
   CASE "M"
      DispOut( PadR( Transform( ftmp, cPicture ), nWidth ), cColor )
      EXIT

   CASE "N"
      DispOut( PadL( Transform( ftmp, cPicture ), nWidth ), cColor )
      EXIT

   CASE "D"
      DispOut( PadR( Transform( ftmp, iif( cPicture == "", "@D", cPicture ) ), nWidth ), cColor )
      EXIT

   CASE "L"
      ftmp := PadC( iif( ftmp, "T", "F" ), nWidth )
      nSkip := nWidth - Len( LTrim( ftmp ) ) - 1
      DispOut( Space( Len( ftmp ) - Len( LTrim( ftmp ) ) ), ::aColorSpec[ 1 ] )
      DispOut( ftmp, cColor )
      DispOut( Space( Len( ftmp ) - Len( RTrim( ftmp ) ) ), ::aColorSpec[ 1 ] )
      EXIT

   OTHERWISE
      DispOut( Space( nWidth ), cColor )

   ENDSWITCH

   RETURN nSkip

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
         cStr := Replicate( ";", ::nHeaderHeight - hb_TokenCount( cStr, ";" ) ) + cStr

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
   local nColor

   if ::lHeaders // Drawing headers

      // Clear area of screen occupied by headers
      DispBox( ::n_Top, ::n_Left, ::n_Top + ::nHeaderHeight - 1, ::n_Right, cBlankBox, ::aColorSpec[ 1 ] )

      if Empty( ::cHeadSep ) // Draw horizontal heading separator line
         nScreenRowT := NIL
         /* ; NOTE: This is a bug in CA-Cl*pper 5.3. [vszakats] */
         nColor := TBC_CLR_STANDARD
      else
         DispOutAt( ( nScreenRowT := ::n_Top + ::nHeaderHeight ), ::n_Left,;
                   Replicate( Right( ::cHeadSep, 1 ), nWidth ), ::aColorSpec[ 1 ] )
         nColor := TBC_CLR_HEADING
      endif

      // Set cursor at first field start of description
      SetPos( ::n_Top, ::n_Left + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Heading, ::aColsWidth[ n ], .T., tbr_GetColor( ::aColorSpec, ::aColumns[ n ]:defColor, nColor ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[ n + 1 ]:ColSep != NIL, Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::cColSep ) ) )
         endif
      next
   endif

   if ::lFooters // Drawing footers

      // Clear area of screen occupied by footers
      DispBox( ::n_Bottom - ::nFooterHeight + 1, ::n_Left, ::n_Bottom, ::n_Right, cBlankBox, ::aColorSpec[ 1 ] )

      if Empty( ::cFootSep ) // Draw horizontal footing separator line
         nScreenRowB := NIL
         /* ; NOTE: This is a bug in CA-Cl*pper 5.3. [vszakats] */
         nColor := TBC_CLR_STANDARD
      else
         DispOutAt( ( nScreenRowB := ::n_Bottom - ::nFooterHeight ), ::n_Left,;
                   Replicate( Right( ::cFootSep, 1 ), nWidth ), ::aColorSpec[ 1 ] )
         nColor := TBC_CLR_FOOTING
      endif

      // Set cursor at first field start of description
      SetPos( ::n_Bottom, ::n_Left + ( ( nWidth - ::nColsWidth ) / 2 ) )

      for n := iif( ::nFrozenCols > 0, 1, ::leftVisible ) to ::rightVisible
         if ::nFrozenCols > 0 .and. n == ::nFrozenCols + 1
            n := ::leftVisible
         endif

         ::WriteMLineText( ::aColumns[ n ]:Footing, ::aColsWidth[ n ], .F., tbr_GetColor( ::aColorSpec, ::aColumns[ n ]:defColor, nColor ) )

         if n < ::rightVisible
            // Set cursor at start of next field description
            SetPos( Row(), Col() + iif( ::aColumns[ n + 1 ]:ColSep != NIL, Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::cColSep ) ) )
         endif
      next
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
            DispOutAt( nScreenRowT, ( nTPos += ::aColsWidth[ n ] ), Left( ::cHeadSep, nLCS ), ::aColorSpec[ 1 ] )
            nTPos += nLCS
         endif

         if nScreenRowB != NIL
            DispOutAt( nScreenRowB, ( nBPos += ::aColsWidth[ n ] ), Left( ::cFootSep, nLCS ), ::aColorSpec[ 1 ] )
            nBPos += nLCS
         endif

      endif
   next

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
      if ::lStable
         ::Moved()
      else
         ::stabilize()
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

   IF nTop != NIL
      #ifdef HB_COMPAT_C53
         ::n_Top := _eInstVar( Self, "NTOP", nTop, "N", 1001 )
         IF !Empty( ::cBorder )
            ::n_Top++
         ENDIF
      #else
         ::n_Top := _eInstVar( Self, "NTOP", nTop, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::Configure( 2 )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Top - 1
      ENDIF
   #endif

   RETURN ::n_Top

METHOD nLeft( nLeft ) CLASS TBrowse

   IF nLeft != NIL
      #ifdef HB_COMPAT_C53
         ::n_Left := _eInstVar( Self, "NLEFT", nLeft, "N", 1001 )
         IF !Empty( ::cBorder )
            ::n_Left++
         ENDIF
      #else
         ::n_Left := _eInstVar( Self, "NLEFT", nLeft, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::Configure( 2 )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Left - 1
      ENDIF
   #endif

   RETURN ::n_Left

METHOD nBottom( nBottom ) CLASS TBrowse

   IF nBottom != NIL
      ::n_Bottom := _eInstVar( Self, "NBOTTOM", nBottom, "N", 1001, {| o, x | x >= o:nTop } )
      #ifdef HB_COMPAT_C53
         IF !Empty( ::cBorder )
            ::n_Bottom--
         ENDIF
      #endif
      ::Configure( 2 )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Bottom + 1
      ENDIF
   #endif

   RETURN ::n_Bottom

METHOD nRight( nRight ) CLASS TBrowse

   IF nRight != NIL
      ::n_Right := _eInstVar( Self, "NRIGHT", nRight, "N", 1001, {| o, x | x >= o:nLeft } )
      #ifdef HB_COMPAT_C53
         IF !Empty( ::cBorder )
            ::n_Right--
         ENDIF
      #endif
      ::Configure( 2 )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Right + 1
      ENDIF
   #endif

   RETURN ::n_Right

METHOD colorSpec( cColorSpec ) CLASS TBrowse

   if cColorSpec != NIL
      ::cColorSpec := _eInstVar( Self, "COLORSPEC", cColorSpec, "C", 1001 )
      ::aColorSpec := tbr_CookColor( ::cColorSpec )
      ::Configure( 1 )
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

#define _TBC_SETKEY_KEY         1
#define _TBC_SETKEY_BLOCK       2

METHOD setKey( nKey, bBlock ) CLASS TBrowse

   LOCAL bReturn
   LOCAL nPos

   /* NOTE: Assigned codeblock receives two parameters:
            {| oTBrowse, nKey | <action> } */

   IF ::aKeys == NIL
      ::aKeys := { { K_DOWN       , {| o | o:Down()    , TBR_CONTINUE   } },;
                   { K_END        , {| o | o:End()     , TBR_CONTINUE   } },;
                   { K_CTRL_PGDN  , {| o | o:GoBottom(), TBR_CONTINUE   } },;
                   { K_CTRL_PGUP  , {| o | o:GoTop()   , TBR_CONTINUE   } },;
                   { K_HOME       , {| o | o:Home()    , TBR_CONTINUE   } },;
                   { K_LEFT       , {| o | o:Left()    , TBR_CONTINUE   } },;
                   { K_PGDN       , {| o | o:PageDown(), TBR_CONTINUE   } },;
                   { K_PGUP       , {| o | o:PageUp()  , TBR_CONTINUE   } },;
                   { K_CTRL_END   , {| o | o:PanEnd()  , TBR_CONTINUE   } },;
                   { K_CTRL_HOME  , {| o | o:PanHome() , TBR_CONTINUE   } },;
                   { K_CTRL_LEFT  , {| o | o:PanLeft() , TBR_CONTINUE   } },;
                   { K_CTRL_RIGHT , {| o | o:PanRight(), TBR_CONTINUE   } },;
                   { K_RIGHT      , {| o | o:Right()   , TBR_CONTINUE   } },;
                   { K_UP         , {| o | o:Up()      , TBR_CONTINUE   } },;
                   { K_ESC        , {|   |               TBR_EXIT       } },;
                   { K_LBUTTONDOWN, {| o | TBMouse( o, MRow(), MCol() ) } } }

      #ifdef HB_EXTENSION
         AAdd( ::aKeys, { K_MWFORWARD  , {| o | o:Up()      , TBR_CONTINUE   } } )
         AAdd( ::aKeys, { K_MWBACKWARD , {| o | o:Down()    , TBR_CONTINUE   } } )
      #endif
   ENDIF

   IF ( nPos := AScan( ::aKeys, {| x | x[ _TBC_SETKEY_KEY ] == nKey } ) ) == 0
      IF ISBLOCK( bBlock )
         AAdd( ::aKeys, { nKey, bBlock } )
      ENDIF
      bReturn := bBlock
   ELSEIF ISBLOCK( bBlock )
      ::aKeys[ nPos ][ _TBC_SETKEY_BLOCK ] := bBlock
      bReturn := bBlock
   ELSEIF PCount() == 1
      bReturn := ::aKeys[ nPos ][ _TBC_SETKEY_BLOCK ]
   ELSE
      bReturn := ::aKeys[ nPos ][ _TBC_SETKEY_BLOCK ]
      IF PCount() == 2 .AND. bBlock == NIL .AND. nKey != 0
         ADel( ::aKeys, nPos )
         ASize( ::aKeys, Len( ::aKeys ) - 1 )
      ENDIF
   ENDIF

   RETURN bReturn

METHOD applyKey( nKey ) CLASS TBrowse

   LOCAL bBlock := ::setKey( nKey )

   DEFAULT bBlock TO ::setKey( 0 )

   IF bBlock == NIL
      RETURN TBR_EXCEPTION
   ENDIF

   RETURN Eval( bBlock, Self, nKey )

METHOD hitTest( nMRow, nMCol ) CLASS TBrowse
   local i

   ::nmRowPos := ::nRowPos
   ::nmColPos := ::nColPos
  
   if nMRow < ::rect[ 1 ] .or. nMRow > ::rect[ 3 ]
      return HTNOWHERE
   endif
  
   if nMCol < ::rect[ 2 ] .or. nMCol > ::rect[ 4 ]
      return HTNOWHERE
   endif
  
   ::nmRowPos := nMRow - ::rect[ 1 ] + 1

   for i := 1 to Len( ::aVisibleCols )
      if nMCol < ::aVisibleCols[ i ]
         exit
      endif
   next
  
   ::mColPos := ::aVisibleCols[ i ]

   return HTCELL

METHOD border( cBorder ) CLASS TBrowse

   IF PCount() > 0

      cBorder := _eInstVar( Self, "BORDER", cBorder, "C", 1001 )

      IF Len( cBorder ) == 0 .OR. ;
         Len( cBorder ) == 8
      
         IF Empty( ::cBorder ) .AND. !Empty( cBorder )
            ::n_Top++
            ::n_Left++
            ::n_Bottom--
            ::n_Right--
            ::configure( 2 )
         ELSEIF !Empty( ::cBorder ) .AND. Empty( cBorder )
            ::n_Top--
            ::n_Left--
            ::n_Bottom++
            ::n_Right++
            ::configure( 2 )
         ENDIF

         ::cBorder := cBorder
      ENDIF
   ENDIF

   RETURN ::cBorder

METHOD nRow() CLASS TBrowse
   return ::n_Row

METHOD nCol() CLASS TBrowse
   return ::n_Col

METHOD mRowPos() CLASS TBrowse
   return ::nmRowPos

METHOD mColPos() CLASS TBrowse
   return ::nmColPos

METHOD message( cMessage ) CLASS TBrowse

   IF cMessage != NIL
      ::cMessage := _eInstVar( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD setStyle( nStyle, lNewValue ) CLASS TBrowse

   /* NOTE: CA-Cl*pper 5.3 does no checks on the value of nStyle, so in case 
            it is zero or non-numeric, a regular RTE will happen. [vszakats] */
  
   IF nStyle > Len( ::aSetStyle ) .AND. nStyle <= 4096 /* Some reasonable limit for maximum number of styles */
      ASize( ::aSetStyle, nStyle )
   ENDIF
  
   IF ISLOGICAL( lNewValue )
      ::aSetStyle[ nStyle ] := lNewValue
   ENDIF

   RETURN ::aSetStyle[ nStyle ]

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

   ::cColorSpec := SetColor()
   ::aColorSpec := tbr_CookColor( ::cColorSpec )

   #ifdef HB_COMPAT_C53
      ::rect := { ::n_Top, ::n_Left, ::n_Bottom, ::n_Right }
   #endif

   return Self

FUNCTION TBrowseNew( nTop, nLeft, nBottom, nRight )
   return TBrowse():New( nTop, nLeft, nBottom, nRight )

/* -------------------------------------------- */

/* NOTE: Preprocess user-supplied colorstring for internal usage. This is 
         needed to keep full C5.x compatibility while maintaining performace.
         C5.x would always have at least two items, defaulted to the 
         current SetColor() values, the rest of the items are defaulted 
         to "N/N". [vszakats] */
STATIC FUNCTION tbr_CookColor( cColorSpec )

   LOCAL nCount := Max( hb_TokenCount( cColorSpec, "," ), 2 )
   LOCAL aColorSpec := Array( nCount )
   LOCAL cColor
   LOCAL nPos

   FOR nPos := 1 TO nCount
      cColor := hb_TokenGet( @cColorSpec, nPos, "," )
      IF nPos <= 2
         aColorSpec[ nPos ] := iif( hb_ColorToN( cColor ) == 0 .AND. !( Upper( StrTran( cColor, " ", "" ) ) == "N/N" ), hb_ColorIndex( "W/N,N/W", nPos - 1 ), cColor )
      ELSE
         aColorSpec[ nPos ] := iif( hb_ColorToN( cColor ) == 0, "N/N", cColor )
      ENDIF
   NEXT

   RETURN aColorSpec

/* NOTE: Preprocess defColor so that it can be used "blindly" afterwards. */
STATIC FUNCTION tbr_DefColor( aDefColor, aColorSpec )

   IF !ISARRAY( aDefColor )
      aDefColor := {}
   ENDIF

   ASize( aDefColor, TBC_CLR_MAX_ )

   IF !ISNUMBER( aDefColor[ TBC_CLR_STANDARD ] ) .OR. aDefColor[ TBC_CLR_STANDARD ] > Len( aColorSpec )
      aDefColor[ TBC_CLR_STANDARD ] := 1
   ENDIF
   IF !ISNUMBER( aDefColor[ TBC_CLR_ENHANCED ] ) .OR. aDefColor[ TBC_CLR_ENHANCED ] > Len( aColorSpec )
      aDefColor[ TBC_CLR_ENHANCED ] := 2
   ENDIF
#ifdef HB_COMPAT_C53
   /* NOTE: To be backwards compatible, C5.3 will fall back to C5.2 colors 
            if the extra HEADING/FOOTING positions are not specified. [vszakats] */
   IF !ISNUMBER( aDefColor[ TBC_CLR_HEADING ] ) .OR. aDefColor[ TBC_CLR_HEADING ] > Len( aColorSpec )
      aDefColor[ TBC_CLR_HEADING ] := aDefColor[ TBC_CLR_STANDARD ]
   ENDIF
   IF !ISNUMBER( aDefColor[ TBC_CLR_FOOTING ] ) .OR. aDefColor[ TBC_CLR_FOOTING ] > Len( aColorSpec )
      aDefColor[ TBC_CLR_FOOTING ] := aDefColor[ TBC_CLR_STANDARD ]
   ENDIF
#endif

   RETURN aDefColor

/* NOTE: Strict sanity check for a color array. We need to use this 
         for the array returned by a :colorBlock. */
STATIC FUNCTION tbr_GetColor( aColorSpec, aDefColor, nMode )

   IF !ISARRAY( aDefColor )
      /* NOTE: This fits both C5.2 and C5.3. In C5.2 nMode is 1 or 2. [vszakats] */
      RETURN aColorSpec[ { 1, 2, 1, 1 }[ nMode ] ]
   ELSEIF nMode > Len( aDefColor )
      /* NOTE: C5.3 and C5.2 compatible method. To be backwards compatible, 
               C5.3 will fall back to C5.2 colors if the extra HEADING/FOOTING 
               positions are not specified. [vszakats] */
      SWITCH NMODE
      CASE TBC_CLR_STANDARD ; RETURN aColorSpec[ 1 ]
      CASE TBC_CLR_ENHANCED ; RETURN aColorSpec[ 2 ]
      CASE TBC_CLR_HEADING  ; RETURN aColorSpec[ iif( Len( aDefColor ) >= TBC_CLR_STANDARD .AND. ISNUMBER( aDefColor[ TBC_CLR_STANDARD ] ) .AND. aDefColor[ 1 ] <= Len( aColorSpec ), aDefColor[ TBC_CLR_STANDARD ], 1 ) ]
      CASE TBC_CLR_FOOTING  ; RETURN aColorSpec[ iif( Len( aDefColor ) >= TBC_CLR_STANDARD .AND. ISNUMBER( aDefColor[ TBC_CLR_STANDARD ] ) .AND. aDefColor[ 1 ] <= Len( aColorSpec ), aDefColor[ TBC_CLR_STANDARD ], 1 ) ]
      ENDSWITCH
   ENDIF

   RETURN aColorSpec[ iif( ISNUMBER( aDefColor[ nMode ] ) .AND. aDefColor[ nMode ] <= Len( aColorSpec ), aDefColor[ nMode ], { 1, 2, 1, 1 }[ nMode ] ) ]

STATIC FUNCTION tbr_CalcWidth( xValue, cType, cPicture )

   IF !ISCHARACTER( cPicture )
      cPicture := ""
   ENDIF

   SWITCH cType
   CASE "M"
   CASE "C" ; RETURN Len( iif( Empty( cPicture ), xValue        , Transform( xValue, cPicture ) ) )
   CASE "N" ; RETURN Len( iif( Empty( cPicture ), Str( xValue ) , Transform( xValue, cPicture ) ) )
   CASE "D" ; RETURN Len( iif( Empty( cPicture ), DToC( xValue ), Transform( xValue, cPicture ) ) )
   CASE "L" ; RETURN 1
   ENDSWITCH

   RETURN 0
