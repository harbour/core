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
 * Copyright 2000, 2001 Maurilio Longo <maurilio.longo@libero.it>
 * Cursor movement handling and stabilization loop
 * ::PageUp(), ::PageDown(), ::Down(), ::Up(), ::GoBottom(), ::GoTop(), ::Stabilize()
 * ::GotoXY()
 *
 * Copyright 2001 Manu Exposito <maex14@dipusevilla.es>
 * Activate data PICTURE DispCell(nColumn, nColor)
 *
 */


/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible. [vszakats] */

/* TODO: Fix TBrowse when AutoLite = .F. 
         This is also needed to make dbEdit() work correctly. [vszakats] */

/* TODO: :firstScrCol() --> nScreenCol
         Determines screen column where the first table column is displayed.
         Xbase++ compatible method */

/* TODO: :viewArea() --> aViewArea
         Determines the coordinates for the data area of a TBrowse object.
         Xbase++ compatible method */

/* TOFIX: Clipper will determine the column width when the TBROWSE is displayed
          at the first time. (while Harbour does that when adding the column)
          Clipper will leave NIL in the :width variable until determined. Also
          Clipper will not allow the user to assign a NIL to the :width
          variable. Clipper will determine the width even when the caller
          explicitly set the :width after adding the column. [vszakats] */

/* TOFIX: Multiline headers and footer are not supported. [vszakats] */

#include "common.ch"
#include "hbclass.ch"
#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

CLASS TBrowse

   DATA aColumns              // Array to hold all browse columns
   DATA autoLite              // Logical value to control highlighting
   DATA cargo                 // User-definable variable
   DATA colorSpec             // Color table for the TBrowse display
   DATA colPos                // Current cursor column position
   DATA colSep                // Column separator character
   DATA footSep               // Footing separator character
   DATA freeze                // Number of columns to freeze
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
   DATA skipBlock             // Code block used to reposition data source
   DATA stable                // Indicates if the TBrowse object is stable

#ifdef HB_COMPAT_C53
   DATA aKeys
#endif

   METHOD New(nTop, nLeft, nBottom, nRight)                 // Constructor
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
   METHOD ColCount() INLINE Len(::aColumns)
   METHOD ColorRect()                                       // Alters the color of a rectangular group of cells

   METHOD ColWidth( nColumn ) INLINE;                       // Returns the display width of a particular column
          iif( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ::aColumns[ nColumn ]:Width, NIL )
   METHOD Configure( nMode )                                // Reconfigures the internal settings of the TBrowse object
                                                            // nMode is an undocumented parameter in CA-Cl*pper

   METHOD DeHilite()                                        // Dehighlights the current cell
   METHOD DelColumn( nPos )                                 // Delete a column object from a browse
   METHOD ForceStable()                                     // Performs a full stabilization
   METHOD GetColumn( nColumn ) INLINE;                      // Gets a specific TBColumn object
          iif( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ::aColumns[ nColumn ], NIL )

   METHOD Hilite()                                          // Highlights the current cell
   METHOD InsColumn( nPos, oCol ) INLINE;                   // Insert a column object in a browse
          ASize( ::aColumns, Len( ::aColumns + 1 ) ), AIns( ::aColumns, nPos ),;
          ::aColumns[ nPos ] := oCol, ::Configure( 2 ), oCol
   METHOD Invalidate()                                      // Forces entire redraw during next stabilization
   METHOD RefreshAll()                                      // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent() INLINE;                          // Causes the current row to be refilled and repainted on next stabilize
          ::aRedraw[ ::RowPos ] := .T., ::stable := .F., Self
   METHOD SetColumn( nColumn, oCol ) INLINE;                // Replaces one TBColumn object with another
          iif( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ::aColumns[ nColumn ] := oCol, NIL ), oCol
   METHOD Stabilize()                                       // Performs incremental stabilization

#ifdef HB_COMPAT_C53
   METHOD SetKey(nKey, bBlock)
   METHOD ApplyKey(nKey)
   METHOD InitKeys(Self)
   METHOD TApplyKey(nKey, o)
#endif

   PROTECTED:     /* P R O T E C T E D */

   METHOD MGotoYX(nRow, nCol)             // Given screen coordinates nRow, nCol sets TBrowse cursor on underlaying cell
                                          // _M_GotoXY because this method will mostly be called to handle mouse requests

   HIDDEN:         /* H I D D E N */

   METHOD LeftDetermine()                 // Determine leftmost unfrozen column in display
   METHOD DispCell(nColumn, nColor)       // Displays a single cell
   METHOD HowManyCol(nWidth)              // Counts how many cols can be displayed
   METHOD RedrawHeaders(nWidth)           // Repaints TBrowse Headers
   METHOD Moved()                         // Every time a movement key is issued I need to reset certain properties
                                          // of TBrowse, I do these settings inside this method

   DATA aRect                             // The rectangle specified with ColorRect()
   DATA aRectColor                        // The color positions to use in the rectangle specified with ColorRect()
   DATA aRedraw                           // Array of logical items indicating, is appropriate row need to be redraw
   DATA lHeaders                          // Internal variable which indicates whether there are column footers to paint
   DATA lFooters                          // Internal variable which indicates whether there are column footers to paint
   DATA lRedrawFrame                      // True if I need to redraw Headers/Footers
   DATA nColsWidth                        // Total width of visible columns plus ColSep
   DATA nColsVisible                      // Number of columns that fit on the browse width
   DATA lHitTop                           // Internal Top/Bottom reached flag
   DATA lHitBottom
   DATA nRecsToSkip                       // Recs to skip on next Stabilize()
   DATA nNewRowPos                        // Next position of data source (after first phase of stabilization)
   DATA nLastRetrieved                    // Position, relative to first row, of last retrieved row (with an Eval(::SkipBlock, n))

ENDCLASS


METHOD New(nTop, nLeft, nBottom, nRight) CLASS TBrowse

   default  nTop    to 0
   default  nLeft   to 0
   default  nBottom to MaxRow()
   default  nRight  to MaxCol()

   ::aColumns        := {}
   ::AutoLite        := .T.
   ::leftVisible     := 1
   ::ColPos          := 1
   ::Freeze          := 0
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


   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight

return Self


METHOD Invalidate() CLASS TBrowse

   AFill(::aRedraw, .T.)
   ::stable := .F.
   ::lRedrawFrame := .T.

return Self


METHOD RefreshAll() CLASS TBrowse

   AFill(::aRedraw, .T.)
   ::stable := .F.

return Self


METHOD Configure(nMode) CLASS TBrowse

   local n

   ::lHeaders := .F.
   ::lFooters := .F.
   ::lRedrawFrame := .T.

   // Are there any column header to paint ?
   for n := 1 to Len(::aColumns)
      if !Empty(::aColumns[n]:Heading)
         ::lHeaders := .T.
         exit
      endif
   next

   // Are there any column footer to paint ?
   for n := 1 to Len(::aColumns)
      if !Empty(::aColumns[n]:Footing)
         ::lFooters := .T.
         exit
      endif
   next

   // 20/nov/2000 - maurilio.longo@libero.it
   // If I add (or remove) header or footer separator I have to change number
   // of available rows
   ::RowCount := ::nBottom - ::nTop + 1 - iif( ::lHeaders, 1, 0 ) - ;
                  iif( ::lFooters, 1, 0 ) - iif( Empty( ::HeadSep ), 0, 1 ) - ;
                  iif( Empty( ::FootSep ), 0, 1 )

   if Len(::aRedraw) <> ::RowCount
      ::aRedraw := Array(::RowCount)
   endif

   ::Invalidate()

return Self

// Adds a TBColumn object to the TBrowse object
METHOD AddColumn( oCol ) CLASS TBrowse

   local nWidthMax := ::nRight - ::nLeft + 1  // Visible width of the browse

   if oCol:Width > nWidthMax
      // with values lower than -4 it SIGSEVs here and there :-(
      oCol:Width := nWidthMax - 4
   endif

   AAdd( ::aColumns, oCol )
   ::Configure( 2 )

return Self


METHOD DelColumn( nPos ) CLASS TBrowse

   local oCol := ::aColumns[ nPos ]
   local n

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, Len( ::aColumns ) - 1 )

   ::Configure( 2 )

return oCol


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

   local nToEnd

   ::Moved()

   Eval(::goBottomBlock)
   nToEnd := Abs(Eval(::SkipBlock, -::RowCount))
   ::nNewRowPos := nToEnd
   ::nLastRetrieved := ::RowCount - nToEnd
   ::RefreshAll()

return Self


METHOD GoTop() CLASS TBrowse

   ::Moved()

   Eval(::goTopBlock)
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
      if ::ColPos < Len( ::aColumns )
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
      if ::ColPos <= Max(::leftVisible, ::Freeze) .AND. ::ColPos > 1
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
   local nWidth := 0
   local nCol

   if ::Freeze > 0
      for nCol := 1 TO ::Freeze
         nWidth += ::aColumns[ nCol ]:Width
         if nCol < Len( ::aColumns )
            nWidth += iif( ::aColumns[ nCol + 1 ]:ColSep != NIL,;
               Len( ::aColumns[ nCol + 1 ]:ColSep ),;
               Len( ::ColSep ) )
         endif
      next
   endif

   for nCol := ::rightVisible to ::Freeze + 1 step -1

      nWidth += ::aColumns[ nCol ]:Width +;
         iif( ::aColumns[ nCol ]:ColSep != NIL,;
            Len( ::aColumns[ nCol ]:ColSep ),;
            Len( ::ColSep ) )

      if nWidth > nWidthMax
         exit
      endif

   next

return nCol + 1


METHOD PageDown() CLASS TBrowse

   ::Moved()

   ::nRecsToSkip := (::RowCount - ::RowPos) + ::RowCount

return Self


METHOD PageUp() CLASS TBrowse

   ::Moved()

   ::nRecsToSkip := - ((::RowPos - 1) + ::RowCount)

return Self


METHOD PanEnd() CLASS TBrowse

   ::Moved()

   if ::ColPos < Len( ::aColumns )
      if ::rightVisible < Len( ::aColumns )
         ::rightVisible := Len( ::aColumns )
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
      if ::leftVisible > ::Freeze + 1
         ::leftVisible := ::Freeze + 1
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

   if ::leftVisible > ::Freeze + 1
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

   if ::rightVisible < Len( ::aColumns )
      ::rightVisible++
      ::leftVisible := ::LeftDetermine()
      ::ColPos := Min( ::leftVisible + n, ::rightVisible )
      ::lRedrawFrame := .T.
      ::RefreshAll()
   endif

return Self


METHOD DeHilite() CLASS TBrowse

   local nRow := ::nTop + ::RowPos - iif( ::lHeaders, 0, 1 ) + iif( Empty( ::HeadSep ), 0, 1 )
   local nCol := ::aColumns[ ::ColPos ]:ColPos

   SetPos( nRow, nCol )
   ::DispCell(::ColPos, 1)

return Self


METHOD ForceStable() CLASS TBrowse

   while !::Stabilize()
   end

return Self


METHOD Hilite() CLASS TBrowse

   local nColor
   local cColor
   local nRow
   local nCol

   if ::AutoLite
      nRow := ::nTop + ::RowPos - iif( ::lHeaders, 0, 1 ) + iif( Empty( ::HeadSep ), 0, 1 )
      nCol := ::aColumns[ ::ColPos ]:ColPos

      SetPos( nRow, nCol )
      ::DispCell(::ColPos, 2)
   /*QUESTION : Is this the correct fix when autolite is .f. */
   else
      nRow := ::nTop + ::RowPos - iif( ::lHeaders, 0, 1 ) + iif( Empty( ::HeadSep ), 0, 1 )
      nCol := ::aColumns[ ::ColPos ]:ColPos

      SetPos( nRow, nCol )
      ::DispCell(::ColPos, 3)

   endif

return Self

// Calculate how many columns fit on the browse width including ColSeps
METHOD HowManyCol(nWidth) CLASS TBrowse

   local nToAdd

   // They were locals, so now I need to clear them (should fix this)
   ::nColsWidth := 0
   ::nColsVisible := 0

   if ::Freeze > 0
      if ::leftVisible <= ::Freeze
         ::leftVisible := ::Freeze + 1
      endif

      ::nColsVisible := 0
      while ::nColsVisible < ::Freeze

         nToAdd := ::aColumns[ ::nColsVisible + 1 ]:Width

         if ::nColsVisible >= 1 .and. ::nColsVisible < Len( ::aColumns )
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
         ::Freeze := 0
         ::nColsWidth := 0
      endif
   endif

   ::nColsVisible := ::leftVisible - 1

   while ::nColsVisible < Len( ::aColumns )

      nToAdd := ::aColumns[ ::nColsVisible + 1 ]:Width

      if ::nColsVisible >= ::leftVisible .or. ::Freeze > 0
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
METHOD RedrawHeaders(nWidth) CLASS TBrowse

   local n, iW

   if ::lHeaders          // Drawing headers
      DispOutAt( ::nTop, ::nLeft, Space( ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )
      for n := iif(::Freeze > 0, 1, ::leftVisible) to ::rightVisible
         if ::Freeze > 0 .and. n == ::Freeze + 1
            n := ::leftVisible
         endif
         DispOut( PadR( ::aColumns[ n ]:Heading, ::aColumns[ n ]:Width ), ::ColorSpec )
         if n < ::rightVisible
            DispOut( Space( iif( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                     Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) ) ), ::ColorSpec )
         endif
      next

      // Every time there is some space to fill up and this space is an odd number of chars we need to
      // round it up to next value here since first DispOutAt() does round it down because it uses a simple
      // division (ie Space(7/2) gives 3 on first call and 4 here).
      DispOut(Space(Int(Round((nWidth - ::nColsWidth) / 2, 0))), ::ColorSpec)
   endif

   if ! Empty( ::HeadSep )  //Drawing heading separator
      DispOutAt( ::nTop + iif( ::lHeaders, 1, 0 ), ::nLeft, Replicate( Right( ::HeadSep, 1 ), ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )
      if Len( ::HeadSep ) > 1
         iW := 0
         for n := iif( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
            if ::Freeze > 0 .and. n == ::Freeze + 1
               n := ::leftVisible
            endif
            DispOut( Replicate( Right( ::HeadSep, 1 ), ::aColumns[ n ]:Width - iW ), ::ColorSpec )
            if n < ::rightVisible
               DispOut( Left( ::HeadSep, Len( ::HeadSep ) - 1 ), ::ColorSpec )
               iW := Len( ::HeadSep ) - 1 - iif( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                     Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) )
            endif
         next
      else
         DispOut( Replicate( ::HeadSep, ::nColsWidth ), ::ColorSpec )
      endif
      DispOut( Replicate( Right( ::HeadSep, 1 ), Int(Round((nWidth - ::nColsWidth) / 2, 0)) ), ::ColorSpec )
   endif

   if ! Empty( ::FootSep ) // Drawing footing separator
      DispOutAt( ::nBottom - iif( ::lFooters, 1, 0 ), ::nLeft, Replicate( Right( ::FootSep, 1 ), ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )
      if Len( ::FootSep ) > 1
         iW := 0
         for n := iif( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
            if ::Freeze > 0 .and. n == ::Freeze + 1
               n := ::leftVisible
            endif
            DispOut( Replicate( Right( ::FootSep, 1 ), ::aColumns[ n ]:Width - iW ), ::ColorSpec )
            if n < ::rightVisible
               DispOut( Left( ::FootSep, Len( ::FootSep ) - 1 ), ::ColorSpec )
               iW := Len( ::FootSep ) - 1 - iif( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                     Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) )
            endif
         next
      else
         DispOut( Replicate( ::FootSep, ::nColsWidth ), ::ColorSpec )
      endif
      DispOut( Replicate( Right( ::FootSep, 1 ), Int(Round((nWidth - ::nColsWidth) / 2, 0)) ), ::ColorSpec )
   endif

   if ::lFooters                // Drawing footers
      DispOutAt( ::nBottom, ::nLeft, Space( ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )
      for n := iif( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
         if ::Freeze > 0 .and. n == ::Freeze + 1
            n := ::leftVisible
         endif
         DispOut( PadR( ::aColumns[ n ]:Footing, ::aColumns[ n ]:Width ), ::ColorSpec )
         if n < ::rightVisible
            DispOut( Space( iif( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                     Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) ) ), ::ColorSpec )
         endif
      next
      DispOut(Space(Int(Round((nWidth - ::nColsWidth) / 2, 0))), ::ColorSpec)
   endif

return Self


METHOD Stabilize() CLASS TBrowse

   local nRow, nCol, n
   local nWidth := ::nRight - ::nLeft + 1 // Visible width of the browse
   local cColColor                        // Column color to use
   local oStartCol, oEndCol
   local lDisplay                      // Is there something to show inside current cell?
   local nRecsSkipped                  // How many records do I really skipped?
   local nFirstRow                     // Where is on screen first row of TBrowse?
   local nOldCursor                    // Current shape of cursor (which I remove before stabilization)


   nOldCursor := SetCursor(SC_NONE)

   if ::lRedrawFrame
      // How may columns fit on TBrowse width?
      ::HowManyCol(nWidth)
      ::RedrawHeaders(nWidth)

      // Now that browser frame has been redrawn we don't need to redraw it unless
      // displayed columns change
      ::lRedrawFrame := .F.

   else
      oStartCol := ::aColumns[ iif( ::rightVisible != 0, ::rightVisible, 1 ) ]
      oEndCol := ::aColumns[ iif( ::Freeze > 0, 1, ::leftVisible ) ]
      ::nColsWidth := iif( oStartCol != NIL, oStartCol:ColPos, 0 ) + ;
        iif( oStartCol != NIL, oStartCol:Width, 0 ) - oEndCol:ColPos

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
            Eval(::SkipBlock, ::nNewRowPos - ::nLastRetrieved)
            ::nLastRetrieved := ::nNewRowPos
         endif

         nRecsSkipped := Eval(::SkipBlock, ::nRecsToSkip)

         // I've tried to move past top or bottom margin
         if nRecsSkipped == 0

            if ::nRecsToSkip > 0
               ::lHitBottom := .T.

            elseif ::nRecsToSkip < 0
               ::lHitTop := .T.

            else //::nRecsToSkip == 0
               //
            endif

         elseif nRecsSkipped == ::nRecsToSkip

            // If after movement I'm still inside present TBrowse
            if (::nNewRowPos + nRecsSkipped >= 1) .AND. (::nNewRowPos + nRecsSkipped <= ::RowCount)
               ::nNewRowPos += nRecsSkipped
               ::nLastRetrieved := ::nNewRowPos
               // This is needed since present TBrowse has no cache, so I need to repaint current row
               // rereading it from data source and to force rereading from data source I have to mark
               // row as invalid
               ::aRedraw[::nNewRowPos] := .T.

            else
               // It was K_PGDN or K_PGUP
               if Abs(nRecsSkipped) >= ::RowCount

                  // K_PGDN
                  if nRecsSkipped > 0
                     ::nLastRetrieved := ::RowCount

                  else // K_PGUP
                     ::nLastRetrieved := 1

                  endif
                  ::RefreshAll()

               else // K_DN or K_UP

                  // Where does really start first TBrowse row?
                  nFirstRow := ::nTop + iif( ::lHeaders, 1, 0 ) + iif( Empty( ::HeadSep ), 0, 1 )

                  // I'm at top or bottom of TBrowse so I can scroll
                  if ::nNewRowPos == ::RowCount
                     Scroll(nFirstRow + nRecsSkipped - 1, ::nLeft, nFirstRow + ::RowCount - 1, ::nRight, nRecsSkipped)
                     ::nLastRetrieved := ::RowCount

                  else
                     Scroll(nFirstRow, ::nLeft, nFirstRow + ::RowCount + nRecsSkipped, ::nRight, nRecsSkipped)
                     ::nLastRetrieved := 1

                  endif

                  // I've scrolled on screen rows, now I need to scroll ::aRedraw array as well!
                  if nRecsSkipped > 0
                     for nRow := 2 to Len(::aRedraw)
                        ::aRedraw[nRow - 1] := ::aRedraw[nRow]
                     next
                  else
                     for nRow := (Len(::aRedraw) - 1) to 1 step -1
                        ::aRedraw[nRow + 1] := ::aRedraw[nRow]
                     next
                  endif

                  ::aRedraw[::nNewRowPos] := .T.
               endif
            endif

         else // I couldn't move as far as requested

            // I need to refresh all rows if I go past current top or bottom row
            if (::nNewRowPos + nRecsSkipped < 1) .OR. (::nNewRowPos + nRecsSkipped > ::RowCount)
               // don't go past boundaries
               ::nNewRowPos := iif(nRecsSkipped > 0, ::RowCount, 1)
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
         SetCursor(nOldCursor)
         return .F.

      endif

      // Data source is alredy at correct record number, now we need
      // to repaint browser accordingly.
      for nRow := 1 to ::RowCount

         // if there is a row to repaint
         if ::aRedraw[nRow]

            DispOutAt(::nTop + nRow + iif( ::lHeaders, 0, -1 ) + iif( Empty( ::HeadSep ), 0, 1 ), ::nLeft,;
                      Space( ( nWidth - ::nColsWidth ) / 2 ), ::ColorSpec )

            for n := iif( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible

               if ::Freeze > 0 .and. n == ::Freeze + 1
                  n := ::leftVisible
               endif
               if nRow == 1
                  ::aColumns[ n ]:ColPos := Col()
               endif

               nCol := Col()

               // NOTE: If my TBrowse has 20 rows but I have only 3 recs, clipper clears
               //       remaining 17 rows in a single operation, I will, instead, try to skip
               //       17 times. Should be made more clever.
               if nRow <> ::nLastRetrieved
                  if lDisplay := Eval(::SkipBlock, nRow - ::nLastRetrieved) == (nRow - ::nLastRetrieved)
                     ::nLastRetrieved := nRow
                  endif
               else
                  lDisplay := .T.
               endif

               if lDisplay
                  ::DispCell(n, 1)
                  SetPos( Row(), nCol + ::aColumns[ n ]:Width )

               else
                  // Clear cell
                  DispOut( Space( ::aColumns[ n ]:Width ), ::ColorSpec )

               endif

               if n < ::rightVisible
                  if ::aColumns[ n + 1 ]:ColSep != NIL
                     DispOut( ::aColumns[ n + 1 ]:ColSep, ::ColorSpec )
                  elseif ::ColSep != NIL
                     DispOut( ::ColSep, ::ColorSpec )
                  endif
               endif
            next

            DispOut(Space(Int(Round((nWidth - ::nColsWidth) / 2, 0))), ::ColorSpec)

            // doesn't need to be redrawn
            ::aRedraw[nRow] := .F.

            // Exit incremental row stabilization
            SetCursor(nOldCursor)
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
            Eval(::SkipBlock, ::nNewRowPos - ::nLastRetrieved)
            ::nLastRetrieved := ::nNewRowPos
         endif

         // new cursor position
         ::RowPos := ::nNewRowPos

         ::HitTop := ::lHitTop
         ::HitBottom := ::lHitBottom

         ::Hilite()
         SetCursor(nOldCursor)
         ::stable := .T.

         return .T.
      endif

   else
      /* NOTE: DBU relies upon current cell being reHilited() even if already stable */
      ::Hilite()
      SetCursor(nOldCursor)
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


METHOD ColorRect( aRect, aRectColor ) CLASS TBrowse

   ::aRect       := aRect
   ::aRectColor  := aRectColor

return Self


METHOD DispCell( nColumn, nColor ) CLASS TBrowse

   local oCol   := ::aColumns[nColumn]
   LOCAL ftmp   := Eval(oCol:block)
   LOCAL nCol   := Col()
   LOCAL cType  := ValType( ftmp )
   local cPict  := if( !empty( oCol:Picture ), oCol:Picture, "@" )
   local cDisp  := ""
   local cColor := iif(oCol:ColorBlock != NIL,;
                       hb_ColorIndex(::ColorSpec, Eval(oCol:ColorBlock, ftmp)[nColor] - 1),;
                       hb_ColorIndex(::ColorSpec, nColor - 1))

   do case
      case cType $ "CM"
         cDisp := transform( Left( ftmp, ::aColumns[ nColumn ]:Width ), cPict )

      case cType == "N"
         cDisp := Right( transform(  ftmp, cPict ), ::aColumns[ nColumn ]:Width )

      case cType == "D"
         cPict := if( cPict == "@", "@D", cPict )
         cDisp := Right( transform(  ftmp, cPict ), ::aColumns[ nColumn ]:Width )

      case cType == "L"
         cDisp := Space( ::aColumns[ nColumn ]:Width / 2 ) + iif( ftmp, "T","F" )
   endcase

   DispOut( cDisp, cColor )
   DispOut( Space( nCol + ::aColumns[ nColumn ]:Width - Col() ), cColor)

   // Logical fields are centered on column width
//   SetPos( Row(), nCol + if( cType == "L", ::aColumns[::ColPos]:Width / 2, 0 ) )

return Self


#ifdef HB_COMPAT_C53

METHOD ApplyKey(nKey)  CLASS TBrowse

return ::TApplyKey(nKey, self)


METHOD InitKeys(o) CLASS TBROWSE

   Default o:aKeys to {{K_DOWN,{|Ob,nKey| Ob:Down(),0}},;
              {K_END,{|Ob,nKey| Ob:End(),0}},;
              {K_CTRL_PGDN,{|Ob,nKey| Ob:GoBottom(),0}},;
              {K_CTRL_PGUP,{|Ob,nKey| Ob:GoTop(),0}},;
              {K_HOME,{|Ob,nKey| Ob:Home(),0}},;
              {K_LEFT,{|Ob,nKey| Ob:Left(),0}},;
              {K_PGDN,{|Ob,nKey| Ob:PageDown(),0}},;
              {K_PGUP,{|Ob,nKey| Ob:PageUp(),0}},;
              {K_CTRL_END,{|Ob,nKey| Ob:PanEnd(),0}},;
              {K_CTRL_HOME,{|Ob,nKey| Ob:PanHome(),0}},;
              {K_CTRL_LEFT,{|Ob,nKey| Ob:PanLeft(),0}},;
              {K_CTRL_RIGHT,{|Ob,nKey| Ob:PanRight(),0}},;
              {K_RIGHT,{|Ob,nKey| Ob:Right(),0}},;
              {K_UP,{|Ob,nKey| Ob:Up(),0}},;
              {K_ESC,{|Ob,nKey| -1 }}}
return o


METHOD SetKey(nKey,bBlock) CLASS TBrowse

   local bReturn,nPos

   ::InitKeys(self)

   if (nPos:=ascan(::aKeys,{|x| x[1]==nkey}))==0
      if ( ISBLOCK( bBlock ) )
         bReturn:= bBlock
         aadd(::aKeys,{nKey,bBlock})
      endif
      bReturn:=bBlock

   elseif (ISBLOCK(bBlock))
      ::aKeys[npos][2]:=bBlock
      bReturn:=bBlock

   elseif PCOUNT()==1
      bReturn:= ::aKeys[npos][2]

   elseif ( bReturn:= ::aKeys[ nPos ][ 2 ], PCount() == 2 .AND. ;
           ISNIL( bBlock ) .AND. nKey != 0 )
      adel(::aKeys, nPos)
      asize(::akeys, Len(::aKeys) - 1)
   endif

return bReturn


METHOD TApplyKey( nKey, oBrowse ) CLASS tBrowse

   local bBlock := oBrowse:setkey(nKey), nReturn

   default bBlock to oBrowse:setkey(0)

   if ( ISNIL( bBlock ) )
      nReturn := 1
   else
      nReturn := eval(bBlock, oBrowse, nKey)
   endif

return nReturn
#endif


// NOTE: Not tested, could be broken
METHOD MGotoYX(nRow, nCol) CLASS TBrowse

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
      nNewRow := nRow - ::nTop + iif(::lHeaders, 0, -1) + iif(Empty(::HeadSep), 0, 1)
      ::nRecsToSkip := nNewRow - ::nNewRowPos

      // move data source accordingly
      ::Stabilize()

      // Now move to column under nCol
      nColsLen := 0
      // NOTE: I don't think it is correct, have to look up docs
      nI := iif(::Freeze > 0, ::Freeze, ::leftVisible)

      while nColsLen < nCol .AND. nI < ::rightVisible

         nColsLen += ::aColumns[nI]:Width
         if nI >= 1 .AND. nI < Len( ::aColumns )
            nColsLen += iif(::aColumns[nI]:ColSep != NIL, Len(::aColumns[nI]:ColSep), Len(::ColSep))
         endif

         nI++

      enddo

      ::ColPos := nI

      // Force redraw of current row with new cell position
      ::RefreshCurrent()

   endif

return Self


function TBrowseNew(nTop, nLeft, nBottom, nRight)

return TBrowse():New(nTop, nLeft, nBottom, nRight)

