/*
 * $Id$
 */

/*
 * Harbour Class TBrowse
 * Copyright(C) 1999 by Antonio Linares <alinares@fivetech.com>
 * Portions Copyright(c) 1999 by Alexander S.Kresin <alex@belacy.belgorod.su>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "classes.ch"
#include "color.ch"

CLASS TBrowse

   DATA aColumns      // Array to hold all browse columns
   DATA autoLite      // Logical value to control highlighting
   DATA cargo         // User-definable variable
   DATA colorSpec     // Color table for the TBrowse display
   DATA colPos        // Current cursor column position
   DATA colSep        // Column separator character
   DATA footSep       // Footing separator character
   DATA freeze        // Number of columns to freeze
   DATA goBottomBlock // Code block executed by TBrowse:goBottom()
   DATA goTopBlock    // Code block executed by TBrowse:goTop()
   DATA headSep       // Heading separator character
   DATA hitBottom     // Indicates the end of available data
   DATA hitTop        // Indicates the beginning of available data
   DATA leftVisible   // Indicates position of leftmost unfrozen column in display
   DATA nBottom       // Bottom row number for the TBrowse display
   DATA nLeft         // Leftmost column for the TBrowse display
   DATA nRight        // Rightmost column for the TBrowse display
   DATA nTop          // Top row number for the TBrowse display
   DATA rightVisible  // Indicates position of rightmost unfrozen column in display
   DATA rowCount      // Number of visible data rows in the TBrowse display
   DATA rowPos        // Current cursor row position
   DATA skipBlock     // Code block used to reposition data source
   DATA stable        // Indicates if the TBrowse object is stable
   DATA aRedraw       // Array of logical items indicating, is appropriate row need to be redraw
   DATA RelativePos   // Indicates record position relatively position of first record on the screen

   METHOD New()            // Constructor
   METHOD Down()           // Moves the cursor down one row
   METHOD End()            VIRTUAL // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()               // Repositions the data source to the bottom of file
   METHOD GoTop()                  // Repositions the data source to the top of file
   METHOD Home()           VIRTUAL // Moves the cursor to the leftmost visible data column
   METHOD Left()                   // Moves the cursor left one column
   METHOD PageDown()               // Repositions the data source downward
   METHOD PageUp()                 // Repositions the data source upward
   METHOD PanEnd()         VIRTUAL // Moves the cursor to the rightmost data column
   METHOD PanHome()        VIRTUAL // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()        VIRTUAL // Pans left without changing the cursor position
   METHOD PanRight()       VIRTUAL // Pans right without changing the cursor position
   METHOD Right()                  // Moves the cursor right one column
   METHOD Up()             // Moves the cursor up one row

   METHOD AddColumn( oCol ) INLINE ;
      AAdd( ::aColumns, oCol ), ::Configure( 2 ), Self // Adds a TBColumn object to the TBrowse object

   METHOD ColCount()       INLINE Len( ::aColumns )
   METHOD ColorRect()      VIRTUAL // Alters the color of a rectangular group of cells
   METHOD ColWidth()       VIRTUAL // Returns the display width of a particular column
   METHOD Configure()      VIRTUAL // Reconfigures the internal settings of the TBrowse object
   METHOD DeHilite()       VIRTUAL // Dehighlights the current cell

   METHOD DelColumn( nPos )        // Delete a column object from a browse

   METHOD ForceStable()    // Performs a full stabilization                             ê

   METHOD GetColumn( nColumn ) INLINE If( 0 < nColumn .and. nColumn <= Len( ::aColumns ),;
                                          ::aColumns[ nColumn ], nil ) // Gets a specific TBColumn object

   METHOD Hilite()         VIRTUAL // Highlights the current cell

   METHOD InsColumn( nPos, oCol ) INLINE ASize( ::aColumns, Len( ::aColumns + 1 ) ),;
                                  AIns( ::aColumns, nPos ),;
                                  ::aColumns[ nPos ] := oCol, ::Configure( 2 ), oCol
                                  // Insert a column object in a browse

   METHOD Invalidate()     INLINE AFill( ::aRedraw, .f. ), ::Stable := .f. // Forces entire redraw during next stabilization
   METHOD RefreshAll()     INLINE ::Invalidate() // Causes all data to be recalculated during the next stabilize
   METHOD RefreshCurrent() INLINE ::aRedraw[ ::RowPos ] := .f., ::Stable := .f. // Causes the current row to be refilled and repainted on next stabilize

   METHOD SetColumn( nColumn, oCol ) INLINE If( 0 < nColumn .and. nColumn <= Len( ::aColumns ),;
                                                ::aColumns[ nColumn ] := oCol, nil ), oCol // Replaces one TBColumn object with another

   METHOD Stabilize()              // Performs incremental stabilization

ENDCLASS

METHOD New() CLASS TBrowse

   ::aColumns  = {}
   ::nTop      = 0
   ::nLeft     = 0
   ::nBottom   = MaxRow()
   ::nRight    = MaxCol()
   ::AutoLite  = .t.
   ::leftVisible = 1
   ::ColPos    = 1
   ::Freeze    = 0
   ::HitBottom = .f.
   ::HitTop    = .f.
   ::ColorSpec = SetColor()
   ::ColSep    = " "
   ::FootSep   = ""
   ::HeadSep   = ""
   ::RowPos    = 1
   ::stable    = .f.
   ::RelativePos = 1
   ::aRedraw = nil

return Self

METHOD DelColumn( nPos ) CLASS TBrowse

   local oCol := ::aColumns[ nPos ]

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, Len( ::aColumns ) - 1 )
   ::Configure( 2 )

return oCol

METHOD Down() CLASS TBrowse
   ::HitTop = .F.
   if .not. ::HitBottom
      if Eval( ::SkipBlock, 1 ) != 0
         if ::RowPos < ::RowCount
            ::RefreshCurrent()
            ::RowPos++
            ::RefreshCurrent()
            ::RelativePos++
         else
            ::RefreshAll()
         endif
      else
         ::HitBottom = .t.
      endif
   endif
return Self

METHOD GoBottom() CLASS TBrowse
   ::HitTop = .F.
   ::HitBottom = .F.
   if Eval( ::goBottomBlock ) != 0
      ::RefreshAll()
      ::RowPos = ::RowCount
      ::RelativePos = ::RowCount
   endif
return Self

METHOD GoTop() CLASS TBrowse
   ::HitTop = .F.
   ::HitBottom = .F.
   if Eval( ::goTopBlock ) != 0
      ::RefreshAll()
      ::RowPos = 1
      ::RelativePos = 1
   endif
return Self

METHOD Left() CLASS TBrowse
   if ::ColPos > ::leftVisible
      ::ColPos--
      ::RefreshCurrent()
   else
      if ::ColPos > 1
         ::leftVisible--
         ::ColPos--
         ::RefreshAll()
      endif
   endif
return Self

METHOD PageDown() CLASS TBrowse
   local nDown
   
   ::HitTop = .F.
   if .not. ::HitBottom
      if ( nDown := Eval( ::SkipBlock, ::RowCount )  ) != 0
         if nDown < ::RowCount
            ::RefreshAll()
            ::RowPos = ::RowCount
            ::RelativePos = ::RowCount
         else
            ::RefreshAll()
         endif
      else
         ::HitBottom = .t.
      endif
   endif
return Self

METHOD PageUp() CLASS TBrowse
   local nUp
   
   ::HitBottom = .F.
   if .not. ::HitTop
      if ( nUp := Abs( Eval( ::SkipBlock, - ::RowCount )  ) ) != 0
         if nUp < ::RowCount
            ::RefreshAll()
            ::RowPos = 1
            ::RelativePos = 1
         else
            ::RefreshAll()
         endif
      else
         ::HitBottom = .t.
      endif
   endif
return Self

METHOD Right() CLASS TBrowse
   if ::ColPos < ::rightVisible
      ::ColPos++
      ::RefreshCurrent()
   else
      if ::ColPos < Len( ::aColumns )
         ::leftVisible++
         ::ColPos++
         ::RefreshAll()
      endif
   endif
return Self

METHOD ForceStable() CLASS TBrowse

   while ! ::Stabilize()
   end

return nil

METHOD Stabilize() CLASS TBrowse

   // Actually we are just trying to make it work. Once it is working
   // ok, we will turn it incremental

   local n, nRow, lDisplay := .t.
   local nWidth := ::nRight - ::nLeft + 1  // Visible width of the browse
   local nColsWidth := 0                   // Total width of visible columns plus ColSep
   local nColsVisible := ::leftVisible - 1 // Number of columns that fit on the browse width
   local lHeaders := .f.                   // Are there column headers to paint ?

   // Are there any column header to paint ?
   for n = 1 to Len( ::aColumns )
      if ! Empty( ::aColumns[ n ]:Heading )
         lHeaders = .t.
         exit
      endif
   next

   if ::aRedraw == Nil .or. ! ::aRedraw[ 1 ]
      // Calculate how many columns fit on the browse width including ColSeps
      while nColsVisible < Len( ::aColumns ) .and. nColsWidth + ::aColumns[ nColsVisible+1 ]:Width <= nWidth
         nColsWidth += ::aColumns[ ++nColsVisible ]:Width
         nColsWidth += Len( ::ColSep )
      end
      nColsWidth -= Len( ::ColSep )
      ::rightVisible = nColsVisible
      if ::aRedraw == nil
         ::RowCount = ::nBottom - ::nTop + 1 - If( lHeaders, 1, 0 )
         ::aRedraw = Array( ::RowCount )
         AFill( ::aRedraw, .F. )
      endif
   else
      nColsWidth = ::aColumns[::rightVisible]:ColPos +  ;
        ::aColumns[::rightVisible]:Width - ::aColumns[::leftVisible]:ColPos
   endif

   if lHeaders .and. .not. ::aRedraw[ 1 ]
      SetPos( ::nTop, ::nLeft )
      DevOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      for n = ::leftVisible to ::rightVisible
         DevOut( Padr( ::aColumns[ n ]:Heading, ::aColumns[ n ]:Width ), ::ColorSpec )
         if ::ColSep != nil .and. n < ::rightVisible
            DevOut( ::ColSep, ::ColorSpec )
         endif
      next
      DevOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
   endif

   for nRow := 1 to ::RowCount
      if .not. ::aRedraw[ nRow ]
         ::aRedraw[ nRow ] = .T.
         exit
      endif
   next

   if nRow > ::RowCount
      if !::stable
         Eval( ::SkipBlock, ::RowPos - ::RelativePos )
         ::RelativePos = ::RowPos
         if ::AutoLite
            @ ::nTop + ::RowPos - If( lHeaders, 0, 1 ), ::aColumns[ ::ColPos ]:ColPos ;
              SAY PadR( Eval( ::aColumns[ ::ColPos ]:block ), ::aColumns[ ::ColPos ]:Width ) ;
              COLOR __ColorIndex( ::ColorSpec, CLR_ENHANCED )
         endif
      endif
      ::stable = .t.
      return .t.
   else
      if nRow != ::RelativePos
         lDisplay = Eval( ::SkipBlock, nRow - ::RelativePos ) != 0
      endif
      ::RelativePos = nRow
      SetPos( ::nTop + nRow + If( lHeaders, 0, -1 ), ::nLeft )
      DevOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      for n = ::leftVisible to ::rightVisible
         if nRow == 1
            ::aColumns[ n ]:ColPos = Col()
         endif
         if lDisplay
            DevOut( PadR( Eval( ::aColumns[ n ]:block ),;
                 ::aColumns[ n ]:Width ), ::ColorSpec )
         else
            DevOut( Space( ::aColumns[ n ]:Width ), ::ColorSpec )
         endif
         if ::ColSep != nil .and. n < ::rightVisible
            DevOut( ::ColSep, ::ColorSpec )
         endif
      next
      DevOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
   endif

return .f.

METHOD Up() CLASS TBrowse

   ::HitBottom = .F.
   if ! ::HitTop
      if Eval( ::SkipBlock, -1 ) != 0
         if ::RowPos > 1
            ::RefreshCurrent()
            ::RowPos--
            ::RelativePos--
            ::RefreshCurrent()
         else
            ::RefreshAll()
         endif
      else
         ::HitTop = .t.
      endif
   endif

return Self

function TBrowseNew( nTop, nLeft, nBottom, nRight )

   local oBrw := TBrowse():New()

   if nTop != nil
      oBrw:nTop = nTop
   endif

   if nLeft != nil
      oBrw:nLeft = nLeft
   endif

   if nBottom != nil
      oBrw:nBottom = nBottom
   endif

   if nRight != nil
      oBrw:nRight = nRight
   endif

return oBrw

function TBrowseDb( nTop, nLeft, nBott, nRight )

   local oTb := TBrowseNew( nTop, nLeft, nBott, nRight )

   oTb:SkipBlock     := { | n | TBSkip( n ) }
   oTb:GoTopBlock    := { || DbGoTop() }
   oTb:GoBottomBlock := {|| DbGoBottom() }

Return oTb

static function TbSkip( nRecs )

   local nSkipped := 0

   if _LastRec() != 0
      if nRecs == 0
         DbSkip( 0 )
      elseif nRecs > 0 .and. _Recno() != _LastRec() + 1
         while nSkipped < nRecs
            DbSkip( 1 )
            if Eof()
               DbSkip( -1 )
               exit
            endif
            ++nSkipped
         end
      elseif nRecs < 0
         while nSkipped > nRecs
            DbSkip( -1 )
            if Bof()
               exit
            endif
            --nSkipped
         end
      endif
   endif

return nSkipped

static function _LastRec()  // Waiting for those function to become available

return 0

static function _RecNo()  // Waiting for those function to become available

return 0

