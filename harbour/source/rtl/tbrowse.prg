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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 *    Rewritten using the lower-level Harbour class creation way.
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: Don't use SAY in this module, use DispOut(), DispOutAt() instead,
         otherwise it will not be CA-Cl*pper compatible. [vszakats] */

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

/* TOFIX: The cursor is not left at the same position as in Clipper, this is
          very important, since several apps relies on it. Check CA-Cl*pper
          5.2e for the right implementation since 5.3 broke it. [vszakats] */

/* TOFIX: Clipper will refresh the current row even when a Down() is issued in
          the last row, or an Up() in the first one, this is important for
          cursor positioning. Yes, Harbour is smarter, but it's not compatible.
          [vszakats] */

#include "common.ch"
#include "color.ch"
#include "hbsetup.ch"

function TBrowseNew( nTop, nLeft, nBottom, nRight )

   LOCAL oClass := TClass():New( "TBROWSE" )

   DEFAULT nTop    TO 0
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO MaxRow()
   DEFAULT nRight  TO MaxCol()

   oClass:AddData( "aColumns", {} )          // Array to hold all browse columns
   oClass:AddData( "autoLite", .t. )         // Logical value to control highlighting
   oClass:AddData( "cargo" )                 // User-definable variable
   oClass:AddData( "colorSpec", SetColor() ) // Color table for the TBrowse display
   oClass:AddData( "colPos", 1 )             // Current cursor column position
   oClass:AddData( "colSep", " " )           // Column separator character
   oClass:AddData( "footSep", "" )           // Footing separator character
   oClass:AddData( "freeze", 0 )             // Number of columns to freeze
   oClass:AddData( "goBottomBlock" )         // Code block executed by TBrowse:goBottom()
   oClass:AddData( "goTopBlock" )            // Code block executed by TBrowse:goTop()
   oClass:AddData( "headSep", "" )           // Heading separator character
   oClass:AddData( "hitBottom", .f. )        // Indicates the end of available data
   oClass:AddData( "hitTop", .f. )           // Indicates the beginning of available data
   oClass:AddData( "leftVisible", 1 )        // Indicates position of leftmost unfrozen column in display
   oClass:AddData( "nBottom", nBottom )      // Bottom row number for the TBrowse display
   oClass:AddData( "nLeft", nLeft )          // Leftmost column for the TBrowse display
   oClass:AddData( "nRight", nRight )        // Rightmost column for the TBrowse display
   oClass:AddData( "nTop", nTop )            // Top row number for the TBrowse display
   oClass:AddData( "rightVisible" )          // Indicates position of rightmost unfrozen column in display
   oClass:AddData( "rowCount" )              // Number of visible data rows in the TBrowse display
   oClass:AddData( "rowPos", 1 )             // Current cursor row position
   oClass:AddData( "skipBlock" )             // Code block used to reposition data source
   oClass:AddData( "stable", .f. )           // Indicates if the TBrowse object is stable
                                             
   oClass:AddData( "aRedraw", {} )           // Array of logical items indicating, is appropriate row need to be redraw
   oClass:AddData( "RelativePos", 1 )        // Indicates record position relatively position of first record on the screen
   oClass:AddData( "lHeaders", .f. )         // Internal variable, indicate, are there column headers to paint
   oClass:AddData( "aRect", {} )             // The rectangle specified with ColorRect()
   oClass:AddData( "aRectColor", {} )        // The color positions to use in the rectangle specified with ColorRect()

#ifdef HB_EXTENSION
   oClass:AddMethod( "New",            @New() )             // Constructor
#endif

   oClass:AddMethod( "Down",           @Down() )            // Moves the cursor down one row
   oClass:AddMethod( "End",            @End() )             // Moves the cursor to the rightmost visible data column
   oClass:AddMethod( "GoBottom",       @GoBottom() )        // Repositions the data source to the bottom of file
   oClass:AddMethod( "GoTop",          @GoTop() )           // Repositions the data source to the top of file
   oClass:AddMethod( "Home",           @Home() )            // Moves the cursor to the leftmost visible data column
   oClass:AddMethod( "Left",           @_Left() )           // Moves the cursor left one column
   oClass:AddMethod( "PageDown",       @PageDown() )        // Repositions the data source downward
   oClass:AddMethod( "PageUp",         @PageUp() )          // Repositions the data source upward
   oClass:AddMethod( "PanEnd",         @PanEnd() )          // Moves the cursor to the rightmost data column
   oClass:AddMethod( "PanHome",        @PanHome() )         // Moves the cursor to the leftmost visible data column
   oClass:AddMethod( "PanLeft",        @PanLeft() )         // Pans left without changing the cursor position
   oClass:AddMethod( "PanRight",       @PanRight() )        // Pans right without changing the cursor position
   oClass:AddMethod( "Right",          @Right() )           // Moves the cursor right one column
   oClass:AddMethod( "Up",             @Up() )              // Moves the cursor up one row
   oClass:AddMethod( "AddColumn",      @AddColumn() )       // Adds a TBColumn object to the TBrowse object
   oClass:AddMethod( "ColCount",       @ColCount() )        
   oClass:AddMethod( "ColorRect",      @ColorRect() )       // Alters the color of a rectangular group of cells
                                                            // Returns the display width of a particular column
   oClass:AddMethod( "ColWidth",       @ColWidth() )        
   oClass:AddMethod( "Configure",      @Configure() )       // Reconfigures the internal settings of the TBrowse object
                                                            // nMode is an undocumented parameter in CA-Cl*pper
   oClass:AddMethod( "LeftDetermine",  @Leftdetermine() )   // Determine leftmost unfrozen column in display
   oClass:AddMethod( "DeHilite",       @DeHilite() )        // Dehighlights the current cell
   oClass:AddMethod( "DelColumn",      @DelColumn() )       // Delete a column object from a browse
   oClass:AddMethod( "ForceStable",    @ForceStable() )     // Performs a full stabilization
   oClass:AddMethod( "GetColumn",      @GetColumn() )       // Gets a specific TBColumn object
   oClass:AddMethod( "Hilite",         @Hilite() )          // Highlights the current cell
   oClass:AddMethod( "InsColumn",      @InsColumn() )       // Insert a column object in a browse
   oClass:AddMethod( "Invalidate",     @Invalidate() )      // Forces entire redraw during next stabilization
   oClass:AddMethod( "RefreshAll",     @RefreshAll() )      // Causes all data to be recalculated during the next stabilize
   oClass:AddMethod( "RefreshCurrent", @RefreshCurrent() )  // Causes the current row to be refilled and repainted on next stabilize
   oClass:AddMethod( "SetColumm",      @SetColumn() )       // Replaces one TBColumn object with another
   oClass:AddMethod( "Stabilize",      @Stabilize() )       // Performs incremental stabilization
   oClass:AddMethod( "DispCell",       @DispCell() )        // Displays a single cell

   oClass:Create()

return oClass:Instance()

#ifdef HB_EXTENSION
//--------------------------------------------------------------------------//
static function New( oCol )

   LOCAL Self := QSelf()

   ::aColumns    := {}
   ::nTop        := 0
   ::nLeft       := 0
   ::nBottom     := MaxRow()
   ::nRight      := MaxCol()
   ::AutoLite    := .t.
   ::leftVisible := 1
   ::ColPos      := 1
   ::Freeze      := 0
   ::HitBottom   := .f.
   ::HitTop      := .f.
   ::ColorSpec   := SetColor()
   ::ColSep      := " "
   ::FootSep     := ""
   ::HeadSep     := ""
   ::RowPos      := 1
   ::stable      := .f.
   ::RelativePos := 1
   ::aRedraw     := {}
   ::lHeaders    := .f.
   ::aRect       := {}
   ::aRectColor  := {}

return Self
#endif

//--------------------------------------------------------------------------//
static function AddColumn( oCol )

   LOCAL Self := QSelf()

   AAdd( ::aColumns, oCol )
   ::Configure( 2 )

return Self

//--------------------------------------------------------------------------//
static function ColCount()
return Len( QSelf():aColumns )

//--------------------------------------------------------------------------//
static function ColorRect( aRect, aRectColor )

   LOCAL Self := QSelf()

   ::aRect        := aRect
   ::aRectColor   := aRectColor

return Self

//--------------------------------------------------------------------------//
static function ColWidth( nColumn )

   LOCAL Self := QSelf()

return If( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ;
            ::aColumns[ nColumn ]:Width, NIL )

//--------------------------------------------------------------------------//
static function Configure( nMode ) /* VIRTUAL */
return NIL

//--------------------------------------------------------------------------//
static function DeHilite()

   LOCAL Self := QSelf()
   LOCAL nColor := If( ::aColumns[ ::ColPos ]:ColorBlock != NIL,;
                       Eval( ::aColumns[ ::ColPos ]:ColorBlock,;
                       Eval( ::aColumns[ ::ColPos ]:Block ) )[ 1 ], 1 )
   LOCAL cColor := hb_ColorIndex( ::ColorSpec, nColor - 1 )
   LOCAL nRow := ::nTop + ::RowPos - If( ::lHeaders, 0, 1 ) + If( Empty( ::HeadSep ), 0, 1 )
   LOCAL nCol := ::aColumns[ ::ColPos ]:ColPos

   SetPos( nRow, nCol )
   ::DispCell( ::ColPos, cColor )
   SetPos( nRow, nCol )

return Self

//--------------------------------------------------------------------------//
static function DelColumn( nPos )

   LOCAL Self := QSelf()
   LOCAL oCol  := ::aColumns[ nPos ]

   ADel( ::aColumns, nPos )
   ASize( ::aColumns, Len( ::aColumns ) - 1 )
   ::Configure( 2 )

return oCol

//--------------------------------------------------------------------------//
static function DispCell( nColumn, cColor )

   LOCAL Self := QSelf()
   LOCAL ftmp := Eval( ::aColumns[ nColumn ]:block )
   LOCAL nCol := Col()

   do case
   case valtype( ftmp ) $ "CM"
      DispOut( Left( ftmp, ::aColumns[ nColumn ]:Width ), cColor )
   case valtype( ftmp ) == "N"
      DispOut( Left( Str( ftmp ), ::aColumns[ nColumn ]:Width ), cColor )
   case valtype( ftmp ) == "D"
      DispOut( Right( DToC( ftmp ), ::aColumns[ nColumn ]:Width ), cColor )
   case valtype( ftmp ) == "L"
      DispOut( Space( ::aColumns[ nColumn ]:Width / 2 ), ::ColorSpec )
      DispOut( If( ftmp, "T","F" ), cColor )
   endcase

   DispOut( Space( nCol + ::aColumns[ nColumn ]:Width - Col() ), ::ColorSpec )

return Self

//--------------------------------------------------------------------------//
static function Down()

   LOCAL Self := QSelf()
   LOCAL n

   ::HitTop := .F.
   if !::HitBottom
      ::DeHilite()
      if Eval( ::SkipBlock, 1 ) != 0
         if ::RowPos < ::RowCount
            ::RowPos++
            ::Hilite()
            ::RelativePos++
         else
            n := ::nTop + If( ::lHeaders, 1, 0 ) + If( Empty( ::HeadSep ), 0, 1 )
            Scroll( n, ::nLeft, n + ::RowCount - 1, ::nRight, 1 )
            ::RefreshCurrent()
         endif
      else
         ::Hilite()
         ::HitBottom := .t.
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function End()

   LOCAL Self := QSelf()

   if ::ColPos < ::rightVisible
      ::ColPos := ::rightVisible
      ::RefreshCurrent()
   endif

return Self

//--------------------------------------------------------------------------//
static function ForceStable()

   LOCAL Self := QSelf()

   while !::Stabilize()
   end

return Self

//--------------------------------------------------------------------------//
static function GetColumn( nColumn )

   LOCAL Self := QSelf()

return If( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ;
            ::aColumns[ nColumn ], NIL )

//--------------------------------------------------------------------------//
static function GoBottom()

   LOCAL Self := QSelf()

   ::HitTop := .F.
   ::HitBottom := .F.
   if Eval( ::goBottomBlock ) != 0
      ::RefreshAll()
      ::RowPos := ::RowCount
      ::RelativePos := ::RowCount
   endif

return Self

//--------------------------------------------------------------------------//
static function GoTop()

   LOCAL Self := QSelf()

   ::HitTop := .F.
   ::HitBottom := .F.
   if Eval( ::goTopBlock ) != 0
      ::RefreshAll()
      ::RowPos := 1
      ::RelativePos := 1
   endif

return Self

//--------------------------------------------------------------------------//
static function Hilite()

   LOCAL Self := QSelf()
   LOCAL nColor
   LOCAL cColor
   LOCAL nRow
   LOCAL nCol

   if ::AutoLite

      nColor := If( ::aColumns[ ::ColPos ]:ColorBlock != NIL,;
                    Eval( ::aColumns[ ::ColPos ]:ColorBlock,;
                    Eval( ::aColumns[ ::ColPos ]:Block ) )[ 2 ], 2 )
      cColor := hb_ColorIndex( ::ColorSpec, nColor - 1 )
      nRow := ::nTop + ::RowPos - If( ::lHeaders, 0, 1 ) + If( Empty( ::HeadSep ), 0, 1 )
      nCol := ::aColumns[ ::ColPos ]:ColPos

      SetPos( nRow, nCol )
      ::DispCell( ::ColPos, cColor )
      SetPos( nRow, nCol )
   endif

return Self

//--------------------------------------------------------------------------//
static function Home()

   LOCAL Self := QSelf()

   if ::ColPos != ::leftVisible
      ::ColPos := ::leftVisible
      ::RefreshCurrent()
   endif

return Self

//--------------------------------------------------------------------------//
static function InsColumn( nPos, oCol )

   LOCAL Self := QSelf()

   ASize( ::aColumns, Len( ::aColumns + 1 ) )
   AIns( ::aColumns, nPos )
   ::aColumns[ nPos ] := oCol
   ::Configure( 2 )

return oCol

//--------------------------------------------------------------------------//
static function Invalidate()

   LOCAL Self := QSelf()
   LOCAL n
   LOCAL lFooters := .f.

   for n := 1 to Len( ::aColumns )
      if ! Empty( ::aColumns[ n ]:Footing )
         lFooters := .t.
         exit
      endif
   next

   ::RowCount := ::nBottom - ::nTop + 1 - If( ::lHeaders, 1, 0 ) - ;
               If( lFooters, 1, 0 ) - If( Empty( ::HeadSep ), 0, 1 ) - ;
               If( Empty( ::FootSep ), 0, 1 )

   AFill( ::aRedraw, .f. )

   ::Stable := .f.

return Self

//--------------------------------------------------------------------------//
static function _Left()

   LOCAL Self := QSelf()
   LOCAL leftVis := ::leftVisible

   if ::ColPos > ::leftVisible .or. ( ::ColPos < ::leftVisible .and. ::ColPos > 1 ) ;
       .or. ( ::ColPos == ::leftVisible .and. ::Freeze > 0 .and. ::leftVisible  - ::Freeze == 1 )
      ::DeHilite()
      ::ColPos--
      ::Hilite()
   else
      if ::ColPos > 1 .and. ::leftVisible  - ::Freeze > 1
         while leftVis == ::leftVisible
            ::rightVisible--
            ::leftVisible := ::LeftDetermine()
            ::RefreshAll()
         end
         if --::ColPos < ::leftVisible
            ::ColPos := ::rightVisible
         endif
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function LeftDetermine()

   LOCAL Self := QSelf()
   LOCAL nWidthMax := ::nRight - ::nLeft + 1  // Visible width of the browse
   LOCAL nWidth := 0
   LOCAL nCol

   if ::Freeze > 0
      for nCol := 1 TO ::Freeze
         nWidth += ::aColumns[ nCol ]:Width
         if nCol < Len( ::aColumns )
            nWidth += If( ::aColumns[ nCol + 1 ]:ColSep != NIL,;
               Len( ::aColumns[ nCol + 1 ]:ColSep ),;
               Len( ::ColSep ) )
         endif
      next
   endif

   for nCol := ::rightVisible to ::Freeze + 1 step -1

      nWidth += ::aColumns[ nCol ]:Width +;
         If( ::aColumns[ nCol ]:ColSep != NIL,;
            Len( ::aColumns[ nCol ]:ColSep ),;
            Len( ::ColSep ) )

      if nWidth > nWidthMax
         exit
      endif

   next

return nCol + 1

//--------------------------------------------------------------------------//
static function PageDown()

   LOCAL Self := QSelf()
   LOCAL nDown

   ::HitTop := .F.
   if !::HitBottom
      if ( nDown := Eval( ::SkipBlock, ::RowCount )  ) != 0
         if nDown < ::RowCount
            ::RefreshAll()
            ::RowPos := ::RowCount
            ::RelativePos := ::RowCount
         else
            ::RefreshAll()
         endif
      else
         ::HitBottom := .t.
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function PageUp()

   LOCAL Self := QSelf()
   LOCAL nUp

   ::HitBottom := .F.
   if !::HitTop
      if ( nUp := Abs( Eval( ::SkipBlock, - ::RowCount )  ) ) != 0
         if nUp < ::RowCount
            ::RefreshAll()
            ::RowPos := 1
            ::RelativePos := 1
         else
            ::RefreshAll()
         endif
      else
         ::HitTop := .t.
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function PanEnd()

   LOCAL Self := QSelf()

   if ::ColPos < Len( ::aColumns )
      if ::rightVisible < Len( ::aColumns )
         ::rightVisible := Len( ::aColumns )
         ::leftVisible := ::LeftDetermine()
         ::ColPos := ::rightVisible
         ::RefreshAll()
      else
         ::ColPos := ::rightVisible
         ::RefreshCurrent()
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function PanHome()

   LOCAL Self := QSelf()

   if ::ColPos > 1
      if ::leftVisible > ::Freeze + 1
         ::leftVisible := ::Freeze + 1
         ::ColPos := 1
         ::RefreshAll()
      else
         ::ColPos := 1
         ::RefreshCurrent()
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function PanLeft()

   LOCAL Self := QSelf()
   LOCAL n := ::ColPos - ::leftVisible

   if ::leftVisible > ::Freeze + 1
      ::rightVisible--
      ::leftVisible := ::LeftDetermine()
      ::ColPos := Min( ::leftVisible + n, ::rightVisible )
      ::RefreshAll()
   endif

return Self

//--------------------------------------------------------------------------//
static function PanRight()

   LOCAL Self := QSelf()
   LOCAL n := ::ColPos - ::leftVisible

   if ::rightVisible < Len( ::aColumns )
      ::rightVisible++
      ::leftVisible := ::LeftDetermine()
      ::ColPos := Min( ::leftVisible + n, ::rightVisible )
      ::RefreshAll()
   endif

return Self

//--------------------------------------------------------------------------//
static function RefreshAll()

   LOCAL Self := QSelf()

   /* TOFIX: This is not exactly the same as invalidate, since this one will 
             also refresh the data not only redisplay the tbrowse.
             [vszakats] */

return ::Invalidate()

//--------------------------------------------------------------------------//
static function RefreshCurrent()

   LOCAL Self := QSelf()

   ::aRedraw[ ::RowPos ] := .f.
   ::Stable := .f.

return Self

//--------------------------------------------------------------------------//
static function Right()

   LOCAL Self := QSelf()

   if ::ColPos < ::rightVisible
      ::DeHilite()
      ::ColPos++
      ::Hilite()
   else
      if ::ColPos < Len( ::aColumns )
         ::rightVisible++
         ::leftVisible := ::LeftDetermine()
         ::ColPos++
         ::RefreshAll()
      endif
   endif

return Self

//--------------------------------------------------------------------------//
static function SetColumn( nColumn, oCol )

   LOCAL Self := QSelf()

   If( 0 < nColumn .and. nColumn <= Len( ::aColumns ), ;
            ::aColumns[ nColumn ] := oCol, NIL )

return oCol

//--------------------------------------------------------------------------//
static function Stabilize()

   LOCAL Self := QSelf()
   LOCAL iW, n, nRow, nCol, lDisplay := .t.
   LOCAL nWidth := ::nRight - ::nLeft + 1  // Visible width of the browse
   LOCAL nColsWidth := 0                   // Total width of visible columns plus ColSep
   LOCAL nColsVisible                      // Number of columns that fit on the browse width
   LOCAL lFooters := .f.                   // Are there column footers to paint ?
   LOCAL cColColor                         // Column color to use
   LOCAL oCol, oCol2
   LOCAL nToAdd

   if Empty(::aRedraw) .or. !::aRedraw[ 1 ]
      // Are there any column header to paint ?
      for n := 1 to Len( ::aColumns )
         if ! Empty( ::aColumns[ n ]:Heading )
            ::lHeaders := .t.
            exit
         endif
      next
      // Are there any column footer to paint ?
      for n := 1 to Len( ::aColumns )
         if ! Empty( ::aColumns[ n ]:Footing )
            lFooters := .t.
            exit
         endif
      next
      // Calculate how many columns fit on the browse width including ColSeps
      if ::Freeze > 0
         if ::leftVisible <= ::Freeze
            ::leftVisible := ::Freeze + 1
         endif

         nColsVisible := 0
         while nColsVisible < ::Freeze

            nToAdd := ::aColumns[ nColsVisible + 1 ]:Width

            if nColsVisible >= 1 .and. nColsVisible < Len( ::aColumns )
               nToAdd += If( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL,;
                  Len( ::aColumns[ nColsVisible + 1 ]:ColSep ),;
                  Len( ::ColSep ) )
            endif

            if nColsWidth + nToAdd > nWidth
               exit
            endif

            nColsWidth += nToAdd
            nColsVisible++
         enddo

         if nColsWidth > nWidth
            ::Freeze := 0
            nColsWidth := 0
         endif
      endif

      nColsVisible := ::leftVisible - 1

      while nColsVisible < Len( ::aColumns )

         nToAdd := ::aColumns[ nColsVisible + 1 ]:Width

         if nColsVisible >= ::leftVisible .or. ::Freeze > 0
            nToAdd += If( ::aColumns[ nColsVisible + 1 ]:ColSep != NIL,;
               Len( ::aColumns[ nColsVisible + 1 ]:ColSep ),;
               Len( ::ColSep ) )
         endif

         if nColsWidth + nToAdd > nWidth
            exit
         endif

         nColsWidth += nToAdd
         nColsVisible++
      enddo

      ::rightVisible := nColsVisible

      if Empty(::aRedraw)
         ::RowCount := ::nBottom - ::nTop + 1 - If( ::lHeaders, 1, 0 ) - ;
               If( lFooters, 1, 0 ) - If( Empty( ::HeadSep ), 0, 1 ) - If( Empty( ::FootSep ), 0, 1 )
         ::aRedraw := Array( ::RowCount )
         AFill( ::aRedraw, .F. )
      endif
   else
      oCol := ::aColumns[ If( ::rightVisible != 0, ::rightVisible, 1 ) ]
      oCol2 := ::aColumns[ If( ::Freeze > 0, 1, ::leftVisible ) ]
      nColsWidth := If( oCol != NIL, oCol:ColPos, 0 ) + ;
        If( oCol != NIL, oCol:Width, 0 ) - oCol2:ColPos
      lFooters := ( ::RowCount != ::nBottom - ::nTop + 1 - If( ::lHeaders, 1, 0 ) - ;
                - If( Empty( ::HeadSep ), 0, 1 ) - If( Empty( ::FootSep ), 0, 1 ) )
   endif

   if !::aRedraw[ 1 ]
      if ::lHeaders          // Drawing headers
         DispOutAt( ::nTop, ::nLeft, Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
         for n := If( ::Freeze>0, 1, ::leftVisible ) to ::rightVisible
            if ::Freeze > 0 .and. n == ::Freeze + 1
               n := ::leftVisible
            endif
            DispOut( PadR( ::aColumns[ n ]:Heading, ::aColumns[ n ]:Width ), ::ColorSpec )
            if n < ::rightVisible
               DispOut( Space( If( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                 Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) ) ), ::ColorSpec )
            endif
         next
         DispOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      endif
      if ! Empty( ::HeadSep )  //Drawing heading separator
         DispOutAt( ::nTop + If( ::lHeaders, 1, 0 ), ::nLeft, Replicate( Right( ::HeadSep, 1 ), ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
         if Len( ::HeadSep ) > 1
            iW := 0
            for n := If( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
               if ::Freeze > 0 .and. n == ::Freeze + 1
                  n := ::leftVisible
               endif
               DispOut( Replicate( Right( ::HeadSep, 1 ), ::aColumns[ n ]:Width - iW ), ::ColorSpec )
               if n < ::rightVisible
                  DispOut( Left( ::HeadSep, Len( ::HeadSep ) - 1 ), ::ColorSpec )
                  iW := Len( ::HeadSep ) - 1 - If( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                      Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) )
               endif
            next
         else
            DispOut( Replicate( ::HeadSep, nColsWidth ), ::ColorSpec )
         endif
         DispOut( Replicate( Right( ::HeadSep, 1 ), ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      endif
      if ! Empty( ::FootSep ) // Drawing footing separator
         DispOutAt( ::nBottom - If( lFooters, 1, 0 ), ::nLeft, Replicate( Right( ::FootSep, 1 ), ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
         if Len( ::FootSep ) > 1
            iW := 0
            for n := If( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
               if ::Freeze > 0 .and. n == ::Freeze + 1
                  n := ::leftVisible
               endif
               DispOut( Replicate( Right( ::FootSep, 1 ), ::aColumns[ n ]:Width - iW ), ::ColorSpec )
               if n < ::rightVisible
                  DispOut( Left( ::FootSep, Len( ::FootSep ) - 1 ), ::ColorSpec )
                  iW := Len( ::FootSep ) - 1 - If( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                      Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) )
               endif
            next
         else
            DispOut( Replicate( ::FootSep, nColsWidth ), ::ColorSpec )
         endif
         DispOut( Replicate( Right( ::FootSep, 1 ), ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      endif
      if lFooters                // Drawing footers
         DispOutAt( ::nBottom, ::nLeft, Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
         for n := If( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible
               if ::Freeze > 0 .and. n == ::Freeze + 1
                  n := ::leftVisible
               endif
            DispOut( PadR( ::aColumns[ n ]:Footing, ::aColumns[ n ]:Width ), ::ColorSpec )
            if n < ::rightVisible
               DispOut( Space( If( ::aColumns[ n + 1 ]:ColSep != NIL, ;
                 Len( ::aColumns[ n + 1 ]:ColSep ), Len( ::ColSep ) ) ), ::ColorSpec )
            endif
         next
         DispOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )
      endif
   endif

   for nRow := 1 to ::RowCount    // Looking for row to redraw
      if !::aRedraw[ nRow ]
         ::aRedraw[ nRow ] := .T.
         exit
      endif
   next

   if nRow > ::RowCount          // if all rows are draw, hilite current
      if !::stable
         Eval( ::SkipBlock, ::RowPos - ::RelativePos )
         ::RelativePos := ::RowPos
         ::HitBottom := .F.
         ::HiLite()
      endif
      ::stable := .t.
      return .t.
   else             // redraw a row
      if !::HitBottom
         if nRow != ::RelativePos
            if lDisplay := ( Eval( ::SkipBlock, nRow - ::RelativePos ) != 0 )
               ::RelativePos := nRow
            else
               ::HitBottom := .T.
            endif
         endif
      else
         lDisplay := .F.
      endif

      DispOutAt( ::nTop + nRow + If( ::lHeaders, 0, -1 ) + If( Empty( ::HeadSep ), 0, 1 ), ::nLeft,;
         Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )

      for n := If( ::Freeze > 0, 1, ::leftVisible ) to ::rightVisible

         if ::Freeze > 0 .and. n == ::Freeze + 1
            n := ::leftVisible
         endif
         if nRow == 1
            ::aColumns[ n ]:ColPos := Col()
         endif

         nCol := Col()

         if lDisplay

            cColColor := If( ::aColumns[ n ]:ColorBlock != NIL,;
                             hb_ColorIndex( ::ColorSpec,;
                             Eval( ::aColumns[ n ]:ColorBlock,;
                             Eval( ::aColumns[ n ]:Block ) )[ 1 ] - 1 ),;
                             ::ColorSpec )

            ::DispCell( n, cColColor )
            SetPos( Row(), nCol + ::aColumns[ n ]:Width )
         else
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

      DispOut( Space( ( nWidth - nColsWidth ) / 2 ), ::ColorSpec )

   endif

return .f.

//--------------------------------------------------------------------------//
static function Up()

   LOCAL Self := QSelf()
   LOCAL n

   ::HitBottom := .F.
   if !::HitTop
      ::DeHilite()
      if Eval( ::SkipBlock, -1 ) != 0
         if ::RowPos > 1
            ::RowPos--
            ::Hilite()
            ::RelativePos--
         else
            n := ::nTop + If( ::lHeaders, 1, 0 ) + If( Empty( ::HeadSep ), 0, 1 )
            Scroll( n, ::nLeft, n + ::RowCount - 1, ::nRight, -1 )
            ::RefreshCurrent()
         endif
      else
         ::Hilite()
         ::HitTop := .t.
      endif
   endif

return Self

