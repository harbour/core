/*
 * Harbour Class TBrowse
 * Copyright(C) 1999 by Antonio Linares.
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
 *
 * You can contact me at: alinares@fivetech.com
 */

#include "classes.ch"

CLASS TBrowse

   DATA aColumns      // Array to hold all browse columns
   DATA autoLite      // Logical value to control highlighting
   DATA cargo         // User-definable variable
   DATA colCount      // Number of browse columns
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

   METHOD New()                    // Constructor
   METHOD Down()           VIRTUAL // Moves the cursor down one row
   METHOD End()            VIRTUAL // Moves the cursor to the rightmost visible data column
   METHOD GoBottom()       VIRTUAL // Repositions the data source to the bottom of file
   METHOD GoTop()          VIRTUAL // Repositions the data source to the top of file
   METHOD Home()           VIRTUAL // Moves the cursor to the leftmost visible data column
   METHOD Left()           VIRTUAL // Moves the cursor left one column
   METHOD PageDown()       VIRTUAL // Repositions the data source downward
   METHOD PageUp()         VIRTUAL // Repositions the data source upward
   METHOD PanEnd()         VIRTUAL // Moves the cursor to the rightmost data column
   METHOD PanHome()        VIRTUAL // Moves the cursor to the leftmost visible data column
   METHOD PanLeft()        VIRTUAL // Pans left without changing the cursor position
   METHOD PanRight()       VIRTUAL // Pans right without changing the cursor position
   METHOD Right()          VIRTUAL // Moves the cursor right one column
   METHOD Up()             VIRTUAL // Moves the cursor up one row
   METHOD AddColumn()      VIRTUAL // Adds a TBColumn object to the TBrowse object
   METHOD ColorRect()      VIRTUAL // Alters the color of a rectangular group of cells
   METHOD ColWidth()       VIRTUAL // Returns the display width of a particular column
   METHOD Configure()      VIRTUAL // Reconfigures the internal settings of the TBrowse object
   METHOD DeHilite()       VIRTUAL // Dehighlights the current cell
   METHOD DelColumn()      VIRTUAL // Delete a column object from a browse
   METHOD ForceStable()    VIRTUAL // Performs a full stabilization                             ê

   METHOD GetColumn( nColumn ) INLINE If( 0 > nColumn .and. nColumn <= Len( ::aColumns ),;
                                          ::aColumns[ nColumn ], nil ) // Gets a specific TBColumn object

   METHOD Hilite()         VIRTUAL // Highlights the current cell
   METHOD InsColumn()      VIRTUAL // Insert a column object in a browse
   METHOD Invalidate()     VIRTUAL // Forces redraw during next stabilization
   METHOD RefreshAll()     VIRTUAL // Causes all data to be refreshed during the next stabilize
   METHOD RefreshCurrent() VIRTUAL // Causes the current row to be refreshed on next stabilize
   METHOD SetColumn()      VIRTUAL // Replaces one TBColumn object with another
   METHOD Stabilize()      VIRTUAL // Performs incremental stabilization

ENDCLASS

METHOD New() CLASS TBrowse

   ::aColumns  = {}
   ::nTop      = 0
   ::nLeft     = 0
   ::nBottom   = MaxRow()
   ::nRight    = MaxCol()
   ::AutoLite  = .t.
   ::ColPos    = 1
   ::Freeze    = 0
   ::HitBottom = .f.
   ::HitTop    = .f.
   ::ColorSpec = SetColor()
   ::ColSep    = " "
   ::FootSep   = ""
   ::HeadSep   = ""

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

