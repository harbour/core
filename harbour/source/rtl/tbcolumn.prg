/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBColumn Class
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

#include "hbclass.ch"
#include "common.ch"

CLASS TBColumn

   DATA Block      // Code block to retrieve data for the column
   DATA Cargo      // User-definable variable
   DATA ColorBlock // Code block that determines color of data items
   DATA ColSep     // Column separator character
   DATA DefColor   // Array of numeric indexes into the color table
   DATA Footing    // Column footing
   DATA FootSep    // Footing separator character
   DATA Heading    // Column heading
   DATA HeadSep    // Heading separator character
   DATA Width      // Column display width
   DATA ColPos     // Temporary column position on screen

   METHOD New()    // Constructor

ENDCLASS

METHOD New() CLASS TBColumn

   ::DefColor = { 1, 2 }
   ::FootSep  = ""
   ::ColPos   = 1

return Self

/* NOFIX: In Clipper the column width are not determined at this point. */

function TBColumnNew( cHeading, bBlock )

   local oCol := TBColumn():New()
   local nWidth, cType

   oCol:Heading := cHeading

   if ISBLOCK( bBlock )

      oCol:block := bBlock

      cType := Valtype( Eval( bBlock ) )

      do case
         case cType == "N"
            nWidth := 10

         case cType == "L"
            nWidth := 3

         case cType == "C"
            nWidth := Len( Eval( bBlock ) )

         otherwise
            nWidth := 0
      endcase

      oCol:Width := If( cHeading != nil, Max( Len( cHeading ), nWidth ), nWidth )

   endif

return oCol

