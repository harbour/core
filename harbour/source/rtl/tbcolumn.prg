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
#include "hbsetup.ch"
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
   DATA Picture    // Column picture string
   DATA Width      // Column display width

   DATA ColPos     // Temporary column position on screen

   METHOD New( cHeading, bBlock )    // Constructor

#ifdef HB_COMPAT_C53
   METHOD SetStyle()
#endif

ENDCLASS

METHOD New( cHeading, bBlock ) CLASS TBColumn

   local cType

   ::DefColor := { 1, 2 }
   ::FootSep  := ""
   ::ColPos   := 1

   ::Width    := 0
   ::Heading  := iif(!Empty(cHeading), cHeading, "")

   /* TOFIX: In Clipper the column widths are not determined at this point.
          [vszakats] */
   if ISBLOCK( bBlock )

      ::block := bBlock

      cType := Valtype( Eval( bBlock ) )

      do case
         case cType == "N"
            ::Width := Len( Str( Eval( bBlock ) ) )

         case cType == "L"
            ::Width := 1

         case cType == "C"
            ::Width := Len( Eval( bBlock ) )

         case cType == "D"
            ::Width := Len( DToC( Eval( bBlock ) ) )

         otherwise
            ::Width := 0
      endcase

      ::Width := iif( cHeading != NIL, Max( Len( cHeading ), ::Width ), ::Width )
   endif

return Self


#ifdef HB_COMPAT_C53
METHOD SetStyle() CLASS TBColumn

   /* TODO */

return Self
#endif


function TBColumnNew( cHeading, bBlock )

return TBColumn():New(cHeading, bBlock)




