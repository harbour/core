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

#include "common.ch"
#include "hbsetup.ch"

function TBColumnNew( cHeading, bBlock )

   LOCAL oClass
   LOCAL nWidth, cType

   /* TOFIX: In Clipper the column widths are not determined at this point. 
             [vszakats] */

   if ISBLOCK( bBlock )

      cType := Valtype( Eval( bBlock ) )

      do case
         case cType == "N"
            nWidth := Len( Str( Eval( bBlock ) ) )

         case cType == "L"
            nWidth := 1

         case cType == "C"
            nWidth := Len( Eval( bBlock ) )

         case cType == "D"
            nWidth := Len( DToC( Eval( bBlock ) ) )

         otherwise
            nWidth := 0
      endcase

   endif

   oClass := TClass():New( "TBCOLUMN" )

   oClass:AddData( "Block", ;             // Code block to retrieve data for the column
      iif( ISBLOCK( bBlock ), bBlock, NIL ) )

   oClass:AddData( "Cargo" )              // User-definable variable
   oClass:AddData( "ColorBlock" )         // Code block that determines color of data items
   oClass:AddData( "ColPos", 1 )          // Temporary column position on screen
   oClass:AddData( "ColSep" )             // Column separator character
   oClass:AddData( "DefColor", { 1, 2 } ) // Array of numeric indexes into the color table
   oClass:AddData( "Footing" )            // Column footing
   oClass:AddData( "FootSep", "" )        // Footing separator character
   oClass:AddData( "Heading", cHeading )  // Column heading
   oClass:AddData( "HeadSep" )            // Heading separator character

   oClass:AddData( "Width", ;             // Column display width
      iif( cHeading != NIL, Max( Len( cHeading ), nWidth ), nWidth ) )

#ifdef HB_EXTENSION
   oClass:AddMethod( "New", @New() )      // Constructor
#endif

#ifdef HB_COMPAT_C53
   oClass:AddMethod( "SetStyle", @SetStyle() )
#endif

   oClass:Create()

return oClass:Instance()

#ifdef HB_EXTENSION
static function New()

   LOCAL Self := QSelf()

   ::DefColor := { 1, 2 }
   ::FootSep  := ""
   ::ColPos   := 1

return Self
#endif

#ifdef HB_COMPAT_C53
static function SetStyle( nStyle, lStyle )

   LOCAL Self := QSelf()

   /* TODO: Implement this */

return Self
#endif
