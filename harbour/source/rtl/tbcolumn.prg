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

#include "hbclass.ch"
#include "hbsetup.ch"
#include "common.ch"

CLASS TBColumn

   DATA  Block                // Code block to retrieve data for the column
   DATA  Cargo                // User-definable variable
   DATA  ColorBlock           // Code block that determines color of data items
   DATA  ColSep               // Column separator character
   DATA  DefColor             // Array of numeric indexes into the color table
   DATA  Heading              // Column heading
   DATA  Footing              // Column footing
   DATA  FootSep              // Footing separator character
   DATA  HeadSep              // Heading separator character
   DATA  Picture              // Column picture string
   DATA  Width                // Column display width

   // NOTE: 17/08/01 - <maurilio.longo@libero.it>
   //       It is not correct in my opinion that this instance variable be exported
   DATA  ColPos               // Temporary column position on screen needed by TBrowse class

   METHOD New(cHeading, bBlock)  // Constructor

#ifdef HB_COMPAT_C53
   METHOD SetStyle()
#endif

ENDCLASS

METHOD New( cHeading, bBlock ) CLASS TBColumn

   local cType, nTokenPos := 0, nL

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

      cHeading +=  ";"
      while (nL := Len(__StrTkPtr(@cHeading, @nTokenPos, ";"))) > 0
         if nL > ::Width
            ::Width := nL
         endif
      enddo

   endif

return Self


#ifdef HB_COMPAT_C53
METHOD SetStyle() CLASS TBColumn

   /* TODO */

return Self
#endif


function TBColumnNew(cHeading, bBlock)

return TBColumn():New(cHeading, bBlock)




