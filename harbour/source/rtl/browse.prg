/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Quick Clipper Browse()
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

#include "box.ch"
#include "common.ch"
#include "inkey.ch"

function Browse( nTop, nLeft, nBottom, nRight )

   local oBrw
   local cOldScreen
   local n, nKey, nOldCursor
   local lExit := .f.

   if ! Used()
      return .f.
	end

   DEFAULT nTop TO 1, nLeft TO 0, nBottom TO MaxRow(), nRight TO MaxCol()

   nOldCursor = SetCursor( 0 )
   cOldScreen = SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft, nBottom, nRight BOX B_DOUBLE

   oBrw = TBrowseDb( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep = Chr( 205 )

   for n = 1 to FCount()
      oBrw:AddColumn( TbColumnNew( FieldName( n ), FieldBlock( FieldName( n ) ) ) )
   next

   while ! lExit

       oBrw:ForceStable()

       nKey = InKey( 0 )

       do case
          case nKey == K_ESC
               lExit = .t.

           case nKey == K_UP
                oBrw:Up()

           case nKey == K_DOWN
                oBrw:Down()

           case nKey == K_END
                oBrw:End()

           case nKey == K_HOME
                oBrw:Home()

           case nKey == K_LEFT
                oBrw:Left()

           case nKey == K_RIGHT
                oBrw:Right()

           case nKey == K_PGUP
                oBrw:PageUp()

           case nKey == K_PGDN
                oBrw:PageDown()

           case nKey == K_CTRL_PGUP
                oBrw:GoTop()

           case nKey == K_CTRL_PGDN
                oBrw:GoBottom()
       endcase
   end

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

return .t.