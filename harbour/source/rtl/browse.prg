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

#include "inkey.ch"

function Browse( nTop, nLeft, nBottom, nRight )

   local oBrw
   local cOldScreen
   local n, nOldCursor
   local nKey := 0
   local lExit := .f.
   local lGotKey := .f.
   local bAction

   if ! Used()
      return .f.
   end

   if PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   endif

   nOldCursor := SetCursor( 0 )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 3, nLeft SAY Chr( 198 )
   @ nTop + 3, nRight SAY Chr( 181 )
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   oBrw := TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep := " " + Chr( 205 )

   for n := 1 to FCount()
      oBrw:AddColumn( TBColumnNew( FieldName( n ), FieldBlock( FieldName( n ) ) ) )
   next

   oBrw:ForceStable()

   while ! lExit

      if nKey == 0
         while !oBrw:stabilize() .and. NextKey() == 0
         enddo
      endif

      if NextKey() == 0

         oBrw:forceStable()
         Statline( oBrw )

         nKey := Inkey( 0 )

         if ( bAction := SetKey( nKey ) ) != nil
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            loop
         endif
      else
         nKey := Inkey()
      endif

      do case
         case nKey == K_ESC
            lExit := .t.

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

         case nKey == K_CTRL_LEFT
            oBrw:panLeft()

         case nKey == K_CTRL_RIGHT
            oBrw:panRight()

         case nKey == K_CTRL_HOME
            oBrw:panHome()

         case nKey == K_CTRL_END
            oBrw:panEnd()

      endcase
   end

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

return .t.

static procedure Statline( oBrw )

   local nTop   := oBrw:nTop - 1
   local nRight := oBrw:nRight

   @ nTop, nRight - 27 SAY "Record "

   if LastRec() == 0
      @ nTop, nRight - 20 SAY "<none>               "
   elseif RecNo() == LastRec() + 1
      @ nTop, nRight - 40 SAY "         "
      @ nTop, nRight - 20 SAY "                <new>"
   else
      @ nTop, nRight - 40 SAY iif( Deleted(), "<Deleted>", "         " )
      @ nTop, nRight - 20 SAY PadR( LTrim( Str( RecNo() ) ) + "/" +;
                                    Ltrim( Str( LastRec() ) ), 16 ) +;
                              iif( oBrw:hitTop, "<bof>", "     " )
   endif

return

