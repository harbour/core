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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    Documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "inkey.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      BROWSE()
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Browse a database file
 *  $SYNTAX$
 *      BROWSE( [<nTop>, <nLeft>, <nBottom>, <nRight>] ) --> lOk
 *  $ARGUMENTS$
 *      <nTop> coordinate for top row display.
 *
 *      <nLeft> coordinate for left column display.
 *
 *      <nBottom> coordinate for bottom row display.
 *
 *      <nRight> coordinate for right column display.
 *  $RETURNS$
 *      BROWSE() return .F. if there is no database open in this work area,
 *      else it return .T.
 *  $DESCRIPTION$
 *      BROWSE() is a general purpose database browser, without any
 *      thinking you can browse a file using the following keys:
 *
 *      Left           - Move one column to the left (previous field)
 *      Right          - Move one column to the right (next field)
 *      Up             - Move up one row (previous record)
 *      Down           - Move down one row (next record)
 *      Page-Up        - Move to the previous screen
 *      Page-Down      - Move to the next screen
 *      Ctrl Page-Up   - Move to the top of the file
 *      Ctrl Page-Down - Move to the end of the file
 *      Home           - Move to the leftmost visible column
 *      End            - Move to the rightmost visible column
 *      Ctrl Left      - Pan one column to the left
 *      Ctrl Right     - Pan one column to the right
 *      Ctrl Home      - Move to the leftmost column
 *      Ctrl End       - Move to the rightmost column
 *      Esc            - Terminate BROWSE()
 *
 *      On top of the screen you see a status line with the following
 *      indication:
 *
 *      Record ###/###  - Current record number / Total number of records.
 *      <none>          - There are no records, the file is empty.
 *      <new>           - You are in append mode at the bottom of file.
 *      <Deleted>       - Current record is deleted.
 *      <bof>           - You are at the top of file.
 *
 *      You should pass whole four valid coordinate, if less than four
 *      parameters are passed to BROWSE() the coordinate are default to:
 *      1, 0, MAXROW(), MAXCOL().
 *  $EXAMPLES$
 *      // this one shows you how to browse around
 *      USE Around
 *      BROWSE()
 *  $TESTS$
 *  $STATUS$
 *      S
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *      DBEDIT(), TBrowse class
 *  $END$
 */

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

   nOldCursor = SetCursor( 0 )
   cOldScreen = SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 3, nLeft SAY Chr( 198 )
   @ nTop + 3, nRight SAY Chr( 181 )
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   oBrw = TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep = " " + Chr( 205 )

   for n = 1 to FCount()
      oBrw:AddColumn( TBColumnNew( FieldName( n ), FieldBlock( FieldName( n ) ) ) )
   next

   oBrw:ForceStable()

   while ! lExit

      if nKey == 0
         while !oBrw:stabilize() .and. NextKey() == 0
         enddo
      endif

      if NextKey() == 0

         Statline( oBrw )
         oBrw:forceStable()

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

