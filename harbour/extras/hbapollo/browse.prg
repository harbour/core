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
 * This program is hb_xfree software; you can redistribute it and/or modify
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

FUNCTION sx_Browse( nTop, nLeft, nBottom, nRight )

   LOCAL oBrw
   LOCAL cOldScreen
   LOCAL n, nOldCursor
   LOCAL nKey := 0
   LOCAL lExit := .F.

// LOCAL lGotKey := .f.
   LOCAL bAction

   IF !sx_Used()
      RETURN .F.
   end

   sx_GoTop()

   IF PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   ENDIF

   nOldCursor := SetCursor( 0 )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 3, nLeft SAY Chr( 198 )
   @ nTop + 3, nRight SAY Chr( 181 )
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   oBrw := sx_TBrowseDB( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )
   oBrw:HeadSep := " " + Chr( 205 )

   for n := 1 TO sx_FieldCount()
      oBrw:AddColumn( TBColumnNew( sx_FieldName( n ), sx_FieldBlock( sx_FieldName( n ) ) ) )
   next

   oBrw:ForceStable()

   WHILE ! lExit

      IF nKey == 0
         WHILE !oBrw:stabilize() .AND. NextKey() == 0
         ENDDO
      ENDIF

      IF NextKey() == 0

         oBrw:forceStable()
         Statline( oBrw )

         nKey := Inkey( 0 )

         IF ( bAction := SetKey( nKey ) ) != nil
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            LOOP
         ENDIF
      ELSE
         nKey := Inkey()
      ENDIF

      SWITCH nKey
      CASE K_ESC
         lExit := .T.
         EXIT

      CASE K_UP
         oBrw:Up()
         EXIT

      CASE K_DOWN
         oBrw:Down()
         EXIT

      CASE K_END
         oBrw:End()
         EXIT

      CASE K_HOME
         oBrw:Home()
         EXIT

      CASE K_LEFT
         oBrw:Left()
         EXIT

      CASE K_RIGHT
         oBrw:Right()
         EXIT

      CASE K_PGUP
         oBrw:PageUp()
         EXIT

      CASE K_PGDN
         oBrw:PageDown()
         EXIT

      CASE K_CTRL_PGUP
         oBrw:GoTop()
         EXIT

      CASE K_CTRL_PGDN
         oBrw:GoBottom()
         EXIT

      CASE K_CTRL_LEFT
         oBrw:panLeft()
         EXIT

      CASE K_CTRL_RIGHT
         oBrw:panRight()
         EXIT

      CASE K_CTRL_HOME
         oBrw:panHome()
         EXIT

      CASE K_CTRL_END
         oBrw:panEnd()
         EXIT

      ENDSWITCH
   ENDDO

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

   RETURN .T.

STATIC PROCEDURE Statline( oBrw )

   LOCAL nTop   := oBrw:nTop - 1
   LOCAL nRight := oBrw:nRight

   @ nTop, nRight - 27 SAY "Record "

   IF sx_LastRec() == 0
      @ nTop, nRight - 20 SAY "<none>               "
   ELSEIF sx_RecNo() == sx_LastRec() + 1
      @ nTop, nRight - 40 SAY "         "
      @ nTop, nRight - 20 SAY "                <new>"
   ELSE
      @ nTop, nRight - 40 SAY iif( sx_Deleted(), "<Deleted>", "         " )
      @ nTop, nRight - 20 SAY PadR( LTrim( Str( sx_RecNo() ) ) + "/" + ;
         LTrim( Str( sx_LastRec() ) ), 16 ) + ;
         iif( oBrw:hitTop, "<bof>", "     " )
   ENDIF

   RETURN
