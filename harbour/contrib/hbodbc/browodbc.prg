/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Browse() for ODBC data
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com> for code derived from browse.prg
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net) for original FieldBlock function
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca> for original Skipped function
 * Copyright 2002 Tomaz Zupan <tomaz.zupan@orpo.si> modifications for ODBC
 * www - http://harbour-project.org
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

 /* CREDITS:
 * This code is mostly derived work from harbours RTL browse.prg, browdb.prg.
 * and fieldbl.prg. Only minor changes were needed to adapt them to ODBC.
 */

#include "inkey.ch"
#include "setcurs.ch"

FUNCTION BrowseODBC( nTop, nLeft, nBottom, nRight, oDataSource )

   LOCAL oBrw
   LOCAL cOldScreen
   LOCAL n, nOldCursor
   LOCAL nKey := 0
   LOCAL lExit := .F.
   LOCAL bAction
   LOCAL oColumn

   // TODO: Check if datasource is open
// IF ! Used()
//    RETURN .F.
// ENDIF

   IF PCount() < 4
      nTop    := 1
      nLeft   := 0
      nBottom := MaxRow()
      nRight  := MaxCol()
   ENDIF

   nOldCursor := SetCursor( SC_NONE )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   @ nTop, nLeft TO nBottom, nRight
   @ nTop + 1, nLeft + 1 SAY Space( nRight - nLeft - 1 )

   oBrw := TBrowseNew( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )

   oBrw:SkipBlock     := {| nRecs | Skipped( nRecs, oDataSource ) }
   oBrw:GoTopBlock    := {|| oDataSource:first() }
   oBrw:GoBottomBlock := {|| oDataSource:last() }

   oBrw:HeadSep := "-"

   // TODO: Find out number of columns in ODBC result set, up to then you have to add columns by hand
   FOR n := 1 to Len( oDataSource:Fields )
      oColumn:= TBColumn():New( oDataSource:Fields[ n ]:FieldName, ODBCFget( oDataSource:Fields[ n ]:FieldName, oDataSource ) )
      oBrw:AddColumn( oColumn )
   NEXT

   oBrw:Configure()
   oBrw:ForceStable()

   DO WHILE ! lExit

      IF nKey == 0
         DO WHILE !oBrw:stabilize() .AND. NextKey() == 0
         ENDDO
      ENDIF

      IF NextKey() == 0

         oBrw:forceStable()
         Statline( oBrw, oDataSource)

         nKey := Inkey( 0 )

         IF ( bAction := SetKey( nKey ) ) != NIL
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            LOOP
         ENDIF
      ELSE
         nKey := Inkey()
      ENDIF

      DO CASE
      CASE nKey == K_ESC
         lExit := .T.

      CASE nKey == K_UP
         oBrw:Up()

      CASE nKey == K_DOWN
         oBrw:Down()

      CASE nKey == K_END
         oBrw:End()

      CASE nKey == K_HOME
         oBrw:Home()

      CASE nKey == K_LEFT
         oBrw:Left()

      CASE nKey == K_RIGHT
         oBrw:Right()

      CASE nKey == K_PGUP
         oBrw:PageUp()

      CASE nKey == K_PGDN
         oBrw:PageDown()

      CASE nKey == K_CTRL_PGUP
         oBrw:GoTop()

      CASE nKey == K_CTRL_PGDN
         oBrw:GoBottom()

      CASE nKey == K_CTRL_LEFT
         oBrw:panLeft()

      CASE nKey == K_CTRL_RIGHT
         oBrw:panRight()

      CASE nKey == K_CTRL_HOME
         oBrw:panHome()

      CASE nKey == K_CTRL_END
         oBrw:panEnd()

      ENDCASE
   ENDDO

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

   RETURN .T.

STATIC PROCEDURE Statline( oBrw, oDataSource )

   LOCAL nTop   := oBrw:nTop - 1
   LOCAL nRight := oBrw:nRight

   hb_DispOutAt( nTop, nRight - 27, "Record " )

   IF oDataSource:LastRec() == 0
      hb_DispOutAt( nTop, nRight - 20, "<none>               " )
   ELSEIF oDataSource:RecNo() == oDataSource:LastRec() + 1
      hb_DispOutAt( nTop, nRight - 40, "         " )
      hb_DispOutAt( nTop, nRight - 20, "                <new>" )
   ELSE
      hb_DispOutAt( nTop, nRight - 20, PadR( hb_ntos( oDataSource:RecNo() ) + "/" +;
                                             hb_ntos( oDataSource:LastRec() ), 16 ) +;
                                       iif( oBrw:hitTop, "<bof>", "     " ) +;
                                       iif( oBrw:hitBottom, "<eof>", "     " ) )
   ENDIF

   RETURN

STATIC FUNCTION Skipped( nRecs, oDataSource )

   LOCAL nSkipped := 0

   IF ! oDataSource:Eof()
      IF nRecs == 0
         // ODBC doesn't have Skip( 0 )
      ELSEIF nRecs > 0
         DO WHILE nSkipped < nRecs
           IF ! oDataSource:Eof()
             oDataSource:next( )
             IF oDataSource:Eof()
               oDataSource:prior( )
               EXIT
             ENDIF
             nSkipped++
           ENDIF
         ENDDO
      ELSEIF nRecs < 0
         DO WHILE nSkipped > nRecs
           IF ! oDataSource:Bof()
             oDataSource:prior( )
             IF oDataSource:Bof()
               EXIT
             ENDIF
             nSkipped--
           ENDIF
         ENDDO
      ENDIF
   ENDIF

   RETURN nSkipped

STATIC FUNCTION ODBCFGet( cFieldName, oDataSource )

   IF HB_ISSTRING( cFieldName )
      // For changing value rather write a decent SQL statement
      RETURN {| x | iif( x == NIL, oDataSource:FieldByName( cFieldName ):value, NIL ) }
   ENDIF

   RETURN NIL
