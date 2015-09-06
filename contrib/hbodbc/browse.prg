/*
 * Browse() for ODBC data
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com> for code derived from browse.prg
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour) for original FieldBlock function
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca> for original Skipped function
 * Copyright 2002 Tomaz Zupan <tomaz.zupan@orpo.si> modifications for ODBC
 *   This code is mostly derived work from Harbour RTL browse.prg, browdb.prg.
 *   and fieldbl.prg. Only minor changes were needed to adapt them to ODBC.
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

FUNCTION hb_odbcBrowse( nTop, nLeft, nBottom, nRight, oDataSource )

   LOCAL oBrw
   LOCAL cOldScreen
   LOCAL nOldCursor, f
   LOCAL nKey, nKeyStd
   LOCAL bAction

   IF !( oDataSource:ClassName() == "TODBC" ) .OR. ! oDataSource:Active
      RETURN .F.
   ENDIF

   hb_default( @nTop, 1 )
   hb_default( @nLeft, 0 )
   hb_default( @nBottom, MaxRow() )
   hb_default( @nRight, MaxCol() )

   nOldCursor := SetCursor( SC_NONE )
   cOldScreen := SaveScreen( nTop, nLeft, nBottom, nRight )

   hb_DispBox( nTop, nLeft, nBottom, nRight, HB_B_SINGLE_UNI )
   hb_DispOutAt( nTop + 1, nLeft + 1, Space( nRight - nLeft - 1 ) )

   oBrw := TBrowseNew( nTop + 2, nLeft + 1, nBottom - 1, nRight - 1 )

   oBrw:SkipBlock     := {| nRecs | Skipped( nRecs, oDataSource ) }
   oBrw:GoTopBlock    := {|| oDataSource:first() }
   oBrw:GoBottomBlock := {|| oDataSource:last() }

   oBrw:HeadSep := " " + hb_UTF8ToStrBox( "═" )

   FOR EACH f IN oDataSource:Fields
      oBrw:AddColumn( TBColumn():New( f:FieldName, odbc_FieldGet( f:FieldName, oDataSource ) ) )
   NEXT

   oBrw:Configure()
   oBrw:ForceStable()

   DO WHILE .T.

      DO WHILE .T.
         nKey := Inkey(, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )
         IF oBrw:stabilize() .OR. nKey != 0
            EXIT
         ENDIF
      ENDDO

      IF nKey == 0

         oBrw:forceStable()
         StatLine( oBrw, oDataSource )

         nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

         IF ( bAction := SetKey( nKey ) ) != NIL .OR. ;
            ( bAction := SetKey( nKeyStd ) ) != NIL
            Eval( bAction, ProcName( 1 ), ProcLine( 1 ), "" )
            LOOP
         ENDIF
      ENDIF

      DO CASE
      CASE nKey == K_ESC        ; EXIT
      CASE nKey == K_UP         ; oBrw:Up()
      CASE nKey == K_DOWN       ; oBrw:Down()
      CASE nKey == K_END        ; oBrw:End()
      CASE nKey == K_HOME       ; oBrw:Home()
      CASE nKey == K_LEFT       ; oBrw:Left()
      CASE nKey == K_RIGHT      ; oBrw:Right()
      CASE nKey == K_PGUP       ; oBrw:PageUp()
      CASE nKey == K_PGDN       ; oBrw:PageDown()
      CASE nKey == K_CTRL_PGUP  ; oBrw:GoTop()
      CASE nKey == K_CTRL_PGDN  ; oBrw:GoBottom()
      CASE nKey == K_CTRL_LEFT  ; oBrw:panLeft()
      CASE nKey == K_CTRL_RIGHT ; oBrw:panRight()
      CASE nKey == K_CTRL_HOME  ; oBrw:panHome()
      CASE nKey == K_CTRL_END   ; oBrw:panEnd()
      ENDCASE
   ENDDO

   RestScreen( nTop, nLeft, nBottom, nRight, cOldScreen )
   SetCursor( nOldCursor )

   RETURN .T.

STATIC PROCEDURE StatLine( oBrw, oDataSource )

   LOCAL nTop   := oBrw:nTop - 1
   LOCAL nRight := oBrw:nRight

   hb_DispOutAt( nTop, nRight - 32, "Record " )

   IF oDataSource:LastRec() == 0
      hb_DispOutAt( nTop, nRight - 25, "<none>               " )
   ELSEIF oDataSource:RecNo() == oDataSource:LastRec() + 1
      hb_DispOutAt( nTop, nRight - 45, "         " )
      hb_DispOutAt( nTop, nRight - 25, "                <new>" )
   ELSE
      hb_DispOutAt( nTop, nRight - 25, PadR( hb_ntos( oDataSource:RecNo() ) + "/" + ;
         hb_ntos( oDataSource:LastRec() ), 16 ) + ;
         iif( oBrw:hitTop, "<bof>", "     " ) + ;
         iif( oBrw:hitBottom, "<eof>", "     " ) )
   ENDIF

   RETURN

STATIC FUNCTION Skipped( nRecs, oDataSource )

   LOCAL nSkipped := 0

   IF ! oDataSource:Eof()
      DO CASE
      CASE nRecs == 0
         // ODBC doesn't have Skip( 0 )
      CASE nRecs > 0
         DO WHILE nSkipped < nRecs
            IF ! oDataSource:Eof()
               oDataSource:next()
               IF oDataSource:Eof()
                  oDataSource:prior()
                  EXIT
               ENDIF
               nSkipped++
            ENDIF
         ENDDO
      CASE nRecs < 0
         DO WHILE nSkipped > nRecs
            IF oDataSource:Bof()
               EXIT
            ELSE
               oDataSource:prior()
               IF oDataSource:Bof()
                  EXIT
               ENDIF
               nSkipped--
            ENDIF
         ENDDO
      ENDCASE
   ENDIF

   RETURN nSkipped

STATIC FUNCTION odbc_FieldGet( cFieldName, oDataSource )
   // For changing value rather write a decent SQL statement
   RETURN {| x | iif( x == NIL, oDataSource:FieldByName( cFieldName ):value, NIL ) }

#ifdef HB_LEGACY_LEVEL5
FUNCTION BrowseODBC( ... )
   RETURN hb_odbcBrowse( ... )

FUNCTION hb_odbcSToD( s )
   RETURN hb_CToD( s, "yyyy-mm-dd" )
#endif
