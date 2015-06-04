/*
 * $Id: misc.prg 403 2015-02-13 20:56:02Z bedipritpal $
 */

/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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

#include "common.ch"


FUNCTION SetConfig( aINI )
   LOCAL oINI
   STATIC aINI_:= {}
   oINI := aINI_
   IF hb_isArray( aINI )
      aINI_:= aINI
   ENDIF
   RETURN oINI


FUNCTION ReadINI()
   LOCAL cBuffer, a_, s, cKey, cVal
   LOCAL aINI := {}

   IF ! hb_fileExists( "cachemgr.ini" )  /* Always looking at from where cachemgr is invoked */
      RETURN NIL
   ENDIF
   cBuffer := hb_MemoRead( "cachemgr.ini" )
   cBuffer := StrTran( cBuffer, chr( 13 ), "" )
   a_:= hb_aTokens( cBuffer, chr( 10 ) )

   FOR EACH s IN a_
      IF ParseKeyValue( s, @cKey, @cVal )
         AAdd( aINI, { cKey, cVal } )
      ENDIF
   NEXT

   SetConfig( aINI )

   RETURN NIL


FUNCTION ParseKeyValue( s, cKey, cVal )
   LOCAL n

   IF ( n := at( "=", s ) ) > 0
      cKey := alltrim( substr( s, 1, n-1 ) )
      cVal := alltrim( substr( s, n+1 ) )
      RETURN .T.
   ENDIF

   RETURN .F.


FUNCTION IniGetYesNo( cKey )
   LOCAL aINI := SetConfig()

   cKey := lower( cKey )

   IF ascan( aINI, {|e_| lower( e_[ 1 ] ) == cKey .and. lower( e_[ 2 ] ) == "yes" } ) > 0
      RETURN .T.
   ENDIF

   RETURN .F.


FUNCTION GetINIValue( cKey, cDefault )
   LOCAL aINI := SetConfig()
   LOCAL n

   cKey := lower( cKey )
   IF( n := ascan( aINI, {|e_| lower( e_[ 1 ] ) == cKey } ) ) > 0
      RETURN aINI[ n,2 ]
   ENDIF

   RETURN iif( Empty( cDefault ), "", cDefault )


FUNCTION SetINIValue( cKey, cVal )
   LOCAL aINI := SetConfig()
   LOCAL n

   cKey := lower( cKey )
   IF ( n := ascan( aINI, {|e_| lower( e_[ 1 ] ) == cKey } ) ) == 0
      AAdd( aINI, { cKey, cVal } )
   ELSE
      aINI[ n,2 ] := cVal
   ENDIF
   setConfig( aINI )

   RETURN NIL


FUNCTION SaveINI()
   LOCAL aINI := SetConfig()
   LOCAL n, cText

   n := 0
   AEval( aINI, {|e_| n := max( n, Len( e_[ 1 ] ) ) } )

   cText := ""
   AEval( aINI, {|e_| cText += pad( e_[ 1 ], n ) + " = " + e_[ 2 ] + hb_eol() } )

   hb_MemoWrit( "CacheMGR.ini", cText )

   RETURN NIL


FUNCTION GetScreenRowsCols()
   LOCAL aFontNew   := Wvt_GetFontInfo()
   LOCAL nScrWidth  := Wvt_GetScreenWidth()
   LOCAL nScrHeight := Wvt_GetScreenHeight() - 40
   LOCAL wvtRows    := int( nScrHeight / aFontNew[ 6 ] )
   LOCAL wvtCols    := int( nScrWidth  / aFontNew[ 7 ] ) - 1

   RETURN { wvtRows, wvtCols }


FUNCTION ChangeConsoleFont()
   LOCAL nSel := Alert( "Console Font?", { "Courier", "Lucida Console", "Terminal" } )

   IF nSel > 0
      SetINIValue( "Font", iif( nSel == 1, "Courier", iif( nSel == 2, "Lucida Console", "Terminal" ) ) )
      Wvt_SetFont( GetINIValue( "Font", "Courier" ) )
   ENDIF

   RETURN NIL

