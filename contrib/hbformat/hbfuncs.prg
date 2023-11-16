/*
 * Processing .hbx files for function lists
 *
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "directry.ch"

PROCEDURE __hbformat_BuildListOfFunctions( /* @ */ cFunctions, cHBXList )

   LOCAL aFile
   LOCAL cName
   LOCAL lContribHBR := .F.

   /* from built-in core .hbx file */
   HBXToFuncList( @cFunctions, __harbour_hbx() )

   /* from .hbr container files */
   FOR EACH aFile IN Directory( hb_DirBase() + "*.hbr" )
      IF hb_FileMatch( hb_FNameName( aFile[ F_NAME ] ), "contrib" )
         lContribHBR := .T.
      ENDIF
      FOR EACH cName IN hb_Deserialize( hb_ZUncompress( hb_MemoRead( hb_DirBase() + aFile[ F_NAME ] ) ) )
         cFunctions += "," + cName:__enumKey()
      NEXT
   NEXT

   /* from standalone .hbx files in some known locations */
   IF ! lContribHBR
      WalkDir( hb_DirBase() + ".." + hb_ps() + "contrib", @cFunctions )
   ENDIF
   WalkDir( hb_DirBase() + ".." + hb_ps() + "addons", @cFunctions )

   /* from specified list of .hbx files */

   FOR EACH cName IN hb_ATokens( cHBXList )
      HBXToFuncList( @cFunctions, hb_MemoRead( hb_PathJoin( hb_DirBase(), cName ) ) )
   NEXT

   RETURN

STATIC PROCEDURE WalkDir( cDir, /* @ */ cFunctions )

   LOCAL aFile

   cDir := hb_DirSepAdd( cDir )

   FOR EACH aFile IN hb_DirScan( cDir, "*.hbx" )
      HBXToFuncList( @cFunctions, hb_MemoRead( cDir + aFile[ F_NAME ] ) )
   NEXT

   RETURN

STATIC PROCEDURE HBXToFuncList( /* @ */ cFunctions, cHBX )
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cHBX, Chr( 13 ) ), Chr( 10 ) )
      IF Left( cLine, Len( "DYNAMIC " ) ) == "DYNAMIC "
         cFunctions += "," + SubStr( cLine, Len( "DYNAMIC " ) + 1 )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION __harbour_hbx()

#pragma __streaminclude "harbour.hbx" | RETURN %s
