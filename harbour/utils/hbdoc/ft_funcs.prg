/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File support Functions For hbdoc
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"

STATIC s_oFileBase

/****
*   FT_FUSE( cFile, nMode ) ---> nHandle
*   Open a File
*/
FUNCTION FT_FUSE( cFile, nMode )

   IF cFile == NIL
      s_oFileBase:closefile()
   ELSE
      s_oFileBase := FileBase():new( cFile )
      IF nMode != NIL
         s_oFileBase:nOpenMode := nMode
      ENDIF
      s_oFileBase:open()
   ENDIF

   RETURN s_oFileBase:nHan

FUNCTION FT_FEOF()
   RETURN s_oFileBase:lAtBottom

FUNCTION FReadLn()
   RETURN s_oFileBase:retrieve()

FUNCTION FT_FReadLn()
   RETURN StrTran( FReadLn(), Chr( 13 ), "" )

PROCEDURE FT_FGotop()
   s_oFileBase:Gotop()
   RETURN

PROCEDURE FT_FSKIP( n )
   s_oFileBase:Skip( n )
   RETURN

PROCEDURE FT_MKDIR( cDir )

#ifdef HB_COMPAT_C53
   MakeDir( cDir )
#else
   HB_SYMBOL_UNUSED( cDir ) // TOFIX
#endif

   RETURN

FUNCTION StrPos( cBuffer )

   LOCAL x
   LOCAL cChar

   FOR x := 1 TO LEN( cBuffer )

      cChar := SUBSTR( cBuffer, x, 1 )

      IF cChar >= "A" .AND. cChar <= "Z" .OR. ;
         cChar >= "a" .AND. cChar <= "z" .OR. ;
         cChar >= "0" .AND. cChar <= "9" .OR. ;
         cChar $ "@-(|.*#/=?!<>+" .OR. ;
         cChar == '"'

         RETURN x
      ENDIF
   NEXT

   RETURN 0

FUNCTION GetNumberofTableItems( cBuffer )

   LOCAL cItem
   LOCAL nItem := 0

   cBuffer := ALLTRIM( cBuffer )

   DO WHILE AT( SPACE( 3 ), cBuffer ) > 0
      cItem := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      IF AT( SPACE( 3 ), cBuffer ) == 0
         nItem++
      ELSE
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ) )
         nItem++
      ENDIF
   ENDDO
   nItem++

   RETURN nItem

FUNCTION FREADline( nH, cB, nMaxLine )

   LOCAL cLine := SPACE( nMaxLine )
   LOCAL nSavePos
   LOCAL nEol
   LOCAL nNumRead

   cB       := ""
   nSavePos := FSEEK( nH, 0, FS_RELATIVE )
   nNumRead := FREAD( nH, @cLine, nMaxLine )
   IF ( nEol := AT( hb_OSNewLine(), SUBSTR( cLine, 1, nNumRead ) ) ) == 0
      cB := cLine
   ELSE
      cB := SUBSTR( cLine, 1, nEol - 1 )
      FSEEK( nH, nSavePos + nEol + 1, FS_SET )
   ENDIF

   RETURN nNumRead != 0
