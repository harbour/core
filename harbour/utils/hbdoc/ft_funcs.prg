/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FT_FUNCS.Prg File support Functions For hbdoc
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"
#include 'hbdocdef.ch'
#include 'common.ch'

#define xReadBuffer 4096

STATIC TheHandle
/****
*   FT_FUSE(cFile,nMode)   ---> nHandle
*   Open a File
*/

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_FUSE()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FUSE( cFile, nMode )

   IF nMode == nil
      nMode := 2
   ENDIF
   IF cFile == Nil
      theHandle:closefile()
   ENDIF
   IF cFile <> Nil
      IF nMode <> 0
         theHandle := FileBase():new( cFile ):open()
      ELSE
         theHandle := FileBase():new( cFile ):open()
      ENDIF
   ENDIF
RETURN theHandle:nHan

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ft_FEOF()
*+
*+    Called from ( hbdoc.prg    )   1 - function readln()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ft_FEOF()

   LOCAL lRETURN := theHandle:lAtBottom
RETURN lRETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FReadLn()
*+
*+    Called from ( ft_funcs.prg )   1 - function ft_freadln()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FReadLn( nH, cLine )

   cLine := theHandle:retrieve()
RETURN cLine

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_FReadLn()
*+
*+    Called from ( hbdoc.prg    )   1 - function readln()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FReadLn()

   LOCAL cBuffer := ''

   cBuffer := FReadLn( theHandle:nHan, @cBuffer )

   cBuffer := STRTRAN( cBuffer, CHR( 13 ), '' )

RETURN cBuffer

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_FGotop()
*+
*+    Called from ( genng.prg    )   1 - static function readfromtop()
*+                ( genrtf.prg   )   1 - static function readfromtop()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FGotop()

   theHandle:Gotop()
RETURN NIL

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_FSKIP()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FSKIP( n )

   TheHandle:Skip( n )
RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_MKDIR()
*+
*+    Called from ( hbdoc.prg    )   6 - function main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_MKDIR( CDIR )

   MAKEDIR( cdir )

RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function StrPos()
*+
*+    Called from ( genhtm1.prg  )   1 - function prochtmtable()
*+                ( genhtm2.prg  )   1 - function prochtmtable()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION StrPos( cBuffer )

   LOCAL nPos
   LOCAL x
   LOCAL cChar
   DEFAULT nPos TO 0
   FOR x := 1 TO LEN( cBuffer )
      cChar := SUBSTR( cBuffer, x, 1 )
      IF cChar >= CHR( 64 ) .AND. cChar <= CHR( 90 ) .OR. cChar >= CHR( 97 ) ;
                 .AND. cChar <= CHR( 122 ) .OR. cChar >= CHR( 48 ) .AND. cChar <= CHR( 57 ) ;
                 .OR. cChar == CHR( 60 ) .OR. cchar == CHR( ASC( "-" ) ) ;
                 .OR. cchar == CHR( ASC( "(" ) ) .OR. cchar = CHR( ASC( "|" ) ) .OR. ;
                 cchar == CHR( ASC( '.' ) ) .OR. cchar == CHR( ASC( '*' ) ) .OR. ;
                 cchar == CHR( ASC( '#' ) ) .OR. cchar == CHR( ASC( '"' ) ) .OR. ;
                 cchar == CHR( ASC( '/' ) ) .OR. cchar == CHR( ASC( "@" ) ) ;
                 .OR. cchar == CHR( ASC( "=" ) ) .OR. cchar == CHR( ASC( 'Ä' ) ) ;
                 .OR. cchar == CHR( ASC( '?' ) ) .OR. cchar == CHR( ASC( '!' ) ) ;
                 .OR. cchar == CHR( ASC( "<" ) ) .OR. cchar == CHR( ASC( '>' ) ) ;
                 .OR. cchar == CHR( ASC( '!' ) ) .OR. cchar == CHR( ASC( '+' ) )

         nPos := x

         EXIT
      ENDIF
   NEXT

RETURN nPos

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function GetNumberofTableItems()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+                ( genng.prg    )   1 - function procngdesc()
*+                ( genng1.prg   )   1 - function procngdesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION GetNumberofTableItems( cBuffer )

   LOCAL cItem
   LOCAL nItem := 0

   cBuffer := ALLTRIM( cBuffer )

   DO WHILE AT( SPACE( 3 ), cBuffer ) > 0
      cItem := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      IF AT( SPACE( 3 ), cBuffer ) == 0
         nItem ++
      ELSE
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, '' ) )
         nItem ++
      ENDIF
   ENDDO
   nItem ++
   RETURN nItem

#define EOL CHR(13)+CHR(10)

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FREADline()
*+
*+    Called from ( genng.prg    )   1 - static function readfromtop()
*+                ( genrtf.prg   )   1 - static function readfromtop()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FREADline( nH, cB, nMaxLine )

   LOCAL cLine
   LOCAL nSavePos
   LOCAL nEol
   LOCAL nNumRead
   cLine    := SPACE( nMaxLine )
   cB       := ''
   nSavePos := FSEEK( nH, 0, FS_RELATIVE )
   nNumRead := FREAD( nH, @cLine, nMaxLine )
   IF ( nEol := AT( EOL, SUBSTR( cLine, 1, nNumRead ) ) ) == 0
      cB := cLine
   ELSE
      cB := SUBSTR( cLine, 1, nEol - 1 )
      FSEEK( nH, nSavePos + nEol + 1, FS_SET )
   ENDIF
RETURN nNumRead != 0

*+ EOF: FT_FUNCS.PRG
