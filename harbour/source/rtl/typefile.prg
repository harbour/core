/*
 * $Id:
 */

/*
 * Harbour Project source code:
 * __Typefile() function
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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

#include 'common.ch'
#include 'error.ch'

STATIC theHandle
#ifdef TEST
function main
type alpha.lnk
type x.lnk
return nil
#endif

FUNCTION __TYPEFILE(cFile,lPrint)
LOCAL lEof := .F.
LOCAL cBuffer := ""
LOCAL Err
LOCAL nHandle
DEFAULT lPrint to .F.

IF lPrint
    SET DEVICE TO PRINTER
    SET PRINTER ON
    SET CONSOLE OFF
ENDIF

IF cFile == NIL 
    err := ErrorNew()
    err:severity := ES_ERROR
    err:genCode  := EG_OPEN
    err:subSystem := "BASE"
    err:SubCode := 2009
    err:Description := "Argument error: __TYPEFILE"
    Eval(ErrorBlock(), err)
ENDIF

nReadHandle := FT_FUSE(cFile)

IF  nReadHandle <0
    err := ErrorNew()
    err:severity := ES_ERROR
    err:genCode  := EG_OPEN
    err:subSystem := "BASE"
    err:SubCode := 2011
    err:Description := "Open Error: " + cFile
    err:OsCode := 2
    Eval(ErrorBlock(), err)
ENDIF
DO WHILE !lEof
       cBuffer := TRIM(ReadLN( @lEof ))
       ? cBuffer
ENDDO

FT_FUSE()
IF lPrint
    SET DEVICE TO SCREEN
    SET PRINTER TO
    SET CONSOLE ON
ENDIF

RETURN NIL

#define xReadBuffer 4096
   /****
*   FT_FUSE(cFile,nMode)   ---> nHandle
*   Open a File
*/

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION ft_fuse()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION ft_fuse( cFile, nMode )

   IF nMode == nil
      nMode := 2
   ENDIF
   IF cFile == Nil
      theHandle:close()
   ENDIF
   IF cFile <> Nil
      IF nMode <> 0
         theHandle := TFileRead():new( cFile ):open( nMode )
      ELSE
         theHandle := TFileRead():new( cFile ):open()
      ENDIF
   ENDIF
RETURN theHandle:nHan

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION ft_FEOF()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION ft_FEOF()

   LOCAL lRETURN := theHandle:lEOF

RETURN lRETURN

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION FReadLn()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION FReadLn( nH, cLine )

   //     cline:= thehandle:readline()
   // ENDIF
   IF theHandle:MoreToRead()
      cLine := theHandle:ReadLine()
   ELSE
      FSEEK( theHandle:nHan, 0, 0 )
      theHandle:lEOF := .f.

      cLine := theHandle:ReadLine()
   ENDIF
RETURN cLine

STATIC FUNCTION ReadLN( leof )

   LOCAL cBuffer := ""

   cBuffer := FT_FREADLN()
   FT_FSKIP( 1 )
   lEof := FT_FEOF()

RETURN cBuffer
//  End of ReadLN

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION FT_FReadLn()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION FT_FReadLn()

   LOCAL cBuffer := ''

   cBuffer := FReadLn( theHandle:nHan, @cBuffer )

RETURN cBuffer

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION FT_FGotop()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION FT_FGotop()

   FSEEK( theHandle:nHan, 0, 0 )

RETURN        NIL

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    STATIC FUNCTION ft_fskip()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION ft_fskip( )

RETURN nil


