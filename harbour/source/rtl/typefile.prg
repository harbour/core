/*
 * $Id$
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
#include 'fileio.ch'
#define pBUFFER_LENGTH 4096
#define CRLF HB_OSNEWLINE()

#xtranslate FTELL(<nhandle>) => FSEEK(<nhandle>,0,FS_RELATIVE)
Static TheHandle
#ifdef TEST
function main
type treport.prg
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

nHandle := FT_FUSE(cFile)

IF  nHandle <0
    err := ErrorNew()
    err:severity := ES_ERROR
    err:genCode  := EG_OPEN
    err:subSystem := "BASE"
    err:SubCode := 2011
    err:Description := "Open Error: " + cFile
    err:OsCode := 2
    Eval(ErrorBlock(), err)
ENDIF

DO WHILE Freadln(thehandle,@cbuffer,pBUFFER_LENGTH)
       ? cBuffer
ENDDO

FT_FUSE()
IF lPrint
    SET DEVICE TO SCREEN
    SET PRINTER TO
    SET CONSOLE ON
ENDIF

RETURN NIL


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
      fclose(theHandle)
   ENDIF
   IF cFile <> Nil
      IF nMode <> 0
         theHandle := FOPEN( cFile , nMode )
      ELSE
         theHandle := FOPEN( cFile)
      ENDIF
   ENDIF
RETURN theHandle
Static function Freadln(nh,cb,nmaxline)
local cline,nsavepos,neol,nnumread
cline:=space(nmaxline)
cb:=''
nsavepos:=FTELL(nh)
nnumread:=fread(nh,@cline,nmaxline)
if (neol:= at(CRLF,substr(cline,1,nnumread)))==0
    cb:=cline
else
    cb:=substr(cline,1,neol-1)
    fseek(nh,nsavepos+neol+1,FS_SET)
endif
return nnumread != 0
