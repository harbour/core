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
*+    Function ft_fuse()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ft_fuse( cFile, nMode )

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
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ft_FEOF()
   LOCAL lRETURN := theHandle:lAtBottom
RETURN lRETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FReadLn()
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
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FReadLn()

   LOCAL cBuffer := ''

   cBuffer := FReadLn( theHandle:nHan, @cBuffer )

 cBuffer:=STRTRAN(cBuffer,chr(13),'')

RETURN cBuffer

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_FGotop()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_FGotop()

    theHandle:Gotop()
RETURN        NIL

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ft_fskip()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ft_fskip( n)
    TheHandle:Skip(n)
RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FT_MKDIR()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FT_MKDIR( CDIR )

   MAKEDIR( cdir )

RETURN nil

FUNCTION StrPos(cBuffer)
LOCAL nPos,x,cChar
default nPos to 0
      FOR x:=1 to LEN(cBuffer)
          cChar:=SubStr(cBuffer,x,1)
          if cChar>=chr(64) .and. cChar <=Chr(90) .or. cChar>=chr(97) ;
          .and. cChar <=Chr(122) .or. cChar>=Chr(48)  .and. cChar <=chr(57) ;
          .or. cChar==chr(60) .or. cchar==CHR(ASC("-")) ;
          .or. cchar==CHR(ASC("(")) .or. cchar=chr(asc("|")) .or. ;
          cchar==chr(asc('.')) .or. cchar==chr(asc('*')) .or. ;
          cchar==chr(asc('#')) .or. cchar==chr(asc('"')) .or. ;
          cchar==chr(asc('/')) .or. cchar==chr(asc("@")) ;
          .or. cchar==chr(asc("=")) .or. cchar==chr(asc('Ä')) ;
          .or. cchar==chr(asc('?')) .or. cchar==chr(asc('!')) ;
          .or. cchar==chr(asc("<")) .or. cchar==chr(asc('>')) ;
          .or. cchar==chr(asc('!')) .or. cchar==chr(asc('+'))
          
             nPos=x

             Exit
          ENDIF
      NEXT

Return nPos

