/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RTF Documentation Support Code For HBDOC
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

#define CRLF HB_OSNewLine()

#include 'hbclass.ch'
#include 'common.ch'
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class TRTF
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
CLASS TRTF

   DATA cFile
   DATA nHandle
   METHOD WriteHeader()
   METHOD New( cFile )
   METHOD WritePar( cPar )
   METHOD WriteParText( cPar )
   METHOD WriteLink( clink )
   METHOD WriteJumpLink( clink )
   METHOD Close()
   METHOD WriteParBold( cPar ,lCenter)
   METHOD WriteParBoldText( cPar ,cText)
   METHOD WriteTitle( cTitle, cTopic )
   METHOD WriteJumpTitle( cTitle, cTopic )
   METHOD EndPar()
   METHOD EndPage()
ENDCLASS

METHOD NEW( cFile ) CLASS TRTF
   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
RETURN Self

METHOD WriteHeader() CLASS TRTF

   LOCAL cHeader := '{\rtf1\ansi\pard\fs20' + CRLF + ;
      '\deff5{\fonttbl' + CRLF + ;
      '{\f0\froman Tms Rmn;}' + CRLF + ;
      '{\f1\fdecor Symbol;}' + CRLF + ;
      '{\f2\fswiss Helv;}' + CRLF + ;
      '{\f3\fmodern LinePrinter;}' + CRLF + ;
      '{\f4\froman Terminal;}' + CRLF + ;
      '{\f5\froman Times New Roman;}' + CRLF + ;
      '{\f6\fswiss Arial;}' + CRLF + ;
      '{\f7\froman CG Times (WN);}' + CRLF + ;
      '{\f8\fmodern Courier;}' + CRLF + ;
      '{\f9\fmodern Modern;}' + CRLF + ;
      '{\f10\fscript Script;}' + CRLF + ;
      '{\f11\fswiss Univers (WN);}' + CRLF + ;
      '{\f12\fnil Wingdings;}' + CRLF + ;
      '{\f13\fswiss MS Sans Serif;}' + CRLF + ;
      '}' + CRLF

   LOCAL cColortable := '{\colortbl;' + CRLF + ;
      '\red0\green0\blue0;' + CRLF + ;
      '\red0\green0\blue128;' + CRLF + ;
      '\red0\green128\blue128;' + CRLF + ;
      '\red0\green128\blue0;' + CRLF + ;
      '\red128\green0\blue0;' + CRLF + ;
      '\red128\green0\blue128;' + CRLF + ;
      '\red128\green128\blue0;' + CRLF + ;
      '\red128\green128\blue128;' + CRLF + ;
      '\red64\green64\blue64;' + CRLF + ;
      '\red0\green0\blue255;' + CRLF + ;
      '\red0\green255\blue255;' + CRLF + ;
      '\red0\green255\blue0;' + CRLF + ;
      '\red255\green0\blue0;' + CRLF + ;
      '\red192\green192\blue192;' + CRLF + ;
      '\red255\green255\blue0;' + CRLF + ;
      '\red255\green255\blue255;' + CRLF + ;
      '}' + CRLF

   FWRITE( Self:nHandle, cHeader )

   FWRITE( Self:nHandle, cColorTable )

RETURN Self

METHOD WritePar( cPar ) CLASS TRTF
   cPar:=StrTran(cPar,"{","\{")
   cPar:=StrTran(cPar,"}","\}")
   FWRITE( Self:nHandle, '\par \pard\cf1\f6\fs20\b0\i0\li300' + HB_OEMTOANSI(cPar )+CRLF)
RETURN Self
METHOD WriteParText( cPar ) CLASS TRTF
   cPar:=StrTran(cPar,"{","\{")
   cPar:=StrTran(cPar,"}","\}")
   FWRITE( Self:nHandle,  HB_OEMTOANSI(cPar ))
RETURN Self
METHOD EndPar() CLASS TRTF
   FWRITE( Self:nHandle, '\par' + CRLF )
RETURN Self

METHOD WriteParBold( cPar,lCenter ) CLASS TRTF
   DEFAULT lCenter to .F.
   cPar:=StrTran(cPar,"{","\{")
   cPar:=StrTran(cPar,"}","\}")
   if lCenter
       FWRITE( Self:nHandle, '\par \pard\qc\cf1\f6\fs30\i\b\li300 ' +  ALLTRIM(HB_OEMTOANSI( cPar ))  + CRLF )
   else
       FWRITE( Self:nHandle, '\par \pard\cf1\f6\fs30\i\b\li300 ' +  ALLTRIM(HB_OEMTOANSI( cPar ))  + CRLF )
   Endif
RETURN Self

METHOD WriteParBoldText( cPar,cText ) CLASS TRTF

   cPar:=StrTran(cPar,"{","\{")
   cPar:=StrTran(cPar,"}","\}")
   cText:=StrTran(cText,"{","\{")
   cText:=StrTran(cText,"}","\}")

   FWRITE( Self:nHandle, '\par \pard\cf1\f6\fs20\i\b       ' +  ALLTRIM(HB_OEMTOANSI( cPar )) + ' \b\cf1\f6\fs20\i0\b0\li300 ' +ALLTRIM(HB_OEMTOANSI(cText)) + CRLF )
RETURN Self

METHOD WriteTitle( cTitle, cTopic ) CLASS TRTF

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   IF nPos > 0
      cTemp := ALLTRIM( HB_OEMTOANSI(STRTRAN( cTitle, "()", "xx" ) ))
   ELSE
      cTemp := HB_OEMTOANSI(ALLTRIM( cTitle ))
      cTemp :=STRTRAN( cTemp,"@", "x" )
   ENDIF

   cTopic := ALLTRIM( HB_OEMTOANSI(cTopic ))

   cWrite := '{\f6' + CRLF + ;
             '  #{\footnote \pard\fs20 # ' + "IDH_"+cTemp + ' }' + CRLF + ;
             '  ${\footnote \pard\fs20 $ ' + ALLTRIM( cTopic ) + ' }' + CRLF + ;
             '  K{\footnote \pard\fs20 K ' + ALLTRIM( cTopic ) + ' }' + CRLF + ;
             '}' + CRLF

   FWRITE( Self:nHandle, cWrite )

   FWRITE( Self:nHandle, '\pard\qc\cf1\f6\fs30\i\b ' +  ALLTRIM(HB_OEMTOANSI( cTopic )) +  CRLF )
RETURN Self

METHOD WriteJumpTitle( cTitle, cTopic ) CLASS TRTF

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   IF nPos > 0
      cTemp := ALLTRIM( HB_OEMTOANSI(STRTRAN( cTitle, "()", "xx" ) ))
   ELSE
      cTemp := HB_OEMTOANSI(ALLTRIM( cTitle ))
      cTemp :=STRTRAN( cTemp,"@", "x" )
   ENDIF

   cTopic := ALLTRIM( HB_OEMTOANSI(cTopic ))

   cWrite := '{\f6' + CRLF + ;
             '  #{\footnote \pard\fs20 # ' + "IDH_"+cTemp + ' }' + CRLF + ;
             '  ${\footnote \pard\fs20 $ ' + ALLTRIM( cTopic ) + ' }' + CRLF + ;
             '}' + CRLF

   FWRITE( Self:nHandle, cWrite )

   Self:WriteParBold( cTopic )

RETURN Self
METHOD EndPage() CLASS TRTF
   FWRITE( Self:nHandle,"\par "+CRLF+ '\page' + CRLF )
Return Self

METHOD CLOSE() CLASS TRTF
   FWRITE( Self:nHandle, '\page' + CRLF )

//   FWRITE( Self:nHandle, '}' + CRLF )

   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink ) CLASS TRTF

   FWRITE( Self:nHandle, '\par \pard\cf1\fs20       {\f6\uldb '+ ALLTRIM( HB_OEMTOANSI(cLink) ) + '}{\v\f6 ' + "IDH_"+IF(AT( "()",cLink)>0 , ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "()", "xx" ) )),ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "@", "x" )) )) + '}' + CRLF )

RETURN Self
METHOD WriteJumpLink( cLink,cName,cText ) CLASS TRTF

   FWRITE( Self:nHandle, '\par \pard\cf1\fs20       {\f6\ul '+ ALLTRIM( HB_OEMTOANSI(cName) ) + '}{\v\f6 ' + "IDH_"+IF(AT( "()",cLink)>0 , ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "()", "xx" ) )),ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "@", "x" )) )) + '}'+cText+  CRLF )

RETURN Self

