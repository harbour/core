/*
 * Harbour Project source code:
 * RTF Support Code For FT_HELPC
 *
 * Copyright 2000 Luiz Rafael Culik Culik@sl.conex.net
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
Class TRTF

DATA cFile
DATA nHandle
METHOD WriteHeader()
METHOD New(cFile)

METHOD WritePar(cPar)
METHOD WriteLink(clink)
METHOD Close()
METHOD WriteParBold(cPar)
METHOD WriteTitle(cTitle,cTopic)
METHOD EndPar()
ENDCLASS
METHOD NEW(cFile) Class TRTF

IF VALTYPE(cFile ) <> NIL .and. VALTYPE(cFile )=="C"
     Self:cFile:=cFile
     Self:nHandle:=FCREATE(Self:cFile)
ENDIF
RETURN Self

METHOD WriteHeader()  CLASS TRTF
LOCAL cHeader:='{\rtf1\ansi\pard\plain\fs20'+CRLF+;
'\deff5{\fonttbl'+CRLF+;
'{\f0\froman Tms Rmn;}'+CRLF+;
'{\f1\fdecor Symbol;}'+CRLF+;
'{\f2\fswiss Helv;}'+CRLF+;
'{\f3\fmodern LinePrinter;}'+CRLF+;
'{\f4\froman Terminal;}'+CRLF+;
'{\f5\froman Times New Roman;}'+CRLF+;
'{\f6\fswiss Arial;}'+CRLF+;
'{\f7\froman CG Times (WN);}'+CRLF+;
'{\f8\fmodern Courier;}'+CRLF+;
'{\f9\fmodern Modern;}'+CRLF+;
'{\f10\fscript Script;}'+CRLF+;
'{\f11\fswiss Univers (WN);}'+CRLF+;
'{\f12\fnil Wingdings;}'+CRLF+;
'{\f13\fswiss MS Sans Serif;}'+CRLF+;
'}'+CRLF

LOCAL cColortable:='{\colortbl;'+CRLF+;
'\red0\green0\blue0;'+CRLF+;
'\red0\green0\blue128;'+CRLF+;
'\red0\green128\blue128;'+CRLF+;
'\red0\green128\blue0;'+CRLF+;
'\red128\green0\blue0;'+CRLF+;
'\red128\green0\blue128;'+CRLF+;
'\red128\green128\blue0;'+CRLF+;
'\red128\green128\blue128;'+CRLF+;
'\red64\green64\blue64;'+CRLF+;
'\red0\green0\blue255;'+CRLF+;
'\red0\green255\blue255;'+CRLF+;
'\red0\green255\blue0;'+CRLF+;
'\red255\green0\blue0;'+CRLF+;
'\red192\green192\blue192;'+CRLF+;
'\red255\green255\blue0;'+CRLF+;
'\red255\green255\blue255;'+CRLF+;
'}'+CRLF

FWRITE(Self:nHandle,cHeader)
FWRITE(Self:nHandle,cColorTable)
RETURN Self

METHOD WritePar(cPar)  CLASS TRTF
        FWRITE(Self:nHandle,'\pard{\cf1\fs20 '+cPar)
RETURN Self
METHOD EndPar() CLASS TRTF
 FWRITE(Self:nHandle,     ' }\par'+CRLF)
 RETURN Self
METHOD WriteParBold(cPar)  CLASS TRTF
     FWRITE(Self:nHandle,'\pard{\plain\cf1\f2\fs20\i\b\qc '+alltrim(cPar)+' }\par'+CRLF)
RETURN Self

METHOD WriteTitle(cTitle,cTopic) Class TRTF
LOCAL cTemp,nPos
LOCAL cWrite
nPos:=At("()",cTitle)
IF nPos>0
     cTemp:=alltrim(STRTRAN(cTitle,"()","xx"))
else
   cTemp:=alltrim(cTitle)
endif
cTopic:=Alltrim(cTopic)
cWrite:='{\f2'+CRLF+;
'  #{\footnote \pard\plain \fs20 # '+"IDH_"+cTemp  +' }'+CRLF+;
'  ${\footnote \pard\plain \fs20 $ '+Alltrim(cTopic) +' }'+CRLF+;
'  K{\footnote \pard\plain \fs20 K '+Alltrim(cTopic) +' }'+CRLF+;
'}'+CRLF

     FWRITE(Self:nHandle,cWrite)
     Self:WriteParBold(cTopic)
RETURN Self
METHOD CLOSE()  CLASS TRTF
FWRITE(Self:nHandle,'\page'+CRLF)
FWRITE(Self:nHandle,'}'+CRLF)

FCLOSE(Self:nHandle)
RETURN Self

METHOD WriteLink(cLink) CLASS TRTF

     FWRITE(Self:nHandle,'\pard{\cf1\fs20 {\f2\uldb '+alltrim(cLink)+'}{\v\f2 '+"IDH_"+alltrim(strtran(cLink,"()","xx")) +'} }\par'+CRLF)

RETURN Self
