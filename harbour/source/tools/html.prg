/*
 * Harbour Project source code:
 * HTML Support Code For FT_HELPC
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

#include 'hbclass.ch'

#define CRLF HB_OSNewLine()

CLASS THTML

DATA nHandle
DATA cFile
METHOD New(cFile)
METHOD WriteTitle(cTitle)
METHOD WritePar(cPar)
METHOD WriteParBold(cPar)
METHOD WriteLink(cLink)
METHOD Close()
ENDCLASS

Method New(cFile) CLASS THTML
IF VALTYPE(cFile ) <> NIL .and. VALTYPE(cFile )=="C"
     Self:cFile:=cFile
     Self:nHandle:=FCREATE(Self:cFile)
ENDIF
FWRITE(Self:nHandle,"<HEAD>"+CRLF)
RETURN Self
METHOD WriteTitle(cTitle) CLASS THTML
FWRITE(Self:nHandle,"<TITLE>"+CRLF+cTitle+CRLF+"</Title>"+CRLF+'<body bgcolor="#FFFFFF">'+CRLF)
RETURN Self
METHOD WritePar(cPar)     CLASS THTML
FWRITE(Self:nHandle,"<p>"+cPar+'</p>'+CRLF)
RETURN Self

METHOD WriteParBold(cPar) CLASS THTML
FWRITE(Self:nHandle,"<p><b>"+cPar+'</b></p>'+CRLF)
RETURN Self
METHOD Close()            CLASS THTML
FWRITE(Self:nHandle,"</body>"+CRLF)
FCLOSE(Self:nHandle)
RETURN Self
METHOD WriteLink(cLink) CLASS THTML
LOCAL nPos,cTemp:=''
nPos:=AT("()",cLink)
IF nPos>0
     cTemp:=SubStr(cLink,1,nPos-1)+'.html'
     FWrite(Self:nHandle,"<p><a href="+cTemp+">"+cLink+"</a></p>"+CRLF)
ELSE
     cTemp:=Alltrim(cLink)+'.html'
      FWrite(Self:nHandle,"<p><a href="+cTemp+">"+cLink+"</a></p>"+CRLF)
ENDIF
RETURN Self
