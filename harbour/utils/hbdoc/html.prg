/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HTML Support Code For HBDOC
 * HTML .CMH support code for HBDOC
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

#include 'hbclass.ch'
#include 'common.ch'
#define CRLF HB_OSNewLine()
STATIC nX := 0

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class THTML
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
CLASS THTML

   DATA nHandle
   DATA cFile
   METHOD New( cFile ,aMetaContents)
   METHOD WriteTitle( cTitle )
   METHOD WritePar( cPar )
   METHOD WriteParBold( cPar )
   METHOD WriteLink( cLink ,cName)
   METHOD WriteLinkTable( cLink ,cName)
   METHOD WriteChmLink( cLink ,cName)
   METHOD WriteText( cText )
   METHOD WriteMetaTag(cTag,cDescription)
   METHOD CLOSE()
   // The Follow methods is for html source files for .CHM help
   METHOD NewChm( cFile ,aMetaContents,cFuncName)
   METHOD ADDoBJECT(cType,cClassid)
   METHOD ADDPARAM(cName,cValue)
   METHOD EndOBJect()
   METHOD NewContent(cFile)
   METHOD ListItem()
ENDCLASS

METHOD New( cFile,aMetaContents ) CLASS THTML
    
   Local nCount
   IF Nx > 0
      FCLOSE( NX )
   ENDIF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   nX := Self:nHandle
   FWRITE( Self:nHandle, "<HTML>" + CRLF )
   FWRITE( Self:nHandle, "<HEAD>" + CRLF )
   if Valtype(aMetaContents) <> NIL .and. Valtype(aMetaContents)=="A"
   For nCount:=1 to len(aMetaContents)
      Self:WriteMetaTag(aMetaContents[nCount,1],aMetaContents[nCount,2])
   NEXT
    Endif


RETURN Self

METHOD WriteTitle( cTitle ) CLASS THTML

   FWRITE( Self:nHandle, "<TITLE>" + CRLF + cTitle + CRLF + "</Title>" + CRLF + '</HEAD>' + CRLF  )
   FWRITE( Self:nHandle, "<BODY>" + CRLF )
RETURN Self

METHOD WritePar( cPar ) CLASS THTML

   //   cPar:=STRTRAN(cPar,"<","&lt;")
   //   cPar:=STRTRAN(cPar,">","&gt;")
   FWRITE( Self:nHandle, '<dd>' + ALLTRIM( cPar ) + '</dd>' + CRLF )

RETURN Self
METHOD WriteText( cPar ) CLASS THTML

   FWRITE( Self:nHandle, cPar + CRLF )

RETURN Self

METHOD WriteParBold( cPar, lEndDl, lPar ) CLASS THTML

   DEFAULT lEnddl TO .T.
   DEFAULT lPar TO .T.
   IF lEndDl .AND. lPar
      FWRITE( Self:nHandle, "</P></dd>" + CRLF + "</DL>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + '</b></dt><p>' + CRLF )
   ELSEIF !lPar .AND. !lEnddl
      FWRITE( Self:nHandle, '<DL>' + CRLF + "<dt><b>" + ALLTRIM( cPar ) + '</b></dt><p>' + CRLF )
   ELSEIF !lPar .AND. lEnddl
      FWRITE( Self:nHandle, "</PRE></dd>" + CRLF + "</DL>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + '</b></dt><p>' + CRLF )
   ELSEIF lPar .AND. !lEnddl
      FWRITE( Self:nHandle, "</P></dd>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + '</b></dt><p>' + CRLF )

   ENDIF
RETURN Self

METHOD CLOSE() CLASS THTML

   FWRITE( Self:nHandle, "</body>" + CRLF )
   FWRITE( Self:nHandle, "</html>" + CRLF )
   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink, cName ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp := ''

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + '.htm'
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
     if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + '.htm'
        else
     cTemp := ALLTRIM( cLink ) 
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, " ", "" )

   FWRITE( Self:nHandle, "<LI><a href=" + LOWER( cTemp ) + ">" + cLink + "</a></LI>" + CRLF )

RETURN Self

METHOD WriteLinkTable( cLink, cName,cInfo ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp := ''

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + '.htm'
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
         if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + '.htm'
        else
     cTemp := ALLTRIM( cLink ) 
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, " ", "" )
   FWRITE( Self:nHandle, "<tr><td><a href=" + LOWER( cTemp ) + ">" + cLink + "</a></td><td>" +cinfo +'</td></tr>'+ CRLF )

RETURN Self

METHOD WriteMetaTag(cTag,cDescription) Class THtml
    fWrite(Self:nHandle,'<META NAME="'+cTag+'" CONTENT="'+cDescription+'">'+CRLF)
return Self

/////////////////////Method for .CHM html source files support////////////////
METHOD NewChm( cFile ,aMetaContents,cFuncName) CLASS THTML
    
   Local nCount
   IF Nx > 0
      FCLOSE( NX )
   ENDIF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   nX := Self:nHandle
   FWRITE( Self:nHandle, "<HTML>" + CRLF +"<HEAD>" +CRLF)
   if Valtype(aMetaContents) <> NIL .and. Valtype(aMetaContents)=="A"
   For nCount:=1 to len(aMetaContents)
      Self:WriteMetaTag(aMetaContents[nCount,1],aMetaContents[nCount,2])
   NEXT
    Endif
   ::WriteTitle(cFuncName)

   FWRITE( Self:nHandle, '<BODY BGCOLOR="#FFFFFF" TEXT="#000000">' + CRLF )
   ::AddObject("application/x-oleobject","clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e")
   ::ADDPARAM("Keyword",cFuncName)
   ::ENDOBJECT()
RETURN Self

method ADDOBJECT(cType,cClassId) Class THTML
   IF VALTYPE(cClassId)<>NIL .and. VALTYPE(cClassId)=="C"
      FWRITE( Self:nHandle,'<OBJECT TYPE="'+cType+'" CLASSID="'+cClassId+'">'+CRLF)
   ELSE
      FWRITE( Self:nHandle,'<OBJECT TYPE="'+ cType +'">'+CRLF)
   ENDIF
RETURN Self
METHOD  ENDOBJECT() Class THTML
   FWRITE( Self:nHandle,"</OBJECT>"+CRLF)
RETURN Self
METHOD ADDPARAM(cType,cValue) Class THTML
   FWRITE( Self:nHandle,'<PARAM NAME="'+cType+ '" VALUE="'+cValue +'">'  +CRLF)
RETURN Self

METHOD NewContent( cFile ) CLASS THTML
    
   Local nCount
   IF Nx > 0
      FCLOSE( NX )
   ENDIF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   nX := Self:nHandle
   FWRITE( Self:nHandle, "<HTML>" + CRLF )
RETURN Self

METHOD ListItem() CLASS tHtml
   FWRITE( Self:nHandle, "<LI>" )
RETURN SELF
METHOD WriteChmLink( cLink, cName ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp := ''

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + '.htm'
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
         if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + '.htm'
        else
     cTemp := ALLTRIM( cLink ) 
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, " ", "" )
   FWRITE( Self:nHandle, "<a href=" + LOWER( cTemp ) + ">" + cLink + "</a><br>" + CRLF )
Return Self
*+ EOF: HTML.PRG
