*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊
*+
*+    Source Module => C:\HARB\UTILS\HBDOC\OS2.PRG
*+
*+    Click! is a Clipper/Xbase++ source code reformatter.
*+    
*+    Copyright(C) 1996-1999 by Phil Barnett.
*+       
*+    This program is free software; you can redistribute it and/or modify it
*+    under the terms of the GNU General Public License as published by the
*+    Free Software Foundation; either version 2 of the License, or (at your
*+    option) any later version.
*+    
*+    This program is distributed in the hope that it will be useful, but
*+    WITHOUT ANY WARRANTY; without even the implied warranty of
*+    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*+    General Public License for more details.
*+    
*+    You should have received a copy of the GNU General Public License along
*+    with this program; if not, write to the Free Software Foundation, Inc.,
*+    675 Mass Ave, Cambridge, MA 02139, USA.
*+    
*+    You can contact me at:
*+    
*+    Phil Barnett
*+    Box 944
*+    Plymouth, Florida  32768
*+    
*+    or
*+    
*+    philb@iag.net
*+
*+    Functions: Class TOs2
*+
*+    Reformatted by Click! 2.03 on Apr-10-2000 at 10:28 am
*+
*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OS/2 IPF Documentation Support Code For HBDOC
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

#define CRLF hb_osnewline()

#include 'hbclass.ch'
#include 'common.ch'

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class TOs2
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
CLASS TOs2

   DATA cFile
   DATA nHandle
   DATA aLinkRef
   DATA nRef
   DATA aHeadRef 
   METHOD New( cFile )
   METHOD WritePar( cPar )
   METHOD WriteLink( cLink )
   METHOD ScanLink( cLink )
   METHOD ScanRef( cRef )    
   METHOD WriteJumpLink( cLink, cName, cText )
   METHOD CLOSE()
   METHOD WriteText( cPar )
   METHOD WriteParBold( cPar ,lMarg)
   METHOD WriteTitle( cTopic, cTitle ,cCategorie)
   METHOD DostoOs2Text( cText )
   METHOD WriteJumpTitle( cTitle, cTopic )
ENDCLASS

METHOD New( cFile ) CLASS TOs2

   IF Self:aLinkRef == NIL
      Self:aLinkRef := {}
      Self:aHeadRef := {}
      Self:nRef     := 1
   ENDIF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF

   FWRITE( Self:nHandle, ':userdoc.' + CRLF )

   FWRITE( Self:nHandle, ':docprof toc=123456.' + CRLF )
   FWRITE( Self:nHandle, ':title.' + "Harbour Reference Guide" + CRLF )

   FWRITE( Self:nHandle, '.* HBDOC generated IPF Source File.' + CRLF )

   FWRITE( Self:nHandle, '.* HBDOC Document Source Extractor, (c)1999-2000 Luiz Rafael Culik.' + CRLF )

RETURN Self

METHOD WritePar( cPar ) CLASS TOs2

   FWRITE( Self:nHandle,  Self:DostoOs2Text( cPar ) + CRLF)

RETURN Self

METHOD WriteText( cPar ) CLASS TOs2

   FWRITE( Self:nHandle, cPar + CRLF )

RETURN Self

METHOD WriteParBold( cPar ,lMarg) CLASS TOs2
DEFAULT lMarg to .t.
    IF lMarg
      FWRITE( Self:nHandle,".br"+CRLF+ ":hp2." + SELF:DosToOs2Text( cPar ) + ':ehp2.'+CRLF +".br" + CRLF +":p."+CRLF+':lm margin=6.' +CRLF )
    Else
      FWRITE( Self:nHandle,":hp2." + SELF:DosToOs2Text( cPar ) + ':ehp2.'+CRLF +".br")
    Endif

RETURN Self

METHOD WriteTitle( cTopic, cTitle ,cCategory) CLASS TOs2

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite
   LOCAL nItem
   LOCAL nrItem
   LOCAL cRefCateg
   
   cTopic := ALLTRIM( cTopic )
   cRefCateg:=ALLTRIM(left(cCategory,5 )) 
   IF Self:Scanlink( cTopic ) == 0
      nItem := ASCAN( Self:aLinkRef, { | a | upper(a[ 1 ]) == upper(cTopic )} )
   ELSE             // Just in case that nItem>0 so the Link is already referenced
      nItem := ASCAN( Self:aLinkRef, { | a | upper(a[ 1 ]) == upper(cTopic) } )
   ENDIF
   FWRITE( Self:nHandle, ':h1 res=' + ALLTRIM( STR( nItem ) ) + '.' + cTopic + CRLF )
   If Self:ScanRef(cRefCateg)==0
      nrItem := ASCAN( Self:aHeadRef, { | a | upper(a) == upper(cRefCateg )} )
      FWRITE( Self:nHandle, ':i1 id=' + ::aHeadRef[nrItem] + " global." + UPPER( cCategory ) + CRLF )
   ELSE             // Just in case that nItem>0 so the Link is already referenced
      nrItem := ASCAN( Self:aHeadRef, { | a | upper(a) == upper(cRefCateg) } )
   ENDIF
   if nritem>0
      FWRITE( Self:nHandle, ':i2 refid=' + ::aHeadRef[nrItem] + " global." + UPPER( cTopic ) + CRLF )
   Endif
   cTopic := ::DosToOs2Text(cTopic)
   cTitle := ::DosToOs2Text(cTitle)

   FWRITE( Self:nHandle, ":p." + cTitle + CRLF +".br"+CRLF)

RETURN Self

METHOD CLOSE() CLASS TOs2

   FWRITE( Self:nHandle, ':euserdoc.' + CRLF )

   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink ) CLASS TOs2

   LOCAL nItem

   IF Self:Scanlink( cLink ) == 0
      nItem := ASCAN( Self:aLinkRef, { | a | upper(a[ 1 ]) == upper(cLink) } )                // Again.
   ELSE
      nItem := ASCAN( Self:aLinkRef, { | a, b | upper(a[ 1 ]) == upper(cLink) } )

   ENDIF

   IF nItem = 0
      nItem := Self:nRef
   ENDIF

   FWRITE( Self:nHandle, ":link reftype=hd res=" + ALLTRIM( STR( nItem ) ) + "." + Self:aLinkRef[ nItem, 1 ] + ":elink." + CRLF )

   FWRITE( Self:nHandle, ".br" + CRLF )

RETURN Self

METHOD ScanLink( cLink ) CLASS TOs2

   LOCAL nItem
   LOCAL nReturn

   nItem := ASCAN( Self:aLinkRef, { | a, b | Upper(a[ 1 ] )== upper(cLink) } )

   IF nItem == 0
      AADD( Self:aLinkRef, { upper(cLink), Self:nRef } )
      Self:nRef ++
   ENDIF

RETURN nItem
METHOD ScanRef( cLink ) CLASS TOs2

   LOCAL nItem
   LOCAL nReturn

   nItem := ASCAN( Self:aHeadRef, { | a | Upper(a)== upper(cLink) } )

   IF nItem == 0
      AADD( Self:aHeadRef,  upper(cLink))
   ENDIF

RETURN nItem

METHOD DosToOs2Text( cText ) CLASS TOs2

   LOCAL cReturn

   cReturn := STRTRAN( cText, '&', "&amp." )
   cReturn := STRTRAN( cReturn, '"', "&cdq." )
   cReturn := STRTRAN( cReturn, ':', "&colon." )
   cReturn := STRTRAN( cReturn, ',', "&comma." )
   cReturn := STRTRAN( cReturn, '_', "&us." )
   cReturn := STRTRAN( cReturn, '~', "&tilde." )
   cReturn := STRTRAN( cReturn, '|', "&splitvbar." )
   cReturn := STRTRAN( cReturn, '/', "&slash." )
   cReturn := STRTRAN( cReturn, ';', "&semi." )
   cReturn := STRTRAN( cReturn, ')', "&rpar." )
   cReturn := STRTRAN( cReturn, ']', "&rbrk.." )
   cReturn := STRTRAN( cReturn, '}', "&rbrc." )
   cReturn := STRTRAN( cReturn, '(', "&lpar." )
   cReturn := STRTRAN( cReturn, '[', "&lbrk." )
   cReturn := STRTRAN( cReturn, '{', "&lbrc." )
   cReturn := STRTRAN( cReturn, '=', "&eq." )
   cReturn := STRTRAN( cReturn, '$', "&dollar." )
   cReturn := STRTRAN( cReturn, "<", "&lt." )
   cReturn := STRTRAN( cReturn, ">", "&gt." )
   cReturn := STRTRAN( cReturn, "-", "&minus." )
RETURN cReturn

METHOD WriteJumpTitle( cTitle, cTopic ) CLASS TOs2

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   cTopic := ALLTRIM( HB_OEMTOANSI( cTopic ) )

   cWrite := ':fn id=' + cTopic + '.'

   FWRITE( Self:nHandle, cWrite )

   Self:WriteParBold( cTopic )

RETURN Self
METHOD WriteJumpLink( cLink, cText ) CLASS TOs2

   FWRITE( Self:nHandle, "       :link refid=" + ALLTRIM( HB_OEMTOANSI( cLink ) ) + "reftype=fn." + cLink + ":elink." + cText + CRLF )

RETURN Self

*+ EOF: OS2.PRG
