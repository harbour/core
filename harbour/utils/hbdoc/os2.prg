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
   METHOD New( cFile )
   METHOD WritePar(cPar)
   METHOD WriteLink(cLink)
   METHOD ScanLink(cLink)
   
   METHOD WriteJumpLink( cLink,cName,cText )
   METHOD Close()
   METHOD WriteText(cPar)
   METHOD WriteParBold(cPar)
   METHOD WriteTitle(cTopic, cTitle )
   METHOD DostoOs2Text(cText)
   METHOD WriteJumpTitle( cTitle, cTopic ) 
ENDCLASS

METHOD New( cFile ) CLASS TOs2

   IF Self:aLinkRef==NIL
      Self:aLinkRef:={}
      Self:nRef:=1
   ENDIF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF

   FWRITE( Self:nHandle, ':userdoc.'+CRLF)

   FWRITE( Self:nHandle, ':docprof toc=123456.'+CRLF)
   fWrite(Self:nHandle,':title.'+"Harbour Reference Guide"+CRLF)

   fWrite(Self:nHandle,'.* FT_HelpC generated IPF Source File.'+CRLF)

   fWrite(Self:nHandle,'.* FT_HELPC Document Source Extractor, (c)2000 Luiz Rafael Culik.'+CRLF)

RETURN Self

METHOD WritePar( cPar ) CLASS TOs2

   FWRITE( Self:nHandle, Self:DostoOs2Text( cPar ) + CRLF )

RETURN Self

method WriteText(cPar) CLASS TOs2

   FWRITE( Self:nHandle, cPar  + CRLF )

Return Self

METHOD WriteParBold( cPar ) CLASS TOs2

   FWRITE( Self:nHandle, ':p.:hp2.' + ALLTRIM( cPar ) +  CRLF +':ehp2.'+ CRLF)

RETURN Self

METHOD WriteTitle( cTopic, cTitle ) CLASS TOs2

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite
   LOCAL nItem
   cTopic := ALLTRIM( cTopic )

   IF  Self:Scanlink(cTopic) ==0
         nItem:=ASCAN(Self:aLinkRef,{|a| a[1]==cTopic})
   ELSE  // Just in case that nItem>0 so the Link is already referenced
     nItem:=ASCAN(Self:aLinkRef,{|a| a[1]==cTopic})
   ENDIF



   fWrite(Self:nHandle,':h1 res='+Alltrim(str(nItem))+'.'+cTopic+CRLF)
   fWrite(Self:nHandle,':i1 id='+UPPER(cTopic)+"."+UPPER(cTopic)+CRLF)
   FWRITE( Self:nHandle,":p."+cTitle+CRLF)


RETURN Self

METHOD CLOSE() CLASS TOs2

   FWRITE( Self:nHandle,':euserdoc.'+CRLF)

   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink ) CLASS TOs2
   LOCAL nItem

   if  Self:Scanlink(cLink) ==0
         nItem:=ASCAN(Self:aLinkRef,{|a| a[1]==cLink})  // Again.
   ELSE
      nItem:=ASCAN(Self:aLinkRef,{|a,b| a[1]==cLink})

   endif

   IF nItem=0
      nItem:=Self:nRef
   ENDIF

   FWRITE( Self:nHandle, ":link reftype=hd res="+ALLTRIM(STR(nItem))+"."+Self:aLinkRef[nItem,1]+":elink."+CRLF)

   FWRITE( Self:nHandle,".br"+CRLF)

RETURN Self

METHOD ScanLink(cLink) CLASS TOs2

   LOCAL nItem
   LOCAL nReturn

   nItem:=ASCAN(Self:aLinkRef,{|a,b| a[1]==cLink})

   IF nItem==0
      AADD(Self:aLinkRef,{cLink,Self:nRef})
      Self:nRef++
   ENDIF

RETURN nItem

METHOD DosToOs2Text(cText) CLASS TOs2
   LOCAL cReturn

   cReturn := STRTRAN(cText,'"',"&cdq.")
   cReturn := STRTRAN(cReturn,':',"&colon.")
   cReturn := STRTRAN(cReturn,',',"&comma.")

Return cReturn

METHOD WriteJumpTitle( cTitle, cTopic ) class TOs2

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   cTopic := ALLTRIM( HB_OEMTOANSI(cTopic ))

   cWrite := ':fn id='+cTopic+'.'

   FWRITE( Self:nHandle, cWrite )

   Self:WriteParBold( cTopic )

RETURN Self
METHOD WriteJumpLink( cLink,cText ) class TOs2

   FWRITE( Self:nHandle, "       :link refid="+aLLTRIM( HB_OEMTOANSI(cLink) ) +"reftype=fn."+ cLink+":elink." +cText+  CRLF )

RETURN Self

