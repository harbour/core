/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Norton Guide Support Code For FT_HELPC
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

#define CRLF chr(13)+chr(10)

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
   METHOD CLOSE()
   METHOD WriteParBold(cPar)
   METHOD WriteTitle(cTopic, cTitle )
   METHOD DostoOs2Text(cText)
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

RETURN Self

METHOD WritePar( cPar ) CLASS TOs2

   FWRITE( Self:nHandle, ".br"+CRLF+Self:DostoOs2Text( cPar ) + CRLF )

RETURN Self

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

   fWrite(Self:nHandle,':title.'+ALLTRIM(cTitle)+CRLF)

   fWrite(Self:nHandle,'.* FT_HelpC generated IPF Source File.'+CRLF)

   fWrite(Self:nHandle,'.* FT_HELPC Document Source Extractor, (c)2000 Luiz Rafael Culik.'+CRLF)


   fWrite(Self:nHandle,':h1 res='+Alltrim(str(nItem))+'.'+cTopic+CRLF)


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

/*  $DOC$
 *  $FUNCNAME$
 *     TOs2()
 *  $CATEGORY$
 *     Harbour Tools
 *  $ONELINER$
 *     OS/2 Class
 *  $SYNTAX$
 *     oNg:=TOs2():New(<cFile>)
 *  $ARGUMENTS$
 *     <cFile> Name of the IPF Source file to create
 *  $RETURNS$
 *     An  instance of the TOs2 Class
 *  $DESCRIPTION$
 *     TOs2() is a class that create the Norton Guide Source
 *     Code of the same name you pass to the constructor.
 *     The class methods are as follows:
 *        New(<cFile>) Create a new instance of the THtml class.
 *        Close() Close the create file
 *        WriteTitle(<cTopic>,<cTitle>) Write the file title
 *        WritePar(<cPar>)   Writes a paragrafer
 *        WriteParBold(<cPar>)   Same as WritePar(), but the text is bold style.
 *        WriteLink(<cLink>)  Write a link to another topic
 *        ScanLink(<clink>) Scan the aLinkRef array for a valid topic
 *        DosToOs2Text(<cText>) Convert a Dos string to a OS/2 String
 *  $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     LOCAL oNg
 *
 *     oNg := TOs2():New( "ngi\harbour.ngi" )
 *     oNg:WriteTitle( "Harbour Reference Guide" )
 *     oNg:WritePar( "HARBOUR" )
 *     oNg:WriteLink( "OverView" )
 *     oNg:WriteLink( "License" )
 *     
 *     oNg:WritePar( "See the Links Above" )
 *     oNg:Close()
 *     RETURN Nil
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This is a new Harbour Tools class
 *  $PLATFORMS$
 *     ALL
 *  $FILES$
 *
 *  $SEEALSO$
 *     TCLASS()
 *  $END$
 */
