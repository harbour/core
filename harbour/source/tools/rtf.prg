
/*
 * $Id$
 */

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
   METHOD WriteLink( clink )
   METHOD CLOSE()
   METHOD WriteParBold( cPar )
   METHOD WriteTitle( cTitle, cTopic )
   METHOD EndPar()
ENDCLASS
METHOD NEW( cFile ) CLASS TRTF

   IF VALTYPE( cFile ) <> NIL .AND. VALTYPE( cFile ) == "C"
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
RETURN Self

METHOD WriteHeader() CLASS TRTF

   LOCAL cHeader := '{\rtf1\ansi\pard\plain\fs20' + CRLF + ;
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
   FWRITE( Self:nHandle, '\pard{\cf1\fs30 ' + HB_OEMTOANSI("    ")+ HB_OEMTOANSI(cPar )) 
RETURN Self

METHOD EndPar() CLASS TRTF

   FWRITE( Self:nHandle, ' }\par' + CRLF )

RETURN Self

METHOD WriteParBold( cPar ) CLASS TRTF

   cPar:=StrTran(cPar,"{","\{")
   cPar:=StrTran(cPar,"}","\}")
   FWRITE( Self:nHandle, '\pard{\plain\cf1\f2\fs30\i\b\qc ' +  ALLTRIM(HB_OEMTOANSI( cPar )) + ' }\par' + CRLF )
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

   cWrite := '{\f2' + CRLF + ;
             '  #{\footnote \pard\plain \fs20 # ' + "IDH_"+cTemp + ' }' + CRLF + ;
             '  ${\footnote \pard\plain \fs20 $ ' + ALLTRIM( cTopic ) + ' }' + CRLF + ;
             '  K{\footnote \pard\plain \fs20 K ' + ALLTRIM( cTopic ) + ' }' + CRLF + ;
             '}' + CRLF

   FWRITE( Self:nHandle, cWrite )

   Self:WriteParBold( cTopic )

RETURN Self

METHOD CLOSE() CLASS TRTF

   FWRITE( Self:nHandle, '\page' + CRLF )

   FWRITE( Self:nHandle, '}' + CRLF )

   FCLOSE( Self:nHandle )

RETURN Self

METHOD WriteLink( cLink ) CLASS TRTF

   FWRITE( Self:nHandle, '\pard{\cf1\fs30 {\f2\uldb ' + ALLTRIM( HB_OEMTOANSI(cLink) ) + '}{\v\f2 ' + "IDH_"+IF(AT( "()",cLink)>0 , ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "()", "xx" ) )),ALLTRIM( HB_OEMTOANSI(STRTRAN( cLink, "@", "x" )) )) + '} }\par' + CRLF )

RETURN Self

*+ EOF: RTF.PRG
/*  $DOC$
 *  $FUNCNAME$
 *     TRtf()
 *  $CATEGORY$
 *     Harbour Tools
 *  $ONELINER$
 *     Rtf Class
 *  $SYNTAX$
 *     oNg:=TRtf():New(<cFile>)
 *  $ARGUMENTS$
 *     <cFile> Name of the RTF file to create
 *  $RETURNS$
 *     An  instance of the TRtf Class
 *  $DESCRIPTION$
 *     TRtf() is a class that create the Norton Guide Source
 *     Code of the same name you pass to the constructor.
 *     The class methods are as follows:
 *        New(<cFile>) Create a new instance of the THtml class.
 *        Close() Close the create file
 *        WriteTitle(<cTopic>,<cTitle>) Write the file title
 *        WritePar(<cPar>)   Writes a paragrafer
 *        WriteParBold(<cPar>)   Same as WritePar(), but the text is bold style.
 *        WriteLink(<cLink>)  Write a link to another topic
 *        WriteHeader()  Writes the RTF header
 *        EndPar()       Write the end paragrafer delimeter  
 *  $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     LOCAL oRtf
 *
 *     oRtf := TRtf():New( "rtf\harbour.rtf" )
 *     oRtf:WriteHeader()
 *     oRtf:WriteTitle( "Harbour Reference Guide" )
 *     oRtf:WritePar( "HARBOUR" ):Endpar()
 *     oRtf:WriteLink( "OverView" )
 *     oRtf:WriteLink( "License" )
 *     
 *     oRtf:WritePar( "See the Links Above" ):EndPar()
 *     oRtf:Close()
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
