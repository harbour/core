/*
 * Harbour Project source code:
 *
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 * www - http://harbour-project.org
 *
 * Thanks to Robert F Greer, PHP original version
 * http://sourceforge.net/projects/excelwriterxml/
 *
 * This program is free software; you can redistribute it AND/OR modify
 * it under the terms of the GNU General PUBLIC License as published by
 * the Free Software Foundation; either version 2, OR( at your option )
 * any later version.
 *
 * This program is distributed IN the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General PUBLIC License FOR more details.
 *
 * You should have received a copy of the GNU General PUBLIC License
 * along WITH this software; see the file COPYING.txt.  IF NOT, write TO
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA( OR visit the web site http://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission FOR
 * additional uses of the text contained IN its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries WITH other
 * files TO produce an executable, this does NOT by itself cause the
 * resulting executable TO be covered by the GNU General PUBLIC License.
 * Your use of that executable is IN no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does NOT however invalidate any other reasons why
 * the executable file might be covered by the GNU General PUBLIC License.
 *
 * This exception applies only TO the code released by the Harbour
 * Project under the name Harbour.  IF you copy code FROM other
 * Harbour Project OR Free Software Foundation releases into a copy of
 * Harbour, as the General PUBLIC License permits, the exception does
 * NOT apply TO the code that you add IN this way.  TO avoid misleading
 * anyone as TO the status of such modified files, you must delete
 * this exception notice FROM them.
 *
 * IF you write modifications of your own FOR Harbour, it is your choice
 * whether TO permit this exception TO apply TO your modifications.
 * IF you DO NOT wish that, delete this exception notice.
 *
 */

/* UTF-8 */

#include "hbclass.ch"
#include "fileio.ch"

CREATE CLASS ExcelWriterXML

   VAR    styles                                  INIT {}
   VAR    formatErrors                            INIT { => }
   VAR    sheets                                  INIT {}
   VAR    lShowErrorSheet                         INIT .F.
   VAR    overwriteFile                           INIT .F.
   VAR    docFileName
   VAR    cDocTitle
   VAR    cDocSubject
   VAR    cDocAuthor
   VAR    cDocCreated
   VAR    cDocManager
   VAR    cDocCompany
   VAR    cDocVersion                             INIT "11.9999"

   VAR    cError                                  INIT ""
   VAR    errors                                  INIT .F.

   METHOD New( fileName )
   METHOD setOverwriteFile( overwrite )
   METHOD showErrorSheet( show )
   METHOD addError( cFunction, cMessage )
   METHOD getDefaultStyle()
   METHOD addStyle( id )
   METHOD addSheet( id )
   METHOD checkSheetID( id )
   METHOD checkStyleID( id )
   METHOD writeData( target )
   METHOD docTitle( title )
   METHOD docSubject( subject )
   METHOD docAuthor( author )
   METHOD docManager( manager )
   METHOD docCompany( company )

ENDCLASS

METHOD ExcelWriterXML:new( fileName )

   LOCAL style

   style := ::addStyle( "DEFAULT" )
   style:name( "Normal" )
   style:alignVertical( "Bottom" )

   IF Empty( fileName )
      fileName := "file.xml"
   ENDIF

   ::docFileName := fileName
   ::cDocCreated := DToS( Date() ) + "T" + Time() + "Z"

   RETURN SELF

METHOD ExcelWriterXML:setOverwriteFile( overwrite )

   hb_default( @overwrite, .F. )

   ::overwriteFile := overwrite

   RETURN NIL

METHOD ExcelWriterXML:showErrorSheet( show )

   hb_default( @show, .T. )

   ::lShowErrorSheet := show

   RETURN NIL

METHOD ExcelWriterXML:addError( cFunction, cMessage )

   ::formatErrors += { ;
      "FUNCTION" => cFunction, ;
      "MESSAGE"  => cMessage  }

   RETURN NIL

METHOD ExcelWriterXML:getDefaultStyle()

   RETURN ::styles[ 1 ]

METHOD ExcelWriterXML:addStyle( id )

   LOCAL style

   STATIC s_styleNum := 1

   IF Empty( id )
      id := NIL
   ENDIF

   IF id == NIL
      id := "CustomStyle" + hb_ntos( s_styleNum )
      s_styleNum++
   ENDIF

   WHILE ! ::checkStyleID( id )
      id := "CustomStyle" + hb_ntos( s_styleNum )
      s_styleNum++
   ENDDO

   style := ExcelWriterXML_Style():new( id )
   AAdd( ::styles, style )

   RETURN style

METHOD ExcelWriterXML:addSheet( id )

   LOCAL sheet

   STATIC s_sheetNum := 1

   IF id == NIL
      id := "Sheet" + hb_ntos( s_sheetNum )
      s_sheetNum++
   ENDIF

   WHILE ! ::checkSheetID( id )
      id := "Sheet" + hb_ntos( s_sheetNum )
      s_sheetNum++
   ENDDO

   sheet := ExcelWriterXML_Sheet():New( id )
   AAdd( ::sheets, sheet )

   RETURN sheet

METHOD ExcelWriterXML:checkSheetID( id )

   LOCAL sheet

   IF Len( ::sheets ) > 0
      FOR EACH sheet IN ::sheets
         IF id == sheet:getID()
            RETURN .F.
         ENDIF
      NEXT
   ELSE
      RETURN .T.
   ENDIF

   RETURN .T.

METHOD ExcelWriterXML:checkStyleID( id )

   LOCAL style

   IF Len( ::styles ) > 0
      FOR EACH style IN ::styles
         IF id == style:getID()
            RETURN .F.
         ENDIF
      NEXT
   ELSE
      RETURN .T.
   ENDIF

   RETURN .T.

METHOD ExcelWriterXML:writeData( target )

   LOCAL style, sheet, xml := "", handle, fileExists, format

   LOCAL docTitle   := ""
   LOCAL docSubject := ""
   LOCAL docAuthor  := ""
   LOCAL docCreated := ""
   LOCAL docManager := ""
   LOCAL docCompany := ""

   IF target == NIL
      ::cError := "Target filename missing!"
      ::errors := .T.
      RETURN .T.
   ENDIF

   fileExists := hb_FileExists( target )
   IF fileExists .AND. ! ::overwriteFile
      ::cError := target + " exists and overwriteFile is set to false"
      ::errors := .T.
      RETURN .F.
   ENDIF
   handle := hb_FCreate( target, FC_NORMAL, FO_EXCLUSIVE )
   IF handle == F_ERROR
      ::cError := "Not able to open " + target + " for writing"
      ::errors := .T.
      RETURN .F.
   ENDIF

   IF ::lShowErrorSheet
      format := ::addStyle( "formatErrorsHeader" )
      format:setFontBold()
      format:bgColor( "red" )
   ENDIF

   IF ! Empty( ::cDocTitle   ); docTitle   := "<Title>"   + StrToHtmlSpecial( ::cDocTitle   ) + "</Title>"   + hb_eol(); ENDIF
   IF ! Empty( ::cDocSubject ); docSubject := "<Subject>" + StrToHtmlSpecial( ::cDocSubject ) + "</Subject>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocAuthor  ); docAuthor  := "<Author>"  + StrToHtmlSpecial( ::cDocAuthor  ) + "</Author>"  + hb_eol(); ENDIF
   IF ! Empty( ::cDocCreated ); docCreated := "<Created>" + StrToHtmlSpecial( ::cDocCreated ) + "</Created>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocManager ); docManager := "<Manager>" + StrToHtmlSpecial( ::cDocManager ) + "</Manager>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocCompany ); docCompany := "<Company>" + StrToHtmlSpecial( ::cDocCompany ) + "</Company>" + hb_eol(); ENDIF

   xml := '<?xml version="1.0"?>' + hb_eol()
   xml += '<?mso-application progid="Excel.Sheet"?>' + hb_eol()
   xml += "<Workbook" + hb_eol()
   xml += 'xmlns="urn:schemas-microsoft-com:office:spreadsheet"' + hb_eol()
   xml += 'xmlns:o="urn:schemas-microsoft-com:office:office"' + hb_eol()
   xml += 'xmlns:x="urn:schemas-microsoft-com:office:excel"' + hb_eol()
   xml += 'xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' + hb_eol()
   xml += 'xmlns:html="http://www.w3.org/TR/REC-html40">' + hb_eol()
   xml += '<DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' + hb_eol()
   IF ! Empty( ::cDocTitle   ); xml += "   " + docTitle  ; ENDIF
   IF ! Empty( ::cDocSubject ); xml += "   " + docSubject; ENDIF
   IF ! Empty( ::cDocAuthor  ); xml += "   " + docAuthor ; ENDIF
   IF ! Empty( ::cDocCreated ); xml += "   " + docCreated; ENDIF
   IF ! Empty( ::cDocManager ); xml += "   " + docManager; ENDIF
   IF ! Empty( ::cDocCompany ); xml += "   " + docCompany; ENDIF
   xml += "   <Version>" + ::cDocVersion + "</Version>" + hb_eol()
   xml += "</DocumentProperties>" + hb_eol()
   xml += '<ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel" />' + hb_eol()
   xml += "<Styles>" + hb_eol()

   FWrite( handle, xml )
   xml := ""

   FOR EACH style IN ::styles
      xml += style:getStyleXML()
   NEXT
   xml += "</Styles>" + hb_eol()

   FWrite( handle, xml )
   xml := ""

   IF Len( ::sheets ) == 0
      ::addSheet()
   ENDIF
   IF Len( ::sheets ) > 0
      FOR EACH sheet IN ::sheets
         xml += sheet:getSheetXML( handle )
         IF Len( sheet:getErrors() ) > 0
            ::errors := .T.
         ENDIF
      NEXT
   ENDIF
   IF Len( ::formatErrors ) > 0
      ::errors := .T.
   ENDIF

   xml += "</Workbook>"

   FWrite( handle, xml )
   xml := ""
   FClose( handle )

   RETURN .T.

METHOD ExcelWriterXML:docTitle( title )

   IF HB_ISSTRING( title )
      ::cDocTitle := title
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML:docSubject( subject )

   IF HB_ISSTRING( subject )
      ::cDocSubject := subject
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML:docAuthor( author )

   IF HB_ISSTRING( author )
      ::cDocAuthor := author
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML:docManager( manager )

   IF HB_ISSTRING( manager )
      ::cDocManager := manager
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML:docCompany( company )

   IF HB_ISSTRING( company )
      ::cDocCompany := company
   ENDIF

   RETURN NIL

FUNCTION StrToHtml( xtxt )

   LOCAL afrm, i, xret := "", xpos

   afrm := { ;
      { "á", "&aacute;" }, ;
      { "â", "&acirc;"  }, ;
      { "à", "&agrave;" }, ;
      { "ã", "&atilde;" }, ;
      { "ç", "&ccedil;" }, ;
      { "é", "&eacute;" }, ;
      { "ê", "&ecirc;"  }, ;
      { "í", "&iacute;" }, ;
      { "ó", "&oacute;" }, ;
      { "ô", "&ocirc;"  }, ;
      { "õ", "&otilde;" }, ;
      { "ú", "&uacute;" }, ;
      { "ü", "&uuml;"   }, ;
      { "Á", "&Aacute;" }, ;
      { "Â", "&Acirc;"  }, ;
      { "À", "&Agrave;" }, ;
      { "Ã", "&Atilde;" }, ;
      { "Ç", "&Ccedil;" }, ;
      { "É", "&Eacute;" }, ;
      { "Ê", "&Ecirc;"  }, ;
      { "Í", "&Iacute;" }, ;
      { "Ó", "&Oacute;" }, ;
      { "Ô", "&Ocirc;"  }, ;
      { "Õ", "&Otilde;" }, ;
      { "Ú", "&Uacute;" }, ;
      { "Ü", "&Uuml;"   }, ;
      { "-", "&ndash;"  } }

   FOR i := 1 TO Len( xtxt )
      IF ( xpos := AScan( afrm, {| x | SubStr( xtxt, i, 1 ) == hb_UTF8ToStr( x[ 1 ] ) } ) ) > 0
         xret += afrm[ xpos, 2 ]
      ELSE
         xret += SubStr( xtxt, i, 1 )
      ENDIF
   NEXT

   RETURN xret

FUNCTION StrToHtmlSpecial( xtxt )

   LOCAL afrm, i, xret := "", xpos

   xtxt := RemoveAccents( xtxt )
   afrm := { ;
      { "&", "&amp;"  }, ;
      { '"', "&quot;" }, ;
      { "'", "&#039;" }, ;
      { "<", "&lt;"   }, ;
      { ">", "&gt;"   } }

   FOR i := 1 TO Len( xtxt )
      IF ( xpos := AScan( afrm, {| x | SubStr( xtxt, i, 1 ) == x[ 1 ] } ) ) > 0
         xret += afrm[ xpos, 2 ]
      ELSE
         xret += SubStr( xtxt, i, 1 )
      ENDIF
   NEXT

   RETURN xret

STATIC FUNCTION RemoveAccents( xtxt )

   LOCAL afrm, i, xret := "", xpos

   afrm := { ;
      { "á", "a" }, ;
      { "â", "a" }, ;
      { "à", "a" }, ;
      { "ã", "a" }, ;
      { "ç", "c" }, ;
      { "é", "e" }, ;
      { "ê", "e" }, ;
      { "í", "i" }, ;
      { "ó", "o" }, ;
      { "ô", "o" }, ;
      { "õ", "o" }, ;
      { "ú", "u" }, ;
      { "ü", "u" }, ;
      { "Á", "A" }, ;
      { "Â", "A" }, ;
      { "À", "A" }, ;
      { "Ã", "A" }, ;
      { "Ç", "C" }, ;
      { "É", "E" }, ;
      { "Ê", "E" }, ;
      { "Í", "I" }, ;
      { "Ó", "O" }, ;
      { "Ô", "O" }, ;
      { "Õ", "O" }, ;
      { "Ú", "U" }, ;
      { "Ü", "U" }, ;
      { "ª", "." }, ;
      { "º", "." }, ;
      { "°", "." } }

   FOR i := 1 TO Len( xtxt )
      IF ( xpos := AScan( afrm, {| x | SubStr( xtxt, i, 1 ) == hb_UTF8ToStr( x[ 1 ] ) } ) ) > 0
         xret += afrm[ xpos, 2 ]
      ELSE
         xret += SubStr( xtxt, i, 1 )
      ENDIF
   NEXT

   RETURN xret
