/*
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 *
 * Thanks to Robert F Greer, PHP original version
 * https://sourceforge.net/projects/excelwriterxml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbclass.ch"
#include "fileio.ch"

CREATE CLASS ExcelWriterXML

   VAR styles               INIT {}
   VAR formatErrors         INIT { => }
   VAR sheets               INIT {}
   VAR lShowErrorSheet      INIT .F.
   VAR overwriteFile        INIT .F.
   VAR cDocTitle
   VAR cDocSubject
   VAR cDocAuthor
   VAR cDocCreated
   VAR cDocManager
   VAR cDocCompany
   VAR cDocVersion          INIT "11.9999"

   VAR cError               INIT ""
   VAR errors               INIT .F.

   METHOD New()
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

METHOD ExcelWriterXML:New()

   LOCAL style := ::addStyle( "DEFAULT" )

   style:name( "Normal" )
   style:alignVertical( "Bottom" )

   ::cDocCreated := DToS( Date() ) + "T" + Time() + "Z"

   RETURN Self

METHOD PROCEDURE ExcelWriterXML:setOverwriteFile( overwrite )

   ::overwriteFile := hb_defaultValue( overwrite, .F. )

   RETURN

METHOD PROCEDURE ExcelWriterXML:showErrorSheet( show )

   ::lShowErrorSheet := hb_defaultValue( show, .T. )

   RETURN

METHOD PROCEDURE ExcelWriterXML:addError( cFunction, cMessage )

   ::formatErrors += { ;
      "FUNCTION" => cFunction, ;
      "MESSAGE"  => cMessage  }

   RETURN

METHOD ExcelWriterXML:getDefaultStyle()
   RETURN ::styles[ 1 ]

METHOD ExcelWriterXML:addStyle( id )

   STATIC s_styleNum := 1

   LOCAL style

   IF ! HB_ISSTRING( id ) .OR. Empty( id )
      id := "CustomStyle" + hb_ntos( s_styleNum )
      s_styleNum++
   ENDIF

   DO WHILE ! ::checkStyleID( id )
      id := "CustomStyle" + hb_ntos( s_styleNum )
      s_styleNum++
   ENDDO

   style := ExcelWriterXML_Style():new( id )
   AAdd( ::styles, style )

   RETURN style

METHOD ExcelWriterXML:addSheet( id )

   STATIC s_sheetNum := 1

   LOCAL sheet

   IF ! HB_ISSTRING( id )
      id := "Sheet" + hb_ntos( s_sheetNum )
      s_sheetNum++
   ENDIF

   DO WHILE ! ::checkSheetID( id )
      id := "Sheet" + hb_ntos( s_sheetNum )
      s_sheetNum++
   ENDDO

   sheet := ExcelWriterXML_Sheet():New( id )
   AAdd( ::sheets, sheet )

   RETURN sheet

METHOD ExcelWriterXML:checkSheetID( id )

   LOCAL sheet

   FOR EACH sheet IN ::sheets
      IF id == sheet:getID()
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

METHOD ExcelWriterXML:checkStyleID( id )

   LOCAL style

   FOR EACH style IN ::styles
      IF id == style:getID()
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

METHOD ExcelWriterXML:writeData( target )

   LOCAL style, sheet, xml := "", handle, format

   IF ! HB_ISSTRING( target ) .OR. Empty( target )
      ::cError := "Target filename missing!"
      ::errors := .T.
      RETURN .T.
   ENDIF

   IF hb_FileExists( target ) .AND. ! ::overwriteFile
      ::cError := target + " exists and overwriteFile is set to false"
      ::errors := .T.
      RETURN .F.
   ENDIF
   IF ( handle := hb_FCreate( target,, FO_EXCLUSIVE ) ) == F_ERROR
      ::cError := "Not able to open " + target + " for writing"
      ::errors := .T.
      RETURN .F.
   ENDIF

   IF ::lShowErrorSheet
      format := ::addStyle( "formatErrorsHeader" )
      format:setFontBold()
      format:bgColor( "red" )
   ENDIF

   xml := ;
      '<?xml version="1.0"?>' + hb_eol() + ;
      '<?mso-application progid="Excel.Sheet"?>' + hb_eol() + ;
      "<Workbook" + hb_eol() + ;
      'xmlns="urn:schemas-microsoft-com:office:spreadsheet"' + hb_eol() + ;
      'xmlns:o="urn:schemas-microsoft-com:office:office"' + hb_eol() + ;
      'xmlns:x="urn:schemas-microsoft-com:office:excel"' + hb_eol() + ;
      'xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"' + hb_eol() + ;
      'xmlns:html="http://www.w3.org/TR/REC-html40">' + hb_eol() + ;
      '<DocumentProperties xmlns="urn:schemas-microsoft-com:office:office">' + hb_eol()

   IF ! Empty( ::cDocTitle   ); xml += "   " + "<Title>"   + StrToHtmlSpecial( ::cDocTitle   ) + "</Title>"   + hb_eol(); ENDIF
   IF ! Empty( ::cDocSubject ); xml += "   " + "<Subject>" + StrToHtmlSpecial( ::cDocSubject ) + "</Subject>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocAuthor  ); xml += "   " + "<Author>"  + StrToHtmlSpecial( ::cDocAuthor  ) + "</Author>"  + hb_eol(); ENDIF
   IF ! Empty( ::cDocCreated ); xml += "   " + "<Created>" + StrToHtmlSpecial( ::cDocCreated ) + "</Created>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocManager ); xml += "   " + "<Manager>" + StrToHtmlSpecial( ::cDocManager ) + "</Manager>" + hb_eol(); ENDIF
   IF ! Empty( ::cDocCompany ); xml += "   " + "<Company>" + StrToHtmlSpecial( ::cDocCompany ) + "</Company>" + hb_eol(); ENDIF

   xml += ;
      "   " + "<Version>" + ::cDocVersion + "</Version>" + hb_eol() + ;
      "</DocumentProperties>" + hb_eol() + ;
      '<ExcelWorkbook xmlns="urn:schemas-microsoft-com:office:excel" />' + hb_eol() + ;
      "<Styles>" + hb_eol()

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
   FOR EACH sheet IN ::sheets
      xml += sheet:getSheetXML( handle )
      IF Len( sheet:getErrors() ) > 0
         ::errors := .T.
      ENDIF
   NEXT
   IF Len( ::formatErrors ) > 0
      ::errors := .T.
   ENDIF

   FWrite( handle, xml + "</Workbook>" )
   FClose( handle )

   RETURN .T.

METHOD PROCEDURE ExcelWriterXML:docTitle( title )

   IF HB_ISSTRING( title )
      ::cDocTitle := title
   ENDIF

   RETURN

METHOD PROCEDURE ExcelWriterXML:docSubject( subject )

   IF HB_ISSTRING( subject )
      ::cDocSubject := subject
   ENDIF

   RETURN

METHOD PROCEDURE ExcelWriterXML:docAuthor( author )

   IF HB_ISSTRING( author )
      ::cDocAuthor := author
   ENDIF

   RETURN

METHOD PROCEDURE ExcelWriterXML:docManager( manager )

   IF HB_ISSTRING( manager )
      ::cDocManager := manager
   ENDIF

   RETURN

METHOD PROCEDURE ExcelWriterXML:docCompany( company )

   IF HB_ISSTRING( company )
      ::cDocCompany := company
   ENDIF

   RETURN

STATIC FUNCTION RemoveAccents( xtxt )
   RETURN hb_StrReplace( xtxt, ;
      hb_UTF8ToStr( ;
      "áâàãçéêíóôõúüÁÂÀÃÇÉÊÍÓÔÕÚÜªº°" ), ;
      "aaaaceeiooouuAAAACEEIOOOUU..." )

FUNCTION StrToHtml( xtxt )
   RETURN hb_StrReplace( xtxt, hb_UTF8ToStr( ;
      "áâàãçéêíóôõúüÁÂÀÃÇÉÊÍÓÔÕÚÜ-" ), { ;
      "&aacute;" , ;
      "&acirc;"  , ;
      "&agrave;" , ;
      "&atilde;" , ;
      "&ccedil;" , ;
      "&eacute;" , ;
      "&ecirc;"  , ;
      "&iacute;" , ;
      "&oacute;" , ;
      "&ocirc;"  , ;
      "&otilde;" , ;
      "&uacute;" , ;
      "&uuml;"   , ;
      "&Aacute;" , ;
      "&Acirc;"  , ;
      "&Agrave;" , ;
      "&Atilde;" , ;
      "&Ccedil;" , ;
      "&Eacute;" , ;
      "&Ecirc;"  , ;
      "&Iacute;" , ;
      "&Oacute;" , ;
      "&Ocirc;"  , ;
      "&Otilde;" , ;
      "&Uacute;" , ;
      "&Uuml;"   , ;
      "&ndash;"  } )

FUNCTION StrToHtmlSpecial( xtxt )
   RETURN hb_StrReplace( RemoveAccents( xtxt ), '"' + "'&<>", { ;
      "&quot;" , ;
      "&#039;" , ;
      "&amp;"  , ;
      "&lt;"   , ;
      "&gt;"   } )
