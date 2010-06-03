/*
 * $Id$
 */

/*
 * Harbour Project source code
 *
 * This file contains source for a script of a Guestbook
 *
 * Copyright (C) 1999 Felipe G. Coury <fcoury@creation.com.br>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

*/

FUNCTION Main()

   LOCAL oIni  := TIniFile():New( "C:\inetpub\wwwroot\guestbk.ini" )
   LOCAL oHTML := THTML():New()
   LOCAL aEntr := {}
   LOCAL cOddColor, cEvenColor
   LOCAL cCode, i, j, l, cField, nEntry, cColor
   LOCAL aEntries, aLine, cLine

   oHTML:ProcessCGI()

   IF oHTML:QueryFields( "Oper" ) == "A"  // Add Entry

      nEntry := oIni:ReadNumber( "Entries", "Entries", 0 ) + 1
      oIni:WriteNumber( "Entries", "Entries", nEntry )

      // Reads all "Header" fields from CGI
      FOR i := 1 TO oIni:ReadNumber( "Header", "DataFields", 0 )

         cField := oIni:ReadString( "Header", "DataField"+ltrim(str(i)), "" )
         oIni:WriteString( "Entries", cField + ltrim(str(nEntry)), ;
           strtran( strtran( oHTML:QueryFields( cField ), chr(10), "" ), chr(13), "<BR>" ) )

      NEXT

      // Write fields to .ini file
      oIni:WriteString( "Entries", "DateTime" + ltrim(str(nEntry)), ;
        cmonth( date() ) + " " + ltrim(str(day(date()))) + ", " + ;
        ltrim(str(year(date()))) + " " + time() )

      oIni:UpdateFile()

      oHTML:cContent := '<HTML><HEAD><META HTTP-EQUIV="Refresh" ' + ;
         'CONTENT="0;URL=/cgi-bin/guestbk.exe"></HEAD>'  + ;
         '<BODY></BODY></HTML>'

      oHTML:ShowResult()

   ELSE

      // Sets the metahtml file
      oHTML:SetHTMLFile( "C:\inetpub\wwwroot\guestbk.htm" )

      // Retrieves odd and even entries color
      cOddColor := oIni:ReadString( "Header", "OddColor", "#FFFFFF" )
      cEvenColor := oIni:ReadString( "Header", "EvenColor", "#F0F0F0" )

      cCode := ""
      i := oIni:ReadNumber( "Entries", "Entries", 0 )

      aEntries := {}

      // Preprocess entries and stores in aEntries
      DO WHILE i > 0


         aLine := {}

         FOR j := 1 TO oIni:ReadNumber( "Header", "DataFields", 0 )

            cField := oIni:ReadString( "Header", "DataField" + ltrim( str(j) ), "" )
            aadd( aLine, { cField, ;
              oIni:ReadString( "Entries", cField + ltrim( str(i) ), "" ) } )

         NEXT

         aadd( aEntries, aLine )

         i--

      ENDDO

      cCode := ""

      // Formats each line according to the INI file
      FOR i := 1 TO len( aEntries )

         cCode += "<TABLE WIDTH=100% CELLSPACING=0>" + chr(13) + chr(10)
         cColor := iif( Mod( i, 2 ) == 0, cEvenColor, cOddColor )

         FOR j := 1 TO oIni:ReadNumber( "Format", "FormatLines", 0 )

            cCode += "<TR><TD BGCOLOR='" + cColor +"'>"

            cLine := oIni:ReadString( "Format", "Format" + ltrim(str(j)), "" )
            FOR l := 1 TO len( aEntries[i] )
               cLine := strtran( cLine, "<#" + aEntries[i,l,1] + ">", ;
                 aEntries[i,l,2] )
            NEXT

            cLine := strtran( cLine, "<#DateTime>", ;
              oIni:ReadString( "Entries", "DateTime" + ltrim(str(len(aEntries)-i+1)), "" ) )

            cCode += cLine + "</TD></TR>" + chr(13)+chr(10)

         NEXT

         cCode += "</TABLE>" + chr(13)+chr(10)

      NEXT

      // Generates the output
      oHTML:AddReplaceTag( "Entries", cCode )
      oHTML:Generate()
      oHTML:ShowResult()

   ENDIF

   RETURN NIL
