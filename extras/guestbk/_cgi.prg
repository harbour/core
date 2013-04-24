/*
 * Harbour Project source code
 *
 * Copyright (C) 1999 Eddie Runia
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
 *
 */

/*
 *
 * Harbour Test of a CGI/HTML-Generator class.
 *
 * 1999-05-30  First implementation.
 *
 *             Tips: - Use ShowResults to make dynamic html (to test dynamic
 *                     results, put the exe file on CGI-BIN dir or equivalent);
 *                   - Use SaveToFile to make static html page
 *
 * 1999-05-31  Initial CGI functionality.
 * 1999-06-01  Translated %nn to correct chars.
 * 1999-06-02  Dynamic TAG matching routines (inspired on Delphi).
 *             First attempt to convert Delphi's ISAPI dll of WebSites'
 *             Function List
 * 1999-07-29  Changed QOut() calls to OutStd() calls.
 *
 */

#include "fileio.ch"
#include "hbclass.ch"

#include "cgi.ch"

#define IF_BUFFER 65535

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf, aElem, nPosFim, nSize, i

   nSize := Len( cString ) - Len( StrTran( cString, cDelim ) ) + 1
   aElem := Array( nSize )

   cBuf := cString

   FOR i := 1 TO nSize
      nPosFim := At( cDelim, cBuf )

      IF nPosFim > 0
         aElem[ i ] := SubStr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[ i ] := cBuf
      ENDIF

      cBuf := SubStr( cBuf, nPosFim + 1, Len( cBuf ) )

   NEXT

   RETURN aElem[ nRet ]

CREATE CLASS THTML

   VAR cTitle                          // Page Title
   VAR cBody                           // HTML Body Handler
   VAR cBGColor                        // Background Color
   VAR cLinkColor                      // Link Color
   VAR cvLinkColor                     // Visited Link Color
   VAR cContent                        // Page Content Handler

   VAR aCGIContents
   VAR aQueryFields
   VAR cHTMLFile
   VAR aReplaceTags

   METHOD New()
   METHOD SetTitle( cTitle )
   METHOD AddLink( cLinkTo, cLinkName )
   METHOD AddHead( cDescr )
   METHOD AddPara( cPara, cAlign )
   METHOD Generate()
   METHOD ShowResult()
   METHOD SaveToFile( cFile )
   METHOD ProcessCGI()
   METHOD GetCGIParam( nParam )
   METHOD QueryFields( cQueryName )
   METHOD SetHTMLFile( cFile )
   METHOD AddReplaceTag( cTag, cReplaceText )

END CLASS

METHOD New() CLASS THTML

   ::cTitle       := "Untitled"
   ::cBGColor     := "#FFFFFF"
   ::cLinkColor   := "#0000FF"
   ::cvLinkColor  := "#FF0000"
   ::cContent     := ""
   ::cBody        := ""
   ::aCGIContents := {}
   ::aQueryFields := {}
   ::aReplaceTags := {}
   ::cHTMLFile    := ""

   RETURN Self

METHOD SetTitle( cTitle ) CLASS THTML

   ::cTitle := cTitle

   RETURN Self

METHOD AddLink( cLinkTo, cLinkName ) CLASS THTML

   ::cBody := ::cBody + ;
      "<a href='" + cLinkTo + "'>" + cLinkName + "</a>"

   RETURN Self

METHOD AddHead( cDescr ) CLASS THTML

   ::cBody += "<h1>" + cDescr + "</h1>"

   RETURN NIL

METHOD AddPara( cPara, cAlign ) CLASS THTML

   ::cBody := ::cBody + ;
      "<p align='" + cAlign + "'>" + hb_eol() + ;
      cPara + hb_eol() + ;
      "</p>"

   RETURN Self

METHOD Generate() CLASS THTML

   LOCAL cFile, i, hFile, nPos, cRes := ""
#if 0
   LOCAL lFlag := .F.
#endif

   // Is this a meta file or hand generated script?
   IF Empty( ::cHTMLFile )
      ::cContent :=                                                         ;
         "<html><head>"                                        + hb_eol() + ;
         "<title>" + ::cTitle + "</title>"                     + hb_eol() + ;
         "<body link='" + ::cLinkColor + "' " +                             ;
         "vlink='" + ::cvLinkColor + "'>" +                    + hb_eol() + ;
         ::cBody                                               + hb_eol() + ;
         "</body></html>"
   ELSE
      ::cContent := ""

      // Does cHTMLFile exists?
      IF ! hb_FileExists( ::cHTMLFile )
         ::cContent := "<h1>Server Error</h1><p><i>No such file: " + ;
            ::cHTMLFile
      ELSE
         // Read from file
         hFile := FOpen( ::cHTMLFile, FO_READ )
         cFile := Space( IF_BUFFER )
         DO WHILE ( nPos := FRead( hFile, @cFile, IF_BUFFER ) ) > 0

            cFile := hb_BLeft( cFile, nPos )
            cRes += cFile
            cFile := Space( IF_BUFFER )

         ENDDO

         FClose( hFile )

         // Replace matched tags
         i := 1
         ::cContent := cRes
         /* TODO: Replace this DO WHILE with FOR..NEXT */
         DO WHILE i <= Len( ::aReplaceTags )
            ::cContent := StrTran( ::cContent, ;
               "<#" + ::aReplaceTags[ i, 1 ] + ">", ::aReplaceTags[ i, 2 ] )
            i++
         ENDDO

         /* TODO: Clear remaining (not matched) tags */
#if 0
         cRes := ""
         FOR i := 1 TO Len( ::cContent )
            IF SubStr( ::cContent, i, 1 ) == "<" .AND. ;
               SubStr( ::cContent, i + 1, 1 ) == "#"
               lFlag := .T.
            ELSEIF SubStr( ::cContent, i, 1 ) == ">" .AND. lFlag
               lFlag := .F.
            ELSEIF ! lFlag
               cRes += SubStr( ::cContent, i, 1 )
            ENDIF
         NEXT

         ::cContent := cRes
#endif

      ENDIF
   ENDIF

   RETURN Self

METHOD ShowResult() CLASS THTML

   OutStd(                                                                   ;
      "HTTP/1.0 200 OK"                                         + hb_eol() + ;
      "CONTENT-TYPE: TEXT/HTML"                      + hb_eol() + hb_eol() + ;
      ::cContent )

   RETURN Self

METHOD SaveToFile( cFile ) CLASS THTML

   LOCAL hFile := FCreate( cFile )

   FWrite( hFile, ::cContent )
   FClose( hFile )

   RETURN Self

METHOD ProcessCGI() CLASS THTML

   LOCAL cQuery
   LOCAL cBuff := ""
   LOCAL nBuff := 0
   LOCAL i

   IF Empty( ::aCGIContents )
      ::aCGIContents := { ;
         GetEnv( "SERVER_SOFTWARE"   ), ;
         GetEnv( "SERVER_NAME"       ), ;
         GetEnv( "GATEWAY_INTERFACE" ), ;
         GetEnv( "SERVER_PROTOCOL"   ), ;
         GetEnv( "SERVER_PORT"       ), ;
         GetEnv( "REQUEST_METHOD"    ), ;
         GetEnv( "HTTP_ACCEPT"       ), ;
         GetEnv( "HTTP_USER_AGENT"   ), ;
         GetEnv( "HTTP_REFERER"      ), ;
         GetEnv( "PATH_INFO"         ), ;
         GetEnv( "PATH_TRANSLATED"   ), ;
         GetEnv( "SCRIPT_NAME"       ), ;
         GetEnv( "QUERY_STRING"      ), ;
         GetEnv( "REMOTE_HOST"       ), ;
         GetEnv( "REMOTE_ADDR"       ), ;
         GetEnv( "REMOTE_USER"       ), ;
         GetEnv( "AUTH_TYPE"         ), ;
         GetEnv( "CONTENT_TYPE"      ), ;
         GetEnv( "CONTENT_LENGTH"    ), ;
         GetEnv( "ANNOTATION_SERVER" ) }

      cQuery := ::GetCGIParam( CGI_QUERY_STRING )

      IF ! Empty( cQuery )

         ::aQueryFields := {}

         FOR i := 1 TO Len( cQuery ) + 1

            IF i > Len( cQuery ) .OR. SubStr( cQuery, i, 1 ) == "&"

               AAdd( ::aQueryFields, { ;
                  SubStr( cBuff, 1, At( "=", cBuff ) - 1 ), ;
                  StrTran( SubStr( cBuff, At( "=", cBuff ) + 1, ;
                  Len( cBuff ) - At( "=", cBuff ) + 1 ), "+", " " ) } )
               cBuff := ""
            ELSE
               IF SubStr( cQuery, i, 1 ) == "%"
                  cBuff += Chr( hb_HexToNum( SubStr( cQuery, i + 1, 2 ) ) )
                  nBuff := 3
               ENDIF

               IF nBuff == 0
                  cBuff += SubStr( cQuery, i, 1 )
               ELSE
                  nBuff--
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Self

METHOD GetCGIParam( nParam ) CLASS THTML

   ::ProcessCGI()

   IF nParam > 20 .OR. nParam < 1
      OutErr( "Invalid CGI parameter" )
      RETURN NIL
   ENDIF

   RETURN ::aCGIContents[ nParam ]

METHOD QueryFields( cQueryName ) CLASS THTML

   LOCAL cRet := ""
   LOCAL nRet

   ::ProcessCGI()

   nRet := AScan( ::aQueryFields, ;
      {| x | Upper( x[ 1 ] ) == Upper( cQueryName ) } )

   IF nRet > 0
      cRet := ::aQueryFields[ nRet, 2 ]
   ENDIF

   RETURN cRet

METHOD SetHTMLFile( cFile ) CLASS THTML

   ::cHTMLFile := cFile

   RETURN Self

METHOD AddReplaceTag( cTag, cReplaceText ) CLASS THTML

   AAdd( ::aReplaceTags, { cTag, cReplaceText } )

   RETURN Self
