/*
 * Copyright (C) 1999 Eddie Runia
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

/* 1999-05-30  First implementation.
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
 */

#include "hbclass.ch"

#define CGI_SERVER_SOFTWARE     1
#define CGI_SERVER_NAME         2
#define CGI_GATEWAY_INTERFACE   3
#define CGI_SERVER_PROTOCOL     4
#define CGI_SERVER_PORT         5
#define CGI_REQUEST_METHOD      6
#define CGI_HTTP_ACCEPT         7
#define CGI_HTTP_USER_AGENT     8
#define CGI_HTTP_REFERER        9
#define CGI_PATH_INFO           10
#define CGI_PATH_TRANSLATED     11
#define CGI_SCRIPT_NAME         12
#define CGI_QUERY_STRING        13
#define CGI_REMOTE_HOST         14
#define CGI_REMOTE_ADDR         15
#define CGI_REMOTE_USER         16
#define CGI_AUTH_TYPE           17
#define CGI_CONTENT_TYPE        18
#define CGI_CONTENT_LENGTH      19
#define CGI_ANNOTATION_SERVER   20

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL nPosFim, i

   LOCAL nSize := Len( cString ) - Len( StrTran( cString, cDelim ) ) + 1
   LOCAL aElem := Array( nSize )

   FOR i := 1 TO nSize

      IF ( nPosFim := At( cDelim, cString ) ) > 0
         aElem[ i ] := Left( cString, nPosFim - 1 )
      ELSE
         aElem[ i ] := cString
      ENDIF

      cString := SubStr( cString, nPosFim + 1 )
   NEXT

   RETURN aElem[ nRet ]

CREATE CLASS THTML

   VAR cTitle       INIT "Untitled"      // Page Title
   VAR cBody        INIT ""              // HTML Body Handler
   VAR cBGColor     INIT "#ffffff"       // Background Color
   VAR cLinkColor   INIT "#0000ff"       // Link Color
   VAR cvLinkColor  INIT "#ff0000"       // Visited Link Color
   VAR cContent     INIT ""              // Page Content Handler

   VAR aCGIContents INIT {}
   VAR aQueryFields INIT {}
   VAR aReplaceTags INIT {}
   VAR cHTMLFile    INIT ""

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

ENDCLASS

METHOD New() CLASS THTML
   RETURN Self

METHOD PROCEDURE SetTitle( cTitle ) CLASS THTML

   ::cTitle := cTitle

   RETURN

METHOD PROCEDURE AddLink( cLinkTo, cLinkName ) CLASS THTML

   ::cBody += "<a href='" + cLinkTo + "'>" + cLinkName + "</a>"

   RETURN

METHOD PROCEDURE AddHead( cDescr ) CLASS THTML

   ::cBody += "<h1>" + cDescr + "</h1>"

   RETURN

METHOD PROCEDURE AddPara( cPara, cAlign ) CLASS THTML

   ::cBody += ;
      "<p align='" + cAlign + "'>" + hb_eol() + ;
      cPara + hb_eol() + ;
      "</p>"

   RETURN

METHOD PROCEDURE Generate() CLASS THTML

#if 0
   LOCAL i
   LOCAL lFlag := .F.
   LOCAL cRes
#endif

   // Is this a meta file or hand generated script?
   IF ! HB_ISSTRING( ::cHTMLFile ) .OR. HB_ISNULL( ::cHTMLFile )
      ::cContent := ;
         "<html><head>" + hb_eol() + ;
         "<title>" + ::cTitle + "</title>" + hb_eol() + ;
         "<body link='" + ::cLinkColor + "' " + "vlink='" + ::cvLinkColor + "'>" + hb_eol() + ;
         ::cBody + hb_eol() + ;
         "</body></html>"
   ELSE
      ::cContent := ""

      IF hb_vfExists( ::cHTMLFile )
         ::cContent := hb_StrReplace( hb_MemoRead( ::cHTMLFile ), ::aReplaceTags )

#if 0
         /* TODO: Clear remaining (not matched) tags */
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
      ELSE
         ::cContent := "<h1>Server Error</h1><p><i>No such file: " + ::cHTMLFile + "</i></p>"
      ENDIF
   ENDIF

   RETURN

METHOD PROCEDURE ShowResult() CLASS THTML

   OutStd( ;
      "HTTP/1.0 200 OK" + hb_eol() + ;
      "CONTENT-TYPE: TEXT/HTML" + hb_eol() + hb_eol() + ;
      ::cContent )

   RETURN

METHOD SaveToFile( cFile ) CLASS THTML
   RETURN hb_MemoWrit( cFile, ::cContent )

METHOD PROCEDURE ProcessCGI() CLASS THTML

   LOCAL cQuery
   LOCAL cBuff
   LOCAL nBuff
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

      IF ! Empty( cQuery := ::GetCGIParam( CGI_QUERY_STRING ) )

         ::aQueryFields := {}

         cBuff := ""
         nBuff := 0

         FOR i := 1 TO Len( cQuery ) + 1

            IF i > Len( cQuery ) .OR. SubStr( cQuery, i, 1 ) == "&"

               AAdd( ::aQueryFields, { ;
                  Left( cBuff, At( "=", cBuff ) - 1 ), ;
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

   RETURN

METHOD GetCGIParam( nParam ) CLASS THTML

   ::ProcessCGI()

   IF nParam >= 1 .AND. nParam <= Len( ::aCGIContents )
      RETURN ::aCGIContents[ nParam ]
   ENDIF

   OutErr( "Invalid CGI parameter" )

   RETURN NIL

METHOD QueryFields( cQueryName ) CLASS THTML

   LOCAL nRet

   ::ProcessCGI()

   IF ( nRet := AScan( ::aQueryFields, {| x | Upper( x[ 1 ] ) == Upper( cQueryName ) } ) ) > 0
      RETURN ::aQueryFields[ nRet ][ 2 ]
   ENDIF

   RETURN ""

METHOD PROCEDURE SetHTMLFile( cFile ) CLASS THTML

   ::cHTMLFile := cFile

   RETURN

METHOD PROCEDURE AddReplaceTag( cTag, cReplaceText ) CLASS THTML

   AAdd( ::aReplaceTags, { "<#" + cTag + ">", cReplaceText } )

   RETURN
