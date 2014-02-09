/*
 * xHarbour Project source code:
 * TIPCgi Class oriented cgi protocol
 *
 * Copyright 2006 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
 *
 * code from:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://harbour-project.org
 *
 *    CGI Session Manager Class
 *
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://www.xharbour.org
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

#define _CRLF Chr( 13 ) + Chr( 10 )

CREATE CLASS TIPCgi

   VAR HTTP_RAW_POST_DATA

   VAR cCgiHeader
   VAR cHtmlPage
   VAR hGets        INIT { => }
   VAR hPosts       INIT { => }
   VAR hCookies     INIT { => }
   VAR hSession     INIT { => }
   VAR bSavedErrHandler
   VAR cSessionSavePath
   VAR cSID
   VAR cDumpSavePath
   VAR lDumpHtml    INIT .F.
   VAR Cargo

   METHOD New()
   METHOD Header( cValue )
   METHOD Redirect( cUrl )
   METHOD Write( cString )
   METHOD Flush()
   METHOD ErrHandler( xError )

   METHOD StartHtml( hOptions )
   METHOD EndHtml()
   METHOD SaveHtmlPage( cFile )

   METHOD StartSession( cSID )
   METHOD DestroySession( cID )
   METHOD CreateSID( cCRCKey ) INLINE ::cSID := tip_GenerateSID( cCrcKey )
   METHOD CheckCrcSID( cSID, cCRCKey ) INLINE tip_CheckSID( cSID, cCRCKey )
   METHOD SessionEncode()
   METHOD SessionDecode( cData )

ENDCLASS

METHOD New() CLASS TIPCgi

   LOCAL aVar
   LOCAL nLen
   LOCAL nRead
   LOCAL cTemp
   LOCAL item

   ::bSavedErrHandler := ErrorBlock( {| e | ::ErrHandler( e ) } )

   ::cCgiHeader := ""
   ::cHtmlPage := ""

   IF "POST" $ Upper( GetEnv( "REQUEST_METHOD" ) )
      nLen := Val( GetEnv( "CONTENT_LENGTH" ) )
      cTemp := Space( nLen )
      IF ( nRead := FRead( hb_GetStdIn(), @cTemp, nLen ) ) != nLen
         ::ErrHandler( "post error read " + hb_ntos( nRead ) + " instead of " + hb_ntos( nLen ) )
      ELSE
         ::HTTP_RAW_POST_DATA := cTemp
         FOR EACH item IN hb_ATokens( cTemp, "&" )
            IF Len( aVar := hb_ATokens( item, "=" ) ) == 2
               ::hPosts[ AllTrim( tip_URLDecode( aVar[ 1 ] ) ) ] := tip_URLDecode( aVar[ 2 ] )
            ENDIF
         NEXT
      ENDIF
   ELSEIF ! Empty( cTemp := GetEnv( "QUERY_STRING" ) )
      FOR EACH item IN hb_ATokens( cTemp, "&" )
         IF Len( aVar := hb_ATokens( item, "=" ) ) == 2
            ::hGets[ AllTrim( tip_URLDecode( aVar[ 1 ] ) ) ] := tip_URLDecode( aVar[ 2 ] )
         ENDIF
      NEXT
   ENDIF

   IF ! Empty( cTemp := GetEnv( "HTTP_COOKIE" ) )
      FOR EACH item IN hb_ATokens( cTemp, ";" )
         IF Len( aVar := hb_ATokens( item, "=" ) ) == 2
            ::hCookies[ AllTrim( tip_URLDecode( aVar[ 1 ] ) ) ] := tip_URLDecode( aVar[ 2 ] )
         ENDIF
      NEXT
   ENDIF

   RETURN Self

METHOD Header( cValue ) CLASS TIPCgi

   IF HB_ISSTRING( cValue ) .AND. ! Empty( cValue )
      ::cCgiHeader += cValue + _CRLF
   ELSE
      ::cCgiHeader += "Content-Type: text/html" + _CRLF
   ENDIF

   RETURN Self

METHOD Redirect( cUrl ) CLASS TIPCgi

   IF HB_ISSTRING( cUrl ) .AND. ! Empty( cUrl )
      ::cCgiHeader += "Location: " + cUrl + _CRLF
   ENDIF

   RETURN Self

METHOD Flush() CLASS TIPCgi

   LOCAL cStream
   LOCAL lRet

   LOCAL nH
   LOCAL cFile

   LOCAL cSession

   hb_HEval( ::hCookies, {| k, v | ::cCgiHeader += "Set-Cookie: " + k + "=" + v + ";" + _CRLF } )

   cStream := ::cCgiHeader + _CRLF + ::cHtmlPage + _CRLF

   lRet := ( FWrite( hb_GetStdOut(), cStream ) == hb_BLen( cStream ) )

   IF ::lDumpHtml
      IF Empty( ::cDumpSavePath )
         ::cDumpSavePath := hb_DirTemp()
      ENDIF
      hb_MemoWrit( ::cDumpSavePath + "dump.html", ::cHtmlPage )
   ENDIF

   ::cCgiHeader := ""
   ::cHtmlPage := ""

   IF ! Empty( ::cSID )

      cFile := ::cSessionSavePath + "SESSIONID_" + ::cSID

      IF ( nH := FCreate( cFile, FC_NORMAL ) ) != F_ERROR
         cSession := ::SessionEncode()
         IF FWrite( nH, cSession ) != hb_BLen( cSession )
            ::Write( "ERROR: On writing session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
         ENDIF
         FClose( nH )
      ELSE
         ::Write( "ERROR: On writing session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
      ENDIF
   ENDIF

   RETURN lRet

METHOD SaveHtmlPage( cFile ) CLASS TIPCgi
   RETURN hb_MemoWrit( cFile, ::cHtmlPage + _CRLF )

METHOD StartSession( cSID ) CLASS TIPCgi

   LOCAL nH
   LOCAL cFile
   LOCAL nFileSize
   LOCAL cBuffer

   IF ! HB_ISSTRING( cSID ) .OR. Empty( cSID )
      DO CASE
      CASE hb_HGetRef( ::hGets, "SESSIONID", @cSID )
      CASE hb_HGetRef( ::hPosts, "SESSIONID", @cSID )
      CASE hb_HGetRef( ::hCookies, "SESSIONID", @cSID )
      ENDCASE
   ENDIF

   IF Empty( ::cSessionSavePath )
      ::cSessionSavePath := hb_DirTemp()
   ENDIF

   IF ! Empty( cSID )

      ::cSID := cSID

      cFile := ::cSessionSavePath + "SESSIONID_" + cSID

      IF hb_FileExists( cFile )
         IF ( nH := FOpen( cFile, FO_READ ) ) != F_ERROR
            nFileSize := FSeek( nH, 0, FS_END )
            FSeek( nH, 0, FS_SET )
            cBuffer := Space( nFileSize )
            IF FRead( nH, @cBuffer, nFileSize ) != nFileSize
               ::ErrHandler( "ERROR: On reading session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
            ELSE
               ::SessionDecode( cBuffer )
            ENDIF
            FClose( nH )
         ENDIF
      ELSE
         ::ErrHandler( "ERROR: On opening session file: " + cFile + ", file not exist." )
      ENDIF
   ELSE
      ::CreateSID()
      ::hSession := { => }
   ENDIF

   ::hCookies[ "SESSIONID" ] := ::cSID

   RETURN Self

METHOD SessionEncode() CLASS TIPCgi
   RETURN hb_Serialize( ::hSession )

METHOD SessionDecode( cData ) CLASS TIPCgi
   RETURN HB_ISHASH( ::hSession := hb_Deserialize( cData ) )

METHOD DestroySession( cID ) CLASS TIPCgi

   LOCAL cFile
   LOCAL cSID
   LOCAL lRet

   IF HB_ISSTRING( cID ) .AND. ! Empty( cID )
      cSID := cID
   ELSE
      cSID := ::cSID
   ENDIF

   IF ! Empty( cSID )

      ::hSession := { => }

      cFile := ::cSessionSavePath + "SESSIONID_" + cSID

      IF ( lRet := ( FErase( cFile ) != F_ERROR ) )
         ::hCookies[ "SESSIONID" ] := cSID + "; expires= " + tip_DateToGMT( Date() - 1 )
         ::CreateSID()
         ::hCookies[ "SESSIONID" ] := ::cSID
      ELSE
         ::Write( "ERROR: On deleting session file: " + cFile + ", File error: " + hb_CStr( FError() ) )
      ENDIF
   ENDIF

   RETURN lRet

METHOD ErrHandler( xError ) CLASS TIPCgi

   LOCAL nCalls

   LOCAL cErrMsg := ;
      '<table border="1">' + ;
      "<tr><td>SCRIPT NAME:</td><td>" + GetEnv( "SCRIPT_NAME" ) + "</td></tr>"

   DO CASE
   CASE HB_ISOBJECT( xError )
      cErrMsg += ;
         "<tr><td>CRITICAL ERROR:</td><td>" + xError:Description + "</td></tr>" + ;
         "<tr><td>OPERATION:</td><td>" + xError:Operation + "</td></tr>" + ;
         "<tr><td>OS ERROR:</td><td>" + hb_ntos( xError:OsCode ) + " IN " + xError:SubSystem + "/" + hb_ntos( xError:SubCode ) + "</td></tr>" + ;
         "<tr><td>FILENAME:</td><td>" + Right( xError:FileName, 40 ) + "</td></tr>"
   CASE HB_ISSTRING( xError )
      cErrMsg += "<tr><td>ERROR MESSAGE:</td><td>" + tip_HtmlSpecialChars( xError ) + "</td></tr>"
   ENDCASE

   nCalls := 0
   DO WHILE ! Empty( ProcName( ++nCalls ) )
      cErrMsg += "<tr><td>PROC/LINE:</td><td>" + ProcName( nCalls ) + "/" + hb_ntos( ProcLine( nCalls ) ) + "</td></tr>"
   ENDDO

   cErrMsg += "</table>"

   ::Write( cErrMsg )

   OutErr( cErrMsg )

   ::Flush()

   QUIT

   RETURN NIL

METHOD Write( cString ) CLASS TIPCgi

   ::cHtmlPage += cString + _CRLF

   RETURN Self

METHOD StartHtml( hOptions ) CLASS TIPCgi

   ::cHtmlPage += ;
      "<!DOCTYPE html>" + _CRLF + ;
      "<html>" + ;
      '<head><meta charset="' + HtmlOption( hOptions, "encoding" ) + '" />' + ;
      HtmlTag( hOptions, "title", "title" ) + ;
      HtmlScript( hOptions ) + ;
      HtmlStyle( hOptions ) + ;
      HtmlLinkRel( hOptions ) + ;
      "</head>" + ;
      "<body " + HtmlAllOption( hOptions ) + ">"

   RETURN Self

METHOD EndHtml() CLASS TIPCgi

   ::cHtmlPage += "</body></html>"

   RETURN Self

STATIC FUNCTION HtmlTag( xVal, cKey, cDefault )

   LOCAL cVal

   IF HB_ISHASH( xVal ) .AND. ! Empty( cKey ) .AND. cKey $ xVal
      cVal := xVal[ cKey ]
      hb_HDel( xVal, cKey )
   ELSE
      cVal := ""
   ENDIF

   IF cVal == ""
      cVal := hb_defaultValue( cDefault, "" )
   ENDIF

   IF !( cVal == "" )
      cVal := "<" + cKey + ">" + cVal + "</" + cKey + ">"
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlOption( xVal, cKey, cPre, cPost, lScan )

   LOCAL cVal := ""

   IF HB_ISHASH( xVal )
      IF Empty( cKey )
         cVal := xVal
      ELSEIF cKey $ xVal
         cVal := xVal[ cKey ]
         IF Empty( lScan )
            hb_HDel( xVal, cKey )
         ENDIF
         cVal := cKey + "=" + '"' + cVal + '"'
         IF HB_ISSTRING( cPre )
            cVal := cPre + cVal
         ENDIF
         IF HB_ISSTRING( cPost )
            cVal += cPost
         ENDIF
      ENDIF
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlAllOption( hOptions, cSep )

   LOCAL cVal := ""

   IF HB_ISHASH( hOptions )
      hb_default( @cSep, " " )

      hb_HEval( hOptions, {| k | cVal += HtmlOption( hOptions, k,,, .T. ) + cSep } )
   ENDIF

   RETURN cVal

STATIC FUNCTION HtmlScript( hVal, cKey )

   LOCAL cRet := ""

   LOCAL hTmp
   LOCAL cVal
   LOCAL cTmp

   hb_default( @cKey, "script" )

   IF hb_HGetRef( hVal, cKey, @hTmp )
      IF hb_HGetRef( hTmp, "src", @cVal )
         IF HB_ISSTRING( cVal )
            cVal := { cVal }
         ENDIF
         IF HB_ISARRAY( cVal )
            cTmp := ""
            AScan( cVal, {| cFile | cTmp += '<script src="' + cFile + '" type="text/javascript"></script>' + _CRLF } )
            cRet += cTmp
         ENDIF
      ENDIF
      IF hb_HGetRef( hTmp, "var", @cVal )
         IF HB_ISSTRING( cVal )
            cVal := { cVal }
         ENDIF
         IF HB_ISARRAY( cVal )
            cTmp := ""
            AScan( cVal, {| cVar | cTmp += cVar } )
            cRet += '<script type="text/javascript">' + _CRLF + "<!--" + _CRLF + cTmp + _CRLF + "-->" + _CRLF + "</script>" + _CRLF
         ENDIF
      ENDIF
      hb_HDel( hVal, cKey )
   ENDIF

   RETURN cRet

STATIC FUNCTION HtmlStyle( hVal, cKey )

   LOCAL cRet := ""

   LOCAL hTmp
   LOCAL cVal
   LOCAL cTmp

   hb_default( @cKey, "style" )

   IF hb_HGetRef( hVal, cKey, @hTmp )
      IF hb_HGetRef( hTmp, "src", @cVal )
         IF HB_ISSTRING( cVal )
            cVal := { cVal }
         ENDIF
         IF HB_ISARRAY( cVal )
            cTmp := ""
            AScan( cVal, {| cFile | cTmp += '<link rel="StyleSheet" href="' + cFile + '" type="text/css">' + _CRLF } )
            cRet += cTmp
         ENDIF
      ENDIF
      IF hb_HGetRef( hTmp, "var", @cVal )
         IF HB_ISSTRING( cVal )
            cVal := { cVal }
         ENDIF
         IF HB_ISARRAY( cVal )
            cTmp := ""
            AScan( cVal, {| cVar | cTmp += cVar } )
            cRet += '<style type="text/css">' + _CRLF + "<!--" + _CRLF + cTmp + _CRLF + "-->" + _CRLF + "</style>" + _CRLF
         ENDIF
      ENDIF
      hb_HDel( hVal, cKey )
   ENDIF

   RETURN cRet

STATIC FUNCTION HtmlLinkRel( hVal, cKey )

   LOCAL cRet := ""

   LOCAL hTmp
   LOCAL cVal

   hb_default( @cKey, "link" )

   IF hb_HGetRef( hVal, cKey, @hTmp )
      IF hb_HGetRef( hTmp, "rel", @cVal )
         IF HB_ISSTRING( cVal )
            cVal := { cVal, cVal }
         ENDIF
         IF HB_ISARRAY( cVal )
            AScan( cVal, {| aVal | cRet += '<link rel="' + aVal[ 1 ] + '" href="' + aVal[ 2 ] + '"/>' + _CRLF } )
         ENDIF
      ENDIF
      hb_HDel( hVal, cKey )
   ENDIF

   RETURN cRet
