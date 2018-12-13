/*
 * TIP Class oriented Internet protocol library (HTTP)
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

CREATE CLASS TIPClientHTTP INHERIT TIPClient

   VAR cMethod
   VAR nReplyCode
   VAR cReplyDescr
   VAR nVersion     INIT 1
   VAR nSubversion  INIT 0
   VAR bChunked
   VAR hHeaders     INIT { => }
   VAR hCookies     INIT { => }
   VAR hFields      INIT { => }
   VAR cUserAgent   INIT "Mozilla/3.0 compatible"
   VAR cAuthMode    INIT ""
   VAR cBoundary
   VAR aAttachments INIT {}
   VAR lPersistent  INIT .F.

   METHOD New( oUrl, xTrace, oCredentials )
   METHOD Get( cQuery )
   METHOD Post( xPostData, cQuery )
   METHOD Put( xPostData, cQuery )
   METHOD Delete( xPostData, cQuery )
   METHOD Head( xPostData, cQuery )
   METHOD ReadHeaders( lClear )
   METHOD Read( nLen )
   METHOD UseBasicAuth()                INLINE ::cAuthMode := "Basic"
   METHOD ReadAll()
   METHOD setCookie( cLine )
   METHOD getcookies( cHost, cPath )
   METHOD Boundary( nType )
   METHOD Attach( cName, cFileName, cType )
   METHOD PostMultiPart( xPostData, cQuery )
   METHOD WriteAll( cFile )
   METHOD StandardFields()
   METHOD SetConnectionPersistent()     INLINE ::lPersistent := .T.
   METHOD IsConnectionAlive()           INLINE ::inetErrorCode( ::SocketCon ) == 0

   PROTECTED:

   METHOD PostByVerb( xPostData, cQuery, cVerb )

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClientHTTP

   ::super:new( oUrl, iif( hb_defaultValue( xTrace, .F. ), "http", xTrace ), oCredentials )

   ::nDefaultPort := iif( ::oUrl:cProto == "https", 443, 80 )
   ::nConnTimeout := 5000
   ::bChunked     := .F.

   hb_HCaseMatch( ::hHeaders, .F. )

   RETURN Self

METHOD Get( cQuery ) CLASS TIPClientHTTP

   IF ! HB_ISSTRING( cQuery )
      cQuery := ::oUrl:BuildQuery()
   ENDIF

   ::inetSendAll( ::SocketCon, "GET " + cQuery + " HTTP/1.1" + ::cCRLF )
   ::StandardFields()
   ::inetSendAll( ::SocketCon, ::cCRLF )
   IF ::inetErrorCode( ::SocketCon ) ==  0
      RETURN ::ReadHeaders()
   ENDIF

   RETURN .F.

METHOD Post( xPostData, cQuery ) CLASS TIPClientHTTP
   RETURN ::postByVerb( xPostData, cQuery, "POST" )

METHOD Put( xPostData, cQuery ) CLASS TIPClientHTTP
   RETURN ::postByVerb( xPostData, cQuery, "PUT" )

METHOD Delete( xPostData, cQuery ) CLASS TIPClientHTTP
   RETURN ::postByVerb( xPostData, cQuery, "DELETE" )

METHOD Head( xPostData, cQuery ) CLASS TIPClientHTTP
   RETURN ::postByVerb( xPostData, cQuery, "HEAD" )

METHOD PostByVerb( xPostData, cQuery, cVerb ) CLASS TIPClientHTTP

   LOCAL cData
   LOCAL item

   DO CASE
   CASE HB_ISHASH( xPostData )
      cData := ""
      FOR EACH item IN xPostData
         cData += ;
            tip_URLEncode( AllTrim( hb_CStr( item:__enumKey() ) ) ) + "=" + ;
            tip_URLEncode(          hb_CStr( item ) )
         IF ! item:__enumIsLast()
            cData += "&"
         ENDIF
      NEXT
   CASE HB_ISARRAY( xPostData )
      cData := ""
      FOR EACH item IN xPostData
         cData += ;
            tip_URLEncode( AllTrim( hb_CStr( item[ 1 ] ) ) ) + "=" + ;
            tip_URLEncode(          hb_CStr( item[ 2 ] ) )
         IF ! item:__enumIsLast()
            cData += "&"
         ENDIF
      NEXT
   CASE HB_ISSTRING( xPostData )
      cData := xPostData
   OTHERWISE
      RETURN .F.
   ENDCASE

   IF ! HB_ISSTRING( cQuery )
      cQuery := ::oUrl:BuildQuery()
   ENDIF

   ::inetSendAll( ::SocketCon, hb_defaultValue( cVerb, "POST" ) + " " + cQuery + " HTTP/1.1" + ::cCRLF )

   ::StandardFields()
   IF ! "Content-Type" $ ::hFields
      ::inetSendAll( ::SocketCon, "Content-Type: application/x-www-form-urlencoded" + ::cCRLF )
   ENDIF
   ::inetSendAll( ::SocketCon, "Content-Length: " + hb_ntos( hb_BLen( cData ) ) + ::cCRLF )

   // End of header
   ::inetSendAll( ::SocketCon, ::cCRLF )

   IF ::inetErrorCode( ::SocketCon ) == 0
      ::inetSendAll( ::SocketCon, cData )
      ::bInitialized := .T.
      RETURN ::ReadHeaders()
   ENDIF

   RETURN .F.

METHOD StandardFields() CLASS TIPClientHTTP

   LOCAL oEncoder, cCookies
   LOCAL field

   ::inetSendAll( ::SocketCon, "Host: " + ::oUrl:cServer + ::cCRLF )
   ::inetSendAll( ::SocketCon, "User-agent: " + ::cUserAgent + ::cCRLF )
   IF ! ::lPersistent
      ::inetSendAll( ::SocketCon, "Connection: close" + ::cCRLF )
   ENDIF

   // Perform a basic authentication request
   IF ::cAuthMode == "Basic" .AND. ! "Authorization" $ ::hFields
      oEncoder := TIPEncoderBase64():New()
      oEncoder:bHttpExcept := .T.
      ::inetSendAll( ::SocketCon, "Authorization: Basic " + ;
         oEncoder:Encode( ::oUrl:cUserID + ":" + ::oUrl:cPassword ) + ::cCRLF )
   ENDIF

   // send cookies
   cCookies := ::getCookies()
   IF ! Empty( cCookies )
      ::inetSendAll( ::SocketCon, "Cookie: " + cCookies + ::cCRLF )
   ENDIF

   // Send optional Fields
   FOR EACH field IN ::hFields
      ::inetSendAll( ::SocketCon, field:__enumKey() + ": " + field + ::cCRLF )
   NEXT

   RETURN .T.

METHOD ReadHeaders( lClear ) CLASS TIPClientHTTP

   LOCAL cLine, nPos, aVersion
   LOCAL aHead

   // Now reads the fields and set the content length
   IF ( cLine := hb_defaultValue( ::inetRecvLine( ::SocketCon, @nPos, 500 ), "" ) ) == ""
      // In case of timeout or error on receiving
      RETURN .F.
   ENDIF

   // Get Protocol version
   aVersion := hb_regex( "^HTTP/(.)\.(.) ([0-9][0-9][0-9]) +(.*)$", cLine )
   ::cReply := cLine

   IF Empty( aVersion )
      ::nVersion := 0
      ::nSubversion := 9
      ::nReplyCode := 0
      ::cReplyDescr := ""
   ELSE
      ::nVersion := Val( aVersion[ 2 ] )
      ::nSubversion := Val( aVersion[ 3 ] )
      ::nReplyCode := Val( aVersion[ 4 ] )
      ::cReplyDescr := aVersion[ 5 ]
   ENDIF

   ::nLength := -1
   ::bChunked := .F.
   IF hb_defaultValue( lClear, .F. ) .AND. ! Empty( ::hHeaders )
      ::hHeaders := { => }
   ENDIF
   cLine := ::inetRecvLine( ::SocketCon, @nPos, 500 )
   DO WHILE ::inetErrorCode( ::SocketCon ) == 0 .AND. HB_ISSTRING( cLine ) .AND. ! cLine == ""

      IF Len( aHead := hb_regexSplit( ":", cLine,,, 1 ) ) != 2
         cLine := ::inetRecvLine( ::SocketCon, @nPos, 500 )
         LOOP
      ENDIF

      ::hHeaders[ aHead[ 1 ] ] := LTrim( aHead[ 2 ] )

      DO CASE
      // RFC 2068 forces to discard content length on chunked encoding
      CASE Lower( aHead[ 1 ] ) == "content-length" .AND. ! ::bChunked
         cLine := SubStr( cLine, 16 )
         ::nLength := Val( cLine )

      // as above
      CASE Lower( aHead[ 1 ] ) == "transfer-encoding"
         IF "chunked" $ Lower( cLine )
            ::bChunked := .T.
            ::nLength := -1
         ENDIF
      CASE Lower( aHead[ 1 ] ) == "set-cookie"
         ::setCookie( aHead[ 2 ] )
      ENDCASE

      cLine := ::inetRecvLine( ::SocketCon, @nPos, 500 )
   ENDDO
   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD Read( nLen ) CLASS TIPClientHTTP

   LOCAL cData, nPos, cLine, aHead

   IF ! ::bInitialized
      ::bInitialized := .T.
      IF ! ::Get()
         RETURN NIL
      ENDIF
   ENDIF

   /* On HTTP/1.1 protocol, content length can be in hex format before each chunk.
      The chunk header is read each time nLength is -1; While reading the chunk,
      nLength is set to nRead plus the expected chunk size. After reading the
      chunk, the footer is discarded, and nLength is reset to -1.
    */
   IF ::nLength == -1 .AND. ::bChunked

      cLine := ::inetRecvLine( ::SocketCon, @nPos, 1024 )

      IF ! HB_ISSTRING( cLine ) .OR. cLine == ""
         RETURN NIL
      ENDIF

      // if this is the last chunk ...
      IF cLine == "0"

         // read the footers.
         DO WHILE ! ( cLine := hb_defaultValue( ::inetRecvLine( ::SocketCon, @nPos, 1024 ), "" ) ) == ""
            // add Headers to footers
            IF Len( aHead := hb_regexSplit( ":", cLine,,, 1 ) ) == 2
               ::hHeaders[ aHead[ 1 ] ] := LTrim( aHead[ 2 ] )
            ENDIF
         ENDDO

         // we are done
         ::bEof := .T.
         RETURN NIL
      ENDIF

      // A normal chunk here

      // Remove the extensions
      IF ( nPos := At( ";", cLine ) ) > 0
         cLine := Left( cLine, nPos - 1 )
      ENDIF

      // Convert to length
      // Set length so that super::Read reads in at max cLine bytes.
      ::nLength := hb_HexToNum( cLine ) + ::nRead

   ENDIF

   // nLen is normalized by super:read()
   cData := ::super:Read( nLen )

   // If bEof is set with chunked encoding, this means that the whole chunk has been read;
   IF ::bEof .AND. ::bChunked
      ::bEof := .F.
      ::nLength := -1
      // chunked data is followed by a blank line
      ::InetRecvLine( ::SocketCon, @nPos, 1024 )
   ENDIF

   RETURN cData

METHOD ReadAll() CLASS TIPClientHTTP

   LOCAL cOut := "", cChunk

   IF ! ::bInitialized
      ::bInitialized := .T.
      IF ! ::Get()
         RETURN NIL
      ENDIF
   ENDIF
   IF ::bChunked
      DO WHILE ( cChunk := ::read() ) != NIL
         cOut += cChunk
         // ::nLength := -1
      ENDDO
   ELSE
      RETURN ::read()
   ENDIF

   RETURN cOut

METHOD PROCEDURE setCookie( cLine ) CLASS TIPClientHTTP

   // docs from https://tools.ietf.org/html/rfc2109
   LOCAL cHost, cPath, cName, cValue, aElements
   LOCAL cDefaultHost := ::oUrl:cServer, cDefaultPath := ::oUrl:cPath
   LOCAL x

   IF cDefaultPath == ""
      cDefaultPath := "/"
   ENDIF

   // this function currently ignores expires, secure and other tags that may be in the cookie for now...
   //   ? "Setting COOKIE:", cLine
   cName := cValue := ""
   cHost := cDefaultHost
   cPath := cDefaultPath
   FOR EACH x IN hb_regexSplit( ";", cLine )
      IF Len( aElements := hb_regexSplit( "=", x, 1 ) ) == 2
         IF x:__enumIsFirst()
            cName := AllTrim( aElements[ 1 ] )
            cValue := AllTrim( aElements[ 2 ] )
         ELSE
            SWITCH Upper( AllTrim( aElements[ 1 ] ) )
#if 0
            CASE "EXPIRES"
               EXIT
#endif
            CASE "PATH"
               cPath := AllTrim( aElements[ 2 ] )
               EXIT
            CASE "DOMAIN"
               cHost := AllTrim( aElements[ 2 ] )
               EXIT
            ENDSWITCH
         ENDIF
      ENDIF
   NEXT
   IF ! Empty( cName )
      // cookies are stored in hashes as host.path.name
      // check if we have a host hash yet
      IF ! cHost $ ::hCookies
         ::hCookies[ cHost ] := { => }
      ENDIF
      IF ! cPath $ ::hCookies[ cHost ]
         ::hCookies[ cHost ][ cPath ] := { => }
      ENDIF
      ::hCookies[ cHost ][ cPath ][ cName ] := cValue
   ENDIF

   RETURN

METHOD getcookies( cHost, cPath ) CLASS TIPClientHTTP

   LOCAL x, aDomKeys := {}, z, cKey, aPathKeys, nPath
   LOCAL a, cOut := "", c

   hb_default( @cHost, ::oUrl:cServer )

   IF ! HB_ISSTRING( cPath )
      cPath := ::oUrl:cPath
      IF cPath == ""
         cPath := "/"
      ENDIF
   ENDIF
   IF cHost == ""
      RETURN cOut
   ENDIF

   // tail matching the domain
   z := Len( cHost )
   cHost := Upper( cHost )
   FOR EACH x IN hb_HKeys( ::hCookies )
      IF Upper( Right( x, z ) ) == cHost .AND. ( Len( x ) == z .OR. SubStr( x, -z, 1 ) == "." )
         AAdd( aDomKeys, x )
      ENDIF
   NEXT

   // now that we have the domain matches we have to do path matching
   nPath := Len( cPath )
   FOR EACH x IN ASort( aDomKeys,,, {| cX, cY | Len( cX ) > Len( cY ) } )  // more specific paths should be sent before lesser generic paths
      aPathKeys := {}
      FOR EACH cKey IN hb_HKeys( ::hCookies[ x ] )
         IF cKey == "/" .OR. ( Len( cKey ) <= nPath .AND. Left( cKey, nPath ) == cKey )
            AAdd( aPathKeys, cKey )
         ENDIF
      NEXT

      FOR EACH a IN ASort( aPathKeys,,, {| cX, cY | Len( cX ) > Len( cY ) } )
         FOR EACH c IN hb_HKeys( ::hCookies[ x ][ a ] )
            IF ! cOut == ""
               cOut += "; "
            ENDIF
            cOut += c + "=" + ::hCookies[ x ][ a ][ c ]
         NEXT
      NEXT
   NEXT

   RETURN cOut

/* nType: 0=as found as the separator in the stdin stream
          1=as found as the last one in the stdin stream
          2=as found in the CGI environment
   Examples:
   0: -----------------------------41184676334     // in the body or stdin stream
   1: -----------------------------41184676334--   // last one of the stdin stream
   2: ---------------------------41184676334       // in the header or CGI environment
 */
METHOD Boundary( nType ) CLASS TIPClientHTTP

   IF ::cBoundary == NIL
      ::cBoundary := Replicate( "-", 27 ) + StrZero( hb_randNum( 99999999999 ), 11, 0 )
   ENDIF

   hb_default( @nType, 0 )

   RETURN ;
      iif( nType <= 1, "--", "" ) + ;
      ::cBoundary + ;
      iif( nType == 1, "--", "" )

METHOD PROCEDURE Attach( cName, cFileName, cType ) CLASS TIPClientHTTP

   AAdd( ::aAttachments, { cName, cFileName, cType } )

   RETURN

/* https://tools.ietf.org/html/rfc2388 */
METHOD PostMultiPart( xPostData, cQuery ) CLASS TIPClientHTTP

   LOCAL cData := "", item, cBound := ::boundary()
   LOCAL cCrlf := ::cCRlf, aAttachment
   LOCAL cFile, cType
   LOCAL hFile, cBuffer, nRead

   DO CASE
   CASE Empty( xPostData )
   CASE HB_ISHASH( xPostData )
      FOR EACH item IN xPostData
         cData += ;
            cBound + cCrlf + "Content-Disposition: form-data; name=" + '"' + ;
            tip_URLEncode( AllTrim( hb_CStr( item:__enumKey() ) ) ) + '"' + cCrlf + cCrLf + ;
            tip_URLEncode( AllTrim( hb_CStr( item ) ) ) + cCrLf
      NEXT
   CASE HB_ISARRAY( xPostData )
      FOR EACH item IN xPostData
         IF Len( item ) >= 2
            cData += ;
               cBound + cCrlf + "Content-Disposition: form-data; name=" + '"' + ;
               tip_URLEncode( AllTrim( hb_CStr( item[ 1 ] ) ) ) + '"' + cCrlf + cCrLf + ;
               tip_URLEncode( AllTrim( hb_CStr( item[ 2 ] ) ) ) + cCrLf
         ENDIF
      NEXT
   CASE HB_ISSTRING( xPostData )
      cData := xPostData
   ENDCASE

   FOR EACH aAttachment IN ::aAttachments

      cFile := hb_defaultValue( aAttachment[ 2 ], "" )

      cType := aAttachment[ 3 ]
      IF ! HB_ISSTRING( cType ) .OR. Empty( cType )
         cType := "text/html"
      ENDIF

      cData += cBound + cCrlf + ;
         "Content-Disposition: form-data; " + ;
         "name=" + '"' + hb_defaultValue( aAttachment[ 1 ], "unspecified" ) + '"' + "; " + ;
         "filename=" + '"' + hb_FNameNameExt( hb_DirSepToOS( cFile ) ) + '"' + cCrlf + ;
         "Content-Type: " + cType + cCrLf + ;
         cCrLf

      IF ( hFile := hb_vfOpen( cFile, FO_READ ) ) != NIL
         cBuffer := Space( 65536 )
         DO WHILE ( nRead := hb_vfRead( hFile, @cBuffer, hb_Blen( cBuffer ) ) ) > 0
            cData += hb_BLeft( cBuffer, nRead )
         ENDDO
         hb_vfClose( hFile )
      ENDIF

      cData += cCrlf
   NEXT

   cData += cBound + "--" + cCrlf

   IF ! HB_ISSTRING( cQuery )
      cQuery := ::oUrl:BuildQuery()
   ENDIF

   ::inetSendAll( ::SocketCon, "POST " + cQuery + " HTTP/1.1" + ::cCRLF )
   ::StandardFields()

   IF ! "Content-Type" $ ::hFields
      ::inetSendAll( ::SocketCon, "Content-Type: multipart/form-data; boundary=" + ::boundary( 2 ) + ::cCrlf )
   ENDIF

   ::inetSendAll( ::SocketCon, "Content-Length: " + hb_ntos( hb_BLen( cData ) ) + ::cCRLF )
   // End of header
   ::inetSendAll( ::SocketCon, ::cCRLF )

   IF ::inetErrorCode( ::SocketCon ) ==  0
      ::inetSendAll( ::SocketCon, cData )
      ::bInitialized := .T.
      RETURN ::ReadHeaders()
   ENDIF

   RETURN .F.

METHOD WriteAll( cFile ) CLASS TIPClientHTTP

   LOCAL hFile
   LOCAL lSuccess
   LOCAL cStream

   IF ( hFile := hb_vfOpen( cFile, FO_CREAT + FO_TRUNC + FO_WRITE + FO_EXCLUSIVE ) ) != NIL
      cStream := ::ReadAll()
      lSuccess := ( hb_vfWrite( hFile, cStream ) == hb_BLen( cStream ) )
      hb_vfClose( hFile )
   ELSE
      lSuccess := .F.
   ENDIF

   RETURN lSuccess
