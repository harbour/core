/*
 * uHTTPD (Micro HTTP server) cgi functions
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

#include "error.ch"
#include "fileio.ch"

#define CRLF           ( Chr( 13 ) + Chr( 10 ) )
#define THROW( oErr )  ( Eval( ErrorBlock(), oErr ), Break( oErr ) )

MEMVAR _SERVER, _GET, _POST, _COOKIE, _REQUEST, _HTTP_REQUEST

FUNCTION uhttpd_GetVars( cFields, cSeparator )

   LOCAL hHashVars := { => }
   LOCAL aField, cField
   LOCAL cName, xValue

   FOR EACH cField IN uhttpd_Split( hb_defaultValue( cSeparator, "&" ), cFields )

      IF Len( aField := uhttpd_Split( "=", cField, 1 ) ) == 1
         hHashVars[ aField[ 1 ] ] := NIL
         LOOP
      ELSEIF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[ 1 ] )
      xValue := tip_URLDecode( aField[ 2 ] )

      // Is it an array entry?
      IF Right( cName, 2 ) == "[]"
         cName := hb_StrShrink( cName, 2 )
         hHashVars[ cName ] := { xValue }
      ELSE
         // now check if variable already exists. If yes and I have already another element
         // with same name, then I will change it to an array
         IF cName $ hHashVars
            IF ! HB_ISARRAY( hHashVars[ cName ] )
               // Transform it to array
               hHashVars[ cName ] := { hHashVars[ cName ] }
            ENDIF
            AAdd( hHashVars[ cName ], xValue )
         ELSE
            hHashVars[ cName ] := xValue
         ENDIF
      ENDIF
   NEXT

   RETURN hHashVars

/*
  uhttpd_SplitUrl( cUrl ) --> hUrl
  (C) 2006 Francesco Saverio Giudice

  Splits a valid URL into simple components and return them in a hash
  it works like parse_url() PHP function

  a URL string is something like this:
  https://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

  Parameters:
  cUrl     -   Valid URL string

  Returns:
  hUrl     -   Hash containing these keys:
               SCHEME   - protocol name
               HOST     - hostname
               PORT     - protocol port number
               USER     - username
               PASS     - password
               PATH     - path to directory and/or file
               QUERY    - part after question mark ?
               FRAGMENT - part after hashmark #

 */
FUNCTION uhttpd_SplitUrl( cUrl )

   LOCAL hUrl
   LOCAL nPos, cTemp, cUserNamePassword, cHostnamePort
   LOCAL cProto, cHost, cPort, nPort, cUser, cPass, cPath, cQuery, cFragment
   LOCAL cUri

   cTemp := cUrl
   cUri  := ""

   // Starting with
   // https://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read protocol
   IF ( nPos := At( "://", cTemp ) ) > 0
      cProto := Left( cTemp, nPos - 1 )
      // delete protocol from temp string
      cTemp := SubStr( cTemp, nPos + 3 )
   ELSE
      cProto := ""
   ENDIF

   cUri += cProto + iif( Empty( cProto ), "", "://" )

   // Now we have:
   // [username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read username and password
   IF ( nPos := At( "@", cTemp ) ) > 0
      cUserNamePassword := Left( cTemp, nPos - 1 )
      // delete Username and Password from temp string
      cTemp := SubStr( cTemp, nPos + 1 )
      // Split username and password
      IF ( nPos := At( ":", cUserNamePassword ) ) > 0
         cUser := Left( cUserNamePassword, nPos - 1 )
         cPass := SubStr( cUserNamePassword, nPos + 1 )
      ELSE
         cUser := cUserNamePassword
         cPass := ""
      ENDIF
   ELSE
      cUser := ""
      cPass := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Search for anchor using # char from right
   IF ( nPos := RAt( "#", cTemp ) ) > 0
      cFragment := SubStr( cTemp, nPos + 1 )

      // delete anchor from temp string
      cTemp := Left( cTemp, nPos - 1 )
   ELSE
      cFragment := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]]]

   // Search for Query part using ? char from right
   IF ( nPos := RAt( "?", cTemp ) ) > 0
      cQuery := SubStr( cTemp, nPos + 1 )

      // delete query from temp string
      cTemp := Left( cTemp, nPos - 1 )
   ELSE
      cQuery := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]]

   cUri += cTemp

   // Search for Path part using / char from right
   IF ( nPos := RAt( "/", cTemp ) ) > 0
      cPath := SubStr( cTemp, nPos )

      // delete path from temp string
      cTemp := Left( cTemp, nPos - 1 )
   ELSE
      cPath := "/"
   ENDIF

   // Now we have:
   // hostname[:port]

   cHostnamePort := cTemp

   // Searching port number
   IF ( nPos := At( ":", cHostnamePort ) ) > 0
      cHost := Left( cHostnamePort, nPos - 1 )
      cPort := SubStr( cHostnamePort, nPos + 1 )
      nPort := Val( cPort )
      IF nPort <= 0
         nPort := -1
      ENDIF
   ELSE
      cHost := cHostnamePort
      nPort := -1
   ENDIF

   // Assemble hash
   hUrl := { ;
      "SCHEME"   => cProto, ;
      "HOST"     => cHost, ;
      "PORT"     => nPort, ;
      "USER"     => cUser, ;
      "PASS"     => cPass, ;
      "PATH"     => cPath, ;
      "QUERY"    => cQuery, ;
      "FRAGMENT" => cFragment, ;
      "URI"      => cURI }

   hb_HCaseMatch( hUrl, .F. )  // Prevents case matching
   hb_HAutoAdd( hUrl, .F. )  // Prevents externals to add something else to this Hash

   RETURN hUrl


/*
  uhttpd_SplitString( cString ) --> aLines
  (C) 2006 Francesco Saverio Giudice

  Splits a string into simple components and return them in an array

  Parameters:
  cString     -   Initial string
  cDelim      -   Delimiter - default CRLF
  lRemDelim   -   Remove delimiter from return values - default .T.

  Returns:
  aLines      -   Array with lines / fields for each element

  Sample:
  SplitString( "this=is=a=line=with=equals", "=" ) -> { "this", "is", "a", "line", "with", "equals" }

 */
FUNCTION uhttpd_SplitString( cString, cDelim, lRemDelim, nCount )

   LOCAL nEOLPos
   LOCAL cBuffer := cString
   LOCAL aLines  := {}, cLine
   LOCAL nHowMany := 0

   hb_default( @cDelim, Chr( 13 ) + Chr( 10 ) )
   hb_default( @lRemDelim, .T. )
   hb_default( @nCount, -1 )

   // WriteToLogFile( "Splitstring: " + CStr( cString ) )

   DO WHILE ( nEOLPos := At( cDelim, cBuffer ) ) > 0
      nHowMany++
      IF lRemDelim
         cLine := Left( cBuffer, nEOLPos - 1 )
      ELSE
         cLine := Left( cBuffer, nEOLPos - 1 + Len( cDelim ) )
      ENDIF
      // WriteToLogFile( "cBuffer, cDelim, nEOLPos, cLine: " + CStr( cBuffer ) + "," + CStr( cDelim ) + "," + CStr( nEOLPos ) + "," + CStr( cLine ) )
      AAdd( aLines, cLine )
      cBuffer := SubStr( cBuffer, nEOLPos + Len( cDelim ) )
      IF nCount > -1
         IF nHowMany >= nCount
            EXIT
         ENDIF
      ENDIF
   ENDDO

   // Check last line
   IF ! HB_ISNULL( cBuffer )
      AAdd( aLines, cBuffer )
   ENDIF

   RETURN aLines

/*
 * uhttpd_DateToGMT( tDate, nDayToAdd, nSecsToAdd ) --> cGMTDate
 *
 * tDate     : default hb_DateTime()
 * cTime     : default "00:00:00"
 * nDayToAdd : default 0 - may be a negative number
 *
 * cGMTDate  : The string return in form of "Sat, 31 Oct 2003 00:00:00 GMT"
 */

FUNCTION uhttpd_DateToGMT( tDate, nDayToAdd, nSecsToAdd )

   hb_default( @tDate, hb_DateTime() )

   tDate += hb_defaultValue( nDayToAdd, 0 )
   tDate += hb_defaultValue( nSecsToAdd, 0 ) / 86400

   RETURN ;
      { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }[ DoW( tDate ) ] + ", " + ;
      StrZero( Day( tDate ), 2 ) + " " + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[ Month( tDate ) ] + " " + ;
      StrZero( Year( tDate ), 4 ) + " " + ;
      hb_TToC( tDate, "", "hh:mm:ss" ) + " GMT"

FUNCTION uhttpd_OutputString( cString, hTranslate, lProtected )

   // TraceLog( "OutputString( cString, hTranslate, lProtected )", cString, hTranslate, lProtected )

   RETURN iif( hb_defaultValue( lProtected, .F. ), ;
      uhttpd_HtmlSpecialChars( cString ), ;
      uhttpd_TranslateStrings( cString, hb_defaultValue( hTranslate, { '"' => "&quot;", " " => "&nbsp;" } ) ) )

FUNCTION uhttpd_HtmlSpecialChars( cString, cQuote_style )
   RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, { ;
      "&" => "&amp;", ;
      "<" => "&lt;", ;
      ">" => "&gt;" } )

FUNCTION uhttpd_HtmlConvertChars( cString, cQuote_style, hTranslations )

   SWITCH hb_defaultValue( cQuote_style, "ENT_COMPAT" )
   CASE "ENT_COMPAT"
      hTranslations[ '"' ] := "&quot;"
      EXIT
   CASE "ENT_QUOTES"
      hTranslations[ '"' ] := "&quot;"
      hTranslations[ "'" ] := "&#039;"
      EXIT
   CASE "ENT_NOQUOTES"
      EXIT
   ENDSWITCH

   RETURN uhttpd_TranslateStrings( cString, hTranslations )

FUNCTION uhttpd_CRLF2BR( cString )
   RETURN hb_StrReplace( cString, { CRLF => "<br />" } )

FUNCTION uhttpd_TranslateStrings( cString, hTranslate )
   RETURN hb_StrReplace( cString, hTranslate )

FUNCTION uhttpd_StrStr( cString, cSearch )

   LOCAL nPos := At( cSearch, cString )

   RETURN iif( nPos > 0, SubStr( cString, nPos ), NIL )

FUNCTION uhttpd_StrIStr( cString, cSearch )

   LOCAL nPos := hb_AtI( cSearch, cString )

   RETURN iif( nPos > 0, SubStr( cString, nPos ), NIL )

FUNCTION uhttpd_HtmlEntities( cString, cQuote_style )

   LOCAL hTranslations := { => }
   LOCAL i

   // ATTENTION, these chars are visible only with OEM font
   FOR i := 160 TO 255
      hTranslations[ hb_BChar( i ) ] := "&#" + StrZero( i, 3 ) + ";"
   NEXT

   RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, hTranslations )

PROCEDURE uhttpd_Die( cError )

   LOCAL oErr

   IF HB_ISSTRING( cError )
#if 0
      __OutDebug( "cError: ", cError )
      IF ! oCGI:HeaderSent()
        oCGI:WriteLN( CRLF2BR( cError ), CRLF2BR( CRLF() ) )
        // oCGI:WriteLN( CRLF2BR( hb_DumpVar( TConfigure():hConfig ) ) )
      ENDIF
#endif
      // Generate Error
      oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_LIMIT
      oErr:subSystem   := "uhttpd_CGI"
      oErr:subCode     := 0
      oErr:description := cError
      oErr:canRetry    := .F.
      oErr:canDefault  := .F.
      oErr:fileName    := ""
      oErr:osCode      := 0
      IF hb_defaultValue( Eval( ErrorBlock(), oErr ), .T. )
         __errInHandler()
      ENDIF
      Break( oErr )
      // QUIT
   ELSE
      QUIT
   ENDIF

   RETURN

FUNCTION uhttpd_HTMLSpace( n )
   RETURN Replicate( "&nbsp;", n )  // "&#32;"

PROCEDURE uhttpd_WriteToLogFile( cString, cLog )

   LOCAL hFile

#if 0
   hb_default( @cLog, hb_DirBase() + "logfile.log" )
#endif
   hb_default( @cLog, hb_ps() + "tmp" + hb_ps() + "logfile.log" )

   // cString := "PROCEDURE: " + ProcName( -2 ) + " " + cString

   IF ( hFile := hb_vfOpen( cLog, FO_CREAT + FO_WRITE + FO_SHARED ) ) != NIL
      hb_vfSeek( hFile, 0, FS_END )
      hb_vfWrite( hFile, cString + hb_eol() )
      hb_vfClose( hFile )
   ENDIF

   RETURN

/* --- */

FUNCTION uhttpd_AppFullPath()
   RETURN hb_DirBase()

FUNCTION uhttpd_AppFullName()
   RETURN hb_FNameNameExt( hb_ProgName() )

FUNCTION uhttpd_CStrToVal( cExp, cType )

   IF ! HB_ISSTRING( cExp )
      THROW( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDIF

   SWITCH cType
   CASE "M"
   CASE "C"
      RETURN cExp
   CASE "P"
      RETURN hb_HexToNum( cExp )
   CASE "D"
      IF SubStr( cExp, 3, 1 ) >= "0" .AND. ;
         SubStr( cExp, 3, 1 ) <= "9" .AND. ;
         SubStr( cExp, 5, 1 ) >= "0" .AND. ;
         SubStr( cExp, 5, 1 ) <= "9"
         RETURN hb_SToD( cExp )
      ELSE
         RETURN CToD( cExp )
      ENDIF
   CASE "L"
      RETURN Left( cExp, 1 ) $ "TY" .OR. SubStr( cExp, 2, 1 ) $ "TY"
   CASE "N"
      RETURN Val( cExp )
   CASE "U"
      RETURN NIL
   ENDSWITCH

   THROW( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

   RETURN NIL

FUNCTION uhttpd_GetField( cVar, cType )

   LOCAL xVal

   IF hb_HGetRef( _Request, cVar, @xVal )
      IF Empty( xVal )
         xVal := NIL
      ENDIF
      IF HB_ISSTRING( cType ) .AND. cType $ "NLD"
         xVal := uhttpd_CStrToVal( xVal, cType )
      ENDIF
   ENDIF

   RETURN xVal

FUNCTION uhttpd_SetField( cVar, cVal )

   LOCAL xVal := uhttpd_HGetValue( _Request, cVar )

   _Request[ cVar ] := cVal

   RETURN xVal

FUNCTION uhttpd_HGetValue( hHash, cKey )
   RETURN iif( HB_ISHASH( hHash ), hb_HGetDef( hHash, cKey ), NIL )
