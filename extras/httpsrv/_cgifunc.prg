/*
 * Harbour Project source code:
 *    uHTTPD (Micro HTTP server) cgi functions
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

// #define HB_USE_HBTIP    // Use functions from HBTIP - TOIMPLEMENT

#define CRLF ( Chr( 13 ) + Chr( 10 ) )
#xtranslate Throw( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )

MEMVAR _SERVER, _GET, _POST, _COOKIE, _REQUEST, _HTTP_REQUEST

FUNCTION uhttpd_GetVars( cFields, cSeparator )

   LOCAL hHashVars := { => }
   LOCAL aField, cField, aFields
   LOCAL cName, xValue

   __defaultNIL( @cSeparator, "&" )

   aFields := uhttpd_Split( cSeparator, cFields )

   FOR EACH cField in aFields
      aField := uhttpd_Split( "=", cField, 1 )
      IF Len( aField ) == 1
         hHashVars[ aField[ 1 ] ] := NIL
         LOOP
      ELSEIF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[ 1 ] )
      xValue := uhttpd_UrlDecode( aField[ 2 ] )

      // Is it an array entry?
      IF SubStr( cName, Len( cName ) - 1 ) == "[]"
         cName := Left( cName, Len( cName ) - 2 )
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
  http://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

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

   LOCAL hUrl := { => }
   LOCAL nPos, cTemp, cUserNamePassword, cHostnamePort
   LOCAL cProto, cHost, cPort, nPort, cUser, cPass, cPath, cQuery, cFragment
   LOCAL cUri

   // Prevents case matching
   hb_HCaseMatch( hUrl, .F. )

   cTemp := cUrl
   cUri  := ""

   // Starting with
   // http://[username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read protocol
   nPos := At( "://", cTemp )
   IF nPos > 0
      cProto := Left( cTemp, nPos - 1 )
      // delete protocol from temp string
      cTemp := SubStr( cTemp, nPos + 3 )
   ELSE
      cProto := ""
   ENDIF

   cUri += cProto + iif( ! Empty( cProto ), "://", "" )

   // Now we have:
   // [username:password@]hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]][#anchor]]

   // Read username and password
   nPos := At( "@", cTemp )
   IF nPos > 0
      cUserNamePassword := Left( cTemp, nPos - 1 )
      // delete Username and Password from temp string
      cTemp := SubStr( cTemp, nPos + 1 )
      // Split username and password
      nPos := At( ":", cUserNamePassword )
      IF nPos > 0
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
   nPos := RAt( "#", cTemp )
   IF nPos > 0
      cFragment := SubStr( cTemp, nPos + 1 )

      // delete anchor from temp string
      cTemp := Left( cTemp, nPos - 1 )

   ELSE
      cFragment := ""
   ENDIF

   // Now we have:
   // hostname[:port][/path[/file[.ext]][?arg1=[value][&arg2=[value]]]]

   // Search for Query part using ? char from right
   nPos := RAt( "?", cTemp )
   IF nPos > 0
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
   nPos := RAt( "/", cTemp )
   IF nPos > 0
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
   nPos := At( ":", cHostnamePort )
   IF nPos > 0
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
   hUrl[ "SCHEME" ]   := cProto
   hUrl[ "HOST" ]     := cHost
   hUrl[ "PORT" ]     := nPort
   hUrl[ "USER" ]     := cUser
   hUrl[ "PASS" ]     := cPass
   hUrl[ "PATH" ]     := cPath
   hUrl[ "QUERY" ]    := cQuery
   hUrl[ "FRAGMENT" ] := cFragment
   hUrl[ "URI" ]      := cURI

   // Prevents externals to add something else to this Hash
   hb_HAutoAdd( hUrl, .F. )

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

   __defaultNIL( @cDelim, ( Chr( 13 ) + Chr( 10 ) ) )
   __defaultNIL( @lRemDelim, .T. )
   __defaultNIL( @nCount, -1 )

   // WriteToLogFile( "Splitstring: " + CStr( cString ) )

   DO WHILE ( nEOLPos := At( cDelim, cBuffer ) ) > 0
      nHowMany++
      IF lRemDelim
         cLine := Left( cBuffer, nEOLPos - 1 )
      ELSE
         cLine := Left( cBuffer, ( nEOLPos + Len( cDelim ) ) - 1 )
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
   IF Len( cBuffer ) > 0
      AAdd( aLines, cBuffer )
   ENDIF

   RETURN aLines

/************************************************************
* Encoding URL
*/
FUNCTION uhttpd_URLEncode( cString, lComplete )

#ifdef HB_USE_HBTIP

   __defaultNIL( @lComplete, .T. )

   RETURN TIPENCODERURL_ENCODE( cString, lComplete )
#else
   LOCAL cRet := "", i, nVal, cChar

   __defaultNIL( @lComplete, .T. )

   FOR i := 1 TO Len( cString )
      cChar := SubStr( cString, i, 1 )
      DO CASE
      CASE cChar == " "
         cRet += "+"

      CASE ( cChar >= "A" .AND. cChar <= "Z" ) .OR. ;
           ( cChar >= "a" .AND. cChar <= "z" ) .OR. ;
           ( cChar >= "0" .AND. cChar <= "9" ) .OR. ;
           cChar == "." .OR. cChar == "," .OR. cChar == "&" .OR. ;
           cChar == "/" .OR. cChar == ";" .OR. cChar == "_"
         cRet += cChar

      CASE iif( ! lComplete, cChar == ":" .OR. cChar == "?" .OR. cChar == "=", .F. )
         cRet += cChar

      OTHERWISE
         nVal := Asc( cChar )
         cRet += "%" + hb_NumToHex( nVal )
      ENDCASE
   NEXT

   RETURN cRet

/************************************************************
* Decoding URL
*/
FUNCTION uhttpd_URLDecode( cString )

#ifdef HB_USE_HBTIP
   RETURN TIPENCODERURL_DECODE( cString )
#else
   LOCAL cRet := "", i, cChar

   FOR i := 1 TO Len( cString )
      cChar := SubStr( cString, i, 1 )
      DO CASE
      CASE cChar == "+"
         cRet += " "

      CASE cChar == "%"
         i++
         cRet += Chr( hb_HexToNum( SubStr( cString, i, 2 ) ) )
         i++

      OTHERWISE
         cRet += cChar

      ENDCASE

   NEXT

   RETURN cRet
#endif

/*
 * DateToGMT( dDate, cTime, nDayToAdd ) --> cGMTDate
 *
 * dDate     : default Date()
 * cTime     : default "00:00:00"
 * nDayToAdd : default 0 - may be a negative number
 *
 * cGMTDate  : The string return in form of "Sat, 31 Oct 2003 00:00:00 GMT"
 */

FUNCTION uhttpd_DateToGMT( dDate, cTime, nDayToAdd, nSecsToAdd )

   __defaultNIL( @dDate, Date() )
   __defaultNIL( @cTime, Time() )
   __defaultNIL( @nDayToAdd, 0 )
   __defaultNIL( @nSecsToAdd, 0 )

   // TraceLog( "DateToGMT - StartingValue", dDate, cTime, nDayToAdd, nSecsToAdd )

   cTime := uhttpd_AddSecondsToTime( cTime, nSecsToAdd, @nDayToAdd )
   dDate += nDayToAdd

   RETURN ;
      { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }[ DoW( dDate ) ] + ", " + ;
      StrZero( Day( dDate ), 2 ) + " " + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[ Month( dDate ) ] + " " + ;
      StrZero( Year( dDate ), 4 ) + " " + ;
      cTime + " GMT"

/*
 * AddSecondsToTime( cTime, nSecsToAdd, @nDaysAdded ) --> cNewTime
 *
 * cTime      : default "00:00:00"
 * nSecsToAdd : default 0 - may be a negative number
 * nDaysAdded : (out) return how many days add (or subtract) to actual date if numbers seconds is
 *                    more than 86400 seconds (1 day)
 *
 * cNewTime   : The new time string
 *
 * Rules: time is converted to seconds from midnight, then added of nSecsToAdd. Divided of 1 day and
 *        then reverted to Time string
 */

FUNCTION uhttpd_AddSecondsToTime( cTime, nSecsToAdd, nDaysAdded )

   LOCAL nOneDaySeconds := 86400  // 24 * 60 * 60
   LOCAL cNewTime, nSecs

   __defaultNIL( @cTime, Time() )
   __defaultNIL( @nSecsToAdd, 0 )
   // nDaysAdded can be already valued, so below i add to this value
   __defaultNIL( @nDaysAdded, 0 )

   IF nSecsToAdd != 0
      nSecs      := Secs( cTime ) + nSecsToAdd
      nDaysAdded += Int( nSecs / nOneDaySeconds )  // Attention! nDaysAdded can be already filled
      nSecs      := nSecs - nDaysAdded
      cNewTime   := TString( nSecs )
   ELSE
      cNewTime := cTime
   ENDIF

   RETURN cNewTime

FUNCTION uhttpd_OutputString( cString, aTranslate, lProtected )

   LOCAL cHtml

   __defaultNIL( @lProtected, .F. )
   __defaultNIL( @aTranslate, { { '"', "&quot;" }, { " ", "&nbsp;" } } )

   // TraceLog( "OutputString( cString, aTranslate, lProtected )", cString, aTranslate, lProtected )
   IF lProtected
      cHtml := uhttpd_HtmlSpecialChars( cString )
   ELSE
      cHtml := uhttpd_TranslateStrings( cString, aTranslate )
   ENDIF
   // TraceLog( "OutputString(): cHtml", cHtml )

   RETURN cHtml

FUNCTION uhttpd_HtmlSpecialChars( cString, cQuote_style )

   LOCAL aTranslations := { ;
      { "&", "&amp;" }, ;
      { "<", "&lt;"  }, ;
      { ">", "&gt;"  } }

   RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

FUNCTION uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

   __defaultNIL( @cQuote_style, "ENT_COMPAT" )

   DO CASE
   CASE cQuote_style == "ENT_COMPAT"
      AAdd( aTranslations, { '"', "&quot;" } )
   CASE cQuote_style == "ENT_QUOTES"
      AAdd( aTranslations, { '"', "&quot;" } )
      AAdd( aTranslations, { "'", "&#039;" } )
   CASE cQuote_style == "ENT_NOQUOTES"
   ENDCASE

   RETURN uhttpd_TranslateStrings( cString, aTranslations )

FUNCTION uhttpd_CRLF2BR( cString )

   LOCAL aTranslations := { ;
      { CRLF, "<br />" } }

   RETURN uhttpd_TranslateStrings( cString, aTranslations )

FUNCTION uhttpd_TranslateStrings( cString, aTranslate )

   LOCAL aTran

   FOR EACH aTran IN aTranslate
      IF aTran[ 1 ] $ cString
         cString := StrTran( cString, aTran[ 1 ], aTran[ 2 ] )
      ENDIF
   NEXT

   RETURN cString

FUNCTION uhttpd_StrStr( cString, cSearch )

   LOCAL nPos := At( cSearch, cString )
   LOCAL cVal := iif( nPos > 0, SubStr( cString, nPos ), NIL )

   RETURN cVal

FUNCTION uhttpd_StrIStr( cString, cSearch )
   RETURN uhttpd_StrStr( Upper( cSearch ), Upper( cString ) )

FUNCTION uhttpd_HtmlEntities( cString, cQuote_style )

  LOCAL aTranslations := {}
  LOCAL i

  // ATTENTION, these chars are visible only with OEM font
  FOR i := 160 TO 255
      AAdd( aTranslations, { hb_BChar( i ), "&#" + Str( i, 3 ) + ";" } )
  NEXT

RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

PROCEDURE uhttpd_Die( cError )

   LOCAL oErr, lError

   IF cError != NIL // THEN OutStd( cError )
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
      lError := Eval( ErrorBlock(), oErr )
      IF ! HB_ISLOGICAL( lError ) .OR. lError
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

PROCEDURE uhttpd_WriteToLogFile( cString, cLog, lCreate )

   LOCAL nHandle, cSep

   cSep := hb_ps()

   // __defaultNIL( @cLog, AppFullPath() + cSep + "logfile.log" )
   __defaultNIL( @cLog, cSep + "tmp" + cSep + "logfile.log" )
   __defaultNIL( @lCreate, .F. )

   IF cLog != NIL

      IF ! lCreate .AND. hb_FileExists( cLog )
         nHandle := FOpen( cLog, FO_READWRITE + FO_SHARED )
      ELSE
         nHandle := hb_FCreate( cLog, FC_NORMAL, FO_READWRITE + FO_SHARED )
         // __OutDebug( "Create ", nHandle )
      ENDIF

      // cString := "PROCEDURE: " + ProcName( -2 ) + " " + cString

      IF nHandle != F_ERROR
         FSeek( nHandle, 0, FS_END )
         FWrite( nHandle, cString )
         FWrite( nHandle, CRLF )
         FClose( nHandle )
      ENDIF
   ENDIF

   RETURN

/*********************************************************************************/

FUNCTION uhttpd_SplitFileName( cFile )

   LOCAL hFile
   LOCAL cPath, cName, cExt, cDrive, cSep

   hb_FNameSplit( cFile, @cPath, @cName, @cExt, @cDrive )
   hFile := { ;
      "FILE"     => cFile, ;
      "DRIVE"    => cDrive, ;
      "PATH"     => cPath, ;
      "NAME"     => cName, ;
      "EXT"      => cExt, ;
      "FULLPATH" => NIL, ;
      "FULLNAME" => cName + cExt, ;
      "UNC"      => NIL }

   cSep := hb_ps()

   hFile:FULLPATH := iif( ! Empty( hFile:PATH ), iif( !( Right( hFile:PATH, Len( cSep ) ) == cSep ), hFile:PATH + cSep, hFile:PATH ), "" )
   hFile:UNC      := hFile:FULLPATH + hFile:FULLNAME

   RETURN hFile

FUNCTION uhttpd_AppFullPath()

   LOCAL hExeFile     := uhttpd_SplitFileName( hb_argv( 0 ) )
   LOCAL cPrgFullPath := hExeFile:FULLPATH
   LOCAL cPath, cSep

   cSep := hb_ps()

   IF Right( cPrgFullPath, Len( cSep ) ) == cSep
      cPath := Left( cPrgFullPath, Len( cPrgFullPath ) - Len( cSep ) )
   ELSE
      cPath := cPrgFullPath
   ENDIF

   RETURN cPath

FUNCTION uhttpd_AppFullName()

   LOCAL hExeFile     := uhttpd_SplitFileName( hb_argv( 0 ) )

   RETURN hExeFile:FULLNAME


FUNCTION uhttpd_CStrToVal( cExp, cType )

   IF ! HB_ISSTRING( cExp )
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDIF

   SWITCH cType
   CASE "C"
      RETURN cExp

   CASE "P"
      RETURN hb_HexToNum( cExp )

   CASE "D"
      IF cExp[ 3 ] >= "0" .AND. cExp[ 3 ] <= "9" .AND. cExp[ 5 ] >= "0" .AND. cExp[ 5 ] <= "9"
         RETURN hb_SToD( cExp )
      ELSE
         RETURN CToD( cExp )
      ENDIF

   CASE "L"
      RETURN iif( cExp[ 1 ] == "T" .OR. cExp[ 1 ] == "Y" .OR. cExp[ 2 ] == "T" .OR. cExp[ 2 ] == "Y", .T., .F. )

   CASE "N"
      RETURN Val( cExp )

   CASE "M"
      RETURN cExp

   CASE "U"
      RETURN NIL

#if 0
   CASE "A"
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

   CASE "B"
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

   CASE "O"
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
#endif

   OTHERWISE
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDSWITCH

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
