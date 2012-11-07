/*
 * $Id$
 */

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
 *
 */

#include "error.ch"
#include "fileio.ch"

// #define HB_USE_HBTIP    // Use functions from HBTIP - TOIMPLEMENT

#define CRLF ( Chr( 13 ) + Chr( 10 ) )
#xtranslate THROW( <oErr> ) => ( Eval( ErrorBlock(), <oErr> ), Break( <oErr> ) )
#define HB_IHASH()   HB_HSETCASEMATCH( {=>}, .F. )

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
         cName := SubStr( cName, 1, Len( cName ) - 2 )
         hHashVars[ cName ] := { xValue }
      ELSE
         // now check if variable already exists. If yes and I have already another element
         // with same name, then I will change it to an array
         IF ( hb_HPos( hHashVars, cName ) ) > 0
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
   hb_HSetCaseMatch( hUrl, .F. )

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
      cTemp := SubStr( cTemp, 1, nPos - 1 )

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
      cTemp := SubStr( cTemp, 1, nPos - 1 )

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
      cTemp := SubStr( cTemp, 1, nPos - 1 )

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
   hb_HSet( hUrl, "SCHEME"  , cProto    )
   hb_HSet( hUrl, "HOST"    , cHost     )
   hb_HSet( hUrl, "PORT"    , nPort     )
   hb_HSet( hUrl, "USER"    , cUser     )
   hb_HSet( hUrl, "PASS"    , cPass     )
   hb_HSet( hUrl, "PATH"    , cPath     )
   hb_HSet( hUrl, "QUERY"   , cQuery    )
   hb_HSet( hUrl, "FRAGMENT", cFragment )
   hb_HSet( hUrl, "URI"     , cURI      )

   // Prevents externals to add something else to this Hash
   hb_HSetAutoAdd( hUrl, .F. )

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

   // WriteToLogFile( "Splitstring: " + cStr( cString ) )

   DO WHILE ( nEOLPos := At( cDelim, cBuffer ) ) > 0
      nHowMany++
      IF lRemDelim
         cLine := Left( cBuffer, nEOLPos - 1 )
      ELSE
         cLine := Left( cBuffer, ( nEOLPos + Len( cDelim ) ) - 1 )
      ENDIF
      // WriteToLogFile( "cBuffer, cDelim, nEOLPos, cLine: " + cStr( cBuffer ) + "," + cStr( cDelim ) + "," + cStr( nEOLPos ) + "," + cStr( cLine ) )
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
           cChar == '.' .OR. cChar == ',' .OR. cChar == '&' .OR. ;
           cChar == '/' .OR. cChar == ';' .OR. cChar == '_'
         cRet += cChar

      CASE iif( ! lComplete, cChar == ':' .OR. cChar == '?' .OR. cChar == '=', .F. )
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
 * dDate     : default DATE()
 * cTime     : default "00:00:00"
 * nDayToAdd : default 0 - may be a negative number
 *
 * cGMTDate  : The string return in form of "Saturday, 31-Oct-03 00:00:00 GMT"
 */

FUNCTION uhttpd_DateToGMT( dDate, cTime, nDayToAdd, nSecsToAdd )

   LOCAL cStr
   LOCAL cOldDateFormat := Set( _SET_DATEFORMAT, "dd-mm-yy" )
   LOCAL nDay, nMonth, nYear, nDoW
   LOCAL aDays   := { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" }
   LOCAL aMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }

   __defaultNIL( @dDate, Date() )
   __defaultNIL( @cTime, Time() )
   __defaultNIL( @nDayToAdd, 0 )
   __defaultNIL( @nSecsToAdd, 0 )

   // Tracelog( "DateToGMT - StartingValue", dDate, cTime, nDayToAdd, nSecsToAdd )

   cTime := uhttpd_AddSecondsToTime( cTime, nSecsToAdd, @nDayToAdd )
   dDate += nDayToAdd

   nDay   := Day( dDate )
   nMonth := Month( dDate )
   nYear  := Year( dDate )
   nDoW   := DoW( dDate )

   cStr := aDays[ nDow ] + ", " + StrZero( nDay, 2 ) + "-" + aMonths[ nMonth ] + "-" + ;
      Right( StrZero( nYear, 4 ), 2 ) + " " + cTime + " GMT"

   // Tracelog( "DateToGMT", cStr )

   Set( _SET_DATEFORMAT, cOldDateFormat )

   RETURN cStr

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

FUNCTION uhttpd_TimeDiffAsSeconds( dDateStart, dDateEnd, cTimeStart, cTimeEnd )

   LOCAL aRetVal

   __defaultNIL( @dDateEnd, Date() )
   __defaultNIL( @cTimeEnd, Time() )

   aRetVal := ft_Elapsed( dDateStart, dDateEnd, cTimeStart, cTimeEnd )

   RETURN aRetVal[ 4, 2 ]

FUNCTION uhttpd_OutputString( cString, aTranslate, lProtected )

   LOCAL cHtml

   __defaultNIL( @lProtected, .F. )
   __defaultNIL( @aTranslate, { { '"', '&quot;' }, { ' ', '&nbsp;' } } )

   // TraceLog( "OutputString( cString, aTranslate, lProtected )", cString, aTranslate, lProtected )
   IF lProtected
      cHtml := uhttpd_HtmlSpecialChars( cString )
   ELSE
      cHtml := uhttpd_TranslateStrings( cString, aTranslate )
   ENDIF
   // TraceLog( "OutputString() = cHtml", cHtml )

   RETURN cHtml

FUNCTION uhttpd_HtmlSpecialChars( cString, cQuote_style )

   LOCAL aTranslations := { ;
      { '&', '&amp;' }, ;
      { '<', '&lt;'  }, ;
      { '>', '&gt;'  }  ;
      }

   RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

FUNCTION uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

   __defaultNIL( @cQuote_style, "ENT_COMPAT" )
   DO CASE
   CASE cQuote_style == "ENT_COMPAT"
      AAdd( aTranslations, { '"', '&quot;'  } )
   CASE cQuote_style == "ENT_QUOTES"
      AAdd( aTranslations, { '"', '&quot;'  } )
      AAdd( aTranslations, { "'", '&#039;'  } )
   CASE cQuote_style == "ENT_NOQUOTES"
   ENDCASE

   RETURN uhttpd_TranslateStrings( cString, aTranslations )

FUNCTION uhttpd_CRLF2BR( cString )

   LOCAL aTranslations := { ;
      { CRLF, '<br />' } ;
      }

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
//  LOCAL aTranslations := { ;  // ATTENTION, this chars are visible only with OEM font
//                           { hb_BChar( 160 ), '&#160;' } ,; // &nbsp;   Nonbreaking space
//                           { hb_BChar( 161 ), '&#161;' } ,; // &iexcl;  Inverted exclamation
//                           { hb_BChar( 162 ), '&#162;' } ,; // &cent;   Cent sign
//                           { hb_BChar( 163 ), '&#163;' } ,; // &pound;  Pound sterling
//                           { hb_BChar( 164 ), '&#164;' } ,; // &curren; General currency sign
//                           { hb_BChar( 165 ), '&#165;' } ,; // &yen;    Yen sign
//                           { hb_BChar( 166 ), '&#166;' } ,; // &brvbar; or &brkbar; Broken vertical bar
//                           { hb_BChar( 167 ), '&#167;' } ,; // &sect;   Section sign
//                           { hb_BChar( 168 ), '&#168;' } ,; // &uml; or &die; Diaeresis / Umlaut
//                           { hb_BChar( 169 ), '&#169;' } ,; // &copy;   Copyright
//                           { hb_BChar( 170 ), '&#170;' } ,; // &ordf;   Feminine ordinal
//                           { hb_BChar( 171 ), '&#171;' } ,; // &laquo;  Left angle quote, guillemet left
//                           { hb_BChar( 172 ), '&#172;' } ,; // &not     Not sign
//                           { hb_BChar( 173 ), '&#173;' } ,; // &shy;    Soft hyphen
//                           { hb_BChar( 174 ), '&#174;' } ,; // &reg;    Registered trademark
//                           { hb_BChar( 175 ), '&#175;' } ,; // &macr; or &hibar; Macron accent
//                           { hb_BChar( 176 ), '&#176;' } ,; // &deg;    Degree sign
//                           { hb_BChar( 177 ), '&#177;' } ,; // &plusmn; Plus or minus
//                           { hb_BChar( 178 ), '&#178;' } ,; // &sup2;   Superscript two
//                           { hb_BChar( 179 ), '&#179;' } ,; // &sup3;   Superscript three
//                           { hb_BChar( 180 ), '&#180;' } ,; // &acute;  Acute accent
//                           { hb_BChar( 181 ), '&#181;' } ,; // &micro;  Micro sign
//                           { hb_BChar( 182 ), '&#182;' } ,; // &para;   Paragraph sign
//                           { hb_BChar( 183 ), '&#183;' } ,; // &middot; Middle dot
//                           { hb_BChar( 184 ), '&#184;' } ,; // &cedil;  Cedilla
//                           { hb_BChar( 185 ), '&#185;' } ,; // &sup1;   Superscript one
//                           { hb_BChar( 186 ), '&#186;' } ,; // &ordm;   Masculine ordinal
//                           { hb_BChar( 187 ), '&#187;' } ,; // &raquo;  Right angle quote, guillemet right
//                           { hb_BChar( 188 ), '&#188;' } ,; // &frac14; Fraction one-fourth
//                           { hb_BChar( 189 ), '&#189;' } ,; // &frac12; Fraction one-half
//                           { hb_BChar( 190 ), '&#190;' } ,; // &frac34; Fraction three-fourths
//                           { hb_BChar( 191 ), '&#191;' } ,; // &iquest; Inverted question mark
//                           { hb_BChar( 192 ), '&#192;' } ,; // &Agrave; Capital A, grave accent
//                           { hb_BChar( 193 ), '&#193;' } ,; // &Aacute; Capital A, acute accent
//                           { hb_BChar( 194 ), '&#194;' } ,; // &Acirc;  Capital A, circumflex
//                           { hb_BChar( 195 ), '&#195;' } ,; // &Atilde; Capital A, tilde
//                           { hb_BChar( 196 ), '&#196;' } ,; // &Auml;   Capital A, diaeresis / umlaut
//                           { hb_BChar( 197 ), '&#197;' } ,; // &Aring;  Capital A, ring
//                           { hb_BChar( 198 ), '&#198;' } ,; // &AElig;  Capital AE ligature
//                           { hb_BChar( 199 ), '&#199;' } ,; // &Ccedil; Capital C, cedilla
//                           { hb_BChar( 200 ), '&#200;' } ,; // &Egrave; Capital E, grave accent
//                           { hb_BChar( 201 ), '&#201;' } ,; // &Eacute; Capital E, acute accent
//                           { hb_BChar( 202 ), '&#202;' } ,; // &Ecirc;  Capital E, circumflex
//                           { hb_BChar( 203 ), '&#203;' } ,; // &Euml;   Capital E, diaeresis / umlaut
//                           { hb_BChar( 204 ), '&#204;' } ,; // &Igrave; Capital I, grave accent
//                           { hb_BChar( 205 ), '&#205;' } ,; // &Iacute; Capital I, acute accent
//                           { hb_BChar( 206 ), '&#206;' } ,; // &Icirc;  Capital I, circumflex
//                           { hb_BChar( 207 ), '&#207;' } ,; // &Iuml;   Capital I, diaeresis / umlaut
//                           { hb_BChar( 208 ), '&#208;' } ,; // &ETH;    Capital Eth, Icelandic
//                           { hb_BChar( 209 ), '&#209;' } ,; // &Ntilde; Capital N, tilde
//                           { hb_BChar( 210 ), '&#210;' } ,; // &Ograve; Capital O, grave accent
//                           { hb_BChar( 211 ), '&#211;' } ,; // &Oacute; Capital O, acute accent
//                           { hb_BChar( 212 ), '&#212;' } ,; // &Ocirc;  Capital O, circumflex
//                           { hb_BChar( 213 ), '&#213;' } ,; // &Otilde; Capital O, tilde
//                           { hb_BChar( 214 ), '&#214;' } ,; // &Ouml;   Capital O, diaeresis / umlaut
//                           { hb_BChar( 215 ), '&#215;' } ,; // &times;  Multiply sign
//                           { hb_BChar( 216 ), '&#216;' } ,; // &Oslash; Capital O, slash
//                           { hb_BChar( 217 ), '&#217;' } ,; // &Ugrave; Capital U, grave accent
//                           { hb_BChar( 218 ), '&#218;' } ,; // &Uacute; Capital U, acute accent
//                           { hb_BChar( 219 ), '&#219;' } ,; // &Ucirc;  Capital U, circumflex
//                           { hb_BChar( 220 ), '&#220;' } ,; // &Uuml;   Capital U, diaeresis / umlaut
//                           { hb_BChar( 221 ), '&#221;' } ,; // &Yacute; Capital Y, acute accent
//                           { hb_BChar( 222 ), '&#222;' } ,; // &THORN;  Capital Thorn, Icelandic
//                           { hb_BChar( 223 ), '&#223;' } ,; // &szlig;  Small sharp s, German sz
//                           { hb_BChar( 224 ), '&#224;' } ,; // &agrave; Small a, grave accent
//                           { hb_BChar( 225 ), '&#225;' } ,; // &aacute; Small a, acute accent
//                           { hb_BChar( 226 ), '&#226;' } ,; // &acirc;  Small a, circumflex
//                           { hb_BChar( 227 ), '&#227;' } ,; // &atilde; Small a, tilde
//                           { hb_BChar( 228 ), '&#228;' } ,; // &auml;   Small a, diaeresis / umlaut
//                           { hb_BChar( 229 ), '&#229;' } ,; // &aring;  Small a, ring
//                           { hb_BChar( 230 ), '&#230;' } ,; // &aelig;  Small ae ligature
//                           { hb_BChar( 231 ), '&#231;' } ,; // &ccedil; Small c, cedilla
//                           { hb_BChar( 232 ), '&#232;' } ,; // &egrave; Small e, grave accent
//                           { hb_BChar( 233 ), '&#233;' } ,; // &eacute; Small e, acute accent
//                           { hb_BChar( 234 ), '&#234;' } ,; // &ecirc;  Small e, circumflex
//                           { hb_BChar( 235 ), '&#235;' } ,; // &euml;   Small e, diaeresis / umlaut
//                           { hb_BChar( 236 ), '&#236;' } ,; // &igrave; Small i, grave accent
//                           { hb_BChar( 237 ), '&#237;' } ,; // &iacute; Small i, acute accent
//                           { hb_BChar( 238 ), '&#238;' } ,; // &icirc;  Small i, circumflex
//                           { hb_BChar( 239 ), '&#239;' } ,; // &iuml;   Small i, diaeresis / umlaut
//                           { hb_BChar( 240 ), '&#240;' } ,; // &eth;    Small eth, Icelandic
//                           { hb_BChar( 241 ), '&#241;' } ,; // &ntilde; Small n, tilde
//                           { hb_BChar( 242 ), '&#242;' } ,; // &ograve; Small o, grave accent
//                           { hb_BChar( 243 ), '&#243;' } ,; // &oacute; Small o, acute accent
//                           { hb_BChar( 244 ), '&#244;' } ,; // &ocirc;  Small o, circumflex
//                           { hb_BChar( 245 ), '&#245;' } ,; // &otilde; Small o, tilde
//                           { hb_BChar( 246 ), '&#246;' } ,; // &ouml;   Small o, diaeresis / umlaut
//                           { hb_BChar( 247 ), '&#247;' } ,; // &divide; Division sign
//                           { hb_BChar( 248 ), '&#248;' } ,; // &oslash; Small o, slash
//                           { hb_BChar( 249 ), '&#249;' } ,; // &ugrave; Small u, grave accent
//                           { hb_BChar( 250 ), '&#250;' } ,; // &uacute; Small u, acute accent
//                           { hb_BChar( 251 ), '&#251;' } ,; // &ucirc;  Small u, circumflex
//                           { hb_BChar( 252 ), '&#252;' } ,; // &uuml;   Small u, diaeresis / umlaut
//                           { hb_BChar( 253 ), '&#253;' } ,; // &yacute; Small y, acute accent
//                           { hb_BChar( 254 ), '&#254;' } ,; // &thorn;  Small thorn, Icelandic
//                           { hb_BChar( 255 ), '&#255;' }  ; // &yuml;   Small y, diaeresis / umlaut
//                         }

  LOCAL aTranslations := {}
  LOCAL i

  FOR i := 160 TO 255
      aAdd( aTranslations, { hb_BChar( i ), "&#" + Str( i, 3 ) + ";" } )
  NEXT

RETURN uhttpd_HtmlConvertChars( cString, cQuote_style, aTranslations )

PROCEDURE uhttpd_Die( cError )

   LOCAL oErr, lError

   IF cError != NIL // THEN OutStd( cError )
      // __OutDebug( "cError: ", cError )
      // IF ! oCGI:HeaderSent()
      //   oCGI:WriteLN( CRLF2BR( cError ), CRLF2BR( CRLF() ) )
      //   //oCGI:WriteLN( CRLF2BR( hb_dumpVar(TConfigure():hConfig) ) )
      // ENDIF
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

      IF nHandle > 0
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
      "UNC"      => NIL                    ;
      }

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
      cPath := SubStr( cPrgFullPath, 1, Len( cPrgFullPath ) - Len( cSep ) )
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
   CASE 'C'
      RETURN cExp

   CASE 'P'
      RETURN hb_HexToNum( cExp )

   CASE 'D'
      IF cExp[ 3 ] >= '0' .AND. cExp[ 3 ] <= '9' .AND. cExp[ 5 ] >= '0' .AND. cExp[ 5 ] <= '9'
         RETURN SToD( cExp )
      ELSE
         RETURN CToD( cExp )
      ENDIF

   CASE 'L'
      RETURN iif( cExp[ 1 ] == 'T' .OR. cExp[ 1 ] == 'Y' .OR. cExp[ 2 ] == 'T' .OR. cExp[ 2 ] == 'Y', .T., .F. )

   CASE 'N'
      RETURN Val( cExp )

   CASE 'M'
      RETURN cExp

   CASE 'U'
      RETURN NIL

      /*
      CASE 'A'
         Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

      CASE 'B'
         Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )

      CASE 'O'
         Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
      */

   OTHERWISE
      Throw( ErrorNew( "CSTR", 0, 3101, ProcName(), "Argument error", { cExp, cType } ) )
   ENDSWITCH

   RETURN NIL

FUNCTION uhttpd_GetField( cVar, cType )

   LOCAL xVal
   LOCAL nPos := hb_HPos( _Request, cVar )

   IF nPos > 0 // cVar IN ::h_Request:Keys
      xVal := hb_HValueAt( _Request, nPos ) // ::h_Request[ cVar ]
      IF Empty( xVal )
         xVal := NIL
      ENDIF
      IF cType != NIL .AND. cType $ "NLD"
         xVal := uhttpd_CStrToVal( xVal, cType )
      ENDIF
   ENDIF

   RETURN xVal

FUNCTION uhttpd_SetField( cVar, cVal )

   LOCAL xVal := uhttpd_HGetValue( _Request, cVar )

   _Request[ cVar ] := cVal

   RETURN xVal

FUNCTION uhttpd_HGetValue( hHash, cKey )

   LOCAL nPos
   LOCAL xVal

   IF hHash != NIL
      xVal := iif( ( nPos := hb_HPos( hHash, cKey ) ) == 0, NIL, hb_HValueAt( hHash, nPos ) )
   ENDIF
   // RETURN iif( cKey IN hHash:Keys, hHash[ cKey ], NIL )

   RETURN xVal
