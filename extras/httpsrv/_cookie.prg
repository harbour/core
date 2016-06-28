/*
 * uHTTPD (Micro HTTP server) cookie functions
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

#include "hbclass.ch"

MEMVAR _COOKIE

FUNCTION uhttpd_CookieNew( cDomain, cPath, nExpireDays, nExpireSecs )
   RETURN uhttpd_Cookie():New( cDomain, cPath, nExpireDays, nExpireSecs )

CLASS uhttpd_Cookie

   // Data for cookies
   VAR aCookies           INIT {}  // Using an array to mantain order
   VAR cDomain            INIT ""
   VAR cPath              INIT "/"
   VAR cExpire
   VAR lSecure            INIT .F.
   VAR lHttpOnly
   VAR nExpireDays        INIT 0
   VAR nExpireSecs        INIT 120 * 60  // 2 hours - TODO set environment constant
   VAR lCookiesSent       INIT .F.

   METHOD SetCookie( cCookieName, xValue, cDomain, cPath, cExpires, lSecure, lHttpOnly )
   METHOD DeleteCookie( cCookieName, cDomain, cPath, lSecure )
   METHOD DeleteAllCookies( cDomain, cPath, lSecure )
   METHOD GetCookie( cCookieName )
   METHOD IsCookie( cCookieName )  INLINE ::GetCookie( cCookieName ) != NIL
   METHOD IsCookies()      INLINE ! Empty( ::aaCookieToSet )
   METHOD SetCookieDefaults( cDomain, cPath, nExpireDays, nExpireSecs )

ENDCLASS

// ---

METHOD PROCEDURE SetCookieDefaults( cDomain, cPath, nExpireDays, nExpireSecs ) CLASS uhttpd_Cookie

   IF HB_ISSTRING( cDomain )
      ::cDomain := cDomain
   ENDIF
   IF HB_ISSTRING( cPath )
      ::cPath := cPath
   ENDIF
   IF HB_ISNUMERIC( nExpireDays )
      ::nExpireDays := nExpireDays
   ENDIF
   IF HB_ISNUMERIC( nExpireSecs )
      ::nExpireSecs := nExpireSecs
   ENDIF

   RETURN

METHOD PROCEDURE SetCookie( cCookieName, xValue, cDomain, cPath, cExpires, lSecure, lHttpOnly ) CLASS uhttpd_Cookie

   LOCAL cStr, nPos, nCookies

   hb_default( @cDomain, ::cDomain )
   hb_default( @cPath, ::cPath )

   IF ! HB_ISSTRING( cExpires )
      cExpires := uhttpd_DateToGMT( , ::nExpireDays, ::nExpireSecs )
   ENDIF

   ::lHttpOnly := hb_defaultValue( lHttpOnly, .F. )

   IF xValue != NIL
      // Search if a cookie already exists
      // case sensitive
      IF ( nPos := AScan( ::aCookies, {| e | e[ 1 ] == cCookieName } ) ) > 0
         ::aCookies[ nPos ][ 2 ] := tip_URLEncode( hb_CStr( xValue ) )
      ELSE
         AAdd( ::aCookies, { cCookieName, tip_URLEncode( hb_CStr( xValue ) ) } )
      ENDIF
   ELSEIF ( nPos := AScan( ::aCookies, {| e | e[ 1 ] == cCookieName } ) ) > 0
      hb_ADel( ::aCookies, nPos, .T. )
   ENDIF

   // Rebuild cookie string as per RFC2616 (comma separated list)
   cStr     := ""
   nCookies := Len( ::aCookies )
   AEval( ::aCookies, {| e, i | cStr += e[ 1 ] + "=" + e[ 2 ] + iif( i < nCookies, ",", "" ) } )

   // cStr := cCookieName + "=" + tip_URLEncode( hb_CStr( xValue ) )

   IF ! Empty( cDomain )
      cStr += "; domain=" + cDomain
   ENDIF
   IF ! Empty( cPath )
      cStr += "; path=" + cPath
   ENDIF
   cStr += "; expires=" + cExpires
   IF hb_defaultValue( lSecure, .F. )
      cStr += "; secure"
   ENDIF

   // Send the header
   // uhttpd_SetHeader( "Set-Cookie", cStr, .F. )
   uhttpd_SetHeader( "Set-Cookie", cStr )

   RETURN

METHOD PROCEDURE DeleteCookie( cCookieName, cDomain, cPath, lSecure ) CLASS uhttpd_Cookie

   // Setting date in the past deletes cookie
   ::SetCookie( cCookieName, "", cDomain, cPath, uhttpd_DateToGMT( , -1 ), lSecure )

   RETURN

METHOD PROCEDURE DeleteAllCookies( cDomain, cPath, lSecure ) CLASS uhttpd_Cookie

   LOCAL cCookieName

   FOR EACH cCookieName IN _COOKIE:Keys
      // ::DeleteCookie( SubStr( cCookieName, 2 ), cDomain, cPath, lSecure )
      ::DeleteCookie( cCookieName, cDomain, cPath, lSecure )
   NEXT

   RETURN

METHOD GetCookie( cCookieName ) CLASS uhttpd_Cookie

   LOCAL cHeader

   IF ( cHeader := uhttpd_GetHeader( "Set-Cookie" ) ) != NIL .AND. ;
      cHeader == cCookieName

      RETURN cHeader
   ENDIF

   RETURN NIL
