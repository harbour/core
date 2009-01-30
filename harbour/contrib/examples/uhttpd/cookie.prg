
#include "common.ch"
#include "hbclass.ch"

#command IF <lexpr> THEN <*statement*>  =>;
         IF (<lexpr>) ; <statement> ; END

#command IF <lexpr> THEN <statement1> ELSE <statement2> =>;
         IF (<lexpr>) ; <statement1> ; ELSE ; <statement2> ; END

MEMVAR _COOKIE

FUNCTION uhttpd_CookieNew( cDomain, cPath, nExpireDays, nExpireSecs )
RETURN uhttpd_Cookie():New( cDomain, cPath, nExpireDays, nExpireSecs )

CLASS uhttpd_Cookie

   // Data for cookies
   DATA cCookieDomain
   DATA cCookiePath        INIT "/"
   DATA cCookieExpire
   DATA nCookieExpireDays  INIT 0
   DATA nCookieExpireSecs  INIT 7200       // 1 hour  - TODO set environment constant
   DATA lCookiesSent       INIT FALSE

   METHOD SetCookie()
   METHOD DeleteCookie()
   METHOD DeleteAllCookies()
   METHOD GetCookie()
   METHOD IsCookie( cCookieName )  INLINE ::GetCookie( cCookieName ) != NIL
   METHOD IsCookies()      INLINE !Empty( ::aaCookieToSet )
   METHOD SetCookieDefaults()

ENDCLASS

// ------------------------------ ***************************** -----------------------------------

METHOD SetCookieDefaults( cDomain, cPath, nExpireDays, nExpireSecs ) CLASS uhttpd_Cookie
   IF cDomain     <> NIL THEN ::cCookieDomain := cDomain
   IF cPath       <> NIL THEN ::cCookiePath   := cPath
   IF nExpireDays <> NIL THEN ::nCookieExpireDays := nExpireDays
   IF nExpireSecs <> NIL THEN ::nCookieExpireSecs := nExpireSecs
RETURN NIL

METHOD SetCookie( cCookieName, xValue, cDomain, cPath, cExpires, lSecure ) CLASS uhttpd_Cookie
   LOCAL cStr

   DEFAULT cDomain      TO ::cCookieDomain
   DEFAULT cPath        TO ::cCookiePath
   DEFAULT cExpires     TO uhttpd_DateToGMT( Date(), Time(), ::nCookieExpireDays, ::nCookieExpireSecs )

   cStr := cCookieName + "=" + uhttpd_UrlEncode( hb_cStr( xValue ) )

   IF cDomain <> NIL
      cStr += "; domain=" + cDomain
   ENDIF
   IF cPath <> NIL
      cStr += "; path=" + cPath
   ENDIF
   IF cExpires <> NIL
      cStr += "; expires=" + cExpires
   ENDIF
   IF ValType( lSecure ) == "L" .AND. lSecure
       cStr += "; secure"
   ENDIF

   // Send the header
   uhttpd_AddHeader( "Set-Cookie", cStr, FALSE )

   RETURN NIL

METHOD DeleteCookie( cCookieName, cDomain, cPath, lSecure ) CLASS uhttpd_Cookie
   LOCAL cExpires := uhttpd_DateToGMT( DATE() - 1 ) // Setting date in the past delete cookie

   ::SetCookie( cCookieName, "", cDomain, cPath, cExpires, lSecure )

   RETURN NIL

METHOD DeleteAllCookies( cDomain, cPath, lSecure ) CLASS uhttpd_Cookie
   LOCAL cCookieName

   FOR EACH cCookieName IN _COOKIE:Keys
       //::DeleteCookie( Substr( cCookieName, 2 ), cDomain, cPath, lSecure )
       ::DeleteCookie( cCookieName, cDomain, cPath, lSecure )
   NEXT

   RETURN NIL

METHOD GetCookie( cCookieName ) CLASS uhttpd_Cookie
   LOCAL cHeader, cRet
   LOCAL nPos := 1
   DO WHILE TRUE
       IF ( cHeader := uhttpd_GetHeader( "Set-Cookie", @nPos ) ) != NIL
          IF cHeader == cCookieName
             cRet := cHeader
             EXIT
          ELSE
             nPos++
          ENDIF
       ELSE
          EXIT
       ENDIF
   ENDDO
   RETURN cRet
