#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main( cProxy )

   LOCAL cURL := iif( tip_SSL(), "https", "http" ) + "://example.com/"
   LOCAL oHTTP := TIPClientHTTP():New( cURL, .T. )

   ? "Proxy:", cProxy
   ? "URL:", cURL

   IF HB_ISSTRING( cProxy )
      oHTTP:setProxy( TUrl():New( cProxy ) )
   ENDIF
   oHTTP:setCookie( "test01=value01" )

   IF oHTTP:Open()
      IF oHTTP:Post( "test" )
         ? oHTTP:cReply
         ? hb_ValToExp( oHTTP:hHeaders )
      ELSE
         ? "Error:", "oHTTP:Post()", oHTTP:lastErrorMessage()
      ENDIF
      oHTTP:Close()
   ELSE
      ? "Error:", "oHTTP:Open()", oHTTP:lastErrorMessage()
   ENDIF

   RETURN
