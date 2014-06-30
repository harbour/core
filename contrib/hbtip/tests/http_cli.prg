#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main( cProxy )

   LOCAL cURL := iif( tip_SSL(), "https://", "http://" ) + "www.google.com"
   LOCAL oHTTP := TIPClientHTTP():New( cURL, .T. )
   LOCAL oProxy

   ? "URL:", cURL

   IF HB_ISSTRING( cProxy )
      oProxy := TUrl():New( cProxy )
      oHTTP:setProxy( oProxy:cServer, oProxy:nPort )
   ENDIF
   oHTTP:setCookie( "test01=value01" )

   IF oHTTP:Open()
      IF ! oHTTP:Post( "test" )
         ? "Error:", "oHTTP:Post()", oHTTP:lastErrorMessage()
      ENDIF
      oHTTP:Close()
   ELSE
      ? "Error:", "oHTTP:Open()", oHTTP:lastErrorMessage()
   ENDIF

   RETURN
