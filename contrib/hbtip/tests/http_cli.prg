#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main( cProxy )

   LOCAL cURL
   LOCAL oHttp
   LOCAL oProxy

   ? "URL:", cURL := iif( tip_SSL(), "https://", "http://" ) + "www.google.com"

   oHttp := TIPClientHTTP():New( cURL, .T. )
   IF cProxy != NIL
      oProxy := TUrl():New( cProxy )
      oHttp:setProxy( oProxy:cServer, oProxy:nPort )
   ENDIF
   oHttp:setCookie( "test01=value01" )

   IF oHttp:open()
      IF ! oHttp:post( "test" )
         ? "Error: oHttp:post():", oHttp:lastErrorMessage()
      ENDIF
      oHttp:close()
   ELSE
      ? "Error: oHttp:open():", oHttp:lastErrorMessage()
   ENDIF

   RETURN
