#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

PROCEDURE Main()

   LOCAL cURL
   LOCAL oHttp

   ? "URL:", cURL := iif( tip_SSL(), "https://", "http://" ) + "www.google.com"

   oHttp := TIPClientHTTP():New( cURL, .T. )
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
