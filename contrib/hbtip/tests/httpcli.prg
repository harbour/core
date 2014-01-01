#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

PROCEDURE Main()

   LOCAL oHttp := TIPClientHTTP():New( iif( tip_SSL(), "https://", "http://" ) + "www.google.com", .T. )

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
