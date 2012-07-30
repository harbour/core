/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL oHttp

   CLS

   oHttp := TIPClientHTTP():New( "http://www.google.com", .T. )

   oHttp:setCookie( "test01=value01" )

   IF ! oHttp:open()
      ? "Error: oHttp:open(): " + oHttp:lastErrorMessage()
      RETURN
   ENDIF

   IF ! oHttp:post( "test" )
      ? "Error: oHttp:post(): " + oHttp:lastErrorMessage()
   ENDIF

   oHttp:close()

   RETURN
