/* Sends a query to Google and displays the links from the response HTML page */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main()

   LOCAL cURL := iif( tip_SSL(), "https://", "http://" ) + "www.google.com/search"
   LOCAL oHTTP := TIPClientHTTP():New( cURL )
   LOCAL cHtml, oNode, oDoc

   ? "URL:", cURL

   /* build the Google query and add it to the TUrl object */
   oHTTP:oURL:addGetForm( { ;
      "q"    => "Harbour", ;
      "hl"   => "en", ;
      "btnG" => "Google+Search" } )

   /* Connect to the HTTP server */
   IF ! oHTTP:Open()
      ? "Connection error:", oHTTP:lastErrorMessage()
      RETURN
   ENDIF

   /* download the Google response */
   cHtml := oHTTP:ReadAll()
   oHTTP:Close()
   ? hb_ntos( Len( cHtml ) ), "bytes received"

   oDoc := THtmlDocument():New( cHtml )

   oDoc:writeFile( "result.htm" )

   /* ":a" retrieves the first <a href="url"> text </a> tag */
   oNode := oDoc:body:a
   ? oNode:getText( "" ), oNode:href

   /* ":divs(5)" returns the 5th <div> tag */
   /* "aS" is the plural of "a" and returns all <a href="url"> tags */
   FOR EACH oNode IN oDoc:body:divs( 5 ):aS
      ? tip_HtmlToStr( oNode:getText( "" ) ), oNode:href
   NEXT

   RETURN
