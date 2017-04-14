/* Makes an internet search and displays the links from the response HTML page */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main()

   LOCAL cURL := iif( tip_SSL(), "https", "http" ) + "://duckduckgo.com/html/"
   LOCAL oHTTP := TIPClientHTTP():New( cURL )
   LOCAL cHtml, oNode, oDoc

   ? "URL:", cURL

   /* build the search query and add it to the TUrl object */
   oHTTP:oURL:addGetForm( { ;
      "q"  => "Harbour+Project", ;
      "kl" => "us-en" } )

   /* Connect to the HTTP server */
   IF ! oHTTP:Open()
      ? "Connection error:", oHTTP:lastErrorMessage()
      RETURN
   ENDIF

   /* download the response */
   cHtml := oHTTP:ReadAll()
   oHTTP:Close()
   ? hb_ntos( Len( cHtml ) ), "bytes received"
   ?

   oDoc := THtmlDocument():New( cHtml )

   oDoc:writeFile( "result.htm" )

   /* "aS" is the plural of "a" and returns all <a href="url"> tags */
   FOR EACH oNode IN oDoc:body:div( "links" ):aS
      IF oNode:class == "large"
         ? tip_HtmlToStr( oNode:getText( "" ) ), oNode:href
      ENDIF
   NEXT

   RETURN
