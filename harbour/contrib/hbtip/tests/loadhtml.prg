/*
 * $Id$
 */

/*
 * Sends a query to Google and displays the Links from the response HTML page
 */

PROCEDURE Main

   LOCAL oHttp, cHtml, hQuery, aLink, oNode, oDoc

   oHttp := TIpClientHttp():new( "http://www.google.de/search" )

   /* build the Google query */
   hQUery := hb_Hash()
   hb_hSetCaseMatch( hQuery, .F. )

   hQuery["q"]    := "Harbour"
   hQuery["hl"]   := "en"
   hQuery["btnG"] := "Google+Search"

   /* add query data to the TUrl object */
   oHttp:oUrl:addGetForm( hQuery )

   /* Connect to the HTTP server */
   IF ! oHttp:open()
      ? "Connection error:", oHttp:lastErrorMessage()
      QUIT
   ENDIF

   /* download the Google response */
   cHtml   := oHttp:readAll()
   oHttp:close()
   ? Len( cHtml ), "bytes received "

   oDoc := THtmlDocument():new( cHtml )

   oDoc:writeFile( "Google.html" )

   /* ":a" retrieves the first <a href="url"> text </a> tag */
   oNode := oDoc:body:a
   ? oNode:getText( "" ), oNode:href

   /* ":divs(5)" returns the 5th <div> tag */
   oNode := oDoc:body:divs( 5 )

   /* "aS" is the plural of "a" and returns all <a href="url"> tags */
   aLink := oNode:aS

   FOR EACH oNode IN aLink
      ? HtmlToOem( oNode:getText( "" ) ), oNode:href
   NEXT

   RETURN
