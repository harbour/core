/*
 * Sends a query to Google and displays the links from the response HTML page
 */

#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

PROCEDURE Main()

   LOCAL oHttp, cHtml, hQuery, aLink, oNode, oDoc

   oHttp := TIPClientHTTP():New( iif( tip_SSL(), "https://", "http://" ) + "www.google.com/search" )

   /* build the Google query */
   hQUery := { => }
   hb_HCaseMatch( hQuery, .F. )

   hQuery[ "q" ]    := "Harbour"
   hQuery[ "hl" ]   := "en"
   hQuery[ "btnG" ] := "Google+Search"

   /* add query data to the TUrl object */
   oHttp:oUrl:addGetForm( hQuery )

   /* Connect to the HTTP server */
   IF ! oHttp:open()
      ? "Connection error:", oHttp:lastErrorMessage()
      RETURN
   ENDIF

   /* download the Google response */
   cHtml := oHttp:readAll()
   oHttp:close()
   ? hb_ntos( Len( cHtml ) ), "bytes received"

   oDoc := THtmlDocument():New( cHtml )

   oDoc:writeFile( "google.html" )

   /* ":a" retrieves the first <a href="url"> text </a> tag */
   oNode := oDoc:body:a
   ? oNode:getText( "" ), oNode:href

   /* ":divs(5)" returns the 5th <div> tag */
   oNode := oDoc:body:divs( 5 )

   /* "aS" is the plural of "a" and returns all <a href="url"> tags */
   aLink := oNode:aS

   FOR EACH oNode IN aLink
      ? tip_HtmlToStr( oNode:getText( "" ) ), oNode:href
   NEXT

   RETURN
