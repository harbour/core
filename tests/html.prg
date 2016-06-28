/* Harbour Test of a HTML-Generator class.
 *
 * Tips: - Use ShowResults to make dynamic html (to test dynamic
 *         results, put the exe file on CGI-BIN dir or equivalent);
 *       - Use SaveToFile to make static html page
 */

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oHTML := THtml():New()

   oHTML:SetTitle( "Harbour Power Demonstration" )
   oHTML:AddHead( "Harbour" )
   oHTML:AddPara( "<b>Harbour</b> is xBase at its best. Have a taste today!", "left" )
   oHTML:AddPara( "<b>Links</b>", "center" )
   oHTML:AddLink( "https://example.org", "Meet the Harbour power!" )
   oHTML:Generate()

#if 0
   // Uncomment the following if you don't have a Web Server to test
   // this sample
   oHTML:SaveToFile( "test.htm" )
#endif

   // If the above is uncommented, you may comment this line:
   oHTML:ShowResult()

   RETURN

CREATE CLASS THTML STATIC

   VAR cTitle      INIT "Untitled"        // Page Title
   VAR cBody       INIT ""                // HTML Body Handler
   VAR cBGColor    INIT "#FFFFFF"         // Background Color
   VAR cLinkColor  INIT "#0000FF"         // Link Color
   VAR cvLinkColor INIT "#FF0000"         // Visited Link Color
   VAR cContent    INIT ""                // Page Content Handler

   METHOD New()                           // New Method
   METHOD SetTitle( cTitle )              // Set Page Title
   METHOD AddLink( cLinkTo, cLinkName )   // Add <H1> Header
   METHOD AddHead( cDescr )               // Add Hyperlink
   METHOD AddPara( cPara, cAlign )        // Add Paragraph
   METHOD Generate()                      // Generate HTML
   METHOD ShowResult()                    // Saves Content to File
   METHOD SaveToFile( cFile )             // Show Result

ENDCLASS

METHOD New() CLASS THTML
   RETURN Self

METHOD SetTitle( cTitle ) CLASS THTML

   ::cTitle := cTitle

   RETURN Self

METHOD AddLink( cLinkTo, cLinkName ) CLASS THTML

   ::cBody += "<a href='" + cLinkTo + "'>" + cLinkName + "</a>"

   RETURN Self

METHOD AddHead( cDescr ) CLASS THTML

   // Why this doesn't work?
   // ::cBody += ...
   // ???

   ::cBody += "<h1>" + cDescr + "</h1>"

   RETURN Self

METHOD AddPara( cPara, cAlign ) CLASS THTML

   ::cBody += ;
      "<p align='" + hb_defaultValue( cAlign, "Left" ) + "'>" + hb_eol() + ;
      cPara + hb_eol() + ;
      "</p>"

   RETURN Self

METHOD Generate() CLASS THTML

   ::cContent := ;
      "<html><head>"                                          + hb_eol() + ;
      "<title>" + ::cTitle + "</title>"                       + hb_eol() + ;
      "<body link='" + ::cLinkColor + "' " +                               ;
      "vlink='" + ::cvLinkColor + "'>" +                      + hb_eol() + ;
      ::cBody                                                 + hb_eol() + ;
      "</body></html>"

   RETURN Self

METHOD ShowResult() CLASS THTML

   OutStd( ;
;//      "HTTP/1.0 200 OK"                                     + hb_eol() + ;
      "CONTENT-TYPE: TEXT/HTML"                     + hb_eol() + hb_eol() + ;
      ::cContent )

   RETURN Self

METHOD SaveToFile( cFile ) CLASS THTML

   hb_MemoWrit( cFile, ::cContent )

   RETURN Self
