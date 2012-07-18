/*
 * $Id$
 */

/*
*
*  testhtml.prg
*  Harbour Test of a HTML-Generator class.
*
*  1999/05/30  First implementation.
*
*              Tips: - Use ShowResults to make dynamic html (to test dynamic
*                      results, put the exe file on CGI-BIN dir or equivalent);
*                    - Use SaveToFile to make static html page
*
**/

PROCEDURE Main()

   LOCAL oHTML := THTML():New()

   oHTML:SetTitle( "Harbour Power Demonstration" )
   oHTML:AddHead( "Harbour" )
   oHTML:AddPara( "<b>Harbour</b> is xBase at its best. Have a taste today!", "LEFT" )
   oHTML:AddPara( "<b>L i n k s</b>", "CENTER" )
   oHTML:AddLink( "http://harbour-project.org", "Meet the Harbour power!" )
   oHTML:Generate()

   // Uncomment the following if you don't have a Web Server to test
   // this sample

   // oHTML:SaveToFile( "test.htm" )

   // If the above is uncommented, you may comment this line:

   oHTML:ShowResult()

   RETURN

/*---------------------------------------------------------------------------*/

FUNCTION THTML()

   STATIC oClass

   IF oClass == NIL
      oClass := HBClass():New( "THTML" )

      oClass:AddData( "cTitle" )                       // Page Title
      oClass:AddData( "cBody" )                        // HTML Body Handler
      oClass:AddData( "cBGColor" )                     // Background Color
      oClass:AddData( "cLinkColor" )                   // Link Color
      oClass:AddData( "cvLinkColor" )                  // Visited Link Color
      oClass:AddData( "cContent" )                     // Page Content Handler

      oClass:AddMethod( "New",        @New() )         // New Method
      oClass:AddMethod( "SetTitle",   @SetTitle() )    // Set Page Title
      oClass:AddMethod( "AddHead",    @AddHead() )     // Add <H1> Header
      oClass:AddMethod( "AddLink",    @AddLink() )     // Add Hyperlink
      oClass:AddMethod( "AddPara",    @AddPara() )     // Add Paragraph
      oClass:AddMethod( "Generate",   @Generate() )    // Generate HTML
      oClass:AddMethod( "SaveToFile", @SaveToFile() )  // Saves Content to File
      oClass:AddMethod( "ShowResult", @ShowResult() )  // Show Result - SEE Fcn

      oClass:Create()
   ENDIF

   RETURN oClass:Instance()

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ::cTitle      := "Untitled"
   ::cBGColor    := "#FFFFFF"
   ::cLinkColor  := "#0000FF"
   ::cvLinkColor := "#FF0000"
   ::cContent    := ""
   ::cBody       := ""

   RETURN Self

STATIC FUNCTION SetTitle( cTitle )

   LOCAL Self := QSelf()

   ::cTitle := cTitle

   RETURN Self

STATIC FUNCTION AddLink( cLinkTo, cLinkName )

   LOCAL Self := QSelf()

   ::cBody := ::cBody + ;
      "<a href='" + cLinkTo + "'>" + cLinkName + "</a>"

   RETURN Self

STATIC FUNCTION AddHead( cDescr )

   LOCAL Self := QSelf()

   // Why this doesn't work?
   // ::cBody += ...
   // ???

   ::cBody := ::cBody + ;
      "<h1>" + cDescr + "</h1>"

   RETURN NIL

STATIC FUNCTION AddPara( cPara, cAlign )

   LOCAL Self := QSelf()

   cAlign := iif( cAlign == NIL, "Left", cAlign ) // Added Patrick Mast 2000-06-17

   ::cBody := ::cBody + ;
      "<p align='" + cAlign + "'>" + hb_eol() + ;
      cPara + hb_eol() + ;
      "</p>"

   RETURN Self

STATIC FUNCTION Generate()

   LOCAL Self := QSelf()

   ::cContent :=                                                           ;
      "<html><head>"                                          + hb_eol() + ;
      "<title>" + ::cTitle + "</title>"                       + hb_eol() + ;
      "<body link='" + ::cLinkColor + "' " +                               ;
      "vlink='" + ::cvLinkColor + "'>" +                      + hb_eol() + ;
      ::cBody                                                 + hb_eol() + ;
      "</body></html>"

   RETURN Self

STATIC FUNCTION ShowResult()

   LOCAL Self := QSelf()

   OutStd(                                                                  ;
;//      "HTTP/1.0 200 OK"                                     + hb_eol() + ;
      "CONTENT-TYPE: TEXT/HTML"                     + hb_eol() + hb_eol() + ;
      ::cContent )

   RETURN Self

STATIC FUNCTION SaveToFile( cFile )

   LOCAL Self  := QSelf()
   LOCAL hFile := FCreate( cFile )

   FWrite( hFile, ::cContent )
   FClose( hFile )

   RETURN Self
