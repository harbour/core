/*
*
*  TestCGI.PRG
*  Harbour Test of a CGI/HTML-Generator class.
*
*  1999/05/30  First implementation.
*
*              Tips: - Use ShowResults to make dynamic html (to test dynamic
*                      results, put the exe file on CGI-BIN dir or equivalent);
*                    - Use SaveToFile to make static html page
*
*  1999/05/31  Initial CGI functionality.
*  1999/06/01  Translated %nn to correct chars.
*
**/

#include "CGI.ch"
#define NewLine chr(10)+chr(13)

FUNCTION Main()

   LOCAL oHTML := THTML():New()
   LOCAL cName := ""

   cName := oHTML:QueryFields( "NAME" )

   oHTML:SetTitle( "Harbour CGI Scripting Demo" )
   oHTML:AddHead( "Harbour CGI Scripting DEMO" )
   oHTML:AddPara( "<UL>"	                                   + NewLine + ;
                  "<LI>Name: "    + oHTML:QueryFields( "Name" )    + NewLine + ;
		  "<LI>Phone: "   + oHTML:QueryFields( "Phone" )   + NewLine + ;
		  "<LI>Address: " + oHTML:QueryFields( "Address" ) + NewLine + ;
                  "</UL><P><HR><SMALL><CENTER>Copyright &copy 1999 by Harbour Project" + ;
                  "<BR>Generated at: " + dtoc( date() ) + " - " + time(), "LEFT" )
   oHTML:Generate()

   // Uncomment the following if you don't have a Web Server to test
   // this sample

   // oHTML:SaveToFile( "test.htm" )

   // If the above is uncommented, you may comment this line:

   oHTML:ShowResult()

   RETURN( NIL )

FUNCTION Hex2Dec( cHex )

   LOCAL aHex := { { "0", 00 }, ;
                   { "1", 01 }, ;
                   { "2", 02 }, ;
                   { "3", 03 }, ;
                   { "4", 04 }, ;
                   { "5", 05 }, ;
                   { "6", 06 }, ;
                   { "7", 07 }, ;
                   { "8", 08 }, ;
                   { "9", 09 }, ;
                   { "A", 10 }, ;
                   { "B", 11 }, ;
                   { "C", 12 }, ;
                   { "D", 13 }, ;
                   { "E", 14 }, ;
                   { "F", 15 } }
   LOCAL nRet
   LOCAL nRes

   nRet := ascan( aHex, { |x| upper( x[1] ) = upper( left( cHex, 1 ) ) } )
   nRes := aHex[nRet, 2] * 16
   nRet := ascan( aHex, { |x| upper( x[1] ) = upper( right( cHex, 1 ) ) } )
   nRes += aHex[nRet, 2]

   RETURN( nRes )

/*-------------------------------------------------------------------------*/

FUNCTION THTML

   STATIC oClass

   IF oClass == NIL
      oClass = TClass():New( "THTML" )

      oClass:AddData( "cTitle" )                       // Page Title
      oClass:AddData( "cBody" )                        // HTML Body Handler
      oClass:AddData( "cBGColor" )                     // Background Color
      oClass:AddData( "cLinkColor" )                   // Link Color
      oClass:AddData( "cvLinkColor" )                  // Visited Link Color
      oClass:AddData( "cContent" )                     // Page Content Handler

      oClass:AddData( "aCGIContents" )
      oClass:AddData( "aQueryFields" )

      oClass:AddMethod( "New",        @New() )         // New Method
      oClass:AddMethod( "SetTitle",   @SetTitle() )    // Set Page Title
      oClass:AddMethod( "AddHead",    @AddHead() )     // Add <H1> Header
      oClass:AddMethod( "AddLink",    @AddLink() )     // Add Hyperlink
      oClass:AddMethod( "AddPara",    @AddPara() )     // Add Paragraph
      oClass:AddMethod( "SaveToFile", @SaveToFile() )  // Saves Content to File
      oClass:AddMethod( "ShowResult", @ShowResult() )  // Show Result - SEE Fcn
      oClass:AddMethod( "Generate",   @Generate() )    // Generate HTML

      oClass:AddMethod( "ProcessCGI",  @ProcessCGI() )
      oClass:AddMethod( "GetCGIParam", @GetCGIParam() )
      oClass:AddMethod( "QueryFields", @QueryFields() )

      oClass:Create()

   ENDIF

   RETURN( oClass:Instance() )

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ::cTitle       := "Untitled"
   ::cBGColor     := "#FFFFFF"
   ::cLinkColor   := "#0000FF"
   ::cvLinkColor  := "#FF0000"
   ::cContent     := ""
   ::cBody        := ""
   ::aCGIContents := {}
   ::aQueryFields := {}

   RETURN( Self )

STATIC FUNCTION SetTitle( cTitle )

   LOCAL Self := QSelf()

   ::cTitle := cTitle

   RETURN( Self )

STATIC FUNCTION AddLink( cLinkTo, cLinkName )

   LOCAL Self := QSelf()

   ::cBody := ::cBody + ;
      "<A HREF='" + cLinkTo + "'>" + cLinkName + "</A>"

   RETURN( Self )

STATIC FUNCTION AddHead( cDescr )

   LOCAL Self := QSelf()

   // Why this doesn't work?
   // ::cBody += ...
   // ???

   ::cBody := ::cBody + ;
      "<H1>" + cDescr + "</H1>"

   RETURN( NIL )

STATIC FUNCTION AddPara( cPara, cAlign )

   LOCAL Self := QSelf()

   ::cBody := ::cBody + ;
      "<P ALIGN='" + cAlign + "'>" + NewLine + ;
      cPara + NewLine + ;
      "</P>"

   RETURN( Self )

STATIC FUNCTION Generate()

   LOCAL Self := QSelf()

   ::cContent :=                                                           ;
      "<HTML><HEAD>"                                           + NewLine + ;
      "<TITLE>" + ::cTitle + "</TITLE>"                        + NewLine + ;
      "<BODY link='" + ::cLinkColor + "' " +                               ;
      "vlink='" + ::cvLinkColor + "'>" +                       + NewLine + ;
      ::cBody                                                  + NewLine + ;
      "</BODY></HTML>"

   RETURN( Self )

STATIC FUNCTION ShowResult()

   LOCAL Self := QSelf()

   qqOut(                                                                  ;
      "HTTP/1.0 200 OK"                                        + NewLine + ;
      "CONTENT-TYPE: TEXT/HTML"                      + NewLine + NewLine + ;
      ::cContent )

   RETURN( Self )

STATIC FUNCTION SaveToFile( cFile )

   LOCAL Self  := QSelf()
   LOCAL hFile := fCreate( cFile )

   fWrite( hFile, ::cContent )
   fClose( hFile )

   RETURN( Self )

STATIC FUNCTION ProcessCGI()

   LOCAL Self   := QSelf()
   LOCAL cQuery := ""
   LOCAL cBuff  := ""
   LOCAL nBuff  := 0
   LOCAL i

   IF empty( ::aCGIContents )
      ::aCGIContents := {               ;
         GetEnv( "SERVER_SOFTWARE"   ), ;
         GetEnv( "SERVER_NAME"       ), ;
         GetEnv( "GATEWAY_INTERFACE" ), ;
         GetEnv( "SERVER_PROTOCOL"   ), ;
         GetEnv( "SERVER_PORT"       ), ;
         GetEnv( "REQUEST_METHOD"    ), ;
         GetEnv( "HTTP_ACCEPT"       ), ;
         GetEnv( "HTTP_USER_AGENT"   ), ;
         GetEnv( "HTTP_REFERER"      ), ;
         GetEnv( "PATH_INFO"         ), ;
         GetEnv( "PATH_TRANSLATED"   ), ;
         GetEnv( "SCRIPT_NAME"       ), ;
         GetEnv( "QUERY_STRING"      ), ;
         GetEnv( "REMOTE_HOST"       ), ;
         GetEnv( "REMOTE_ADDR"       ), ;
         GetEnv( "REMOTE_USER"       ), ;
         GetEnv( "AUTH_TYPE"         ), ;
         GetEnv( "CONTENT_TYPE"      ), ;
         GetEnv( "CONTENT_LENGTH"    ), ;
         GetEnv( "ANNOTATION_SERVER" )  ;
          }

      cQuery := ::GetCGIParam( CGI_QUERY_STRING )

      IF !empty( cQuery )

        ::aQueryFields := {}

        FOR i := 1 TO len( cQuery ) + 1

          IF i > len( cQuery ) .OR. substr( cQuery, i, 1 ) == "&"

             aadd( ::aQueryFields,                          ;
                { substr( cBuff, 1, at( "=", cBuff ) - 1 ), ;
                  strtran( substr( cBuff, at( "=", cBuff ) + 1,      ;
                     len( cBuff ) - at( "=", cBuff ) + 1 ), "+", " " ) } )
             cBuff := ""
          ELSE
             IF substr( cQuery, i, 1 ) = "%"
                cBuff += chr( Hex2Dec( substr( cQuery, i + 1, 2 ) ) )
                nBuff := 3
             ENDIF

             IF nBuff = 0
                cBuff += substr( cQuery, i, 1 )
             ELSE
                nBuff--
             ENDIF
          ENDIF

        NEXT

      ENDIF

   ENDIF

   RETURN( Self )

STATIC FUNCTION GetCGIParam( nParam )

   LOCAL Self := QSelf()

   ::ProcessCGI()

   IF nParam > 20 .OR. nParam < 1
      outerr( "Invalid CGI parameter" )
      RETURN( NIL )
   ENDIF

   RETURN( ::aCGIContents[nParam] )

STATIC FUNCTION QueryFields( cQueryName )

   LOCAL Self := QSelf()
   LOCAL cRet := ""
   LOCAL nRet

   ::ProcessCGI()

   nRet := aScan( ::aQueryFields, ;
      { |x| upper( x[1] ) = upper( cQueryName ) } )

   IF nRet > 0
      cRet := ::aQueryFields[nRet, 2]
   ENDIF

   RETURN( cRet )
