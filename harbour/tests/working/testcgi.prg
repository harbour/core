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
*  1999/06/02  Dynamic TAG matching routines (inspired on Delphi).
*              First attempt to convert Delphi's ISAPI dll of WebSites'
*              Function List
*              (See http://www.flexsys-ci.com/harbour-project/functions.htm)
*  1999/06/11  List can be viewed online at
*              http://www.flexsys-ci.com/cgi-bin/testcgi.exe
*
**/

#include "CGI.ch"
#define IF_BUFFER 65535
#define NewLine   chr(10)+chr(13)

FUNCTION Main()

   LOCAL oHTML := THTML():New()
   LOCAL hFile, nPos, cString, cBuf, i, cTable, cLine

   oHTML:SetHTMLFile( "function.cfm" )

   hFile := fOpen( "list.txt", 0 )

   cString := space( IF_BUFFER )
   cBuf	   := ""
   cTable  := ""

   // Builds dynamic table replacement
   WHILE (nPos := fRead( hFile, @cString, IF_BUFFER )) > 0
      i := 1
      DO WHILE i <= nPos 

	 IF substr( cString, i, 1 ) = chr( 13 )
            i := i + 1
            cLine := cBuf
  	    cBuf  := ""

  	    IF left( cLine, 1 ) <> ';' 
               cTable += '<TR>' + chr(10)+chr(13) + ;
                 '<TD WIDTH="50%"><FONT SIZE="2" FACE="Tahoma">' +                 ;
                 ParseString( cLine, ';', 1 ) + '</FONT></TD>' + chr(10)+chr(13) + ;
                 '<TD WIDTH="16%">' +                                              ;
                 if( ParseString( cLine, ';', 2 ) = 'R',                           ;
                    '<CENTER><IMG SRC="images/purple-m.gif">',                     ;
                    '&nbsp' ) +                                                    ;
                 '</TD>' + chr(10)+chr(13) +                                       ;
                 '<TD WIDTH="16%">' +                                              ;
                 if( ParseString( cLine, ';', 2 ) = 'S',                           ;
                    '<CENTER><IMG SRC="images/purple-m.gif">',                     ;
                    '&nbsp' ) +                                                    ;
                 '</TD>' + chr(10)+chr(13) +                                       ;
                 '<TD WIDTH="16%">' +                                              ;
                 if( ParseString( cLine, ';', 2 ) = 'N',                           ;
                    '<CENTER><IMG SRC="images/purple-m.gif">',                     ;
                    '&nbsp' ) +                                                    ;
                 '</TD>' + chr(10)+chr(13) +                                       ;
                 '</TR>'
  	    ENDIF
	 ELSE
	    cBuf := cBuf + substr( cString, i, 1 )
	 ENDIF

	 i++
      ENDDO
   ENDDO

   fClose( hFile )

   oHTML:AddReplaceTag( "Functions", cTable )
   oHTML:Generate()

   // Uncomment the following if you don't have a Web Server to test
   // this sample

//   oHTML:SaveToFile( "test.htm" )

   // If the above is uncommented, you may comment this line:

   oHTML:ShowResult()

   RETURN( NIL )

FUNCTION ParseString( cString, cDelim, nRet )

   LOCAL cBuf, aElem, nPosFim, nSize, i

   nSize := len( cString ) - len( StrTran( cString, cDelim, '' ) ) + 1
   aElem := array( nSize )

   cBuf := cString
   i := 1
   FOR i := 1 TO nSize
      nPosFim := at( cDelim, cBuf )

      IF nPosFim > 0 
         aElem[i] := substr( cBuf, 1, nPosFim - 1 )
      ELSE
         aElem[i] := cBuf
      ENDIF

      cBuf := substr( cBuf, nPosFim + 1, len( cBuf ) )
         
   NEXT i

   RETURN( aElem[ nRet ] )

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
      oClass:AddData( "cHTMLFile" )
      oClass:AddData( "aReplaceTags" )

      oClass:AddMethod( "New",        @New() )         // New Method
      oClass:AddMethod( "SetTitle",   @SetTitle() )    // Set Page Title
      oClass:AddMethod( "AddHead",    @AddHead() )     // Add <H1> Header
      oClass:AddMethod( "AddLink",    @AddLink() )     // Add Hyperlink
      oClass:AddMethod( "AddPara",    @AddPara() )     // Add Paragraph
      oClass:AddMethod( "SaveToFile", @SaveToFile() )  // Saves Content to File
      oClass:AddMethod( "ShowResult", @ShowResult() )  // Show Result - SEE Fcn
      oClass:AddMethod( "Generate",   @Generate() )    // Generate HTML
      oClass:AddMethod( "SetHTMLFile",@SetHTMLFile() ) // Sets source HTML file

      oClass:AddMethod( "ProcessCGI",    @ProcessCGI() )
      oClass:AddMethod( "GetCGIParam",   @GetCGIParam() )
      oClass:AddMethod( "QueryFields",   @QueryFields() )
      oClass:AddMethod( "AddReplaceTag", @AddReplaceTag() )

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
   ::aReplaceTags := {}
   ::cHTMLFile    := ""

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
   LOCAL cFile, i, hFile, nPos, cRes
   LOCAL lFlag := .f.

   // Is this a meta file or hand generated script?
   IF empty( ::cHTMLFile )
      ::cContent :=                                                        ;
         "<HTML><HEAD>"                                        + NewLine + ;
         "<TITLE>" + ::cTitle + "</TITLE>"                     + NewLine + ;
         "<BODY link='" + ::cLinkColor + "' " +                            ;
         "vlink='" + ::cvLinkColor + "'>" +                    + NewLine + ;
         ::cBody                                               + NewLine + ;
         "</BODY></HTML>"
   ELSE
      ::cContent := ""

      // Does cHTMLFile exists?
      IF !File( ::cHTMLFile )
         ::cContent := "<H1>Server Error</H1><P><I>No such file: " + ;
           ::cHTMLFile
      ELSE
         // Read from file
         hFile := fOpen( ::cHTMLFile, 0 )
         cFile := space( IF_BUFFER )
         DO WHILE (nPos := fRead( hFile, @cFile, IF_BUFFER )) > 0

            cFile := left( cFile, nPos )
            cRes += cFile
            cFile := space( IF_BUFFER )

         ENDDO

         fClose( hFile )

         // Replace matched tags
         i := 1
         ::cContent := cRes
         /* TODO: Replace this DO WHILE with FOR..NEXT */
         DO WHILE i <= len( ::aReplaceTags )
            ::cContent := strtran( ::cContent, ;
               "<#" + ::aReplaceTags[i, 1] + ">", ::aReplaceTags[i, 2] )
            i++
         ENDDO

         /* TODO: Clear remaining (not matched) tags */
         /*
         cRes := ""
         FOR i := 1 TO len( ::cContent )
            IF substr( ::cContent, i, 1 ) == "<" .AND. ;
               substr( ::cContent, i + 1, 1 ) == "#"
               lFlag := .t.
            ELSEIF substr( ::cContent, i, 1 ) == ">" .AND. lFlag
               lFlag := .f.
            ELSEIF !lFlag
               cRes += substr( ::cContent, i, 1 )
            ENDIF
         NEXT i

         ::cContent := cRes
         */

      ENDIF
   ENDIF

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

STATIC FUNCTION SetHTMLFile( cFile )

   LOCAL Self := QSelf()

   ::cHTMLFile := cFile

   RETURN( Self )

STATIC FUNCTION AddReplaceTag( cTag, cReplaceText )

   LOCAL Self := QSelf()

   aAdd( ::aReplaceTags, { cTag, cReplaceText } )

   RETURN( Self )
