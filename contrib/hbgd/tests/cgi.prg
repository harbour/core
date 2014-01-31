/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * CGI test application
 */

/* run with parameter like:
     photo=imgs_in/conv_tst.jpg
 */

#require "hbgd"

#command WRITE <c> => OutStd( <c> + hb_eol() )

PROCEDURE Main( ... )

   LOCAL cPar
   LOCAL aParams := hb_AParams()
   LOCAL cQuery  := GetEnv( "QUERY_STRING" )
   LOCAL hParams := { => }

   LOCAL cImg, nPt, nWidth, nHeight, cPhoto

#if 0
   LOCAL cText
#endif

   IF Empty( aParams )
      IF ! Empty( cQuery )
         hParams := GetVars( cQuery )
      ENDIF
   ELSE
      hParams := GetParams( aParams )
   ENDIF

   // Gestione parametri
   IF ! Empty( hParams )
      FOR EACH cPar IN hParams:Keys

         SWITCH cPar
         CASE "txt"
            // cText := hParams[ cPar ]
            EXIT
         CASE "img"
            cImg := hParams[ cPar ]
            EXIT
         CASE "photo"
            cPhoto := hParams[ cPar ]
            EXIT
         CASE "width"
            nWidth := Val( hParams[ cPar ] )
            EXIT
         CASE "height"
            nHeight := Val( hParams[ cPar ] )
            EXIT
         CASE "pt"
            nPt := Val( hParams[ cPar ] )
            EXIT
         ENDSWITCH
      NEXT
   ENDIF

   // __OutDebug( cQuery, hb_ValToExp( hParams ) )

#if 0
   hb_default( @cText, "Testo di Prova" )
#endif
   hb_default( @nPt, 30 )

   IF cImg != NIL
#if 0
      OutJpg( cImg, nPt )
#endif
      OutPhoto( cImg, nWidth, nHeight )

   ELSEIF cPhoto != NIL
      StartHTML()
#if 0
      WRITE hb_ValToExp( hParams ) + "<br />"
      WRITE hb_ValToExp( cParams ) + "<br />"
      WRITE hb_ValToExp( cQuery ) + "<br />"
      WRITE "<img src='test_out.exe?img=" + cPhoto + "&width=" + hb_ntos( nWidth ) + "&height=" + hb_ntos( nHeight ) + "'>" + "<br />"
#endif
      WRITE "<table border=1>"
      WRITE "<tr><td align='center'>"
      WRITE "<img src='test_out.exe?img=" + cPhoto + "'>" + "<br />"
      WRITE "</td></tr>"
      WRITE "<tr><td align='center'>"
      WRITE "<img src='test_out.exe?img=" + cPhoto + ;
         iif( HB_ISNUMERIC( nWidth ), "&width="  + hb_ntos( nWidth ), "" ) + ;
         iif( HB_ISNUMERIC( nHeight ), "&height=" + hb_ntos( nHeight ), "" ) + ;
         "'>" + "<br />"
      WRITE "</td></tr>"
      WRITE "<tr><td align='center'>"
      WRITE cPhoto
      WRITE "</td></tr>"
      WRITE "</table>"
      WRITE "<br />"
#if 0
      WRITE "<img src='test_out.exe?img=" + cText + "_2&pt=" + hb_ntos( nPt ) + "'>" + "<br />"
      WRITE OS() + "<br />"
#endif
      EndHTML()
   ELSE
      StartHTML()
      EndHTML()
   ENDIF

   RETURN

STATIC PROCEDURE StartHTML( cTitle )

   hb_default( @cTitle, "" )

   WRITE "content-type: text/html"
   WRITE "Pragma: no-cache"
   WRITE hb_eol()
   WRITE "<html>"
   WRITE "<head>"
   WRITE "<title>" + cTitle + "</title>"
   WRITE "</head>"
   WRITE "<body>"

   RETURN

STATIC PROCEDURE EndHTML()

   WRITE "</body>"
   WRITE "</html>"

   RETURN

STATIC PROCEDURE OutPhoto( cPhoto, nWidth, nHeight )

   LOCAL cType

   LOCAL oImage := GDImage():LoadFromFile( cPhoto )

   DO CASE
   CASE HB_ISNUMERIC( nWidth ) .AND. HB_ISNUMERIC( nHeight )
      oImage:Resize( nWidth, nHeight )
   CASE HB_ISNUMERIC( nWidth ) .AND. ! HB_ISNUMERIC( nHeight )
      nHeight := oImage:Height() * ( nWidth / oImage:Width() )
      oImage:Resize( nWidth, nHeight )
   CASE ! HB_ISNUMERIC( nWidth ) .AND. HB_ISNUMERIC( nHeight )
      nWidth := oImage:Width() * ( nHeight / oImage:Height() )
      oImage:Resize( nWidth, nHeight )
   ENDCASE

#if 0
   __OutDebug( hb_DumpVar( oImage ) )
#endif

   WRITE "content-type: " + oImage:cMime + hb_eol()
   cType := oImage:cType

   DO CASE
   CASE cType == "jpeg"
      oImage:OutputJpeg()
   CASE cType == "gif"
      oImage:OutputGif()
   CASE cType == "png"
      oImage:OutputPng()
   ENDCASE

   RETURN

STATIC PROCEDURE OutJpg( cText, nPitch )

   LOCAL oI

   // LOCAL cyan
   LOCAL blue
   LOCAL aSize, nWidth, nHeight, nX, nY

   hb_default( @cText, "Sample TEXT" )
   hb_default( @nPitch, 30 )

   /* Create an image in memory */
   oI := GDImage( 400, 100 )

#if 0
   /* Allocate background */
   cyan := oI:SetColor( 0, 255, 255 )

   /* Allocate drawing color */
   blue := oI:SetColor( 0, 0, 200 )

   oI:SetTransparent( blue )
#endif

   oI:SetFontName( "Arial" )
   oI:SetFontPitch( nPitch )
#if 0
   __OutDebug( oI:GetFTFontHeight() )
#endif
   aSize := oI:GetFTStringSize( cText )
   nWidth  := aSize[ 1 ]
   nHeight := aSize[ 2 ]
   nX      := aSize[ 3 ]
   nY      := aSize[ 4 ]
   oI:Resize( nWidth, nHeight )

   /* Allocate drawing color */
   blue := oI:SetColor( 0, 0, 200 )
   oI:SetFontName( "Arial" )
   oI:SetFontPitch( nPitch )
   oI:SayFreeType( 0 - nX, 0 + nHeight - nY, cText, , , 0, blue )
#if 0
   oI:SayFreeType( 0, 0, cText, , , 0, blue )

   oI:Resize( nWidth, nHeight )
   __OutDebug( "prima", oI:Width(), oI:Height() )
   oI:Resize( 60, 40 )
   __OutDebug( "dopo", oI:Width(), oI:Height() )

   oI:SetFontLarge()
   oI:SetColor( blue )
   oI:Say( 0, 0, cText )
#endif

   WRITE "content-type: image/jpeg" + hb_eol()

   oI:OutputJpeg()

   RETURN

STATIC FUNCTION GetVars( cFields, cSeparator )

   LOCAL hHashVars := { => }
   LOCAL aField, cField, aFields
   LOCAL cName, xValue

   hb_default( @cSeparator, "&" )

   aFields := hb_regexSplit( cSeparator, cFields )

   FOR EACH cField in aFields
      aField := hb_regexSplit( "=", cField, 2 )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[ 1 ] )
      xValue := UrlDecode( aField[ 2 ] )

      // TraceLog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF Right( cName, 2 ) == "[]"
         cName := Left( cName, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }
      ELSE
         hHashVars[ cName ] := xValue
      ENDIF
      // TraceLog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   // __OutDebug( hHashVars )

   RETURN hHashVars

STATIC FUNCTION GetParams( aParams )

   LOCAL hHashVars := { => }
   LOCAL aField, cField, aFields
   LOCAL cName, xValue

   aFields := aParams

   FOR EACH cField in aFields
      aField := hb_regexSplit( "=", cField, 2 )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[ 1 ] )
      xValue := UrlDecode( aField[ 2 ] )

      // TraceLog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF Right( cName, 2 ) == "[]"
         cName := Left( cName, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }
      ELSE
         hHashVars[ cName ] := xValue
      ENDIF
      // TraceLog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   // __OutDebug( hHashVars )

   RETURN hHashVars

//
// Decoding URL
// Can return both a string or a number
//

STATIC FUNCTION URLDecode( cStr )

   LOCAL cRet := "", i, cCar

#if 0
   LOCAL lNumeric := .T.
#endif

   FOR i := 1 TO Len( cStr )

      cCar := SubStr( cStr, i, 1 )

      DO CASE
      CASE cCar == "+"
         cRet += " "
      CASE cCar == "%"
         i++
         cRet += Chr( hb_HexToNum( SubStr( cStr, i++, 2 ) ) )
      OTHERWISE
         cRet += cCar
      ENDCASE

#if 0
      IF ( SubStr( cRet, i, 1 ) > "9" .OR. SubStr( cRet, i, 1 ) < "0" ) .AND. !( SubStr( cRet, i, 1 ) == "." )
         lNumeric := .F.
      ENDIF
#endif
   NEXT

#if 0
   IF lNumeric
      cRet := Val( cRet )
   ENDIF
#endif

   RETURN cRet

STATIC FUNCTION URLEncode( cStr )

   LOCAL cRet := "", i, nVal, cCar

   FOR i := 1 TO Len( cStr )

      cCar := SubStr( cStr, i, 1 )

      DO CASE
      CASE cCar == " "
         cRet += "+"
      CASE cCar >= "A" .AND. cCar <= "Z"
         cRet += cCar
      CASE cCar >= "a" .AND. cCar <= "z"
         cRet += cCar
      CASE cCar >= "0" .AND. cCar <= "9"
         cRet += cCar
      OTHERWISE
         nVal := Asc( cCar )
         cRet += "%" + hb_NumToHex( nVal )
      ENDCASE
   NEXT

   RETURN cRet
