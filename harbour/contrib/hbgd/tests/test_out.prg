/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * Windows CGI test application
 */

#include "gd.ch"

#command WRITE <c> => FWrite( 1, <c> + CHR(13)+CHR(10) )
#command OutHTML <c> => WRITE <c>

PROCEDURE Main(...)

   LOCAL cPar
   LOCAL aParams := hb_aParams()
   LOCAL cQuery  := GetEnv( "QUERY_STRING" )
   LOCAL hParams := { => }

   LOCAL cImg, nPt, nWidth, nHeight, cPhoto
   // LOCAL cText

   IF Empty( aParams )
      IF !Empty( cQuery )
          hParams := GetVars( cQuery )
      ENDIF
   ELSE
      hParams := GetParams( aParams )
   ENDIF

  //-----------------------------------------------------------------------------------------

  // Gestione parametri
  IF !Empty( hParams )
     FOR EACH cPar IN hParams:Keys

        do case
        case cPar == "txt"
             // cText := hb_hGet( hParams, cPar )

        case cPar == "img"
             cImg := hb_hGet( hParams, cPar )

        case cPar == "photo"
             cPhoto := hb_hGet( hParams, cPar )

        case cPar == "width"
             nWidth := Val( hb_hGet( hParams, cPar ) )

        case cPar == "height"
             nHeight := Val( hb_hGet( hParams, cPar ) )

        case cPar == "pt"
             nPt := Val( hb_hGet( hParams, cPar ) )

        endcase
     NEXT
  ENDIF

  //__OutDebug( cQuery, ValToPrg( hParams ) )

  //-----------------------------------------------------------------------------------------
   //hb_default( @cText, "Testo di Prova" )
   hb_default( @nPt, 30 )

   IF cImg != NIL
      //OutJpg( cImg, nPt )
      OutPhoto( cImg, nWidth, nHeight )

   ELSEIF cPhoto != NIL
      StartHTML()
      //OutHTML ValToPrg( hParams ) + "<br>"
      //OutHTML ValToPrg( cParams ) + "<br>"
      //OutHTML ValToPrg( cQuery ) + "<br>"
      //OutHTML "<img src='test_out.exe?img=" + cPhoto + "&width=" + AllTrim( Str( nWidth ) ) + "&height=" + AllTrim( Str( nHeight ) ) + "'>" + "<br>"
      OutHTML "<table border=1>"
      OutHTML "<tr><td align='center'>"
      OutHTML "<img src='test_out.exe?img=" + cPhoto + "'>" + "<br>"
      OutHTML "</td></tr>"
      OutHTML "<tr><td align='center'>"
      OutHTML "<img src='test_out.exe?img=" + cPhoto + ;
              IIF( nWidth != NIL , "&width="  + AllTrim( Str( nWidth ) ) , "" ) + ;
              IIF( nHeight != NIL, "&height=" + AllTrim( Str( nHeight ) ), "" ) + ;
              "'>" + "<br>"
      OutHTML "</td></tr>"
      OutHTML "<tr><td align='center'>"
      OutHTML cPhoto
      OutHTML "</td></tr>"
      OutHTML "</table>"
      OutHTML "<br>"
      //OutHTML "<img src='test_out.exe?img=" + cText + "_2&pt=" + AllTrim( Str( nPt ) ) + "'>" + "<br>"
      //OutHTML OS() + "<br>"
      //OutHTML IIF( OS_ISWINNT(), "WIN NT", "NON WIN NT" ) + "<br>"
      EndHTML()
   ELSE
      StartHTML()
      EndHTML()
   ENDIF

RETURN

PROCEDURE StartHTML( cTitle )

   hb_default( @cTitle, "" )

   WRITE 'content-type: text/html'
   WRITE 'Pragma: no-cache'
   WRITE CHR(13)+CHR(10)
   WRITE "<html>"
   WRITE "<head>"
   WRITE "<title>" + cTitle + "</title>"
   WRITE "</head>"
   WRITE "<body>"
RETURN

PROCEDURE EndHTML()
   WRITE "</body>"
   WRITE "</html>"
RETURN

   // per windows: SET GDFONTPATH=C:\windows\fonts
   // per linux  : export GDFONTPATH=/usr/share/fonts/default/TrueType

PROCEDURE OutPhoto( cPhoto, nWidth, nHeight )
   LOCAL cType

   LOCAL oImage := GDImage():LoadFromFile( cPhoto )

   IF nWidth != NIL .AND. nHeight != NIL
      oImage:Resize( nWidth, nHeight )
   ELSEIF nWidth != NIL .AND. nHeight == NIL
      nHeight := oImage:Height() * ( nWidth / oImage:Width() )
      oImage:Resize( nWidth, nHeight )
   ELSEIF nWidth == NIL .AND. nHeight != NIL
      nWidth := oImage:Width() * ( nHeight / oImage:Height() )
      oImage:Resize( nWidth, nHeight )
   ENDIF

   //__OutDebug( hb_dumpvar( oImage ) )

   WRITE 'content-type: ' + oImage:cMime + CHR(13)+CHR(10)
   cType := oImage:cType

   DO CASE
      CASE cType == "jpeg"
           oImage:OutputJpeg()
      CASE cType == "gif"
           oImage:OutputGif()
      CASE cType == "png"
           oImage:OutputPng()
   ENDCASE

   oImage := NIL
RETURN

PROCEDURE OutJpg( cText, nPitch )
   LOCAL cOS := OS()
   LOCAL cPath := IIF( Left( cOS, 10 ) == "Windows NT", "C:\winnt\fonts\", "C:\windows\fonts\" )
   LOCAL oI
   // LOCAL cyan
   LOCAL blue
   LOCAL aSize, nWidth, nHeight, nX, nY
   LOCAL cFont := cPath + "verdana.ttf"

   hb_default( @cText , "Sample TEXT" )
   hb_default( @nPitch, 30 )

   /* Create an image in memory */
   oI := GDImage( 400, 100 )

   /* Allocate background */
   // cyan  := oI:SetColor(0, 255, 255)

   /* Allocate drawing color */
   // blue := oI:SetColor(0, 0, 200)

   //oI:SetTransparent( blue )
   oI:SetFontName( cFont )
   oI:SetFontPitch( nPitch )
   //__OutDebug( oI:GetFTFontHeight() )
   aSize := oI:GetFTStringSize( cText )
   nWidth  := aSize[1]
   nHeight := aSize[2]
   nX      := aSize[3]
   nY      := aSize[4]
   oI:Resize( nWidth, nHeight )


   /* Allocate drawing color */
   blue := oI:SetColor(0, 0, 200)
   oI:SetFontName( cPath + "verdana.ttf" )
   oI:SetFontPitch( nPitch )
   oI:SayFreeType( 0 - nX, 0 + nHeight - nY, cText, , , 0, blue )
   //oI:SayFreeType( 0, 0, cText, , , 0, blue )

   //oI:Resize( nWidth, nHeight )
   //__OutDebug( "prima", oI:Width(), oI:Height() )
   //oI:Resize( 60, 40 )
   //__OutDebug( "dopo", oI:Width(), oI:Height() )

   //oI:SetFontLarge()
   //oI:SetColor( blue )
   //oI:Say( 0, 0, cText )

   WRITE 'content-type: image/jpeg' + CHR(13)+CHR(10)

   oI:OutputJpeg()

RETURN

FUNCTION GetVars( cFields, cSeparator )
   LOCAL hHashVars := { => }
   LOCAL aField, cField, aFields
   LOCAL cName, xValue

   hb_default( @cSeparator, "&" )

   aFields := HB_RegExSplit( cSeparator, cFields )

   FOR EACH cField in aFields
      aField := HB_RegexSplit( "=", cField, 2 )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[1] )
      xValue := UrlDecode( aField[2] )

      // Tracelog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF Substr( cName, Len( cName ) - 1 ) == "[]"
         cName := Substr( cName, 1, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }

      ELSE

         hHashVars[ cName ] := xValue

      ENDIF
      //Tracelog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   //__OutDebug( hHashVars )

RETURN hHashVars

FUNCTION GetParams( aParams )
   LOCAL hHashVars := { => }
   LOCAL aField, cField, aFields
   LOCAL cName, xValue

   aFields := aParams

   FOR EACH cField in aFields
      aField := HB_RegexSplit( "=", cField, 2 )
      IF Len( aField ) != 2
         LOOP
      ENDIF

      cName  := LTrim( aField[1] )
      xValue := UrlDecode( aField[2] )

      // Tracelog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF Substr( cName, Len( cName ) - 1 ) == "[]"
         cName := Substr( cName, 1, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }

      ELSE

         hHashVars[ cName ] := xValue

      ENDIF
      //Tracelog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   //__OutDebug( hHashVars )

RETURN hHashVars

************************************************************
* Decoding URL
* Can return both a string or a number
*
FUNCTION URLDecode( cStr )
   LOCAL cRet := "", i, cCar
   // LOCAL lNumeric := .T.

   FOR i := 1 TO Len( cStr )
      cCar := cStr[i]
      DO CASE

         CASE cCar == "+"
            cRet += " "

         CASE cCar == "%"
            i ++
            cRet += Chr( hb_HexToNum( SubStr( cStr, i, 2 ) ) )
            i ++

         OTHERWISE
            cRet += cCar

      ENDCASE

      // IF (cRet[i] > "9" .or. cRet[i] < "0") .and. cRet[i] != "."
      //    lNumeric := .F.
      // ENDIF
   NEXT

   // IF lNumeric
   //    cRet := Val( cRet )
   // ENDIF

RETURN cRet

FUNCTION URLEncode( cStr )
   LOCAL cRet := "", i, nVal, cCar

   FOR i := 1 TO Len( cStr )
      cCar := cStr[i]
      DO CASE

         CASE cCar == " "
              cRet += "+"

         CASE cCar >= "A" .and. cCar <= "Z"
            cRet += cCar

         CASE cCar >= "a" .and. cCar <= "z"
            cRet += cCar

         CASE cCar >= "0" .and. cCar <= "9"
            cRet += cCar

         OTHERWISE
            nVal := Asc( cCar )
            cRet += "%" + hb_NumToHex( nVal )
      ENDCASE
   NEXT

RETURN cRet
