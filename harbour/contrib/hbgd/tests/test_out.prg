/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * CGI test application
 */

#require "hbgd"

#include "gd.ch"

#command WRITE <c> => FWrite( 1, <c> + hb_eol() )
#command OutHTML <c> => WRITE <c>

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

         DO CASE
         CASE cPar == "txt"
            // cText := hb_HGet( hParams, cPar )

         CASE cPar == "img"
            cImg := hb_HGet( hParams, cPar )

         CASE cPar == "photo"
            cPhoto := hb_HGet( hParams, cPar )

         CASE cPar == "width"
            nWidth := Val( hb_HGet( hParams, cPar ) )

         CASE cPar == "height"
            nHeight := Val( hb_HGet( hParams, cPar ) )

         CASE cPar == "pt"
            nPt := Val( hb_HGet( hParams, cPar ) )

         ENDCASE
      NEXT
   ENDIF

   // __OutDebug( cQuery, ValToPrg( hParams ) )

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
      OutHTML ValToPrg( hParams ) + "<br>"
      OutHTML ValToPrg( cParams ) + "<br>"
      OutHTML ValToPrg( cQuery ) + "<br>"
      OutHTML "<img src='test_out.exe?img=" + cPhoto + "&width=" + hb_ntos( nWidth ) + "&height=" + hb_ntos( nHeight ) + "'>" + "<br>"
#endif
      OutHTML "<table border=1>"
      OutHTML "<tr><td align='center'>"
      OutHTML "<img src='test_out.exe?img=" + cPhoto + "'>" + "<br>"
      OutHTML "</td></tr>"
      OutHTML "<tr><td align='center'>"
      OutHTML "<img src='test_out.exe?img=" + cPhoto + ;
         iif( nWidth != NIL, "&width="  + hb_ntos( nWidth ), "" ) + ;
         iif( nHeight != NIL, "&height=" + hb_ntos( nHeight ), "" ) + ;
         "'>" + "<br>"
      OutHTML "</td></tr>"
      OutHTML "<tr><td align='center'>"
      OutHTML cPhoto
      OutHTML "</td></tr>"
      OutHTML "</table>"
      OutHTML "<br>"
#if 0
      OutHTML "<img src='test_out.exe?img=" + cText + "_2&pt=" + hb_ntos( nPt ) + "'>" + "<br>"
      OutHTML OS() + "<br>"
#endif
      EndHTML()
   ELSE
      StartHTML()
      EndHTML()
   ENDIF

   RETURN

PROCEDURE StartHTML( cTitle )

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

PROCEDURE EndHTML()

   WRITE "</body>"
   WRITE "</html>"

   RETURN

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

PROCEDURE OutJpg( cText, nPitch )

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

   oI:SetFontName( "Verdana" ) // TOFIX
   oI:SetFontPitch( nPitch )
#endif
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
   oI:SetFontName( "Verdana" ) // TOFIX
   oI:SetFontPitch( nPitch )
   oI:SayFreeType( 0 - nX, 0 + nHeight - nY, cText, , , 0, blue )
#endif
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

FUNCTION GetVars( cFields, cSeparator )

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

      // Tracelog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF SubStr( cName, Len( cName ) - 1 ) == "[]"
         cName := SubStr( cName, 1, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }

      ELSE

         hHashVars[ cName ] := xValue

      ENDIF
      // Tracelog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   // __OutDebug( hHashVars )

   RETURN hHashVars

FUNCTION GetParams( aParams )

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

      // Tracelog( "cName, xValue", cName, xValue )

      // is it an array entry?
      IF SubStr( cName, Len( cName ) - 1 ) == "[]"
         cName := SubStr( cName, 1, Len( cName ) - 2 )

         hHashVars[ cName ] := { xValue }

      ELSE

         hHashVars[ cName ] := xValue

      ENDIF
      // Tracelog( "hHashVars, cName, xValue", DumpValue( hHashVars ), cName, xValue )
   NEXT
   // __OutDebug( hHashVars )

   RETURN hHashVars

// ***********************************************************
// Decoding URL
// Can return both a string or a number
//

FUNCTION URLDecode( cStr )

   LOCAL cRet := "", i, cCar

   // LOCAL lNumeric := .T.

   FOR i := 1 TO Len( cStr )
      cCar := cStr[ i ]

      DO CASE
      CASE cCar == "+"
         cRet += " "

      CASE cCar == "%"
         i++
         cRet += Chr( hb_HexToNum( SubStr( cStr, i, 2 ) ) )
         i++

      OTHERWISE
         cRet += cCar

      ENDCASE

      // IF ( cRet[ i ] > "9" .or. cRet[ i ] < "0" ) .AND. !( cRet[ i ] == "." )
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
      cCar := cStr[ i ]
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
