/*
 * Copyright 2026 Aleksander Czajczynski (hb fki.pl)
 */

#require "hbzebra"

#define WIDTH     10

REQUEST HB_GT_CGI_DEFAULT

/* Demo how to use hbzebra to "prepend" a QR-Code to PCL printing pipeline */

PROCEDURE Main( nDpi, nX, nY, cCode, cLabel, cFile, ... ) // pclqr 300 1 1 Harbour Label harbour.pcl
   LOCAL n

   IF HB_ISSTRING( nDpi )
      nDpi := Val( nDpi )
   ELSE
      nDpi := 300
   ENDIF

   IF HB_ISSTRING( nX )
      nX := Val( nX )
   ENDIF

   IF HB_ISSTRING( nY )
      nY := Val( nY )
   ENDIF

   IF PCount() > 6
      FOR n := 7 TO PCount()
         cFile += " " + hb_PValue( n )
      NEXT
   ENDIF

   IF Empty( cCode )
      cCode := "https://harbour.github.io/"
   ENDIF

   DrawBarcodePCL( WIDTH, "QRCODE",, nDpi, nX, nY, cCode, cLabel, cFile )

   RETURN

STATIC PROCEDURE DrawBarcodePCL( nLineWidth, cType, nFlags, nDpi, nX, nY, cCode, cLabel, cFile )

   LOCAL hZebra, cBitMap, nXPad, nYPad, ;
      nLineHeight, nWidth, nHeight

   /* this sample prepends or writes single graphics to PCL printfile
     TODO: DEMO mode with a whole PCL page of different barcodes */

   SWITCH cType
/*
   CASE "EAN13"      ; hZebra := hb_zebra_create_ean13( cCode, nFlags )   ; EXIT
   CASE "EAN8"       ; hZebra := hb_zebra_create_ean8( cCode, nFlags )    ; EXIT
   CASE "UPCA"       ; hZebra := hb_zebra_create_upca( cCode, nFlags )    ; EXIT
   CASE "UPCE"       ; hZebra := hb_zebra_create_upce( cCode, nFlags )    ; EXIT
   CASE "CODE39"     ; hZebra := hb_zebra_create_code39( cCode, nFlags )  ; EXIT
   CASE "ITF"        ; hZebra := hb_zebra_create_itf( cCode, nFlags )     ; EXIT
   CASE "MSI"        ; hZebra := hb_zebra_create_msi( cCode, nFlags )     ; EXIT
   CASE "CODABAR"    ; hZebra := hb_zebra_create_codabar( cCode, nFlags ) ; EXIT
   CASE "CODE93"     ; hZebra := hb_zebra_create_code93( cCode, nFlags )  ; EXIT
   CASE "CODE11"     ; hZebra := hb_zebra_create_code11( cCode, nFlags )  ; EXIT
   CASE "CODE128"    ; hZebra := hb_zebra_create_code128( cCode, nFlags ) ; EXIT
   CASE "PDF417"     ; hZebra := hb_zebra_create_pdf417( cCode, nFlags ); nLineHeight := nLineWidth * 3 ; EXIT
   CASE "DATAMATRIX" ; hZebra := hb_zebra_create_datamatrix( cCode, nFlags ); nLineHeight := nLineWidth ; EXIT
*/
   CASE "QRCODE"     ; hZebra := hb_zebra_create_qrcode( cCode, nFlags ); nLineHeight := nLineWidth ; EXIT
   ENDSWITCH

   IF hZebra != NIL
      IF hb_zebra_geterror( hZebra ) == 0

         IF nLineHeight == NIL
            nLineHeight := nLineWidth * 36
         ENDIF

         nXPad := nYPad := 1
         /* get barcode size and add 1 pixel border over it */
         hb_zebra_getsize( hZebra, @nWidth, @nHeight )
         nWidth :=  nWidth * nLineWidth + nYPad + nYPad
         nHeight := nHeight * nLineHeight + nXPad + nXPad

         /* get bitmap data */
         cBitMap := hb_zebra_getbitmap( hZebra, , .F. /*<lBottomUp>*/, @nWidth, @nHeight, nLineWidth, nLineHeight, nX )

         /* produce a file with PCL raster mode commands */
         PclWriteBitmap( cBitMap, nWidth, nHeight, nDpi, nX, nY, cLabel, cFile, .T. )

      ELSE
         ? "Type", cType, "Code", cCode, "Error", hb_zebra_geterror( hZebra )
      ENDIF
      hb_zebra_destroy( hZebra )
   ELSE
      ? "Invalid barcode type", cType
   ENDIF

   RETURN

/* Write a 1bpp bitmap buffer as PCL5 raster graphics */

PROCEDURE PclWriteBitmap( cBitMap, nWidth, nHeight, nDpi, nX, nY, cLabel, cFile )

   LOCAL hFile, cContents, lPrepend := .T.
   LOCAL nRow, nBytesPerRow, nPos, cRow

   IF Empty( cFile )
      cFile := "output.pcl"
      lPrepend := .F.
   ENDIF

   IF ! Empty( lPrepend ) .AND. File( cFile )
      cContents := hb_MemoRead( cFile )
   ENDIF

   hFile := FCreate( cFile )
   IF hFile < 0
      ? "Cannot create file:", cFile
      RETURN
   ENDIF

   nBytesPerRow := Int( ( nWidth + 7 ) / 8 )

   /* PCL Job Header */

// FWrite( hFile, Chr(27) + "E" )          // Printer reset
// FWrite( hFile, Chr(27) + "&l0O" )       // Portrait
   FWrite( hFile, Chr( 27 ) + "*t" + hb_ntos( nDpi ) + "R" )  // Raster resolution

   /* Optional: move cursor x*1 inch from top-left */
   IF ! Empty( nX )
      FWrite( hFile, Chr( 27 ) + "*p" + hb_ntos( Int( nDpi * nX ) ) + "X" ) // offset
   ENDIF

   IF ! Empty( nY )
      FWrite( hFile, Chr( 27 ) + "*p" + hb_ntos( Int( nDpi * nY ) ) + "Y" ) // offset
   ENDIF

   FWrite( hFile, Chr( 27 ) + "*r1U" )  // Presentation mode
   FWrite( hFile, Chr( 27 ) + "*r0F" )  // Set compression mode to 0 (unencoded)

   /* Enter raster graphics mode */
   IF Empty( nX )
      FWrite( hFile, Chr( 27 ) + "*r0A" )  // Start raster graphics (starts at left)
   ELSE
      FWrite( hFile, Chr( 27 ) + "*r1A" )  // Start raster graphics
   ENDIF

   FWrite( hFile, Chr( 27 ) + "*r" + hb_ntos( nHeight ) + "T" )  // Declare number of rows

   /* Send each row */

   nPos := 1
   FOR nRow := 1 TO nHeight
      cRow := hb_BSubStr( cBitMap, nPos, nBytesPerRow )

      nPos += nBytesPerRow

      /* ESC *b#W   send # bytes of raster data */
      FWrite( hFile, Chr( 27 ) + "*b" + hb_ntos( nBytesPerRow ) + "W" )
      FWrite( hFile, cRow )
   NEXT

   /* End raster graphics */
   FWrite( hFile, Chr( 27 ) + "*rC" )

   IF ! Empty( cLabel ) .AND. ! cLabel == "."
      cLabel := StrTran( cLabel, "~", " " )
      FWrite( hFile, Chr( 10 ) )
      IF ! Empty( nX )
         FWrite( hFile, Chr( 27 ) + "*p" + hb_ntos( Int( nDpi * nX ) ) + "X" ) // offset
      ENDIF
      FWrite( hFile, cLabel )
   ENDIF

   FWrite( hFile, Chr( 27 ) + "*p0X" ) // offset
   FWrite( hFile, Chr( 27 ) + "*p0Y" ) // offset

   /* Form feed (optional) */
// FWrite( hFile, Chr(12) )

   IF ! Empty( cContents )
      FWrite( hFile, cContents )
   ENDIF

   FClose( hFile )

   RETURN
