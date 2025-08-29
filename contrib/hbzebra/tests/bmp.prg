/*
 * Copyright 2025 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 */

#require "hbzebra"
#require "hbbmp"

#define WIDTH     1

REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main()

   DrawBarcode( WIDTH, "EAN13",      "477012345678" )
   DrawBarcode( WIDTH, "EAN8",       "1234567" )
   DrawBarcode( WIDTH, "UPCA",       "01234567891" )
   DrawBarcode( WIDTH, "UPCE",       "123456" )
   DrawBarcode( WIDTH, "CODE39",     "ABC123" )
   DrawBarcode( WIDTH, "CODE39",     "ABC123", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode( WIDTH, "ITF",        "12345678901", HB_ZEBRA_FLAG_CHECKSUM )
   DrawBarcode( WIDTH, "MSI",        "1234567", HB_ZEBRA_FLAG_CHECKSUM )
   DrawBarcode( WIDTH, "CODABAR",    "40156", HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode( WIDTH, "CODE93",     "ABC-123" )
   DrawBarcode( WIDTH, "CODE11",     "1234567890", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode( WIDTH, "CODE128",    "Code 128" )
   DrawBarcode( WIDTH, "PDF417",     "Hello, World of Harbour!!! It's 2D barcode PDF417 :)" )
   DrawBarcode( WIDTH, "DATAMATRIX", "Hello, World of Harbour!!! It's 2D barcode DataMatrix :)" )
   DrawBarcode( WIDTH, "QRCODE",     "https://en.wikipedia.org/wiki/QR_Code" )

   ?

   RETURN

STATIC PROCEDURE DrawBarcode( nLineWidth, cType, cCode, nFlags )

   LOCAL hZebra, cFile, cBitMap, pBMP, ;
         nLineHeight, nX, nY, nWidth, nHeight, nDepth, nAlign, nColor

   SWITCH cType
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
   CASE "QRCODE"     ; hZebra := hb_zebra_create_qrcode( cCode, nFlags ); nLineHeight := nLineWidth ; EXIT
   ENDSWITCH

   IF hZebra != NIL
      IF hb_zebra_geterror( hZebra ) == 0

         IF nLineHeight == NIL
            nLineHeight := nLineWidth * 36
         ENDIF

         nAlign := 32
         nDepth := 1
         nX := nY := 1
         /* get barcode size and add 1 pixel border over it */
         hb_zebra_getsize( hZebra, @nWidth, @nHeight )
         nWidth :=  nWidth * nLineWidth + nY + nY
         nHeight := nHeight * nLineHeight + nX + nX

         ? cType, "code width", hb_ntos( nWidth ), "height", hb_ntos( nHeight )
         IF Empty( pBMP := hb_bmp_new( nWidth, nHeight, nDepth ) )
            ? "Cannot create BMP image"
         ELSE
            /* allocate white color (RGB) and use it to fill the background */
            nColor := hb_bmp_color( pBMP, 255, 255, 255 )
            hb_bmp_rect( pBMP, 0, 0, nWidth, nHeight, nColor )
            /* allocate black color for barcode drawing */
            nColor := hb_bmp_color( pBMP, 0, 0, 0 )
            ? "Building BMP with", cType, "code for vale:", hb_zebra_getcode( hZebra )
            hb_zebra_draw_bmp( hZebra, pBMP, nColor, nX, nY, nLineWidth, nLineHeight )
            cFile := Lower( cType ) + ".bmp"
            ? "Creating BMP file:", cFile
            IF ! hb_bmp_save( pBMP, cFile )
               ? "Cannot save BMP to file:", cFile
            ENDIF
            /* destroy BMP file */
            pBMP := NIL
         ENDIF

         /* and now much faster version */
         cBitMap := hb_zebra_getbitmap( hZebra, nAlign, .T./*lBottomUp>*/, @nWidth, @nHeight, nLineWidth, nLineHeight, nX )
         pBMP := hb_bmp_frombitmap( cBitMap, nAlign, nWidth, nHeight, nDepth, /*nDPI*/, /*aPalette*/, /*@nError*/ )
         cFile := Lower( cType ) + "b.bmp"
         ? "Creating BMP file:", cFile
         IF ! hb_bmp_save( pBMP, cFile )
            ? "Cannot save BMP to file:", cFile
         ENDIF
         /* destroy BMP file */
         pBMP := NIL

      ELSE
         ? "Type", cType, "Code", cCode, "Error", hb_zebra_geterror( hZebra )
      ENDIF
      hb_zebra_destroy( hZebra )
   ELSE
      ? "Invalid barcode type", cType
   ENDIF
   ?

   RETURN

STATIC FUNCTION hb_zebra_draw_bmp( hZebra, pBMP, nColor, ... )

   IF hb_zebra_geterror( hZebra ) != 0
      RETURN HB_ZEBRA_ERROR_INVALIDZEBRA
   ENDIF

   RETURN hb_zebra_draw( hZebra, {| x, y, w, h | hb_bmp_rect( pBMP, x, y, w, h, nColor ) }, ... )
