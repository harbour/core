/* Copyright 2010 Viktor Szakats */

/* Adapted to OS independent hb_gfxFilledRect() 2014 elch
   Choose a graphics-capable GT at link time.
   Notice that in GTWVT graphics is not persistent */

#require "hbzebra"

#include "hbgtinfo.ch"
#include "hbgfx.ch"

#define _SCALE_ 2  /* modifies y: font height, x: pixel */

PROCEDURE Main()

   SetMode( 21 * _SCALE_, 80 )
   SetColor( "B*/W" )  /* blink attribute for light white background in graphic GT */

   CLS

   DrawBarcode(  1, 32, 1, "EAN13",      "477012345678" )
   DrawBarcode(  2, 32, 1, "EAN8",       "1234567" )
   DrawBarcode(  3, 32, 1, "UPCA",       "01234567891" )
   DrawBarcode(  4, 32, 1, "UPCE",       "123456" )
   DrawBarcode(  5, 32, 1, "CODE39",     "ABC123" )
   DrawBarcode(  6, 32, 1, "CODE39",     "ABC123", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode(  7, 32, 1, "ITF",        "12345678901", HB_ZEBRA_FLAG_CHECKSUM )
   DrawBarcode(  8, 32, 1, "MSI",        "1234567", HB_ZEBRA_FLAG_CHECKSUM )
   DrawBarcode(  9, 32, 1, "CODABAR",    "40156", HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode( 10, 32, 1, "CODE93",     "ABC-123" )
   DrawBarcode( 11, 32, 1, "CODE11",     "1234567890", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
   DrawBarcode( 12, 32, 1, "CODE128",    "Code 128" )
   DrawBarcode( 13, 32, 1, "PDF417",     "Hello, World of Harbour!!! It's 2D barcode PDF417 :)" )
   DrawBarcode( 17, 32, 1, "DATAMATRIX", "Hello, World of Harbour!!! It's 2D barcode DataMatrix :)" )
   DrawBarcode( 19, 32, 1, "QRCODE",     "https://en.wikipedia.org/wiki/QR_Code" )

   WAIT

   RETURN

STATIC PROCEDURE DrawBarcode( nY, nX, nLineWidth, cType, cCode, nFlags )

   LOCAL hZebra, nLineHeight, cTxt, nYPixel
   LOCAL nColor := hb_gfxMakeColor( 0, 0, 0 )  /* RGB black */

   IF nLineWidth < 1  /* smaller as one pixel impossible */
      nLineWidth := 1
   ENDIF

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

   nY *= _SCALE_
   nYPixel := nY * hb_gtInfo( HB_GTI_FONTSIZE )
   nLineWidth *= _SCALE_

   IF hZebra != NIL
      IF hb_zebra_geterror( hZebra ) == 0
         IF nLineHeight == NIL
            nLineHeight := 16
         ENDIF
         hb_dispOutAt( nY, nX - 31, cType )
         cTxt := hb_zebra_getcode( hZebra )
         IF Len( cTxt ) > nX - 12
            cTxt := Left( cTxt, nX - 12 - 4 ) + "..."
         ENDIF
         hb_dispOutAt( nY, nx - 20, cTxt )
         hb_zebra_draw_gfx( hZebra, nColor, nX * hb_gtInfo( HB_GTI_FONTWIDTH ), nYPixel, nLineWidth, nLineHeight * _SCALE_ )
      ELSE
         ? "Type", cType, "Code", cCode, "Error", hb_zebra_geterror( hZebra )
      ENDIF
      hb_zebra_destroy( hZebra )
   ELSE
      ? "Invalid barcode type", cType
   ENDIF

   RETURN

STATIC FUNCTION hb_zebra_draw_gfx( hZebra, nColor, ... )

   IF hb_zebra_geterror( hZebra ) != 0
      RETURN HB_ZEBRA_ERROR_INVALIDZEBRA
   ENDIF

   RETURN hb_zebra_draw( hZebra, {| x, y, w, h | hb_gfxfilledRect( y, x, y + h, x + w, nColor ) }, ... )
