/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbzebra"
#require "hbwin"

#include "hbzebra.ch"
#include "hbwin.ch"

PROCEDURE Main()

   LOCAL hDC
   LOCAL pDEVMODE := __wapi_DEVMODE_New( "Microsoft XPS Document Writer" )
   LOCAL hOBJECT

   __wapi_DEVMODE_Set( pDEVMODE, { ;
      "dmPaperSize"   => WIN_DMPAPER_A4, ;
      "dmOrientation" => WIN_DMORIENT_PORTRAIT } )

   hDC := wapi_CreateDC( NIL, "Microsoft XPS Document Writer", NIL, pDEVMODE )
   IF ! Empty( hDC )

      wapi_SetMapMode( hDC, WIN_MM_TEXT )

      IF wapi_StartDoc( hDC, { "lpszDocName" => "(barcode test)" } ) > 0

         wapi_SetBkMode( hDC, WIN_TRANSPARENT )

         IF wapi_StartPage( hDC ) > 0

            hOBJECT := wapi_CreateFont( ;
               96, ;
               42, ;
               NIL, ;
               NIL, ;
               WIN_FW_NORMAL, ;
               .F., ;
               .F., ;
               NIL, ;
               WIN_ANSI_CHARSET, ;
               NIL, ;
               NIL, ;
               NIL, ;
               NIL, ;
               "Arial" )

            wapi_SelectObject( hDC, hOBJECT )

            DrawBarcode( hDC,  20,   1, "EAN13",      "477012345678" )
            DrawBarcode( hDC,  40,   1, "EAN8",       "1234567" )
            DrawBarcode( hDC,  60,   1, "UPCA",       "01234567891" )
            DrawBarcode( hDC,  80,   1, "UPCE",       "123456" )
            DrawBarcode( hDC, 100,   1, "CODE39",     "ABC123" )
            DrawBarcode( hDC, 120,   1, "CODE39",     "ABC123", HB_ZEBRA_FLAG_CHECKSUM )
            DrawBarcode( hDC, 140, 0.5, "CODE39",     "ABC123", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE2_5 )
            DrawBarcode( hDC, 160,   1, "CODE39",     "ABC123", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 180,   1, "ITF",        "1234", HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 200,   1, "ITF",        "12345678901", HB_ZEBRA_FLAG_CHECKSUM )
            DrawBarcode( hDC, 220,   1, "MSI",        "1234" )
            DrawBarcode( hDC, 240,   1, "MSI",        "1234", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 260,   1, "MSI",        "1234567", HB_ZEBRA_FLAG_CHECKSUM )
            DrawBarcode( hDC, 280,   1, "CODABAR",    "40156", HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 300,   1, "CODABAR",    "-1234", HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 320,   1, "CODE93",     "ABC-123" )
            DrawBarcode( hDC, 340,   1, "CODE93",     "TEST93" )
            DrawBarcode( hDC, 360,   1, "CODE11",     "12", HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 380,   1, "CODE11",     "1234567890", HB_ZEBRA_FLAG_CHECKSUM + HB_ZEBRA_FLAG_WIDE3 )
            DrawBarcode( hDC, 400,   1, "CODE128",    "Code 128" )
            DrawBarcode( hDC, 420,   1, "CODE128",    "1234567890" )
            DrawBarcode( hDC, 440,   1, "CODE128",    "Wikipedia" )
            DrawBarcode( hDC, 460,   1, "PDF417",     "Hello, World of Harbour!!! It's 2D barcode PDF417 :)" )
            DrawBarcode( hDC, 540,   1, "DATAMATRIX", "Hello, World of Harbour!!! It's 2D barcode DataMatrix :)" )
            DrawBarcode( hDC, 580,   1, "QRCODE",     "http://harbour-project.org/" )

            wapi_EndPage( hDC )
         ENDIF
         wapi_EndDoc( hDC )
      ENDIF
   ENDIF

   RETURN

#define _SCALE_ 7.2

PROCEDURE DrawBarcode( hDC, nY, nLineWidth, cType, cCode, nFlags )

   LOCAL hZebra, nLineHeight, cTxt

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
   nLineWidth *= _SCALE_

   IF hZebra != NIL
      IF hb_zebra_geterror( hZebra ) == 0
         IF Empty( nLineHeight )
            nLineHeight := 16
         ENDIF
         wapi_TextOut( hDC,  40 * _SCALE_, nY, cType )
         IF Len( cTxt := hb_zebra_getcode( hZebra ) ) < 20
            wapi_TextOut( hDC, 150 * _SCALE_, nY, cTxt )
         ENDIF
         hb_zebra_draw_wapi( hZebra, hDC, wapi_CreateSolidBrush( 0 ), 300 * _SCALE_, nY, nLineWidth, nLineHeight * _SCALE_ )
      ELSE
         ? "Type", cType, "Code", cCode, "Error", hb_zebra_geterror( hZebra )
      ENDIF
      hb_zebra_destroy( hZebra )
   ELSE
      ? "Invalid barcode type", cType
   ENDIF

   RETURN

STATIC FUNCTION hb_zebra_draw_wapi( hZebra, hDC, hBrush, ... )

   IF hb_zebra_geterror( hZebra ) != 0
      RETURN HB_ZEBRA_ERROR_INVALIDZEBRA
   ENDIF

   RETURN hb_zebra_draw( hZebra, {| x, y, w, h | wapi_FillRect( hDC, { Int( x + .5 ), Int( y + .5 ), Int( x + .5 ) + Int( w ), Int( y + .5 ) + Int( h ) + 1 }, hBrush ) }, ... )
