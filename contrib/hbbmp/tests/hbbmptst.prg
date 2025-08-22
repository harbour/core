/*
 * Copyright 2025 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 */

#define WIDTH     240
#define HEIGHT    200

#xtranslate _out( <frm> [, <params,...>] ) => outstd( hb_strFormat( <frm>, <params> ) + hb_eol() )
#xtranslate _err( <frm> [, <params,...>] ) => outerr( hb_strFormat( <frm>, <params> ) + hb_eol() )

REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main()
LOCAL pBMP, depth, bkg, black, red, green, blue, last, fname, x1, x2, y1, y2, nn

   FOR EACH depth IN { 1, 4, 8, 24, 32 }
      IF Empty( pBMP := hb_bmp_new( WIDTH, HEIGHT, depth ) )
         _err( "Cannot create BMP with DEPTH=%d", depth )
      ELSE
         _out( "Created BMP width=%d, height=%d, depth=%d", ;
               hb_bmp_width( pBMP ), hb_bmp_height( pBMP ), hb_bmp_depth( pBMP ) )
         bmp_color( pBMP, 255, 255, 255,   0, @bkg,   @last )
         bmp_color( pBMP,   0,   0,   0,   0, @black, @last )
         bmp_color( pBMP, 127,   0,   0,   0, @red,   @last )
         bmp_color( pBMP,   0, 127,   0,   0, @green, @last )
         bmp_color( pBMP,   0,   0, 127,   0, @blue,  @last )

         hb_bmp_rect( pBMP, 0, 0, WIDTH, HEIGHT, bkg )
         hb_bmp_rect( pBMP, 1, 1, WIDTH - 2, HEIGHT - 2, black, .f. )

         hb_bmp_rect( pBMP, 20, 20, WIDTH - 40, HEIGHT - 40, black )
         hb_bmp_rect( pBMP, 30, 30, WIDTH - 60, HEIGHT - 60, bkg )
         x1 := 40 ; y1 := 40 ; x2 := WIDTH - 40 ; y2 := HEIGHT - 40
         hb_bmp_rect( pBMP, x1, y1, x2 - x1 + 1, y2 - y1 + 1, red, .f. )
         hb_bmp_line( pBMP, x1, y1, x2, y2, blue )
         hb_bmp_line( pBMP, x2, y1, x1, y2, green )
         hb_bmp_line( pBMP, x1 + x1, y1, x2 - x1, y2, blue )
         hb_bmp_line( pBMP, x2 - x1, y1, x1 + x1, y2, green )

         FOR nn := 1 TO 5
           hb_bmp_rect( pBMP, 20 * nn, 8, 12, 8, { green, blue, red, black, green }[ nn ] )
         NEXT

         fname := "test_" + strzero( depth, 2 ) + ".bmp"
         bmp_save( pBMP, fname )
         pBMP := NIL
      ENDIF
      _out( "" )
   NEXT
RETURN

STATIC FUNCTION bmp_color( pBMP, r, g, b, a, /*@*/clr, /*@*/last )
LOCAL r2, g2, b2, a2
   clr = hb_bmp_color( pBMP, r, g, b, a )
   IF clr >= 0
      last := clr
      hb_bmp_color2rgb( pBMP, clr, @r2, @g2, @b2, @a2 )
      _out( "Allocated color 0x%08x: r=%s, g=%s, b=%s, a=%s => r=%s, g=%s, b=%s, a=%s", clr, ;
           hb_valToExp( r ),  hb_valToExp( g ),  hb_valToExp( b ),  hb_valToExp( a ), ;
           hb_valToExp( r2 ), hb_valToExp( g2 ), hb_valToExp( b2 ), hb_valToExp( a2 ) )
      RETURN .T.
   ELSE
      clr := last
      _err( "Cannot allocate color" )
   ENDIF
RETURN .F.

STATIC FUNCTION bmp_save( pBMP, fname )
   IF hb_bmp_save( pBMP, fname )
      _out( "Created file: %s", fname )
      RETURN .T.
   ELSE
      _err( "Cannot save file: %s", fname )
   ENDIF
RETURN .F.
