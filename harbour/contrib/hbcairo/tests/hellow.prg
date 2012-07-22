/*
 * $Id$
 */

/* UTF-8 */

#include "hbcairo.ch"

REQUEST HB_CODEPAGE_LTWIN

PROC main()
   LOCAL hSurface, hCairo, nI

   HB_CDPSELECT("LTWIN")
   hSurface := cairo_pdf_surface_create( "hellow.pdf", 566.9, 793.7 )  // 200x280 mm in pt
   hCairo := cairo_create( hSurface )

   cairo_select_font_face( hCairo, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD )
   cairo_set_font_size( hCairo, 16 )
   cairo_set_source_rgb( hCairo, 0, 0, 0 )  // black

   cairo_move_to( hCairo, 50, 50 )
   cairo_show_text( hCairo, "Hello, World!" )

   cairo_set_line_width( hCairo, 1 )
   FOR nI := 1 TO 10
      cairo_set_source_rgb( hCairo, HB_RANDOM(), HB_RANDOM(), HB_RANDOM() )
      cairo_rectangle( hCairo, 100 + nI * 5, 50 + nI * 5, 100, 70 )
      cairo_stroke( hCairo )
   NEXT

   // Let's try some national characters
   cairo_select_font_face( hCairo, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL )
   cairo_set_font_size( hCairo, 10 )
   cairo_set_source_rgb( hCairo, 0, 0, 0 )
   cairo_move_to( hCairo, 50, 300 )
   cairo_show_text( hCairo, hb_UTF8ToStr( "Plaukė žąselė per ežerėlį..." ) )

   cairo_show_page( hCairo )
   cairo_destroy( hCairo )
   cairo_surface_destroy( hSurface )
RETURN
