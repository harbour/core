/*
 * $Id$
 */


#include "hbcairo.ch"


PROC main()
   LOCAL hSurface, hCairo
   FIELD CODE, NAME, RESIDENTS

   // Create database
   DBCREATE( "country", {{"CODE", "C", 3, 0}, {"NAME", "C", 30, 0}, {"RESIDENTS", "N", 10, 0}},, .T. )
   DBAPPEND(); CODE := "LTU";  NAME := "Lithuania";                 RESIDENTS :=   3369600
   DBAPPEND(); CODE := "USA";  NAME := "United States of America";  RESIDENTS := 305397000
   DBAPPEND(); CODE := "POR";  NAME := "Portugal";                  RESIDENTS :=  10617600
   DBAPPEND(); CODE := "POL";  NAME := "Poland";                    RESIDENTS :=  38115967
   DBAPPEND(); CODE := "AUS";  NAME := "Australia";                 RESIDENTS :=  21446187
   DBAPPEND(); CODE := "FRA";  NAME := "France";                    RESIDENTS :=  64473140
   DBAPPEND(); CODE := "RUS";  NAME := "Russia";                    RESIDENTS := 141900000

   // Draw
   hSurface := cairo_pdf_surface_create( "table.pdf", 566.9, 793.7 )  // 200x280 mm in pt
   hCairo := cairo_create( hSurface )

   cairo_select_font_face( hCairo, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD )
   cairo_set_font_size( hCairo, 16 )
   cairo_set_source_rgb( hCairo, 0, 0, 0 )

   cairo_move_to( hCairo, 50, 50 )
   cairo_show_text( hCairo, "Table of countries" )

   draw_table( hCairo, 50, 75, {{"Code", "CODE"}, {"Country", "NAME"}, {"Residents", "RESIDENTS"}} )

   cairo_show_page( hCairo )
   cairo_destroy( hCairo )
   cairo_surface_destroy( hSurface )
   DBCLOSEALL()
RETURN


STATIC PROC draw_table( hCairo, nX, nY, aCol )
   LOCAL nI, aWidth, nDX, nW, xValue

   cairo_save( hCairo )
   cairo_select_font_face( hCairo, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL )
   cairo_set_font_size( hCairo, 10 )
   cairo_set_source_rgb( hCairo, 0, 0, 0 )
   cairo_set_line_width( hCairo, 1 )

   DBGOTOP()
   aWidth := ARRAY( LEN( aCol ) )
   FOR nI := 1 TO LEN( aCol )
     aWidth[ nI ] := cairo_text_extents( hCairo, REPLICATE( "9", FIELDLEN( FIELDPOS( aCol[ nI, 2 ] ) ) ) )[ 5 ]
     aWidth[ nI ] := MAX( aWidth[ nI ], cairo_text_extents( hCairo, aCol[ nI, 1 ] )[ 5 ] ) + 20
   NEXT
   nW := 0
   AEVAL( aWidth, {| X | nW += X } )

   cairo_move_to( hCairo, nX, nY )
   cairo_rel_line_to( hCairo, nW, 0 )
   cairo_stroke( hCairo )

   nDX := nX
   FOR nI := 1 TO LEN( aCol )
     cairo_move_to( hCairo, nDX + aWidth[ nI ] / 2, nY + 10 )
     show_text_center( hCairo, aCol[ nI, 1 ] )
     nDX += aWidth[ nI ]
     IF nI < LEN( aCol )
        cairo_move_to( hCairo, nDX, nY )
        cairo_rel_line_to( hCairo, 0, 13 )
        cairo_stroke( hCairo )
     ENDIF
   NEXT
   nY += 13
   cairo_move_to( hCairo, nX, nY )
   cairo_rel_line_to( hCairo, nW, 0 )
   cairo_stroke( hCairo )

   DO WHILE ! EOF()
      nDX := nX
      FOR nI := 1 TO LEN( aCol )
        xValue := FIELDGET( FIELDPOS( aCol[ nI, 2 ] ) )
        IF VALTYPE( xValue ) == "C"
          cairo_move_to( hCairo, nDX + 10, nY + 10 )
          cairo_show_text( hCairo, xValue )
        ELSEIF VALTYPE( xValue ) == "N"
          cairo_move_to( hCairo, nDX + aWidth[ nI ] - 10, nY + 10 )
          show_text_right( hCairo, STR( xValue ) )
        ELSEIF VALTYPE( xValue ) == "D"
          cairo_move_to( hCairo, nDX + 10, nY + 10 )
          show_text_right( hCairo, DTOC( xValue ) )
        ENDIF
        nDX += aWidth[ nI ]
        IF nI < LEN( aCol )
           cairo_move_to( hCairo, nDX, nY )
           cairo_rel_line_to( hCairo, 0, 13 )
           cairo_stroke( hCairo )
        ENDIF
      NEXT
      DBSKIP()
      nY += 13
   ENDDO
   cairo_move_to( hCairo, nX, nY )
   cairo_rel_line_to( hCairo, nW, 0 )
   cairo_stroke( hCairo )

   cairo_restore( hCairo )
RETURN


STATIC PROC show_text_right( hCairo, cText )
   cairo_rel_move_to( hCairo, - cairo_text_extents( hCairo, cText )[ 5 ], 0 )
   cairo_show_text( hCairo, cText )
RETURN


STATIC PROC show_text_center( hCairo, cText )
   cairo_rel_move_to( hCairo, -0.5 * cairo_text_extents( hCairo, cText )[ 5 ], 0 )
   cairo_show_text( hCairo, cText )
RETURN
