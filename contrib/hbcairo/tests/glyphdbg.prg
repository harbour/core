
#require "hbcairo"

PROCEDURE Main()

   LOCAL hSurface, hCairo

   hSurface := cairo_pdf_surface_create( "glyphdbg.pdf", 566.9, 793.7 )  // 200x280 mm in pt
   hCairo := cairo_create( hSurface )

   cairo_select_font_face( hCairo, "Times", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL )
   cairo_set_font_size( hCairo, 200 )
   cairo_set_source_rgb( hCairo, 0, 0, 0.7 )

   cairo_move_to( hCairo, 100, 250 )
   cairo_text_path( hCairo, "Ag" )
   path_debug( hCairo )

   cairo_move_to( hCairo, 100, 500 )
   cairo_text_path( hCairo, "Ag" )
   path_debug( hCairo, 4 )

   cairo_show_page( hCairo )
   cairo_destroy( hCairo )
   cairo_surface_destroy( hSurface )

   RETURN


PROCEDURE path_debug( hCairo, nTolerance )

   LOCAL hPath, hIterator, nType, aPoints

   cairo_save( hCairo )

   IF Empty( nTolerance )
      hPath := cairo_copy_path( hCairo )
   ELSE
      cairo_save( hCairo )
      cairo_set_tolerance( hCairo, nTolerance )
      hPath := cairo_copy_path_flat( hCairo )
      cairo_restore( hCairo )
   ENDIF

   // Draw lines
   cairo_new_path( hCairo )
   cairo_append_path( hCairo, hPath )
   cairo_set_source_rgb( hCairo, 0, 0.4, 0 )
   cairo_set_line_width( hCairo, 1.0 )
   cairo_stroke( hCairo )

   // Draw points
   cairo_set_source_rgb( hCairo, 0, 0, 0 )
   cairo_set_line_width( hCairo, 2.0 )
   cairo_set_line_cap( hCairo, CAIRO_LINE_CAP_ROUND )
   hIterator := cairo_path_iterator_create( hPath )
   DO WHILE ( nType := cairo_path_iterator_next( hIterator ) ) != NIL
      aPoints := cairo_path_iterator_get_points( hIterator )
      IF nType == CAIRO_PATH_MOVE_TO
         cairo_move_to( hCairo, aPoints[ 1, 1 ], aPoints[ 1, 2 ] )
         cairo_rel_line_to( hCairo, 0, 0 )
      ELSEIF nType == CAIRO_PATH_LINE_TO
         cairo_move_to( hCairo, aPoints[ 1, 1 ], aPoints[ 1, 2 ] )
         cairo_rel_line_to( hCairo, 0, 0 )
      ELSEIF nType == CAIRO_PATH_CURVE_TO
         cairo_stroke( hCairo )
         cairo_set_source_rgb( hCairo, 0.5, 0.5, 0.5 )
         cairo_move_to( hCairo, aPoints[ 1, 1 ], aPoints[ 1, 2 ] )
         cairo_rel_line_to( hCairo, 0, 0 )
         cairo_move_to( hCairo, aPoints[ 2, 1 ], aPoints[ 2, 2 ] )
         cairo_rel_line_to( hCairo, 0, 0 )
         cairo_stroke( hCairo )
         cairo_set_source_rgb( hCairo, 0, 0, 0 )
         cairo_move_to( hCairo, aPoints[ 3, 1 ], aPoints[ 3, 2 ] )
         cairo_rel_line_to( hCairo, 0, 0 )
      ENDIF
   ENDDO
   cairo_path_iterator_destroy( hIterator )
   cairo_stroke( hCairo )
   cairo_path_destroy( hPath )
   cairo_restore( hCairo )

   RETURN
