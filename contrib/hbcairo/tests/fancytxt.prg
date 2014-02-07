#require "hbcairo"

PROCEDURE Main()

   LOCAL hSurface

   hSurface := cairo_pdf_surface_create( "fancytxt.pdf", 566.9, 793.7 )  // 200x280 mm in pt
   draw( hSurface )
   cairo_surface_destroy( hSurface )

   hSurface := cairo_image_surface_create( "fancytxt.pdf", 567, 794 )
   draw( hSurface )
   cairo_surface_write_to_png( hSurface, "fancytxt.png" )
   cairo_surface_destroy( hSurface )

   RETURN

STATIC PROCEDURE draw( hSurface )

   LOCAL hCairo, hPath

   hCairo := cairo_create( hSurface )
   cairo_set_tolerance( hCairo, 0.01 )

   // Draw base line
   cairo_move_to( hCairo, 50, 650 )
   cairo_rel_line_to( hCairo, 250, 50 )
   cairo_rel_curve_to( hCairo, 100, 20, 200, -50, 200, -150 )
   cairo_rel_curve_to( hCairo, 0, -400, -300, -100, -400, -300 )
   hPath := cairo_copy_path_flat( hCairo )

   cairo_set_line_width( hCairo, 1 )
   cairo_set_source_rgb( hCairo, 0.6, 0.0, 0.0 )
   cairo_stroke( hCairo )

   // Draw text
   cairo_new_path( hCairo )
   cairo_select_font_face( hCairo, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL )
   cairo_set_font_size( hCairo, 72 )
   cairo_move_to( hCairo, 0, -5 )
   cairo_text_path( hCairo, "Welcome to the world of Harbour..." )

   // Transform
   map_path_onto( hCairo, hPath )
   cairo_path_destroy( hPath )

   // Paint
   cairo_set_line_cap( hCairo, CAIRO_LINE_CAP_ROUND )
   cairo_set_source_rgb( hCairo, 0.2, 0.1, 0.8 )
   cairo_fill_preserve( hCairo )

   cairo_set_line_width( hCairo, 1 )
   cairo_set_source_rgb( hCairo, 0.2, 0.2, 0.2 )
   cairo_stroke( hCairo )

   cairo_show_page( hCairo )
   cairo_destroy( hCairo )

   RETURN

STATIC PROCEDURE map_path_onto( hCairo, hPath )

   LOCAL hCurrentPath, aLengths, hIterator, pt, aPoints

   hCurrentPath := cairo_copy_path( hCairo )
   aLengths := path_lengths( hPath )
   hIterator := cairo_path_iterator_create( hCurrentPath )
   DO WHILE cairo_path_iterator_next( hIterator ) != NIL
      IF Len( aPoints := cairo_path_iterator_get_points( hIterator ) ) > 0
         FOR EACH pt IN aPoints
            transform_point( @pt[ 1 ], @pt[ 2 ], hPath, aLengths )
         NEXT
         cairo_path_iterator_set_points( hIterator, aPoints )
      ENDIF
   ENDDO
   cairo_path_iterator_destroy( hIterator )
   cairo_append_path( hCairo, hCurrentPath )

   RETURN

STATIC PROCEDURE transform_point( nX, nY, hPath, aLengths )

   LOCAL hIterator, nI, nNX, nNY, nDX, nDY, nRatio, nType, aLast, aPoints, nK1, nK2

   nNX := nX
   nNY := nY
   hIterator := cairo_path_iterator_create( hPath )
   nI := 1
   DO WHILE ( nType := cairo_path_iterator_next( hIterator ) ) != NIL
      aPoints := cairo_path_iterator_get_points( hIterator )
      IF nNX <= aLengths[ nI ] .AND. nType != CAIRO_PATH_MOVE_TO
         EXIT
      ENDIF
      nNX -= aLengths[ nI ]
      nI++
      DO CASE
      CASE nType == CAIRO_PATH_MOVE_TO .OR. ;
           nType == CAIRO_PATH_LINE_TO
         aLast := aPoints[ 1 ]
      CASE nType == CAIRO_PATH_CURVE_TO
         aLast := aPoints[ 3 ]
      ENDCASE
   ENDDO

   DO CASE
   CASE nType == CAIRO_PATH_MOVE_TO
   CASE nType == CAIRO_PATH_LINE_TO
      nRatio := nNX / aLengths[ nI ]
      nX := aLast[ 1 ] * ( 1 - nRatio ) + aPoints[ 1, 1 ] * nRatio
      nY := aLast[ 2 ] * ( 1 - nRatio ) + aPoints[ 1, 2 ] * nRatio

      nDX := -( aLast[ 1 ] - aPoints[ 1, 1 ] )
      nDY := -( aLast[ 2 ] - aPoints[ 1, 2 ] )

      nRatio := nNY / aLengths[ nI ]
      nX += -nDY * nRatio
      nY += nDX * nRatio
   CASE nType == CAIRO_PATH_CURVE_TO
      nX := aLast[ 1 ] * ( 1 - nRatio ) ^ 3 + 3 * aPoints[ 1, 1 ] * ( 1 - nRatio ) ^ 2 * nRatio + 3 * aPoints[ 2, 1 ] * ( 1 - nRatio ) * nRatio ^ 2 + aPoints[ 3, 1 ] * nRatio ^ 3
      nY := aLast[ 2 ] * ( 1 - nRatio ) ^ 3 + 3 * aPoints[ 1, 2 ] * ( 1 - nRatio ) ^ 2 * nRatio + 3 * aPoints[ 2, 2 ] * ( 1 - nRatio ) * nRatio ^ 2 + aPoints[ 3, 2 ] * nRatio ^ 3

      nK1 := 1 - 4 * nRatio + 3 * nRatio ^ 2
      nK2 := 2 * nRatio - 3 * nRatio ^ 2

      nDX := -3 * aLast[ 1 ] * ( 1 - nRatio ) ^ 2 + 3 * aPoints[ 1, 1 ] * nK1 + 3 * aPoints[ 2, 1 ] * nK2 + 3 * aPoints[ 3, 1 ] * nRatio ^ 2
      nDY := -3 * aLast[ 2 ] * ( 1 - nRatio ) ^ 2 + 3 * aPoints[ 1, 2 ] * nK1 + 3 * aPoints[ 2, 2 ] * nK2 + 3 * aPoints[ 3, 2 ] * nRatio ^ 2

      nRatio := nNY / Sqrt( nDX * nDX + nDY * nDY )
      nX += -nDY * nRatio
      nY += nDX * nRatio
   ENDCASE
   cairo_path_iterator_destroy( hIterator )

   RETURN

STATIC FUNCTION path_lengths( hPath )

   LOCAL hIterator, nType, aLast, aRet, aPoints, nLen

   aRet := {}
   hIterator := cairo_path_iterator_create( hPath )
   DO WHILE ( nType := cairo_path_iterator_next( hIterator ) ) != NIL
      aPoints := cairo_path_iterator_get_points( hIterator )
      nLen := 0
      DO CASE
      CASE nType == CAIRO_PATH_MOVE_TO
         aLast := aPoints[ 1 ]
      CASE nType == CAIRO_PATH_LINE_TO
         nLen := distance( aLast[ 1 ], aLast[ 2 ], aPoints[ 1, 1 ], aPoints[ 1, 2 ] )
         aLast := aPoints[ 1 ]
      CASE nType == CAIRO_PATH_CURVE_TO
         nLen := curve_length( aLast[ 1 ], aLast[ 2 ], aPoints[ 1, 1 ], aPoints[ 1, 2 ], ;
            aPoints[ 2, 1 ], aPoints[ 2, 2 ], aPoints[ 3, 1 ], aPoints[ 3, 2 ] )
         aLast := aPoints[ 3 ]
      ENDCASE
      AAdd( aRet, nLen )
   ENDDO
   cairo_path_iterator_destroy( hIterator )

   RETURN aRet

STATIC FUNCTION distance( nX1, nY1, nX2, nY2 )
   RETURN Sqrt( ( nX1 - nX2 ) ^ 2 + ( nY1 - nY2 ) ^ 2 )

STATIC FUNCTION curve_length( nX1, nY1, nX2, nY2, nX3, nY3, nX4, nY4 )

   LOCAL nLength := 0, hSurface, hCairo, hPath, hIterator, nType, aLast, aPoints

   hSurface := cairo_image_surface_create( CAIRO_FORMAT_A8, 0, 0 )
   hCairo := cairo_create( hSurface )
   cairo_move_to( hCairo, nX1, nY1 )
   cairo_curve_to( hCairo, nX2, nY2, nX3, nY3, nX4, nY4 )
   hPath := cairo_copy_path_flat( hCairo )
   hIterator := cairo_path_iterator_create( hPath )
   DO WHILE ( nType := cairo_path_iterator_next( hIterator, @aPoints ) ) != NIL
      DO CASE
      CASE nType == CAIRO_PATH_MOVE_TO
         aLast := aPoints
      CASE nType == CAIRO_PATH_LINE_TO
         nLength += distance( aLast[ 1 ], aLast[ 2 ], aPoints[ 1 ], aPoints[ 2 ] )
         aLast := aPoints
      ENDCASE
   ENDDO
   cairo_path_iterator_destroy( hIterator )
   cairo_path_destroy( hPath )
   cairo_destroy( hCairo )
   cairo_surface_destroy( hSurface )

   RETURN nLength
