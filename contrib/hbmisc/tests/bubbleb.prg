/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbmisc"

PROCEDURE Main()

   ? BubbleBabbleEncode_prg( "" )
   ? BubbleBabbleEncode( "" )
   ? "xexax"

   ? BubbleBabbleEncode_prg( "1234567890" )
   ? BubbleBabbleEncode( "1234567890" )
   ? "xesef-disof-gytuf-katof-movif-baxux"

   ? BubbleBabbleEncode_prg( "Pineapple" )
   ? BubbleBabbleEncode( "Pineapple" )
   ? "xigak-nyryk-humil-bosek-sonax"

   ? BubbleBabbleEncode_prg( "hello" )
   ? BubbleBabbleEncode( "hello" )

   ? BubbleBabbleEncode_prg( "vszakats" )
   ? BubbleBabbleEncode( "vszakats" )

   RETURN

/* Harbour implementation */

FUNCTION BubbleBabbleEncode_Prg( cString )

   LOCAL vo := "aeiouy"
   LOCAL co := "bcdfghklmnprstvzx"

   LOCAL cResult := "x"

   LOCAL i
   LOCAL byte1, byte2

   LOCAL nSeed := 1

   i := 1
   DO WHILE .T.

      IF i > Len( cString )
         cResult += ;
            SubStr( vo, nSeed % 6 + 1, 1 ) + ;
            SubStr( co, 16 + 1, 1 ) + ;
            SubStr( vo, nSeed / 6 + 1, 1 )
         EXIT
      ENDIF

      byte1 := Asc( SubStr( cString, i, 1 ) )

      cResult += ;
         SubStr( vo, ( ( hb_bitAnd( hb_bitShift( byte1, -6 ), 3 ) + nSeed ) % 6 ) + 1, 1 ) + ;
         SubStr( co, hb_bitAnd( hb_bitShift( byte1, -2 ), 15 ) + 1, 1 ) + ;
         SubStr( vo, ( ( hb_bitAnd( byte1, 3 ) + ( nSeed / 6 ) ) % 6 ) + 1, 1 )

      IF i + 1 > Len( cString )
         EXIT
      ENDIF

      byte2 := Asc( SubStr( cString, i + 1, 1 ) )

      cResult += ;
         SubStr( co, hb_bitAnd( hb_bitShift( byte2, -4 ), 15 ) + 1, 1 ) + ;
         "-" + ;
         SubStr( co, hb_bitAnd( byte2, 15 ) + 1, 1 )

      nSeed := ( nSeed * 5 + byte1 * 7 + byte2 ) % 36

      i += 2
   ENDDO

   RETURN cResult + "x"
