/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbmisc"
#require "hbtest"

PROCEDURE Main()

   HBTEST BubbleBabbleEncode_prg( "" )           IS "xexax"
   HBTEST BubbleBabbleEncode( "" )               IS "xexax"
   HBTEST BubbleBabbleEncode_prg( "1234567890" ) IS "xesef-disof-gytuf-katof-movif-baxux"
   HBTEST BubbleBabbleEncode( "1234567890" )     IS "xesef-disof-gytuf-katof-movif-baxux"
   HBTEST BubbleBabbleEncode_prg( "Pineapple" )  IS "xigak-nyryk-humil-bosek-sonax"
   HBTEST BubbleBabbleEncode( "Pineapple" )      IS "xigak-nyryk-humil-bosek-sonax"
   HBTEST BubbleBabbleEncode_prg( "hello" )      IS "xipak-herek-serix"
   HBTEST BubbleBabbleEncode( "hello" )          IS "xipak-herek-serix"
   HBTEST BubbleBabbleEncode_prg( "vszakats" )   IS "xitil-fyvok-capek-citol-fixix"
   HBTEST BubbleBabbleEncode( "vszakats" )       IS "xitil-fyvok-capek-citol-fixix"

   RETURN

/* Harbour implementation */

STATIC FUNCTION BubbleBabbleEncode_prg( cString )

   LOCAL vo := "aeiouy"
   LOCAL co := "bcdfghklmnprstvzx"

   LOCAL cResult := "x"

   LOCAL i
   LOCAL byte1, byte2

   LOCAL nSeed := 1

   i := 1
   DO WHILE .T.

      IF i > hb_BLen( cString )
         cResult += ;
            SubStr( vo, nSeed % 6 + 1, 1 ) + ;
            SubStr( co, 16 + 1, 1 ) + ;
            SubStr( vo, nSeed / 6 + 1, 1 )
         EXIT
      ENDIF

      byte1 := hb_BCode( hb_BSubStr( cString, i, 1 ) )

      cResult += ;
         SubStr( vo, ( ( hb_bitAnd( hb_bitShift( byte1, -6 ), 3 ) + nSeed ) % 6 ) + 1, 1 ) + ;
         SubStr( co, hb_bitAnd( hb_bitShift( byte1, -2 ), 15 ) + 1, 1 ) + ;
         SubStr( vo, ( ( hb_bitAnd( byte1, 3 ) + ( nSeed / 6 ) ) % 6 ) + 1, 1 )

      IF i + 1 > hb_BLen( cString )
         EXIT
      ENDIF

      byte2 := hb_BCode( hb_BSubStr( cString, i + 1, 1 ) )

      cResult += ;
         SubStr( co, hb_bitAnd( hb_bitShift( byte2, -4 ), 15 ) + 1, 1 ) + ;
         "-" + ;
         SubStr( co, hb_bitAnd( byte2, 15 ) + 1, 1 )

      nSeed := ( nSeed * 5 + byte1 * 7 + byte2 ) % 36

      i += 2
   ENDDO

   RETURN cResult + "x"
