/*
 * $Id$
 */

// This test was written by Jose Gimenez (JFG) <jfgimenez@wanadoo.es>
//                                             <tecnico.sireinsa@ctv.es>
// and is placed into the public domain.

// Number of files to build/open
#define NFILES   1000

// use only *one* at a time
// dejar solo una de las dos lineas siguientes:
#define WITH_DBFCDX
// #define WITH_ADS

#ifdef WITH_ADS
#include "ads.ch"
REQUEST _ADS
#endif

PROCEDURE Main()

   LOCAL aCampos := { ;
      { "Codigo", "C",  6, 0 }, ;
      { "Nombre", "C", 35, 0 } }

   LOCAL n := 0, h := Array( NFILES )

#ifdef WITH_ADS

   rddRegister( "ADS", 1 )
   rddSetDefault( "ADS" )
   SET SERVER LOCAL
   SET FILETYPE TO CDX
   SET CHARTYPE TO OEM
   SET AXS LOCKING ON
   AdsRightsCheck( .F. )

#endif

   CLS
   AFill( h, 0 )
   DO WHILE n < NFILES
      n++
      @ 10, 0 SAY "Building files.... " + Str( n )
      dbCreate( "file" + hb_ntos( n ), aCampos )
      USE ( "file" + hb_ntos( n ) ) NEW

#ifdef WITH_ADS
      INDEX ON CODIGO TAG CODIGO TO ( "file" + hb_ntos( n ) )
#endif

      CLOSE DATA
   ENDDO

   n := 0

   DO WHILE n < NFILES
      n++
      @ 12, 0 SAY "Opening files.... " + Str( n )
      USE ( "file" + hb_ntos( n ) ) NEW

#ifdef WITH_ADS
      SET ORDER TO TAG CODIGO
#endif

   ENDDO

   CLOSE DATA

   n := 0

   DO WHILE n < NFILES
      n++
      @ 14, 0 SAY "Deleting files.... " + Str( n )
      hb_dbDrop( "file" + hb_ntos( n ) + ".dbf" )
   ENDDO

   RETURN
