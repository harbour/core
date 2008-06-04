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
#define CON_DBFCDX
//#define CON_ADS

#ifdef CON_ADS
#include "ads.ch"
REQUEST _ADS
#endif

STATIC aCampos := { {"Codigo", "C", 6, 0}, {"Nombre", "C", 35, 0} }

PROCEDURE FILES()

   Local n := 0, h:=Array(NFILES)

#ifdef CON_ADS

   rddRegister( "ADS", 1 )
   rddsetdefault( "ADS" )
   SET SERVER LOCAL
   SET FILETYPE TO CDX
   SET CHARTYPE TO OEM
   SET AXS LOCKING ON
   AdsRightsCheck(.F.)

#endif

   CLS
   AFill( h, 0 )
   DO WHILE n < NFILES
      n++
      @10,0 SAY "Building files.... "+Str( n )
      DbCreate( "File" + LTrim( Str( n ) ), aCampos )
      USE ( "File" + LTrim( Str( n ) ) ) NEW

#ifdef CON_ADS
      INDEX ON CODIGO TAG CODIGO TO ( "File" + LTrim( Str( n ) ) )
#endif

      CLOSE DATA
   ENDDO

   n := 0

   DO WHILE n < NFILES
      n++
      @12,0 SAY "Opening files.... "+Str( n )
      USE ( "File" + LTrim( Str( n ) ) ) NEW

   #ifdef CON_ADS
      SET ORDER TO TAG CODIGO
   #endif

   ENDDO

   CLOSE DATA

   n := 0

   DO WHILE n < NFILES
      n++
      @14,0 SAY "Deleting files.... "+Str( n )
      FErase ( "File" + LTrim( Str( n ) ) + ".Dbf" )
   ENDDO

RETURN NIL
