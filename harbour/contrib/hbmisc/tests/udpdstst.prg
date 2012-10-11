/*
 * $Id$
 */

/*
 * This module demonstrates a simple UDP Discovery Server
 */

#require "hbmisc"

PROCEDURE main( cParam )

   LOCAL h

   IF ! hb_MTVM()
      ? "This sample should be compiled using MultiThread"
      RETURN
   ENDIF

   IF cParam == NIL
      ? "udpdstst {c|s|cs}"
      ? "Parameter:"
      ? "   s - run as a server"
      ? "   c - run as a client"
      RETURN
   ENDIF

   IF "S" $ Upper( cParam )
      IF ! Empty( h := hb_UDPDS_Start( 39999, "UDPDSDemo", NetName() + " " + hb_TSToStr( hb_DateTime() ) ) )
         hb_idleSleep( 0.1 )
      ENDIF
   ENDIF

   IF "C" $ Upper( cParam )
      ? HB_VALTOEXP( hb_UDPDS_Find( 39999, "UDPDSDemo" ) )
   ENDIF

   IF "S" $ Upper( cParam )
      ? "Press any key to stop server"
      Inkey( 0 )
      hb_UDPDS_Stop( h )
   ENDIF

   RETURN
