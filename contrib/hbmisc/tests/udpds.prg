/*
 * This module demonstrates a simple UDP Discovery Server
 */

#require "hbmisc"

PROCEDURE Main( cParam )

   LOCAL h

   IF ! hb_mtvm()
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
      IF ! Empty( h := hb_udpds_Start( 39999, "UDPDSDemo", NetName() + " " + hb_TSToStr( hb_DateTime() ) ) )
         hb_idleSleep( 0.1 )
      ENDIF
   ENDIF

   IF "C" $ Upper( cParam )
      ? hb_ValToExp( hb_udpds_Find( 39999, "UDPDSDemo" ) )
   ENDIF

   IF "S" $ Upper( cParam )
      ? "Press any key to stop server"
      Inkey( 0 )
      hb_udpds_Stop( h )
   ENDIF

   RETURN
