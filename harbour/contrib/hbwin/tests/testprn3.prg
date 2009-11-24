/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   Dump( GetPrinters( .F., .F. ) )
   Dump( GetPrinters( .F., .T. ) )
   Dump( GetPrinters( .T., .F. ) )
   Dump( GetPrinters( .T., .T. ) )

   ? "GETDEFAULTPRINTER:", ">" + GETDEFAULTPRINTER() + "<"
   ? "XISPRINTER:", XISPRINTER()

   RETURN

STATIC PROCEDURE Dump( a )
   LOCAL b, c

   ? "=================="
   ?
   FOR EACH b IN a
      IF ValType( b ) == "A"
         FOR EACH c IN b
            ?? c
            IF c:__enumIndex() == 2
               ?? ">>" + PRINTERPORTTONAME( c ) + "<<",  "|>>" + PRINTERPORTTONAME( c, .T. ) + "<<|"
            ENDIF
            ?
         NEXT
      ELSE
         ? b, PRINTEREXISTS( b ), XISPRINTER( b )
      ENDIF
   NEXT

   RETURN
