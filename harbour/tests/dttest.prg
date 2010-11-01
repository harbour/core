/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Carlos Bacco
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   ? HB_DATETIME( 1974 )

   ? " VALTYPE( HB_DATETIME() ) =>", VALTYPE( HB_DATETIME() )
   ? " YEAR(    HB_DATETIME() ) =>", YEAR(    HB_DATETIME() )
   ? " MONTH(   HB_DATETIME() ) =>", MONTH(   HB_DATETIME() )
   ? " DAY(     HB_DATETIME() ) =>", DAY(     HB_DATETIME() )
   ?

   ? " VALTYPE( HB_DATETIME( 1974, 5, 31 ) ) =>", VALTYPE( HB_DATETIME( 1974, 5, 31 ) )
   ? " YEAR(    HB_DATETIME( 1974, 5, 31 ) ) =>", YEAR(    HB_DATETIME( 1974, 5, 31 ) )
   ? " MONTH(   HB_DATETIME( 1974, 5, 31 ) ) =>", MONTH(   HB_DATETIME( 1974, 5, 31 ) )
   ? " DAY(     HB_DATETIME( 1974, 5, 31 ) ) =>", DAY(     HB_DATETIME( 1974, 5, 31 ) )
   ? " DTOC(    HB_DATETIME( 1974, 5, 31 ) ) =>", DTOC(    HB_DATETIME( 1974, 5, 31 ) )
   ?

   ? " VALTYPE( HB_DATETIME( 1974, 31, 5, NIL, NIL, NIL ) ) =>", VALTYPE( HB_DATETIME( 1974, 31, 5, NIL, NIL, NIL ) )
   ?

   ? " VALTYPE( HB_DATETIME( 2001, 10, 13, 18, 42, 16  ) ) =>", VALTYPE( HB_DATETIME( 2001, 10, 13, 18, 42, 16  ) )
   ?

   ? " VALTYPE( HB_DATETIME( NIL, NIL, NIL, 10, 36, 05  ) )     =>", VALTYPE( HB_DATETIME( NIL, NIL, NIL, 10, 36, 05  ) )
   ? "          HB_DATETIME( NIL, NIL, NIL, 10, 36, 05  )       =>",          HB_DATETIME( NIL, NIL, NIL, 10, 36, 05  )
   ?

   ? " VALTYPE( HB_DATETIME( NIL, NIL, NIL, 10, 36, 05, 176 ) ) =>", VALTYPE( HB_DATETIME( NIL, NIL, NIL, 10, 36, 05, 176  ) )
   ? "          HB_DATETIME( NIL, NIL, NIL, 10, 36, 05, 176 )   =>",          HB_DATETIME( NIL, NIL, NIL, 10, 36, 05, 176  )

   ? " VALTYPE( HB_DATETIME( 0, 0, 0, 10, 36, 05, 176 ) ) =>", VALTYPE( HB_DATETIME( 0, 0, 0, 10, 36, 05, 176  ) )
   ? "          HB_DATETIME( 0, 0, 0, 10, 36, 05, 176 )   =>",          HB_DATETIME( 0, 0, 0, 10, 36, 05, 176  )

   RETURN
