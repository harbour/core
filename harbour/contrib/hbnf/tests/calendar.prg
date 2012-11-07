/*
 * $Id$
 */

#require "hbnf"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL aRet

   SET DATE ANSI
   SET CENTURY ON

   SetColor( "w+/b" )
   CLS
   IF ft_NumLock()
      ft_NumLock( .F. )
   ENDIF
   hb_keyPut( K_F1 )
   aRet := ft_Calendar( 10, 40, "w+/rb", .T., .T. ) // display calendar, return all.
   @ 1, 0 SAY "Date        :" + DToC( aRet[ 1 ] )
   @ 2, 0 SAY "Month Number:" + Str( aRet[ 2 ], 2, 0 )
   @ 3, 0 SAY "Day Number  :" + Str( aRet[ 3 ], 2, 0 )
   @ 4, 0 SAY "Year Number :" + Str( aRet[ 4 ], 4, 0 )
   @ 5, 0 SAY "Month       :" + aRet[ 5 ]
   @ 6, 0 SAY "Day         :" + aRet[ 6 ]
   @ 7, 0 SAY "Julian Day  :" + Str( aRet[ 7 ], 3, 0 )
   @ 8, 0 SAY "Current Time:" + aRet[ 8 ]

   RETURN
