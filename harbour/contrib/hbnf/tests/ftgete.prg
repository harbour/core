/*
 * $Id$
 */

#include "simpleio.ch"

function main()

   LOCAL a
   LOCAL c

   a := Array( FT_GETE() )
   FT_GETE( @a )
   FOR tmp := 1 TO Len( a )
      ? a[ tmp ]
   NEXT

   ? "-------------------------------------"

   c := ""
   FT_GETE( @c )
   ? c

   RETURN
