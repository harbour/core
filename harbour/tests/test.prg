/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL s := " " + Chr( 0 ) + "  mab  " + Chr( 0 ) + " "

   StrDump( s )
   ? s

   ? Chr( 34 ) + LTrim( s ) + Chr( 34 )
   ? Chr( 34 ) + RTrim( s ) + Chr( 34 )
   ? Chr( 34 ) + AllTrim( s ) + Chr( 34 )

   RETURN

STATIC PROCEDURE StrDump( s )
   LOCAL tmp
   FOR EACH tmp IN s
      ? Asc( tmp )
   NEXT
   RETURN
