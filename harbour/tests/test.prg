/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL s := " " + Chr( 0 ) + "  mab  " + Chr( 0 ) + " "

   StrDump( s )
   QOut( s )

   QOut( '"' + LTrim( s ) + '"' )
   QOut( '"' + RTrim( s ) + '"' )
   QOut( '"' + AllTrim( s ) + '"' )

   RETURN

STATIC PROCEDURE StrDump( s )
   LOCAL tmp
   FOR EACH tmp IN s
      QOut( Asc( tmp ) )
   NEXT
   RETURN
