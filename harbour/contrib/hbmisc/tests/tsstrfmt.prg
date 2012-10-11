/*
 * $Id$
 */

#require "hbmisc"

PROCEDURE Main()

   ? StrFormat( "%1 thing %2", "a", "b" )
   ? StrFormat( "%1 thing %2", "a", Date() )
   ? StrFormat( "%1 thing %2", .T., Date() )
   ? StrFormat( "%1 thing %2", .T., NIL )
   ? StrFormat( "%1 thing %2", "space1  ", 100.20 )
   ? StrFormat( "%1 thing %2", "   space2  ", 213 )
   ? StrFormat( "%1 thing %2", "   space3", 0.12 )
   ? StrFormat( "%2 thing %1 stuff", "   space3", 0.12 )
   ? StrFormat( "%2 %1 born on %3", "Smith", "Mr.", Date() )

   RETURN
