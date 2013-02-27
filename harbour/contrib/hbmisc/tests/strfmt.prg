/*
 * $Id$
 */

#require "hbmisc"
#require "hbtest"

PROCEDURE Main()

   SET DATE ANSI
   SET CENTURY ON

   HBTEST StrFormat( "%1 thing %2", "a", "b" )                                 IS "a thing b"
   HBTEST StrFormat( "%1 thing %2", "a", 0d20121127 )                          IS "a thing 2012.11.27"
   HBTEST StrFormat( "%1 thing %2", .T., 0d20121127 )                          IS ".T. thing 2012.11.27"
   HBTEST StrFormat( "%1 thing %2", .T., NIL )                                 IS ".T. thing NIL"
   HBTEST StrFormat( "%1 thing %2", "space1  ", 100.20 )                       IS "space1 thing 100.20"
   HBTEST StrFormat( "%1 thing %2", "   space2  ", 213 )                       IS "space2 thing 213"
   HBTEST StrFormat( "%1 thing %2", "   space3", 0.12 )                        IS "space3 thing 0.12"
   HBTEST StrFormat( "%2 thing %1 stuff", "   space3", 0.12 )                  IS "0.12 thing space3 stuff"
   HBTEST StrFormat( "%2 %1 born on %3", "Smith", "Mr.", 0d20121127 )          IS "Mr. Smith born on 2012.11.27"

   HBTEST StrFormat( "Please insert disk %1 to drive %2", hb_ntos( 2 ), "A:" ) IS "Please insert disk 2 to drive A:"
   HBTEST StrFormat( "This is %1 from %2", "Victor", "Europe" )                IS "This is Victor from Europe"
   HBTEST StrFormat( "%2 %1 %2", "Param1", "Param2" )                          IS "Param2 Param1 Param2"
   HBTEST StrFormat( "Hello" )                                                 IS "Hello"
   HBTEST StrFormat( "%1 - %2", "one" )                                        IS "one - "
   HBTEST StrFormat( "%1 - %2", "one", "two" )                                 IS "one - two"
   HBTEST StrFormat( "%2 - %1", "one", "two" )                                 IS "two - one"
   HBTEST StrFormat( "%2 - %", "one", "two" )                                  IS "two - "
   HBTEST StrFormat( "%% - %", "one", "two" )                                  IS "% - "
   HBTEST StrFormat( "%9 - %", "one", "two" )                                  IS " - "

   RETURN
