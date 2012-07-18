/*
 * $Id$
 */

// Testing Harbour Error system

PROCEDURE Main()

   LOCAL n

   QOut( "We are running and now an error will raise" )

   n++      // an error should raise here

   RETURN
