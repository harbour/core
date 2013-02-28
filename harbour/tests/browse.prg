/*
 * $Id$
 */

// Testing Browse()

PROCEDURE Main()

   LOCAL cColor := SetColor( "W+/B" )

   CLS

   USE test
   Browse()

   SetColor( cColor )
   CLS

   RETURN
