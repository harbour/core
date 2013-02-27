/*
 * $Id$
 */

// Testing Browse()

PROCEDURE Main()

   LOCAL cColor

   cColor := SetColor( "W+/B" )
   CLS

   USE test
   Browse()

   SetColor( cColor )
   CLS

   RETURN
