/*
 * $Id$
 */

// Testing Browse()

#include "inkey.ch"

PROCEDURE Main()

   LOCAL cColor := SetColor( "W+/B" )

#ifdef _SET_EVENTMASK
   Set( _SET_EVENTMASK, INKEY_ALL )
   MSetCursor( .T. )
#endif

   CLS

   USE test
   Browse()

   SetColor( cColor )
   CLS

   RETURN
