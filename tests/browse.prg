// Testing Browse()

#include "inkey.ch"

PROCEDURE Main()

#ifdef _SET_EVENTMASK
   Set( _SET_EVENTMASK, INKEY_ALL )
   MSetCursor( .T. )
#endif

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   SetColor( "W+/B" )
   CLS

   USE test.dbf READONLY
   Browse()

   SetColor( "" )
   CLS

   RETURN
