#ifndef __HARBOUR__
#include "clipper.ch"
#endif

#include "setcurs.ch"

PROCEDURE Main()

   LOCAL aCursors := { ;
      { SC_NONE    , "SC_NONE"    , "None"      }, ;
      { SC_NORMAL  , "SC_NORMAL"  , "Underline" }, ;
      { SC_INSERT  , "SC_INSERT"  , "HalfBlock" }, ;
      { SC_SPECIAL1, "SC_SPECIAL1", "FullBlock" }, ;
      { SC_SPECIAL2, "SC_SPECIAL2", "UpperHalf" } }

   LOCAL tmp

   ? "This lists the cursor modes, along with the expected shape"
   ? "Press a key after each example"
   IF hb_gtVersion() == "WIN"
      ? "NOTE: With GTWIN (Windows Console), SC_SPECIAL2 cannot be emulated (it is 2/3 size)"
   ENDIF
   ?
   FOR tmp := 1 TO Len( aCursors )
      SetCursor( aCursors[ tmp ][ 1 ] )
      ? ;
         PadR( aCursors[ tmp ][ 2 ], 12 ), ;
         PadR( aCursors[ tmp ][ 3 ], 10 ), ;
         hb_ntos( SetCursor() )
      Inkey( 0 )
   NEXT

   SetCursor( SC_NORMAL )

   RETURN
