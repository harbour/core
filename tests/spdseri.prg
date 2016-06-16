#include "hbserial.ch"

procedure Main()

   local a, b, t

   ? "init...."
   a := Array( 1000000 )
   AEval( a, {| x, i | ( x ), a[ i ] := Array( 5 ) } )

   t := Seconds()
   while Seconds() - t < 1
   enddo

   ? "AClone()...", ""
   t := Seconds()
   b := AClone( a )
   ?? Seconds() - t, "sec."

   ? "hb_Serialize( HB_SERIALIZE_IGNOREREF )...", ""
   t := Seconds()
   hb_Serialize( b, HB_SERIALIZE_IGNOREREF )
   ?? Seconds() - t, "sec."

   ? "hb_Serialize()...", ""
   t := Seconds()
   a := hb_Serialize( b )
   ?? Seconds() - t, "sec."

   ? "hb_Deserialize()...", ""
   t := Seconds()
   b := hb_Deserialize( a )
   ?? Seconds() - t, "sec."

   ? "array size:", hb_ntos( Len( b ) ), ;
               "x", hb_ntos( Len( ATail( b ) ) )

   ?

   return
