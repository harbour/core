#require "hbtinymt"

PROCEDURE Main()

   SET DECIMAL TO 0

   IF tinymt64_init( 0x123, 0x234, 0x345 )
      TestMatrix( "64-bit unsigned integers r, where 0 <= r < 2^64", {|| PadL( tinymt64_generate_uint64(), 22 ) } )
   ENDIF

   SET DECIMAL TO 22

   IF tinymt64_init_by_array( { 0x123, 0x234, 0x345 } )
      TestMatrix( "Double numbers r, where 0.0 <= r <  1.0", {|| PadL( tinymt64_generate_double(), 30 ) } )
      TestMatrix( "Double numbers r, where 0.0 <= r <  1.0", {|| PadL( tinymt64_generate_double01(), 30 ) } )
      TestMatrix( "Double numbers r, where 1.0 <= r <  2.0", {|| PadL( tinymt64_generate_double12(), 30 ) } )
      TestMatrix( "Double numbers r, where 0.0 <  r <= 1.0", {|| PadL( tinymt64_generate_doubleOC(), 30 ) } )
      TestMatrix( "Double numbers r, where 0.0 <  r <  1.0", {|| PadL( tinymt64_generate_doubleOO(), 30 ) } )
   ENDIF

   RETURN

STATIC PROCEDURE TestMatrix( cDescription, bBlock )

   LOCAL i, j

   ? cDescription
   FOR i := 1 TO 10
      FOR j := 1 TO 5
         ? Eval( bBlock )
      NEXT
      ?
   NEXT
   ?

   RETURN
