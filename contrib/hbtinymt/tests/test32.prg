/* Test program for HBTINYMT library by Andi Jahja */

#require "hbtinymt"

PROCEDURE Main()

   Set( _SET_DECIMALS, 0 )

   IF tinymt32_init( 0x123, 0x234, 0x345 )
      TestMatrix( "32-bit unsigned integers r, where 0 <= r < 2^32", {|| PadL( tinymt32_generate_uint32(), 11 ) } )
   ENDIF

   Set( _SET_DECIMALS, 12 )

   IF tinymt32_init_by_array( { 0x123, 0x234, 0x345 } )
      TestMatrix( "Float numbers r, where 0.0 <= r <  1.0", {|| PadL( tinymt32_generate_float(), 15 ) } )
      TestMatrix( "Float numbers r, where 0.0 <= r <  1.0", {|| PadL( tinymt32_generate_float01(), 15 ) } )
      TestMatrix( "Float numbers r, where 1.0 <= r <  2.0", {|| PadL( tinymt32_generate_float12(), 15 ) } )
      TestMatrix( "Float numbers r, where 0.0 <  r <= 1.0", {|| PadL( tinymt32_generate_floatOC(), 15 ) } )
      TestMatrix( "Float numbers r, where 0.0 <  r <  1.0", {|| PadL( tinymt32_generate_floatOO(), 15 ) } )
      TestMatrix( "32-bit precision double numbers r, where 0.0 < r <= 1.0", {|| PadL( tinymt32_generate_32double(), 15 ) } )
   ENDIF

   RETURN

STATIC PROCEDURE TestMatrix( cDescription, bBlock )

   LOCAL i, j

   ? cDescription
   FOR i := 1 TO 10
      FOR j := 1 TO 5
         ?? Eval( bBlock )
      NEXT
      ?
   NEXT
   ?

   RETURN
