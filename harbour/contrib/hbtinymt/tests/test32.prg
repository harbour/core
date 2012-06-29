/*
 * $Id$
 */

/*
 * Test program for HBTINYMT library
 * Andi Jahja
 */

#require "hbtinymt"

PROCEDURE Main()

   LOCAL i, j

   SET DECIMAL TO 0

   IF hb_tinymt32_init( 0x123, 0x234, 0x345 )
      OutStd( "32-bit unsigned integers r, where 0 <= r < 2^32" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( HB_TINYMT32_GENERATE_UINT32(), 11 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
   ENDIF
   OutStd( hb_eol() )

   SET DECIMAL TO 12

   IF hb_tinymt32_init_by_array( { 0x123, 0x234, 0x345 } )
      OutStd( "Float numbers r, where 0.0 <= r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_float(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Float numbers r, where 0.0 <= r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_float01(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Float numbers r, where 1.0 <= r < 2.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_float12(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Float numbers r, where 0.0 < r <= 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_floatoc(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Float numbers r, where 0.0 < r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_floatoo(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "32-bit precision double numbers r, where 0.0 <= r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt32_generate_32double(), 15 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
   ENDIF

   RETURN
