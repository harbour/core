/*
 * $Id$
 */

#require "hbtinymt"

PROCEDURE Main()

   LOCAL i, j

   SET DECIMAL TO 20

   IF hb_tinymt64_init( 0x123, 0x234, 0x345 )
      OutStd( "64-bit unsigned integers r, where 0 <= r < 2^64" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( HB_TINYMT64_GENERATE_UINT64(), 22 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
   ENDIF
   OutStd( hb_eol() )

   SET DECIMAL TO 12

   IF hb_tinymt64_init_by_array( { 0x123, 0x234, 0x345 } )
      OutStd( "Double numbers r, where 0.0 <= r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt64_generate_double(), 30 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Double numbers r, where 0.0 < r <= 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt64_generate_double01(), 30 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Double numbers r, where 1.0 <= r < 2.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt64_generate_double12(), 30 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Double numbers r, where 0.0 < r <= 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt64_generate_doubleoc(), 30 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )

      OutStd( "Double numbers r, where 0.0 < r < 1.0" )
      OutStd( hb_eol() )
      FOR i := 1 TO 10
         FOR j := 1 TO 5
            OutStd( PadL( hb_tinymt64_generate_doubleoo(), 30 ) )
         NEXT
         OutStd( hb_eol() )
      NEXT
      OutStd( hb_eol() )
      OutStd( hb_eol() )
   ENDIF

   RETURN
