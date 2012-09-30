/*
 * $Id$
 */

PROCEDURE Main()

   OutStd( hb_eol() )
   OutStd( "Running with SET FIXED OFF (the default setting): " )
   OutStd( hb_eol() )
   test()
   __ACCEPT( "Pause before running again with SET FIXED ON: " )
   OutStd( hb_eol() )
   OutStd( "Running with SET FIXED ON: " )
   OutStd( hb_eol() )
   Set( _SET_FIXED, "ON" )
   test()

   RETURN

PROCEDURE test()

   LOCAL a := 15.1
   LOCAL b := 10.0002575
   LOCAL nI, c, d

   OutStd( "1: " )
   OutStd( 10 )
   OutStd( a )
   OutStd( - a )
   OutStd( b )
   OutStd( - b )
   OutStd( hb_eol() )
   OutStd( "2: " )
   OutStd( a + b )
   OutStd( a - b )
   OutStd( a * b )
   OutStd( a / b )
   OutStd( hb_eol() )
   OutStd( "3: " )
   OutStd( a % b )
   OutStd( a ** b )
   OutStd( hb_eol() )

   c := a * b
   d := b * a
   OutStd( hb_eol() )
   OutStd( "4: " )
   OutStd( Str( c ) )
   OutStd( Str( d ) )
   OutStd( hb_eol() )
   OutStd( "5: " )
   OutStd( Str( c + d ) )
   OutStd( Str( c - d ) )
   OutStd( Str( c * d ) )
   OutStd( Str( c / d ) )
   OutStd( hb_eol() )

   OutStd( hb_eol() )
   OutStd( "6: " )
   OutStd( a + b + c )
   OutStd( c - b - a )
   OutStd( b * a * c )
   OutStd( b * a * c * d )
   b := 1.000213
   OutStd( b * b * b * b * b * b * b )
   OutStd( hb_eol() )

   FOR nI := 1 TO 20
      OutStd( hb_eol() )
      OutStd( LTrim( Str( 6 + nI ) ) + ": " )
      OutStd( 10 ** nI + ( 1.02 * 1.02 ) )
   NEXT
   OutStd( hb_eol() )

   OutStd( hb_eol() )
   OutStd( "27: " )
   OutStd( Str( a ), a )
   OutStd( hb_eol() )

   OutStd( "28: " )
   OutStd( Str( b ), b )
   OutStd( hb_eol() )

   OutStd( "29: " )
   OutStd( Str( b, 15 ) )
   OutStd( hb_eol() )

   OutStd( "30: " )
   OutStd( Str( b, 20, 5 ) )
   OutStd( hb_eol() )

   OutStd( "31: " )
   OutStd( Str( b, 20, 10 ) )
   OutStd( hb_eol() )

   OutStd( "32: " )
   OutStd( Str( b, 5, 10 ) )
   OutStd( hb_eol() )

   OutStd( "33: " )
   OutStd( Str( b, 20, - 10 ) )
   OutStd( hb_eol() )

   OutStd( "34: " )
   OutStd( Str( b, - 12, 7 ) )
   OutStd( hb_eol() )

   OutStd( "35: " )
   OutStd( Str( b, 0 ) )
   OutStd( hb_eol() )

   OutStd( hb_eol() )
   a := 15.1004
   OutStd( "36: " )
   OutStd( Str( a ), a )
   OutStd( hb_eol() )

   RETURN
