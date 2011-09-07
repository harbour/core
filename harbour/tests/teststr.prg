/*
 * $Id$
 */

STATIC s_cNewLine

PROCEDURE Main( cParam )

   IF Empty( cParam )
      s_cNewLine := Chr( 13 ) + Chr( 10 )
   ELSE
      s_cNewLine := Chr( 10 )
   ENDIF

   OutStd( s_cNewLine )
   OutStd( "Running with SET FIXED OFF (the default setting): " )
   OutStd( s_cNewLine )
   test()
   __ACCEPT( "Pause before running again with SET FIXED ON: " )
   OutStd( s_cNewLine )
   OutStd( "Running with SET FIXED ON: " )
   OutStd( s_cNewLine )
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
   OutStd( s_cNewLine )
   OutStd( "2: " )
   OutStd( a + b )
   OutStd( a - b )
   OutStd( a * b )
   OutStd( a / b )
   OutStd( s_cNewLine )
   OutStd( "3: " )
   OutStd( a % b )
   OutStd( a ** b )
   OutStd( s_cNewLine )

   c := a * b
   d := b * a
   OutStd( s_cNewLine )
   OutStd( "4: " )
   OutStd( Str( c ) )
   OutStd( Str( d ) )
   OutStd( s_cNewLine )
   OutStd( "5: " )
   OutStd( Str( c + d ) )
   OutStd( Str( c - d ) )
   OutStd( Str( c * d ) )
   OutStd( Str( c / d ) )
   OutStd( s_cNewLine )

   OutStd( s_cNewLine )
   OutStd( "6: " )
   OutStd( a + b + c )
   OutStd( c - b - a )
   OutStd( b * a * c )
   OutStd( b * a * c * d )
   b := 1.000213
   OutStd( b * b * b * b * b * b * b )
   OutStd( s_cNewLine )

   FOR nI := 1 TO 20
      OutStd( s_cNewLine )
      OutStd( LTrim( Str( 6 + nI ) ) + ": " )
      OutStd( 10 ** nI + ( 1.02 * 1.02 ) )
   NEXT
   OutStd( s_cNewLine )

   OutStd( s_cNewLine )
   OutStd( "27: " )
   OutStd( Str( a ), a )
   OutStd( s_cNewLine )

   OutStd( "28: " )
   OutStd( Str( b ), b )
   OutStd( s_cNewLine )

   OutStd( "29: " )
   OutStd( Str( b, 15 ) )
   OutStd( s_cNewLine )

   OutStd( "30: " )
   OutStd( Str( b, 20, 5 ) )
   OutStd( s_cNewLine )

   OutStd( "31: " )
   OutStd( Str( b, 20, 10 ) )
   OutStd( s_cNewLine )

   OutStd( "32: " )
   OutStd( Str( b, 5, 10 ) )
   OutStd( s_cNewLine )

   OutStd( "33: " )
   OutStd( Str( b, 20, - 10 ) )
   OutStd( s_cNewLine )

   OutStd( "34: " )
   OutStd( Str( b, - 12, 7 ) )
   OutStd( s_cNewLine )

   OutStd( "35: " )
   OutStd( Str( b, 0 ) )
   OutStd( s_cNewLine )

   OutStd( s_cNewLine )
   a := 15.1004
   OutStd( "36: " )
   OutStd( Str( a ), a )
   OutStd( s_cNewLine )

   RETURN
