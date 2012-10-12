/*
 * $Id$
 */

PROCEDURE Main()

   ? "Running with SET FIXED OFF (the default setting):"
   ?
   test()
   __ACCEPT( "Pause before running again with SET FIXED ON: " )
   ? "Running with SET FIXED ON:"
   ?
   Set( _SET_FIXED, "ON" )
   test()

   RETURN

PROCEDURE test()

   LOCAL a := 15.1
   LOCAL b := 10.0002575
   LOCAL nI, c, d

   ?? "1: "
   ?? 10
   ?? a
   ?? -a
   ?? b
   ?? -b
   ?

   ?? "2: "
   ?? a + b
   ?? a - b
   ?? a * b
   ?? a / b
   ?

   ?? "3: "
   ?? a % b
   ?? a ** b
   ?

   c := a * b
   d := b * a
   ?

   ?? "4: "
   ?? Str( c )
   ?? Str( d )
   ?

   ?? "5: "
   ?? Str( c + d )
   ?? Str( c - d )
   ?? Str( c * d )
   ?? Str( c / d )
   ?

   ?
   ?? "6: "
   ?? a + b + c
   ?? c - b - a
   ?? b * a * c
   ?? b * a * c * d
   b := 1.000213
   ?? b * b * b * b * b * b * b
   ?

   FOR nI := 1 TO 20
      ?
      ?? Str( 6 + nI ) + ": "
      ?? 10 ** nI + ( 1.02 * 1.02 )
   NEXT
   ?

   ?
   ?? "27: "
   ?? Str( a ), a
   ?

   ?? "28: "
   ?? Str( b ), b
   ?

   ?? "29: "
   ?? Str( b, 15 )
   ?

   ?? "30: "
   ?? Str( b, 20, 5 )
   ?

   ?? "31: "
   ?? Str( b, 20, 10 )
   ?

   ?? "32: "
   ?? Str( b, 5, 10 )
   ?

   ?? "33: "
   ?? Str( b, 20, -10 )
   ?

   ?? "34: "
   ?? Str( b, -12, 7 )
   ?

   ?? "35: "
   ?? Str( b, 0 )
   ?

   ?
   a := 15.1004
   ?? "36: "
   ?? Str( a ), a
   ?

   RETURN
