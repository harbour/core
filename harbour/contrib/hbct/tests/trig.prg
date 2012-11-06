/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 TRIGONOMETRIC functions - PART 1
 *   - PI
 *   - SIN
 *   - COS
 *   - TAN
 *   - COT
 *   - ASIN
 *   - ACOS
 *   - ATAN
 *   - ATN2
 *   - SINH
 *   - COSH
 *   - TANH
 * Copyright 2001   Alejandro de garate  <alex_degarate@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

PROCEDURE Main()

   LOCAL X, Y

   ctinit()

   SET DECIMAL TO 14
   CLS
   ? "Begin test of Trigonometric functions... "
   ?
   ? "PI = " + Str( Pi(), 18, 15 )

   ? "STR( SIN( Pi() / 4 ), 18, 15 ) =  0.707106781186548   // CT3"
   ? Space( 33 ) + Str( Sin( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( SIN( Pi() / 2 ), 18, 15 ) =  1.000000000000000   // CT3"
   ? Space( 33 ) + Str( Sin( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( SIN( Pi() * 99.5 ), 18, 15 ) = -1.000000000000000   // CT3"
   ? Space( 36 ) + Str( Sin( Pi() * 99.5 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( SIN( Pi() / 9 ), 18, 15 ) =  0.342020143325669   // CT3"
   ? Space( 33 ) + Str( Sin( Pi() / 9 ), 18, 15 ) + "  <-- CT for Harbour"

   WAIT4()


   ? "STR( COS( 0 ), 18, 15 ) =  1.000000000000000   // CT3"
   ? Space( 26 ) + Str( Cos( 0 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COS( Pi() / 4 ), 18, 15 ) =  0.707106781186548   // CT3"
   ? Space( 33 ) + Str( Cos( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COS( Pi() / 2 ), 18, 15 ) =  0.000000000000000   // CT3"
   ? Space( 33 ) + Str( Cos( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COS( Pi() * 99.5 ), 18, 15 ) = 0.000000000000000   // CT3"
   ? Space( 35 ) + Str( Cos( Pi() * 99.5 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COS( Pi() / 9 ), 18, 15 ) =  0.939692620785908   // CT3"
   ? Space( 33 ) + Str( Cos( Pi() / 9 ), 18, 15 ) + "  <-- CT for Harbour"
   ?


   WAIT4()


   ? "STR( TAN( 0 ), 18, 15 ) =  1.000000000000000   // CT3 wrong ! "
   ? Space( 26 ) + Str( Tan( 0 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( TAN( Pi() / 4 ), 18, 15 ) =  1.000000000000000   // CT3"
   ? Space( 33 ) + Str( Tan( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( TAN( Pi() / 9 ), 18, 15 ) =  0.363970234266202   // CT3"
   ? Space( 33 ) + Str( Tan( Pi() / 9 ), 18, 15 )  + "  <-- CT for Harbour"
   ?
   ?
   ?

   ? "STR( ASIN( 0.5 ), 18, 15 ) =  0.523598775598299   // CT3"
   ? Space( 29 ) + Str( Asin( 0.5 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( ACOS( 0.7 ), 18, 15 ) =  0.795398830184144   // CT3"
   ? Space( 29 ) + Str( Acos( 0.7 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( ATAN( Pi() / 4 ), 18, 15 ) =  0.665773750028354   // CT3"
   ? Space( 34 ) + Str( Atan( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   WAIT4()


   ? "STR( COT( Pi() / 4 ), 18, 15 ) =  1.000000000000000   // CT3"
   ? Space( 33 ) + Str( Cot( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COT( Pi() / 2 ), 18, 15 ) =  0.000000000000000   // CT3"
   ? Space( 33 ) + Str( Cot( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COT( Pi() / 9 ), 18, 15 ) =  2.747477419454622   // CT3"
   ? Space( 33 ) + Str( Cot( Pi() / 9 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   WAIT4()

   ?? "Testing Hiperbolic Sine..."
   ?
   ? "STR( SINH( Pi() / 2 ), 18, 15 ) =  2.301298902307295   // CT3"
   ? Space( 34 ) + Str( Sinh( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( SINH( Pi() / 4 ), 18, 15 ) =  0.868670961486010   // CT3"
   ? Space( 34 ) + Str( Sinh( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "Testing Hiperbolic Cosine..."
   ?
   ? "STR( COSH( Pi() / 2 ), 18, 15 ) =  2.509178478658057   // CT3"
   ? Space( 34 ) + Str( Cosh( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "STR( COSH( Pi() / 4 ), 18, 15 ) =  1.324609089252006   // CT3"
   ? Space( 34 ) + Str( Cosh( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   ? "Testing Hiperbolic Tangent..."
   ?
   ? "STR( TANH( Pi() / 2 ), 18, 15 ) =  0.917152335667274   // CT3"
   ? Space( 34 ) + Str( Tanh( Pi() / 2 ), 18, 15 ) + "  <-- CT for Harbour"
   ?
   ? "STR( TANH( Pi() / 4 ), 18, 15 ) =  0.655794202632672   // CT3"
   ? Space( 34 ) + Str( Tanh( Pi() / 4 ), 18, 15 ) + "  <-- CT for Harbour"
   ?

   WAIT4()

   ? "Testing Degree TO Radian..."
   ?
   ? "STR( DTOR( 360 ), 18, 15 ) = 6.283185307179588    // CT3"
   ?  Space( 28 ) + Str( DToR( 360 ), 18, 15 ), " <-- CT for Harbour "
   ?

   ? "STR( DTOR( 180 ), 18, 15 ) = 3.141592653589794    // CT3"
   ?  Space( 28 ) + Str( DToR( 180 ), 18, 15 ), " <-- CT for Harbour "
   ?

   ? "STR( DTOR( 180.5 ), 18, 15 ) = 3.150319299849766   // CT3"
   ?  Space( 30 ) + Str( DToR( 180.5 ), 18, 15 ), " <-- CT for Harbour "
   ?

   ? "STR( DTOR( 720 ), 18, 15 ) = 12.566370614359180    // CT3"
   ?  Space( 29 ) + Str( DToR( 720 ), 18, 15 ), " <-- CT for Harbour "
   ?

   ? "STR( DTOR( -180 ), 18, 15 ) = -3.141592653589794   // CT3"
   ?  Space( 30 ) + Str( DToR( -180 ), 18, 15 ), " <-- CT for Harbour "

   WAIT4()


   ? "Testing Radian TO Degree..."
   ?
   ? "RTOD( Pi() ) = 180             // CT3  "
   ? Space( 7 ), RToD( Pi() ), " <-- CT for Harbour "

   ? "RTOD( 2 * Pi() ) = 360             // CT3  "
   ? Space( 11 ), RToD( 2 * Pi() ), " <-- CT for Harbour "

   ? "RTOD( 4 * Pi() ) = 720             // CT3  "
   ? Space( 11 ), RToD( 4 * Pi() ), " <-- CT for Harbour "

   ? "RTOD( -Pi() ) = -180             // CT3"
   ? Space( 9 ), RToD( -Pi() ), " <-- CT for Harbour "
   ?

   WAIT4()


/* NOTE: ATN2( x, y) have the parameters inverted, when
         comparing with the standard C languaje ATAN2( y, x)
*/


   ? "Testing ATN2( x, y )... where:"

   x := Sin( DToR( 30 ) )
   y := Cos( DToR( 30 ) )
   ? "x = SIN( DTOR( 30 ) ) =", x
   ? "y = COS( DTOR( 30 ) ) =", y
   ?
   ? "STR( ATN2( x, y ), 18, 15 ) =  0.523598775598299   // CT3"
   ? Space( 30 ) + Str( Atn2( x, y ), 18, 15 ) + "  <-- CT for Harbour"
   ?
   ? "RTOD( ATN2( x, y ) ) =" + Str( RToD( Atn2( x, y ) ), 18, 4 ) + "  <-- CT for Harbour"
   ?

   WAIT4()

   ctexit()

   RETURN

PROCEDURE WAIT4

   ? "  PRESS ANY KEY"
   Inkey( 0 )
   CLS

   RETURN
