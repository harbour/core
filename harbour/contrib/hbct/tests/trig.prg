/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *   Test CT3 TRIGONOMETRIC functions - PART 1
 *
 * - PI
 * - SIN
 * - COS
 * - TAN
 * - COT
 * - ASIN
 * - ACOS
 * - ATAN
 * - ATN2
 * - SINH
 * - COSH
 * - TANH
 *
 * Copyright 2001   Alejandro de garate  <alex_degarate@hotmail.com>
 *
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



 PROCEDURE MAIN

 local X, Y

 CTINIT()

 SET DECIMAL TO 14
 CLS
 ? "Begin test of Trigonometric functions... "
 ?
 ? "PI = " + STR( PI(), 18, 15 )

 ? "STR( SIN( PI() /4 ), 18, 15 ) =  0.707106781186548   // CT3"
 ? SPACE(32) + STR( SIN( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( SIN( PI() /2 ), 18, 15 ) =  1.000000000000000   // CT3"
 ? SPACE(32) + STR( SIN( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( SIN( PI() *99.5 ), 18, 15 ) = -1.000000000000000   // CT3"
 ? SPACE(35) + STR( SIN( PI() * 99.5 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( SIN( PI() /9 ), 18, 15 ) =  0.342020143325669   // CT3"
 ? SPACE(32) + STR( SIN( PI() / 9 ), 18, 15 ) + "  <-- CT for Harbour"

  WAIT4()


 ? "STR( COS( 0 ), 18, 15 ) =  1.000000000000000   // CT3"
 ? SPACE(26) + STR( COS( 0 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COS( PI() /4 ), 18, 15 ) =  0.707106781186548   // CT3"
 ? SPACE(32) + STR( COS( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COS( PI() /2 ), 18, 15 ) =  0.000000000000000   // CT3"
 ? SPACE(32) + STR( COS( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COS( PI() *99.5 ), 18, 15 ) = 0.000000000000000   // CT3"
 ? SPACE(34) + STR( COS( PI() *99.5), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COS( PI() /9 ), 18, 15 ) =  0.939692620785908   // CT3"
 ? SPACE(32) + STR( COS( PI() / 9 ), 18, 15 ) + "  <-- CT for Harbour"
 ?


  WAIT4()


 ? "STR( TAN( 0 ), 18, 15 ) =  1.000000000000000   // CT3 wrong ! "
 ? SPACE(26) + STR( TAN( 0 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( TAN( PI() /4 ), 18, 15 ) =  1.000000000000000   // CT3"
 ? SPACE(32) + STR( TAN( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( TAN( PI() /9 ), 18, 15 ) =  0.363970234266202   // CT3"
 ? SPACE(32) + STR( TAN( PI() / 9 ), 18, 15 )  + "  <-- CT for Harbour"
 ?
 ?
 ?

 ? "STR( ASIN( 0.5 ), 18, 15 ) =  0.523598775598299   // CT3"
 ? SPACE(29) + STR( ASIN( 0.5 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( ACOS( 0.7 ), 18, 15 ) =  0.795398830184144   // CT3"
 ? SPACE(29) + STR( ACOS( 0.7 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( ATAN( PI() /4 ), 18, 15 ) =  0.665773750028354   // CT3"
 ? SPACE(33) + STR( ATAN( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

  WAIT4()


 ? "STR( COT( PI() /4 ), 18, 15 ) =  1.000000000000000   // CT3"
 ? SPACE(32) + STR( COT( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COT( PI() /2 ), 18, 15 ) =  0.000000000000000   // CT3"
 ? SPACE(32) + STR( COT( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COT( PI() /9 ), 18, 15 ) =  2.747477419454622   // CT3"
 ? SPACE(32) + STR( COT( PI() / 9 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

  WAIT4()

 ?? "Testing Hiperbolic Sine..."
 ?
 ? "STR( SINH( PI() /2 ), 18, 15 ) =  2.301298902307295   // CT3"
 ? SPACE(33) + STR( SINH( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( SINH( PI() /4 ), 18, 15 ) =  0.868670961486010   // CT3"
 ? SPACE(33) + STR( SINH( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "Testing Hiperbolic Cosine..."
 ?
 ? "STR( COSH( PI() /2 ), 18, 15 ) =  2.509178478658057   // CT3"
 ? SPACE(33) + STR( COSH( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "STR( COSH( PI() /4 ), 18, 15 ) =  1.324609089252006   // CT3"
 ? SPACE(33) + STR( COSH( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 ? "Testing Hiperbolic Tangent..."
 ?
 ? "STR( TANH( PI() /2 ), 18, 15 ) =  0.917152335667274   // CT3"
 ? SPACE(33) + STR( TANH( PI() /2 ), 18, 15 ) + "  <-- CT for Harbour"
 ?
 ? "STR( TANH( PI() /4 ), 18, 15 ) =  0.655794202632672   // CT3"
 ? SPACE(33) + STR( TANH( PI() /4 ), 18, 15 ) + "  <-- CT for Harbour"
 ?

 WAIT4()

 ? "Testing Degree TO Radian..."
 ?
 ? "STR( DTOR( 360), 18, 15 ) = 6.283185307179588    // CT3"
 ?  SPACE(27) + STR( DTOR( 360), 18, 15 ), " <-- CT for Harbour "
 ?

 ? "STR( DTOR( 180), 18, 15 ) = 3.141592653589794    // CT3"
 ?  SPACE(27) + STR( DTOR( 180), 18, 15 ), " <-- CT for Harbour "
 ?

 ? "STR( DTOR( 180.5), 18, 15 ) = 3.150319299849766   // CT3"
 ?  SPACE(29) + STR( DTOR( 180.5), 18, 15 ), " <-- CT for Harbour "
 ?

 ? "STR( DTOR( 720), 18, 15 ) = 12.566370614359180    // CT3"
 ?  SPACE(28) + STR( DTOR( 720), 18, 15 ), " <-- CT for Harbour "
 ?

 ? "STR( DTOR( -180), 18, 15 ) = -3.141592653589794   // CT3"
 ?  SPACE(29) + STR( DTOR( -180), 18, 15 ), " <-- CT for Harbour "

  WAIT4()


 ? "Testing Radian TO Degree..."
 ?
 ? "RTOD( PI() ) = 180             // CT3  "
 ? SPACE(7), RTOD( PI() ), " <-- CT for Harbour "

 ? "RTOD( 2 * PI()) = 360             // CT3  "
 ? SPACE(10), RTOD( 2 * PI() ), " <-- CT for Harbour "

 ? "RTOD( 4 * PI()) = 720             // CT3  "
 ? SPACE(10), RTOD( 4 * PI() ), " <-- CT for Harbour "

 ? "RTOD( -PI() ) = -180             // CT3"
 ? SPACE(9), RTOD( -PI() ), " <-- CT for Harbour "
 ?

  WAIT4()


/* NOTE: ATN2( x, y) have the parameters inverted, when
         comparing with the standard C languaje ATAN2( y, x)
*/


 ? "Testing ATN2( x, y )... where:"

 x := SIN( DTOR( 30 ) )
 y := COS( DTOR( 30 ) )
 ? "x = SIN( DTOR( 30 ) ) =", x
 ? "y = COS( DTOR( 30 ) ) =", y
 ?
 ? "STR( ATN2( x, y  ), 18, 15 ) =  0.523598775598299   // CT3"
 ? SPACE(31) + STR( ATN2( x, y), 18, 15 ) + "  <-- CT for Harbour"
 ?
 ? "RTOD( ATN2( x, y)) ="+ STR( RTOD( ATN2( x,y)), 18,4) +"  <-- CT for Harbour"
 ?

 WAIT4()

 CTEXIT()

RETURN


PROCEDURE WAIT4
 ? "  PRESS ANY KEY"
 INKEY(0)
 CLS

RETURN
