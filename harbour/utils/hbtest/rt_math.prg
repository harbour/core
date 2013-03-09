/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (math)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

PROCEDURE Main_MATH()

   LOCAL l, s, o

   MEMVAR s0, s1, v2
   PRIVATE s0 := "V2", s1 := "V", v2

   /* Log() */

   HBTEST Log( "A" )                      IS "E 1 BASE 1095 Argument error (LOG) OS:0 #:0 A:1:C:A F:S"
   HBTEST Str( Log( -1 ) )                IS "***********************"
// HBTEST Str( Log( 0 ) )                 IS "***********************"
   HBTEST Str( Log( 1 ) )                 IS "         0.00"
   HBTEST Str( Log( 12 ) )                IS "         2.48"
   HBTEST Str( Log( snIntP ) )            IS "         2.30"
#ifdef __HARBOUR__
   HBTEST Str( Log( @snIntP ) )           IS "         2.30"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1095 Argument error (LOG) OS:0 #:0 A:1:U:10 F:S" */
#endif

   /* Sqrt() */

   HBTEST Sqrt( "A" )                     IS "E 1 BASE 1097 Argument error (SQRT) OS:0 #:0 A:1:C:A F:S"
   HBTEST Sqrt( -1 )                      IS 0
   HBTEST Sqrt( 0 )                       IS 0
   HBTEST Sqrt( 4 )                       IS 2
   HBTEST Str( Sqrt( snIntP ) )           IS "         3.16"
#ifdef __HARBOUR__
   HBTEST Str( Sqrt( @snIntP ) )          IS "         3.16"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1097 Argument error (SQRT) OS:0 #:0 A:1:U:10 F:S" */
#endif
   HBTEST Str( Sqrt( 4 ), 21, 18 )        IS " 2.000000000000000000"
   HBTEST Str( Sqrt( 3 ), 21, 18 )        IS " 1.732050807568877000"

   /* Abs() */

   HBTEST Abs( "A" )                      IS "E 1 BASE 1089 Argument error (ABS) OS:0 #:0 A:1:C:A F:S"
   HBTEST Abs( 0 )                        IS 0
   HBTEST Abs( 10 )                       IS 10
   HBTEST Abs( -10 )                      IS 10
   HBTEST Str( Abs( snIntN ) )            IS "        10"
#ifdef __HARBOUR__
   HBTEST Str( Abs( @snIntN ) )           IS "        10"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1089 Argument error (ABS) OS:0 #:0 A:1:U:-10 F:S" */
#endif
   HBTEST Abs( Month( sdDate ) )          IS 3
   HBTEST Abs( -Month( sdDate ) )         IS 3
   HBTEST Str( Abs( Year( sdDateE ) ) )   IS "    0"
   HBTEST Str( Abs( -Year( sdDateE ) ) )  IS "         0"
   HBTEST Str( Abs( Year( sdDate ) ) )    IS " 1984"
   HBTEST Str( Abs( -Year( sdDate ) ) )   IS "      1984"
   HBTEST Str( Abs( Month( sdDate ) ) )   IS "  3"
   HBTEST Str( Abs( -Month( sdDate ) ) )  IS "         3"
   HBTEST Str( Abs( 0 ) )                 IS "         0"
   HBTEST Str( Abs( 0.0 ) )               IS "         0.0"
   HBTEST Str( Abs( -0 ) )                IS "         0"
   HBTEST Str( Abs( 150 ) )               IS "       150"
   HBTEST Str( Abs( -150 ) )              IS "       150"
   HBTEST Str( Abs( 150.245 ) )           IS "       150.245"
   HBTEST Str( Abs( -150.245 ) )          IS "       150.245"
   HBTEST Str( Abs( Val( "0" ) ) )        IS "0"
   HBTEST Str( Abs( Val( "-0" ) ) )       IS " 0"
   HBTEST Str( Abs( Val( "150" ) ) )      IS "150"
   HBTEST Str( Abs( Val( "-150" ) ) )     IS "       150"
   HBTEST Str( Abs( Val( "150.245" ) ) )  IS "       150.245"
   HBTEST Str( Abs( Val( "-150.245" ) ) ) IS "       150.245"
   HBTEST Abs( 0.1 )                      IS 0.1
   HBTEST Abs( 10.5 )                     IS 10.5
   HBTEST Abs( -10.7 )                    IS 10.7
   HBTEST Abs( 10.578 )                   IS 10.578
   HBTEST Abs( -10.578 )                  IS 10.578
   HBTEST Abs( 100000 )                   IS 100000
   HBTEST Abs( -100000 )                  IS 100000

   /* Exp() */

   HBTEST Exp( "A" )                      IS "E 1 BASE 1096 Argument error (EXP) OS:0 #:0 A:1:C:A F:S"
   HBTEST Exp( 0 )                        IS 1.00
   HBTEST Str( Exp( 15 ) )                IS "   3269017.37"
   HBTEST Str( Exp( snIntZ ) )            IS "         1.00"
#ifdef __HARBOUR__
   HBTEST Str( Exp( @snIntZ ) )           IS "         1.00"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1096 Argument error (EXP) OS:0 #:0 A:1:U:0 F:S" */
#endif
   HBTEST Round( Exp( 1 ), 2 )            IS 2.72
   HBTEST Str( Exp( 1 ), 20, 10 )         IS "        2.7182818285"
   HBTEST Round( Exp( 10 ), 2 )           IS 22026.47
   HBTEST Str( Exp( 10 ), 20, 10 )        IS "    22026.4657948067"

   /* Round() */

   HBTEST Round( snDoubleP, snIntZ )      IS 11
#ifdef __HARBOUR__
   HBTEST Round( @snDoubleP, @snIntZ )    IS 11  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:U:10.567;U:0 F:S" */
#endif
   HBTEST Round( NIL, 0 )                 IS "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:U:NIL;N:0 F:S"
   HBTEST Round( 0, NIL )                 IS "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:N:0;U:NIL F:S"
   HBTEST Round( 0, 0 )                   IS 0
   HBTEST Round( 0, 2 )                   IS 0.00
   HBTEST Round( 0, -2 )                  IS 0
   HBTEST Round( 0.5, 0 )                 IS 1
   HBTEST Round( 0.5, 1 )                 IS 0.5
   HBTEST Round( 0.5, 2 )                 IS 0.50
   HBTEST Round( 0.5, -1 )                IS 0
   HBTEST Round( 0.5, -2 )                IS 0
   HBTEST Round( 0.50, 0 )                IS 1
   HBTEST Round( 0.50, 1 )                IS 0.5
   HBTEST Round( 0.50, 2 )                IS 0.50
   HBTEST Round( 0.50, -1 )               IS 0
   HBTEST Round( 0.50, -2 )               IS 0
   HBTEST Round( 0.55, 0 )                IS 1
   HBTEST Round( 0.55, 1 )                IS 0.6
   HBTEST Round( 0.55, 2 )                IS 0.55
   HBTEST Round( 0.55, -1 )               IS 0
   HBTEST Round( 0.55, -2 )               IS 0
   HBTEST Round( 0.557, 0 )               IS 1
   HBTEST Round( 0.557, 1 )               IS 0.6
   HBTEST Round( 0.557, 2 )               IS 0.56
   HBTEST Round( 0.557, -1 )              IS 0
   HBTEST Round( 0.557, -2 )              IS 0
   HBTEST Round( 50, 0 )                  IS 50
   HBTEST Round( 50, 1 )                  IS 50.0
   HBTEST Round( 50, 2 )                  IS 50.00
   HBTEST Round( 50, -1 )                 IS 50
   HBTEST Round( 50, -2 )                 IS 100
   HBTEST Round( 10.50, 0 )               IS 11
   HBTEST Round( 10.50, -1 )              IS 10
   HBTEST Round( 500000, 0 )              IS 500000
   HBTEST Round( 500000, 1 )              IS 500000.0
   HBTEST Round( 500000, 2 )              IS 500000.00
   HBTEST Round( 500000, -1 )             IS 500000
   HBTEST Round( 500000, -2 )             IS 500000
   HBTEST Round( 500000, -2 )             IS 500000
   HBTEST Round( 5000000000, 0 )          IS 5000000000
   HBTEST Round( 5000000000, 1 )          IS 5000000000.0
   HBTEST Round( 5000000000, 2 )          IS 5000000000.00
   HBTEST Round( 5000000000, -1 )         IS 5000000000
   HBTEST Round( 5000000000, -2 )         IS 5000000000
   HBTEST Round( 5000000000, -2 )         IS 5000000000
   HBTEST Round( 5000000000.129, 0 )      IS 5000000000
   HBTEST Round( 5000000000.129, 1 )      IS 5000000000.1
   HBTEST Round( 5000000000.129, 2 )      IS 5000000000.13
   HBTEST Round( 5000000000.129, -1 )     IS 5000000000
   HBTEST Round( 5000000000.129, -2 )     IS 5000000000
   HBTEST Round( 5000000000.129, -2 )     IS 5000000000
   HBTEST Round( -0.5, 0 )                IS -1
   HBTEST Round( -0.5, 1 )                IS -0.5
   HBTEST Round( -0.5, 2 )                IS -0.50
   HBTEST Round( -0.5, -1 )               IS 0
   HBTEST Round( -0.5, -2 )               IS 0
   HBTEST Round( -0.50, 0 )               IS -1
   HBTEST Round( -0.50, 1 )               IS -0.5
   HBTEST Round( -0.50, 2 )               IS -0.50
   HBTEST Round( -0.50, -1 )              IS 0
   HBTEST Round( -0.50, -2 )              IS 0
   HBTEST Round( -0.55, 0 )               IS -1
   HBTEST Round( -0.55, 1 )               IS -0.6
   HBTEST Round( -0.55, 2 )               IS -0.55
   HBTEST Round( -0.55, -1 )              IS 0
   HBTEST Round( -0.55, -2 )              IS 0
   HBTEST Round( -0.557, 0 )              IS -1
   HBTEST Round( -0.557, 1 )              IS -0.6
   HBTEST Round( -0.557, 2 )              IS -0.56
   HBTEST Round( -0.557, -1 )             IS 0
   HBTEST Round( -0.557, -2 )             IS 0
   HBTEST Round( -50, 0 )                 IS -50
   HBTEST Round( -50, 1 )                 IS -50.0
   HBTEST Round( -50, 2 )                 IS -50.00
   HBTEST Round( -50, -1 )                IS -50
   HBTEST Round( -50, -2 )                IS -100
   HBTEST Round( -10.50, 0 )              IS -11
   HBTEST Round( -10.50, -1 )             IS -10
   HBTEST Round( -500000, 0 )             IS -500000
   HBTEST Round( -500000, 1 )             IS -500000.0
   HBTEST Round( -500000, 2 )             IS -500000.00
   HBTEST Round( -500000, -1 )            IS -500000
   HBTEST Round( -500000, -2 )            IS -500000
   HBTEST Round( -500000, -2 )            IS -500000
   HBTEST Round( -5000000000, 0 )         IS -5000000000
   HBTEST Round( -5000000000, 1 )         IS -5000000000.0
   HBTEST Round( -5000000000, 2 )         IS -5000000000.00
   HBTEST Round( -5000000000, -1 )        IS -5000000000
   HBTEST Round( -5000000000, -2 )        IS -5000000000
   HBTEST Round( -5000000000, -2 )        IS -5000000000
   HBTEST Round( -5000000000.129, 0 )     IS -5000000000
   HBTEST Round( -5000000000.129, 1 )     IS -5000000000.1
   HBTEST Round( -5000000000.129, 2 )     IS -5000000000.13
   HBTEST Round( -5000000000.129, -1 )    IS -5000000000
   HBTEST Round( -5000000000.129, -2 )    IS -5000000000
   HBTEST Round( -5000000000.129, -2 )    IS -5000000000

   /* Int() */

   HBTEST Int( NIL )                       IS "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Int( "A" )                       IS "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:C:A F:S"
   HBTEST Int( {} )                        IS "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:A:{.[0].} F:S"
   HBTEST Int( 0 )                         IS 0
   HBTEST Int( 0.0 )                       IS 0
   HBTEST Int( 10 )                        IS 10
   HBTEST Int( snIntP )                    IS 10
#ifdef __HARBOUR__
   HBTEST Int( @snIntP )                   IS 10  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:U:10 F:S" */
#endif
   HBTEST Int( -10 )                       IS -10
   HBTEST Int( 100000 )                    IS 100000
   HBTEST Int( -100000 )                   IS -100000
   HBTEST Int( 10.5 )                      IS 10
   HBTEST Int( -10.5 )                     IS -10
   HBTEST Str( Int( Val( "100.290" ) ) )   IS "100"
   HBTEST Str( Int( Val( "  100.290" ) ) ) IS "  100"
   HBTEST Str( Int( Val( " 100" ) ) )      IS " 100"
   HBTEST Int( 5000000000.90 )             IS 5000000000
   HBTEST Int( -5000000000.90 )            IS -5000000000
   HBTEST Int( 5000000000 )                IS 5000000000
   HBTEST Int( -5000000000 )               IS -5000000000
   HBTEST Int( 5000000000 ) / 100000       IS 50000
   HBTEST Int( -5000000000 ) / 100000      IS -50000

   /* Min()/Max() */

   HBTEST Max( NIL, NIL )                                     IS "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Max( 10, NIL )                                      IS "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:N:10;U:NIL F:S"
   HBTEST Max( hb_SToD( "19800101" ), 10 )                    IS "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:D:0d19800101;N:10 F:S"
   HBTEST Max( hb_SToD( "19800101" ), hb_SToD( "19800101" ) ) IS hb_SToD( "19800101" )
   HBTEST Max( hb_SToD( "19800102" ), hb_SToD( "19800101" ) ) IS hb_SToD( "19800102" )
   HBTEST Max( hb_SToD( "19800101" ), hb_SToD( "19800102" ) ) IS hb_SToD( "19800102" )
   HBTEST Max( snIntP, snLongP )                              IS 100000
#ifdef __HARBOUR__
   HBTEST Max( @snIntP, @snLongP )                            IS 100000  /* Bug in CA-Cl*pper, it will return: "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:U:10;U:100000 F:S" */
#endif
   HBTEST Min( NIL, NIL )                                     IS "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Min( 10, NIL )                                      IS "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:N:10;U:NIL F:S"
   HBTEST Min( hb_SToD( "19800101" ), 10 )                    IS "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:D:0d19800101;N:10 F:S"
   HBTEST Min( hb_SToD( "19800101" ), hb_SToD( "19800101" ) ) IS hb_SToD( "19800101" )
   HBTEST Min( hb_SToD( "19800102" ), hb_SToD( "19800101" ) ) IS hb_SToD( "19800101" )
   HBTEST Min( hb_SToD( "19800101" ), hb_SToD( "19800102" ) ) IS hb_SToD( "19800101" )
   HBTEST Min( snIntP, snLongP )                              IS 10
#ifdef __HARBOUR__
   HBTEST Min( @snIntP, @snLongP )                            IS 10      /* Bug in CA-Cl*pper, it will return: "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:U:10;U:100000 F:S" */
#endif

   /* Decimals handling */

   HBTEST Str( Max( 10, 12 )           )  IS "        12"
   HBTEST Str( Max( 10.50, 10 )        )  IS "        10.50"
   HBTEST Str( Max( 10, 9.50 )         )  IS "        10"
   HBTEST Str( Max( 100000, 10 )       )  IS "    100000"
   HBTEST Str( Max( 20.50, 20.670 )    )  IS "        20.670"
   HBTEST Str( Max( 20.5125, 20.670 )  )  IS "        20.670"
   HBTEST Str( Min( 10, 12 )           )  IS "        10"
   HBTEST Str( Min( 10.50, 10 )        )  IS "        10"
   HBTEST Str( Min( 10, 9.50 )         )  IS "         9.50"
   HBTEST Str( Min( 100000, 10 )       )  IS "        10"
   HBTEST Str( Min( 20.50, 20.670 )    )  IS "        20.50"
   HBTEST Str( Min( 20.5125, 20.670 )  )  IS "        20.5125"
   HBTEST Str( Val( "0x10" )           )  IS "   0"
   HBTEST Str( Val( "0X10" )           )  IS "   0"
   HBTEST Str( Val( "15E2" )           )  IS "  15"
   HBTEST Str( Val( "15E21" )          )  IS "   15"
   HBTEST Str( Val( "15.1A10" )        )  IS "15.1000"
   HBTEST Str( Val( "15.1A1" )         )  IS "15.100"
   HBTEST Str( Val( "A" )              )  IS "0"
   HBTEST Str( Val( "AAA0" )           )  IS "   0"
   HBTEST Str( Val( "AAA2" )           )  IS "   0"
   HBTEST Str( Val( "" )               )  IS "         0"
   HBTEST Str( Val( "0" )              )  IS "0"
   HBTEST Str( Val( " 0" )             )  IS " 0"
   HBTEST Str( Val( "-0" )             )  IS " 0"
   HBTEST Str( Val( "00" )             )  IS " 0"
   HBTEST Str( Val( "1" )              )  IS "1"
   HBTEST Str( Val( "15" )             )  IS "15"
   HBTEST Str( Val( "200" )            )  IS "200"
   HBTEST Str( Val( " 200" )           )  IS " 200"
   HBTEST Str( Val( "200 " )           )  IS " 200"
   HBTEST Str( Val( " 200 " )          )  IS "  200"
   HBTEST Str( Val( "-200" )           )  IS "-200"
   HBTEST Str( Val( " -200" )          )  IS " -200"
   HBTEST Str( Val( "-200 " )          )  IS " -200"
   HBTEST Str( Val( " -200 " )         )  IS "  -200"
   HBTEST Str( Val( "15.0" )           )  IS "15.0"
   HBTEST Str( Val( "15.00" )          )  IS "15.00"
   HBTEST Str( Val( "15.000" )         )  IS "15.000"
   HBTEST Str( Val( "15.001 " )        )  IS "15.0010"
   HBTEST Str( Val( "100000000" )      )  IS "100000000"
   HBTEST Str( Val( "5000000000" )     )  IS "5000000000"
   HBTEST Str( 10                      )  IS "        10"
   HBTEST Str( 15.0                    )  IS "        15.0"
   HBTEST Str( 10.1                    )  IS "        10.1"
   HBTEST Str( 15.00                   )  IS "        15.00"
// HBTEST Str( Log( 0 )                )  IS "***********************"
   HBTEST Str( 100.2 * 200.12          )  IS "     20052.024"
   HBTEST Str( 100.20 * 200.12         )  IS "     20052.0240"
   HBTEST Str( 1000.2 * 200.12         )  IS "    200160.024"
   HBTEST Str( 100 / 1000              )  IS "         0.10"
   HBTEST Str( 100 / 100000            )  IS "         0.00"
   HBTEST Str( 10 * 10                 )  IS "       100"
   HBTEST Str( 100 / 10                )  IS "        10"
   HBTEST Str( 100 / 13                )  IS "         7.69"
   HBTEST Str( 100.0 / 10              )  IS "        10.00"
   HBTEST Str( 100.0 / 10.00           )  IS "        10.00"
   HBTEST Str( 100.0 / 10.000          )  IS "        10.00"
   HBTEST Str( 100 / 10.00             )  IS "        10.00"
   HBTEST Str( 100 / 10.000            )  IS "        10.00"
   HBTEST Str( 100.00 / 10.0           )  IS "        10.00"
   HBTEST Str( sdDate - sdDateE        )  IS "   2445785"
   HBTEST Str( sdDate - sdDate         )  IS "         0"
   HBTEST Str( 1234567890 * 1234567890 )  IS " 1524157875019052000"  /* real val is 1524157875019052100 */

   /* Mod() */

   HBTEST Mod()                           IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Mod( NIL )                      IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Mod( 100 )                      IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:100;U:NIL F:S"
   HBTEST Mod( "A", "B" )                 IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:C:A;C:B F:S"
   HBTEST Mod( "A", 100 )                 IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:C:A;N:100 F:S"
   HBTEST Mod( 100, "B" )                 IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:100;C:B F:S"
   HBTEST Mod( NIL, NIL )                 IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Mod( 100, 60, "A" )             IS 40.00

   HBTEST Mod( 1, 0 )                     IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST Mod( 1, NIL )                   IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:1;U:NIL F:S"
   HBTEST Str( Mod( 1, 0   ) )            IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST Str( Mod( 2, 4   ) )            IS "         2.00"
   HBTEST Str( Mod( 4, 2   ) )            IS "         0.00"
   HBTEST Str( Mod( 4, 2.0 ) )            IS "         0.00"
   HBTEST Str( Mod( 2, 4.0 ) )            IS "         2.00"
   HBTEST Str( Mod( 8, 3   ) )            IS "         2.00"

   HBTEST Str( Mod(  3,  3 ) )            IS "         0.00"
   HBTEST Str( Mod(  3,  2 ) )            IS "         1.00"
   HBTEST Str( Mod(  3,  1 ) )            IS "         0.00"
   HBTEST Str( Mod(  3,  0 ) )            IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:3;N:0 F:S"
   HBTEST Str( Mod(  3, -1 ) )            IS "         0.00"
   HBTEST Str( Mod(  3, -2 ) )            IS "        -1.00"
   HBTEST Str( Mod(  3, -3 ) )            IS "         0.00"
   HBTEST Str( Mod( -3,  3 ) )            IS "         0.00"
   HBTEST Str( Mod( -3,  2 ) )            IS "         1.00"
   HBTEST Str( Mod( -3,  1 ) )            IS "         0.00"
   HBTEST Str( Mod( -3,  0 ) )            IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:-3;N:0 F:S"
   HBTEST Str( Mod( -3, -1 ) )            IS "         0.00"
   HBTEST Str( Mod( -3, -2 ) )            IS "        -1.00"
   HBTEST Str( Mod( -3, -3 ) )            IS "         0.00"
   HBTEST Str( Mod(  3,  3 ) )            IS "         0.00"
   HBTEST Str( Mod(  2,  3 ) )            IS "         2.00"
   HBTEST Str( Mod(  1,  3 ) )            IS "         1.00"
   HBTEST Str( Mod(  0,  3 ) )            IS "         0.00"
   HBTEST Str( Mod( -1,  3 ) )            IS "         2.00"
   HBTEST Str( Mod( -2,  3 ) )            IS "         1.00"
   HBTEST Str( Mod( -3,  3 ) )            IS "         0.00"
   HBTEST Str( Mod(  3, -3 ) )            IS "         0.00"
   HBTEST Str( Mod(  2, -3 ) )            IS "        -1.00"
   HBTEST Str( Mod(  1, -3 ) )            IS "        -2.00"
   HBTEST Str( Mod(  0, -3 ) )            IS "         0.00"
   HBTEST Str( Mod( -1, -3 ) )            IS "        -1.00"
   HBTEST Str( Mod( -2, -3 ) )            IS "        -2.00"
   HBTEST Str( Mod( -3, -3 ) )            IS "         0.00"

   /* <OP>assign and (pre/post)(inc/dec)rementation */
   o := ErrorNew()
   o:oscode := 1
   HBTEST o:oscode                        IS 1
   o:oscode++
   HBTEST o:oscode                        IS 2
   HBTEST o:oscode++                      IS 2
   ++o:oscode
   HBTEST o:oscode                        IS 4
   HBTEST ++o:oscode                      IS 5
   o:oscode += 10
   HBTEST o:oscode                        IS 15
   HBTEST o:oscode += 200                 IS 215

   l := 1
   HBTEST l                               IS 1
   l++
   HBTEST l                               IS 2
   HBTEST l++                             IS 2
   ++l
   HBTEST l                               IS 4
   HBTEST ++l                             IS 5
   l += 10
   HBTEST l                               IS 15
   HBTEST l += 200                        IS 215

   mnIntN := 1
   HBTEST mnIntN                          IS 1
   mnIntN++
   HBTEST mnIntN                          IS 2
   HBTEST mnIntN++                        IS 2
   ++mnIntN
   HBTEST mnIntN                          IS 4
   HBTEST ++mnIntN                        IS 5
   mnIntN += 10
   HBTEST mnIntN                          IS 15
   HBTEST mnIntN += 200                   IS 215

   snIntN := 1
   HBTEST snIntN                          IS 1
   snIntN++
   HBTEST snIntN                          IS 2
   HBTEST snIntN++                        IS 2
   ++snIntN
   HBTEST snIntN                          IS 4
   HBTEST ++snIntN                        IS 5
   snIntN += 10
   HBTEST snIntN                          IS 15
   HBTEST snIntN += 200                   IS 215

#ifdef __HARBOUR__

   o := ErrorNew()
   s := "oscode"
   o:&s := 1
   HBTEST o:&(s)                          IS 1
   o:&s++
   HBTEST o:&(s)                          IS 2
   HBTEST o:&(s)++                        IS 2
   ++o:&s
   HBTEST o:&(s)                          IS 4
   HBTEST ++o:&(s)                        IS 5
   o:&s += 10
   HBTEST o:&(s)                          IS 15
   HBTEST o:&(s) += 200                   IS 215

   WITH OBJECT ErrorNew()
      :&(s) := 1
      HBTEST :&(s)                           IS 1
      :&(s)++
      HBTEST :&(s)                           IS 2
      HBTEST :&(s)++                         IS 2
      ++:&(s)
      HBTEST :&(s)                           IS 4
      HBTEST ++:&(s)                         IS 5
      :&(s) += 10
      HBTEST :&(s)                           IS 15
      HBTEST :&(s) += 200                    IS 215
   ENDWITH

   WITH OBJECT ErrorNew()
      :oscode := 1
      HBTEST :oscode                         IS 1
      :oscode++
      HBTEST :oscode                         IS 2
      HBTEST :oscode++                       IS 2
      ++:oscode
      HBTEST :oscode                         IS 4
      HBTEST ++:oscode                       IS 5
      :oscode += 10
      HBTEST :oscode                         IS 15
      HBTEST :oscode += 200                  IS 215
   ENDWITH

   &s0 := 1
   HBTEST &s0                             IS 1
   &s0++
   HBTEST &s0                             IS 2
   HBTEST &(s0)++                         IS 2
   ++&s0
   HBTEST &s0                             IS 4
   HBTEST ++&(s0)                         IS 5
   &s0 += 10
   HBTEST &s0                             IS 15
   HBTEST &(s0) += 200                    IS 215

   &s1.2 := 1
   HBTEST &s1.2                           IS 1
   &s1.2++
   HBTEST &s1.2                           IS 2
   HBTEST &s1.2++                         IS 2
   ++&s1.2
   HBTEST &s1.2                           IS 4
   HBTEST ++&s1.2                         IS 5
   &s1.2 += 10
   HBTEST &s1.2                           IS 15
   HBTEST &s1.2 += 200                    IS 215

#endif

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
