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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

PROCEDURE Main_MATH()

   LOCAL l, s, o
   MEMVAR s0, s1, v2
   PRIVATE s0 := "V2", s1 := "V", v2

   /* Log() */

   TEST_LINE( Log("A")                        , "E 1 BASE 1095 Argument error (LOG) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Str(Log(-1))                    , "***********************"              )
// TEST_LINE( Str(Log(0))                     , "***********************"              )
   TEST_LINE( Str(Log(1))                     , "         0.00"                        )
   TEST_LINE( Str(Log(12))                    , "         2.48"                        )
   TEST_LINE( Str(Log(snIntP))                , "         2.30"                        )
#ifdef __HARBOUR__
   TEST_LINE( Str(Log(@snIntP))               , "         2.30"                        ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1095 Argument error (LOG) OS:0 #:0 A:1:U:10 F:S" */
#endif

   /* Sqrt() */

   TEST_LINE( Sqrt("A")                       , "E 1 BASE 1097 Argument error (SQRT) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Sqrt(-1)                        , 0                                      )
   TEST_LINE( Sqrt(0)                         , 0                                      )
   TEST_LINE( Sqrt(4)                         , 2                                      )
   TEST_LINE( Str(Sqrt(snIntP))               , "         3.16"                        )
#ifdef __HARBOUR__
   TEST_LINE( Str(Sqrt(@snIntP))              , "         3.16"                        ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1097 Argument error (SQRT) OS:0 #:0 A:1:U:10 F:S" */
#endif
   TEST_LINE( Str(Sqrt(4),21,18)              , " 2.000000000000000000"                )
   TEST_LINE( Str(Sqrt(3),21,18)              , " 1.732050807568877000"                )

   /* Abs() */

   TEST_LINE( Abs("A")                        , "E 1 BASE 1089 Argument error (ABS) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Abs(0)                          , 0                                      )
   TEST_LINE( Abs(10)                         , 10                                     )
   TEST_LINE( Abs(-10)                        , 10                                     )
   TEST_LINE( Str(Abs(snIntN))                , "        10"                           )
#ifdef __HARBOUR__
   TEST_LINE( Str(Abs(@snIntN))               , "        10"                           ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1089 Argument error (ABS) OS:0 #:0 A:1:U:-10 F:S" */
#endif
   TEST_LINE( Abs(Month(sdDate))              , 3                                      )
   TEST_LINE( Abs(-Month(sdDate))             , 3                                      )
   TEST_LINE( Str(Abs(Year(sdDateE)))         , "    0"                                )
   TEST_LINE( Str(Abs(-Year(sdDateE)))        , "         0"                           )
   TEST_LINE( Str(Abs(Year(sdDate)))          , " 1984"                                )
   TEST_LINE( Str(Abs(-Year(sdDate)))         , "      1984"                           )
   TEST_LINE( Str(Abs(Month(sdDate)))         , "  3"                                  )
   TEST_LINE( Str(Abs(-Month(sdDate)))        , "         3"                           )
   TEST_LINE( Str(Abs(0))                     , "         0"                           )
   TEST_LINE( Str(Abs(0.0))                   , "         0.0"                         )
   TEST_LINE( Str(Abs(-0))                    , "         0"                           )
   TEST_LINE( Str(Abs(150))                   , "       150"                           )
   TEST_LINE( Str(Abs(-150))                  , "       150"                           )
   TEST_LINE( Str(Abs(150.245))               , "       150.245"                       )
   TEST_LINE( Str(Abs(-150.245))              , "       150.245"                       )
   TEST_LINE( Str(Abs(Val("0")))              , "0"                                    )
   TEST_LINE( Str(Abs(Val("-0")))             , " 0"                                   )
   TEST_LINE( Str(Abs(Val("150")))            , "150"                                  )
   TEST_LINE( Str(Abs(Val("-150")))           , "       150"                           )
   TEST_LINE( Str(Abs(Val("150.245")))        , "       150.245"                       )
   TEST_LINE( Str(Abs(Val("-150.245")))       , "       150.245"                       )
   TEST_LINE( Abs(0.1)                        , 0.1                                    )
   TEST_LINE( Abs(10.5)                       , 10.5                                   )
   TEST_LINE( Abs(-10.7)                      , 10.7                                   )
   TEST_LINE( Abs(10.578)                     , 10.578                                 )
   TEST_LINE( Abs(-10.578)                    , 10.578                                 )
   TEST_LINE( Abs(100000)                     , 100000                                 )
   TEST_LINE( Abs(-100000)                    , 100000                                 )

   /* Exp() */

   TEST_LINE( Exp("A")                        , "E 1 BASE 1096 Argument error (EXP) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Exp(0)                          , 1.00                                   )
   TEST_LINE( Str(Exp(15))                    , "   3269017.37"                        )
   TEST_LINE( Str(Exp(snIntZ))                , "         1.00"                        )
#ifdef __HARBOUR__
   TEST_LINE( Str(Exp(@snIntZ))               , "         1.00"                        ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1096 Argument error (EXP) OS:0 #:0 A:1:U:0 F:S" */
#endif
   TEST_LINE( Round(Exp(1),2)                 , 2.72                                   )
   TEST_LINE( Str(Exp(1),20,10)               , "        2.7182818285"                 )
   TEST_LINE( Round(Exp(10),2)                , 22026.47                               )
   TEST_LINE( Str(Exp(10),20,10)              , "    22026.4657948067"                 )

   /* Round() */

   TEST_LINE( Round(snDoubleP, snIntZ)        , 11                                     )
#ifdef __HARBOUR__
   TEST_LINE( Round(@snDoubleP, @snIntZ)      , 11                                     ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:U:10.567;U:0 F:S" */
#endif
   TEST_LINE( Round(NIL, 0)                   , "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:U:NIL;N:0 F:S" )
   TEST_LINE( Round(0, NIL)                   , "E 1 BASE 1094 Argument error (ROUND) OS:0 #:0 A:2:N:0;U:NIL F:S" )
   TEST_LINE( Round(0, 0)                     , 0                )
   TEST_LINE( Round(0, 2)                     , 0.00             )
   TEST_LINE( Round(0, -2)                    , 0                )
   TEST_LINE( Round(0.5, 0)                   , 1                )
   TEST_LINE( Round(0.5, 1)                   , 0.5              )
   TEST_LINE( Round(0.5, 2)                   , 0.50             )
   TEST_LINE( Round(0.5, -1)                  , 0                )
   TEST_LINE( Round(0.5, -2)                  , 0                )
   TEST_LINE( Round(0.50, 0)                  , 1                )
   TEST_LINE( Round(0.50, 1)                  , 0.5              )
   TEST_LINE( Round(0.50, 2)                  , 0.50             )
   TEST_LINE( Round(0.50, -1)                 , 0                )
   TEST_LINE( Round(0.50, -2)                 , 0                )
   TEST_LINE( Round(0.55, 0)                  , 1                )
   TEST_LINE( Round(0.55, 1)                  , 0.6              )
   TEST_LINE( Round(0.55, 2)                  , 0.55             )
   TEST_LINE( Round(0.55, -1)                 , 0                )
   TEST_LINE( Round(0.55, -2)                 , 0                )
   TEST_LINE( Round(0.557, 0)                 , 1                )
   TEST_LINE( Round(0.557, 1)                 , 0.6              )
   TEST_LINE( Round(0.557, 2)                 , 0.56             )
   TEST_LINE( Round(0.557, -1)                , 0                )
   TEST_LINE( Round(0.557, -2)                , 0                )
   TEST_LINE( Round(50, 0)                    , 50               )
   TEST_LINE( Round(50, 1)                    , 50.0             )
   TEST_LINE( Round(50, 2)                    , 50.00            )
   TEST_LINE( Round(50, -1)                   , 50               )
   TEST_LINE( Round(50, -2)                   , 100              )
   TEST_LINE( Round(10.50, 0)                 , 11               )
   TEST_LINE( Round(10.50, -1)                , 10               )
   TEST_LINE( Round(500000, 0)                , 500000           )
   TEST_LINE( Round(500000, 1)                , 500000.0         )
   TEST_LINE( Round(500000, 2)                , 500000.00        )
   TEST_LINE( Round(500000, -1)               , 500000           )
   TEST_LINE( Round(500000, -2)               , 500000           )
   TEST_LINE( Round(500000, -2)               , 500000           )
   TEST_LINE( Round(5000000000, 0)            , 5000000000       )
   TEST_LINE( Round(5000000000, 1)            , 5000000000.0     )
   TEST_LINE( Round(5000000000, 2)            , 5000000000.00    )
   TEST_LINE( Round(5000000000, -1)           , 5000000000       )
   TEST_LINE( Round(5000000000, -2)           , 5000000000       )
   TEST_LINE( Round(5000000000, -2)           , 5000000000       )
   TEST_LINE( Round(5000000000.129, 0)        , 5000000000       )
   TEST_LINE( Round(5000000000.129, 1)        , 5000000000.1     )
   TEST_LINE( Round(5000000000.129, 2)        , 5000000000.13    )
   TEST_LINE( Round(5000000000.129, -1)       , 5000000000       )
   TEST_LINE( Round(5000000000.129, -2)       , 5000000000       )
   TEST_LINE( Round(5000000000.129, -2)       , 5000000000       )
   TEST_LINE( Round(-0.5, 0)                  , -1               )
   TEST_LINE( Round(-0.5, 1)                  , -0.5             )
   TEST_LINE( Round(-0.5, 2)                  , -0.50            )
   TEST_LINE( Round(-0.5, -1)                 , 0                )
   TEST_LINE( Round(-0.5, -2)                 , 0                )
   TEST_LINE( Round(-0.50, 0)                 , -1               )
   TEST_LINE( Round(-0.50, 1)                 , -0.5             )
   TEST_LINE( Round(-0.50, 2)                 , -0.50            )
   TEST_LINE( Round(-0.50, -1)                , 0                )
   TEST_LINE( Round(-0.50, -2)                , 0                )
   TEST_LINE( Round(-0.55, 0)                 , -1               )
   TEST_LINE( Round(-0.55, 1)                 , -0.6             )
   TEST_LINE( Round(-0.55, 2)                 , -0.55            )
   TEST_LINE( Round(-0.55, -1)                , 0                )
   TEST_LINE( Round(-0.55, -2)                , 0                )
   TEST_LINE( Round(-0.557, 0)                , -1               )
   TEST_LINE( Round(-0.557, 1)                , -0.6             )
   TEST_LINE( Round(-0.557, 2)                , -0.56            )
   TEST_LINE( Round(-0.557, -1)               , 0                )
   TEST_LINE( Round(-0.557, -2)               , 0                )
   TEST_LINE( Round(-50, 0)                   , -50              )
   TEST_LINE( Round(-50, 1)                   , -50.0            )
   TEST_LINE( Round(-50, 2)                   , -50.00           )
   TEST_LINE( Round(-50, -1)                  , -50              )
   TEST_LINE( Round(-50, -2)                  , -100             )
   TEST_LINE( Round(-10.50, 0)                , -11              )
   TEST_LINE( Round(-10.50, -1)               , -10              )
   TEST_LINE( Round(-500000, 0)               , -500000          )
   TEST_LINE( Round(-500000, 1)               , -500000.0        )
   TEST_LINE( Round(-500000, 2)               , -500000.00       )
   TEST_LINE( Round(-500000, -1)              , -500000          )
   TEST_LINE( Round(-500000, -2)              , -500000          )
   TEST_LINE( Round(-500000, -2)              , -500000          )
   TEST_LINE( Round(-5000000000, 0)           , -5000000000      )
   TEST_LINE( Round(-5000000000, 1)           , -5000000000.0    )
   TEST_LINE( Round(-5000000000, 2)           , -5000000000.00   )
   TEST_LINE( Round(-5000000000, -1)          , -5000000000      )
   TEST_LINE( Round(-5000000000, -2)          , -5000000000      )
   TEST_LINE( Round(-5000000000, -2)          , -5000000000      )
   TEST_LINE( Round(-5000000000.129, 0)       , -5000000000      )
   TEST_LINE( Round(-5000000000.129, 1)       , -5000000000.1    )
   TEST_LINE( Round(-5000000000.129, 2)       , -5000000000.13   )
   TEST_LINE( Round(-5000000000.129, -1)      , -5000000000      )
   TEST_LINE( Round(-5000000000.129, -2)      , -5000000000      )
   TEST_LINE( Round(-5000000000.129, -2)      , -5000000000      )

   /* INT() */

   TEST_LINE( Int( NIL )                      , "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( Int( "A" )                      , "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Int( {} )                       , "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:A:{.[0].} F:S" )
   TEST_LINE( Int( 0 )                        , 0                                    )
   TEST_LINE( Int( 0.0 )                      , 0                                    )
   TEST_LINE( Int( 10 )                       , 10                                   )
   TEST_LINE( Int( snIntP )                   , 10                                   )
#ifdef __HARBOUR__
   TEST_LINE( Int( @snIntP )                  , 10                                   ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1090 Argument error (INT) OS:0 #:0 A:1:U:10 F:S" */
#endif
   TEST_LINE( Int( -10 )                      , -10                                  )
   TEST_LINE( Int( 100000 )                   , 100000                               )
   TEST_LINE( Int( -100000 )                  , -100000                              )
   TEST_LINE( Int( 10.5 )                     , 10                                   )
   TEST_LINE( Int( -10.5 )                    , -10                                  )
   TEST_LINE( Str(Int(Val("100.290")))        , "100"                                )
   TEST_LINE( Str(Int(Val("  100.290")))      , "  100"                              )
   TEST_LINE( Str(Int(Val(" 100")))           , " 100"                               )
   TEST_LINE( Int(5000000000.90)              , 5000000000                           )
   TEST_LINE( Int(-5000000000.90)             , -5000000000                          )
   TEST_LINE( Int(5000000000)                 , 5000000000                           )
   TEST_LINE( Int(-5000000000)                , -5000000000                          )
   TEST_LINE( Int(5000000000) / 100000        , 50000                                )
   TEST_LINE( Int(-5000000000) / 100000       , -50000                               )

   /* Min()/Max() */

   TEST_LINE( Max(NIL, NIL)                                 , "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Max(10, NIL)                                  , "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:N:10;U:NIL F:S" )
   TEST_LINE( Max(hb_SToD("19800101"), 10)                  , "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:D:19800101;N:10 F:S" )
   TEST_LINE( Max(hb_SToD("19800101"), hb_SToD("19800101")) , hb_SToD("19800101")                  )
   TEST_LINE( Max(hb_SToD("19800102"), hb_SToD("19800101")) , hb_SToD("19800102")                  )
   TEST_LINE( Max(hb_SToD("19800101"), hb_SToD("19800102")) , hb_SToD("19800102")                  )
   TEST_LINE( Max(snIntP, snLongP)                          , 100000                               )
#ifdef __HARBOUR__
   TEST_LINE( Max(@snIntP, @snLongP)                        , 100000                               ) /* Bug in CA-Cl*pper, it will return: "E 1 BASE 1093 Argument error (MAX) OS:0 #:0 A:2:U:10;U:100000 F:S" */
#endif
   TEST_LINE( Min(NIL, NIL)                                 , "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Min(10, NIL)                                  , "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:N:10;U:NIL F:S" )
   TEST_LINE( Min(hb_SToD("19800101"), 10)                  , "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:D:19800101;N:10 F:S" )
   TEST_LINE( Min(hb_SToD("19800101"), hb_SToD("19800101")) , hb_SToD("19800101")                  )
   TEST_LINE( Min(hb_SToD("19800102"), hb_SToD("19800101")) , hb_SToD("19800101")                  )
   TEST_LINE( Min(hb_SToD("19800101"), hb_SToD("19800102")) , hb_SToD("19800101")                  )
   TEST_LINE( Min(snIntP, snLongP)                          , 10                                   )
#ifdef __HARBOUR__
   TEST_LINE( Min(@snIntP, @snLongP)                        , 10                                   ) /* Bug in CA-Cl*pper, it will return: "E 1 BASE 1092 Argument error (MIN) OS:0 #:0 A:2:U:10;U:100000 F:S" */
#endif

   /* Decimals handling */

   TEST_LINE( Str(Max(10, 12)             )   , "        12"                   )
   TEST_LINE( Str(Max(10.50, 10)          )   , "        10.50"                )
   TEST_LINE( Str(Max(10, 9.50)           )   , "        10"                   )
   TEST_LINE( Str(Max(100000, 10)         )   , "    100000"                   )
   TEST_LINE( Str(Max(20.50, 20.670)      )   , "        20.670"               )
   TEST_LINE( Str(Max(20.5125, 20.670)    )   , "        20.670"               )
   TEST_LINE( Str(Min(10, 12)             )   , "        10"                   )
   TEST_LINE( Str(Min(10.50, 10)          )   , "        10"                   )
   TEST_LINE( Str(Min(10, 9.50)           )   , "         9.50"                )
   TEST_LINE( Str(Min(100000, 10)         )   , "        10"                   )
   TEST_LINE( Str(Min(20.50, 20.670)      )   , "        20.50"                )
   TEST_LINE( Str(Min(20.5125, 20.670)    )   , "        20.5125"              )
   TEST_LINE( Str(Val("0x10")             )   , "   0"                         )
   TEST_LINE( Str(Val("0X10")             )   , "   0"                         )
   TEST_LINE( Str(Val("15E2")             )   , "  15"                         )
   TEST_LINE( Str(Val("15E21")            )   , "   15"                        )
   TEST_LINE( Str(Val("15.1A10")          )   , "15.1000"                      )
   TEST_LINE( Str(Val("15.1A1")           )   , "15.100"                       )
   TEST_LINE( Str(Val("A")                )   , "0"                            )
   TEST_LINE( Str(Val("AAA0")             )   , "   0"                         )
   TEST_LINE( Str(Val("AAA2")             )   , "   0"                         )
   TEST_LINE( Str(Val("")                 )   , "         0"                   )
   TEST_LINE( Str(Val("0")                )   , "0"                            )
   TEST_LINE( Str(Val(" 0")               )   , " 0"                           )
   TEST_LINE( Str(Val("-0")               )   , " 0"                           )
   TEST_LINE( Str(Val("00")               )   , " 0"                           )
   TEST_LINE( Str(Val("1")                )   , "1"                            )
   TEST_LINE( Str(Val("15")               )   , "15"                           )
   TEST_LINE( Str(Val("200")              )   , "200"                          )
   TEST_LINE( Str(Val(" 200")             )   , " 200"                         )
   TEST_LINE( Str(Val("200 ")             )   , " 200"                         )
   TEST_LINE( Str(Val(" 200 ")            )   , "  200"                        )
   TEST_LINE( Str(Val("-200")             )   , "-200"                         )
   TEST_LINE( Str(Val(" -200")            )   , " -200"                        )
   TEST_LINE( Str(Val("-200 ")            )   , " -200"                        )
   TEST_LINE( Str(Val(" -200 ")           )   , "  -200"                       )
   TEST_LINE( Str(Val("15.0")             )   , "15.0"                         )
   TEST_LINE( Str(Val("15.00")            )   , "15.00"                        )
   TEST_LINE( Str(Val("15.000")           )   , "15.000"                       )
   TEST_LINE( Str(Val("15.001 ")          )   , "15.0010"                      )
   TEST_LINE( Str(Val("100000000")        )   , "100000000"                    )
   TEST_LINE( Str(Val("5000000000")       )   , "5000000000"                   )
   TEST_LINE( Str(10                      )   , "        10"                   )
   TEST_LINE( Str(15.0                    )   , "        15.0"                 )
   TEST_LINE( Str(10.1                    )   , "        10.1"                 )
   TEST_LINE( Str(15.00                   )   , "        15.00"                )
//   TEST_LINE( Str(Log(0)                  )   , "***********************"      )
   TEST_LINE( Str(100.2 * 200.12          )   , "     20052.024"               )
   TEST_LINE( Str(100.20 * 200.12         )   , "     20052.0240"              )
   TEST_LINE( Str(1000.2 * 200.12         )   , "    200160.024"               )
   TEST_LINE( Str(100/1000                )   , "         0.10"                )
   TEST_LINE( Str(100/100000              )   , "         0.00"                )
   TEST_LINE( Str(10 * 10                 )   , "       100"                   )
   TEST_LINE( Str(100 / 10                )   , "        10"                   )
   TEST_LINE( Str(100 / 13                )   , "         7.69"                )
   TEST_LINE( Str(100.0 / 10              )   , "        10.00"                )
   TEST_LINE( Str(100.0 / 10.00           )   , "        10.00"                )
   TEST_LINE( Str(100.0 / 10.000          )   , "        10.00"                )
   TEST_LINE( Str(100 / 10.00             )   , "        10.00"                )
   TEST_LINE( Str(100 / 10.000            )   , "        10.00"                )
   TEST_LINE( Str(100.00 / 10.0           )   , "        10.00"                )
   TEST_LINE( Str(sdDate - sdDateE        )   , "   2445785"                   )
   TEST_LINE( Str(sdDate - sdDate         )   , "         0"                   )
   TEST_LINE( Str(1234567890 * 1234567890 )   , " 1524157875019052000"         ) // real val is 1524157875019052100

   /* Mod() */

   TEST_LINE( Mod()                           , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Mod( NIL )                      , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Mod( 100 )                      , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:100;U:NIL F:S" )
   TEST_LINE( Mod( "A", "B" )                 , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:C:A;C:B F:S" )
   TEST_LINE( Mod( "A", 100 )                 , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:C:A;N:100 F:S" )
   TEST_LINE( Mod( 100, "B" )                 , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:100;C:B F:S" )
   TEST_LINE( Mod( NIL, NIL )                 , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Mod( 100, 60, "A" )             , 40.00                              )

   TEST_LINE( Mod( 1, 0 )                     , "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S" )
   TEST_LINE( Mod( 1, NIL )                   , "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:1;U:NIL F:S" )
   TEST_LINE( Str( Mod( 1, 0   ) )            , "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S" )
   TEST_LINE( Str( Mod( 2, 4   ) )            , "         2.00"                    )
   TEST_LINE( Str( Mod( 4, 2   ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( 4, 2.0 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( 2, 4.0 ) )            , "         2.00"                    )
   TEST_LINE( Str( Mod( 8, 3   ) )            , "         2.00"                    )

   TEST_LINE( Str( Mod(  3,  3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  3,  2 ) )            , "         1.00"                    )
   TEST_LINE( Str( Mod(  3,  1 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  3,  0 ) )            , "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:3;N:0 F:S" )
   TEST_LINE( Str( Mod(  3, -1 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  3, -2 ) )            , "        -1.00"                    )
   TEST_LINE( Str( Mod(  3, -3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -3,  3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -3,  2 ) )            , "         1.00"                    )
   TEST_LINE( Str( Mod( -3,  1 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -3,  0 ) )            , "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:-3;N:0 F:S" )
   TEST_LINE( Str( Mod( -3, -1 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -3, -2 ) )            , "        -1.00"                    )
   TEST_LINE( Str( Mod( -3, -3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  3,  3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  2,  3 ) )            , "         2.00"                    )
   TEST_LINE( Str( Mod(  1,  3 ) )            , "         1.00"                    )
   TEST_LINE( Str( Mod(  0,  3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -1,  3 ) )            , "         2.00"                    )
   TEST_LINE( Str( Mod( -2,  3 ) )            , "         1.00"                    )
   TEST_LINE( Str( Mod( -3,  3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  3, -3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod(  2, -3 ) )            , "        -1.00"                    )
   TEST_LINE( Str( Mod(  1, -3 ) )            , "        -2.00"                    )
   TEST_LINE( Str( Mod(  0, -3 ) )            , "         0.00"                    )
   TEST_LINE( Str( Mod( -1, -3 ) )            , "        -1.00"                    )
   TEST_LINE( Str( Mod( -2, -3 ) )            , "        -2.00"                    )
   TEST_LINE( Str( Mod( -3, -3 ) )            , "         0.00"                    )

   /* <OP>assign and (pre/post)(inc/dec)rementation */
   o:=ErrorNew()
   o:oscode := 1
   TEST_LINE( o:oscode                        , 1   )
   o:oscode++
   TEST_LINE( o:oscode                        , 2   )
   TEST_LINE( o:oscode++                      , 2   )
   ++o:oscode
   TEST_LINE( o:oscode                        , 4   )
   TEST_LINE( ++o:oscode                      , 5   )
   o:oscode+=10
   TEST_LINE( o:oscode                        , 15  )
   TEST_LINE( o:oscode+=200                   , 215 )

   l := 1
   TEST_LINE( l                               , 1   )
   l++
   TEST_LINE( l                               , 2   )
   TEST_LINE( l++                             , 2   )
   ++l
   TEST_LINE( l                               , 4   )
   TEST_LINE( ++l                             , 5   )
   l+=10
   TEST_LINE( l                               , 15  )
   TEST_LINE( l+=200                          , 215 )

   mnIntN := 1
   TEST_LINE( mnIntN                          , 1   )
   mnIntN++
   TEST_LINE( mnIntN                          , 2   )
   TEST_LINE( mnIntN++                        , 2   )
   ++mnIntN
   TEST_LINE( mnIntN                          , 4   )
   TEST_LINE( ++mnIntN                        , 5   )
   mnIntN+=10
   TEST_LINE( mnIntN                          , 15  )
   TEST_LINE( mnIntN+=200                     , 215 )

   snIntN := 1
   TEST_LINE( snIntN                          , 1   )
   snIntN++
   TEST_LINE( snIntN                          , 2   )
   TEST_LINE( snIntN++                        , 2   )
   ++snIntN
   TEST_LINE( snIntN                          , 4   )
   TEST_LINE( ++snIntN                        , 5   )
   snIntN+=10
   TEST_LINE( snIntN                          , 15  )
   TEST_LINE( snIntN+=200                     , 215 )

#ifdef __HARBOUR__

   o := ErrorNew()
   s := "oscode"
   o:&s := 1
   TEST_LINE( o:&(s)                          , 1   )
   o:&s++
   TEST_LINE( o:&(s)                          , 2   )
   TEST_LINE( o:&(s)++                        , 2   )
   ++o:&s
   TEST_LINE( o:&(s)                          , 4   )
   TEST_LINE( ++o:&(s)                        , 5   )
   o:&s+=10
   TEST_LINE( o:&(s)                          , 15  )
   TEST_LINE( o:&(s)+=200                     , 215 )

   WITH OBJECT ErrorNew()
      :&(s) := 1
      TEST_LINE( :&(s)                           , 1   )
      :&(s)++
      TEST_LINE( :&(s)                           , 2   )
      TEST_LINE( :&(s)++                         , 2   )
      ++:&(s)
      TEST_LINE( :&(s)                           , 4   )
      TEST_LINE( ++:&(s)                         , 5   )
      :&(s)+=10
      TEST_LINE( :&(s)                           , 15  )
      TEST_LINE( :&(s)+=200                      , 215 )
   ENDWITH

   WITH OBJECT ErrorNew()
      :oscode := 1
      TEST_LINE( :oscode                         , 1   )
      :oscode++
      TEST_LINE( :oscode                         , 2   )
      TEST_LINE( :oscode++                       , 2   )
      ++:oscode
      TEST_LINE( :oscode                         , 4   )
      TEST_LINE( ++:oscode                       , 5   )
      :oscode+=10
      TEST_LINE( :oscode                         , 15  )
      TEST_LINE( :oscode+=200                    , 215 )
   ENDWITH

   &s0 := 1
   TEST_LINE( &s0                             , 1   )
   &s0++
   TEST_LINE( &s0                             , 2   )
   TEST_LINE( &(s0)++                         , 2   )
   ++&s0
   TEST_LINE( &s0                             , 4   )
   TEST_LINE( ++&(s0)                         , 5   )
   &s0+=10
   TEST_LINE( &s0                             , 15  )
   TEST_LINE( &(s0)+=200                      , 215 )

   &s1.2 := 1
   TEST_LINE( &s1.2                           , 1   )
   &s1.2++
   TEST_LINE( &s1.2                           , 2   )
   TEST_LINE( &s1.2++                         , 2   )
   ++&s1.2
   TEST_LINE( &s1.2                           , 4   )
   TEST_LINE( ++&s1.2                         , 5   )
   &s1.2+=10
   TEST_LINE( &s1.2                           , 15  )
   TEST_LINE( &s1.2+=200                      , 215 )

#endif

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
