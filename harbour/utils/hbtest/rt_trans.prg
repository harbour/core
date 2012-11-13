/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (strings)
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

PROCEDURE Main_TRANS()
   LOCAL cOldDate  := Set( _SET_DATEFORMAT )
   local cOldFixed := Set( _SET_FIXED )
   local cOldDecim := Set( _SET_DECIMALS )
   local dt

   /* TRANSFORM() */

   TEST_LINE( Transform( NIL       , NIL        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( Transform( NIL       , ""         )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;C: F:S" )
   TEST_LINE( Transform( NIL       , "@"        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;C:@ F:S" )
   TEST_LINE( Transform( {}        , NIL        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};U:NIL F:S" )
   TEST_LINE( Transform( {}        , ""         )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};C: F:S" )
   TEST_LINE( Transform( {}        , "@"        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};C:@ F:S" )
   TEST_LINE( Transform( ErrorNew(), NIL        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;U:NIL F:S" )
   TEST_LINE( Transform( ErrorNew(), ""         )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( Transform( ErrorNew(), "@"        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;C:@ F:S" )
   TEST_LINE( Transform( {|| NIL } , NIL        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};U:NIL F:S" )
   TEST_LINE( Transform( {|| NIL } , ""         )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};C: F:S" )
   TEST_LINE( Transform( {|| NIL } , "@"        )          , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};C:@ F:S" )

   TEST_LINE( Transform( "", "" )                          , ""                                         )
   TEST_LINE( Transform( "", "@" )                         , ""                                         )
#ifndef __XPP__
   TEST_LINE( Transform( "", NIL )                         , ""                                         )
#endif
   TEST_LINE( Transform( "", 100 )                         , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:;N:100 F:S" )
   TEST_LINE( Transform( "hello", "" )                     , "hello"                                    )
   TEST_LINE( Transform( "hello", "@" )                    , "hello"                                    )
#ifndef __XPP__
   TEST_LINE( Transform( "hello", NIL )                    , "hello"                                    )
#endif
   TEST_LINE( Transform( "hello", 100 )                    , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:hello;N:100 F:S" )
   TEST_LINE( Transform( 100.2, "" )                       , "       100.2"                             )
   TEST_LINE( Transform( 100.2, "@" )                      , "       100.2"                             )
#ifndef __XPP__
   TEST_LINE( Transform( 100.2, NIL )                      , "       100.2"                             )
#endif
   TEST_LINE( Transform( 100.2, 100 )                      , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.2;N:100 F:S" )
   TEST_LINE( Transform( 100.20, "" )                      , "       100.20"                            )
   TEST_LINE( Transform( 100.20, "@" )                     , "       100.20"                            )
#ifndef __XPP__
   TEST_LINE( Transform( 100.20, NIL )                     , "       100.20"                            )
#endif
   TEST_LINE( Transform( 100.20, 100 )                     , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.20;N:100 F:S" )
   TEST_LINE( Transform( Val("100.2"), "" )                , "100.2"                                    )
   TEST_LINE( Transform( Val("100.2"), "@" )               , "100.2"                                    )
#ifndef __XPP__
   TEST_LINE( Transform( Val("100.2"), NIL )               , "100.2"                                    )
#endif
   TEST_LINE( Transform( Val("100.2"), 100 )               , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.2;N:100 F:S" )
   TEST_LINE( Transform( Val("100.20"), "" )               , "100.20"                                   )
// TEST_LINE( Transform( Val("100.20"), "@" )              , "100.20"                                   )
#ifndef __XPP__
   TEST_LINE( Transform( Val("100.20"), NIL )              , "100.20"                                   )
#endif
   TEST_LINE( Transform( Val("100.20"), 100 )              , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.20;N:100 F:S" )
   TEST_LINE( Transform( sdDate, "" )                      , "1984.03.25"                               )
   TEST_LINE( Transform( sdDate, "@" )                     , "1984.03.25"                               )
#ifndef __XPP__
   TEST_LINE( Transform( sdDate, NIL )                     , "1984.03.25"                               )
#endif
   TEST_LINE( Transform( sdDate, 100 )                     , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:D:19840325;N:100 F:S" )
   TEST_LINE( Transform( .T., "" )                         , "T"                                        )
   TEST_LINE( Transform( .T., "@" )                        , "T"                                        )
#ifndef __XPP__
   TEST_LINE( Transform( .F., NIL )                        , "F"                                        )
#endif
   TEST_LINE( Transform( .F., 100 )                        , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:L:.F.;N:100 F:S" )

   TEST_LINE( Transform( scStringM , "!!!!!"    )          , "HELLO"                       )
   TEST_LINE( Transform( scStringM , "@!"       )          , "HELLO"                       )
#ifdef __HARBOUR__
   TEST_LINE( Transform( @scStringM, "!!!!!"    )          , "HELLO"                       ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:Hello;C:!!!!! F:S" */
   TEST_LINE( Transform( @scStringM, "@!"       )          , "HELLO"                       ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:Hello;C:@! F:S" */
#endif
   TEST_LINE( Transform( scStringM , "" )                  , "Hello"                       )
#ifndef __XPP__
   TEST_LINE( Transform( scStringM , NIL )                 , "Hello"                       )
#endif
   TEST_LINE( Transform( scStringM , 100 )                 , "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:Hello;N:100 F:S" )

   TEST_LINE( Transform("abcdef", "@! !lkm!")              , "ABkmE"                       )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! 1234567890"), "12345678I0" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyzabcdefg", "@! abcdefghijklmnopqrstuvwxyzabcdefg"), "AbcdefghijkLmNopqrstuvwXYzAbcdefg" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! `~!@#$% ^&*()_+-={}\|;':")  , "`~C@E$% ^&*()_+-={}\|;':" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! ,./<>?")                    , ",./<>?" )
   TEST_LINE( Transform("hello", " @!")                    , " @L"   )

   TEST_LINE( Transform("abcdef", "@R! !lkm!")              , "ABkmC"                       )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! 1234567890"), "12345678A0" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyzabcdefg", "@R! abcdefghijklmnopqrstuvwxyzabcdefg"), "AbcdefghijkBmCopqrstuvwDNzFbcdefg" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! `~!@#$% ^&*()_+-={}\|;':")  , "`~A@B$% ^&*()_+-={}\|;':"         )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,./<>?")                    , ",./<>?ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
   TEST_LINE( Transform("hello", " @R!")                   , " @RL"  )

   TEST_LINE( Transform("abc", "@R !!!!")                  , "ABC "  )
   TEST_LINE( Transform("abc", "@R XXXX")                  , "abc "  )
   TEST_LINE( Transform("abc", "@R !!")                    , "AB"    )
   TEST_LINE( Transform("abc", "@R XX")                    , "ab"    )
   TEST_LINE( Transform("abc", "@!R !!!!")                 , "ABC "  )
   TEST_LINE( Transform("abc", "@!R XXXX")                 , "ABC "  )
   TEST_LINE( Transform("abc", "@!R !!")                   , "AB"    )
   TEST_LINE( Transform("abc", "@!R XX")                   , "AB"    )

   TEST_LINE( Transform( "Hallo   ", "!!!!!"    )          , "HALLO"                       )
   TEST_LINE( Transform( "Hallo   ", "!!A!!"    )          , "HAlLO"                       )
   TEST_LINE( Transform( "Hallo   ", "!!A9!"    )          , "HAllO"                       )
   TEST_LINE( Transform( "Hallo   ", "!QA9!"    )          , "HQllO"                       )
   TEST_LINE( Transform( "Hallo   ", "ZQA9!"    )          , "ZQllO"                       )
   TEST_LINE( Transform( "Hall"    , "ZQA9!"    )          , "ZQll"                        )
   TEST_LINE( Transform( "Hallo   ", "!AAA"     )          , "Hall"                        )
   TEST_LINE( Transform( "Hallo   ", "@!"       )          , "HALLO   "                    )
   TEST_LINE( Transform( "Hallo   ", "@! AA"    )          , "HA"                          )
   TEST_LINE( Transform( "Hallo   ", "@R"       )          , "Hallo   "                    )
   TEST_LINE( Transform( "Hallo   ", "@Z"       )          , "        "                    )
   TEST_LINE( Transform( "Hallo   ", "@R !!"    )          , "HA"                          )
   TEST_LINE( Transform( "Hi"      , "@R !!!"   )          , "HI "                         )
   TEST_LINE( Transform( "Hallo   ", ""         )          , "Hallo   "                    )

   TEST_LINE( Transform( .T.       , ""         )          , "T"                           )
   TEST_LINE( Transform( .F.       , ""         )          , "F"                           )
   TEST_LINE( Transform( .T.       , "L"        )          , "T"                           )
   TEST_LINE( Transform( .F.       , "L"        )          , "F"                           )
   TEST_LINE( Transform( .T.       , "Y"        )          , "Y"                           )
   TEST_LINE( Transform( .F.       , "Y"        )          , "N"                           )
   TEST_LINE( Transform( .T.       , "X"        )          , "X"                           )
   TEST_LINE( Transform( .F.       , "#"        )          , "F"                           )
   TEST_LINE( Transform( .T.       , "X!"       )          , "X"                           )
   TEST_LINE( Transform( .F.       , "@R Y"     )          , "N"                           )
   TEST_LINE( Transform( .T.       , "@R X!"    )          , "X!T"                         )

   TEST_LINE( Transform( hb_SToD("20000101") , "@B"         ) , "2000.01.01"                  )
   TEST_LINE( Transform( hb_SToD("19901214") , "99/99/9999" ) , "1990.12.14"                  )
   TEST_LINE( Transform( hb_SToD("19901202") , "99.99.9999" ) , "1990.12.02"                  )
   TEST_LINE( Transform( hb_SToD("")         , "99/99/9999" ) , "    .  .  "                  )
   TEST_LINE( Transform( hb_SToD("19901202") , "99/99/99"   ) , "1990.12.02"                  )
   TEST_LINE( Transform( hb_SToD("19901214") , "99-99-99"   ) , "1990.12.14"                  )
   TEST_LINE( Transform( hb_SToD("20040430") , "99.99.99"   ) , "2004.04.30"                  )
   TEST_LINE( Transform( hb_SToD("")         , "99/99/99"   ) , "    .  .  "                  )
   TEST_LINE( Transform( hb_SToD("19920101") , "THISWRNG"   ) , "1992.01.01"                  )
   TEST_LINE( Transform( hb_SToD("19350605") , "999/99/9"   ) , "1935.06.05"                  )
   TEST_LINE( Transform( hb_SToD("19101112") , "9#-9#/##"   ) , "1910.11.12"                  )
   TEST_LINE( Transform( hb_SToD("19920101") , ""           ) , "1992.01.01"                  )
   TEST_LINE( Transform( hb_SToD("19920101") , "DO THIS "   ) , "1992.01.01"                  )
   TEST_LINE( Transform( hb_SToD("19920102") , "@E"         ) , "02.01.1992"                  ) /* Bug in CA-Cl*pper, it returns: "2.91901.02" */
   TEST_LINE( Transform( 1234                , "@D 9999"    ) , "1234.00.0 "                  )
   TEST_LINE( Transform( 1234                , "@BD 9999"   ) , "1234.00.0 "                  )

   SET CENTURY OFF

   TEST_LINE( Transform( hb_SToD("20000101") , "@B"         ) , "00.01.01"                    )
   TEST_LINE( Transform( hb_SToD("19901214") , "99/99/9999" ) , "90.12.14"                    )
   TEST_LINE( Transform( hb_SToD("19901202") , "99.99.9999" ) , "90.12.02"                    )
   TEST_LINE( Transform( hb_SToD("")         , "99/99/9999" ) , "  .  .  "                    )
   TEST_LINE( Transform( hb_SToD("19901202") , "99/99/99"   ) , "90.12.02"                    )
   TEST_LINE( Transform( hb_SToD("19901214") , "99-99-99"   ) , "90.12.14"                    )
   TEST_LINE( Transform( hb_SToD("20040430") , "99.99.99"   ) , "04.04.30"                    )
   TEST_LINE( Transform( hb_SToD("")         , "99/99/99"   ) , "  .  .  "                    )
   TEST_LINE( Transform( hb_SToD("19920101") , "THISWRNG"   ) , "92.01.01"                    )
   TEST_LINE( Transform( hb_SToD("19350605") , "999/99/9"   ) , "35.06.05"                    )
   TEST_LINE( Transform( hb_SToD("19101112") , "9#-9#/##"   ) , "10.11.12"                    )
   TEST_LINE( Transform( hb_SToD("19920101") , ""           ) , "92.01.01"                    )
   TEST_LINE( Transform( hb_SToD("19920101") , "DO THIS "   ) , "92.01.01"                    )
   TEST_LINE( Transform( hb_SToD("19920102") , "@E"         ) , "02.01.92"                    ) /* Bug in CA-Cl*pper, it returns: "01.92.02" */
   TEST_LINE( Transform( 1234                , "@D 9999"    ) , "**.**.* "                    )
   TEST_LINE( Transform( 1234                , "@BD 9999"   ) , "**.**.* "                    )

   SET CENTURY ON

   TEST_LINE( Transform( 1         , "@b"          )       , "1         "                  )
   TEST_LINE( Transform( 1         , "@B"          )       , "1         "                  )
   TEST_LINE( Transform( 1.0       , "@B"          )       , "1.0         "                )
   TEST_LINE( Transform( 15        , "9999"        )       , "  15"                        )
   TEST_LINE( Transform( 1.5       , "99.99"       )       , " 1.50"                       )
   TEST_LINE( Transform( 1.5       , "9999"        )       , "   2"                        )
   TEST_LINE( Transform( 15        , "####"        )       , "  15"                        )
   TEST_LINE( Transform( 1.5       , "##.##"       )       , " 1.50"                       )
   TEST_LINE( Transform( 1.5       , "####"        )       , "   2"                        )
   TEST_LINE( Transform( 15        , " AX##"       )       , " AX15"                       )
   TEST_LINE( Transform( 1.5       , "!9XPA.9"     )       , "!1XPA.5"                     )
   TEST_LINE( Transform( -15       , "9999"        )       , " -15"                        )
   TEST_LINE( Transform( -1.5      , "99.99"       )       , "-1.50"                       )
   TEST_LINE( Transform( -15       , "$999"        )       , "$-15"                        )
   TEST_LINE( Transform( -1.5      , "*9.99"       )       , "-1.50"                       )
   TEST_LINE( Transform( 41        , "$$$9"        )       , "$$41"                        )
   TEST_LINE( Transform( 41        , "***9"        )       , "**41"                        )
   TEST_LINE( Transform( 15000     , "9999"        )       , "****"                        )
   TEST_LINE( Transform( 15000     , "99,999"      )       , "15,000"                      )
   TEST_LINE( Transform( 1500      , "99,999"      )       , " 1,500"                      )
   TEST_LINE( Transform( 150       , "99,999"      )       , "   150"                      )
   TEST_LINE( Transform( 150       , "99,99"       )       , " 1,50"                       )
   TEST_LINE( Transform( 41        , "@Z 9999"     )       , "  41"                        )
   TEST_LINE( Transform( 0         , "@Z 9999"     )       , "    "                        )
#ifdef __HARBOUR__
   TEST_LINE( Transform( 41        , "@0 9999"     )       , "0041"                        ) /* Extension in Harbour, in CA-Cl*pper it should return: "  41" */
   TEST_LINE( Transform( 0         , "@0 9999"     )       , "0000"                        ) /* Extension in Harbour, in CA-Cl*pper it should return: "   0" */
#endif
   TEST_LINE( Transform( 41        , "@B 9999"     )       , "41  "                        )
   TEST_LINE( Transform( 41        , "@B 99.9"     )       , "41.0"                        )
   TEST_LINE( Transform( 7         , "@B 99.9"     )       , "7.0 "                        )
   TEST_LINE( Transform( 7         , "@C 99.9"     )       , " 7.0 CR"                     )
   TEST_LINE( Transform( -7        , "@C 99.9"     )       , "-7.0"                        )
   TEST_LINE( Transform( 7         , "@X 99.9"     )       , " 7.0"                        )
   TEST_LINE( Transform( -7        , "@X 99.9"     )       , " 7.0 DB"                     )
   TEST_LINE( Transform( 7         , "@( 99.9"     )       , " 7.0"                        )
   TEST_LINE( Transform( -7        , "@( 99.9"     )       , "(7.0)"                       )
   TEST_LINE( Transform( 7         , "9X9Z5.9"     )       , " X7Z5.0"                     )
   TEST_LINE( Transform( -7        , "@R 9X9^"     )       , "-X7^"                        )
   TEST_LINE( Transform( -7        , "9X9^"        )       , "-X7^"                        )
   TEST_LINE( Transform( 1         , "@R 9HI!"     )       , "1HI!"                        )
   TEST_LINE( Transform( 1         , "9HI!"        )       , "1HI!"                        )
   TEST_LINE( Transform( -12       , "@( 99"       )       , "(*)"                         ) /* Bug in CA-Cl*pper, it returns: "(2)" */
   TEST_LINE( Transform( 12        , "@( 99"       )       , "12"                          )
   TEST_LINE( Transform( 1         , ""            )       , "         1"                  )
   TEST_LINE( Transform( 32768     , ""            )       , "     32768"                  )
   TEST_LINE( Transform( -20       , ""            )       , "       -20"                  )
   TEST_LINE( Transform( 1048576   , ""            )       , "   1048576"                  )
   TEST_LINE( Transform( 21.65     , ""            )       , "        21.65"               )
   TEST_LINE( Transform( -3.33     , ""            )       , "        -3.33"               )
   TEST_LINE( Transform( -1234     , "@( 9999"     )       , "(***)"                       ) /* Bug in CA-Cl*pper, it returns: "(234)" */
   TEST_LINE( Transform( -1234     , "@B 9999"     )       , "****"                        )
   TEST_LINE( Transform( -1234     , "@B( 9999"    )       , "(***)"                       ) /* Bug in CA-Cl*pper, it returns: "(234)" */
   TEST_LINE( Transform( 1234      , "@E 9,999.99" )       , "1.234,00"                    )
   TEST_LINE( Transform( 12.2      , "@E 9,999.99" )       , "   12,20"                    )
   TEST_LINE( Transform( -1234     , "@X 9999"     )       , "1234 DB"                     )
   TEST_LINE( Transform( -1234     , "@BX 9999"    )       , "1234 DB"                     )
   TEST_LINE( Transform( 1234      , "@B 9999"     )       , "1234"                        )
   TEST_LINE( Transform( 1234      , "@BX 9999"    )       , "1234"                        )
   TEST_LINE( Transform( 0         , "@Z 9999"     )       , "    "                        )
   TEST_LINE( Transform( 0         , "@BZ 9999"    )       , "    "                        )
   TEST_LINE( Transform( 2334      , "Xxxxx: #####")       , "Xxxxx:  2334"                )

   TEST_LINE( Transform("Hello", "@S-1")                                               , "Hello"                            )
   TEST_LINE( Transform("Hello", "@S3.0!")                                             , "HEL"                              )
   TEST_LINE( Transform("Hello", "@S3")                                                , "Hel"                              )
   TEST_LINE( Transform("Hello", "@S13") + "<"                                         , "Hello<"                           )
   TEST_LINE( Transform("Hello", "@S000000000003K")                                    , "Hel"                              )
   TEST_LINE( Transform("Hello", "@S3K")                                               , "Hel"                              )
   TEST_LINE( Transform("Hello", "@S 3K")                                              , "3K"                               )
   TEST_LINE( Transform("Hello", "@S3"+Chr(9))                                         , "Hel"                              )
   TEST_LINE( Transform("abcdef","@! !lkm!")                                           , "ABkmE"                            )
   TEST_LINE( Transform("abcdef","@! !LKM!")                                           , "ABKME"                            )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! !lkm!x")                     , "ABkmEF"                           )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! abcdefghijklmnopqrstuvwxyz") , "AbcdefghijkLmNopqrstuvwXYz"       )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! `~!@#$%^&*()_+-={}[]\|;':")  , "`~C@E$%^&*()_+-={}[]\|;':"        )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@! ,./<>?")                     , ",./<>?"                           )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,./<>?" )                   , ",./<>?ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,./<>" )                    , ",./<>ABCDEFGHIJKLMNOPQRSTUVWXYZ"  )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,./<" )                     , ",./<ABCDEFGHIJKLMNOPQRSTUVWXYZ"   )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,./" )                      , ",./ABCDEFGHIJKLMNOPQRSTUVWXYZ"    )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ,." )                       , ",.ABCDEFGHIJKLMNOPQRSTUVWXYZ"     )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! ," )                        , ",ABCDEFGHIJKLMNOPQRSTUVWXYZ"      )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! " )                         , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"       )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! 0" )                        , "0ABCDEFGHIJKLMNOPQRSTUVWXYZ"      )
   TEST_LINE( Transform("abcdefghijklmnopqrstuvwxyz", "@R! B" )                        , "BABCDEFGHIJKLMNOPQRSTUVWXYZ"      )
   TEST_LINE( Transform("zbcdefghijklmnopqrstuvwxya", "@R! A" )                        , "Z"                                )
   TEST_LINE( Transform("zbcdefghijklmnopqrstuvwxya", "@R! Z" )                        , "ZZBCDEFGHIJKLMNOPQRSTUVWXYA"      )

   TEST_LINE( Transform(" Hello ", "@Z")                   , "       "                     )
   TEST_LINE( Transform("Hello", "@Z")                     , "     "                       )
   TEST_LINE( Transform("", "@Z")                          , ""                            )
   TEST_LINE( Transform("   ", "@Z")                       , "   "                         )
   TEST_LINE( Transform(" Hello ", "@Z z")                 , " "                           )
   TEST_LINE( Transform("Hello", "@Z z")                   , " "                           )
   TEST_LINE( Transform("", "@Z z")                        , ""                            )
   TEST_LINE( Transform("   ", "@Z z")                     , " "                           )
   TEST_LINE( Transform("a", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("b", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("c", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("d", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("e", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("f", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("g", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("h", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("i", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("j", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("k", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("l", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("m", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("n", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("o", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("p", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("q", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("r", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("s", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("t", "@! Y")                       , "Y"                           )
   TEST_LINE( Transform("u", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("v", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("w", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("x", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("y", "@! Y")                       , "Y"                           )
   TEST_LINE( Transform("z", "@! Y")                       , "N"                           )
   TEST_LINE( Transform("t", "Y")                          , "Y"                           )
   TEST_LINE( Transform("y", "Y")                          , "Y"                           )
   TEST_LINE( Transform("T", "Y")                          , "Y"                           )
   TEST_LINE( Transform("Y", "Y")                          , "Y"                           )
   TEST_LINE( Transform("T", "@! Y")                       , "Y"                           )
   TEST_LINE( Transform("Y", "@! Y")                       , "Y"                           )
   TEST_LINE( Transform("abc", "@R !!!!")                  , "ABC "                        )
   TEST_LINE( Transform("abc", "@R XXXX")                  , "abc "                        )
   TEST_LINE( Transform("abc", "@R !!")                    , "AB"                          )
   TEST_LINE( Transform("abc", "@R XX")                    , "ab"                          )
   TEST_LINE( Transform("abc", "@!R !!!!")                 , "ABC "                        )
   TEST_LINE( Transform("abc", "@!R XXXX")                 , "ABC "                        )
   TEST_LINE( Transform("abc", "@!R !!")                   , "AB"                          )
   TEST_LINE( Transform("abc", "@!R XX")                   , "AB"                          )
   TEST_LINE( Transform("abc", "@R"+Chr(9)+"!!!!")         , "ABC "                        )
   TEST_LINE( Transform("abc", "@R!!!!!!!!!!!!")           , "ABC"                         )
   TEST_LINE( Transform("hello", "@ !!")                   , "HE"                          )
   TEST_LINE( Transform("hello", "@")                      , "hello"                       )
   TEST_LINE( Transform("hello", "@ ")                     , "hello"                       )
   TEST_LINE( Transform("hello", " @!")                    , " @L"                         )
   TEST_LINE( Transform("abcdefgh", "@R! helloy")          , "heABoN"                      )
   TEST_LINE( Transform("abcdefgh", "@R helloy")           , "heaboN"                      )
   TEST_LINE( Transform("abcdefgh", "@R !!!!!!")           , "ABCDEF"                      )
   TEST_LINE( Transform("abcdefgh", "@")                   , "abcdefgh"                    )

   TEST_LINE( Transform(100, "@B")                         , "100       "                  )
   TEST_LINE( Transform(100, "@b")                         , "100       "                  )

   TEST_LINE( Transform(.T., "l")                          , "T"                           )
   TEST_LINE( Transform(.F., "l")                          , "F"                           )
   TEST_LINE( Transform(.T., "L")                          , "T"                           )
   TEST_LINE( Transform(.F., "L")                          , "F"                           )

   TEST_LINE( Transform("  H", "@B" )                      , "H  "                         )
   TEST_LINE( Transform(Chr(9)+" H", "@B" )                , ""+Chr(9)+" H"                )
   TEST_LINE( Transform("  H ", "@B" )                     , "H   "                        )
   TEST_LINE( Transform("  H", "@Z" )                      , "   "                         )
   TEST_LINE( Transform("  H", "@ZB" )                     , "   "                         )
   TEST_LINE( Transform("  H", "@!" )                      , "  H"                         )
   TEST_LINE( Transform("19840325", "@D" )                 , "1984.32."                    )
   TEST_LINE( Transform("19840325", "@DE" )                , "4.81932."                    )
   TEST_LINE( Transform("1984032598765", "@DE" )           , "4.81932.98"                  )

   SET CENTURY ON
   TEST_LINE( Transform("19840325", "@D" )                 , "1984.32."                    )
   TEST_LINE( Transform("19840325", "@DE" )                , "4.81932."                    )
   TEST_LINE( Transform("1984032598765", "@DE" )           , "4.81932.98"                  )
   SET CENTURY OFF

   TEST_LINE( Transform("19840325", "@D" )                 , "19.40.25"                    )
   TEST_LINE( Transform("19840325", "@DE" )                , "40.19.25"                    )
   TEST_LINE( Transform("1984032598765", "@DE" )           , "40.19.25"                    )
   TEST_LINE( Transform("1", "@D" )                        , "1"                           )
   TEST_LINE( Transform("19840325", "@D" )                 , "19.40.25"                    )
   TEST_LINE( Transform("19840325", "@DR" )                , "19.84.03"                    )
   TEST_LINE( Transform("ABCDEFG", "@D" )                  , "AB.DE.G"                     )
   TEST_LINE( Transform("abcdefg", "@D !!")                , "ab.de.g"                     )
   TEST_LINE( Transform("abcdefg", "@D!")                  , "AB.DE.G"                     )
   TEST_LINE( Transform("ABCDEFG", "@DB" )                 , "AB.DE.G"                     )
   TEST_LINE( Transform("  CDEFG", "@DB" )                 , ".DE.G  "                     )
   TEST_LINE( Transform("ABCDEFG", "@DBZ" )                , "       "                     )
#ifdef __CLIPPER__
   /* CA-Cl*pper do not check result size and always exchanges
    * bytes 1-2 with bytes 4-5 for @E conversion. It's buffer overflow
    * bug and I do not want to replicate it inside our transform
    * implementation. It also causes that the results for for strings
    * smaller then 5 bytes behaves randomly.
    * In fact precise tests can show that it's not random behavior
    * but CA-Cl*pper uses static buffer for result and when current one
    * is smaller then 5 bytes then first two bytes are replaced with
    * 4-5 bytes from previous result which was length enough, f.e.:
    *          ? transform( "0123456789", "" )
    *          ? transform( "AB", "@E" )
    *          ? transform( "ab", "@E" )
    * [druzus]
    */
   TEST_LINE( Transform(".", "@E" )                        , " "                           )
   TEST_LINE( Transform(",", "@E" )                        , "."                           )
   TEST_LINE( Transform("..", "@E" )                       , ","+Chr(0)+""                 )
   TEST_LINE( Transform(",,", "@E" )                       , ".."                          )
   TEST_LINE( Transform(",.,", "@E" )                      , ",,."                         )
   TEST_LINE( Transform(".,.", "@E" )                      , ""+Chr(0)+".."                )
   TEST_LINE( Transform("OPI", "@E ." )                    , ""+Chr(0)+",."                )
   TEST_LINE( Transform("JKL", "@E ," )                    , ""+Chr(0)+"P."                )
   TEST_LINE( Transform("OPI", "@ER ." )                   , "I .OP.  "                    )
   TEST_LINE( Transform("JKL", "@ER ," )                   , "L .JK.  "                    )
   TEST_LINE( Transform("OPI", "@ER" )                     , "I .OP.  "                    )
   TEST_LINE( Transform("JKL", "@ER" )                     , "L .JK.  "                    )
#endif
   TEST_LINE( Transform(CTOD(""), "@DB")                   , ".  .    "                    )
   TEST_LINE( Transform(CTOD(""), "@DBR uiuijk")           , ".. . .    "                  )
   TEST_LINE( Transform(100, "@B $99999")                  , "$  100"                      )
   TEST_LINE( Transform(10, "@BZ $99999")                  , "$   10"                      )
   TEST_LINE( Transform(10, "@BX $99999")                  , "$   10"                      )
   TEST_LINE( Transform(0, "@BZX $99999")                  , "      "                      )
   TEST_LINE( Transform(-10, "@B(X $99999")                , "(10) DB   "                  )
   TEST_LINE( Transform(-10, "@(X $99999")                 , "(   10) DB"                  )
   TEST_LINE( Transform(0, "@B(X $99999")                  , "$    0"                      )
   TEST_LINE( Transform(0, "@B(ZX $99999")                 , "      "                      )

#ifndef __XPP__
   TEST_LINE( Transform(sdDate, NIL)                       , "84.03.25"                    )
#endif
   TEST_LINE( Transform(sdDate, "")                        , "84.03.25"                    )
   TEST_LINE( Transform(sdDate, "@Z")                      , "        "                    )

   Set( _SET_DATEFORMAT, "DD/MMM/YYYY" )

   TEST_LINE( Transform( .T., "#")                                      , "T"                                  )
   TEST_LINE( Transform( .F., "#")                                      , "F"                                  )
   TEST_LINE( Transform( .T., "#ylntfhekko")                            , "T"                                  )
   TEST_LINE( Transform( .F., "#ylntfhekko")                            , "F"                                  )
   TEST_LINE( Transform( .T., "#YLNTFHEKKO")                            , "T"                                  )
   TEST_LINE( Transform( .F., "#YLNTFHEKKO")                            , "F"                                  )
   TEST_LINE( Transform( .T., "YLNTFHEKKO#")                            , "Y"                                  )
   TEST_LINE( Transform( .F., "YLNTFHEKKO#")                            , "N"                                  )
   TEST_LINE( Transform( .T., "XLNTFHEKKO#")                            , "X"                                  )
   TEST_LINE( Transform( .F., "XLNTFHEKKO#")                            , "X"                                  )
   TEST_LINE( Transform( .T., "ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")     , "A"                                  )
   TEST_LINE( Transform( .F., "ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")     , "A"                                  )
   TEST_LINE( Transform( .T., "")                                       , "T"                                  )
   TEST_LINE( Transform( .F., "")                                       , "F"                                  )
   TEST_LINE( Transform( .T., "@")                                      , "T"                                  )
   TEST_LINE( Transform( .F., "@")                                      , "F"                                  )
   TEST_LINE( Transform( .T., "@R #")                                   , "T"                                  )
   TEST_LINE( Transform( .F., "@R #")                                   , "F"                                  )
   TEST_LINE( Transform( .T., "@R #ylntfhekko")                         , "T  ntfhekko"                        )
   TEST_LINE( Transform( .F., "@R #ylntfhekko")                         , "F  ntfhekko"                        )
   TEST_LINE( Transform( .T., "@R #YLNTFHEKKO")                         , "T  NTFHEKKO"                        )
   TEST_LINE( Transform( .F., "@R #YLNTFHEKKO")                         , "F  NTFHEKKO"                        )
   TEST_LINE( Transform( .T., "@R YLNTFHEKKO#")                         , "Y NTFHEKKO "                        )
   TEST_LINE( Transform( .F., "@R YLNTFHEKKO#")                         , "N NTFHEKKO "                        )
   TEST_LINE( Transform( .T., "@R XLNTFHEKKO#")                         , "XTNTFHEKKO "                        )
   TEST_LINE( Transform( .F., "@R XLNTFHEKKO#")                         , "XFNTFHEKKO "                        )
   TEST_LINE( Transform( .T., "@R ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")  , "ABCDEFGHIJKTMNOPQRSTUVWX Z9 !$ *.," )
   TEST_LINE( Transform( .F., "@R ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")  , "ABCDEFGHIJKFMNOPQRSTUVWX Z9 !$ *.," )
   TEST_LINE( Transform( .T., "@Z #")                                   , " "                                  )
   TEST_LINE( Transform( .F., "@Z #")                                   , " "                                  )
   TEST_LINE( Transform( .T., "@Z #ylntfhekko")                         , " "                                  )
   TEST_LINE( Transform( .F., "@Z #ylntfhekko")                         , " "                                  )
   TEST_LINE( Transform( .T., "@Z #YLNTFHEKKO")                         , " "                                  )
   TEST_LINE( Transform( .F., "@Z #YLNTFHEKKO")                         , " "                                  )
   TEST_LINE( Transform( .T., "@Z YLNTFHEKKO#")                         , " "                                  )
   TEST_LINE( Transform( .F., "@Z YLNTFHEKKO#")                         , " "                                  )
   TEST_LINE( Transform( .T., "@Z XLNTFHEKKO#")                         , " "                                  )
   TEST_LINE( Transform( .F., "@Z XLNTFHEKKO#")                         , " "                                  )
   TEST_LINE( Transform( .T., "@Z ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")  , " "                                  )
   TEST_LINE( Transform( .F., "@Z ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,")  , " "                                  )
   TEST_LINE( Transform( .T., "@RZ #")                                  , " "                                  )
   TEST_LINE( Transform( .F., "@RZ #")                                  , " "                                  )
   TEST_LINE( Transform( .T., "@RZ #ylntfhekko")                        , "           "                        )
   TEST_LINE( Transform( .F., "@RZ #ylntfhekko")                        , "           "                        )
   TEST_LINE( Transform( .T., "@RZ #YLNTFHEKKO")                        , "           "                        )
   TEST_LINE( Transform( .F., "@RZ #YLNTFHEKKO")                        , "           "                        )
   TEST_LINE( Transform( .T., "@RZ YLNTFHEKKO#")                        , "           "                        )
   TEST_LINE( Transform( .F., "@RZ YLNTFHEKKO#")                        , "           "                        )
   TEST_LINE( Transform( .T., "@RZ XLNTFHEKKO#")                        , "           "                        )
   TEST_LINE( Transform( .F., "@RZ XLNTFHEKKO#")                        , "           "                        )
   TEST_LINE( Transform( .T., "@RZ ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,") , "                                  " )
   TEST_LINE( Transform( .F., "@RZ ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.,") , "                                  " )

   SET DATE FORMAT TO "MM/DD/YY"

   TEST_LINE( Transform( "abcd", "@9!*" )                   , "ABCD"          )
   TEST_LINE( Transform( "abcd", "@_9!*" )                  , "ABCD"          )
   TEST_LINE( Transform( "abcd", "@_9" + chr( 9 ) + "9!9")  , "aBc"           )
   TEST_LINE( Transform( "abcd", "@!!!" )                   , "ABCD"          )
   TEST_LINE( Transform( "abcd", "@9" )                     , "abcd"          )

   TEST_LINE( Transform(  134.24, "99,999.99" )             , "   134.24"     )
   TEST_LINE( Transform(  134.24, "@E 99,999.99" )          , "   134,24"     )
   TEST_LINE( Transform( -134.24, "@E 99,999.99" )          , "  -134,24"     )
   TEST_LINE( Transform(  134.24, "@E99,999.99" )           , "       134,24" )
   TEST_LINE( Transform( -134.24, "@E99,999.99" )           , "      -134,24" )

   TEST_LINE( Transform(  7, "@C 9999" )                    , "   7 CR"       )
   TEST_LINE( Transform( -7, "@X 9999" )                    , "   7 DB"       )

   TEST_LINE( Transform( hb_SToD( "19920509" ), "@E" )      , "09/05/92"      )

   TEST_LINE( Transform( val("3.10"), "@X" )                , "3.10"          )
   TEST_LINE( Transform(        0.80, ".9999" )             , ".8000"         )
   TEST_LINE( Transform(       -0.80, ".9999" )             , ".****"         )
   TEST_LINE( Transform(   12345.123, "@X99" )              , "     12345.123" )
   TEST_LINE( Transform(  -12345.123, "@X99" )              , "     12345.123 DB" )
   TEST_LINE( Transform(   123456.78, "@E" )                , "    123456,78" )
   TEST_LINE( Transform(           0, "@C 9.99" )           , "0.00"          )

   dt := hb_SToD( "19871231" )
   SET DATE FORMAT TO "MM:DD:YYYY"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:1987"    )
   SET DATE FORMAT TO "MM:DD:YY"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:87"      )
   SET DATE FORMAT TO "MM<DD>YY"
   TEST_LINE( Transform( dt, "@E" )       , "31<12>87"      )
#ifdef __HARBOUR__
   /* this are wrongly converted by CA-Cl*pper */
   SET DATE FORMAT TO "DD:MM:YYYY"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:1987"    )
   SET DATE FORMAT TO "YYYY:MM:DD"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:1987"    )
   SET DATE FORMAT TO "YYYY:DD:MM"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:1987"    )
   SET DATE FORMAT TO "YY:MM:DD"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:87"      )
   SET DATE FORMAT TO "DD:MM:YY"
   TEST_LINE( Transform( dt, "@E" )       , "31:12:87"      )
   SET DATE FORMAT TO "<YY:DD.MM>"
   TEST_LINE( Transform( dt, "@E" )       , "<31:12.87>"    )
   SET DATE FORMAT TO "|YY|MM|DD|"
   TEST_LINE( Transform( dt, "@E" )       , "|31|12|87|"    )
#endif
   SET DATE FORMAT TO "MM.DD.YYYY"
   TEST_LINE( Transform( dt, "@E" )       , "31.12.1987"    )

   TEST_LINE( Transform(           -5, "@(Z $###,##9.99" )  , "(      5.00)"  )
   TEST_LINE( Transform(          -10, "@)Z $###,##9.99" )  , "$    (10.00)"  )
   TEST_LINE( Transform(          -20, "@Z $###,##9.99"  )  , "$    -20.00"   )
   TEST_LINE( Transform(          100, "9999."           )  , " 100."         )
   TEST_LINE( Transform(          1.1, "@B!99.99"        )  , "1.1         "  )
   TEST_LINE( Transform(       12.345, "@R 99/99"        )  , "  /12"         )
   TEST_LINE( Transform( "1234567890", "@9"              )  , "1234567890"    )
   TEST_LINE( Transform(   1234567890, "@9"              )  , " 1234567890"   )
   TEST_LINE( Transform(         1234, "9 999"           )  , "1 234"         )
   TEST_LINE( Transform(   123.123456, "999.99.99.99"    )  , "123.12.45.  "  )
   TEST_LINE( Transform(   123.123456, "$$$.$$.$$.$$"    )  , "123.12.45.  "  )
   TEST_LINE( Transform(   123.123456, "***.**.**.**"    )  , "123.12.45.  "  )
   TEST_LINE( Transform(        99999, "9.999"           )  , "*.***"         )
   TEST_LINE( Transform(           99, "*.***"           )  , "*.***"         )
   TEST_LINE( Transform(        12345, "9999."           )  , "****."         )

   TEST_LINE( Transform(    -12345.00, "@("              )  , "(    12345.00)" )
   TEST_LINE( Transform(    -12345.00, "@)"              )  , "    (12345.00)" )
   TEST_LINE( Transform(-123456789.00, "@("              )  , "(123456789.00)" )
   TEST_LINE( Transform(-123456789.00, "@)"              )  , "(123456789.00)" )
   TEST_LINE( Transform(  -1234567890, "@("              )  , "(         1234567890)" )
   TEST_LINE( Transform(  -1234567890, "@)"              )  , "         (1234567890)" )
   TEST_LINE( Transform(       -12345, "@( [999999]"     )  , "( 12345])"     )
   TEST_LINE( Transform(       -12345, "@) [999999]"     )  , "[(12345])"     )
   TEST_LINE( Transform(       -12345, "@( $999999"      )  , "( 12345)"      )
   TEST_LINE( Transform(       -12345, "@) $999999"      )  , "$(12345)"      )
   TEST_LINE( Transform(       -12345, "@( #999999"      )  , "( 12345)"      )
   TEST_LINE( Transform(       -12345, "@) #999999"      )  , " (12345)"      )
   TEST_LINE( Transform(       -12345, "@( $99999"       )  , "(12345)"       )
   TEST_LINE( Transform(       -12345, "@) $99999"       )  , "(12345)"       )
   TEST_LINE( Transform(       -12345, "@( #99999"       )  , "(12345)"       )
   TEST_LINE( Transform(       -12345, "@) #99999"       )  , "(12345)"       )
   TEST_LINE( Transform(       -12345, "@( 6798^999"     )  , "(7*8^***)"     )
   TEST_LINE( Transform(       -12345, "@( 9798^9999"    )  , "(718^2345)"    )

   TEST_LINE( Transform(       134.24, "@E99,999.99"     )  , "       134,24" )
   TEST_LINE( Transform(      -134.24, "@E99,999.99"     )  , "      -134,24" )
   TEST_LINE( Transform(         0.80, ".9999"           )  , ".8000"         )
   TEST_LINE( Transform(        -0.80, ".9999"           )  , ".****"         )
   TEST_LINE( Transform(    12345.123, "@X99"            )  , "     12345.123" )
   TEST_LINE( Transform(   -12345.123, "@X99"            )  , "     12345.123 DB" )
   TEST_LINE( Transform(    123456.78, "@E"              )  , "    123456,78" )
   TEST_LINE( Transform(            0, "@C 9.99"         )  , "0.00"          )
   TEST_LINE( Transform(          1.1, "@B!99.99"        )  , "1.1         "  )
   TEST_LINE( Transform(       -12345, "@) [999999]"     )  , "[(12345])"     )
   TEST_LINE( Transform(       -12345, "@) $999999"      )  , "$(12345)"      )
   TEST_LINE( Transform(       -12345, "@) *999999"      )  , "*(12345)"      )
   TEST_LINE( Transform(       -12345, "@) #999999"      )  , " (12345)"      )
   TEST_LINE( Transform(       -12345, "@) *9$9*999]"    )  , "*($12345])"    )
   TEST_LINE( Transform(       -12345, "@) *999*999]"    )  , "* (12345])"    )
   TEST_LINE( Transform(       -12345, "@) 0999*999]"    )  , "0 (12345])"    )
   TEST_LINE( Transform(       -12345, "@) 1999*999]"    )  , "1 (12345])"    )
   TEST_LINE( Transform(       -12345, "@) *[99*999]"    )  , "([ 12345])"    )
   TEST_LINE( Transform(       -12345, "@) *****999]"    )  , "(**12345])"    )
   TEST_LINE( Transform(       -12345, "@) *1***999]"    )  , "(1*12345])"    )
   TEST_LINE( Transform(       -12345, "@) * 999999]"    )  , "* (12345])"    )
   TEST_LINE( Transform(           -5, "@(Z $###,##9.99" )  ,  "(      5.00)" )
   TEST_LINE( Transform(          -10, "@)Z $###,##9.99" )  , "$    (10.00)"  )
   TEST_LINE( Transform(           -5, "@(Z $999,999.99" )  ,  "(      5.00)" )
   TEST_LINE( Transform(          -10, "@)Z $999,999.99" )  , "$    (10.00)"  )
   TEST_LINE( Transform(           -5, "@(Z 999,999.99"  )  ,   "(     5.00)" )
   TEST_LINE( Transform(          -10, "@)Z 999,999.99"  )  ,  "    (10.00)"  )
   TEST_LINE( Transform(          -20, "@Z $###,##9.99"  )  ,  "$    -20.00"  )
   TEST_LINE( Transform(          0.1, ".9"              )  , ".1"            )
   TEST_LINE( Transform(          0.0, ".9"              )  , ".0"            )
   TEST_LINE( Transform(            1, ".9"              )  , ".*"            )
   TEST_LINE( Transform(         .456, ".9"              )  , ".5"            )
   TEST_LINE( Transform(          123, "99.-"            )  , "**.-"          )

   TEST_LINE( Transform(      -123.45, "999,999.99"               )  , "   -123.45" )
   TEST_LINE( Transform(   -123456.78, "999,999,999.99"           )  , "   -123,456.78" )
   TEST_LINE( Transform(   -123456.78, "$$$,$$$,$$$.$$"           )  , "$$ -123,456.78" )
   TEST_LINE( Transform(   -123456.78, "***,***,***.**"           )  , "***-123,456.78" )
   TEST_LINE( Transform(    123456.78, "@E 888,$$$,$$$.$$"        )  , "888.123.456,78" )
   TEST_LINE( Transform(    123456.78, "@E 888x,$$$,$$$.$$"       )  , "888xx123.456,78" )
   TEST_LINE( Transform(    123456.78, "@E 888x,,$$$,$$$.$$"      )  , "888xxx123.456,78" )
   TEST_LINE( Transform(    123456.78, "@E 8,88x,,$$$,$$$.$$"     )  , "8.88xxx123.456,78" )
   TEST_LINE( Transform(    123456.78, "@E 8,88x,,$$$,,$$$.$$"    )  , "8.88xxx123..456,78" )
   TEST_LINE( Transform(    123456.78, "@E 8,88x,,$$$,,$$$.$$77,7")  , "8.88xxx123..456,7877,7" )
   TEST_LINE( Transform(       123456, "@E 8,88x,,$$$,,$$$77,7"   )  , "8.88xxx123..45677,7" )
   TEST_LINE( Transform(       123456, "@E -,999,999"             )  , " -123.456" )
   TEST_LINE( Transform(        12345, "@E -,999,999"             )  , " - 12.345" )
   TEST_LINE( Transform(        12345, "@E -,|999,999"            )  , " -| 12.345" )
   TEST_LINE( Transform(        12345, "@E ^-,|999,999"           )  , "^^-| 12.345" )
   TEST_LINE( Transform(        12345, "@E 1-,|999,999"           )  , "11-| 12.345" )
   TEST_LINE( Transform(        12345, "@E |--,|999,999"          )  , "|---| 12.345" )

   TEST_LINE( Transform(        12.34, "@E 99'99"                 )  , "  '12"      )
   TEST_LINE( Transform(        12.34, "99,99,11"                 )  , "   12,11"   )
   TEST_LINE( Transform(        12.34, "@E 99,99,11"              )  , "   12,11"   )
   TEST_LINE( Transform(        12.34, "@E 99,"                   )  , "12,"        )
   TEST_LINE( Transform(        12.34, "@E 9,9"                   )  , "1.2"        )
   TEST_LINE( Transform(        12.34, "@E ab,cd.ef9,9.99,.--"    )  , "abbcd,ef***,* ,.--" )
   TEST_LINE( Transform(        12.34, "@E ab,cd,ef9,9.99,.--"    )  , "abbcddef1.2,34,.--" )
   TEST_LINE( Transform(        12.34, "@E ,ab,cd,ef9,9.99,.--"   )  , ",abbcddef1.2,34,.--" )
   TEST_LINE( Transform(        12.34, "@E ,,,,99,.99,.--"        )  , ",,,,12.,34,.--" )
   TEST_LINE( Transform(        124.4, "@E ,,,,9,9.99,.--"        )  , ",,,,***,**,.--" )
   TEST_LINE( Transform(          1.2, "@E ,,,,*,*.**,.--"        )  , ",,,,**1,20,.--" )
   TEST_LINE( Transform(        12.34, "@E ,,,,*,*.**,.--"        )  , ",,,,1.2,34,.--" )
   TEST_LINE( Transform(        12.34, "@E ,,,,*,*.**,.--,--"     )  , ",,,,1.2,34,.--,--" )
   TEST_LINE( Transform(        12.34, "@E ,,,,*,*,.,**"          )  , ",,,,1.2.,,34" )
   TEST_LINE( Transform(        12.34, ",,,,*,*,.,**"             )  , ",,,,1,2,..34" )
   TEST_LINE( Transform(        12.34, ",,,,*,*,.,*|,*"           )  , ",,,,1,2,..3||4" )
   TEST_LINE( Transform(        12.34, ",,,,*,*,.,*,*"            )  , ",,,,1,2,..3,4" )
   TEST_LINE( Transform(123.345678912, "@E 999.99.99,99.99."      )  , "123,34,67.89,  ." )
#ifdef __HARBOUR__
   TEST_LINE( Transform(  1234567890123456789, "99999999999999999999" ) , " 1234567890123456789" )
   TEST_LINE( Transform( -1234567890123456789, "99999999999999999999" ) , "-1234567890123456789" )
#else
   TEST_LINE( Transform(  1234567890123456789, "99999999999999999999" ) , " 1234567890123457000" )
   TEST_LINE( Transform( -1234567890123456789, "99999999999999999999" ) , "-1234567890123457000" )
#endif
   Set( _SET_DATEFORMAT, "YYYY/MM/DD" )
   TEST_LINE( Transform(      12345678, "@D" ), "1234/56/78" )
   Set( _SET_DATEFORMAT, "YYYY.MM.DD" )
   TEST_LINE( Transform(    1234.56789, "@D" ), "1234.56.9 " )
   Set( _SET_DATEFORMAT, "YYYY.MM:DD" )
   TEST_LINE( Transform(    1234.56789, "@D" ), "1234.56:79" )
   TEST_LINE( Transform( 123.345678912, "@D" ), " 123.34:57" )

   Set( _SET_DATEFORMAT, "MM-DD-YYYY" )
   TEST_LINE( Transform( .T., "@RE <|,yY#lL,|>" )           , "99-99-9999T"   )
   TEST_LINE( Transform( .F., "@RE <|,yY#lL,|>" )           , "99-99-9999F"   )
   TEST_LINE( Transform( .T., "@RD <|,yY#lL,|>" )           , "99-99-9999T"   )
   TEST_LINE( Transform( .F., "@RD <|,yY#lL,|>" )           , "99-99-9999F"   )
   TEST_LINE( Transform( .F., "@DE <|,yY#lL,|>" )           , "9"             )

   TEST_LINE( Transform( "abcdefghij", "@S15! <XXXXXXXX>" ) , "<BCDEFGHI>"    )
   TEST_LINE( Transform( "abcdefghij", "@S0! <XXXXXXXX>"  ) , "<BCDEFGHI>"    )
   TEST_LINE( Transform( "abcdefghij", "@S5! <XXXXXXXX>"  ) , "<BCDE"         )

   SET FIXED ON

   TEST_LINE( Transform(           1234,        ), "            1234"   )
   TEST_LINE( Transform(           1234, ""     ), "            1234"   )
   TEST_LINE( Transform(           1234, "@"    ), "            1234"   )
   TEST_LINE( Transform(           1234, "@!"   ), "            1234"   )
   TEST_LINE( Transform(          -1234,        ), "           -1234"   )
   TEST_LINE( Transform(          -1234, "@"    ), "           -1234"   )
#ifdef HB_CLP_STRICT
   TEST_LINE( Transform( round(123  ,0),        ), "       123.00"      )
   TEST_LINE( Transform( round(123,  0), "@!"   ), "       123.00"      )
   TEST_LINE( Transform( round(123.0,0),        ), "       123.00"      )
   TEST_LINE( Transform( round(123.0,0), "@!"   ), "       123.00"      )
#endif

   TEST_LINE( Transform(       1234.567,        ), "      1234.57"      )
   TEST_LINE( Transform(       1234.567, ""     ), "      1234.57"      )
   TEST_LINE( Transform(       1234.567, "@"    ), "      1234.57"      )
   TEST_LINE( Transform(       1234.567, "@!"   ), "      1234.57"      )
   TEST_LINE( Transform(      -1234.567,        ), "     -1234.57"      )
   TEST_LINE( Transform(      -1234.567, "@"    ), "     -1234.57"      )
   TEST_LINE( Transform(    val("-1.0"),        ), "-1.00"              )
   TEST_LINE( Transform(    val("-1.0"), "@"    ), "-1.00"              )
   TEST_LINE( Transform(    val("-123"),        ), "      -123"         )
   TEST_LINE( Transform(    val("-123"), "@"    ), "      -123"         )
   TEST_LINE( Transform(              0,        ), "               0"   )
   TEST_LINE( Transform(            0.0,        ), "         0.00"      )
   TEST_LINE( Transform(       val("1"),        ), "      1"            )
   TEST_LINE( Transform(      val("12"),        ), "      12"           )
   TEST_LINE( Transform(     val("123"),        ), "      123"          )
   TEST_LINE( Transform(    val("1234"),        ), "      1234"         )
   SET DECIMAL TO 3
   TEST_LINE( Transform(            0.0,        ), "         0.000"     )
   TEST_LINE( Transform(       val("1"),        ), "        1"          )
   TEST_LINE( Transform(      val("12"),        ), "        12"         )
   TEST_LINE( Transform(     val("123"),        ), "        123"        )
   TEST_LINE( Transform(    val("1234"),        ), "        1234"       )
   SET DECIMAL TO 4
   TEST_LINE( Transform(            0.0,        ), "         0.0000"    )
   TEST_LINE( Transform(       val("1"),        ), "          1"        )
   TEST_LINE( Transform(      val("12"),        ), "          12"       )
   TEST_LINE( Transform(     val("123"),        ), "          123"      )
   TEST_LINE( Transform(    val("1234"),        ), "          1234"     )

   SET FIXED OFF

   TEST_LINE( Transform(          -1234,        ), "     -1234"         )
   TEST_LINE( Transform(          -1234, "@B"   ), "-1234     "         )
   TEST_LINE( Transform(          -1234, "@("   ), "(     1234)"        )
   TEST_LINE( Transform(          -1234, "@)"   ), "     (1234)"        )
   TEST_LINE( Transform(          -1234, "@B)"  ), "(1234)     "        )
   TEST_LINE( Transform(          -1234, "@B("  ), "(1234)     "        )
   TEST_LINE( Transform(       "(  12)", "@B("  ), "(  12)"             )
   TEST_LINE( Transform(       "(  12)", "@B)"  ), "(  12)"             )
   TEST_LINE( Transform(        "   12", "@B("  ), "12   "              )
   TEST_LINE( Transform(        "   12", "@B)"  ), "12   "              )
#ifndef __CLIPPER__
   TEST_LINE( Transform(           1234, "@L"   ), "0000001234"         )
   TEST_LINE( Transform(           1234, "@0"   ), "0000001234"         )
   TEST_LINE( Transform(           1234, "@L("  ), "0000001234"         )
   TEST_LINE( Transform(           1234, "@0)"  ), "0000001234"         )
   TEST_LINE( Transform(          -1234, "@L("  ), "(000001234)"        )
   TEST_LINE( Transform(          -1234, "@0)"  ), "(000001234)"        )
   /* please test it with FoxPro and Xbase++ to check if they give
    * the same result
    */
   TEST_LINE( Transform(          -1234, "@L"   ), "-000001234"         )
   TEST_LINE( Transform(          -1234, "@0"   ), "-000001234"         )
#endif
   /* FlagShip extensions */
   TEST_LINE( Transform(          -1234, "@Z"   ), "     -1234"         )
   TEST_LINE( Transform(           1234, "@Z"   ), "      1234"         )
   TEST_LINE( Transform(          -1234, "@F"   ), "     -1234"         )
   TEST_LINE( Transform(           1234, "@F"   ), "      1234"         )
   TEST_LINE( Transform(          -1234, "@T"   ), "     -1234"         )
   TEST_LINE( Transform(           1234, "@T"   ), "      1234"         )

   TEST_LINE( Transform(   123456789.12, "@,39 999,999,999.99" ) , "123,456,789.12" )
   TEST_LINE( Transform(   123456789.12, "@,39 999,999,999.99" ) , "123,456,789.12" )
   TEST_LINE( Transform(        123.456, "@R 9 9 9.9"          ) , "1 2 3.5" )

   Set( _SET_FIXED, cOldFixed )
   Set( _SET_DECIMALS, cOldDecim )
   Set( _SET_DATEFORMAT, cOldDate )

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
