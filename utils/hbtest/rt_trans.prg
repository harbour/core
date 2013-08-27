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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
   LOCAL cOldFixed := Set( _SET_FIXED )
   LOCAL cOldDecim := Set( _SET_DECIMALS )
   LOCAL dt, df

   /* Transform() */

   HBTEST Transform( NIL       , NIL        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST Transform( NIL       , ""         )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;C: F:S"
   HBTEST Transform( NIL       , "@"        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:NIL;C:@ F:S"
   HBTEST Transform( {}        , NIL        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};U:NIL F:S"
   HBTEST Transform( {}        , ""         )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};C: F:S"
   HBTEST Transform( {}        , "@"        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:A:{.[0].};C:@ F:S"
   HBTEST Transform( ErrorNew(), NIL        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;U:NIL F:S"
   HBTEST Transform( ErrorNew(), ""         )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST Transform( ErrorNew(), "@"        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:O:ERROR Object;C:@ F:S"
   HBTEST Transform( {|| NIL } , NIL        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};U:NIL F:S"
   HBTEST Transform( {|| NIL } , ""         )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};C: F:S"
   HBTEST Transform( {|| NIL } , "@"        )          IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:B:{||...};C:@ F:S"

   HBTEST Transform( "", "" )                          IS ""
   HBTEST Transform( "", "@" )                         IS ""
#ifndef __XPP__
   HBTEST Transform( "", NIL )                         IS ""
#endif
   HBTEST Transform( "", 100 )                         IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:;N:100 F:S"
   HBTEST Transform( "hello", "" )                     IS "hello"
   HBTEST Transform( "hello", "@" )                    IS "hello"
#ifndef __XPP__
   HBTEST Transform( "hello", NIL )                    IS "hello"
#endif
   HBTEST Transform( "hello", 100 )                    IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:hello;N:100 F:S"
   HBTEST Transform( 100.2, "" )                       IS "       100.2"
   HBTEST Transform( 100.2, "@" )                      IS "       100.2"
#ifndef __XPP__
   HBTEST Transform( 100.2, NIL )                      IS "       100.2"
#endif
   HBTEST Transform( 100.2, 100 )                      IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.2;N:100 F:S"
   HBTEST Transform( 100.20, "" )                      IS "       100.20"
   HBTEST Transform( 100.20, "@" )                     IS "       100.20"
#ifndef __XPP__
   HBTEST Transform( 100.20, NIL )                     IS "       100.20"
#endif
   HBTEST Transform( 100.20, 100 )                     IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.20;N:100 F:S"
   HBTEST Transform( Val( "100.2" ), "" )              IS "100.2"
   HBTEST Transform( Val( "100.2" ), "@" )             IS "100.2"
#ifndef __XPP__
   HBTEST Transform( Val( "100.2" ), NIL )             IS "100.2"
#endif
   HBTEST Transform( Val( "100.2" ), 100 )             IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.2;N:100 F:S"
   HBTEST Transform( Val( "100.20" ), "" )             IS "100.20"
// HBTEST Transform( Val( "100.20" ), "@" )            IS "100.20"
#ifndef __XPP__
   HBTEST Transform( Val( "100.20" ), NIL )            IS "100.20"
#endif
   HBTEST Transform( Val( "100.20" ), 100 )            IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:N:100.20;N:100 F:S"
   HBTEST Transform( sdDate, "" )                      IS "1984-03-25"
   HBTEST Transform( sdDate, "@" )                     IS "1984-03-25"
#ifndef __XPP__
   HBTEST Transform( sdDate, NIL )                     IS "1984-03-25"
#endif
   HBTEST Transform( sdDate, 100 )                     IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:D:0d19840325;N:100 F:S"
   HBTEST Transform( .T., "" )                         IS "T"
   HBTEST Transform( .T., "@" )                        IS "T"
#ifndef __XPP__
   HBTEST Transform( .F., NIL )                        IS "F"
#endif
   HBTEST Transform( .F., 100 )                        IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:L:.F.;N:100 F:S"

   HBTEST Transform( scStringM, "!!!!!"    )           IS "HELLO"
   HBTEST Transform( scStringM, "@!"       )           IS "HELLO"
#ifdef __HARBOUR__
   HBTEST Transform( @scStringM, "!!!!!"    )          IS "HELLO"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:Hello;C:!!!!! F:S" */
   HBTEST Transform( @scStringM, "@!"       )          IS "HELLO"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:U:Hello;C:@! F:S" */
#endif
   HBTEST Transform( scStringM, "" )                   IS "Hello"
#ifndef __XPP__
   HBTEST Transform( scStringM, NIL )                  IS "Hello"
#endif
   HBTEST Transform( scStringM, 100 )                  IS "E 1 BASE 1122 Argument error (TRANSFORM) OS:0 #:0 A:2:C:Hello;N:100 F:S"

   HBTEST Transform( "abcdef", "@! !lkm!" )            IS "ABkmE"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! 1234567890" ) IS "12345678I0"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyzabcdefg", "@! abcdefghijklmnopqrstuvwxyzabcdefg" ) IS "AbcdefghijkLmNopqrstuvwXYzAbcdefg"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! `~!@#$% ^&*()_+-={}\|;':" )  IS "`~C@E$% ^&*()_+-={}\|;':"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! ,./<>?" )                    IS ",./<>?"
   HBTEST Transform( "hello", " @!" )                  IS " @L"

   HBTEST Transform( "abcdef", "@R! !lkm!" )           IS "ABkmC"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! 1234567890" ) IS "12345678A0"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyzabcdefg", "@R! abcdefghijklmnopqrstuvwxyzabcdefg" ) IS "AbcdefghijkBmCopqrstuvwDNzFbcdefg"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! `~!@#$% ^&*()_+-={}\|;':" )  IS "`~A@B$% ^&*()_+-={}\|;':"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,./<>?" )                    IS ",./<>?ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "hello", " @R!" )                 IS " @RL"

   HBTEST Transform( "abc", "@R !!!!" )                IS "ABC "
   HBTEST Transform( "abc", "@R XXXX" )                IS "abc "
   HBTEST Transform( "abc", "@R !!" )                  IS "AB"
   HBTEST Transform( "abc", "@R XX" )                  IS "ab"
   HBTEST Transform( "abc", "@!R !!!!" )               IS "ABC "
   HBTEST Transform( "abc", "@!R XXXX" )               IS "ABC "
   HBTEST Transform( "abc", "@!R !!" )                 IS "AB"
   HBTEST Transform( "abc", "@!R XX" )                 IS "AB"

   HBTEST Transform( "Hallo   ", "!!!!!"    )          IS "HALLO"
   HBTEST Transform( "Hallo   ", "!!A!!"    )          IS "HAlLO"
   HBTEST Transform( "Hallo   ", "!!A9!"    )          IS "HAllO"
   HBTEST Transform( "Hallo   ", "!QA9!"    )          IS "HQllO"
   HBTEST Transform( "Hallo   ", "ZQA9!"    )          IS "ZQllO"
   HBTEST Transform( "Hall"    , "ZQA9!"    )          IS "ZQll"
   HBTEST Transform( "Hallo   ", "!AAA"     )          IS "Hall"
   HBTEST Transform( "Hallo   ", "@!"       )          IS "HALLO   "
   HBTEST Transform( "Hallo   ", "@! AA"    )          IS "HA"
   HBTEST Transform( "Hallo   ", "@R"       )          IS "Hallo   "
   HBTEST Transform( "Hallo   ", "@Z"       )          IS "        "
   HBTEST Transform( "Hallo   ", "@R !!"    )          IS "HA"
   HBTEST Transform( "Hi"      , "@R !!!"   )          IS "HI "
   HBTEST Transform( "Hallo   ", ""         )          IS "Hallo   "

   HBTEST Transform( .T.       , ""         )          IS "T"
   HBTEST Transform( .F.       , ""         )          IS "F"
   HBTEST Transform( .T.       , "L"        )          IS "T"
   HBTEST Transform( .F.       , "L"        )          IS "F"
   HBTEST Transform( .T.       , "Y"        )          IS "Y"
   HBTEST Transform( .F.       , "Y"        )          IS "N"
   HBTEST Transform( .T.       , "X"        )          IS "X"
   HBTEST Transform( .F.       , "#"        )          IS "F"
   HBTEST Transform( .T.       , "X!"       )          IS "X"
   HBTEST Transform( .F.       , "@R Y"     )          IS "N"
   HBTEST Transform( .T.       , "@R X!"    )          IS "X!T"

   HBTEST Transform( hb_SToD( "20000101" ) , "@B"         ) IS "2000-01-01"
   HBTEST Transform( hb_SToD( "19901214" ) , "99/99/9999" ) IS "1990-12-14"
   HBTEST Transform( hb_SToD( "19901202" ) , "99.99.9999" ) IS "1990-12-02"
   HBTEST Transform( hb_SToD( "" )         , "99/99/9999" ) IS "    -  -  "
   HBTEST Transform( hb_SToD( "19901202" ) , "99/99/99"   ) IS "1990-12-02"
   HBTEST Transform( hb_SToD( "19901214" ) , "99-99-99"   ) IS "1990-12-14"
   HBTEST Transform( hb_SToD( "20040430" ) , "99.99.99"   ) IS "2004-04-30"
   HBTEST Transform( hb_SToD( "" )         , "99/99/99"   ) IS "    -  -  "
   HBTEST Transform( hb_SToD( "19920101" ) , "THISWRNG"   ) IS "1992-01-01"
   HBTEST Transform( hb_SToD( "19350605" ) , "999/99/9"   ) IS "1935-06-05"
   HBTEST Transform( hb_SToD( "19101112" ) , "9#-9#/##"   ) IS "1910-11-12"
   HBTEST Transform( hb_SToD( "19920101" ) , ""           ) IS "1992-01-01"
   HBTEST Transform( hb_SToD( "19920101" ) , "DO THIS "   ) IS "1992-01-01"
   HBTEST Transform( hb_SToD( "19920102" ) , "@E"         ) IS "02-01-1992"  /* Bug in CA-Cl*pper, it returns: "2-91901-02" */
   HBTEST Transform( 1234                  , "@D 9999"    ) IS "    -12-34"
   HBTEST Transform( 1234                  , "@BD 9999"   ) IS "-12-34    "
   df := Set( _SET_DATEFORMAT, "yyyy.mm.dd" )
   HBTEST Transform( 1234                  , "@D 9999"    ) IS "1234.00.0 "
   HBTEST Transform( 1234                  , "@BD 9999"   ) IS "1234.00.0 "
   Set( _SET_DATEFORMAT, df )

   SET CENTURY OFF

   HBTEST Transform( hb_SToD( "20000101" ) , "@B"         ) IS "00-01-01"
   HBTEST Transform( hb_SToD( "19901214" ) , "99/99/9999" ) IS "90-12-14"
   HBTEST Transform( hb_SToD( "19901202" ) , "99.99.9999" ) IS "90-12-02"
   HBTEST Transform( hb_SToD( "" )         , "99/99/9999" ) IS "  -  -  "
   HBTEST Transform( hb_SToD( "19901202" ) , "99/99/99"   ) IS "90-12-02"
   HBTEST Transform( hb_SToD( "19901214" ) , "99-99-99"   ) IS "90-12-14"
   HBTEST Transform( hb_SToD( "20040430" ) , "99.99.99"   ) IS "04-04-30"
   HBTEST Transform( hb_SToD( "" )         , "99/99/99"   ) IS "  -  -  "
   HBTEST Transform( hb_SToD( "19920101" ) , "THISWRNG"   ) IS "92-01-01"
   HBTEST Transform( hb_SToD( "19350605" ) , "999/99/9"   ) IS "35-06-05"
   HBTEST Transform( hb_SToD( "19101112" ) , "9#-9#/##"   ) IS "10-11-12"
   HBTEST Transform( hb_SToD( "19920101" ) , ""           ) IS "92-01-01"
   HBTEST Transform( hb_SToD( "19920101" ) , "DO THIS "   ) IS "92-01-01"
   HBTEST Transform( hb_SToD( "19920102" ) , "@E"         ) IS "02-01-92"   /* Bug in CA-Cl*pper, it returns: "01-92-02" */
   HBTEST Transform( 1234                  , "@D 9999"    ) IS "  -12-34"
   HBTEST Transform( 1234                  , "@BD 9999"   ) IS "-12-34  "
   df := Set( _SET_DATEFORMAT, "yy.mm.dd" )
   HBTEST Transform( 1234                  , "@D 9999"    ) IS "**.**.* "
   HBTEST Transform( 1234                  , "@BD 9999"   ) IS "**.**.* "
   Set( _SET_DATEFORMAT, df )

   SET CENTURY ON

   HBTEST Transform( 1         , "@b"           )      IS "1         "
   HBTEST Transform( 1         , "@B"           )      IS "1         "
   HBTEST Transform( 1.0       , "@B"           )      IS "1.0         "
   HBTEST Transform( 15        , "9999"         )      IS "  15"
   HBTEST Transform( 1.5       , "99.99"        )      IS " 1.50"
   HBTEST Transform( 1.5       , "9999"         )      IS "   2"
   HBTEST Transform( 15        , "####"         )      IS "  15"
   HBTEST Transform( 1.5       , "##.##"        )      IS " 1.50"
   HBTEST Transform( 1.5       , "####"         )      IS "   2"
   HBTEST Transform( 15        , " AX##"        )      IS " AX15"
   HBTEST Transform( 1.5       , "!9XPA.9"      )      IS "!1XPA.5"
   HBTEST Transform( -15       , "9999"         )      IS " -15"
   HBTEST Transform( -1.5      , "99.99"        )      IS "-1.50"
   HBTEST Transform( -15       , "$999"         )      IS "$-15"
   HBTEST Transform( -1.5      , "*9.99"        )      IS "-1.50"
   HBTEST Transform( 41        , "$$$9"         )      IS "$$41"
   HBTEST Transform( 41        , "***9"         )      IS "**41"
   HBTEST Transform( 15000     , "9999"         )      IS "****"
   HBTEST Transform( 15000     , "99,999"       )      IS "15,000"
   HBTEST Transform( 1500      , "99,999"       )      IS " 1,500"
   HBTEST Transform( 150       , "99,999"       )      IS "   150"
   HBTEST Transform( 150       , "99,99"        )      IS " 1,50"
   HBTEST Transform( 41        , "@Z 9999"      )      IS "  41"
   HBTEST Transform( 0         , "@Z 9999"      )      IS "    "
#ifdef __HARBOUR__
   HBTEST Transform( 41        , "@0 9999"      )      IS "0041"  /* Extension in Harbour, in CA-Cl*pper it should return: "  41" */
   HBTEST Transform( 0         , "@0 9999"      )      IS "0000"  /* Extension in Harbour, in CA-Cl*pper it should return: "   0" */
#endif
   HBTEST Transform( 41        , "@B 9999"      )      IS "41  "
   HBTEST Transform( 41        , "@B 99.9"      )      IS "41.0"
   HBTEST Transform( 7         , "@B 99.9"      )      IS "7.0 "
   HBTEST Transform( 7         , "@C 99.9"      )      IS " 7.0 CR"
   HBTEST Transform( -7        , "@C 99.9"      )      IS "-7.0"
   HBTEST Transform( 7         , "@X 99.9"      )      IS " 7.0"
   HBTEST Transform( -7        , "@X 99.9"      )      IS " 7.0 DB"
   HBTEST Transform( 7         , "@( 99.9"      )      IS " 7.0"
   HBTEST Transform( -7        , "@( 99.9"      )      IS "(7.0)"
   HBTEST Transform( 7         , "9X9Z5.9"      )      IS " X7Z5.0"
   HBTEST Transform( -7        , "@R 9X9^"      )      IS "-X7^"
   HBTEST Transform( -7        , "9X9^"         )      IS "-X7^"
   HBTEST Transform( 1         , "@R 9HI!"      )      IS "1HI!"
   HBTEST Transform( 1         , "9HI!"         )      IS "1HI!"
   HBTEST Transform( -12       , "@( 99"        )      IS "(*)"  /* Bug in CA-Cl*pper, it returns: "(2)" */
   HBTEST Transform( 12        , "@( 99"        )      IS "12"
   HBTEST Transform( 1         , ""             )      IS "         1"
   HBTEST Transform( 32768     , ""             )      IS "     32768"
   HBTEST Transform( -20       , ""             )      IS "       -20"
   HBTEST Transform( 1048576   , ""             )      IS "   1048576"
   HBTEST Transform( 21.65     , ""             )      IS "        21.65"
   HBTEST Transform( -3.33     , ""             )      IS "        -3.33"
   HBTEST Transform( -1234     , "@( 9999"      )      IS "(***)"  /* Bug in CA-Cl*pper, it returns: "(234)" */
   HBTEST Transform( -1234     , "@B 9999"      )      IS "****"
   HBTEST Transform( -1234     , "@B( 9999"     )      IS "(***)"  /* Bug in CA-Cl*pper, it returns: "(234)" */
   HBTEST Transform( 1234      , "@E 9,999.99"  )      IS "1.234,00"
   HBTEST Transform( 12.2      , "@E 9,999.99"  )      IS "   12,20"
   HBTEST Transform( -1234     , "@X 9999"      )      IS "1234 DB"
   HBTEST Transform( -1234     , "@BX 9999"     )      IS "1234 DB"
   HBTEST Transform( 1234      , "@B 9999"      )      IS "1234"
   HBTEST Transform( 1234      , "@BX 9999"     )      IS "1234"
   HBTEST Transform( 0         , "@Z 9999"      )      IS "    "
   HBTEST Transform( 0         , "@BZ 9999"     )      IS "    "
   HBTEST Transform( 2334      , "Xxxxx: #####" )      IS "Xxxxx:  2334"

   HBTEST Transform( "Hello", "@S-1" )                                               IS "Hello"
   HBTEST Transform( "Hello", "@S3.0!" )                                             IS "HEL"
   HBTEST Transform( "Hello", "@S3" )                                                IS "Hel"
   HBTEST Transform( "Hello", "@S13" ) + "<"                                         IS "Hello<"
   HBTEST Transform( "Hello", "@S000000000003K" )                                    IS "Hel"
   HBTEST Transform( "Hello", "@S3K" )                                               IS "Hel"
   HBTEST Transform( "Hello", "@S 3K" )                                              IS "3K"
   HBTEST Transform( "Hello", "@S3" + Chr( 9 ) )                                     IS "Hel"
   HBTEST Transform( "abcdef", "@! !lkm!" )                                          IS "ABkmE"
   HBTEST Transform( "abcdef", "@! !LKM!" )                                          IS "ABKME"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! !lkm!x" )                     IS "ABkmEF"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! abcdefghijklmnopqrstuvwxyz" ) IS "AbcdefghijkLmNopqrstuvwXYz"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! `~!@#$%^&*()_+-={}[]\|;':" )  IS "`~C@E$%^&*()_+-={}[]\|;':"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@! ,./<>?" )                     IS ",./<>?"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,./<>?" )                    IS ",./<>?ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,./<>" )                     IS ",./<>ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,./<" )                      IS ",./<ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,./" )                       IS ",./ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ,." )                        IS ",.ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! ," )                         IS ",ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! " )                          IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! 0" )                         IS "0ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "abcdefghijklmnopqrstuvwxyz", "@R! B" )                         IS "BABCDEFGHIJKLMNOPQRSTUVWXYZ"
   HBTEST Transform( "zbcdefghijklmnopqrstuvwxya", "@R! A" )                         IS "Z"
   HBTEST Transform( "zbcdefghijklmnopqrstuvwxya", "@R! Z" )                         IS "ZZBCDEFGHIJKLMNOPQRSTUVWXYA"

   HBTEST Transform( " Hello ", "@Z" )                 IS "       "
   HBTEST Transform( "Hello", "@Z" )                   IS "     "
   HBTEST Transform( "", "@Z" )                        IS ""
   HBTEST Transform( "   ", "@Z" )                     IS "   "
   HBTEST Transform( " Hello ", "@Z z" )               IS " "
   HBTEST Transform( "Hello", "@Z z" )                 IS " "
   HBTEST Transform( "", "@Z z" )                      IS ""
   HBTEST Transform( "   ", "@Z z" )                   IS " "
   HBTEST Transform( "a", "@! Y" )                     IS "N"
   HBTEST Transform( "b", "@! Y" )                     IS "N"
   HBTEST Transform( "c", "@! Y" )                     IS "N"
   HBTEST Transform( "d", "@! Y" )                     IS "N"
   HBTEST Transform( "e", "@! Y" )                     IS "N"
   HBTEST Transform( "f", "@! Y" )                     IS "N"
   HBTEST Transform( "g", "@! Y" )                     IS "N"
   HBTEST Transform( "h", "@! Y" )                     IS "N"
   HBTEST Transform( "i", "@! Y" )                     IS "N"
   HBTEST Transform( "j", "@! Y" )                     IS "N"
   HBTEST Transform( "k", "@! Y" )                     IS "N"
   HBTEST Transform( "l", "@! Y" )                     IS "N"
   HBTEST Transform( "m", "@! Y" )                     IS "N"
   HBTEST Transform( "n", "@! Y" )                     IS "N"
   HBTEST Transform( "o", "@! Y" )                     IS "N"
   HBTEST Transform( "p", "@! Y" )                     IS "N"
   HBTEST Transform( "q", "@! Y" )                     IS "N"
   HBTEST Transform( "r", "@! Y" )                     IS "N"
   HBTEST Transform( "s", "@! Y" )                     IS "N"
   HBTEST Transform( "t", "@! Y" )                     IS "Y"
   HBTEST Transform( "u", "@! Y" )                     IS "N"
   HBTEST Transform( "v", "@! Y" )                     IS "N"
   HBTEST Transform( "w", "@! Y" )                     IS "N"
   HBTEST Transform( "x", "@! Y" )                     IS "N"
   HBTEST Transform( "y", "@! Y" )                     IS "Y"
   HBTEST Transform( "z", "@! Y" )                     IS "N"
   HBTEST Transform( "t", "Y" )                        IS "Y"
   HBTEST Transform( "y", "Y" )                        IS "Y"
   HBTEST Transform( "T", "Y" )                        IS "Y"
   HBTEST Transform( "Y", "Y" )                        IS "Y"
   HBTEST Transform( "T", "@! Y" )                     IS "Y"
   HBTEST Transform( "Y", "@! Y" )                     IS "Y"
   HBTEST Transform( "abc", "@R !!!!" )                IS "ABC "
   HBTEST Transform( "abc", "@R XXXX" )                IS "abc "
   HBTEST Transform( "abc", "@R !!" )                  IS "AB"
   HBTEST Transform( "abc", "@R XX" )                  IS "ab"
   HBTEST Transform( "abc", "@!R !!!!" )               IS "ABC "
   HBTEST Transform( "abc", "@!R XXXX" )               IS "ABC "
   HBTEST Transform( "abc", "@!R !!" )                 IS "AB"
   HBTEST Transform( "abc", "@!R XX" )                 IS "AB"
   HBTEST Transform( "abc", "@R" + Chr( 9 ) + "!!!!" ) IS "ABC "
   HBTEST Transform( "abc", "@R!!!!!!!!!!!!" )         IS "ABC"
   HBTEST Transform( "hello", "@ !!" )                 IS "HE"
   HBTEST Transform( "hello", "@" )                    IS "hello"
   HBTEST Transform( "hello", "@ " )                   IS "hello"
   HBTEST Transform( "hello", " @!" )                  IS " @L"
   HBTEST Transform( "abcdefgh", "@R! helloy" )        IS "heABoN"
   HBTEST Transform( "abcdefgh", "@R helloy" )         IS "heaboN"
   HBTEST Transform( "abcdefgh", "@R !!!!!!" )         IS "ABCDEF"
   HBTEST Transform( "abcdefgh", "@" )                 IS "abcdefgh"

   HBTEST Transform( 100, "@B" )                       IS "100       "
   HBTEST Transform( 100, "@b" )                       IS "100       "

   HBTEST Transform( .T., "l" )                        IS "T"
   HBTEST Transform( .F., "l" )                        IS "F"
   HBTEST Transform( .T., "L" )                        IS "T"
   HBTEST Transform( .F., "L" )                        IS "F"

   HBTEST Transform( "  H", "@B" )                     IS "H  "
   HBTEST Transform( Chr( 9 ) + " H", "@B" )           IS Chr( 9 ) + " H"
   HBTEST Transform( "  H ", "@B" )                    IS "H   "
   HBTEST Transform( "  H", "@Z" )                     IS "   "
   HBTEST Transform( "  H", "@ZB" )                    IS "   "
   HBTEST Transform( "  H", "@!" )                     IS "  H"
   HBTEST Transform( "19840325", "@D" )                IS "1984-32-"
   HBTEST Transform( "19840325", "@DE" )               IS "4-81932-"
   HBTEST Transform( "1984032598765", "@DE" )          IS "4-81932-98"

   SET CENTURY ON
   HBTEST Transform( "19840325", "@D" )                IS "1984-32-"
   HBTEST Transform( "19840325", "@DE" )               IS "4-81932-"
   HBTEST Transform( "1984032598765", "@DE" )          IS "4-81932-98"
   SET CENTURY OFF

   HBTEST Transform( "19840325", "@D" )                IS "19-40-25"
   HBTEST Transform( "19840325", "@DE" )               IS "40-19-25"
   HBTEST Transform( "1984032598765", "@DE" )          IS "40-19-25"
   HBTEST Transform( "1", "@D" )                       IS "1"
   HBTEST Transform( "19840325", "@D" )                IS "19-40-25"
   HBTEST Transform( "19840325", "@DR" )               IS "19-84-03"
   HBTEST Transform( "ABCDEFG", "@D" )                 IS "AB-DE-G"
   HBTEST Transform( "abcdefg", "@D !!" )              IS "ab-de-g"
   HBTEST Transform( "abcdefg", "@D!" )                IS "AB-DE-G"
   HBTEST Transform( "ABCDEFG", "@DB" )                IS "AB-DE-G"
   HBTEST Transform( "  CDEFG", "@DB" )                IS "-DE-G  "
   HBTEST Transform( "ABCDEFG", "@DBZ" )               IS "       "
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
    *          ? Transform( "0123456789", "" )
    *          ? Transform( "AB", "@E" )
    *          ? Transform( "ab", "@E" )
    * [druzus]
    */
   HBTEST Transform( ".", "@E" )                       IS " "
   HBTEST Transform( ",", "@E" )                       IS "."
   HBTEST Transform( "..", "@E" )                      IS "," + Chr( 0 )
   HBTEST Transform( ",,", "@E" )                      IS ".."
   HBTEST Transform( ",.,", "@E" )                     IS ",,-"
   HBTEST Transform( ".,.", "@E" )                     IS Chr( 0 ) + ".-"
   HBTEST Transform( "OPI", "@E ." )                   IS Chr( 0 ) + ",-"
   HBTEST Transform( "JKL", "@E ," )                   IS Chr( 0 ) + "P-"
   HBTEST Transform( "OPI", "@ER ." )                  IS "I -OP-  "
   HBTEST Transform( "JKL", "@ER ," )                  IS "L -JK-  "
   HBTEST Transform( "OPI", "@ER" )                    IS "I -OP-  "
   HBTEST Transform( "JKL", "@ER" )                    IS "L -JK-  "
#endif
   HBTEST Transform( CToD( "" ), "@DB" )               IS "-  -    "
   HBTEST Transform( CToD( "" ), "@DBR uiuijk" )       IS "-- - -    "
   HBTEST Transform( 100, "@B $99999" )                IS "$  100"
   HBTEST Transform( 10, "@BZ $99999" )                IS "$   10"
   HBTEST Transform( 10, "@BX $99999" )                IS "$   10"
   HBTEST Transform( 0, "@BZX $99999" )                IS "      "
   HBTEST Transform( -10, "@B(X $99999" )              IS "(10) DB   "
   HBTEST Transform( -10, "@(X $99999" )               IS "(   10) DB"
   HBTEST Transform( 0, "@B(X $99999" )                IS "$    0"
   HBTEST Transform( 0, "@B(ZX $99999" )               IS "      "

#ifndef __XPP__
   HBTEST Transform( sdDate, NIL )                     IS "84-03-25"
#endif
   HBTEST Transform( sdDate, "" )                      IS "84-03-25"
   HBTEST Transform( sdDate, "@Z" )                    IS "        "

   Set( _SET_DATEFORMAT, "DD/MMM/YYYY" )

   HBTEST Transform( .T., "#" )                                      IS "T"
   HBTEST Transform( .F., "#" )                                      IS "F"
   HBTEST Transform( .T., "#ylntfhekko" )                            IS "T"
   HBTEST Transform( .F., "#ylntfhekko" )                            IS "F"
   HBTEST Transform( .T., "#YLNTFHEKKO" )                            IS "T"
   HBTEST Transform( .F., "#YLNTFHEKKO" )                            IS "F"
   HBTEST Transform( .T., "YLNTFHEKKO#" )                            IS "Y"
   HBTEST Transform( .F., "YLNTFHEKKO#" )                            IS "N"
   HBTEST Transform( .T., "XLNTFHEKKO#" )                            IS "X"
   HBTEST Transform( .F., "XLNTFHEKKO#" )                            IS "X"
   HBTEST Transform( .T., "ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )     IS "A"
   HBTEST Transform( .F., "ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )     IS "A"
   HBTEST Transform( .T., "" )                                       IS "T"
   HBTEST Transform( .F., "" )                                       IS "F"
   HBTEST Transform( .T., "@" )                                      IS "T"
   HBTEST Transform( .F., "@" )                                      IS "F"
   HBTEST Transform( .T., "@R #" )                                   IS "T"
   HBTEST Transform( .F., "@R #" )                                   IS "F"
   HBTEST Transform( .T., "@R #ylntfhekko" )                         IS "T  ntfhekko"
   HBTEST Transform( .F., "@R #ylntfhekko" )                         IS "F  ntfhekko"
   HBTEST Transform( .T., "@R #YLNTFHEKKO" )                         IS "T  NTFHEKKO"
   HBTEST Transform( .F., "@R #YLNTFHEKKO" )                         IS "F  NTFHEKKO"
   HBTEST Transform( .T., "@R YLNTFHEKKO#" )                         IS "Y NTFHEKKO "
   HBTEST Transform( .F., "@R YLNTFHEKKO#" )                         IS "N NTFHEKKO "
   HBTEST Transform( .T., "@R XLNTFHEKKO#" )                         IS "XTNTFHEKKO "
   HBTEST Transform( .F., "@R XLNTFHEKKO#" )                         IS "XFNTFHEKKO "
   HBTEST Transform( .T., "@R ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )  IS "ABCDEFGHIJKTMNOPQRSTUVWX Z9 !$ *.,"
   HBTEST Transform( .F., "@R ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )  IS "ABCDEFGHIJKFMNOPQRSTUVWX Z9 !$ *.,"
   HBTEST Transform( .T., "@Z #" )                                   IS " "
   HBTEST Transform( .F., "@Z #" )                                   IS " "
   HBTEST Transform( .T., "@Z #ylntfhekko" )                         IS " "
   HBTEST Transform( .F., "@Z #ylntfhekko" )                         IS " "
   HBTEST Transform( .T., "@Z #YLNTFHEKKO" )                         IS " "
   HBTEST Transform( .F., "@Z #YLNTFHEKKO" )                         IS " "
   HBTEST Transform( .T., "@Z YLNTFHEKKO#" )                         IS " "
   HBTEST Transform( .F., "@Z YLNTFHEKKO#" )                         IS " "
   HBTEST Transform( .T., "@Z XLNTFHEKKO#" )                         IS " "
   HBTEST Transform( .F., "@Z XLNTFHEKKO#" )                         IS " "
   HBTEST Transform( .T., "@Z ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )  IS " "
   HBTEST Transform( .F., "@Z ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," )  IS " "
   HBTEST Transform( .T., "@RZ #" )                                  IS " "
   HBTEST Transform( .F., "@RZ #" )                                  IS " "
   HBTEST Transform( .T., "@RZ #ylntfhekko" )                        IS "           "
   HBTEST Transform( .F., "@RZ #ylntfhekko" )                        IS "           "
   HBTEST Transform( .T., "@RZ #YLNTFHEKKO" )                        IS "           "
   HBTEST Transform( .F., "@RZ #YLNTFHEKKO" )                        IS "           "
   HBTEST Transform( .T., "@RZ YLNTFHEKKO#" )                        IS "           "
   HBTEST Transform( .F., "@RZ YLNTFHEKKO#" )                        IS "           "
   HBTEST Transform( .T., "@RZ XLNTFHEKKO#" )                        IS "           "
   HBTEST Transform( .F., "@RZ XLNTFHEKKO#" )                        IS "           "
   HBTEST Transform( .T., "@RZ ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," ) IS "                                  "
   HBTEST Transform( .F., "@RZ ABCDEFGHIJKLMNOPQRSTUVWXYZ9#!$ *.," ) IS "                                  "

   SET DATE FORMAT TO "MM/DD/YY"

   HBTEST Transform( "abcd", "@9!*" )                   IS "ABCD"
   HBTEST Transform( "abcd", "@_9!*" )                  IS "ABCD"
   HBTEST Transform( "abcd", "@_9" + Chr( 9 ) + "9!9" ) IS "aBc"
   HBTEST Transform( "abcd", "@!!!" )                   IS "ABCD"
   HBTEST Transform( "abcd", "@9" )                     IS "abcd"

   HBTEST Transform(  134.24, "99,999.99" )             IS "   134.24"
   HBTEST Transform(  134.24, "@E 99,999.99" )          IS "   134,24"
   HBTEST Transform( -134.24, "@E 99,999.99" )          IS "  -134,24"
   HBTEST Transform(  134.24, "@E99,999.99" )           IS "       134,24"
   HBTEST Transform( -134.24, "@E99,999.99" )           IS "      -134,24"

   HBTEST Transform(  7, "@C 9999" )                    IS "   7 CR"
   HBTEST Transform( -7, "@X 9999" )                    IS "   7 DB"

   HBTEST Transform( hb_SToD( "19920509" ), "@E" )      IS "09/05/92"

   HBTEST Transform( Val( "3.10" ), "@X" )              IS "3.10"
   HBTEST Transform(          0.80, ".9999" )           IS ".8000"
   HBTEST Transform(         -0.80, ".9999" )           IS ".****"
   HBTEST Transform(     12345.123, "@X99" )            IS "     12345.123"
   HBTEST Transform(    -12345.123, "@X99" )            IS "     12345.123 DB"
   HBTEST Transform(     123456.78, "@E" )              IS "    123456,78"
   HBTEST Transform(             0, "@C 9.99" )         IS "0.00"

   dt := hb_SToD( "19871231" )
   SET DATE FORMAT TO "MM:DD:YYYY"
   HBTEST Transform( dt, "@E" )       IS "31:12:1987"
   SET DATE FORMAT TO "MM:DD:YY"
   HBTEST Transform( dt, "@E" )       IS "31:12:87"
   SET DATE FORMAT TO "MM<DD>YY"
   HBTEST Transform( dt, "@E" )       IS "31<12>87"
#ifdef __HARBOUR__
   /* this are wrongly converted by CA-Cl*pper */
   SET DATE FORMAT TO "DD:MM:YYYY"
   HBTEST Transform( dt, "@E" )       IS "31:12:1987"
   SET DATE FORMAT TO "YYYY:MM:DD"
   HBTEST Transform( dt, "@E" )       IS "31:12:1987"
   SET DATE FORMAT TO "YYYY:DD:MM"
   HBTEST Transform( dt, "@E" )       IS "31:12:1987"
   SET DATE FORMAT TO "YY:MM:DD"
   HBTEST Transform( dt, "@E" )       IS "31:12:87"
   SET DATE FORMAT TO "DD:MM:YY"
   HBTEST Transform( dt, "@E" )       IS "31:12:87"
   SET DATE FORMAT TO "<YY:DD.MM>"
   HBTEST Transform( dt, "@E" )       IS "<31:12.87>"
   SET DATE FORMAT TO "|YY|MM|DD|"
   HBTEST Transform( dt, "@E" )       IS "|31|12|87|"
#endif
   SET DATE FORMAT TO "MM.DD.YYYY"
   HBTEST Transform( dt, "@E" )       IS "31.12.1987"

   HBTEST Transform(            -5, "@(Z $###,##9.99" )  IS "(      5.00)"
   HBTEST Transform(           -10, "@)Z $###,##9.99" )  IS "$    (10.00)"
   HBTEST Transform(           -20, "@Z $###,##9.99"  )  IS "$    -20.00"
   HBTEST Transform(           100, "9999."           )  IS " 100."
   HBTEST Transform(           1.1, "@B!99.99"        )  IS "1.1         "
   HBTEST Transform(        12.345, "@R 99/99"        )  IS "  /12"
   HBTEST Transform(  "1234567890", "@9"              )  IS "1234567890"
   HBTEST Transform(    1234567890, "@9"              )  IS " 1234567890"
   HBTEST Transform(          1234, "9 999"           )  IS "1 234"
   HBTEST Transform(    123.123456, "999.99.99.99"    )  IS "123.12.45.  "
   HBTEST Transform(    123.123456, "$$$.$$.$$.$$"    )  IS "123.12.45.  "
   HBTEST Transform(    123.123456, "***.**.**.**"    )  IS "123.12.45.  "
   HBTEST Transform(         99999, "9.999"           )  IS "*.***"
   HBTEST Transform(            99, "*.***"           )  IS "*.***"
   HBTEST Transform(         12345, "9999."           )  IS "****."

   HBTEST Transform(     -12345.00, "@("              )  IS "(    12345.00)"
   HBTEST Transform(     -12345.00, "@)"              )  IS "    (12345.00)"
   HBTEST Transform( -123456789.00, "@("              )  IS "(123456789.00)"
   HBTEST Transform( -123456789.00, "@)"              )  IS "(123456789.00)"
   HBTEST Transform(   -1234567890, "@("              )  IS "(         1234567890)"
   HBTEST Transform(   -1234567890, "@)"              )  IS "         (1234567890)"
   HBTEST Transform(        -12345, "@( [999999]"     )  IS "( 12345])"
   HBTEST Transform(        -12345, "@) [999999]"     )  IS "[(12345])"
   HBTEST Transform(        -12345, "@( $999999"      )  IS "( 12345)"
   HBTEST Transform(        -12345, "@) $999999"      )  IS "$(12345)"
   HBTEST Transform(        -12345, "@( #999999"      )  IS "( 12345)"
   HBTEST Transform(        -12345, "@) #999999"      )  IS " (12345)"
   HBTEST Transform(        -12345, "@( $99999"       )  IS "(12345)"
   HBTEST Transform(        -12345, "@) $99999"       )  IS "(12345)"
   HBTEST Transform(        -12345, "@( #99999"       )  IS "(12345)"
   HBTEST Transform(        -12345, "@) #99999"       )  IS "(12345)"
   HBTEST Transform(        -12345, "@( 6798^999"     )  IS "(7*8^***)"
   HBTEST Transform(        -12345, "@( 9798^9999"    )  IS "(718^2345)"

   HBTEST Transform(        134.24, "@E99,999.99"     )  IS "       134,24"
   HBTEST Transform(       -134.24, "@E99,999.99"     )  IS "      -134,24"
   HBTEST Transform(          0.80, ".9999"           )  IS ".8000"
   HBTEST Transform(         -0.80, ".9999"           )  IS ".****"
   HBTEST Transform(     12345.123, "@X99"            )  IS "     12345.123"
   HBTEST Transform(    -12345.123, "@X99"            )  IS "     12345.123 DB"
   HBTEST Transform(     123456.78, "@E"              )  IS "    123456,78"
   HBTEST Transform(             0, "@C 9.99"         )  IS "0.00"
   HBTEST Transform(           1.1, "@B!99.99"        )  IS "1.1         "
   HBTEST Transform(        -12345, "@) [999999]"     )  IS "[(12345])"
   HBTEST Transform(        -12345, "@) $999999"      )  IS "$(12345)"
   HBTEST Transform(        -12345, "@) *999999"      )  IS "*(12345)"
   HBTEST Transform(        -12345, "@) #999999"      )  IS " (12345)"
   HBTEST Transform(        -12345, "@) *9$9*999]"    )  IS "*($12345])"
   HBTEST Transform(        -12345, "@) *999*999]"    )  IS "* (12345])"
   HBTEST Transform(        -12345, "@) 0999*999]"    )  IS "0 (12345])"
   HBTEST Transform(        -12345, "@) 1999*999]"    )  IS "1 (12345])"
   HBTEST Transform(        -12345, "@) *[99*999]"    )  IS "([ 12345])"
   HBTEST Transform(        -12345, "@) *****999]"    )  IS "(**12345])"
   HBTEST Transform(        -12345, "@) *1***999]"    )  IS "(1*12345])"
   HBTEST Transform(        -12345, "@) * 999999]"    )  IS "* (12345])"
   HBTEST Transform(            -5, "@(Z $###,##9.99" )  IS  "(      5.00)"
   HBTEST Transform(           -10, "@)Z $###,##9.99" )  IS "$    (10.00)"
   HBTEST Transform(            -5, "@(Z $999,999.99" )  IS  "(      5.00)"
   HBTEST Transform(           -10, "@)Z $999,999.99" )  IS "$    (10.00)"
   HBTEST Transform(            -5, "@(Z 999,999.99"  )  IS   "(     5.00)"
   HBTEST Transform(           -10, "@)Z 999,999.99"  )  IS  "    (10.00)"
   HBTEST Transform(           -20, "@Z $###,##9.99"  )  IS  "$    -20.00"
   HBTEST Transform(           0.1, ".9"              )  IS ".1"
   HBTEST Transform(           0.0, ".9"              )  IS ".0"
   HBTEST Transform(             1, ".9"              )  IS ".*"
   HBTEST Transform(          .456, ".9"              )  IS ".5"
   HBTEST Transform(           123, "99.-"            )  IS "**.-"

   HBTEST Transform(       -123.45, "999,999.99"                )  IS "   -123.45"
   HBTEST Transform(    -123456.78, "999,999,999.99"            )  IS "   -123,456.78"
   HBTEST Transform(    -123456.78, "$$$,$$$,$$$.$$"            )  IS "$$ -123,456.78"
   HBTEST Transform(    -123456.78, "***,***,***.**"            )  IS "***-123,456.78"
   HBTEST Transform(     123456.78, "@E 888,$$$,$$$.$$"         )  IS "888.123.456,78"
   HBTEST Transform(     123456.78, "@E 888x,$$$,$$$.$$"        )  IS "888xx123.456,78"
   HBTEST Transform(     123456.78, "@E 888x,,$$$,$$$.$$"       )  IS "888xxx123.456,78"
   HBTEST Transform(     123456.78, "@E 8,88x,,$$$,$$$.$$"      )  IS "8.88xxx123.456,78"
   HBTEST Transform(     123456.78, "@E 8,88x,,$$$,,$$$.$$"     )  IS "8.88xxx123..456,78"
   HBTEST Transform(     123456.78, "@E 8,88x,,$$$,,$$$.$$77,7" )  IS "8.88xxx123..456,7877,7"
   HBTEST Transform(        123456, "@E 8,88x,,$$$,,$$$77,7"    )  IS "8.88xxx123..45677,7"
   HBTEST Transform(        123456, "@E -,999,999"              )  IS " -123.456"
   HBTEST Transform(         12345, "@E -,999,999"              )  IS " - 12.345"
   HBTEST Transform(         12345, "@E -,|999,999"             )  IS " -| 12.345"
   HBTEST Transform(         12345, "@E ^-,|999,999"            )  IS "^^-| 12.345"
   HBTEST Transform(         12345, "@E 1-,|999,999"            )  IS "11-| 12.345"
   HBTEST Transform(         12345, "@E |--,|999,999"           )  IS "|---| 12.345"

   HBTEST Transform(         12.34, "@E 99'99"                  )  IS "  '12"
   HBTEST Transform(         12.34, "99,99,11"                  )  IS "   12,11"
   HBTEST Transform(         12.34, "@E 99,99,11"               )  IS "   12,11"
   HBTEST Transform(         12.34, "@E 99,"                    )  IS "12,"
   HBTEST Transform(         12.34, "@E 9,9"                    )  IS "1.2"
   HBTEST Transform(         12.34, "@E ab,cd.ef9,9.99,.--"     )  IS "abbcd,ef***,* ,.--"
   HBTEST Transform(         12.34, "@E ab,cd,ef9,9.99,.--"     )  IS "abbcddef1.2,34,.--"
   HBTEST Transform(         12.34, "@E ,ab,cd,ef9,9.99,.--"    )  IS ",abbcddef1.2,34,.--"
   HBTEST Transform(         12.34, "@E ,,,,99,.99,.--"         )  IS ",,,,12.,34,.--"
   HBTEST Transform(         124.4, "@E ,,,,9,9.99,.--"         )  IS ",,,,***,**,.--"
   HBTEST Transform(           1.2, "@E ,,,,*,*.**,.--"         )  IS ",,,,**1,20,.--"
   HBTEST Transform(         12.34, "@E ,,,,*,*.**,.--"         )  IS ",,,,1.2,34,.--"
   HBTEST Transform(         12.34, "@E ,,,,*,*.**,.--,--"      )  IS ",,,,1.2,34,.--,--"
   HBTEST Transform(         12.34, "@E ,,,,*,*,.,**"           )  IS ",,,,1.2.,,34"
   HBTEST Transform(         12.34, ",,,,*,*,.,**"              )  IS ",,,,1,2,..34"
   HBTEST Transform(         12.34, ",,,,*,*,.,*|,*"            )  IS ",,,,1,2,..3||4"
   HBTEST Transform(         12.34, ",,,,*,*,.,*,*"             )  IS ",,,,1,2,..3,4"
   HBTEST Transform( 123.345678912, "@E 999.99.99,99.99."       )  IS "123,34,67.89,  ."
#ifdef __HARBOUR__
   HBTEST Transform(  1234567890123456789, "99999999999999999999" ) IS " 1234567890123456789"
   HBTEST Transform( -1234567890123456789, "99999999999999999999" ) IS "-1234567890123456789"
#else
   HBTEST Transform(  1234567890123456789, "99999999999999999999" ) IS " 1234567890123457000"
   HBTEST Transform( -1234567890123456789, "99999999999999999999" ) IS "-1234567890123457000"
#endif
   Set( _SET_DATEFORMAT, "YYYY/MM/DD" )
   HBTEST Transform(      12345678, "@D" ) IS "1234/56/78"
   Set( _SET_DATEFORMAT, "YYYY.MM.DD" )
   HBTEST Transform(    1234.56789, "@D" ) IS "1234.56.9 "
   Set( _SET_DATEFORMAT, "YYYY.MM:DD" )
   HBTEST Transform(    1234.56789, "@D" ) IS "1234.56:79"
   HBTEST Transform( 123.345678912, "@D" ) IS " 123.34:57"

   Set( _SET_DATEFORMAT, "MM-DD-YYYY" )
   HBTEST Transform( .T., "@RE <|,yY#lL,|>" )           IS "99-99-9999T"
   HBTEST Transform( .F., "@RE <|,yY#lL,|>" )           IS "99-99-9999F"
   HBTEST Transform( .T., "@RD <|,yY#lL,|>" )           IS "99-99-9999T"
   HBTEST Transform( .F., "@RD <|,yY#lL,|>" )           IS "99-99-9999F"
   HBTEST Transform( .F., "@DE <|,yY#lL,|>" )           IS "9"

   HBTEST Transform( "abcdefghij", "@S15! <XXXXXXXX>" ) IS "<BCDEFGHI>"
   HBTEST Transform( "abcdefghij", "@S0! <XXXXXXXX>"  ) IS "<BCDEFGHI>"
   HBTEST Transform( "abcdefghij", "@S5! <XXXXXXXX>"  ) IS "<BCDE"

   SET FIXED ON

   HBTEST Transform(              1234,        ) IS "            1234"
   HBTEST Transform(              1234, ""     ) IS "            1234"
   HBTEST Transform(              1234, "@"    ) IS "            1234"
   HBTEST Transform(              1234, "@!"   ) IS "            1234"
   HBTEST Transform(             -1234,        ) IS "           -1234"
   HBTEST Transform(             -1234, "@"    ) IS "           -1234"
#ifdef HB_CLP_STRICT
   HBTEST Transform( Round( 123  , 0 ),        ) IS "       123.00"
   HBTEST Transform( Round( 123  , 0 ), "@!"   ) IS "       123.00"
   HBTEST Transform( Round( 123.0, 0 ),        ) IS "       123.00"
   HBTEST Transform( Round( 123.0, 0 ), "@!"   ) IS "       123.00"
#endif

   HBTEST Transform(          1234.567,        ) IS "      1234.57"
   HBTEST Transform(          1234.567, ""     ) IS "      1234.57"
   HBTEST Transform(          1234.567, "@"    ) IS "      1234.57"
   HBTEST Transform(          1234.567, "@!"   ) IS "      1234.57"
   HBTEST Transform(         -1234.567,        ) IS "     -1234.57"
   HBTEST Transform(         -1234.567, "@"    ) IS "     -1234.57"
   HBTEST Transform(     Val( "-1.0" ),        ) IS "-1.00"
   HBTEST Transform(     Val( "-1.0" ), "@"    ) IS "-1.00"
   HBTEST Transform(     Val( "-123" ),        ) IS "      -123"
   HBTEST Transform(     Val( "-123" ), "@"    ) IS "      -123"
   HBTEST Transform(                 0,        ) IS "               0"
   HBTEST Transform(               0.0,        ) IS "         0.00"
   HBTEST Transform(        Val( "1" ),        ) IS "      1"
   HBTEST Transform(       Val( "12" ),        ) IS "      12"
   HBTEST Transform(      Val( "123" ),        ) IS "      123"
   HBTEST Transform(     Val( "1234" ),        ) IS "      1234"
   SET DECIMAL TO 3
   HBTEST Transform(               0.0,        ) IS "         0.000"
   HBTEST Transform(        Val( "1" ),        ) IS "        1"
   HBTEST Transform(       Val( "12" ),        ) IS "        12"
   HBTEST Transform(      Val( "123" ),        ) IS "        123"
   HBTEST Transform(     Val( "1234" ),        ) IS "        1234"
   SET DECIMAL TO 4
   HBTEST Transform(               0.0,        ) IS "         0.0000"
   HBTEST Transform(        Val( "1" ),        ) IS "          1"
   HBTEST Transform(       Val( "12" ),        ) IS "          12"
   HBTEST Transform(      Val( "123" ),        ) IS "          123"
   HBTEST Transform(     Val( "1234" ),        ) IS "          1234"

   SET FIXED OFF

   HBTEST Transform(             -1234,        ) IS "     -1234"
   HBTEST Transform(             -1234, "@B"   ) IS "-1234     "
   HBTEST Transform(             -1234, "@("   ) IS "(     1234)"
   HBTEST Transform(             -1234, "@)"   ) IS "     (1234)"
   HBTEST Transform(             -1234, "@B)"  ) IS "(1234)     "
   HBTEST Transform(             -1234, "@B("  ) IS "(1234)     "
   HBTEST Transform(          "(  12)", "@B("  ) IS "(  12)"
   HBTEST Transform(          "(  12)", "@B)"  ) IS "(  12)"
   HBTEST Transform(           "   12", "@B("  ) IS "12   "
   HBTEST Transform(           "   12", "@B)"  ) IS "12   "
#ifndef __CLIPPER__
   HBTEST Transform(              1234, "@L"   ) IS "0000001234"
   HBTEST Transform(              1234, "@0"   ) IS "0000001234"
   HBTEST Transform(              1234, "@L("  ) IS "0000001234"
   HBTEST Transform(              1234, "@0)"  ) IS "0000001234"
   HBTEST Transform(             -1234, "@L("  ) IS "(000001234)"
   HBTEST Transform(             -1234, "@0)"  ) IS "(000001234)"
   /* please test it with FoxPro and Xbase++ to check if they give
    * the same result
    */
   HBTEST Transform(             -1234, "@L"   ) IS "-000001234"
   HBTEST Transform(             -1234, "@0"   ) IS "-000001234"
#endif
   /* FlagShip extensions */
   HBTEST Transform(             -1234, "@Z"   ) IS "     -1234"
   HBTEST Transform(              1234, "@Z"   ) IS "      1234"
   HBTEST Transform(             -1234, "@F"   ) IS "     -1234"
   HBTEST Transform(              1234, "@F"   ) IS "      1234"
   HBTEST Transform(             -1234, "@T"   ) IS "     -1234"
   HBTEST Transform(              1234, "@T"   ) IS "      1234"

   HBTEST Transform(   123456789.12, "@,39 999,999,999.99" ) IS "123,456,789.12"
   HBTEST Transform(   123456789.12, "@,39 999,999,999.99" ) IS "123,456,789.12"
   HBTEST Transform(        123.456, "@R 9 9 9.9"          ) IS "1 2 3.5"

   Set( _SET_FIXED, cOldFixed )
   Set( _SET_DECIMALS, cOldDecim )
   Set( _SET_DATEFORMAT, cOldDate )

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
