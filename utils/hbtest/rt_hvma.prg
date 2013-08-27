/*
 * Harbour Project source code:
 * Regression tests for the runtime library (HVM)
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

PROCEDURE Main_HVMA()

   LOCAL nA, nB, nC

   // NOTE: These are compiler tests.
   //       The expressions have to be written with no separators!
   HBTEST mnIntP==10.OR.mnIntP==0         IS .T.
   HBTEST mnIntP==10.AND.mnLongP==0       IS .F.

#ifdef __HARBOUR__
   /* disable Harbour extended optimizations to test correct RTE message */
   #pragma -ko-
#endif
   HBTEST NIL + 1                         IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:U:NIL;N:1 F:S"
   HBTEST NIL - 1                         IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:U:NIL;N:1 F:S"

#ifdef __HARBOUR__
   #pragma -ko+
#endif

   HBTEST scString + NIL                  IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"
   HBTEST scString - NIL                  IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"

#ifdef __HARBOUR__
   /* disable Harbour extended optimizations to test correct RTE message */
   #pragma -ko-
#endif
   HBTEST 1 + NIL                         IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:N:1;U:NIL F:S"
#ifdef __HARBOUR__
   #pragma -ko+
#endif
   HBTEST 1 - NIL                         IS "E 1 BASE 1082 Argument error (-) OS:0 #:0 A:2:N:1;U:NIL F:S"

   HBTEST "A" - "B"                       IS "AB"
   HBTEST "A  " - "B"                     IS "AB  "
   HBTEST "A  " - "B "                    IS "AB   "
   HBTEST "A  " - " B"                    IS "A B  "
   HBTEST "   " - "B "                    IS "B    "

   HBTEST 1 / 0                           IS "E 5 BASE 1340 Zero divisor (/) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST 1 / NIL                         IS "E 1 BASE 1084 Argument error (/) OS:0 #:0 A:2:N:1;U:NIL F:S"
   HBTEST 1 * NIL                         IS "E 1 BASE 1083 Argument error (*) OS:0 #:0 A:2:N:1;U:NIL F:S"
   HBTEST 1 ** NIL                        IS "E 1 BASE 1088 Argument error (^) OS:0 #:0 A:2:N:1;U:NIL F:S"
   HBTEST 1 ^ NIL                         IS "E 1 BASE 1088 Argument error (^) OS:0 #:0 A:2:N:1;U:NIL F:S"
   HBTEST 1 % 0                           IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST 1 % NIL                         IS "E 1 BASE 1085 Argument error (%) OS:0 #:0 A:2:N:1;U:NIL F:S"

   HBTEST Str( 1 / 0   )                  IS "E 5 BASE 1340 Zero divisor (/) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST Str( 2 / 4   )                  IS "         0.50"
   HBTEST Str( 4 / 2   )                  IS "         2"
   HBTEST Str( 4 / 2.0 )                  IS "         2.00"
   HBTEST Str( 1 * 0   )                  IS "         0"
   HBTEST Str( 2 * 4   )                  IS "         8"
   HBTEST Str( 4 * 2.0 )                  IS "         8.0"
   HBTEST Str( 2 * 0.5 )                  IS "         1.0"
   HBTEST Str( 1 + 0   )                  IS "         1"
   HBTEST Str( 2 + 4   )                  IS "         6"
   HBTEST Str( 4 + 2.0 )                  IS "         6.0"
   HBTEST Str( 2 + 0.5 )                  IS "         2.5"
   HBTEST Str( 1 - 0   )                  IS "         1"
   HBTEST Str( 2 - 4   )                  IS "        -2"
   HBTEST Str( 4 - 2.0 )                  IS "         2.0"
   HBTEST Str( 2 - 0.5 )                  IS "         1.5"
   HBTEST Str( 1 % 0   )                  IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:1;N:0 F:S"
   HBTEST Str( 2 % 4   )                  IS "         2"
   HBTEST Str( 4 % 2   )                  IS "         0"
   HBTEST Str( 4 % 2.0 )                  IS "         0.00"
   HBTEST Str( 2 % 4.0 )                  IS "         2.00"

   HBTEST Str(  3 %  3 )                   IS "         0"
   HBTEST Str(  3 %  2 )                   IS "         1"
   HBTEST Str(  3 %  1 )                   IS "         0"
   HBTEST Str(  3 %  0 )                   IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:3;N:0 F:S"
   HBTEST Str(  3 % -1 )                   IS "         0"
   HBTEST Str(  3 % -2 )                   IS "         1"
   HBTEST Str(  3 % -3 )                   IS "         0"
   HBTEST Str( -3 %  3 )                   IS "         0"
   HBTEST Str( -3 %  2 )                   IS "        -1"
   HBTEST Str( -3 %  1 )                   IS "         0"
   HBTEST Str( -3 %  0 )                   IS "E 5 BASE 1341 Zero divisor (%) OS:0 #:0 A:2:N:-3;N:0 F:S"
   HBTEST Str( -3 % -1 )                   IS "         0"
   HBTEST Str( -3 % -2 )                   IS "        -1"
   HBTEST Str( -3 % -3 )                   IS "         0"
   HBTEST Str(  3 %  3 )                   IS "         0"
   HBTEST Str(  2 %  3 )                   IS "         2"
   HBTEST Str(  1 %  3 )                   IS "         1"
   HBTEST Str(  0 %  3 )                   IS "         0"
   HBTEST Str( -1 %  3 )                   IS "        -1"
   HBTEST Str( -2 %  3 )                   IS "        -2"
   HBTEST Str( -3 %  3 )                   IS "         0"
   HBTEST Str(  3 % -3 )                   IS "         0"
   HBTEST Str(  2 % -3 )                   IS "         2"
   HBTEST Str(  1 % -3 )                   IS "         1"
   HBTEST Str(  0 % -3 )                   IS "         0"
   HBTEST Str( -1 % -3 )                   IS "        -1"
   HBTEST Str( -2 % -3 )                   IS "        -2"
   HBTEST Str( -3 % -3 )                   IS "         0"

   /* The order of these tests is relevant, don't change it */

   nA := 1
   nB := 2
   nC := 3

   HBTEST nA                              IS 1
   HBTEST nB                              IS 2
   HBTEST nC                              IS 3

   HBTEST nA + nB                         IS 3
   HBTEST nB - nA                         IS 1
   HBTEST nB * nC                         IS 6
   HBTEST nB * nC / 2                     IS 3
   HBTEST nA += nB                        IS 3
   HBTEST nA                              IS 3
   HBTEST nA -= nB                        IS 1
   HBTEST nA                              IS 1
   HBTEST nA < nB                         IS .T.
   HBTEST nA > nB                         IS .F.
   HBTEST nA + nB <= nC                   IS .T.
   HBTEST nA + nB >= nC                   IS .T.
   HBTEST nA *= nB                        IS 2
   HBTEST nA /= nB                        IS 1
   HBTEST nA                              IS 1
   HBTEST nB ** 3                         IS 8
   HBTEST nB ^ 3                          IS 8
   HBTEST 8 % 3                           IS 2
   HBTEST nA++                            IS 1
   HBTEST nA                              IS 2
   HBTEST ++nA                            IS 3
   HBTEST nA                              IS 3
   HBTEST nA--                            IS 3
   HBTEST nA                              IS 2
   HBTEST --nA                            IS 1
   HBTEST nA                              IS 1

   /* These will generate warnings and errors with Harbour */
#ifndef __HARBOUR__
   HBTEST { 1, 2 }[ ( 2, 1 ) ]++          IS 1
   HBTEST ++{ 1, 2 }[ ( 2, 1 ) ]          IS 2
   HBTEST { 1, 2 }[ 1 ]++                 IS 1
   HBTEST ++{ 1, 2 }[ 1 ]                 IS 2
#ifndef __XPP__
   HBTEST ({ 1, 2 }[ 1 ])++               IS 1
   HBTEST ++({ 1, 2 }[ 1 ])               IS 2
#endif
#endif

   /* Operator precedence */

   HBTEST 1 + 2 * 3 / 4 - 2 ** 2 ^ 3      IS -61.50
   HBTEST 1 + 2 * 3 / 4 - 2 ** 2 ^ 3 == 2 IS .F.

   /* */

   HBTEST -Month( sdDate )                IS -3
   HBTEST Str( -( Month( sdDate ) ) )     IS "        -3"
   HBTEST Str( -( Val( "10" ) ) )         IS "       -10"
   HBTEST Str( -( Val( "100000" ) ) )     IS "   -100000"
   HBTEST Str( -( Val( "20.876" ) ) )     IS "       -20.876"
   HBTEST -( 0 )                          IS 0
   HBTEST -( 10 )                         IS -10
   HBTEST -( 10.505 )                     IS -10.505
   HBTEST -( 100000 )                     IS -100000
   HBTEST -( -10 )                        IS 10
   HBTEST -( "1" )                        IS "E 1 BASE 1080 Argument error (-) OS:0 #:0 A:1:C:1 F:S"

   HBTEST "AA" $ 1                        IS "E 1 BASE 1109 Argument error ($) OS:0 #:0 A:2:C:AA;N:1 F:S"
   HBTEST scString $ 1                    IS "E 1 BASE 1109 Argument error ($) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST 1 $ "AA"                        IS "E 1 BASE 1109 Argument error ($) OS:0 #:0 A:2:N:1;C:AA F:S"

   HBTEST !   scStringE $ "XE"            IS .T.
   HBTEST ! ( scStringE $ "XE" )          IS .T.
   HBTEST     scStringE $ "XE"            IS .F.
   HBTEST !   "X" $ "XE"                  IS .F.
   HBTEST ! ( "X" $ "XE" )                IS .F.
   HBTEST     "X" $ "XE"                  IS .T.
   HBTEST     "X" $ Chr( 0 ) + "X"        IS .T.
   HBTEST ( "X" ) $ Chr( 0 ) + "X"        IS .T.
   HBTEST scString $ Chr( 0 ) + scString  IS .T.

   HBTEST scStringE $ scStringE           IS .F.
   HBTEST scStringE $ "bcde"              IS .F.
   /* disable Harbour extensions in compiler to replicate Clipper bugs */
#ifdef __HARBOUR__
   #pragma -kh-
#endif
   HBTEST "" $ ""                         IS .T.  /* Bug in CA-Cl*ppers compiler optimizer. It should return .F. */
   HBTEST "" $ "bcde"                     IS .T.  /* Bug in CA-Cl*ppers compiler optimizer. It should return .F. */
#ifdef __HARBOUR__
   /* enable Harbour extensions and test correct results results */
   #pragma -kh+
   HBTEST "" $ ""                         IS .F.  /* Bug in CA-Cl*ppers compiler optimizer. It should return .F. */
   HBTEST "" $ "bcde"                     IS .F.  /* Bug in CA-Cl*ppers compiler optimizer. It should return .F. */
#endif
   HBTEST "d" $ "bcde"                    IS .T.
   HBTEST "D" $ "BCDE"                    IS .T.
   HBTEST "a" $ "bcde"                    IS .F.
   HBTEST "d" $ "BCDE"                    IS .F.
   HBTEST "D" $ "bcde"                    IS .F.
   HBTEST "de" $ "bcde"                   IS .T.
   HBTEST "bd" $ "bcde"                   IS .F.
   HBTEST "BD" $ "bcde"                   IS .F.

#ifndef __XPP__

   IF TEST_OPT_Z()

   /* With the shortcut optimization *ON* */

   HBTEST "1" .AND. "2"                   IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:C:1 "
   HBTEST 1 .AND. 2                       IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:N:1 "
   HBTEST NIL .AND. NIL                   IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:U:NIL "
   HBTEST scString .AND. scString         IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:C:HELLO "
   HBTEST .T. .AND. 1                     IS 1
   HBTEST .T. .AND. 1.567                 IS 1.567
   HBTEST .T. .AND. scString              IS "HELLO"
   HBTEST .T. .AND. hb_SToD( "" )         IS hb_SToD( "        " )
   HBTEST .T. .AND. NIL                   IS NIL
   HBTEST .T. .AND. {}                    IS "{.[0].}"
   HBTEST .T. .AND. {|| NIL }             IS "{||...}"
   HBTEST .F. .AND. 1                     IS .F.
   HBTEST .F. .AND. 1.567                 IS .F.
   HBTEST .F. .AND. scString              IS .F.
   HBTEST .F. .AND. hb_SToD( "" )         IS .F.
   HBTEST .F. .AND. NIL                   IS .F.
   HBTEST .F. .AND. {}                    IS .F.
   HBTEST .F. .AND. {|| NIL }             IS .F.
   HBTEST "1" .AND. .F.                   IS .F.
   HBTEST 1 .AND. .F.                     IS .F.
   HBTEST 1.567 .AND. .F.                 IS .F.
   HBTEST scString .AND. .F.              IS .F.

   HBTEST "1" .OR. "2"                    IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:C:1 "
   HBTEST 1 .OR. 2                        IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:N:1 "
   HBTEST .F. .OR. 2                      IS 2
   HBTEST .F. .OR. 1.678                  IS 1.678
   HBTEST .F. .OR. scString               IS "HELLO"
   HBTEST .T. .OR. 2                      IS .T.
   HBTEST .T. .OR. 1.678                  IS .T.
   HBTEST .T. .OR. scString               IS .T.
   HBTEST "1" .OR. .F.                    IS "1"
   HBTEST 1 .OR. .F.                      IS 1
   HBTEST 1.0 .OR. .F.                    IS 1.0
   HBTEST scString .OR. .F.               IS "HELLO"

   ELSE

   /* With the shortcut optimization *OFF* (/z switch) */

   HBTEST "1" .AND. "2"                   IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:C:1;C:2 F:S"
   HBTEST 1 .AND. 2                       IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:N:1;N:2 F:S"
   HBTEST NIL .AND. NIL                   IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST scString .AND. scString         IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:C:HELLO;C:HELLO F:S"
   HBTEST .T. .AND. 1                     IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;N:1 F:S"
   HBTEST .T. .AND. 1.567                 IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;N:1.567 F:S"
   HBTEST .T. .AND. scString              IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;C:HELLO F:S"
   HBTEST .T. .AND. hb_SToD( "" )         IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;D:0d00000000 F:S"
   HBTEST .T. .AND. NIL                   IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;U:NIL F:S"
   HBTEST .T. .AND. {}                    IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;A:{.[0].} F:S"
   HBTEST .T. .AND. {|| NIL }             IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.T.;B:{||...} F:S"
   HBTEST .F. .AND. 1                     IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;N:1 F:S"
   HBTEST .F. .AND. 1.567                 IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;N:1.567 F:S"
   HBTEST .F. .AND. scString              IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;C:HELLO F:S"
   HBTEST .F. .AND. hb_SToD( "" )         IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;D:0d00000000 F:S"
   HBTEST .F. .AND. NIL                   IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;U:NIL F:S"
   HBTEST .F. .AND. {}                    IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;A:{.[0].} F:S"
   HBTEST .F. .AND. {|| NIL }             IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:L:.F.;B:{||...} F:S"
   HBTEST "1" .AND. .F.                   IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:C:1;L:.F. F:S"
   HBTEST 1 .AND. .F.                     IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:N:1;L:.F. F:S"
   HBTEST 1.567 .AND. .F.                 IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:N:1.567;L:.F. F:S"
   HBTEST scString .AND. .F.              IS "E 1 BASE 1078 Argument error (.AND.) OS:0 #:0 A:2:C:HELLO;L:.F. F:S"

   HBTEST "1" .OR. "2"                    IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:C:1;C:2 F:S"
   HBTEST 1 .OR. 2                        IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:N:1;N:2 F:S"
   HBTEST .F. .OR. 2                      IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.F.;N:2 F:S"
   HBTEST .F. .OR. 1.678                  IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.F.;N:1.678 F:S"
   HBTEST .F. .OR. scString               IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.F.;C:HELLO F:S"
   HBTEST .T. .OR. 2                      IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.T.;N:2 F:S"
   HBTEST .T. .OR. 1.678                  IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.T.;N:1.678 F:S"
   HBTEST .T. .OR. scString               IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:L:.T.;C:HELLO F:S"
   HBTEST "1" .OR. .F.                    IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:C:1;L:.F. F:S"
   HBTEST 1 .OR. .F.                      IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:N:1;L:.F. F:S"
   HBTEST 1.0 .OR. .F.                    IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:N:1.0;L:.F. F:S"
   HBTEST scString .OR. .F.               IS "E 1 BASE 1079 Argument error (.OR.) OS:0 #:0 A:2:C:HELLO;L:.F. F:S"

   ENDIF

#endif

   HBTEST .NOT. .T.                       IS .F.
   HBTEST .NOT. .F.                       IS .T.
   HBTEST .NOT. 1                         IS "E 1 BASE 1077 Argument error (.NOT.) OS:0 #:0 A:1:N:1 F:S"

#ifndef __HARBOUR__ // this error is reported at compile time
#ifndef __XPP__ // this error is reported at compile time
   HBTEST iif( "A", ":T:", ":F:" )        IS "E 1 BASE 1066 Argument error (conditional) OS:0 #:0 A:1:C:A "
#endif
#endif
   HBTEST iif( .T., ":T:", ":F:" )        IS ":T:"
   HBTEST iif( .F., ":T:", ":F:" )        IS ":F:"

   HBTEST scString++                      IS "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:C:HELLO F:S"
   HBTEST scString--                      IS "E 1 BASE 1087 Argument error (--) OS:0 #:0 A:1:C:HELLO F:S"

   HBTEST mxNotHere                       IS "E 14 BASE 1003 Variable does not exist (MXNOTHERE) OS:0 #:1 F:R"  /* Bug in CA-Cl*pper, it does not initialize 'TRIES' in error object giving strange results */
#ifdef __HARBOUR__
   HBTEST __mvGet( "MXUNDECL" )           IS "E 14 BASE 1003 Variable does not exist (MXUNDECL) OS:0 #:1 F:R"
#endif
   mxNotHere := "MXUNDECL"
   HBTEST &mxNotHere.                     IS "E 14 BASE 1003 Variable does not exist (MXUNDECL) OS:0 #:1 F:R"

#ifndef __HARBOUR__
   // this error is reported at compile time
   HBTEST saArray[ 0 ]                    IS "E 2 BASE 1132 Bound error (array access) OS:0 #:0 "
   HBTEST saArray[ 0 ] := 1               IS "E 2 BASE 1133 Bound error (array assign) OS:0 #:0 "
#endif
   HBTEST saArray[ 1000 ]                 IS "E 2 BASE 1132 Bound error (array access) OS:0 #:0 "
   HBTEST saArray[ 1000 ] := 1            IS "E 2 BASE 1133 Bound error (array assign) OS:0 #:0 "
#ifndef __HARBOUR__
   // this error is reported at compile time
   HBTEST saArray[ -1 ]                   IS "E 2 BASE 1132 Bound error (array access) OS:0 #:0 "
   HBTEST saArray[ -1 ] := 1              IS "E 2 BASE 1133 Bound error (array assign) OS:0 #:0 "
   HBTEST saArray[ "1" ]                  IS "E 1 BASE 1068 Argument error (array access) OS:0 #:0 A:2:A:{.[1].};C:1 F:S"
   HBTEST saArray[ "1" ] := 1             IS "E 1 BASE 1069 Argument error (array assign) OS:0 #:0 A:3:N:1;A:{.[1].};C:1 "
#endif

   /* Alias */

   HBTEST ("NOTHERE")->NOFIELD            IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST (mcString)->NOFIELD             IS "E 15 BASE 1002 Alias does not exist (HELLO) OS:0 #:1 F:R"
   HBTEST ({})->NOFIELD                   IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:A:{.[0].};C:NOFIELD F:S"
   HBTEST ({|| NIL })->NOFIELD            IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:B:{||...};C:NOFIELD F:S"
   HBTEST (.T.)->NOFIELD                  IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:L:.T.;C:NOFIELD F:S"
   HBTEST (.F.)->NOFIELD                  IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:L:.F.;C:NOFIELD F:S"
   HBTEST (NIL)->NOFIELD                  IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:U:NIL;C:NOFIELD F:S"
   HBTEST (2)->NOFIELD                    IS "E 14 BASE 1003 Variable does not exist (NOFIELD) OS:0 #:1 F:R"
   HBTEST (2.5)->NOFIELD                  IS "E 14 BASE 1003 Variable does not exist (NOFIELD) OS:0 #:1 F:R"
   HBTEST (hb_SToD( "" ))->NOFIELD        IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:D:0d00000000;C:NOFIELD F:S"
   HBTEST (ErrorNew())->NOFIELD           IS "E 1 BASE 1065 Argument error (&) OS:0 #:0 A:2:O:ERROR Object;C:NOFIELD F:S"

#ifndef __XPP__
#ifdef __HARBOUR__
   /* disable Harbour extended optimizations to test correct RTE message */
   #pragma -ko-
#endif
   HBTEST ("NOTHERE")->( Eof() )          IS .T.
#ifdef __HARBOUR__
   #pragma -ko+
#endif
   HBTEST (mcString)->( Eof() )           IS .T.
   HBTEST ({})->( Eof() )                 IS .T.
   HBTEST ({|| NIL })->( Eof() )          IS .T.
   HBTEST (.T.)->( Eof() )                IS .T.
   HBTEST (.F.)->( Eof() )                IS .T.
   HBTEST (NIL)->( Eof() )                IS .T.
   HBTEST (2)->( Eof() )                  IS .T.
   HBTEST (2.5)->( Eof() )                IS .T.
   HBTEST (hb_SToD( "" ))->( Eof() )      IS .T.
   HBTEST (ErrorNew())->( Eof() )         IS .T.
#endif

   HBTEST NOTHERE->NOFIELD                IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( "NOFIELD" )          IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( mcString )           IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( {} )                 IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( {|| NIL } )          IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( .T. )                IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( .F. )                IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( NIL )                IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( 1 )                  IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( 1.5 )                IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( hb_SToD( "" ) )      IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"
   HBTEST NOTHERE->( ErrorNew() )         IS "E 15 BASE 1002 Alias does not exist (NOTHERE) OS:0 #:1 F:R"

   HBTEST 200->NOFIELD                    IS "E 14 BASE 1003 Variable does not exist (NOFIELD) OS:0 #:1 F:R"
   HBTEST 200->( "NOFIELD" )              IS "NOFIELD"
   HBTEST 200->( mcString )               IS "HELLO"
   HBTEST 200->( {} )                     IS "{.[0].}"
   HBTEST 200->( {|| NIL } )              IS "{||...}"
   HBTEST 200->( .T. )                    IS .T.
   HBTEST 200->( .F. )                    IS .F.
   HBTEST 200->( NIL )                    IS NIL
   HBTEST 200->( 1 )                      IS 1
   HBTEST 200->( 1.5 )                    IS 1.5
   HBTEST 200->( hb_SToD( "" ) )          IS hb_SToD( "        " )
   HBTEST 200->( ErrorNew() )             IS "ERROR Object"

   HBTEST soObject:hello                  IS "E 13 BASE 1004 No exported method (HELLO) OS:0 #:0 A:1:O:ERROR Object F:S"
   HBTEST soObject:hello := 1             IS "E 16 BASE 1005 No exported variable (HELLO) OS:0 #:0 A:2:O:ERROR Object;N:1 F:S"

   /* Len() */

   HBTEST Len( NIL )                       IS "E 1 BASE 1111 Argument error (LEN) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Len( 123 )                       IS "E 1 BASE 1111 Argument error (LEN) OS:0 #:0 A:1:N:123 F:S"
   HBTEST Len( "" )                        IS 0
   HBTEST Len( "123" )                     IS 3
   HBTEST Len( "123" + Chr( 0 ) + "456 " ) IS 8
   IF TEST_DBFAvail()
   HBTEST Len( w_TEST->TYPE_C )            IS 15
   HBTEST Len( w_TEST->TYPE_C_E )          IS 15
   HBTEST Len( w_TEST->TYPE_M )            IS 11
   HBTEST Len( w_TEST->TYPE_M_E )          IS 0
   ENDIF
   HBTEST Len( saArray )                   IS 1
#ifdef __HARBOUR__
   HBTEST Len( ErrorNew() )                IS 12
   HBTEST Len( Space( 1000000 ) )          IS 1000000
#else
   HBTEST Len( ErrorNew() )                IS 7
   HBTEST Len( Space( 40000 ) )            IS 40000
#endif

   /* Empty() */

#ifdef __HARBOUR__
   HBTEST Empty( @scString                        ) IS .F.  /* Bug in CA-Cl*pper, it will return .T. */
   HBTEST Empty( @scStringE                       ) IS .T.
   HBTEST Empty( @snIntP                          ) IS .F.  /* Bug in CA-Cl*pper, it will return .T. */
   HBTEST Empty( @snIntZ                          ) IS .T.
#endif
   HBTEST Empty( "Hallo"                          ) IS .F.
   HBTEST Empty( ""                               ) IS .T.
   HBTEST Empty( "  "                             ) IS .T.
   HBTEST Empty( " " + Chr( 0 )                   ) IS .F.
   HBTEST Empty( " " + Chr( 13 ) + Chr( 9 )       ) IS .T.
   HBTEST Empty( "  A"                            ) IS .F.
   HBTEST Empty( " x "                            ) IS .F.
   HBTEST Empty( " x" + Chr( 0 )                  ) IS .F.
   HBTEST Empty( " " + Chr( 13 ) + "x" + Chr( 9 ) ) IS .F.
   IF TEST_DBFAvail()
   HBTEST Empty( w_TEST->TYPE_C                   ) IS .F.
   HBTEST Empty( w_TEST->TYPE_C_E                 ) IS .T.
   HBTEST Empty( w_TEST->TYPE_D                   ) IS .F.
   HBTEST Empty( w_TEST->TYPE_D_E                 ) IS .T.
   HBTEST Empty( w_TEST->TYPE_M                   ) IS .F.
   HBTEST Empty( w_TEST->TYPE_M_E                 ) IS .T.
   HBTEST Empty( w_TEST->TYPE_N_I                 ) IS .F.
   HBTEST Empty( w_TEST->TYPE_N_IE                ) IS .T.
   HBTEST Empty( w_TEST->TYPE_N_D                 ) IS .F.
   HBTEST Empty( w_TEST->TYPE_N_DE                ) IS .T.
   HBTEST Empty( w_TEST->TYPE_L                   ) IS .F.
   HBTEST Empty( w_TEST->TYPE_L_E                 ) IS .T.
   ENDIF
   HBTEST Empty( 0                                ) IS .T.
   HBTEST Empty( -0                               ) IS .T.
   HBTEST Empty( 0.0                              ) IS .T.
   HBTEST Empty( 70000-70000                      ) IS .T.
   HBTEST Empty( 1.5 * 1.5 - 2.25                 ) IS .T.
   HBTEST Empty( 10                               ) IS .F.
   HBTEST Empty( 10.0                             ) IS .F.
   HBTEST Empty( 70000+70000                      ) IS .F.
   HBTEST Empty( 1.5 * 1.5 * 2.25                 ) IS .F.
   HBTEST Empty( hb_SToD( "18241010" )            ) IS .F.
   HBTEST Empty( hb_SToD( "18250231" )            ) IS .T.
   HBTEST Empty( hb_SToD( "99999999" )            ) IS .T.
   HBTEST Empty( hb_SToD( "        " )            ) IS .T.
   HBTEST Empty( hb_SToD( "" )                    ) IS .T.
   HBTEST Empty( .T.                              ) IS .F.
   HBTEST Empty( .F.                              ) IS .T.
   HBTEST Empty( NIL                              ) IS .T.
   HBTEST Empty( { 1 }                            ) IS .F.
   HBTEST Empty( {}                               ) IS .T.
   HBTEST Empty( { 0 }                            ) IS .F.
   HBTEST Empty( {| x | x + x }                   ) IS .F.
   HBTEST Empty( ErrorNew()                       ) IS .F.

   /* Some number width handling tests */

   HBTEST RTSTR( 50000000 )                       IS " 10   50000000"
   HBTEST RTSTR( 99999999 )                       IS " 10   99999999"
   HBTEST RTSTR( 100000000 )                      IS " 10  100000000"
   HBTEST RTSTR( 500000000 )                      IS " 10  500000000"
   HBTEST RTSTR( 999999999 )                      IS " 10  999999999"
   HBTEST RTSTR( 999999999.99 )                   IS " 13  999999999.99"
   HBTEST RTSTR( 1000000000 )                     IS " 11  1000000000"
   HBTEST RTSTR( 1000000000.0 )                   IS " 12 1000000000.0"
   HBTEST RTSTR( 1000000000.00 )                  IS " 13 1000000000.00"
   HBTEST RTSTR( 1000000000.99 )                  IS " 13 1000000000.99"
   HBTEST RTSTR( 4000000000 )                     IS " 11  4000000000"
   HBTEST RTSTR( 00005 )                          IS " 10          5"
   HBTEST RTSTR( 00005.5 )                        IS " 12          5.5"
   HBTEST RTSTR( 5000000000 )                     IS " 11  5000000000"
   HBTEST RTSTR( 50000000000 )                    IS " 12  50000000000"
   HBTEST RTSTR( 500000000000 )                   IS " 13  500000000000"
   HBTEST RTSTR( 500000000000.0 )                 IS " 14 500000000000.0"
   HBTEST RTSTR( 5000000000000 )                  IS " 14  5000000000000"
   HBTEST RTSTR( 50000000000000 )                 IS " 15  50000000000000"
   HBTEST RTSTR( 500000000000000 )                IS " 16  500000000000000"
   HBTEST RTSTR( 00000000000005 )                 IS " 10          5"
   HBTEST RTSTR( 00000500000000000000 )           IS " 21       500000000000000"
   HBTEST RTSTR( 0500000000000000 )               IS " 17   500000000000000"
   HBTEST RTSTR( 0500000000000000.5 )             IS " 18  500000000000000.5"
   HBTEST RTSTR( 5000000000000000 )               IS " 17  5000000000000000"
   HBTEST RTSTR( 50000000000000000 )              IS " 18  50000000000000000"
   HBTEST RTSTR( 500000000000000000 )             IS " 19  500000000000000000"
   HBTEST RTSTR( 5000000000000000000 )            IS " 20  5000000000000000000"
   HBTEST RTSTR( 50000000000000000000 )           IS " 21  50000000000000000000"
   HBTEST RTSTR( 500000000000000000000 )          IS " 22  500000000000000000000"
   HBTEST RTSTR( 5000000000000000000000 )         IS " 23  5000000000000000000000"
   HBTEST RTSTR( 50000000000000000000000 )        IS " 24  50000000000000000000000"
   HBTEST RTSTR( 500000000000000000000000 )       IS " 25  500000000000000000000000"
   HBTEST RTSTR( 5000000000000000000000000 )      IS " 26  5000000000000000000000000"
   HBTEST RTSTR( 5000000000000000000000000.0 )    IS " 27 5000000000000000000000000.0"
   HBTEST RTSTR( -50000000 )                      IS " 10  -50000000"
   HBTEST RTSTR( -50000000.0 )                    IS " 12  -50000000.0"
   HBTEST RTSTR( -500000000 )                     IS " 10 -500000000"
   HBTEST RTSTR( -999999999 )                     IS " 10 -999999999"
   HBTEST RTSTR( -1000000000 )                    IS " 20          -1000000000"
   HBTEST RTSTR( -1000000000.0 )                  IS " 22          -1000000000.0"
   HBTEST RTSTR( -4000000000 )                    IS " 20          -4000000000"
   HBTEST RTSTR( -5000000000 )                    IS " 20          -5000000000"
   HBTEST RTSTR( -50000000000 )                   IS " 20         -50000000000"
   HBTEST RTSTR( -500000000000 )                  IS " 20        -500000000000"
   HBTEST RTSTR( -500000000000.0 )                IS " 22        -500000000000.0"
   HBTEST RTSTR( -5000000000000 )                 IS " 20       -5000000000000"
   HBTEST RTSTR( -50000000000000 )                IS " 20      -50000000000000"
   HBTEST RTSTR( -500000000000000 )               IS " 20     -500000000000000"
   HBTEST RTSTR( -5000000000000000 )              IS " 20    -5000000000000000"
   HBTEST RTSTR( -50000000000000000 )             IS " 20   -50000000000000000"
   HBTEST RTSTR( -500000000000000000 )            IS " 20  -500000000000000000"
   HBTEST RTSTR( -5000000000000000000 )           IS " 20 -5000000000000000000"
   HBTEST RTSTR( -50000000000000000000 )          IS " 20 ********************"
   HBTEST RTSTR( -500000000000000000000 )         IS " 20 ********************"
   HBTEST RTSTR( -5000000000000000000000 )        IS " 20 ********************"
   HBTEST RTSTR( -50000000000000000000000 )       IS " 20 ********************"
   HBTEST RTSTR( -500000000000000000000000 )      IS " 20 ********************"
   HBTEST RTSTR( -5000000000000000000000000 )     IS " 20 ********************"

   HBTEST ( nA := 50000000                      , RTSTR( -nA ) ) IS " 10  -50000000"
   HBTEST ( nA := 50000000.0                    , RTSTR( -nA ) ) IS " 12  -50000000.0"
   HBTEST ( nA := 99999999                      , RTSTR( -nA ) ) IS " 10  -99999999"
   HBTEST ( nA := 99999999.9                    , RTSTR( -nA ) ) IS " 12  -99999999.9"
   HBTEST ( nA := 100000000                     , RTSTR( -nA ) ) IS " 10 -100000000"
   HBTEST ( nA := 100000000.0                   , RTSTR( -nA ) ) IS " 12 -100000000.0"
   HBTEST ( nA := 500000000                     , RTSTR( -nA ) ) IS " 10 -500000000"
   HBTEST ( nA := 999999999                     , RTSTR( -nA ) ) IS " 10 -999999999"
   HBTEST ( nA := 999999999.99                  , RTSTR( -nA ) ) IS " 23           -999999999.99"
   HBTEST ( nA := 1000000000                    , RTSTR( -nA ) ) IS " 20          -1000000000"
   HBTEST ( nA := 1000000000.0                  , RTSTR( -nA ) ) IS " 22          -1000000000.0"
   HBTEST ( nA := 1000000000.00                 , RTSTR( -nA ) ) IS " 23          -1000000000.00"
   HBTEST ( nA := 1000000000.99                 , RTSTR( -nA ) ) IS " 23          -1000000000.99"
   HBTEST ( nA := 4000000000                    , RTSTR( -nA ) ) IS " 20          -4000000000"
   HBTEST ( nA := 5000000000                    , RTSTR( -nA ) ) IS " 20          -5000000000"
   HBTEST ( nA := 50000000000                   , RTSTR( -nA ) ) IS " 20         -50000000000"
   HBTEST ( nA := 500000000000                  , RTSTR( -nA ) ) IS " 20        -500000000000"
   HBTEST ( nA := 5000000000000                 , RTSTR( -nA ) ) IS " 20       -5000000000000"
   HBTEST ( nA := 50000000000000                , RTSTR( -nA ) ) IS " 20      -50000000000000"
   HBTEST ( nA := 500000000000000               , RTSTR( -nA ) ) IS " 20     -500000000000000"
   HBTEST ( nA := 5000000000000000              , RTSTR( -nA ) ) IS " 20    -5000000000000000"
   HBTEST ( nA := 50000000000000000             , RTSTR( -nA ) ) IS " 20   -50000000000000000"
   HBTEST ( nA := 500000000000000000            , RTSTR( -nA ) ) IS " 20  -500000000000000000"
   HBTEST ( nA := 5000000000000000000           , RTSTR( -nA ) ) IS " 20 -5000000000000000000"
   HBTEST ( nA := 50000000000000000000          , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 500000000000000000000         , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 5000000000000000000000        , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 50000000000000000000000       , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 500000000000000000000000      , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 5000000000000000000000000     , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := 5000000000000000000000000.0   , RTSTR( -nA ) ) IS " 22 **********************"
   HBTEST ( nA := -50000000                     , RTSTR( -nA ) ) IS " 10   50000000"
   HBTEST ( nA := -50000000.0                   , RTSTR( -nA ) ) IS " 12   50000000.0"
   HBTEST ( nA := -500000000                    , RTSTR( -nA ) ) IS " 10  500000000"
   HBTEST ( nA := -999999999                    , RTSTR( -nA ) ) IS " 10  999999999"
   HBTEST ( nA := -1000000000                   , RTSTR( -nA ) ) IS " 10 1000000000"
   HBTEST ( nA := -4000000000                   , RTSTR( -nA ) ) IS " 10 4000000000"
   HBTEST ( nA := -5000000000                   , RTSTR( -nA ) ) IS " 10 5000000000"
   HBTEST ( nA := -50000000000                  , RTSTR( -nA ) ) IS " 20          50000000000"
   HBTEST ( nA := -500000000000                 , RTSTR( -nA ) ) IS " 20         500000000000"
   HBTEST ( nA := -5000000000000                , RTSTR( -nA ) ) IS " 20        5000000000000"
   HBTEST ( nA := -50000000000000               , RTSTR( -nA ) ) IS " 20       50000000000000"
   HBTEST ( nA := -500000000000000              , RTSTR( -nA ) ) IS " 20      500000000000000"
   HBTEST ( nA := -5000000000000000             , RTSTR( -nA ) ) IS " 20     5000000000000000"
   HBTEST ( nA := -50000000000000000            , RTSTR( -nA ) ) IS " 20    50000000000000000"
   HBTEST ( nA := -500000000000000000           , RTSTR( -nA ) ) IS " 20   500000000000000000"
   HBTEST ( nA := -5000000000000000000          , RTSTR( -nA ) ) IS " 20  5000000000000000000"
   HBTEST ( nA := -50000000000000000000         , RTSTR( -nA ) ) IS " 20 50000000000000000000"
   HBTEST ( nA := -500000000000000000000        , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := -5000000000000000000000       , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := -50000000000000000000000      , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := -500000000000000000000000     , RTSTR( -nA ) ) IS " 20 ********************"
   HBTEST ( nA := -5000000000000000000000000    , RTSTR( -nA ) ) IS " 20 ********************"

   RETURN

FUNCTION RTSTR( nValue )
   RETURN Str( Len( Str( nValue ) ), 3 ) + " " + Str( nValue )

/* Don't change the position of this #include. */
#include "rt_init.ch"
