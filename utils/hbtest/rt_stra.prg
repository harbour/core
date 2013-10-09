/*
 * Harbour Project source code:
 * Regression tests for the runtime library (strings)
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

PROCEDURE Main_STRA()

   /* Str() */

   HBTEST Str( NIL )                     IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Str( "A", 10, 2 )              IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:C:A;N:10;N:2 F:S"
   HBTEST Str( 100, 10, "A" )            IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;N:10;C:A F:S"
   HBTEST Str( 100, 10, NIL )            IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;N:10;U:NIL F:S"
   HBTEST Str( 100, NIL, NIL )           IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;U:NIL;U:NIL F:S"
   IF TEST_DBFAvail()
   HBTEST Str( w_TEST->TYPE_N_I )        IS "        100"
   HBTEST Str( w_TEST->TYPE_N_IE )       IS "          0"
   HBTEST Str( w_TEST->TYPE_N_D )        IS "    101.127"
   HBTEST Str( w_TEST->TYPE_N_DE )       IS "      0.000"
   ENDIF
   HBTEST Str( 5000000000.0 )            IS "5000000000.0"
   HBTEST Str( 50000000 )                IS "  50000000"
   HBTEST Str( 500000000 )               IS " 500000000"
   HBTEST Str( 5000000000 )              IS " 5000000000"
   HBTEST Str( 50000000000 )             IS " 50000000000"
   HBTEST Str( -5000000000.0 )           IS "         -5000000000.0"
   HBTEST Str( -5000000000 )             IS "         -5000000000"
   HBTEST Str( 2.0000000000000001 )      IS "         2.0000000000000000"
   HBTEST Str( 2.0000000000000009 )      IS "         2.0000000000000010"
   HBTEST Str( 2.000000000000001 )       IS "         2.000000000000001"
   HBTEST Str( 2.000000000000009 )       IS "         2.000000000000009"
   HBTEST Str( 2.00000000000001 )        IS "         2.00000000000001"
   HBTEST Str( 2.00000000000009 )        IS "         2.00000000000009"
   HBTEST Str( 2.000000000001 )          IS "         2.000000000001"
   HBTEST Str( 2.00000000001 )           IS "         2.00000000001"
   HBTEST Str( 10 )                      IS "        10"
   HBTEST Str( 10.0 )                    IS "        10.0"
   HBTEST Str( 10.00 )                   IS "        10.00"
   HBTEST Str( 10.50 )                   IS "        10.50"
   HBTEST Str( 100000 )                  IS "    100000"
   HBTEST Str( -10 )                     IS "       -10"
   HBTEST Str( -10.0 )                   IS "       -10.0"
   HBTEST Str( -10.00 )                  IS "       -10.00"
   HBTEST Str( -10.50 )                  IS "       -10.50"
   HBTEST Str( -100000 )                 IS "   -100000"
   HBTEST Str( 10, 5 )                   IS "   10"
   HBTEST Str( 10.0, 5 )                 IS "   10"
   HBTEST Str( 10.00, 5 )                IS "   10"
   HBTEST Str( 10.50, 5 )                IS "   11"
   HBTEST Str( 100000, 5 )               IS "*****"
   HBTEST Str( 100000, 8 )               IS "  100000"
   HBTEST Str( -10, 5 )                  IS "  -10"
   HBTEST Str( -10.0, 5 )                IS "  -10"
   HBTEST Str( -10.00, 5 )               IS "  -10"
   HBTEST Str( -10.50, 5 )               IS "  -11"
   HBTEST Str( -100000, 5 )              IS "*****"
   HBTEST Str( -100000, 6 )              IS "******"
   HBTEST Str( -100000, 8 )              IS " -100000"
#ifndef __XPP__ /* Internal structures corrupted */
   HBTEST Str( 10, -5 )                  IS "        10"
   HBTEST Str( 10.0, -5 )                IS "        10"
   HBTEST Str( 10.00, -5 )               IS "        10"
   HBTEST Str( 10.50, -5 )               IS "        11"
   HBTEST Str( 100000, -5 )              IS "    100000"
   HBTEST Str( 100000, -8 )              IS "    100000"
   HBTEST Str( -10, -5 )                 IS "       -10"
   HBTEST Str( -10.0, -5 )               IS "       -10"
   HBTEST Str( -10.00, -5 )              IS "       -10"
   HBTEST Str( -10.50, -5 )              IS "       -11"
   HBTEST Str( -100000, -5 )             IS "   -100000"
   HBTEST Str( -100000, -6 )             IS "   -100000"
   HBTEST Str( -100000, -8 )             IS "   -100000"
#endif
   HBTEST Str( 10, 5, 0 )                IS "   10"
   HBTEST Str( 10.0, 5, 0 )              IS "   10"
   HBTEST Str( 10.00, 5, 0 )             IS "   10"
   HBTEST Str( 10.50, 5, 0 )             IS "   11"
   HBTEST Str( 100000, 5, 0 )            IS "*****"
   HBTEST Str( -10, 5, 0 )               IS "  -10"
   HBTEST Str( -10.0, 5, 0 )             IS "  -10"
   HBTEST Str( -10.00, 5, 0 )            IS "  -10"
   HBTEST Str( -10.50, 5, 0 )            IS "  -11"
   HBTEST Str( -100000, 5, 0 )           IS "*****"
   HBTEST Str( -100000, 6, 0 )           IS "******"
   HBTEST Str( -100000, 8, 0 )           IS " -100000"
   HBTEST Str( 10, 5, 1 )                IS " 10.0"
   HBTEST Str( 10.0, 5, 1 )              IS " 10.0"
   HBTEST Str( 10.00, 5, 1 )             IS " 10.0"
   HBTEST Str( 10.50, 5, 1 )             IS " 10.5"
   HBTEST Str( 100000, 5, 1 )            IS "*****"
   HBTEST Str( -10, 5, 1 )               IS "-10.0"
   HBTEST Str( -10.0, 5, 1 )             IS "-10.0"
   HBTEST Str( -10.00, 5, 1 )            IS "-10.0"
   HBTEST Str( -10.50, 5, 1 )            IS "-10.5"
   HBTEST Str( -100000, 5, 1 )           IS "*****"
   HBTEST Str( -100000, 6, 1 )           IS "******"
   HBTEST Str( -100000, 8, 1 )           IS "********"
#ifndef __XPP__ /* Internal structures corrupted */
   HBTEST Str( 10, 5, -1 )               IS "   10"
   HBTEST Str( 10.0, 5, -1 )             IS "   10"
   HBTEST Str( 10.00, 5, -1 )            IS "   10"
   HBTEST Str( 10.50, 5, -1 )            IS "   11"
   HBTEST Str( 100000, 5, -1 )           IS "*****"
   HBTEST Str( -10, 5, -1 )              IS "  -10"
   HBTEST Str( -10.0, 5, -1 )            IS "  -10"
   HBTEST Str( -10.00, 5, -1 )           IS "  -10"
   HBTEST Str( -10.50, 5, -1 )           IS "  -11"
   HBTEST Str( -100000, 5, -1 )          IS "*****"
   HBTEST Str( -100000, 6, -1 )          IS "******"
   HBTEST Str( -100000, 8, -1 )          IS " -100000"
#endif

   /* StrZero() */

#ifdef HB_CLP_STRICT
   HBTEST StrZero( NIL )                 IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST StrZero( "A", 10, 2 )          IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:C:A;N:10;N:2 F:S"
   HBTEST StrZero( 100, 10, "A" )        IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;N:10;C:A F:S"
   HBTEST StrZero( 100, 10, NIL )        IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;N:10;U:NIL F:S"
   HBTEST StrZero( 100, NIL, NIL )       IS "E 1 BASE 1099 Argument error (STR) OS:0 #:0 A:3:N:100;U:NIL;U:NIL F:S"
#else
   HBTEST StrZero( NIL )                 IS "E 1 BASE 6003 Argument error (STRZERO) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST StrZero( "A", 10, 2 )          IS "E 1 BASE 6003 Argument error (STRZERO) OS:0 #:0 A:3:C:A;N:10;N:2 F:S"
   HBTEST StrZero( 100, 10, "A" )        IS "E 1 BASE 6003 Argument error (STRZERO) OS:0 #:0 A:3:N:100;N:10;C:A F:S"
   HBTEST StrZero( 100, 10, NIL )        IS "E 1 BASE 6003 Argument error (STRZERO) OS:0 #:0 A:3:N:100;N:10;U:NIL F:S"
   HBTEST StrZero( 100, NIL, NIL )       IS "E 1 BASE 6003 Argument error (STRZERO) OS:0 #:0 A:3:N:100;U:NIL;U:NIL F:S"
#endif
   HBTEST StrZero( 10 )                  IS "0000000010"
   HBTEST StrZero( 10.0 )                IS "0000000010.0"
   HBTEST StrZero( 10.00 )               IS "0000000010.00"
   HBTEST StrZero( 10.50 )               IS "0000000010.50"
   HBTEST StrZero( 100000 )              IS "0000100000"
   HBTEST StrZero( -10 )                 IS "-000000010"
   HBTEST StrZero( -10.0 )               IS "-000000010.0"
   HBTEST StrZero( -10.00 )              IS "-000000010.00"
   HBTEST StrZero( -10.50 )              IS "-000000010.50"
   HBTEST StrZero( -100000 )             IS "-000100000"
   HBTEST StrZero( 10, 5 )               IS "00010"
   HBTEST StrZero( 10.0, 5 )             IS "00010"
   HBTEST StrZero( 10.00, 5 )            IS "00010"
   HBTEST StrZero( 10.50, 5 )            IS "00011"
   HBTEST StrZero( 100000, 5 )           IS "*****"
   HBTEST StrZero( 100000, 8 )           IS "00100000"
   HBTEST StrZero( -10, 5 )              IS "-0010"
   HBTEST StrZero( -10.0, 5 )            IS "-0010"
   HBTEST StrZero( -10.00, 5 )           IS "-0010"
   HBTEST StrZero( -10.50, 5 )           IS "-0011"
   HBTEST StrZero( -100000, 5 )          IS "*****"
   HBTEST StrZero( -100000, 6 )          IS "******"
   HBTEST StrZero( -100000, 8 )          IS "-0100000"
#ifndef __XPP__ /* Internal structures corrupted */
   HBTEST StrZero( 10, -5 )              IS "0000000010"
   HBTEST StrZero( 10.0, -5 )            IS "0000000010"
   HBTEST StrZero( 10.00, -5 )           IS "0000000010"
   HBTEST StrZero( 10.50, -5 )           IS "0000000011"
   HBTEST StrZero( 100000, -5 )          IS "0000100000"
   HBTEST StrZero( 100000, -8 )          IS "0000100000"
   HBTEST StrZero( -10, -5 )             IS "-000000010"
   HBTEST StrZero( -10.0, -5 )           IS "-000000010"
   HBTEST StrZero( -10.00, -5 )          IS "-000000010"
   HBTEST StrZero( -10.50, -5 )          IS "-000000011"
   HBTEST StrZero( -100000, -5 )         IS "-000100000"
   HBTEST StrZero( -100000, -6 )         IS "-000100000"
   HBTEST StrZero( -100000, -8 )         IS "-000100000"
#endif
   HBTEST StrZero( 10, 5, 0 )            IS "00010"
   HBTEST StrZero( 10.0, 5, 0 )          IS "00010"
   HBTEST StrZero( 10.50, 5, 0 )         IS "00011"
   HBTEST StrZero( 100000, 5, 0 )        IS "*****"
   HBTEST StrZero( -10, 5, 0 )           IS "-0010"
   HBTEST StrZero( -10.0, 5, 0 )         IS "-0010"
   HBTEST StrZero( -10.00, 5, 0 )        IS "-0010"
   HBTEST StrZero( -10.50, 5, 0 )        IS "-0011"
   HBTEST StrZero( -100000, 5, 0 )       IS "*****"
   HBTEST StrZero( -100000, 6, 0 )       IS "******"
   HBTEST StrZero( -100000, 8, 0 )       IS "-0100000"
   HBTEST StrZero( 10, 5, 1 )            IS "010.0"
   HBTEST StrZero( 10.0, 5, 1 )          IS "010.0"
   HBTEST StrZero( 10.50, 5, 1 )         IS "010.5"
   HBTEST StrZero( 100000, 5, 1 )        IS "*****"
   HBTEST StrZero( -10, 5, 1 )           IS "-10.0"
   HBTEST StrZero( -10.0, 5, 1 )         IS "-10.0"
   HBTEST StrZero( -10.00, 5, 1 )        IS "-10.0"
   HBTEST StrZero( -10.50, 5, 1 )        IS "-10.5"
   HBTEST StrZero( -100000, 5, 1 )       IS "*****"
   HBTEST StrZero( -100000, 6, 1 )       IS "******"
   HBTEST StrZero( -100000, 8, 1 )       IS "********"
#ifndef __XPP__ /* Internal structures corrupted */
   HBTEST StrZero( 10, 5, -1 )           IS "00010"
   HBTEST StrZero( 10.0, 5, -1 )         IS "00010"
   HBTEST StrZero( 10.50, 5, -1 )        IS "00011"
   HBTEST StrZero( 100000, 5, -1 )       IS "*****"
   HBTEST StrZero( -10, 5, -1 )          IS "-0010"
   HBTEST StrZero( -10.0, 5, -1 )        IS "-0010"
   HBTEST StrZero( -10.00, 5, -1 )       IS "-0010"
   HBTEST StrZero( -10.50, 5, -1 )       IS "-0011"
   HBTEST StrZero( -100000, 5, -1 )      IS "*****"
   HBTEST StrZero( -100000, 6, -1 )      IS "******"
   HBTEST StrZero( -100000, 8, -1 )      IS "-0100000"
#endif

   RETURN

PROCEDURE Comp_Str()

   LOCAL old_exact := Set( _SET_EXACT, .F. )

   HBTEST "ABC" == ""      IS .F.
   HBTEST "ABC" = ""       IS .T.
   HBTEST "ABC" != ""      IS .F.
   HBTEST "ABC" < ""       IS .F.
   HBTEST "ABC" <= ""      IS .T.
   HBTEST "ABC" > ""       IS .F.
   HBTEST "ABC" >= ""      IS .T.
   HBTEST "" == "ABC"      IS .F.
   HBTEST "" = "ABC"       IS .F.
   HBTEST "" != "ABC"      IS .T.
   HBTEST "" < "ABC"       IS .T.
   HBTEST "" <= "ABC"      IS .T.
   HBTEST "" > "ABC"       IS .F.
   HBTEST "" >= "ABC"      IS .F.
   HBTEST "ABC" == " "     IS .F.
   HBTEST "ABC" = " "      IS .F.
   HBTEST "ABC" != " "     IS .T.
   HBTEST "ABC" < " "      IS .F.
   HBTEST "ABC" <= " "     IS .F.
   HBTEST "ABC" > " "      IS .T.
   HBTEST "ABC" >= " "     IS .T.
   HBTEST " " == "ABC"     IS .F.
   HBTEST " " = "ABC"      IS .F.
   HBTEST " " != "ABC"     IS .T.
   HBTEST " " < "ABC"      IS .T.
   HBTEST " " <= "ABC"     IS .T.
   HBTEST " " > "ABC"      IS .F.
   HBTEST " " >= "ABC"     IS .F.
   HBTEST "ABC" == "ABC"   IS .T.
   HBTEST "ABC" = "ABC"    IS .T.
   HBTEST "ABC" != "ABC"   IS .F.
   HBTEST "ABC" < "ABC"    IS .F.
   HBTEST "ABC" <= "ABC"   IS .T.
   HBTEST "ABC" > "ABC"    IS .F.
   HBTEST "ABC" >= "ABC"   IS .T.
   HBTEST "ABC" == "ABCD"  IS .F.
   HBTEST "ABC" = "ABCD"   IS .F.
   HBTEST "ABC" != "ABCD"  IS .T.
   HBTEST "ABC" < "ABCD"   IS .T.
   HBTEST "ABC" <= "ABCD"  IS .T.
   HBTEST "ABC" > "ABCD"   IS .F.
   HBTEST "ABC" >= "ABCD"  IS .F.
   HBTEST "ABCD" == "ABC"  IS .F.
   HBTEST "ABCD" = "ABC"   IS .T.
   HBTEST "ABCD" != "ABC"  IS .F.
   HBTEST "ABCD" < "ABC"   IS .F.
   HBTEST "ABCD" <= "ABC"  IS .T.
   HBTEST "ABCD" > "ABC"   IS .F.
   HBTEST "ABCD" >= "ABC"  IS .T.
   HBTEST "ABC" == "ABC "  IS .F.
   HBTEST "ABC" = "ABC "   IS .F.
   HBTEST "ABC" != "ABC "  IS .T.
   HBTEST "ABC" < "ABC "   IS .T.
   HBTEST "ABC" <= "ABC "  IS .T.
   HBTEST "ABC" > "ABC "   IS .F.
   HBTEST "ABC" >= "ABC "  IS .F.
   HBTEST "ABC " == "ABC"  IS .F.
   HBTEST "ABC " = "ABC"   IS .T.
   HBTEST "ABC " != "ABC"  IS .F.
   HBTEST "ABC " < "ABC"   IS .F.
   HBTEST "ABC " <= "ABC"  IS .T.
   HBTEST "ABC " > "ABC"   IS .F.
   HBTEST "ABC " >= "ABC"  IS .T.
   HBTEST "ABC" == "DEF"   IS .F.
   HBTEST "ABC" = "DEF"    IS .F.
   HBTEST "ABC" != "DEF"   IS .T.
   HBTEST "ABC" < "DEF"    IS .T.
   HBTEST "ABC" <= "DEF"   IS .T.
   HBTEST "ABC" > "DEF"    IS .F.
   HBTEST "ABC" >= "DEF"   IS .F.
   HBTEST "DEF" == "ABC"   IS .F.
   HBTEST "DEF" = "ABC"    IS .F.
   HBTEST "DEF" != "ABC"   IS .T.
   HBTEST "DEF" < "ABC"    IS .F.
   HBTEST "DEF" <= "ABC"   IS .F.
   HBTEST "DEF" > "ABC"    IS .T.
   HBTEST "DEF" >= "ABC"   IS .T.
   HBTEST "ABC" == "DEFG"  IS .F.
   HBTEST "ABC" = "DEFG"   IS .F.
   HBTEST "ABC" != "DEFG"  IS .T.
   HBTEST "ABC" < "DEFG"   IS .T.
   HBTEST "ABC" <= "DEFG"  IS .T.
   HBTEST "ABC" > "DEFG"   IS .F.
   HBTEST "ABC" >= "DEFG"  IS .F.
   HBTEST "DEFG" == "ABC"  IS .F.
   HBTEST "DEFG" = "ABC"   IS .F.
   HBTEST "DEFG" != "ABC"  IS .T.
   HBTEST "DEFG" < "ABC"   IS .F.
   HBTEST "DEFG" <= "ABC"  IS .F.
   HBTEST "DEFG" > "ABC"   IS .T.
   HBTEST "DEFG" >= "ABC"  IS .T.
   HBTEST "ABCD" == "DEF"  IS .F.
   HBTEST "ABCD" = "DEF"   IS .F.
   HBTEST "ABCD" != "DEF"  IS .T.
   HBTEST "ABCD" < "DEF"   IS .T.
   HBTEST "ABCD" <= "DEF"  IS .T.
   HBTEST "ABCD" > "DEF"   IS .F.
   HBTEST "ABCD" >= "DEF"  IS .F.
   HBTEST "DEF" == "ABCD"  IS .F.
   HBTEST "DEF" = "ABCD"   IS .F.
   HBTEST "DEF" != "ABCD"  IS .T.
   HBTEST "DEF" < "ABCD"   IS .F.
   HBTEST "DEF" <= "ABCD"  IS .F.
   HBTEST "DEF" > "ABCD"   IS .T.
   HBTEST "DEF" >= "ABCD"  IS .T.

   Set( _SET_EXACT, old_exact )
   RETURN

PROCEDURE Exact_Str()

   LOCAL old_exact := Set( _SET_EXACT, .T. )

   HBTEST "ABC" == ""      IS .F.
   HBTEST "ABC" = ""       IS .F.
   HBTEST "ABC" != ""      IS .T.
   HBTEST "ABC" < ""       IS .F.
   HBTEST "ABC" <= ""      IS .F.
   HBTEST "ABC" > ""       IS .T.
   HBTEST "ABC" >= ""      IS .T.
   HBTEST "" == "ABC"      IS .F.
   HBTEST "" = "ABC"       IS .F.
   HBTEST "" != "ABC"      IS .T.
   HBTEST "" < "ABC"       IS .T.
   HBTEST "" <= "ABC"      IS .T.
   HBTEST "" > "ABC"       IS .F.
   HBTEST "" >= "ABC"      IS .F.
   HBTEST "ABC" == " "     IS .F.
   HBTEST "ABC" = " "      IS .F.
   HBTEST "ABC" != " "     IS .T.
   HBTEST "ABC" < " "      IS .F.
   HBTEST "ABC" <= " "     IS .F.
   HBTEST "ABC" > " "      IS .T.
   HBTEST "ABC" >= " "     IS .T.
   HBTEST " " == "ABC"     IS .F.
   HBTEST " " = "ABC"      IS .F.
   HBTEST " " != "ABC"     IS .T.
   HBTEST " " < "ABC"      IS .T.
   HBTEST " " <= "ABC"     IS .T.
   HBTEST " " > "ABC"      IS .F.
   HBTEST " " >= "ABC"     IS .F.
   HBTEST "ABC" == "ABC"   IS .T.
   HBTEST "ABC" = "ABC"    IS .T.
   HBTEST "ABC" != "ABC"   IS .F.
   HBTEST "ABC" < "ABC"    IS .F.
   HBTEST "ABC" <= "ABC"   IS .T.
   HBTEST "ABC" > "ABC"    IS .F.
   HBTEST "ABC" >= "ABC"   IS .T.
   HBTEST "ABC" == "ABCD"  IS .F.
   HBTEST "ABC" = "ABCD"   IS .F.
   HBTEST "ABC" != "ABCD"  IS .T.
   HBTEST "ABC" < "ABCD"   IS .T.
   HBTEST "ABC" <= "ABCD"  IS .T.
   HBTEST "ABC" > "ABCD"   IS .F.
   HBTEST "ABC" >= "ABCD"  IS .F.
   HBTEST "ABCD" == "ABC"  IS .F.
   HBTEST "ABCD" = "ABC"   IS .F.
   HBTEST "ABCD" != "ABC"  IS .T.
   HBTEST "ABCD" < "ABC"   IS .F.
   HBTEST "ABCD" <= "ABC"  IS .F.
   HBTEST "ABCD" > "ABC"   IS .T.
   HBTEST "ABCD" >= "ABC"  IS .T.
   HBTEST "ABC" == "ABC "  IS .F.
   HBTEST "ABC" = "ABC "   IS .T.
   HBTEST "ABC" != "ABC "  IS .F.
   HBTEST "ABC" < "ABC "   IS .F.
   HBTEST "ABC" <= "ABC "  IS .T.
   HBTEST "ABC" > "ABC "   IS .F.
   HBTEST "ABC" >= "ABC "  IS .T.
   HBTEST "ABC " == "ABC"  IS .F.
   HBTEST "ABC " = "ABC"   IS .T.
   HBTEST "ABC " != "ABC"  IS .F.
   HBTEST "ABC " < "ABC"   IS .F.
   HBTEST "ABC " <= "ABC"  IS .T.
   HBTEST "ABC " > "ABC"   IS .F.
   HBTEST "ABC " >= "ABC"  IS .T.
   HBTEST "ABC" == "DEF"   IS .F.
   HBTEST "ABC" = "DEF"    IS .F.
   HBTEST "ABC" != "DEF"   IS .T.
   HBTEST "ABC" < "DEF"    IS .T.
   HBTEST "ABC" <= "DEF"   IS .T.
   HBTEST "ABC" > "DEF"    IS .F.
   HBTEST "ABC" >= "DEF"   IS .F.
   HBTEST "DEF" == "ABC"   IS .F.
   HBTEST "DEF" = "ABC"    IS .F.
   HBTEST "DEF" != "ABC"   IS .T.
   HBTEST "DEF" < "ABC"    IS .F.
   HBTEST "DEF" <= "ABC"   IS .F.
   HBTEST "DEF" > "ABC"    IS .T.
   HBTEST "DEF" >= "ABC"   IS .T.
   HBTEST "ABC" == "DEFG"  IS .F.
   HBTEST "ABC" = "DEFG"   IS .F.
   HBTEST "ABC" != "DEFG"  IS .T.
   HBTEST "ABC" < "DEFG"   IS .T.
   HBTEST "ABC" <= "DEFG"  IS .T.
   HBTEST "ABC" > "DEFG"   IS .F.
   HBTEST "ABC" >= "DEFG"  IS .F.
   HBTEST "DEFG" == "ABC"  IS .F.
   HBTEST "DEFG" = "ABC"   IS .F.
   HBTEST "DEFG" != "ABC"  IS .T.
   HBTEST "DEFG" < "ABC"   IS .F.
   HBTEST "DEFG" <= "ABC"  IS .F.
   HBTEST "DEFG" > "ABC"   IS .T.
   HBTEST "DEFG" >= "ABC"  IS .T.
   HBTEST "ABCD" == "DEF"  IS .F.
   HBTEST "ABCD" = "DEF"   IS .F.
   HBTEST "ABCD" != "DEF"  IS .T.
   HBTEST "ABCD" < "DEF"   IS .T.
   HBTEST "ABCD" <= "DEF"  IS .T.
   HBTEST "ABCD" > "DEF"   IS .F.
   HBTEST "ABCD" >= "DEF"  IS .F.
   HBTEST "DEF" == "ABCD"  IS .F.
   HBTEST "DEF" = "ABCD"   IS .F.
   HBTEST "DEF" != "ABCD"  IS .T.
   HBTEST "DEF" < "ABCD"   IS .F.
   HBTEST "DEF" <= "ABCD"  IS .F.
   HBTEST "DEF" > "ABCD"   IS .T.
   HBTEST "DEF" >= "ABCD"  IS .T.

   Set( _SET_EXACT, old_exact )

   RETURN

PROCEDURE New_STRINGS()

#ifdef __HARBOUR__

   HBTEST hb_ValToStr( 4 )                     IS "         4"
   HBTEST hb_ValToStr( 4.0 / 2 )               IS "         2.00"
   HBTEST hb_ValToStr( "String" )              IS "String"
   HBTEST hb_ValToStr( hb_SToD( "20010101" ) ) IS "2001-01-01"
   HBTEST hb_ValToStr( NIL )                   IS "NIL"
   HBTEST hb_ValToStr( .F. )                   IS ".F."
   HBTEST hb_ValToStr( .T. )                   IS ".T."

   HBTEST hb_StrShrink()                       IS ""
   HBTEST hb_StrShrink( NIL )                  IS ""
   HBTEST hb_StrShrink( "" )                   IS ""
   HBTEST hb_StrShrink( "", -1 )               IS ""
   HBTEST hb_StrShrink( "", 0 )                IS ""
   HBTEST hb_StrShrink( "", 1 )                IS ""
   HBTEST hb_StrShrink( "", 10 )               IS ""
   HBTEST hb_StrShrink( "a" )                  IS ""
   HBTEST hb_StrShrink( "a", -1 )              IS "a"
   HBTEST hb_StrShrink( "a", 0 )               IS "a"
   HBTEST hb_StrShrink( "a", 1 )               IS ""
   HBTEST hb_StrShrink( "a", 10 )              IS ""
   HBTEST hb_StrShrink( "ab" )                 IS "a"
   HBTEST hb_StrShrink( "ab", -1 )             IS "ab"
   HBTEST hb_StrShrink( "ab", 0 )              IS "ab"
   HBTEST hb_StrShrink( "ab", 1 )              IS "a"
   HBTEST hb_StrShrink( "ab", 10 )             IS ""
   HBTEST hb_StrShrink( "ab" )                 IS "a"
   HBTEST hb_StrShrink( "ab", -2 )             IS "ab"
   HBTEST hb_StrShrink( "ab", 2 )              IS ""
   HBTEST hb_StrShrink( "hello" )              IS "hell"
   HBTEST hb_StrShrink( "hello", -1 )          IS "hello"
   HBTEST hb_StrShrink( "hello", 0 )           IS "hello"
   HBTEST hb_StrShrink( "hello", 1 )           IS "hell"
   HBTEST hb_StrShrink( "hello", 2 )           IS "hel"
   HBTEST hb_StrShrink( "hello", 3 )           IS "he"
   HBTEST hb_StrShrink( "hello", 4 )           IS "h"
   HBTEST hb_StrShrink( "hello", 5 )           IS ""
   HBTEST hb_StrShrink( "hello", 6 )           IS ""
   HBTEST hb_StrShrink( "hello", 7 )           IS ""

#endif

   RETURN

PROCEDURE Long_STRINGS()

   HBTEST Right( Space( 64 * 1024 - 5 ) + "12345 7890", 10                      ) IS "12345 7890"
   HBTEST Len( Space( 81910 ) + "1234567890"                                    ) IS 81920
   HBTEST ( "1234567890" + Space( 810910 ) ) - ( "1234567890" + Space( 810910 ) ) IS "12345678901234567890" + Space( 810910 * 2 )

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
