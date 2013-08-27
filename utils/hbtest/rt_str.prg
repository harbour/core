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

#ifdef __HARBOUR__
   #include "hbver.ch"
#endif

PROCEDURE Main_STR()

   LOCAL l64

#ifdef __HARBOUR__
   l64 := hb_Version( HB_VERSION_BITWIDTH ) >= 64
#else
   l64 := .F.
#endif

   /* Val() */

   HBTEST Val( NIL )                      IS "E 1 BASE 1098 Argument error (VAL) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Val( 10 )                       IS "E 1 BASE 1098 Argument error (VAL) OS:0 #:0 A:1:N:10 F:S"

#ifndef RT_NO_C

#ifdef __PLATFORM__WINDOWS
   IF .F.   /* [U]LONG is 32 bit integer in Win64 */
#else
   IF l64
#endif
      HBTEST Str( R_PASSENL(  1860637360 ) )   IS "1860637360"
      HBTEST Str( R_PASSENL(         100 ) )   IS "       100"
      HBTEST Str( R_PASSENL(   999999999 ) )   IS " 999999999"
      HBTEST Str( R_PASSENL(  1000000000 ) )   IS "1000000000"
      HBTEST Str( R_PASSENL(  2147483647 ) )   IS "2147483647"
      HBTEST Str( R_PASSENL(  2147483648 ) )   IS "2147483648"
      HBTEST Str( R_PASSENL(          -1 ) )   IS "        -1"
      HBTEST Str( R_PASSENL(  -999999999 ) )   IS "-999999999"
      HBTEST Str( R_PASSENL( -1000000000 ) )   IS "         -1000000000"
      HBTEST Str( R_PASSENL(   -99999999 ) )   IS " -99999999"
      HBTEST Str( R_PASSENL(  -100000000 ) )   IS "-100000000"

      HBTEST Str( R_PASSENLC() )               IS "1000000000"
   ELSE
      HBTEST Str( R_PASSENL(  1860637360 ) )   IS "          1860637360"
      HBTEST Str( R_PASSENL(         100 ) )   IS "       100"
      HBTEST Str( R_PASSENL(   999999999 ) )   IS " 999999999"
      HBTEST Str( R_PASSENL(  1000000000 ) )   IS "          1000000000"
      HBTEST Str( R_PASSENL(  2147483647 ) )   IS "          2147483647"
      HBTEST Str( R_PASSENL(  2147483648 ) )   IS "         -2147483648"
      HBTEST Str( R_PASSENL(          -1 ) )   IS "        -1"
      HBTEST Str( R_PASSENL(  -999999999 ) )   IS "-999999999"
      HBTEST Str( R_PASSENL( -1000000000 ) )   IS "         -1000000000"
      HBTEST Str( R_PASSENL(   -99999999 ) )   IS " -99999999"
      HBTEST Str( R_PASSENL(  -100000000 ) )   IS "-100000000"

      HBTEST Str( R_PASSENLC() )               IS "          1000000000"
   ENDIF
#endif

   HBTEST Str( Val( "" ) )                       IS "         0"

   HBTEST Str( Val( "" ) )                       IS "         0"
   HBTEST Str( Val( " " ) )                      IS "0"
   HBTEST Str( Val( "-" ) )                      IS "0"
   HBTEST Str( Val( "+" ) )                      IS "0"
   HBTEST Str( Val( "-+" ) )                     IS " 0"
   HBTEST Str( Val( "+-" ) )                     IS " 0"
   HBTEST Str( Val( "." ) )                      IS "0"
   HBTEST Str( Val( ".." ) )                     IS "0.0"
   HBTEST Str( Val( "-." ) )                     IS " 0"       /* Bug in CA-Cl*pper 5.3b, it will return: "  0" */
   HBTEST Str( Val( "-.." ) )                    IS "0.0"      /* Bug in CA-Cl*pper 5.3b, it will return: " 0.0" */
   HBTEST Str( Val( "1." ) )                     IS " 1"
   HBTEST Str( Val( "1.." ) )                    IS "1.0"
   HBTEST Str( Val( "1..." ) )                   IS "1.00"
   HBTEST Str( Val( "-1." ) )                    IS " -1"
   HBTEST Str( Val( " -1." ) )                   IS "  -1"
   HBTEST Str( Val( " --1." ) )                  IS "    0"
   HBTEST Str( Val( "-1.." ) )                   IS "-1.0"
   HBTEST Str( Val( "-1..." ) )                  IS "-1.00"
   HBTEST Str( Val( ".1" ) )                     IS "0.1"
   HBTEST Str( Val( "-.1" ) )                    IS "-0.1"
   HBTEST Str( Val( "-.0" ) )                    IS "0.0"      /* Bug in CA-Cl*pper 5.3b, it will return: " 0.0" */
   HBTEST Str( Val( " -.1" ) )                   IS "-0.1"
   HBTEST Str( Val( " --.1" ) )                  IS "  0.0"
   HBTEST Str( Val( "+.1" ) )                    IS "0.1"
   HBTEST Str( Val( " .1" ) )                    IS "0.1"
   HBTEST Str( Val( "- .1" ) )                   IS " 0.0"
   HBTEST Str( Val( "+.1" ) )                    IS "0.1"
   HBTEST Str( Val( "-  12" ) )                  IS "    0"
   HBTEST Str( Val( " - 12" ) )                  IS "    0"
   HBTEST Str( Val( "  -12" ) )                  IS "  -12"
   HBTEST Str( Val( " --12" ) )                  IS "    0"
   HBTEST Str( Val( "   12-" ) )                 IS "    12"
   HBTEST Str( Val( "   12 -" ) )                IS "     12"
   HBTEST Str( Val( " 13.1.9" ) )                IS " 13.100"
   HBTEST Str( Val( " 12" ) )                    IS " 12"
   HBTEST Str( Val( " 12" + Chr( 0 ) + "0" ) )   IS "   12"
   HBTEST Str( Val( " 12.1" + Chr( 0 ) + "2" ) ) IS " 12.100"
   HBTEST Str( Val( " 12" + Chr( 0 ) + ".2" ) )  IS "  12.0"
   HBTEST Str( Val( " 12.0" ) )                  IS " 12.0"
   HBTEST Str( Val( " 12. 0" ) )                 IS " 12.00"
   HBTEST Str( Val( " 12 .0" ) )                 IS "  12.0"
   HBTEST Str( Val( " 12. 00" ) )                IS " 12.000"
   HBTEST Str( Val( " 12 .00" ) )                IS "  12.00"
   HBTEST Str( Val( " 12. 1" ) )                 IS " 12.00"
   HBTEST Str( Val( " 12 .1" ) )                 IS "  12.0"
   HBTEST Str( Val( " 12. 10" ) )                IS " 12.000"
   HBTEST Str( Val( " 12 .10" ) )                IS "  12.00"
   HBTEST Str( Val( "+  12" ) )                  IS "    0"
   HBTEST Str( Val( " + 12" ) )                  IS "    0"
   HBTEST Str( Val( "  +12" ) )                  IS "   12"
   HBTEST Str( Val( "+++12" ) )                  IS "    0"
   HBTEST Str( Val( Chr( 9 ) + "12" ) )          IS " 12"
   HBTEST Str( Val( Chr( 10 ) + "12" ) )         IS " 12"
   HBTEST Str( Val( Chr( 13 ) + "12" ) )         IS " 12"
   HBTEST Str( Val( "1E2" ) )                    IS "  1"
   HBTEST Str( Val( "+INF" ) )                   IS "   0"
   HBTEST Str( Val( "-INF" ) )                   IS "   0"
   HBTEST Str( Val( "+NAN" ) )                   IS "   0"
   HBTEST Str( Val( "-NAN" ) )                   IS "   0"
   HBTEST Str( Val( "2.0000000000000001" ) )     IS "2.0000000000000000"
   HBTEST Str( Val( "2.0000000000000009" ) )     IS "2.0000000000000010"
   HBTEST Str( Val( "2.000000000000001" ) )      IS "2.000000000000001"
   HBTEST Str( Val( "2.000000000000009" ) )      IS "2.000000000000009"
   HBTEST Str( Val( "2.00000000000001" ) )       IS "2.00000000000001"
   HBTEST Str( Val( "2.00000000000009" ) )       IS "2.00000000000009"
   HBTEST Str( Val( "2.000000000001" ) )         IS "2.000000000001"
   HBTEST Str( Val( "2.00000000001" ) )          IS "2.00000000001"
   HBTEST Str( Val( "1HELLO." ) )                IS "      1"

   /* Chr() */

   HBTEST Chr( NIL )                      IS "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Chr( "A" )                      IS "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:C:A F:S"
   HBTEST Chr( "ADDDDDD" )                IS "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:C:ADDDDDD F:S"
   HBTEST Chr( -10000000.0 )              IS Chr( 128 )
   HBTEST Chr( -100000 )                  IS Chr( 96 )
   HBTEST Chr( -65 )                      IS Chr( 191 )
   HBTEST Chr( snIntP1 )                  IS "A"
#ifdef __HARBOUR__
   HBTEST Chr( @snIntP1 )                 IS "A"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:U:65 F:S" */
#endif

   /* disable Harbour extensions in compiler to replicate Clipper bugs */
#ifdef __HARBOUR__
   #pragma -kh-
#endif
   HBTEST Chr( 0 )                        IS Chr( 0 )
   HBTEST Chr( 0.0 )                      IS Chr( 0 )
   HBTEST Chr( 0.1 )                      IS Chr( 0 )
   HBTEST Chr( -0.1 )                     IS Chr( 0 )
   HBTEST Chr( 66.4 )                     IS "B"
   HBTEST Chr( 66.5 )                     IS "B"
   HBTEST Chr( 66.6 )                     IS "B"
   HBTEST Chr( 255 )                      IS Chr( 255 ) /* somewhat pointless test ;) */
   HBTEST Chr( 256 )                      IS ""        /* Due to a bug in CA-Cl*pper compiler optimizer. It should return Chr( 0 ) */
   HBTEST Chr( 256.0 )                    IS Chr( 0 )
   HBTEST Chr( 256.1 )                    IS Chr( 0 )
   HBTEST Chr( ( 256 ) )                  IS Chr( 0 )  /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   HBTEST Chr( 257 )                      IS Chr( 1 )
   HBTEST Chr( ( 512 ) )                  IS Chr( 0 )  /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   HBTEST Chr( 1023 )                     IS Chr( 255 )
   HBTEST Chr( ( 1024 ) )                 IS Chr( 0 )  /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   HBTEST Chr( 1025 )                     IS Chr( 1 )
   HBTEST Chr( 1000 )                     IS Chr( 232 )
   HBTEST Chr( 100000 )                   IS Chr( 160 )
   HBTEST Chr( 100000.0 )                 IS Chr( 160 )
#ifdef __HARBOUR__
   /* enable Harbour extensions and test correct results results */
   #pragma -kh+
   HBTEST Chr( 0 )                        IS Chr( 0 )
   HBTEST Chr( 0.0 )                      IS Chr( 0 )
   HBTEST Chr( 0.1 )                      IS Chr( 0 )
   HBTEST Chr( -0.1 )                     IS Chr( 0 )
   HBTEST Chr( 66.4 )                     IS "B"
   HBTEST Chr( 66.5 )                     IS "B"
   HBTEST Chr( 66.6 )                     IS "B"
   HBTEST Chr( 255 )                      IS Chr( 255 )
   HBTEST Chr( 256 )                      IS Chr( 0 )
   HBTEST Chr( 256.0 )                    IS Chr( 0 )
   HBTEST Chr( 256.1 )                    IS Chr( 0 )
   HBTEST Chr( ( 256 ) )                  IS Chr( 0 )
   HBTEST Chr( 257 )                      IS Chr( 1 )
   HBTEST Chr( ( 512 ) )                  IS Chr( 0 )
   HBTEST Chr( 1023 )                     IS Chr( 255 )
   HBTEST Chr( ( 1024 ) )                 IS Chr( 0 )
   HBTEST Chr( 1025 )                     IS Chr( 1 )
   HBTEST Chr( 1000 )                     IS Chr( 232 )
   HBTEST Chr( 100000 )                   IS Chr( 160 )
   HBTEST Chr( 100000.0 )                 IS Chr( 160 )
#endif

   /* Asc() */

   HBTEST Asc( NIL )                      IS "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Asc( 100 )                      IS "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:N:100 F:S"
   HBTEST Asc( 20000 )                    IS "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:N:20000 F:S"
   HBTEST Asc( "HELLO" )                  IS 72
   HBTEST Asc( Chr( 0 ) )                 IS 0
   HBTEST Asc( "a" )                      IS 97
   HBTEST Asc( "A" )                      IS 65
   HBTEST Asc( scString )                 IS 72
#ifdef __HARBOUR__
   HBTEST Asc( @scString )                IS 72  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:U:HELLO F:S" */
#endif

   /* IsDigit() */

#ifndef __XPP__
   HBTEST IsDigit()                       IS .F.
#endif
   HBTEST IsDigit( 100 )                  IS .F.
#ifdef __HARBOUR__
   HBTEST IsDigit( @scString )            IS .F.  /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   HBTEST IsDigit( "" )                   IS .F.
   HBTEST IsDigit( "A" )                  IS .F.
   HBTEST IsDigit( "AA" )                 IS .F.
   HBTEST IsDigit( "-" )                  IS .F.
   HBTEST IsDigit( "." )                  IS .F.
   HBTEST IsDigit( "0" )                  IS .T.
   HBTEST IsDigit( "9" )                  IS .T.
   HBTEST IsDigit( "123" )                IS .T.
   HBTEST IsDigit( "1" )                  IS .T.
   HBTEST IsDigit( "A1" )                 IS .F.
   HBTEST IsDigit( "1A" )                 IS .T.

   /* IsAlpha() */

#ifndef __XPP__
   HBTEST IsAlpha()                       IS .F.
#endif
   HBTEST IsAlpha( 100 )                  IS .F.
#ifdef __HARBOUR__
   HBTEST IsAlpha( @scString )            IS .T.  /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   HBTEST IsAlpha( "" )                   IS .F.
   HBTEST IsAlpha( "A" )                  IS .T.
   HBTEST IsAlpha( "AA" )                 IS .T.
   HBTEST IsAlpha( "-" )                  IS .F.
   HBTEST IsAlpha( "." )                  IS .F.
   HBTEST IsAlpha( "0" )                  IS .F.
   HBTEST IsAlpha( "9" )                  IS .F.
   HBTEST IsAlpha( "123" )                IS .F.
   HBTEST IsAlpha( "1" )                  IS .F.
   HBTEST IsAlpha( "A" )                  IS .T.
   HBTEST IsAlpha( "A1" )                 IS .T.
   HBTEST IsAlpha( "aa" )                 IS .T.
   HBTEST IsAlpha( "za" )                 IS .T.
   HBTEST IsAlpha( "Aa" )                 IS .T.
   HBTEST IsAlpha( "Za" )                 IS .T.
   HBTEST IsAlpha( "@"  )                 IS .F.
   HBTEST IsAlpha( "["  )                 IS .F.
   HBTEST IsAlpha( "`"  )                 IS .F.
   HBTEST IsAlpha( "{"  )                 IS .F.

   /* IsUpper() */

#ifndef __XPP__
   HBTEST IsUpper()                       IS .F.
#endif
   HBTEST IsUpper( 100 )                  IS .F.
#ifdef __HARBOUR__
   HBTEST IsUpper( @scString )            IS .T.  /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   HBTEST IsUpper( "" )                   IS .F.
   HBTEST IsUpper( "6" )                  IS .F.
   HBTEST IsUpper( "A" )                  IS .T.
   HBTEST IsUpper( "AA" )                 IS .T.
   HBTEST IsUpper( "a" )                  IS .F.
   HBTEST IsUpper( "K" )                  IS .T.
   HBTEST IsUpper( "Z" )                  IS .T.
   HBTEST IsUpper( "z" )                  IS .F.
   HBTEST IsUpper( Chr( 153 ) )           IS .F.
   HBTEST IsUpper( Chr( 148 ) )           IS .F.

   /* IsLower() */

#ifndef __XPP__
   HBTEST IsLower()                       IS .F.
#endif
   HBTEST IsLower( 100 )                  IS .F.
#ifdef __HARBOUR__
   HBTEST IsLower( @scString )            IS .F.  /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   HBTEST IsLower( "" )                   IS .F.
   HBTEST IsLower( "6" )                  IS .F.
   HBTEST IsLower( "A" )                  IS .F.
   HBTEST IsLower( "AA" )                 IS .F.
   HBTEST IsLower( "a" )                  IS .T.
   HBTEST IsLower( "K" )                  IS .F.
   HBTEST IsLower( "Z" )                  IS .F.
   HBTEST IsLower( "z" )                  IS .T.
   HBTEST IsLower( Chr( 153 ) )           IS .F.
   HBTEST IsLower( Chr( 148 ) )           IS .F.

   /* AllTrim() */

#ifdef HB_COMPAT_C53
   /* These lines will cause CA-Cl*pper 5.2e to trash memory and later crash, it was fixed in 5.3 */
   HBTEST AllTrim( NIL )                             IS "E 1 BASE 2022 Argument error (ALLTRIM) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST AllTrim( 100 )                             IS "E 1 BASE 2022 Argument error (ALLTRIM) OS:0 #:0 A:1:N:100 F:S"
#endif
#ifdef __HARBOUR__
   HBTEST AllTrim( @scString )                       IS "HELLO"  /* CA-Cl*pper bug, it will terminate the program on this line. */
#endif
   HBTEST AllTrim( scString )                        IS "HELLO"
   HBTEST AllTrim( "HELLO" )                         IS "HELLO"
   HBTEST AllTrim( "" )                              IS ""
   HBTEST AllTrim( "UA   " )                         IS "UA"
   HBTEST AllTrim( "   UA" )                         IS "UA"
   HBTEST AllTrim( "   UA  " )                       IS "UA"
   HBTEST AllTrim( " " + Chr( 0 ) + " UA  " )        IS Chr( 0 ) + " UA"
   HBTEST AllTrim( " " + Chr( 9 ) + " UA  " )        IS "UA"
   HBTEST AllTrim( " " + Chr( 9 ) + "U" + Chr( 9 ) ) IS "U" + Chr( 9 )
   HBTEST AllTrim( " " + Chr( 9 ) + Chr( 9 ) )       IS ""
   HBTEST AllTrim( Chr( 10 ) + "U" + Chr( 10 ) )     IS "U" + Chr( 10 )
   HBTEST AllTrim( Chr( 13 ) + "U" + Chr( 13 ) )     IS "U" + Chr( 13 )
   HBTEST AllTrim( "A" + Chr( 10 ) )                 IS "A" + Chr( 10 )
   HBTEST AllTrim( "A" + Chr( 13 ) )                 IS "A" + Chr( 13 )
   HBTEST AllTrim( "  " + Chr( 0 ) + "ABC" + Chr( 0 ) + "  " ) IS Chr( 0 ) + "ABC" + Chr( 0 )

   /* Trim() */

   HBTEST Trim( 100 )                              IS "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:N:100 F:S"
   HBTEST Trim( NIL )                              IS "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:U:NIL F:S"
#ifdef __HARBOUR__
   HBTEST Trim( @scString )                        IS "HELLO"  /* CA-Cl*pper bug, it will throw an error here. */
#endif
   HBTEST Trim( scString )                         IS "HELLO"
   HBTEST Trim( "HELLO" )                          IS "HELLO"
   HBTEST Trim( "" )                               IS ""
   HBTEST Trim( "UA   " )                          IS "UA"
   HBTEST Trim( "   UA" )                          IS "   UA"
   HBTEST Trim( "   UA  " )                        IS "   UA"
   HBTEST Trim( " " + Chr( 0 ) + " UA  " )         IS " " + Chr( 0 ) + " UA"
   HBTEST Trim( " " + Chr( 9 ) + " UA  " )         IS " " + Chr( 9 ) + " UA"
   HBTEST Trim( " " + Chr( 9 ) + "U" + Chr( 9 ) )  IS " " + Chr( 9 ) + "U" + Chr( 9 )
   HBTEST Trim( " " + Chr( 9 ) + Chr( 9 ) )        IS " " + Chr( 9 ) + Chr( 9 )
   HBTEST Trim( Chr( 10 ) + "U" + Chr( 10 ) )      IS Chr( 10 ) + "U" + Chr( 10 )
   HBTEST Trim( Chr( 13 ) + "U" + Chr( 13 ) )      IS Chr( 13 ) + "U" + Chr( 13 )
   HBTEST Trim( "A" + Chr( 10 ) )                  IS "A" + Chr( 10 )
   HBTEST Trim( "A" + Chr( 13 ) )                  IS "A" + Chr( 13 )
   HBTEST Trim( "  " + Chr( 0 ) + "ABC" + Chr( 0 ) + "  " ) IS "  " + Chr( 0 ) + "ABC" + Chr( 0 )

   /* RTrim() */

   HBTEST RTrim( 100 )                             IS "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:N:100 F:S"
   HBTEST RTrim( NIL )                             IS "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:U:NIL F:S"
#ifdef __HARBOUR__
   HBTEST RTrim( @scString )                       IS "HELLO"  /* CA-Cl*pper bug, it will throw an error here. */
#endif
   HBTEST RTrim( scString )                        IS "HELLO"
   HBTEST RTrim( "HELLO" )                         IS "HELLO"
   HBTEST RTrim( "" )                              IS ""
   HBTEST RTrim( "UA   " )                         IS "UA"
   HBTEST RTrim( "   UA" )                         IS "   UA"
   HBTEST RTrim( "   UA  " )                       IS "   UA"
   HBTEST RTrim( " " + Chr( 0 ) + " UA  " )        IS " " + Chr( 0 ) + " UA"
   HBTEST RTrim( " " + Chr( 9 ) + " UA  " )        IS " " + Chr( 9 ) + " UA"
   HBTEST RTrim( " " + Chr( 9 ) + "U" + Chr( 9 ) ) IS " " + Chr( 9 ) + "U" + Chr( 9 )
   HBTEST RTrim( " " + Chr( 9 ) + Chr( 9 ) )       IS " " + Chr( 9 ) + Chr( 9 )
   HBTEST RTrim( Chr( 10 ) + "U" + Chr( 10 ) )     IS Chr( 10 ) + "U" + Chr( 10 )
   HBTEST RTrim( Chr( 13 ) + "U" + Chr( 13 ) )     IS Chr( 13 ) + "U" + Chr( 13 )
   HBTEST RTrim( "A" + Chr( 10 ) )                 IS "A" + Chr( 10 )
   HBTEST RTrim( "A" + Chr( 13 ) )                 IS "A" + Chr( 13 )
   HBTEST RTrim( "  " + Chr( 0 ) + "ABC" + Chr( 0 ) + "  " ) IS "  " + Chr( 0 ) + "ABC" + Chr( 0 )

   /* LTrim() */

   HBTEST LTrim( 100 )                             IS "E 1 BASE 1101 Argument error (LTRIM) OS:0 #:0 A:1:N:100 F:S"
   HBTEST LTrim( NIL )                             IS "E 1 BASE 1101 Argument error (LTRIM) OS:0 #:0 A:1:U:NIL F:S"
#ifdef __HARBOUR__
   HBTEST LTrim( @scString )                       IS "HELLO"  /* CA-Cl*pper bug, it will throw an error here. */
#endif
   HBTEST LTrim( scString )                        IS "HELLO"
   HBTEST LTrim( "HELLO" )                         IS "HELLO"
   HBTEST LTrim( "" )                              IS ""
   HBTEST LTrim( "UA   " )                         IS "UA   "
   HBTEST LTrim( "   UA" )                         IS "UA"
   HBTEST LTrim( "   UA  " )                       IS "UA  "
   HBTEST LTrim( " " + Chr( 0 ) + " UA  " )        IS Chr( 0 ) + " UA  "
   HBTEST LTrim( " " + Chr( 9 ) + " UA  " )        IS "UA  "
   HBTEST LTrim( " " + Chr( 9 ) + "U" + Chr( 9 ) ) IS "U" + Chr( 9 )
   HBTEST LTrim( " " + Chr( 9 ) + Chr( 9 ) )       IS ""
   HBTEST LTrim( Chr( 10 ) + "U" + Chr( 10 ) )     IS "U" + Chr( 10 )
   HBTEST LTrim( Chr( 13 ) + "U" + Chr( 13 ) )     IS "U" + Chr( 13 )
   HBTEST LTrim( "A" + Chr( 10 ) )                 IS "A" + Chr( 10 )
   HBTEST LTrim( "A" + Chr( 13 ) )                 IS "A" + Chr( 13 )
   HBTEST LTrim( "  " + Chr( 0 ) + "ABC" + Chr( 0 ) + "  " ) IS Chr( 0 ) + "ABC" + Chr( 0 ) + "  "

   /* StrTran() */

   /* TODO: StrTran() */

/* NOTE: It seems like CA-Cl*pper 5.x is not aware of the BREAK return value of
         the error handler, so the error is thrown, but we can't catch it.
         This bug is fixed in CA-Cl*pper 5.3 [vszakats] */
#ifndef __CLIPPER__
#ifndef __XPP__
   HBTEST StrTran()                       IS "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 F:S"  /* CA-Cl*pper bug, it will exit on this */
   HBTEST StrTran( NIL )                  IS "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:1:U:NIL F:S"  /* CA-Cl*pper bug, it will exit on this */
   HBTEST StrTran( 100 )                  IS "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:1:N:100 F:S"  /* CA-Cl*pper bug, it will exit on this */
#endif
   HBTEST StrTran( "AA", 1 )              IS "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:2:C:AA;N:1 F:S"  /* CA-Cl*pper bug, it will exit on this */
#endif
   HBTEST StrTran( "AA", "A" )            IS ""
   HBTEST StrTran( "AA", "A", "1" )       IS "11"
   HBTEST StrTran( "AA", "A", "1", "2" )  IS "11"

   /* Upper() */

   HBTEST Upper( scString )                IS "HELLO"
#ifdef __HARBOUR__
   HBTEST Upper( @scString )               IS "HELLO"  /* Bug in CA-Cl*pper, it will return argument error */
#endif
   HBTEST Upper( 100 )                     IS "E 1 BASE 1102 Argument error (UPPER) OS:0 #:0 A:1:N:100 F:S"
   HBTEST Upper( "" )                      IS ""
   HBTEST Upper( " " )                     IS " "
   HBTEST Upper( "2" )                     IS "2"
   HBTEST Upper( "{" )                     IS "{"
   HBTEST Upper( Chr( 0 ) )                IS Chr( 0 )
   HBTEST Upper( "aAZAZa" )                IS "AAZAZA"
   HBTEST Upper( "AazazA" )                IS "AAZAZA"
   HBTEST Upper( "Aaz" + Chr( 0 ) + "zA" ) IS "AAZ" + Chr( 0 ) + "ZA"
   HBTEST Upper( "z" )                     IS "Z"
   HBTEST Upper( Chr( 160 ) + Chr( 181 ) ) IS Chr( 160 ) + Chr( 181 )
   HBTEST Upper( "H" + Chr( 160 ) + "rbor 8-) " + Chr( 181 ) ) IS "H" + Chr( 160 ) + "RBOR 8-) " + Chr( 181 )

   /* Lower() */

   HBTEST Lower( scString )                IS "hello"
#ifdef __HARBOUR__
   HBTEST Lower( @scString )               IS "hello"  /* Bug in CA-Cl*pper, it will return argument error */
#endif
   HBTEST Lower( 100 )                     IS "E 1 BASE 1103 Argument error (LOWER) OS:0 #:0 A:1:N:100 F:S"
   HBTEST Lower( "" )                      IS ""
   HBTEST Lower( " " )                     IS " "
   HBTEST Lower( "2" )                     IS "2"
   HBTEST Lower( "{" )                     IS "{"
   HBTEST Lower( Chr( 0 ) )                IS Chr( 0 )
   HBTEST Lower( "aAZAZa" )                IS "aazaza"
   HBTEST Lower( "AazazA" )                IS "aazaza"
   HBTEST Lower( "Aaz" + Chr( 0 ) + "zA" ) IS "aaz" + Chr( 0 ) + "za"
   HBTEST Lower( "z" )                     IS "z"
   HBTEST Lower( Chr( 160 ) + Chr( 181 ) ) IS Chr( 160 ) + Chr( 181 )
   HBTEST Lower( "H" + Chr( 160 ) + "rbor 8-) " + Chr( 181 ) ) IS "h" + Chr( 160 ) + "rbor 8-) " + Chr( 181 )

   /* At() */

   HBTEST At( 90, 100 )                   IS "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:N:90;N:100 F:S"
   HBTEST At( "", 100 )                   IS "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:C:;N:100 F:S"
   HBTEST At( 100, "" )                   IS "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:N:100;C: F:S"
   /* disable Harbour extensions in compiler to replicate Clipper bugs */
#ifdef __HARBOUR__
   #pragma -kh-
#endif
   HBTEST At( "", "" )                    IS 1  /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
   HBTEST At( "", "ABCDEF" )              IS 1  /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
#ifdef __HARBOUR__
   /* enable Harbour extensions and test correct results results */
   #pragma -kh+
   HBTEST At( "", "" )                    IS 0  /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
   HBTEST At( "", "ABCDEF" )              IS 0  /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
#endif
   HBTEST At( scStringE, scStringE )      IS 0
   HBTEST At( scStringE, "ABCDEF" )       IS 0
   HBTEST At( "ABCDEF", "" )              IS 0
   HBTEST At( "AB", "AB" )                IS 1
   HBTEST At( "AB", "AAB" )               IS 2
   HBTEST At( "A", "ABCDEF" )             IS 1
   HBTEST At( "F", "ABCDEF" )             IS 6
   HBTEST At( "D", "ABCDEF" )             IS 4
   HBTEST At( "X", "ABCDEF" )             IS 0
   HBTEST At( "AB", "ABCDEF" )            IS 1
   HBTEST At( "AA", "ABCDEF" )            IS 0
   HBTEST At( "ABCDEF", "ABCDEF" )        IS 1
   HBTEST At( "BCDEF", "ABCDEF" )         IS 2
   HBTEST At( "BCDEFG", "ABCDEF" )        IS 0
   HBTEST At( "ABCDEFG", "ABCDEF" )       IS 0
   HBTEST At( "FI", "ABCDEF" )            IS 0

   /* RAt() */

   HBTEST RAt( 90, 100 )                  IS 0
   HBTEST RAt( "", 100 )                  IS 0
   HBTEST RAt( 100, "" )                  IS 0
   HBTEST RAt( "", "" )                   IS 0
   HBTEST RAt( "", "ABCDEF" )             IS 0
   HBTEST RAt( "ABCDEF", "" )             IS 0
   HBTEST RAt( "AB", "AB" )               IS 1
   HBTEST RAt( "AB", "AAB" )              IS 2
   HBTEST RAt( "AB", "ABAB" )             IS 3
   HBTEST RAt( "A", "ABCADEF" )           IS 4
   HBTEST RAt( "A", "ABCADEFA" )          IS 8
   HBTEST RAt( "A", "ABCDEFA" )           IS 7
   HBTEST RAt( "A", "ABCDEF" )            IS 1
   HBTEST RAt( "F", "ABCDEF" )            IS 6
   HBTEST RAt( "D", "ABCDEF" )            IS 4
   HBTEST RAt( "X", "ABCDEF" )            IS 0
   HBTEST RAt( "AB", "ABCDEF" )           IS 1
   HBTEST RAt( "AA", "ABCDEF" )           IS 0
   HBTEST RAt( "ABCDEF", "ABCDEF" )       IS 1
   HBTEST RAt( "BCDEF", "ABCDEF" )        IS 2
   HBTEST RAt( "BCDEFG", "ABCDEF" )       IS 0
   HBTEST RAt( "ABCDEFG", "ABCDEF" )      IS 0
   HBTEST RAt( "FI", "ABCDEF" )           IS 0

   /* Replicate() */

#ifdef __HARBOUR__
   IF l64
      HBTEST Replicate( "XXX", 9000000000000000000 ) IS "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:9000000000000000000 F:S"
   ELSE
      HBTEST Replicate( "XXX", 2000000000 )          IS "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:2000000000 F:S"
   ENDIF
#else
   HBTEST Replicate( "XXX", 30000)        IS "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:30000 F:S"
#endif
   HBTEST Replicate( 200  , 0 )           IS "E 1 BASE 1106 Argument error (REPLICATE) OS:0 #:0 A:2:N:200;N:0 F:S"
   HBTEST Replicate( ""   , 10 )          IS ""
   HBTEST Replicate( ""   , 0 )           IS ""
   HBTEST Replicate( "A"  , "B" )         IS "E 1 BASE 1106 Argument error (REPLICATE) OS:0 #:0 A:2:C:A;C:B F:S"
   HBTEST Replicate( "A"  , 1 )           IS "A"
   HBTEST Replicate( "A"  , 2 )           IS "AA"
   HBTEST Replicate( "HE", 3 )            IS "HEHEHE"
   HBTEST Replicate( "HE", 3.1 )          IS "HEHEHE"
   HBTEST Replicate( "HE", 3.5 )          IS "HEHEHE"
   HBTEST Replicate( "HE", 3.7 )          IS "HEHEHE"
   HBTEST Replicate( "HE", -3 )           IS ""
   HBTEST Replicate( "H" + Chr( 0 ), 2 )  IS "H" + Chr( 0 ) + "H" + Chr( 0 )

   /* Space() */

   HBTEST Space( "A" )                    IS "E 1 BASE 1105 Argument error (SPACE) OS:0 #:0 A:1:C:A F:S"
   HBTEST Space( 0 )                      IS ""
   HBTEST Space( -10 )                    IS ""
   HBTEST Space( 10 )                     IS "          "
   HBTEST Space( 10.2 )                   IS "          "
   HBTEST Space( 10.5 )                   IS "          "
   HBTEST Space( 10.7 )                   IS "          "

   /* SubStr() */

   HBTEST SubStr( 100     , 0, -1 )       IS "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:N:100;N:0;N:-1 F:S"
   HBTEST SubStr( "abcdef", 1, "a" )      IS "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:C:abcdef;N:1;C:a F:S"
   HBTEST SubStr( "abcdef", "a" )         IS "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:2:C:abcdef;C:a F:S"
   HBTEST SubStr( "abcdef", "a", 1 )      IS "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:C:abcdef;C:a;N:1 F:S"
   HBTEST SubStr( "abcdef", 0, -1 )       IS ""
   HBTEST SubStr( "abcdef", 0, 0 )        IS ""
   HBTEST SubStr( "abcdef", 0, 1 )        IS "a"
   HBTEST SubStr( "abcdef", 0, 7 )        IS "abcdef"
   HBTEST SubStr( "abcdef", 0 )           IS "abcdef"
   HBTEST SubStr( "abcdef", 2, -1 )       IS ""
   HBTEST SubStr( "abcdef", 2, 0 )        IS ""
   HBTEST SubStr( "abcdef", 2, 1 )        IS "b"
   HBTEST SubStr( "abcdef", 2, 7 )        IS "bcdef"
   HBTEST SubStr( "abcdef", 2 )           IS "bcdef"
#ifndef __XPP__
   HBTEST SubStr( "abcdef", -2, -1 )      IS ""
   HBTEST SubStr( "abcdef", -2, 0 )       IS ""
   HBTEST SubStr( "abcdef", -2, 1 )       IS "e"
   HBTEST SubStr( "abcdef", -2, 7 )       IS "ef"
   HBTEST SubStr( "abcdef", -2 )          IS "ef"
#endif
   HBTEST SubStr( "abcdef", 10, -1 )      IS ""
   HBTEST SubStr( "abcdef", 10, 0 )       IS ""
   HBTEST SubStr( "abcdef", 10, 1 )       IS ""
   HBTEST SubStr( "abcdef", 10, 7 )       IS ""
   HBTEST SubStr( "abcdef", 10 )          IS ""
#ifndef __XPP__
   HBTEST SubStr( "abcdef", -10, -1 )     IS ""
   HBTEST SubStr( "abcdef", -10, 0 )      IS ""
   HBTEST SubStr( "abcdef", -10, 1 )      IS "a"
   HBTEST SubStr( "abcdef", -10, 7 )      IS "abcdef"
   HBTEST SubStr( "abcdef", -10, 15 )     IS "abcdef"
   HBTEST SubStr( "abcdef", -10 )         IS "abcdef"
#endif
   HBTEST SubStr( "ab" + Chr( 0 ) + "def", 2, 3 ) IS "b" + Chr( 0 ) + "d"
   HBTEST SubStr( "abc" + Chr( 0 ) + "def", 3, 1 ) IS "c"
   HBTEST SubStr( "abc" + Chr( 0 ) + "def", 4, 1 ) IS Chr( 0 )
   HBTEST SubStr( "abc" + Chr( 0 ) + "def", 5, 1 ) IS "d"

   /* Left() */

   HBTEST Left( 100     , -10 )               IS "E 1 BASE 1124 Argument error (LEFT) OS:0 #:0 A:2:N:100;N:-10 F:S"
   HBTEST Left( "abcdef", "A" )               IS "E 1 BASE 1124 Argument error (LEFT) OS:0 #:0 A:2:C:abcdef;C:A F:S"
   HBTEST Left( "abcdef", -10 )               IS ""
   HBTEST Left( "abcdef", -2 )                IS ""
   HBTEST Left( "abcdef", 0 )                 IS ""
   HBTEST Left( "abcdef", 2 )                 IS "ab"
   HBTEST Left( "abcdef", 10 )                IS "abcdef"
   HBTEST Left( "ab" + Chr( 0 ) + "def", 5 )  IS "ab" + Chr( 0 ) + "de"

   /* Right() */

   HBTEST Right( 100     , -10 )              IS ""
   HBTEST Right( "abcdef", "A" )              IS ""
   HBTEST Right( "abcdef", -10 )              IS ""
   HBTEST Right( "abcdef", -2 )               IS ""
   HBTEST Right( "abcdef", 0 )                IS ""
   HBTEST Right( "abcdef", 2 )                IS "ef"
   HBTEST Right( "abcdef", 10 )               IS "abcdef"
   HBTEST Right( "ab" + Chr( 0 ) + "def", 5 ) IS "b" + Chr( 0 ) + "def"

   /* PadR() */

   HBTEST Pad( NIL, 5 )                            IS ""
   HBTEST Pad( .T., 5 )                            IS ""
   HBTEST Pad( 10, 5 )                             IS "10   "
   HBTEST Pad( 10.2, 5 )                           IS "10.2 "
   HBTEST Pad( 100000, 8 )                         IS "100000  "
   HBTEST Pad( 100000, 8, "-" )                    IS "100000--"
   HBTEST Pad( -100000, 8, "-" )                   IS "-100000-"
   HBTEST Pad( 5000000000, 15 )                    IS "5000000000     "
   HBTEST Pad( hb_SToD( "19840325" ), 12 )         IS "1984-03-25  "
   HBTEST Pad( Year( hb_SToD( "19840325" ) ), 5 )  IS "1984 "
   HBTEST Pad( Day( hb_SToD( "19840325" ) ), 5 )   IS "25   "
#ifdef __HARBOUR__
   HBTEST Pad( @scString, 10 )                     IS "HELLO     "  /* Bug in CA-Cl*pper, it will return "" */
   HBTEST Pad( scString, @snIntP )                 IS "HELLO     "  /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   HBTEST Pad( "abcdef", "A" )                     IS ""
   HBTEST Pad( "abcdef", -5 )                      IS ""
#endif
   HBTEST Pad( "abcdef", 0 )                       IS ""
   HBTEST Pad( "abcdef", 5 )                       IS "abcde"
   HBTEST Pad( "abcdef", 10 )                      IS "abcdef    "
   HBTEST Pad( "abcdef", 10, "" )                  IS "abcdef" + Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST Pad( "abcdef", 10, "1" )                 IS "abcdef1111"
   HBTEST Pad( "abcdef", 10, "12" )                IS "abcdef1111"

   /* PadR() */

   HBTEST PadR( NIL, 5 )                           IS ""
   HBTEST PadR( .T., 5 )                           IS ""
   HBTEST PadR( 10, 5 )                            IS "10   "
   HBTEST PadR( 10.2, 5 )                          IS "10.2 "
   HBTEST PadR( 100000, 8 )                        IS "100000  "
   HBTEST PadR( 100000, 8, "-" )                   IS "100000--"
   HBTEST PadR( -100000, 8, "-" )                  IS "-100000-"
   HBTEST PadR( hb_SToD( "19840325" ), 12 )        IS "1984-03-25  "
   HBTEST PadR( Year( hb_SToD( "19840325" ) ), 5 ) IS "1984 "
   HBTEST PadR( Day( hb_SToD( "19840325" ) ), 5 )  IS "25   "
#ifdef __HARBOUR__
   HBTEST PadR( @scString, 10 )                    IS "HELLO     "  /* Bug in CA-Cl*pper, it will return "" */
   HBTEST PadR( scString, @snIntP )                IS "HELLO     "  /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   HBTEST PadR( "abcdef", "A" )                    IS ""
   HBTEST PadR( "abcdef", -5 )                     IS ""
#endif
   HBTEST PadR( "abcdef", 0 )                      IS ""
   HBTEST PadR( "abcdef", 5 )                      IS "abcde"
   HBTEST PadR( "abcdef", 10 )                     IS "abcdef    "
   HBTEST PadR( "abcdef", 10, "" )                 IS "abcdef" + Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST PadR( "abcdef", 10, "1" )                IS "abcdef1111"
   HBTEST PadR( "abcdef", 10, "12" )               IS "abcdef1111"

   /* PadL() */

   HBTEST PadL( NIL, 5 )                           IS ""
   HBTEST PadL( .T., 5 )                           IS ""
   HBTEST PadL( 10, 5 )                            IS "   10"
   HBTEST PadL( 10.2, 5 )                          IS " 10.2"
   HBTEST PadL( 100000, 8 )                        IS "  100000"
   HBTEST PadL( 100000, 8, "-" )                   IS "--100000"
   HBTEST PadL( -100000, 8, "-" )                  IS "--100000"
   HBTEST PadL( hb_SToD( "19840325" ), 12 )        IS "  1984-03-25"
   HBTEST PadL( Year( hb_SToD( "19840325" ) ), 5 ) IS " 1984"
   HBTEST PadL( Day( hb_SToD( "19840325" ) ), 5 )  IS "   25"
#ifdef __HARBOUR__
   HBTEST PadL( @scString, 10 )                    IS "     HELLO"  /* Bug in CA-Cl*pper, it will return "" */
   HBTEST PadL( scString, @snIntP )                IS "     HELLO"  /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   HBTEST PadL( "abcdef", "A" )                    IS ""
   HBTEST PadL( "abcdef", -5 )                     IS ""
#endif
   HBTEST PadL( "abcdef", 0 )                      IS ""
   HBTEST PadL( "abcdef", 5 )                      IS "abcde"  /* QUESTION: CA-Cl*pper "bug", should return: "bcdef" ? */
   HBTEST PadL( "abcdef", 10 )                     IS "    abcdef"
   HBTEST PadL( "abcdef", 10, "" )                 IS Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + "abcdef"
   HBTEST PadL( "abcdef", 10, "1" )                IS "1111abcdef"
   HBTEST PadL( "abcdef", 10, "12" )               IS "1111abcdef"

   /* PadC() */

   HBTEST PadC( NIL, 5 )                           IS ""
   HBTEST PadC( .T., 5 )                           IS ""
   HBTEST PadC( 10, 5 )                            IS " 10  "
   HBTEST PadC( 10.2, 5 )                          IS "10.2 "
   HBTEST PadC( 100000, 8 )                        IS " 100000 "
   HBTEST PadC( 100000, 8, "-" )                   IS "-100000-"
   HBTEST PadC( -100000, 8, "-" )                  IS "-100000-"
   HBTEST PadC( hb_SToD( "19840325" ), 12 )        IS " 1984-03-25 "
   HBTEST PadC( Year( hb_SToD( "19840325" ) ), 5 ) IS "1984 "
   HBTEST PadC( Day( hb_SToD( "19840325" ) ), 5 )  IS " 25  "
#ifdef __HARBOUR__
   HBTEST PadC( @scString, 10 )                    IS "  HELLO   "  /* Bug in CA-Cl*pper, it will return "" */
   HBTEST PadC( scString, @snIntP )                IS "  HELLO   "  /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   HBTEST PadC( "abcdef", "A" )                    IS ""
   HBTEST PadC( "abcdef", -5 )                     IS ""
#endif
   HBTEST PadC( "abcdef", 0 )                      IS ""
   HBTEST PadC( "abcdef", 2 )                      IS "ab"  /* QUESTION: CA-Cl*pper "bug", should return: "cd" ? */
   HBTEST PadC( "abcdef", 5 )                      IS "abcde"
   HBTEST PadC( "abcdef", 10 )                     IS "  abcdef  "
   HBTEST PadC( "abcdef", 10, "" )                 IS Chr( 0 ) + Chr( 0 ) + "abcdef" + Chr( 0 ) + Chr( 0 )
   HBTEST PadC( "abcdef", 10, "1" )                IS "11abcdef11"
   HBTEST PadC( "abcdef", 10, "12" )               IS "11abcdef11"

   /* Stuff() */

#ifndef __XPP__
   HBTEST Stuff()                                                IS ""
   HBTEST Stuff( 100 )                                           IS ""
#endif
   HBTEST Stuff( "ABCDEF", -6, -5, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -6, -2, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -6,  0, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -6, 10, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -6, 30, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -2, -5, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -2, -2, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -2,  0, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -2, 10, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", -2, 30, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF",  0, -5, NIL )                         IS ""
   HBTEST Stuff( "ABCDEF",  0, -2, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  0,  0, "xyz" )                       IS "xyzABCDEF"
   HBTEST Stuff( "ABCDEF",  0, 10, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  0, 30, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  1, -5, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  1, -2, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  1,  0, "xyz" )                       IS "xyzABCDEF"
   HBTEST Stuff( "ABCDEF",  1, 10, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  1, 30, "xyz" )                       IS "xyz"
   HBTEST Stuff( "ABCDEF",  2,  0, "xyz" )                       IS "AxyzBCDEF"
   HBTEST Stuff( "ABCDEF",  2,  3, "" )                          IS "AEF"
   HBTEST Stuff( "ABCDEF",  2,  3, "xyz" )                       IS "AxyzEF"
   HBTEST Stuff( "ABCDEF",  2,  2, "" )                          IS "ADEF"
   HBTEST Stuff( "ABCDEF",  2, -5, "xyz" )                       IS "Axyz"
   HBTEST Stuff( "ABCDEF",  2, -2, "xyz" )                       IS "Axyz"
   HBTEST Stuff( "ABCDEF",  2,  1, "xyz" )                       IS "AxyzCDEF"
   HBTEST Stuff( "ABCDEF",  2,  4, "xyz" )                       IS "AxyzF"
   HBTEST Stuff( "ABCDEF",  2, 10, "xyz" )                       IS "Axyz"
   HBTEST Stuff( "ABCDEF",  2, 30, "xyz" )                       IS "Axyz"
   HBTEST Stuff( "ABCDEF", 30, -5, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", 30, -2, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", 30,  0, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", 30, 10, "xyz" )                       IS "ABCDEFxyz"
   HBTEST Stuff( "ABCDEF", 30, 30, "xyz" )                       IS "ABCDEFxyz"

   HBTEST Stuff( @scString              ,  2,  3, "xyz" )                IS "HxyzO"
   HBTEST Stuff( "ABC" + Chr( 0 ) + "EF",  2,  3, "xyz" )                IS "AxyzEF"
   HBTEST Stuff( "ABCE" + Chr( 0 ) + "F",  2,  3, "xyz" )                IS "Axyz" + Chr( 0 ) + "F"
   HBTEST Stuff( "ABC" + Chr( 0 ) + "EF",  2,  3, "x" + Chr( 0 ) + "z" ) IS "Ax" + Chr( 0 ) + "zEF"

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
