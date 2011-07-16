/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (strings)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

#ifdef __HARBOUR__
   #include "hbver.ch"
#endif

PROCEDURE Main_STR()
   LOCAL l64

#ifdef __HARBOUR__
   l64 := hb_version( HB_VERSION_BITWIDTH ) >= 64
#else
   l64 := .F.
#endif

   /* VAL() */

   TEST_LINE( Val( NIL )                      , "E 1 BASE 1098 Argument error (VAL) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( Val( 10 )                       , "E 1 BASE 1098 Argument error (VAL) OS:0 #:0 A:1:N:10 F:S" )

#ifndef RT_NO_C

#ifdef __PLATFORM__WINDOWS
   IF .F.   /* [U]LONG is 32 bit integer in Win64 */
#else
   IF l64
#endif
      TEST_LINE( Str(R_PASSENL(  1860637360 ))   , "1860637360"                           )
      TEST_LINE( Str(R_PASSENL(         100 ))   , "       100"                           )
      TEST_LINE( Str(R_PASSENL(   999999999 ))   , " 999999999"                           )
      TEST_LINE( Str(R_PASSENL(  1000000000 ))   , "1000000000"                           )
      TEST_LINE( Str(R_PASSENL(  2147483647 ))   , "2147483647"                           )
      TEST_LINE( Str(R_PASSENL(  2147483648 ))   , "2147483648"                           )
      TEST_LINE( Str(R_PASSENL(          -1 ))   , "        -1"                           )
      TEST_LINE( Str(R_PASSENL(  -999999999 ))   , "-999999999"                           )
      TEST_LINE( Str(R_PASSENL( -1000000000 ))   , "         -1000000000"                 )
      TEST_LINE( Str(R_PASSENL(   -99999999 ))   , " -99999999"                           )
      TEST_LINE( Str(R_PASSENL(  -100000000 ))   , "-100000000"                           )

      TEST_LINE( Str(R_PASSENLC())               , "1000000000"                           )
   ELSE
      TEST_LINE( Str(R_PASSENL(  1860637360 ))   , "          1860637360"                 )
      TEST_LINE( Str(R_PASSENL(         100 ))   , "       100"                           )
      TEST_LINE( Str(R_PASSENL(   999999999 ))   , " 999999999"                           )
      TEST_LINE( Str(R_PASSENL(  1000000000 ))   , "          1000000000"                 )
      TEST_LINE( Str(R_PASSENL(  2147483647 ))   , "          2147483647"                 )
      TEST_LINE( Str(R_PASSENL(  2147483648 ))   , "         -2147483648"                 )
      TEST_LINE( Str(R_PASSENL(          -1 ))   , "        -1"                           )
      TEST_LINE( Str(R_PASSENL(  -999999999 ))   , "-999999999"                           )
      TEST_LINE( Str(R_PASSENL( -1000000000 ))   , "         -1000000000"                 )
      TEST_LINE( Str(R_PASSENL(   -99999999 ))   , " -99999999"                           )
      TEST_LINE( Str(R_PASSENL(  -100000000 ))   , "-100000000"                           )

      TEST_LINE( Str(R_PASSENLC())               , "          1000000000"                 )
   ENDIF
#endif

   TEST_LINE( Str(Val(""))                    , "         0"                           )

   TEST_LINE( Str(Val(""))                    , "         0"                           )
   TEST_LINE( Str(Val(" "))                   , "0"                                    )
   TEST_LINE( Str(Val("-"))                   , "0"                                    )
   TEST_LINE( Str(Val("+"))                   , "0"                                    )
   TEST_LINE( Str(Val("-+"))                  , " 0"                                   )
   TEST_LINE( Str(Val("+-"))                  , " 0"                                   )
   TEST_LINE( Str(Val("."))                   , "0"                                    )
   TEST_LINE( Str(Val(".."))                  , "0.0"                                  )
   TEST_LINE( Str(Val("-."))                  , " 0"                                   ) /* Bug in CA-Cl*pper 5.3b, it will return: "  0" */
   TEST_LINE( Str(Val("-.."))                 , "0.0"                                  ) /* Bug in CA-Cl*pper 5.3b, it will return: " 0.0" */
   TEST_LINE( Str(Val("1."))                  , " 1"                                   )
   TEST_LINE( Str(Val("1.."))                 , "1.0"                                  )
   TEST_LINE( Str(Val("1..."))                , "1.00"                                 )
   TEST_LINE( Str(Val("-1."))                 , " -1"                                  )
   TEST_LINE( Str(Val(" -1."))                , "  -1"                                 )
   TEST_LINE( Str(Val(" --1."))               , "    0"                                )
   TEST_LINE( Str(Val("-1.."))                , "-1.0"                                 )
   TEST_LINE( Str(Val("-1..."))               , "-1.00"                                )
   TEST_LINE( Str(Val(".1"))                  , "0.1"                                  )
   TEST_LINE( Str(Val("-.1"))                 , "-0.1"                                 )
   TEST_LINE( Str(Val("-.0"))                 , "0.0"                                  ) /* Bug in CA-Cl*pper 5.3b, it will return: " 0.0" */
   TEST_LINE( Str(Val(" -.1"))                , "-0.1"                                 )
   TEST_LINE( Str(Val(" --.1"))               , "  0.0"                                )
   TEST_LINE( Str(Val("+.1"))                 , "0.1"                                  )
   TEST_LINE( Str(Val(" .1"))                 , "0.1"                                  )
   TEST_LINE( Str(Val("- .1"))                , " 0.0"                                 )
   TEST_LINE( Str(Val("+.1"))                 , "0.1"                                  )
   TEST_LINE( Str(Val("-  12"))               , "    0"                                )
   TEST_LINE( Str(Val(" - 12"))               , "    0"                                )
   TEST_LINE( Str(Val("  -12"))               , "  -12"                                )
   TEST_LINE( Str(Val(" --12"))               , "    0"                                )
   TEST_LINE( Str(Val("   12-"))              , "    12"                               )
   TEST_LINE( Str(Val("   12 -"))             , "     12"                              )
   TEST_LINE( Str(Val(" 13.1.9"))             , " 13.100"                              )
   TEST_LINE( Str(Val(" 12"))                 , " 12"                                  )
   TEST_LINE( Str(Val(" 12"+Chr(0)+"0"))      , "   12"                                )
   TEST_LINE( Str(Val(" 12.1"+Chr(0)+"2"))    , " 12.100"                              )
   TEST_LINE( Str(Val(" 12"+Chr(0)+".2"))     , "  12.0"                               )
   TEST_LINE( Str(Val(" 12.0"))               , " 12.0"                                )
   TEST_LINE( Str(Val(" 12. 0"))              , " 12.00"                               )
   TEST_LINE( Str(Val(" 12 .0"))              , "  12.0"                               )
   TEST_LINE( Str(Val(" 12. 00"))             , " 12.000"                              )
   TEST_LINE( Str(Val(" 12 .00"))             , "  12.00"                              )
   TEST_LINE( Str(Val(" 12. 1"))              , " 12.00"                               )
   TEST_LINE( Str(Val(" 12 .1"))              , "  12.0"                               )
   TEST_LINE( Str(Val(" 12. 10"))             , " 12.000"                              )
   TEST_LINE( Str(Val(" 12 .10"))             , "  12.00"                              )
   TEST_LINE( Str(Val("+  12"))               , "    0"                                )
   TEST_LINE( Str(Val(" + 12"))               , "    0"                                )
   TEST_LINE( Str(Val("  +12"))               , "   12"                                )
   TEST_LINE( Str(Val("+++12"))               , "    0"                                )
   TEST_LINE( Str(Val(Chr(9)+"12"))           , " 12"                                  )
   TEST_LINE( Str(Val(Chr(10)+"12"))          , " 12"                                  )
   TEST_LINE( Str(Val(Chr(13)+"12"))          , " 12"                                  )
   TEST_LINE( Str(Val("1E2"))                 , "  1"                                  )
   TEST_LINE( Str(Val("+INF"))                , "   0"                                 )
   TEST_LINE( Str(Val("-INF"))                , "   0"                                 )
   TEST_LINE( Str(Val("+NAN"))                , "   0"                                 )
   TEST_LINE( Str(Val("-NAN"))                , "   0"                                 )
   TEST_LINE( Str(Val("2.0000000000000001"))  , "2.0000000000000000"                   )
   TEST_LINE( Str(Val("2.0000000000000009"))  , "2.0000000000000010"                   )
   TEST_LINE( Str(Val("2.000000000000001"))   , "2.000000000000001"                    )
   TEST_LINE( Str(Val("2.000000000000009"))   , "2.000000000000009"                    )
   TEST_LINE( Str(Val("2.00000000000001"))    , "2.00000000000001"                     )
   TEST_LINE( Str(Val("2.00000000000009"))    , "2.00000000000009"                     )
   TEST_LINE( Str(Val("2.000000000001"))      , "2.000000000001"                       )
   TEST_LINE( Str(Val("2.00000000001"))       , "2.00000000001"                        )
   TEST_LINE( Str(Val("1HELLO."))             , "      1"                              )

   /* CHR() */

   TEST_LINE( Chr( NIL )                      , "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( Chr( "A" )                      , "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Chr( "ADDDDDD" )                , "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:C:ADDDDDD F:S" )
   TEST_LINE( Chr( -10000000.0 )              , "€"                                    )
   TEST_LINE( Chr( -100000 )                  , "`"                                    )
   TEST_LINE( Chr( -65 )                      , "¿"                                    )
   TEST_LINE( Chr( snIntP1 )                  , "A"                                    )
#ifdef __HARBOUR__
   TEST_LINE( Chr( @snIntP1 )                 , "A"                                    ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1104 Argument error (CHR) OS:0 #:0 A:1:U:65 F:S" */
#endif

   /* disable Harbour extensions in compiler to replicate Clipper bugs */
#ifdef __HARBOUR__
   #pragma -kh-
#endif
   TEST_LINE( Chr( 0 )                        , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 0.0 )                      , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 0.1 )                      , ""+Chr(0)+""                           )
   TEST_LINE( Chr( -0.1 )                     , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 66.4 )                     , "B"                                    )
   TEST_LINE( Chr( 66.5 )                     , "B"                                    )
   TEST_LINE( Chr( 66.6 )                     , "B"                                    )
   TEST_LINE( Chr( 255 )                      , "ÿ"                                    )
   TEST_LINE( Chr( 256 )                      , ""                                     ) /* Due to a bug in CA-Cl*pper compiler optimizer. It should return Chr(0) */
   TEST_LINE( Chr( 256.0 )                    , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 256.1 )                    , ""+Chr(0)+""                           )
   TEST_LINE( Chr( ( 256 ) )                  , ""+Chr(0)+""                           ) /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   TEST_LINE( Chr( 257 )                      , ""                                    )
   TEST_LINE( Chr( ( 512 ) )                  , ""+Chr(0)+""                           ) /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   TEST_LINE( Chr( 1023 )                     , "ÿ"                                    )
   TEST_LINE( Chr( ( 1024 ) )                 , ""+Chr(0)+""                           ) /* Double paranthesis should be used here to avoid the optimizer of the CA-Cl*pper compiler */
   TEST_LINE( Chr( 1025 )                     , ""                                    )
   TEST_LINE( Chr( 1000 )                     , "è"                                    )
   TEST_LINE( Chr( 100000 )                   , " "                                    )
   TEST_LINE( Chr( 100000.0 )                 , " "                                    )
#ifdef __HARBOUR__
   /* enable Harbour extensions and test correct results results */
   #pragma -kh+
   TEST_LINE( Chr( 0 )                        , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 0.0 )                      , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 0.1 )                      , ""+Chr(0)+""                           )
   TEST_LINE( Chr( -0.1 )                     , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 66.4 )                     , "B"                                    )
   TEST_LINE( Chr( 66.5 )                     , "B"                                    )
   TEST_LINE( Chr( 66.6 )                     , "B"                                    )
   TEST_LINE( Chr( 255 )                      , "ÿ"                                    )
   TEST_LINE( Chr( 256 )                      , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 256.0 )                    , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 256.1 )                    , ""+Chr(0)+""                           )
   TEST_LINE( Chr( ( 256 ) )                  , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 257 )                      , ""                                    )
   TEST_LINE( Chr( ( 512 ) )                  , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 1023 )                     , "ÿ"                                    )
   TEST_LINE( Chr( ( 1024 ) )                 , ""+Chr(0)+""                           )
   TEST_LINE( Chr( 1025 )                     , ""                                    )
   TEST_LINE( Chr( 1000 )                     , "è"                                    )
   TEST_LINE( Chr( 100000 )                   , " "                                    )
   TEST_LINE( Chr( 100000.0 )                 , " "                                    )
#endif

   /* ASC() */

   TEST_LINE( Asc( NIL )                      , "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( Asc( 100 )                      , "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( Asc( 20000 )                    , "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:N:20000 F:S" )
   TEST_LINE( Asc( "HELLO" )                  , 72                                   )
   TEST_LINE( Asc( Chr(0) )                   , 0                                    )
   TEST_LINE( Asc( "a" )                      , 97                                   )
   TEST_LINE( Asc( "A" )                      , 65                                   )
   TEST_LINE( Asc( scString )                 , 72                                   )
#ifdef __HARBOUR__
   TEST_LINE( Asc( @scString )                , 72                                   ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1107 Argument error (ASC) OS:0 #:0 A:1:U:HELLO F:S" */
#endif

   /* ISDIGIT() */

#ifndef __XPP__
   TEST_LINE( IsDigit()                       , .F.              )
#endif
   TEST_LINE( IsDigit( 100 )                  , .F.              )
#ifdef __HARBOUR__
   TEST_LINE( IsDigit( @scString )            , .F.              ) /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   TEST_LINE( IsDigit( "" )                   , .F.              )
   TEST_LINE( IsDigit( "A" )                  , .F.              )
   TEST_LINE( IsDigit( "AA" )                 , .F.              )
   TEST_LINE( IsDigit( "-" )                  , .F.              )
   TEST_LINE( IsDigit( "." )                  , .F.              )
   TEST_LINE( IsDigit( "0" )                  , .T.              )
   TEST_LINE( IsDigit( "9" )                  , .T.              )
   TEST_LINE( IsDigit( "123" )                , .T.              )
   TEST_LINE( IsDigit( "1" )                  , .T.              )
   TEST_LINE( IsDigit( "A1" )                 , .F.              )
   TEST_LINE( IsDigit( "1A" )                 , .T.              )

   /* ISALPHA() */

#ifndef __XPP__
   TEST_LINE( IsAlpha()                       , .F.              )
#endif
   TEST_LINE( IsAlpha( 100 )                  , .F.              )
#ifdef __HARBOUR__
   TEST_LINE( IsAlpha( @scString )            , .T.              ) /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   TEST_LINE( IsAlpha( "" )                   , .F.              )
   TEST_LINE( IsAlpha( "A" )                  , .T.              )
   TEST_LINE( IsAlpha( "AA" )                 , .T.              )
   TEST_LINE( IsAlpha( "-" )                  , .F.              )
   TEST_LINE( IsAlpha( "." )                  , .F.              )
   TEST_LINE( IsAlpha( "0" )                  , .F.              )
   TEST_LINE( IsAlpha( "9" )                  , .F.              )
   TEST_LINE( IsAlpha( "123" )                , .F.              )
   TEST_LINE( IsAlpha( "1" )                  , .F.              )
   TEST_LINE( IsAlpha( "A" )                  , .T.              )
   TEST_LINE( IsAlpha( "A1" )                 , .T.              )
   TEST_LINE( IsAlpha( "aa" )                 , .T.              )
   TEST_LINE( IsAlpha( "za" )                 , .T.              )
   TEST_LINE( IsAlpha( "Aa" )                 , .T.              )
   TEST_LINE( IsAlpha( "Za" )                 , .T.              )
   TEST_LINE( IsAlpha( "@"  )                 , .F.              )
   TEST_LINE( IsAlpha( "["  )                 , .F.              )
   TEST_LINE( IsAlpha( "`"  )                 , .F.              )
   TEST_LINE( IsAlpha( "{"  )                 , .F.              )

   /* ISUPPER() */

#ifndef __XPP__
   TEST_LINE( IsUpper()                       , .F.              )
#endif
   TEST_LINE( IsUpper( 100 )                  , .F.              )
#ifdef __HARBOUR__
   TEST_LINE( IsUpper( @scString )            , .T.              ) /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   TEST_LINE( IsUpper( "" )                   , .F.              )
   TEST_LINE( IsUpper( "6" )                  , .F.              )
   TEST_LINE( IsUpper( "A" )                  , .T.              )
   TEST_LINE( IsUpper( "AA" )                 , .T.              )
   TEST_LINE( IsUpper( "a" )                  , .F.              )
   TEST_LINE( IsUpper( "K" )                  , .T.              )
   TEST_LINE( IsUpper( "Z" )                  , .T.              )
   TEST_LINE( IsUpper( "z" )                  , .F.              )
   TEST_LINE( IsUpper( "™" )                  , .F.              )
   TEST_LINE( IsUpper( "”" )                  , .F.              )

   /* ISLOWER() */

#ifndef __XPP__
   TEST_LINE( IsLower()                       , .F.              )
#endif
   TEST_LINE( IsLower( 100 )                  , .F.              )
#ifdef __HARBOUR__
   TEST_LINE( IsLower( @scString )            , .F.              ) /* Bug in CA-Cl*pper, it will always return .F. */
#endif
   TEST_LINE( IsLower( "" )                   , .F.              )
   TEST_LINE( IsLower( "6" )                  , .F.              )
   TEST_LINE( IsLower( "A" )                  , .F.              )
   TEST_LINE( IsLower( "AA" )                 , .F.              )
   TEST_LINE( IsLower( "a" )                  , .T.              )
   TEST_LINE( IsLower( "K" )                  , .F.              )
   TEST_LINE( IsLower( "Z" )                  , .F.              )
   TEST_LINE( IsLower( "z" )                  , .T.              )
   TEST_LINE( IsLower( "™" )                  , .F.              )
   TEST_LINE( IsLower( "”" )                  , .F.              )

   /* ALLTRIM() */

#ifdef HB_COMPAT_C53
   /* These lines will cause CA-Cl*pper 5.2e to trash memory and later crash, it was fixed in 5.3 */
   TEST_LINE( AllTrim( NIL )                  , "E 1 BASE 2022 Argument error (ALLTRIM) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( AllTrim( 100 )                  , "E 1 BASE 2022 Argument error (ALLTRIM) OS:0 #:0 A:1:N:100 F:S" )
#endif
#ifdef __HARBOUR__
   TEST_LINE( AllTrim(@scString)              , "HELLO"          ) /* CA-Cl*pper bug, it will terminate the program on this line. */
#endif
   TEST_LINE( AllTrim(scString)               , "HELLO"          )
   TEST_LINE( AllTrim("HELLO")                , "HELLO"          )
   TEST_LINE( AllTrim( "" )                   , ""               )
   TEST_LINE( AllTrim( "UA   " )              , "UA"             )
   TEST_LINE( AllTrim( "   UA" )              , "UA"             )
   TEST_LINE( AllTrim( "   UA  " )            , "UA"             )
   TEST_LINE( AllTrim( " "+Chr(0)+" UA  " )   , ""+Chr(0)+" UA"  )
   TEST_LINE( AllTrim( " "+Chr(9)+" UA  " )   , "UA"             )
   TEST_LINE( AllTrim( " "+Chr(9)+"U"+Chr(9)) , "U"+Chr(9)+""    )
   TEST_LINE( AllTrim( " "+Chr(9)+Chr(9))     , ""               )
   TEST_LINE( AllTrim( Chr(10)+"U"+Chr(10))   , "U"+Chr(10)+""   )
   TEST_LINE( AllTrim( Chr(13)+"U"+Chr(13))   , "U"+Chr(13)+""   )
   TEST_LINE( AllTrim( "A"+Chr(10))           , "A"+Chr(10)+""   )
   TEST_LINE( AllTrim( "A"+Chr(13))           , "A"+Chr(13)+""   )
   TEST_LINE( AllTrim( "  "+Chr(0)+"ABC"+Chr(0)+"  "), ""+Chr(0)+"ABC"+Chr(0)+"" )

   /* TRIM() */

   TEST_LINE( Trim( 100 )                     , "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( Trim( NIL )                     , "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:U:NIL F:S" )
#ifdef __HARBOUR__
   TEST_LINE( Trim(@scString)                 , "HELLO"                   ) /* CA-Cl*pper bug, it will throw an error here. */
#endif
   TEST_LINE( Trim(scString)                  , "HELLO"                   )
   TEST_LINE( Trim("HELLO")                   , "HELLO"                   )
   TEST_LINE( Trim( "" )                      , ""                        )
   TEST_LINE( Trim( "UA   " )                 , "UA"                      )
   TEST_LINE( Trim( "   UA" )                 , "   UA"                   )
   TEST_LINE( Trim( "   UA  " )               , "   UA"                   )
   TEST_LINE( Trim( " "+Chr(0)+" UA  " )      , " "+Chr(0)+" UA"          )
   TEST_LINE( Trim( " "+Chr(9)+" UA  " )      , " "+Chr(9)+" UA"          )
   TEST_LINE( Trim( " "+Chr(9)+"U"+Chr(9))    , " "+Chr(9)+"U"+Chr(9)+""  )
   TEST_LINE( Trim( " "+Chr(9)+Chr(9))        , " "+Chr(9)+""+Chr(9)+""   )
   TEST_LINE( Trim( Chr(10)+"U"+Chr(10))      , ""+Chr(10)+"U"+Chr(10)+"" )
   TEST_LINE( Trim( Chr(13)+"U"+Chr(13))      , ""+Chr(13)+"U"+Chr(13)+"" )
   TEST_LINE( Trim( "A"+Chr(10))              , "A"+Chr(10)+""            )
   TEST_LINE( Trim( "A"+Chr(13))              , "A"+Chr(13)+""            )
   TEST_LINE( Trim( "  "+Chr(0)+"ABC"+Chr(0)+"  "), "  "+Chr(0)+"ABC"+Chr(0)+"" )

   /* RTRIM() */

   TEST_LINE( RTrim( 100 )                    , "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( RTrim( NIL )                    , "E 1 BASE 1100 Argument error (TRIM) OS:0 #:0 A:1:U:NIL F:S" )
#ifdef __HARBOUR__
   TEST_LINE( RTrim(@scString)                , "HELLO"                   ) /* CA-Cl*pper bug, it will throw an error here. */
#endif
   TEST_LINE( RTrim(scString)                 , "HELLO"                   )
   TEST_LINE( RTrim("HELLO")                  , "HELLO"                   )
   TEST_LINE( RTrim( "" )                     , ""                        )
   TEST_LINE( RTrim( "UA   " )                , "UA"                      )
   TEST_LINE( RTrim( "   UA" )                , "   UA"                   )
   TEST_LINE( RTrim( "   UA  " )              , "   UA"                   )
   TEST_LINE( RTrim( " "+Chr(0)+" UA  " )     , " "+Chr(0)+" UA"          )
   TEST_LINE( RTrim( " "+Chr(9)+" UA  " )     , " "+Chr(9)+" UA"          )
   TEST_LINE( RTrim( " "+Chr(9)+"U"+Chr(9))   , " "+Chr(9)+"U"+Chr(9)+""  )
   TEST_LINE( RTrim( " "+Chr(9)+Chr(9))       , " "+Chr(9)+""+Chr(9)+""   )
   TEST_LINE( RTrim( Chr(10)+"U"+Chr(10))     , ""+Chr(10)+"U"+Chr(10)+"" )
   TEST_LINE( RTrim( Chr(13)+"U"+Chr(13))     , ""+Chr(13)+"U"+Chr(13)+"" )
   TEST_LINE( RTrim( "A"+Chr(10))             , "A"+Chr(10)+""            )
   TEST_LINE( RTrim( "A"+Chr(13))             , "A"+Chr(13)+""            )
   TEST_LINE( RTrim( "  "+Chr(0)+"ABC"+Chr(0)+"  "), "  "+Chr(0)+"ABC"+Chr(0)+"" )

   /* LTRIM() */

   TEST_LINE( LTrim( 100 )                    , "E 1 BASE 1101 Argument error (LTRIM) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( LTrim( NIL )                    , "E 1 BASE 1101 Argument error (LTRIM) OS:0 #:0 A:1:U:NIL F:S" )
#ifdef __HARBOUR__
   TEST_LINE( LTrim(@scString)                , "HELLO"                   ) /* CA-Cl*pper bug, it will throw an error here. */
#endif
   TEST_LINE( LTrim(scString)                 , "HELLO"                   )
   TEST_LINE( LTrim("HELLO")                  , "HELLO"                   )
   TEST_LINE( LTrim( "" )                     , ""                        )
   TEST_LINE( LTrim( "UA   " )                , "UA   "                   )
   TEST_LINE( LTrim( "   UA" )                , "UA"                      )
   TEST_LINE( LTrim( "   UA  " )              , "UA  "                    )
   TEST_LINE( LTrim( " "+Chr(0)+" UA  " )     , ""+Chr(0)+" UA  "         )
   TEST_LINE( LTrim( " "+Chr(9)+" UA  " )     , "UA  "                    )
   TEST_LINE( LTrim( " "+Chr(9)+"U"+Chr(9))   , "U"+Chr(9)+""             )
   TEST_LINE( LTrim( " "+Chr(9)+Chr(9))       , ""                        )
   TEST_LINE( LTrim( Chr(10)+"U"+Chr(10))     , "U"+Chr(10)+""            )
   TEST_LINE( LTrim( Chr(13)+"U"+Chr(13))     , "U"+Chr(13)+""            )
   TEST_LINE( LTrim( "A"+Chr(10))             , "A"+Chr(10)+""            )
   TEST_LINE( LTrim( "A"+Chr(13))             , "A"+Chr(13)+""            )
   TEST_LINE( LTrim( "  "+Chr(0)+"ABC"+Chr(0)+"  "), ""+Chr(0)+"ABC"+Chr(0)+"  " )

   /* STRTRAN() */

   /* TODO: STRTRAN() */

/* NOTE: It seems like CA-Cl*pper 5.x is not aware of the BREAK return value of
         the error handler, so the error is thrown, but we can't catch it.
         This bug is fixed in CA-Cl*pper 5.3 [vszakats] */
#ifndef __CLIPPER__
#ifndef __XPP__
   TEST_LINE( StrTran()                       , "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 F:S" ) /* CA-Cl*pper bug, it will exit on this */
   TEST_LINE( StrTran( NIL )                  , "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:1:U:NIL F:S" ) /* CA-Cl*pper bug, it will exit on this */
   TEST_LINE( StrTran( 100 )                  , "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:1:N:100 F:S" ) /* CA-Cl*pper bug, it will exit on this */
#endif
   TEST_LINE( StrTran( "AA", 1 )              , "E 1 BASE 1126 Argument error (STRTRAN) OS:0 #:0 A:2:C:AA;N:1 F:S" ) /* CA-Cl*pper bug, it will exit on this */
#endif
   TEST_LINE( StrTran( "AA", "A" )            , "" )
   TEST_LINE( StrTran( "AA", "A", "1" )       , "11" )
   TEST_LINE( StrTran( "AA", "A", "1", "2" )  , "11" )

   /* UPPER() */

   TEST_LINE( Upper( scString )               , "HELLO"                                )
#ifdef __HARBOUR__
   TEST_LINE( Upper( @scString )              , "HELLO"                                ) /* Bug in CA-Cl*pper, it will return argument error */
#endif
   TEST_LINE( Upper( 100 )                    , "E 1 BASE 1102 Argument error (UPPER) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( Upper( "" )                     , ""                                     )
   TEST_LINE( Upper( " " )                    , " "                                    )
   TEST_LINE( Upper( "2" )                    , "2"                                    )
   TEST_LINE( Upper( "{" )                    , "{"                                    )
   TEST_LINE( Upper( Chr(0) )                 , ""+Chr(0)+""                           )
   TEST_LINE( Upper( "aAZAZa" )               , "AAZAZA"                               )
   TEST_LINE( Upper( "AazazA" )               , "AAZAZA"                               )
   TEST_LINE( Upper( "Aaz"+Chr(0)+"zA" )      , "AAZ"+Chr(0)+"ZA"                      )
   TEST_LINE( Upper( "z" )                    , "Z"                                    )
   TEST_LINE( Upper( " µ" )                   , " µ"                                   )
   TEST_LINE( Upper( "H rbor 8-) µ" )         , "H RBOR 8-) µ"                         )

   /* LOWER() */

   TEST_LINE( Lower( scString )               , "hello"                                )
#ifdef __HARBOUR__
   TEST_LINE( Lower( @scString )              , "hello"                                ) /* Bug in CA-Cl*pper, it will return argument error */
#endif
   TEST_LINE( Lower( 100 )                    , "E 1 BASE 1103 Argument error (LOWER) OS:0 #:0 A:1:N:100 F:S" )
   TEST_LINE( Lower( "" )                     , ""                                     )
   TEST_LINE( Lower( " " )                    , " "                                    )
   TEST_LINE( Lower( "2" )                    , "2"                                    )
   TEST_LINE( Lower( "{" )                    , "{"                                    )
   TEST_LINE( Lower( Chr(0) )                 , ""+Chr(0)+""                           )
   TEST_LINE( Lower( "aAZAZa" )               , "aazaza"                               )
   TEST_LINE( Lower( "AazazA" )               , "aazaza"                               )
   TEST_LINE( Lower( "Aaz"+Chr(0)+"zA" )      , "aaz"+Chr(0)+"za"                      )
   TEST_LINE( Lower( "z" )                    , "z"                                    )
   TEST_LINE( Lower( " µ" )                   , " µ"                                   )
   TEST_LINE( Lower( "H rbor 8-) µ" )         , "h rbor 8-) µ"                         )

   /* AT() */

   TEST_LINE( At(90, 100)                     , "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:N:90;N:100 F:S" )
   TEST_LINE( At("", 100)                     , "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:C:;N:100 F:S" )
   TEST_LINE( At(100, "")                     , "E 1 BASE 1108 Argument error (AT) OS:0 #:0 A:2:N:100;C: F:S" )
   /* disable Harbour extensions in compiler to replicate Clipper bugs */
#ifdef __HARBOUR__
   #pragma -kh-
#endif
   TEST_LINE( At("", "")                      , 1                ) /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
   TEST_LINE( At("", "ABCDEF")                , 1                ) /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
#ifdef __HARBOUR__
   /* enable Harbour extensions and test correct results results */
   #pragma -kh+
   TEST_LINE( At("", "")                      , 0                ) /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
   TEST_LINE( At("", "ABCDEF")                , 0                ) /* Bug in CA-Cl*ppers compiler optimizer, it should return 0 */
#endif
   TEST_LINE( At(scStringE, scStringE)        , 0                )
   TEST_LINE( At(scStringE, "ABCDEF")         , 0                )
   TEST_LINE( At("ABCDEF", "")                , 0                )
   TEST_LINE( At("AB", "AB")                  , 1                )
   TEST_LINE( At("AB", "AAB")                 , 2                )
   TEST_LINE( At("A", "ABCDEF")               , 1                )
   TEST_LINE( At("F", "ABCDEF")               , 6                )
   TEST_LINE( At("D", "ABCDEF")               , 4                )
   TEST_LINE( At("X", "ABCDEF")               , 0                )
   TEST_LINE( At("AB", "ABCDEF")              , 1                )
   TEST_LINE( At("AA", "ABCDEF")              , 0                )
   TEST_LINE( At("ABCDEF", "ABCDEF")          , 1                )
   TEST_LINE( At("BCDEF", "ABCDEF")           , 2                )
   TEST_LINE( At("BCDEFG", "ABCDEF")          , 0                )
   TEST_LINE( At("ABCDEFG", "ABCDEF")         , 0                )
   TEST_LINE( At("FI", "ABCDEF")              , 0                )

   /* RAT() */

   TEST_LINE( RAt(90, 100)                    , 0                )
   TEST_LINE( RAt("", 100)                    , 0                )
   TEST_LINE( RAt(100, "")                    , 0                )
   TEST_LINE( RAt("", "")                     , 0                )
   TEST_LINE( RAt("", "ABCDEF")               , 0                )
   TEST_LINE( RAt("ABCDEF", "")               , 0                )
   TEST_LINE( RAt("AB", "AB")                 , 1                )
   TEST_LINE( RAt("AB", "AAB")                , 2                )
   TEST_LINE( RAt("AB", "ABAB")               , 3                )
   TEST_LINE( RAt("A", "ABCADEF")             , 4                )
   TEST_LINE( RAt("A", "ABCADEFA")            , 8                )
   TEST_LINE( RAt("A", "ABCDEFA")             , 7                )
   TEST_LINE( RAt("A", "ABCDEF")              , 1                )
   TEST_LINE( RAt("F", "ABCDEF")              , 6                )
   TEST_LINE( RAt("D", "ABCDEF")              , 4                )
   TEST_LINE( RAt("X", "ABCDEF")              , 0                )
   TEST_LINE( RAt("AB", "ABCDEF")             , 1                )
   TEST_LINE( RAt("AA", "ABCDEF")             , 0                )
   TEST_LINE( RAt("ABCDEF", "ABCDEF")         , 1                )
   TEST_LINE( RAt("BCDEF", "ABCDEF")          , 2                )
   TEST_LINE( RAt("BCDEFG", "ABCDEF")         , 0                )
   TEST_LINE( RAt("ABCDEFG", "ABCDEF")        , 0                )
   TEST_LINE( RAt("FI", "ABCDEF")             , 0                )

   /* REPLICATE() */

#ifdef __HARBOUR__
   IF l64
      TEST_LINE( Replicate("XXX", 9000000000000000000) , "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:9000000000000000000 F:S" )
   ELSE
      TEST_LINE( Replicate("XXX", 2000000000)    , "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:2000000000 F:S" )
   ENDIF
#else
   TEST_LINE( Replicate("XXX", 30000)         , "E 3 BASE 1234 String overflow (REPLICATE) OS:0 #:0 A:2:C:XXX;N:30000 F:S" )
#endif
   TEST_LINE( Replicate(200  , 0 )            , "E 1 BASE 1106 Argument error (REPLICATE) OS:0 #:0 A:2:N:200;N:0 F:S" )
   TEST_LINE( Replicate(""   , 10 )           , "" )
   TEST_LINE( Replicate(""   , 0 )            , "" )
   TEST_LINE( Replicate("A"  , "B" )          , "E 1 BASE 1106 Argument error (REPLICATE) OS:0 #:0 A:2:C:A;C:B F:S" )
   TEST_LINE( Replicate("A"  , 1 )            , "A"                                        )
   TEST_LINE( Replicate("A"  , 2 )            , "AA"                                       )
   TEST_LINE( Replicate("HE", 3 )             , "HEHEHE"                                   )
   TEST_LINE( Replicate("HE", 3.1 )           , "HEHEHE"                                   )
   TEST_LINE( Replicate("HE", 3.5 )           , "HEHEHE"                                   )
   TEST_LINE( Replicate("HE", 3.7 )           , "HEHEHE"                                   )
   TEST_LINE( Replicate("HE", -3 )            , "" )
   TEST_LINE( Replicate("H"+Chr(0), 2 )       , "H"+Chr(0)+"H"+Chr(0)+"" )

   /* SPACE() */

   TEST_LINE( Space( "A" )                    , "E 1 BASE 1105 Argument error (SPACE) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( Space( 0 )                      , "" )
   TEST_LINE( Space( -10 )                    , "" )
   TEST_LINE( Space( 10 )                     , "          " )
   TEST_LINE( Space( 10.2 )                   , "          " )
   TEST_LINE( Space( 10.5 )                   , "          " )
   TEST_LINE( Space( 10.7 )                   , "          " )

   /* SUBSTR() */

   TEST_LINE( SubStr(100     , 0, -1)         , "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:N:100;N:0;N:-1 F:S" )
   TEST_LINE( SubStr("abcdef", 1, "a")        , "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:C:abcdef;N:1;C:a F:S" )
   TEST_LINE( SubStr("abcdef", "a")           , "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:2:C:abcdef;C:a F:S" )
   TEST_LINE( SubStr("abcdef", "a", 1)        , "E 1 BASE 1110 Argument error (SUBSTR) OS:0 #:0 A:3:C:abcdef;C:a;N:1 F:S" )
   TEST_LINE( SubStr("abcdef", 0, -1)         , ""               )
   TEST_LINE( SubStr("abcdef", 0, 0)          , ""               )
   TEST_LINE( SubStr("abcdef", 0, 1)          , "a"              )
   TEST_LINE( SubStr("abcdef", 0, 7)          , "abcdef"         )
   TEST_LINE( SubStr("abcdef", 0)             , "abcdef"         )
   TEST_LINE( SubStr("abcdef", 2, -1)         , ""               )
   TEST_LINE( SubStr("abcdef", 2, 0)          , ""               )
   TEST_LINE( SubStr("abcdef", 2, 1)          , "b"              )
   TEST_LINE( SubStr("abcdef", 2, 7)          , "bcdef"          )
   TEST_LINE( SubStr("abcdef", 2)             , "bcdef"          )
#ifndef __XPP__
   TEST_LINE( SubStr("abcdef", -2, -1)        , ""               )
   TEST_LINE( SubStr("abcdef", -2, 0)         , ""               )
   TEST_LINE( SubStr("abcdef", -2, 1)         , "e"              )
   TEST_LINE( SubStr("abcdef", -2, 7)         , "ef"             )
   TEST_LINE( SubStr("abcdef", -2)            , "ef"             )
#endif
   TEST_LINE( SubStr("abcdef", 10, -1)        , ""               )
   TEST_LINE( SubStr("abcdef", 10, 0)         , ""               )
   TEST_LINE( SubStr("abcdef", 10, 1)         , ""               )
   TEST_LINE( SubStr("abcdef", 10, 7)         , ""               )
   TEST_LINE( SubStr("abcdef", 10)            , ""               )
#ifndef __XPP__
   TEST_LINE( SubStr("abcdef", -10, -1)       , ""               )
   TEST_LINE( SubStr("abcdef", -10, 0)        , ""               )
   TEST_LINE( SubStr("abcdef", -10, 1)        , "a"              )
   TEST_LINE( SubStr("abcdef", -10, 7)        , "abcdef"         )
   TEST_LINE( SubStr("abcdef", -10, 15)       , "abcdef"         )
   TEST_LINE( SubStr("abcdef", -10)           , "abcdef"         )
#endif
   TEST_LINE( SubStr("ab" + Chr(0) + "def", 2, 3) , "b" + Chr(0) + "d" )
   TEST_LINE( SubStr("abc" + Chr(0) + "def", 3, 1) , "c" )
   TEST_LINE( SubStr("abc" + Chr(0) + "def", 4, 1) , "" + Chr(0) + "" )
   TEST_LINE( SubStr("abc" + Chr(0) + "def", 5, 1) , "d" )

   /* LEFT() */

   TEST_LINE( Left(100     , -10)                , "E 1 BASE 1124 Argument error (LEFT) OS:0 #:0 A:2:N:100;N:-10 F:S" )
   TEST_LINE( Left("abcdef", "A")                , "E 1 BASE 1124 Argument error (LEFT) OS:0 #:0 A:2:C:abcdef;C:A F:S" )
   TEST_LINE( Left("abcdef", -10)                , ""               )
   TEST_LINE( Left("abcdef", -2)                 , ""               )
   TEST_LINE( Left("abcdef", 0)                  , ""               )
   TEST_LINE( Left("abcdef", 2)                  , "ab"             )
   TEST_LINE( Left("abcdef", 10)                 , "abcdef"         )
   TEST_LINE( Left("ab" + Chr(0) + "def", 5)     , "ab" + Chr(0) + "de" )

   /* RIGHT() */

   TEST_LINE( Right(100     , -10)               , ""               )
   TEST_LINE( Right("abcdef", "A")               , ""               )
   TEST_LINE( Right("abcdef", -10)               , ""               )
   TEST_LINE( Right("abcdef", -2)                , ""               )
   TEST_LINE( Right("abcdef", 0)                 , ""               )
   TEST_LINE( Right("abcdef", 2)                 , "ef"             )
   TEST_LINE( Right("abcdef", 10)                , "abcdef"         )
   TEST_LINE( Right("ab" + Chr(0) + "def", 5)    , "b" + Chr(0) + "def" )

   /* PADR() */

   TEST_LINE( Pad(NIL, 5)                        , ""               )
   TEST_LINE( Pad(.T., 5)                        , ""               )
   TEST_LINE( Pad(10, 5)                         , "10   "          )
   TEST_LINE( Pad(10.2, 5)                       , "10.2 "          )
   TEST_LINE( Pad(100000, 8)                     , "100000  "       )
   TEST_LINE( Pad(100000, 8, "-")                , "100000--"       )
   TEST_LINE( Pad(-100000, 8, "-")               , "-100000-"       )
   TEST_LINE( Pad(5000000000, 15)                , "5000000000     ")
   TEST_LINE( Pad(HB_SToD("19840325"), 12)       , "1984.03.25  "   )
   TEST_LINE( Pad(Year(HB_SToD("19840325")), 5)  , "1984 "          )
   TEST_LINE( Pad(Day(HB_SToD("19840325")), 5)   , "25   "          )
#ifdef __HARBOUR__
   TEST_LINE( Pad(@scString, 10)                 , "HELLO     "     ) /* Bug in CA-Cl*pper, it will return "" */
   TEST_LINE( Pad(scString, @snIntP)             , "HELLO     "     ) /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   TEST_LINE( Pad("abcdef", "A")                 , ""               )
   TEST_LINE( Pad("abcdef", -5)                  , ""               )
#endif
   TEST_LINE( Pad("abcdef", 0)                   , ""               )
   TEST_LINE( Pad("abcdef", 5)                   , "abcde"          )
   TEST_LINE( Pad("abcdef", 10)                  , "abcdef    "     )
   TEST_LINE( Pad("abcdef", 10, "")              , "abcdef"+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( Pad("abcdef", 10, "1")             , "abcdef1111"     )
   TEST_LINE( Pad("abcdef", 10, "12")            , "abcdef1111"     )

   /* PADR() */

   TEST_LINE( PadR(NIL, 5)                       , ""               )
   TEST_LINE( PadR(.T., 5)                       , ""               )
   TEST_LINE( PadR(10, 5)                        , "10   "          )
   TEST_LINE( PadR(10.2, 5)                      , "10.2 "          )
   TEST_LINE( PadR(100000, 8)                    , "100000  "       )
   TEST_LINE( PadR(100000, 8, "-")               , "100000--"       )
   TEST_LINE( PadR(-100000, 8, "-")              , "-100000-"       )
   TEST_LINE( PadR(HB_SToD("19840325"), 12)      , "1984.03.25  "   )
   TEST_LINE( PadR(Year(HB_SToD("19840325")), 5) , "1984 "          )
   TEST_LINE( PadR(Day(HB_SToD("19840325")), 5)  , "25   "          )
#ifdef __HARBOUR__
   TEST_LINE( PadR(@scString, 10)                , "HELLO     "     ) /* Bug in CA-Cl*pper, it will return "" */
   TEST_LINE( PadR(scString, @snIntP)            , "HELLO     "     ) /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   TEST_LINE( PadR("abcdef", "A")                , ""               )
   TEST_LINE( PadR("abcdef", -5)                 , ""               )
#endif
   TEST_LINE( PadR("abcdef", 0)                  , ""               )
   TEST_LINE( PadR("abcdef", 5)                  , "abcde"          )
   TEST_LINE( PadR("abcdef", 10)                 , "abcdef    "     )
   TEST_LINE( PadR("abcdef", 10, "")             , "abcdef"+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( PadR("abcdef", 10, "1")            , "abcdef1111"     )
   TEST_LINE( PadR("abcdef", 10, "12")           , "abcdef1111"     )

   /* PADL() */

   TEST_LINE( PadL(NIL, 5)                       , ""               )
   TEST_LINE( PadL(.T., 5)                       , ""               )
   TEST_LINE( PadL(10, 5)                        , "   10"          )
   TEST_LINE( PadL(10.2, 5)                      , " 10.2"          )
   TEST_LINE( PadL(100000, 8)                    , "  100000"       )
   TEST_LINE( PadL(100000, 8, "-")               , "--100000"       )
   TEST_LINE( PadL(-100000, 8, "-")              , "--100000"       )
   TEST_LINE( PadL(HB_SToD("19840325"), 12)      , "  1984.03.25"   )
   TEST_LINE( PadL(Year(HB_SToD("19840325")), 5) , " 1984"          )
   TEST_LINE( PadL(Day(HB_SToD("19840325")), 5)  , "   25"          )
#ifdef __HARBOUR__
   TEST_LINE( PadL(@scString, 10)                , "     HELLO"     ) /* Bug in CA-Cl*pper, it will return "" */
   TEST_LINE( PadL(scString, @snIntP)            , "     HELLO"     ) /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   TEST_LINE( PadL("abcdef", "A")                , ""               )
   TEST_LINE( PadL("abcdef", -5)                 , ""               )
#endif
   TEST_LINE( PadL("abcdef", 0)                  , ""               )
   TEST_LINE( PadL("abcdef", 5)                  , "abcde"          ) /* QUESTION: CA-Cl*pper "bug", should return: "bcdef" ? */
   TEST_LINE( PadL("abcdef", 10)                 , "    abcdef"     )
   TEST_LINE( PadL("abcdef", 10, "")             , ""+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+"abcdef" )
   TEST_LINE( PadL("abcdef", 10, "1")            , "1111abcdef"     )
   TEST_LINE( PadL("abcdef", 10, "12")           , "1111abcdef"     )

   /* PADC() */

   TEST_LINE( PadC(NIL, 5)                       , ""               )
   TEST_LINE( PadC(.T., 5)                       , ""               )
   TEST_LINE( PadC(10, 5)                        , " 10  "          )
   TEST_LINE( PadC(10.2, 5)                      , "10.2 "          )
   TEST_LINE( PadC(100000, 8)                    , " 100000 "       )
   TEST_LINE( PadC(100000, 8, "-")               , "-100000-"       )
   TEST_LINE( PadC(-100000, 8, "-")              , "-100000-"       )
   TEST_LINE( PadC(HB_SToD("19840325"), 12)      , " 1984.03.25 "   )
   TEST_LINE( PadC(Year(HB_SToD("19840325")), 5) , "1984 "          )
   TEST_LINE( PadC(Day(HB_SToD("19840325")), 5)  , " 25  "          )
#ifdef __HARBOUR__
   TEST_LINE( PadC(@scString, 10)                , "  HELLO   "     ) /* Bug in CA-Cl*pper, it will return "" */
   TEST_LINE( PadC(scString, @snIntP)            , "  HELLO   "     ) /* Bug in CA-Cl*pper, it will return "" */
#endif
#ifndef __XPP__
   TEST_LINE( PadC("abcdef", "A")                , ""               )
   TEST_LINE( PadC("abcdef", -5)                 , ""               )
#endif
   TEST_LINE( PadC("abcdef", 0)                  , ""               )
   TEST_LINE( PadC("abcdef", 2)                  , "ab"             ) /* QUESTION: CA-Cl*pper "bug", should return: "cd" ? */
   TEST_LINE( PadC("abcdef", 5)                  , "abcde"          )
   TEST_LINE( PadC("abcdef", 10)                 , "  abcdef  "     )
   TEST_LINE( PadC("abcdef", 10, "")             , ""+Chr(0)+""+Chr(0)+"abcdef"+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( PadC("abcdef", 10, "1")            , "11abcdef11"     )
   TEST_LINE( PadC("abcdef", 10, "12")           , "11abcdef11"     )

   /* STUFF() */

#ifndef __XPP__
   TEST_LINE( Stuff()                                          , ""                        )
   TEST_LINE( Stuff( 100 )                                     , ""                        )
#endif
   TEST_LINE( Stuff("ABCDEF", -6, -5, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -6, -2, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -6,  0, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -6, 10, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -6, 30, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -2, -5, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -2, -2, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -2,  0, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -2, 10, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", -2, 30, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF",  0, -5, NIL)                     , ""                        )
   TEST_LINE( Stuff("ABCDEF",  0, -2, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  0,  0, "xyz")                   , "xyzABCDEF"               )
   TEST_LINE( Stuff("ABCDEF",  0, 10, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  0, 30, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  1, -5, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  1, -2, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  1,  0, "xyz")                   , "xyzABCDEF"               )
   TEST_LINE( Stuff("ABCDEF",  1, 10, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  1, 30, "xyz")                   , "xyz"                     )
   TEST_LINE( Stuff("ABCDEF",  2,  0, "xyz")                   , "AxyzBCDEF"               )
   TEST_LINE( Stuff("ABCDEF",  2,  3, ""   )                   , "AEF"                     )
   TEST_LINE( Stuff("ABCDEF",  2,  3, "xyz")                   , "AxyzEF"                  )
   TEST_LINE( Stuff("ABCDEF",  2,  2, "")                      , "ADEF"                    )
   TEST_LINE( Stuff("ABCDEF",  2, -5, "xyz")                   , "Axyz"                    )
   TEST_LINE( Stuff("ABCDEF",  2, -2, "xyz")                   , "Axyz"                    )
   TEST_LINE( Stuff("ABCDEF",  2,  1, "xyz")                   , "AxyzCDEF"                )
   TEST_LINE( Stuff("ABCDEF",  2,  4, "xyz")                   , "AxyzF"                   )
   TEST_LINE( Stuff("ABCDEF",  2, 10, "xyz")                   , "Axyz"                    )
   TEST_LINE( Stuff("ABCDEF",  2, 30, "xyz")                   , "Axyz"                    )
   TEST_LINE( Stuff("ABCDEF", 30, -5, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", 30, -2, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", 30,  0, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", 30, 10, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff("ABCDEF", 30, 30, "xyz")                   , "ABCDEFxyz"               )
   TEST_LINE( Stuff(@scString        ,  2,  3, "xyz")          , "HxyzO"                   )
   TEST_LINE( Stuff("ABC"+Chr(0)+"EF",  2,  3, "xyz")          , "AxyzEF"                  )
   TEST_LINE( Stuff("ABCE"+Chr(0)+"F",  2,  3, "xyz")          , "Axyz"+Chr(0)+"F"         )
   TEST_LINE( Stuff("ABC"+Chr(0)+"EF",  2,  3, "x"+Chr(0)+"z") , "Ax"+Chr(0)+"zEF"         )

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
