/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (HVM)
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
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

FUNCTION Main_HVMA()
   LOCAL nA, nB, nC

   // NOTE: These are compiler tests.
   //       The expressions have to be written with no separators!
   TEST_LINE( mnIntP==10.OR.mnIntP==0         , .T.                                               )
   TEST_LINE( mnIntP==10.AND.mnLongP==0       , .F.                                               )

   TEST_LINE( NIL + 1                         , "E BASE 1081 Argument error + A:2:U:NIL;N:1 F:S" )
   TEST_LINE( NIL - 1                         , "E BASE 1082 Argument error - A:2:U:NIL;N:1 F:S" )

   TEST_LINE( scString + NIL                  , "E BASE 1081 Argument error + A:2:C:HELLO;U:NIL F:S" )
   TEST_LINE( scString - NIL                  , "E BASE 1082 Argument error - A:2:C:HELLO;U:NIL F:S" )

   TEST_LINE( 1 + NIL                         , "E BASE 1081 Argument error + A:2:N:1;U:NIL F:S" )
   TEST_LINE( 1 - NIL                         , "E BASE 1082 Argument error - A:2:N:1;U:NIL F:S" )

   TEST_LINE( "A" - "B"                       , "AB"                               )
   TEST_LINE( "A  " - "B"                     , "AB  "                             )
   TEST_LINE( "A  " - "B "                    , "AB   "                            )
   TEST_LINE( "A  " - " B"                    , "A B  "                            )
   TEST_LINE( "   " - "B "                    , "B    "                            )

   TEST_LINE( 1 / 0                           , "E BASE 1340 Zero divisor / A:2:N:1;N:0 F:S"     )
   TEST_LINE( 1 / NIL                         , "E BASE 1084 Argument error / A:2:N:1;U:NIL F:S" )
   TEST_LINE( 1 * NIL                         , "E BASE 1083 Argument error * A:2:N:1;U:NIL F:S" )
   TEST_LINE( 1 ** NIL                        , "E BASE 1088 Argument error ^ A:2:N:1;U:NIL F:S" )
   TEST_LINE( 1 ^ NIL                         , "E BASE 1088 Argument error ^ A:2:N:1;U:NIL F:S" )
   TEST_LINE( 1 % 0                           , "E BASE 1341 Zero divisor % A:2:N:1;N:0 F:S"     )
   TEST_LINE( 1 % NIL                         , "E BASE 1085 Argument error % A:2:N:1;U:NIL F:S" )

   TEST_LINE( Str( 1 / 0   )                  , "E BASE 1340 Zero divisor / A:2:N:1;N:0 F:S" )
   TEST_LINE( Str( 2 / 4   )                  , "         0.50"                    )
   TEST_LINE( Str( 4 / 2   )                  , "         2"                       )
   TEST_LINE( Str( 4 / 2.0 )                  , "         2.00"                    )
   TEST_LINE( Str( 1 * 0   )                  , "         0"                       )
   TEST_LINE( Str( 2 * 4   )                  , "         8"                       )
   TEST_LINE( Str( 4 * 2.0 )                  , "         8.0"                     )
   TEST_LINE( Str( 2 * 0.5 )                  , "         1.0"                     )
   TEST_LINE( Str( 1 + 0   )                  , "         1"                       )
   TEST_LINE( Str( 2 + 4   )                  , "         6"                       )
   TEST_LINE( Str( 4 + 2.0 )                  , "         6.0"                     )
   TEST_LINE( Str( 2 + 0.5 )                  , "         2.5"                     )
   TEST_LINE( Str( 1 - 0   )                  , "         1"                       )
   TEST_LINE( Str( 2 - 4   )                  , "        -2"                       )
   TEST_LINE( Str( 4 - 2.0 )                  , "         2.0"                     )
   TEST_LINE( Str( 2 - 0.5 )                  , "         1.5"                     )
   TEST_LINE( Str( 1 % 0   )                  , "E BASE 1341 Zero divisor % A:2:N:1;N:0 F:S" )
   TEST_LINE( Str( 2 % 4   )                  , "         2"                       )
   TEST_LINE( Str( 4 % 2   )                  , "         0"                       )
   TEST_LINE( Str( 4 % 2.0 )                  , "         0.00"                    )
   TEST_LINE( Str( 2 % 4.0 )                  , "         2.00"                    )

   TEST_LINE( Str(  3 %  3 )                   , "         0"                      )
   TEST_LINE( Str(  3 %  2 )                   , "         1"                      )
   TEST_LINE( Str(  3 %  1 )                   , "         0"                      )
   TEST_LINE( Str(  3 %  0 )                   , "E BASE 1341 Zero divisor % A:2:N:3;N:0 F:S" )
   TEST_LINE( Str(  3 % -1 )                   , "         0"                      )
   TEST_LINE( Str(  3 % -2 )                   , "         1"                      )
   TEST_LINE( Str(  3 % -3 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  3 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  2 )                   , "        -1"                      )
   TEST_LINE( Str( -3 %  1 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  0 )                   , "E BASE 1341 Zero divisor % A:2:N:-3;N:0 F:S" )
   TEST_LINE( Str( -3 % -1 )                   , "         0"                      )
   TEST_LINE( Str( -3 % -2 )                   , "        -1"                      )
   TEST_LINE( Str( -3 % -3 )                   , "         0"                      )
   TEST_LINE( Str(  3 %  3 )                   , "         0"                      )
   TEST_LINE( Str(  2 %  3 )                   , "         2"                      )
   TEST_LINE( Str(  1 %  3 )                   , "         1"                      )
   TEST_LINE( Str(  0 %  3 )                   , "         0"                      )
   TEST_LINE( Str( -1 %  3 )                   , "        -1"                      )
   TEST_LINE( Str( -2 %  3 )                   , "        -2"                      )
   TEST_LINE( Str( -3 %  3 )                   , "         0"                      )
   TEST_LINE( Str(  3 % -3 )                   , "         0"                      )
   TEST_LINE( Str(  2 % -3 )                   , "         2"                      )
   TEST_LINE( Str(  1 % -3 )                   , "         1"                      )
   TEST_LINE( Str(  0 % -3 )                   , "         0"                      )
   TEST_LINE( Str( -1 % -3 )                   , "        -1"                      )
   TEST_LINE( Str( -2 % -3 )                   , "        -2"                      )
   TEST_LINE( Str( -3 % -3 )                   , "         0"                      )

   /* The order of these tests is relevant, don't change it */

   nA := 1
   nB := 2
   nC := 3

   TEST_LINE( nA                              , 1 )
   TEST_LINE( nB                              , 2 )
   TEST_LINE( nC                              , 3 )

   TEST_LINE( nA + nB                         , 3 )
   TEST_LINE( nB - nA                         , 1 )
   TEST_LINE( nB * nC                         , 6 )
   TEST_LINE( nB * nC / 2                     , 3 )
   TEST_LINE( nA += nB                        , 3 )
   TEST_LINE( nA                              , 3 )
   TEST_LINE( nA -= nB                        , 1 )
   TEST_LINE( nA                              , 1 )
   TEST_LINE( nA < nB                         , .T. )
   TEST_LINE( nA > nB                         , .F. )
   TEST_LINE( nA + nB <= nC                   , .T. )
   TEST_LINE( nA + nB >= nC                   , .T. )
   TEST_LINE( nA *= nB                        , 2 )
   TEST_LINE( nA /= nB                        , 1 )
   TEST_LINE( nA                              , 1 )
   TEST_LINE( nB ** 3                         , 8 )
   TEST_LINE( nB ^ 3                          , 8 )
   TEST_LINE( 8 % 3                           , 2 )
   TEST_LINE( nA++                            , 1 )
   TEST_LINE( nA                              , 2 )
   TEST_LINE( ++nA                            , 3 )
   TEST_LINE( nA                              , 3 )
   TEST_LINE( nA--                            , 3 )
   TEST_LINE( nA                              , 2 )
   TEST_LINE( --nA                            , 1 )
   TEST_LINE( nA                              , 1 )

/* These will generate warnings and errors with Harbour */
#ifndef __HARBOUR__
   TEST_LINE( { 1, 2 }[ ( 2, 1 ) ]++          , 1 )
   TEST_LINE( ++{ 1, 2 }[ ( 2, 1 ) ]          , 2 )
   TEST_LINE( { 1, 2 }[ 1 ]++                 , 1 )
   TEST_LINE( ++{ 1, 2 }[ 1 ]                 , 2 )
#ifndef __XPP__
   TEST_LINE( ({ 1, 2 }[ 1 ])++               , 1 )
   TEST_LINE( ++({ 1, 2 }[ 1 ])               , 2 )
#endif
#endif

   /* Operator precedence */

   TEST_LINE( 1 + 2 * 3 / 4 - 2 ** 2 ^ 3      , -61.50 )
   TEST_LINE( 1 + 2 * 3 / 4 - 2 ** 2 ^ 3 == 2 , .F. )

   /* */

   TEST_LINE( -Month(sdDate)                  , -3                                 )
   TEST_LINE( Str(-(Month(sdDate)))           , "        -3"                       )
   TEST_LINE( Str(-(Val("10")))               , "       -10"                       )
   TEST_LINE( Str(-(Val("100000")))           , "   -100000"                       )
   TEST_LINE( Str(-(Val("20.876")))           , "       -20.876"                   )
   TEST_LINE( -(0)                            , 0                                  )
   TEST_LINE( -(10)                           , -10                                )
   TEST_LINE( -(10.505)                       , -10.505                            )
   TEST_LINE( -(100000)                       , -100000                            )
   TEST_LINE( -(-10)                          , 10                                 )
   TEST_LINE( -("1")                          , "E BASE 1080 Argument error - A:1:C:1 F:S"         )

   TEST_LINE( "AA" $ 1                        , "E BASE 1109 Argument error $ A:2:C:AA;N:1 F:S"    )
   TEST_LINE( scString $ 1                    , "E BASE 1109 Argument error $ A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( 1 $ "AA"                        , "E BASE 1109 Argument error $ A:2:N:1;C:AA F:S"    )

   TEST_LINE( !   scStringE $ "XE"            , .T. )
   TEST_LINE( ! ( scStringE $ "XE" )          , .T. )
   TEST_LINE(     scStringE $ "XE"            , .F. )
   TEST_LINE( !   "X" $ "XE"                  , .F. )
   TEST_LINE( ! ( "X" $ "XE" )                , .F. )
   TEST_LINE(     "X" $ "XE"                  , .T. )
   TEST_LINE(     "X" $ Chr(0) + "X"          , .T. )
   TEST_LINE( ( "X" ) $ Chr(0) + "X"          , .T. )
   TEST_LINE( scString $ Chr(0) + scString    , .T. )

   TEST_LINE( scStringE $ "bcde"              , .F. )
   TEST_LINE( "" $ "bcde"                     , .T. ) /* Bug in CA-Cl*ppers compiler optimizer. It should return .F. */
   TEST_LINE( "d" $ "bcde"                    , .T. )
   TEST_LINE( "D" $ "BCDE"                    , .T. )
   TEST_LINE( "a" $ "bcde"                    , .F. )
   TEST_LINE( "d" $ "BCDE"                    , .F. )
   TEST_LINE( "D" $ "bcde"                    , .F. )
   TEST_LINE( "de" $ "bcde"                   , .T. )
   TEST_LINE( "bd" $ "bcde"                   , .F. )
   TEST_LINE( "BD" $ "bcde"                   , .F. )

#ifndef __XPP__

   IF TEST_OPT_Z()

   /* With the shortcut optimization *ON* */

   TEST_LINE( 1 .AND. 2                       , "E BASE 1066 Argument error conditional A:1:N:1 "     )
   TEST_LINE( NIL .AND. NIL                   , "E BASE 1066 Argument error conditional A:1:U:NIL "   )
   TEST_LINE( scString .AND. scString         , "E BASE 1066 Argument error conditional A:1:C:HELLO " )
   TEST_LINE( .T. .AND. 1                     , 1                                         )
   TEST_LINE( .T. .AND. 1.567                 , 1.567                                     )
   TEST_LINE( .T. .AND. scString              , "HELLO"                                   )
   TEST_LINE( .T. .AND. HB_SToD("")           , HB_SToD("        ")                       )
   TEST_LINE( .T. .AND. NIL                   , NIL                                       )
   TEST_LINE( .T. .AND. {}                    , "{.[0].}"                                 )
   TEST_LINE( .T. .AND. {|| NIL }             , "{||...}"                                 )
   TEST_LINE( .F. .AND. 1                     , .F.                                       )
   TEST_LINE( .F. .AND. 1.567                 , .F.                                       )
   TEST_LINE( .F. .AND. scString              , .F.                                       )
   TEST_LINE( .F. .AND. HB_SToD("")           , .F.                                       )
   TEST_LINE( .F. .AND. NIL                   , .F.                                       )
   TEST_LINE( .F. .AND. {}                    , .F.                                       )
   TEST_LINE( .F. .AND. {|| NIL }             , .F.                                       )
   TEST_LINE( 1 .AND. .F.                     , .F.                                       )
   TEST_LINE( 1.567 .AND. .F.                 , .F.                                       )
   TEST_LINE( scString .AND. .F.              , .F.                                       )

   TEST_LINE( 1 .OR. 2                        , "E BASE 1066 Argument error conditional A:1:N:1 " )
   TEST_LINE( .F. .OR. 2                      , 2                                         )
   TEST_LINE( .F. .OR. 1.678                  , 1.678                                     )
   TEST_LINE( .F. .OR. scString               , "HELLO"                                   )
   TEST_LINE( .T. .OR. 2                      , .T.                                       )
   TEST_LINE( .T. .OR. 1.678                  , .T.                                       )
   TEST_LINE( .T. .OR. scString               , .T.                                       )
   TEST_LINE( 1 .OR. .F.                      , 1                                         )
   TEST_LINE( 1.0 .OR. .F.                    , 1.0                                       )
   TEST_LINE( scString .OR. .F.               , "HELLO"                                   )

   ELSE

   /* With the shortcut optimization *OFF* (/z switch) */

   TEST_LINE( 1 .AND. 2                       , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( NIL .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( scString .AND. scString         , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. 1                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. 1.567                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. scString              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. HB_SToD("")           , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {}                    , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {|| NIL }             , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1.567                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. scString              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. HB_SToD("")           , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. {}                    , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. {|| NIL }             , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( 1 .AND. .F.                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( 1.567 .AND. .F.                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( scString .AND. .F.              , "E BASE 1078 Argument error .AND. F:S"    )

   TEST_LINE( 1 .OR. 2                        , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. 2                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. 1.678                  , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. scString               , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. 2                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. 1.678                  , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. scString               , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( 1 .OR. .F.                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( 1.0 .OR. .F.                    , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( scString .OR. .F.               , "E BASE 1079 Argument error .OR. F:S"     )

   ENDIF

#endif

   TEST_LINE( .NOT. .T.                       , .F.                                       )
   TEST_LINE( .NOT. .F.                       , .T.                                       )
   TEST_LINE( .NOT. 1                         , "E BASE 1077 Argument error .NOT. A:1:N:1 F:S" )

#ifndef __HARBOUR__ // this error is reported at compile time
#ifndef __XPP__ // this error is reported at compile time
   TEST_LINE( iif( "A", ":T:", ":F:" )        , "E BASE 1066 Argument error conditional A:1:C:A " )
#endif
#endif
   TEST_LINE( iif( .T., ":T:", ":F:" )        , ":T:"                                     )
   TEST_LINE( iif( .F., ":T:", ":F:" )        , ":F:"                                     )

   TEST_LINE( scString++                      , "E BASE 1086 Argument error ++ A:1:C:HELLO F:S" )
   TEST_LINE( scString--                      , "E BASE 1087 Argument error -- A:1:C:HELLO F:S" )

   TEST_LINE( mxNotHere                       , "E BASE 1003 Variable does not exist MXNOTHERE F:R" )
#ifdef __HARBOUR__
   TEST_LINE( __MVGET("MXUNDECL")             , "E BASE 1003 Variable does not exist MXUNDECL F:R" )
#else
   mxNotHere ="MXUNDECL"
   TEST_LINE( &mxNotHere.                     , "E BASE 1003 Variable does not exist MXUNDECL F:R" )
#endif

#ifndef __HARBOUR__
   // this error is reported at compile time
   TEST_LINE( saArray[ 0 ]                    , "E BASE 1132 Bound error array access "           )
   TEST_LINE( saArray[ 0 ] := 1               , "E BASE 1133 Bound error array assign "           )
#endif
   TEST_LINE( saArray[ 1000 ]                 , "E BASE 1132 Bound error array access "           )
   TEST_LINE( saArray[ 1000 ] := 1            , "E BASE 1133 Bound error array assign "           )
#ifndef __HARBOUR__
   // this error is reported at compile time
   TEST_LINE( saArray[ -1 ]                   , "E BASE 1132 Bound error array access "           )
   TEST_LINE( saArray[ -1 ] := 1              , "E BASE 1133 Bound error array assign "           )
   TEST_LINE( saArray[ "1" ]                  , "E BASE 1068 Argument error array access A:2:A:{.[1].};C:1 F:S"  )
   TEST_LINE( saArray[ "1" ] := 1             , "E BASE 1069 Argument error array assign A:3:N:1;A:{.[1].};C:1 " )
#endif

   /* Alias */

   TEST_LINE( ("NOTHERE")->NOFIELD            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( (mcString)->NOFIELD             , "E BASE 1002 Alias does not exist HELLO F:R"      )
   TEST_Line( ({})->NOFIELD                   , "E BASE 1065 Argument error & A:2:A:{.[0].};C:NOFIELD F:S" )
   TEST_LINE( ({|| NIL })->NOFIELD            , "E BASE 1065 Argument error & A:2:B:{||...};C:NOFIELD F:S" )
   TEST_LINE( (.T.)->NOFIELD                  , "E BASE 1065 Argument error & A:2:L:.T.;C:NOFIELD F:S"     )
   TEST_LINE( (.F.)->NOFIELD                  , "E BASE 1065 Argument error & A:2:L:.F.;C:NOFIELD F:S"     )
   TEST_LINE( (NIL)->NOFIELD                  , "E BASE 1065 Argument error & A:2:U:NIL;C:NOFIELD F:S"     )
   TEST_LINE( (2)->NOFIELD                    , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( (2.5)->NOFIELD                  , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( (HB_SToD(""))->NOFIELD          , "E BASE 1065 Argument error & A:2:D:        ;C:NOFIELD F:S"     )
   TEST_LINE( (ErrorNew())->NOFIELD           , "E BASE 1065 Argument error & A:2:O:ERROR Object;C:NOFIELD F:S" )

#ifndef __XPP__
   TEST_LINE( ("NOTHERE")->(Eof())            , .T.                                               )
   TEST_LINE( (mcString)->(Eof())             , .T.                                               )
   TEST_LINE( ({})->(Eof())                   , .T.                                               )
   TEST_LINE( ({|| NIL })->(Eof())            , .T.                                               )
   TEST_LINE( (.T.)->(Eof())                  , .T.                                               )
   TEST_LINE( (.F.)->(Eof())                  , .T.                                               )
   TEST_LINE( (NIL)->(Eof())                  , .T.                                               )
   TEST_LINE( (2)->(Eof())                    , .T.                                               )
   TEST_LINE( (2.5)->(Eof())                  , .T.                                               )
   TEST_LINE( (HB_SToD(""))->(Eof())          , .T.                                               )
   TEST_LINE( (ErrorNew())->(Eof())           , .T.                                               )
#endif

   TEST_LINE( NOTHERE->NOFIELD                , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->("NOFIELD")            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(mcString)             , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->({})                   , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->({|| NIL })            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(.T.)                  , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(.F.)                  , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(NIL)                  , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(1)                    , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(1.5)                  , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(HB_SToD(""))          , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( NOTHERE->(ErrorNew())           , "E BASE 1002 Alias does not exist NOTHERE F:R"    )

   TEST_LINE( 200->NOFIELD                    , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( 200->("NOFIELD")                , "NOFIELD"                                         )
   TEST_LINE( 200->(mcString)                 , "HELLO"                                           )
   TEST_LINE( 200->({})                       , "{.[0].}"                                         )
   TEST_LINE( 200->({|| NIL })                , "{||...}"                                         )
   TEST_LINE( 200->(.T.)                      , .T.                                               )
   TEST_LINE( 200->(.F.)                      , .F.                                               )
   TEST_LINE( 200->(NIL)                      , NIL                                               )
   TEST_LINE( 200->(1)                        , 1                                                 )
   TEST_LINE( 200->(1.5)                      , 1.5                                               )
   TEST_LINE( 200->(HB_SToD(""))              , HB_SToD("        ")                               )
   TEST_LINE( 200->(ErrorNew())               , "ERROR Object"                                    )

   TEST_LINE( soObject:hello                  , "E BASE 1004 No exported method HELLO A:1:O:ERROR Object F:S"       )
   TEST_LINE( soObject:hello := 1             , "E BASE 1005 No exported variable HELLO A:2:O:ERROR Object;N:1 F:S" )

   /* LEN() */

   TEST_LINE( Len( NIL )                      , "E BASE 1111 Argument error LEN A:1:U:NIL F:S" )
   TEST_LINE( Len( 123 )                      , "E BASE 1111 Argument error LEN A:1:N:123 F:S" )
   TEST_LINE( Len( "" )                       , 0                                      )
   TEST_LINE( Len( "123" )                    , 3                                      )
   TEST_LINE( Len( "123"+Chr(0)+"456 " )      , 8                                      )
   TEST_LINE( Len( w_TEST->TYPE_C )           , 15                                     )
   TEST_LINE( Len( w_TEST->TYPE_C_E )         , 15                                     )
   TEST_LINE( Len( w_TEST->TYPE_M )           , 11                                     )
   TEST_LINE( Len( w_TEST->TYPE_M_E )         , 0                                      )
   TEST_LINE( Len( saArray )                  , 1                                      )
#ifdef __HARBOUR__
   TEST_LINE( Len( ErrorNew() )               , 14                                     )
   TEST_LINE( Len( Space( 1000000 ) )         , 1000000                                )
#else
   TEST_LINE( Len( ErrorNew() )               , 7                                      )
   TEST_LINE( Len( Space( 40000 ) )           , 40000                                  )
#endif

   /* EMPTY() */

#ifdef __HARBOUR__
   TEST_LINE( Empty( @scString              ) , .F.                                    ) /* Bug in CA-Cl*pper, it will return .T. */
   TEST_LINE( Empty( @scStringE             ) , .T.                                    )
   TEST_LINE( Empty( @snIntP                ) , .F.                                    ) /* Bug in CA-Cl*pper, it will return .T. */
   TEST_LINE( Empty( @snIntZ                ) , .T.                                    )
#endif
   TEST_LINE( Empty( "Hallo"                ) , .F.                                    )
   TEST_LINE( Empty( ""                     ) , .T.                                    )
   TEST_LINE( Empty( "  "                   ) , .T.                                    )
   TEST_LINE( Empty( " "+Chr(0)             ) , .F.                                    )
   TEST_LINE( Empty( " "+Chr(13)+Chr(9)     ) , .T.                                    )
   TEST_LINE( Empty( "  A"                  ) , .F.                                    )
   TEST_LINE( Empty( " x "                  ) , .F.                                    )
   TEST_LINE( Empty( " x"+Chr(0)            ) , .F.                                    )
   TEST_LINE( Empty( " "+Chr(13)+"x"+Chr(9) ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_C         ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_C_E       ) , .T.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_D         ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_D_E       ) , .T.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_M         ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_M_E       ) , .T.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_N_I       ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_N_IE      ) , .T.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_N_D       ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_N_DE      ) , .T.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_L         ) , .F.                                    )
   TEST_LINE( Empty( w_TEST->TYPE_L_E       ) , .T.                                    )
   TEST_LINE( Empty( 0                      ) , .T.                                    )
   TEST_LINE( Empty( -0                     ) , .T.                                    )
   TEST_LINE( Empty( 0.0                    ) , .T.                                    )
   TEST_LINE( Empty( 70000-70000            ) , .T.                                    )
   TEST_LINE( Empty( 1.5*1.5-2.25           ) , .T.                                    )
   TEST_LINE( Empty( 10                     ) , .F.                                    )
   TEST_LINE( Empty( 10.0                   ) , .F.                                    )
   TEST_LINE( Empty( 70000+70000            ) , .F.                                    )
   TEST_LINE( Empty( 1.5*1.5*2.25           ) , .F.                                    )
   TEST_LINE( Empty( HB_SToD("18241010")    ) , .F.                                    )
   TEST_LINE( Empty( HB_SToD("18250231")    ) , .T.                                    )
   TEST_LINE( Empty( HB_SToD("99999999")    ) , .T.                                    )
   TEST_LINE( Empty( HB_SToD("        ")    ) , .T.                                    )
   TEST_LINE( Empty( HB_SToD("")            ) , .T.                                    )
   TEST_LINE( Empty( .T.                    ) , .F.                                    )
   TEST_LINE( Empty( .F.                    ) , .T.                                    )
   TEST_LINE( Empty( NIL                    ) , .T.                                    )
   TEST_LINE( Empty( {1}                    ) , .F.                                    )
   TEST_LINE( Empty( {}                     ) , .T.                                    )
   TEST_LINE( Empty( {0}                    ) , .F.                                    )
   TEST_LINE( Empty( {|x|x+x}               ) , .F.                                    )
   TEST_LINE( Empty( ErrorNew()             ) , .F.                                    )

   /* Some number width handling tests */

   TEST_LINE( RTSTR( 50000000 )                       , " 10   50000000"                     )
   TEST_LINE( RTSTR( 99999999 )                       , " 10   99999999"                     )
   TEST_LINE( RTSTR( 100000000 )                      , " 10  100000000"                     )
   TEST_LINE( RTSTR( 500000000 )                      , " 10  500000000"                     )
   TEST_LINE( RTSTR( 999999999 )                      , " 10  999999999"                     )
   TEST_LINE( RTSTR( 999999999.99 )                   , " 13  999999999.99"                  )
   TEST_LINE( RTSTR( 1000000000 )                     , " 11  1000000000"                    )
   TEST_LINE( RTSTR( 1000000000.0 )                   , " 12 1000000000.0"                   )
   TEST_LINE( RTSTR( 1000000000.00 )                  , " 13 1000000000.00"                  )
   TEST_LINE( RTSTR( 1000000000.99 )                  , " 13 1000000000.99"                  )
   TEST_LINE( RTSTR( 4000000000 )                     , " 11  4000000000"                    )
   TEST_LINE( RTSTR( 00005 )                          , " 10          5"                     )
   TEST_LINE( RTSTR( 00005.5 )                        , " 12          5.5"                   )
   TEST_LINE( RTSTR( 5000000000 )                     , " 11  5000000000"                    )
   TEST_LINE( RTSTR( 50000000000 )                    , " 12  50000000000"                   )
   TEST_LINE( RTSTR( 500000000000 )                   , " 13  500000000000"                  )
   TEST_LINE( RTSTR( 500000000000.0 )                 , " 14 500000000000.0"                 )
   TEST_LINE( RTSTR( 5000000000000 )                  , " 14  5000000000000"                 )
   TEST_LINE( RTSTR( 50000000000000 )                 , " 15  50000000000000"                )
   TEST_LINE( RTSTR( 500000000000000 )                , " 16  500000000000000"               )
   TEST_LINE( RTSTR( 00000000000005 )                 , " 10          5"                     )
   TEST_LINE( RTSTR( 00000500000000000000 )           , " 21       500000000000000"          )
   TEST_LINE( RTSTR( 0500000000000000 )               , " 17   500000000000000"              )
   TEST_LINE( RTSTR( 0500000000000000.5 )             , " 18  500000000000000.5"             )
   TEST_LINE( RTSTR( 5000000000000000 )               , " 17  5000000000000000"              )
   TEST_LINE( RTSTR( 50000000000000000 )              , " 18  50000000000000000"             )
   TEST_LINE( RTSTR( 500000000000000000 )             , " 19  500000000000000000"            )
   TEST_LINE( RTSTR( 5000000000000000000 )            , " 20  5000000000000000000"           )
   TEST_LINE( RTSTR( 50000000000000000000 )           , " 21  50000000000000000000"          )
   TEST_LINE( RTSTR( 500000000000000000000 )          , " 22  500000000000000000000"         )
   TEST_LINE( RTSTR( 5000000000000000000000 )         , " 23  5000000000000000000000"        )
   TEST_LINE( RTSTR( 50000000000000000000000 )        , " 24  50000000000000000000000"       )
   TEST_LINE( RTSTR( 500000000000000000000000 )       , " 25  500000000000000000000000"      )
   TEST_LINE( RTSTR( 5000000000000000000000000 )      , " 26  5000000000000000000000000"     )
   TEST_LINE( RTSTR( 5000000000000000000000000.0 )    , " 27 5000000000000000000000000.0"    )
   TEST_LINE( RTSTR( -50000000 )                      , " 10  -50000000"                     )
   TEST_LINE( RTSTR( -50000000.0 )                    , " 12  -50000000.0"                   )
   TEST_LINE( RTSTR( -500000000 )                     , " 10 -500000000"                     )
   TEST_LINE( RTSTR( -999999999 )                     , " 10 -999999999"                     )
   TEST_LINE( RTSTR( -1000000000 )                    , " 20          -1000000000"           )
   TEST_LINE( RTSTR( -1000000000.0 )                  , " 22          -1000000000.0"         )
   TEST_LINE( RTSTR( -4000000000 )                    , " 20          -4000000000"           )
   TEST_LINE( RTSTR( -5000000000 )                    , " 20          -5000000000"           )
   TEST_LINE( RTSTR( -50000000000 )                   , " 20         -50000000000"           )
   TEST_LINE( RTSTR( -500000000000 )                  , " 20        -500000000000"           )
   TEST_LINE( RTSTR( -500000000000.0 )                , " 22        -500000000000.0"         )
   TEST_LINE( RTSTR( -5000000000000 )                 , " 20       -5000000000000"           )
   TEST_LINE( RTSTR( -50000000000000 )                , " 20      -50000000000000"           )
   TEST_LINE( RTSTR( -500000000000000 )               , " 20     -500000000000000"           )
   TEST_LINE( RTSTR( -5000000000000000 )              , " 20    -5000000000000000"           )
   TEST_LINE( RTSTR( -50000000000000000 )             , " 20   -50000000000000000"           )
   TEST_LINE( RTSTR( -500000000000000000 )            , " 20  -500000000000000000"           )
   TEST_LINE( RTSTR( -5000000000000000000 )           , " 20 -5000000000000000000"           )
   TEST_LINE( RTSTR( -50000000000000000000 )          , " 20 ********************"           )
   TEST_LINE( RTSTR( -500000000000000000000 )         , " 20 ********************"           )
   TEST_LINE( RTSTR( -5000000000000000000000 )        , " 20 ********************"           )
   TEST_LINE( RTSTR( -50000000000000000000000 )       , " 20 ********************"           )
   TEST_LINE( RTSTR( -500000000000000000000000 )      , " 20 ********************"           )
   TEST_LINE( RTSTR( -5000000000000000000000000 )     , " 20 ********************"           )

   TEST_LINE(( nA := 50000000                      , RTSTR( -nA )) , " 10  -50000000"                     )
   TEST_LINE(( nA := 50000000.0                    , RTSTR( -nA )) , " 12  -50000000.0"                   )
   TEST_LINE(( nA := 99999999                      , RTSTR( -nA )) , " 10  -99999999"                     )
   TEST_LINE(( nA := 99999999.9                    , RTSTR( -nA )) , " 12  -99999999.9"                   )
   TEST_LINE(( nA := 100000000                     , RTSTR( -nA )) , " 10 -100000000"                     )
   TEST_LINE(( nA := 100000000.0                   , RTSTR( -nA )) , " 12 -100000000.0"                   )
   TEST_LINE(( nA := 500000000                     , RTSTR( -nA )) , " 10 -500000000"                     )
   TEST_LINE(( nA := 999999999                     , RTSTR( -nA )) , " 10 -999999999"                     )
   TEST_LINE(( nA := 999999999.99                  , RTSTR( -nA )) , " 23           -999999999.99"        )
   TEST_LINE(( nA := 1000000000                    , RTSTR( -nA )) , " 20          -1000000000"           )
   TEST_LINE(( nA := 1000000000.0                  , RTSTR( -nA )) , " 22          -1000000000.0"         )
   TEST_LINE(( nA := 1000000000.00                 , RTSTR( -nA )) , " 23          -1000000000.00"        )
   TEST_LINE(( nA := 1000000000.99                 , RTSTR( -nA )) , " 23          -1000000000.99"        )
   TEST_LINE(( nA := 4000000000                    , RTSTR( -nA )) , " 20          -4000000000"           )
   TEST_LINE(( nA := 5000000000                    , RTSTR( -nA )) , " 20          -5000000000"           )
   TEST_LINE(( nA := 50000000000                   , RTSTR( -nA )) , " 20         -50000000000"           )
   TEST_LINE(( nA := 500000000000                  , RTSTR( -nA )) , " 20        -500000000000"           )
   TEST_LINE(( nA := 5000000000000                 , RTSTR( -nA )) , " 20       -5000000000000"           )
   TEST_LINE(( nA := 50000000000000                , RTSTR( -nA )) , " 20      -50000000000000"           )
   TEST_LINE(( nA := 500000000000000               , RTSTR( -nA )) , " 20     -500000000000000"           )
   TEST_LINE(( nA := 5000000000000000              , RTSTR( -nA )) , " 20    -5000000000000000"           )
   TEST_LINE(( nA := 50000000000000000             , RTSTR( -nA )) , " 20   -50000000000000000"           )
   TEST_LINE(( nA := 500000000000000000            , RTSTR( -nA )) , " 20  -500000000000000000"           )
   TEST_LINE(( nA := 5000000000000000000           , RTSTR( -nA )) , " 20 -5000000000000000000"           )
   TEST_LINE(( nA := 50000000000000000000          , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 500000000000000000000         , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 5000000000000000000000        , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 50000000000000000000000       , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 500000000000000000000000      , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 5000000000000000000000000     , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := 5000000000000000000000000.0   , RTSTR( -nA )) , " 22 **********************"         )
   TEST_LINE(( nA := -50000000                     , RTSTR( -nA )) , " 10   50000000"                     )
   TEST_LINE(( nA := -50000000.0                   , RTSTR( -nA )) , " 12   50000000.0"                   )
   TEST_LINE(( nA := -500000000                    , RTSTR( -nA )) , " 10  500000000"                     )
   TEST_LINE(( nA := -999999999                    , RTSTR( -nA )) , " 10  999999999"                     )
   TEST_LINE(( nA := -1000000000                   , RTSTR( -nA )) , " 10 1000000000"                     )
   TEST_LINE(( nA := -4000000000                   , RTSTR( -nA )) , " 10 4000000000"                     )
   TEST_LINE(( nA := -5000000000                   , RTSTR( -nA )) , " 10 5000000000"                     )
   TEST_LINE(( nA := -50000000000                  , RTSTR( -nA )) , " 20          50000000000"           )
   TEST_LINE(( nA := -500000000000                 , RTSTR( -nA )) , " 20         500000000000"           )
   TEST_LINE(( nA := -5000000000000                , RTSTR( -nA )) , " 20        5000000000000"           )
   TEST_LINE(( nA := -50000000000000               , RTSTR( -nA )) , " 20       50000000000000"           )
   TEST_LINE(( nA := -500000000000000              , RTSTR( -nA )) , " 20      500000000000000"           )
   TEST_LINE(( nA := -5000000000000000             , RTSTR( -nA )) , " 20     5000000000000000"           )
   TEST_LINE(( nA := -50000000000000000            , RTSTR( -nA )) , " 20    50000000000000000"           )
   TEST_LINE(( nA := -500000000000000000           , RTSTR( -nA )) , " 20   500000000000000000"           )
   TEST_LINE(( nA := -5000000000000000000          , RTSTR( -nA )) , " 20  5000000000000000000"           )
   TEST_LINE(( nA := -50000000000000000000         , RTSTR( -nA )) , " 20 50000000000000000000"           )
   TEST_LINE(( nA := -500000000000000000000        , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := -5000000000000000000000       , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := -50000000000000000000000      , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := -500000000000000000000000     , RTSTR( -nA )) , " 20 ********************"           )
   TEST_LINE(( nA := -5000000000000000000000000    , RTSTR( -nA )) , " 20 ********************"           )

   RETURN NIL

FUNCTION RTSTR( nValue )
   RETURN Str( Len( Str( nValue ) ), 3 ) + " " + Str( nValue )

/* Don't change the position of this #include. */
#include "rt_init.ch"
