/*
 * $Id$
 */

/*
   Harbour Project source code

   Runtime library regression tests, currently for some of the
   string manipulating functions.

   Copyright (C) 1999  Victor Szel <info@szelvesz.hu>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

/* TRANSFORM() tests written by Eddie Runia <eddie@runia.com> */
/* EMPTY() tests written by Eddie Runia <eddie@runia.com> */

/* NOTE: Always compile with /n switch */
/* NOTE: It's worth to make tests with and without the /z switch */
/* NOTE: Guard all Harbour extensions with __HARBOUR__ #ifdefs */

/* TODO: Add checks for string parameters with embedded NUL character */
/* TODO: Add test cases for other string functions */
/* TODO: Incorporate tests from test/working/string*.prg */
/* TODO: String overflow on + and - tests */
/* TODO: Tests with MEMO type ? */
/* TODO: Tests with Log(0) type of invalid values */

#include "error.ch"

#translate TEST_LINE( <x>, <result> ) => TEST_CALL( <(x)>, {|| <x> }, <result> )

STATIC s_nPass
STATIC s_nFail
STATIC s_cFileName
STATIC s_nFhnd
STATIC s_cNewLine
STATIC s_nCount
STATIC s_lShowAll
STATIC s_lShortcut

FUNCTION Main( cPar1 )

   /* NOTE: Some basic values we may need for some tests.
            ( passing by reference, avoid preprocessor bugs, etc. ) */

   LOCAL lcString  := "HELLO"
   LOCAL lcStringE := ""
   LOCAL lcStringZ := "A" + Chr( 0 ) + "B"
   LOCAL lnIntZ    := 0
   LOCAL lnDoubleZ := 0.0
   LOCAL lnIntP    := 10
   LOCAL lnLongP   := 100000
   LOCAL lnDoubleP := 10.567 /* Use different number of decimals than the default */
   LOCAL lnIntN    := -10
   LOCAL lnLongN   := -100000
   LOCAL lnDoubleN := -10.567 /* Use different number of decimals than the default */
   LOCAL lnDoubleI := Log( 0 )
   LOCAL ldDateE   := SToD( "" )
   LOCAL llFalse   := .F.
   LOCAL llTrue    := .T.
   LOCAL loObject  := ErrorNew()
   LOCAL luNIL     := NIL
   LOCAL lbBlock   := {|| NIL }
   LOCAL laArray   := { 9898 }

   MEMVAR mxNotHere
   MEMVAR mcString
   MEMVAR mcStringE
   MEMVAR mcStringZ
   MEMVAR mnIntZ
   MEMVAR mnDoubleZ
   MEMVAR mnIntP
   MEMVAR mnLongP
   MEMVAR mnDoubleP
   MEMVAR mnDoubleI
   MEMVAR mnIntN
   MEMVAR mnLongN
   MEMVAR mnDoubleN
   MEMVAR mdDateE
   MEMVAR mlFalse
   MEMVAR mlTrue
   MEMVAR moObject
   MEMVAR muNIL
   MEMVAR mbBlock
   MEMVAR maArray

   /* NOTE: mxNotHere intentionally not declared */
   PRIVATE mcString  := "HELLO"
   PRIVATE mcStringE := ""
   PRIVATE mcStringZ := "A" + Chr( 0 ) + "B"
   PRIVATE mnIntZ    := 0
   PRIVATE mnDoubleZ := 0.0
   PRIVATE mnIntP    := 10
   PRIVATE mnLongP   := 100000
   PRIVATE mnDoubleP := 10.567
   PRIVATE mnIntN    := -10
   PRIVATE mnLongN   := -100000
   PRIVATE mnDoubleN := -10.567
   PRIVATE mnDoubleI := Log( 0 )
   PRIVATE mdDateE   := SToD( "" )
   PRIVATE mlFalse   := .F.
   PRIVATE mlTrue    := .T.
   PRIVATE moObject  := ErrorNew()
   PRIVATE muNIL     := NIL
   PRIVATE mbBlock   := {|| NIL }
   PRIVATE maArray   := { 9898 }

   /* Initialize test */

/* TODO: Need to add this, when multi language support will be available
         to make sure all error messages comes in the original English
         language. */
/* SET LANGID TO EN */

   IF cPar1 == NIL
      cPar1 := ""
   ENDIF

/* NOTE: CA-Cl*pper PP fails on these
   TEST_LINE( "1" .AND. "2"                   , "E BASE 1066 Argument error conditional " )
   TEST_LINE( "1" .AND. .F.                   , .F.                                       )
   TEST_LINE( "A" > 1                         , "E BASE 1075 Argument error > F:S"                )
*/

   TEST_BEGIN( cPar1 )

   /* VALTYPE() */

   TEST_LINE( ValType(  lcString  )           , "C"   )
   TEST_LINE( ValType(  lcStringE )           , "C"   )
   TEST_LINE( ValType(  lcStringZ )           , "C"   )
   TEST_LINE( ValType(  lnIntZ    )           , "N"   )
   TEST_LINE( ValType(  lnDoubleZ )           , "N"   )
   TEST_LINE( ValType(  lnIntP    )           , "N"   )
   TEST_LINE( ValType(  lnLongP   )           , "N"   )
   TEST_LINE( ValType(  lnDoubleP )           , "N"   )
   TEST_LINE( ValType(  lnIntN    )           , "N"   )
   TEST_LINE( ValType(  lnLongN   )           , "N"   )
   TEST_LINE( ValType(  lnDoubleN )           , "N"   )
   TEST_LINE( ValType(  lnDoubleI )           , "N"   )
   TEST_LINE( ValType(  ldDateE   )           , "D"   )
   TEST_LINE( ValType(  llFalse   )           , "L"   )
   TEST_LINE( ValType(  llTrue    )           , "L"   )
   TEST_LINE( ValType(  loObject  )           , "O"   )
   TEST_LINE( ValType(  luNIL     )           , "U"   )
   TEST_LINE( ValType(  lbBlock   )           , "B"   )
   TEST_LINE( ValType(  laArray   )           , "A"   )
   TEST_LINE( ValType( @lcString  )           , "U"   )
   TEST_LINE( ValType( @lcStringE )           , "U"   )
   TEST_LINE( ValType( @lcStringZ )           , "U"   )
   TEST_LINE( ValType( @lnIntZ    )           , "U"   )
   TEST_LINE( ValType( @lnDoubleZ )           , "U"   )
   TEST_LINE( ValType( @lnIntP    )           , "U"   )
   TEST_LINE( ValType( @lnLongP   )           , "U"   )
   TEST_LINE( ValType( @lnDoubleP )           , "U"   )
   TEST_LINE( ValType( @lnIntN    )           , "U"   )
   TEST_LINE( ValType( @lnLongN   )           , "U"   )
   TEST_LINE( ValType( @lnDoubleN )           , "U"   )
   TEST_LINE( ValType( @lnDoubleI )           , "U"   )
   TEST_LINE( ValType( @ldDateE   )           , "U"   )
   TEST_LINE( ValType( @llFalse   )           , "U"   )
   TEST_LINE( ValType( @llTrue    )           , "U"   )
   TEST_LINE( ValType( @loObject  )           , "U"   )
   TEST_LINE( ValType( @luNIL     )           , "U"   )
   TEST_LINE( ValType( @lbBlock   )           , "U"   )
   TEST_LINE( ValType( @laArray   )           , "U"   )
   TEST_LINE( ValType(  mcString  )           , "C"   )
   TEST_LINE( ValType(  mcStringE )           , "C"   )
   TEST_LINE( ValType(  mcStringZ )           , "C"   )
   TEST_LINE( ValType(  mnIntZ    )           , "N"   )
   TEST_LINE( ValType(  mnDoubleZ )           , "N"   )
   TEST_LINE( ValType(  mnIntP    )           , "N"   )
   TEST_LINE( ValType(  mnLongP   )           , "N"   )
   TEST_LINE( ValType(  mnDoubleP )           , "N"   )
   TEST_LINE( ValType(  mnIntN    )           , "N"   )
   TEST_LINE( ValType(  mnLongN   )           , "N"   )
   TEST_LINE( ValType(  mnDoubleN )           , "N"   )
   TEST_LINE( ValType(  mnDoubleI )           , "N"   )
   TEST_LINE( ValType(  mdDateE   )           , "D"   )
   TEST_LINE( ValType(  mlFalse   )           , "L"   )
   TEST_LINE( ValType(  mlTrue    )           , "L"   )
   TEST_LINE( ValType(  moObject  )           , "O"   )
   TEST_LINE( ValType(  muNIL     )           , "U"   )
   TEST_LINE( ValType(  mbBlock   )           , "B"   )
   TEST_LINE( ValType(  maArray   )           , "A"   )
   TEST_LINE( ValType( @mcString  )           , "U"   )
   TEST_LINE( ValType( @mcStringE )           , "U"   )
   TEST_LINE( ValType( @mcStringZ )           , "U"   )
   TEST_LINE( ValType( @mnIntZ    )           , "U"   )
   TEST_LINE( ValType( @mnDoubleZ )           , "U"   )
   TEST_LINE( ValType( @mnIntP    )           , "U"   )
   TEST_LINE( ValType( @mnLongP   )           , "U"   )
   TEST_LINE( ValType( @mnDoubleP )           , "U"   )
   TEST_LINE( ValType( @mnIntN    )           , "U"   )
   TEST_LINE( ValType( @mnLongN   )           , "U"   )
   TEST_LINE( ValType( @mnDoubleN )           , "U"   )
   TEST_LINE( ValType( @mnDoubleI )           , "U"   )
   TEST_LINE( ValType( @mdDateE   )           , "U"   )
   TEST_LINE( ValType( @mlFalse   )           , "U"   )
   TEST_LINE( ValType( @mlTrue    )           , "U"   )
   TEST_LINE( ValType( @moObject  )           , "U"   )
   TEST_LINE( ValType( @muNIL     )           , "U"   )
   TEST_LINE( ValType( @mbBlock   )           , "U"   )
   TEST_LINE( ValType( @maArray   )           , "U"   )

   /* STOD() */

   /* For these tests in CA-Cl*pper 5.2e the following native STOD() has
      been used ( not the emulated one written in Clipper ):

      CLIPPER STOD( void )
      {
         // The length check is a fix to avoid buggy behaviour of _retds()
         _retds( ( ISCHAR( 1 ) && _parclen( 1 ) == 8 ) ? _parc( 1 ) : "        " );
      }
   */

   TEST_LINE( SToD()                          , SToD("        ")             )
   TEST_LINE( SToD(1)                         , SToD("        ")             )
   TEST_LINE( SToD(NIL)                       , SToD("        ")             )
   TEST_LINE( SToD("")                        , SToD("        ")             )
   TEST_LINE( SToD("        ")                , SToD("        ")             )
   TEST_LINE( SToD("       ")                 , SToD("        ")             )
   TEST_LINE( SToD("         ")               , SToD("        ")             )
   TEST_LINE( SToD(" 1234567")                , SToD("        ")             )
   TEST_LINE( SToD("1999    ")                , SToD("        ")             )
   TEST_LINE( SToD("99999999")                , SToD("        ")             )
   TEST_LINE( SToD("99990101")                , SToD("        ")             )
   TEST_LINE( SToD("19991301")                , SToD("        ")             )
   TEST_LINE( SToD("19991241")                , SToD("        ")             )
   TEST_LINE( SToD("01000101")                , SToD("01000101")             )
   TEST_LINE( SToD("29991231")                , SToD("29991231")             )
   TEST_LINE( SToD("19990905")                , SToD("19990905")             )
   TEST_LINE( SToD(" 9990905")                , SToD("        ")             )
   TEST_LINE( SToD("1 990905")                , SToD("        ")             )
   TEST_LINE( SToD("19 90905")                , SToD("17490905")             )
   TEST_LINE( SToD("199 0905")                , SToD("19740905")             )
   TEST_LINE( SToD("1999 905")                , SToD("        ")             )
   TEST_LINE( SToD("19990 05")                , SToD("        ")             )
   TEST_LINE( SToD("199909 5")                , SToD("        ")             )
   TEST_LINE( SToD("1999090 ")                , SToD("        ")             )
   TEST_LINE( SToD("1999 9 5")                , SToD("        ")             )
   TEST_LINE( SToD("1999090" + Chr(0))        , SToD("        ")             )

   /* (operators) */

   TEST_LINE( 2                <= 1                , .F.                                               )
   TEST_LINE( 1                <= 2                , .T.                                               )
   TEST_LINE( 2.0              <= 2                , .T.                                               )
   TEST_LINE( 2                <= 2.0              , .T.                                               )
   TEST_LINE( 2.5              <= 3.7              , .T.                                               )
   TEST_LINE( 3.7              <= 2.5              , .F.                                               )
   TEST_LINE( .F.              <= .F.              , .T.                                               )
   TEST_LINE( .T.              <= .F.              , .F.                                               )
   TEST_LINE( .F.              <= .T.              , .T.                                               )
   TEST_LINE( SToD("")         <= SToD("")         , .T.                                               )
   TEST_LINE( SToD("")         <= SToD("19800101") , .T.                                               )
   TEST_LINE( SToD("19800101") <= SToD("")         , .F.                                               )
   TEST_LINE( 2                <  1                , .F.                                               )
   TEST_LINE( 1                <  2                , .T.                                               )
   TEST_LINE( 2.0              <  2                , .F.                                               )
   TEST_LINE( 2                <  2.0              , .F.                                               )
   TEST_LINE( 2.5              <  3.7              , .T.                                               )
   TEST_LINE( 3.7              <  2.5              , .F.                                               )
   TEST_LINE( .F.              <  .F.              , .F.                                               )
   TEST_LINE( .T.              <  .F.              , .F.                                               )
   TEST_LINE( .F.              <  .T.              , .T.                                               )
   TEST_LINE( SToD("")         <  SToD("")         , .F.                                               )
   TEST_LINE( SToD("")         <  SToD("19800101") , .T.                                               )
   TEST_LINE( SToD("19800101") <  SToD("")         , .F.                                               )
   TEST_LINE( 2                >= 1                , .T.                                               )
   TEST_LINE( 1                >= 2                , .F.                                               )
   TEST_LINE( 2.0              >= 2                , .T.                                               )
   TEST_LINE( 2                >= 2.0              , .T.                                               )
   TEST_LINE( 2.5              >= 3.7              , .F.                                               )
   TEST_LINE( 3.7              >= 2.5              , .T.                                               )
   TEST_LINE( .F.              >= .F.              , .T.                                               )
   TEST_LINE( .T.              >= .F.              , .T.                                               )
   TEST_LINE( .F.              >= .T.              , .F.                                               )
   TEST_LINE( SToD("")         >= SToD("")         , .T.                                               )
   TEST_LINE( SToD("")         >= SToD("19800101") , .F.                                               )
   TEST_LINE( SToD("19800101") >= SToD("")         , .T.                                               )
   TEST_LINE( 2                >  1                , .T.                                               )
   TEST_LINE( 1                >  2                , .F.                                               )
   TEST_LINE( 2.0              >  2                , .F.                                               )
   TEST_LINE( 2                >  2.0              , .F.                                               )
   TEST_LINE( 2.5              >  3.7              , .F.                                               )
   TEST_LINE( 3.7              >  2.5              , .T.                                               )
   TEST_LINE( .F.              >  .F.              , .F.                                               )
   TEST_LINE( .T.              >  .F.              , .T.                                               )
   TEST_LINE( .F.              >  .T.              , .F.                                               )
   TEST_LINE( SToD("")         >  SToD("")         , .F.                                               )
   TEST_LINE( SToD("")         >  SToD("19800101") , .F.                                               )
   TEST_LINE( SToD("19800101") >  SToD("")         , .T.                                               )

   TEST_LINE( 1 + NIL                         , "E BASE 1081 Argument error + F:S" )
   TEST_LINE( 1 - NIL                         , "E BASE 1082 Argument error - F:S" )

   TEST_LINE( "A" - "B"                       , "AB"                               )
   TEST_LINE( "A  " - "B"                     , "AB  "                             )
   TEST_LINE( "A  " - "B "                    , "AB   "                            )
   TEST_LINE( "A  " - " B"                    , "A B  "                            )
   TEST_LINE( "   " - "B "                    , "B    "                            )

   TEST_LINE( 1 / NIL                         , "E BASE 1084 Argument error / F:S" )
   TEST_LINE( 1 * NIL                         , "E BASE 1083 Argument error * F:S" )
   TEST_LINE( 1 ** NIL                        , "E BASE 1088 Argument error ^ F:S" )
/* NOTE: Harbour PP fails to process this line, so it's temporarly commented out */
#ifndef __HARBOUR__
   TEST_LINE( 1 ^ NIL                         , "E BASE 1088 Argument error ^ F:S" )
#endif
   TEST_LINE( 1 % NIL                         , "E BASE 1085 Argument error % F:S" )

   TEST_LINE( -(0)                            , 0                                  )
   TEST_LINE( -(10)                           , -10                                )
   TEST_LINE( -(10.505)                       , -10.505                            )
   TEST_LINE( -(100000)                       , -100000                            )
   TEST_LINE( -(-10)                          , 10                                 )
   TEST_LINE( -("1")                          , "E BASE 1080 Argument error - F:S" )

/* NOTE: Harbour PP fails to process this line, so it's temporarly commented out */
#ifndef __HARBOUR__
   TEST_LINE( "AA" $ 1                        , "E BASE 1109 Argument error $ F:S" )
#endif
   TEST_LINE( lcString $ 1                    , "E BASE 1109 Argument error $ F:S" )
   TEST_LINE( 1 $ "AA"                        , "E BASE 1109 Argument error $ F:S" )

   IF TEST_OPT_Z()

   /* With the shortcut optimalization *ON* */

   TEST_LINE( 1 .AND. 2                       , "E BASE 1066 Argument error conditional " )
   TEST_LINE( NIL .AND. NIL                   , "E BASE 1066 Argument error conditional " )
   TEST_LINE( lcString .AND. lcString         , "E BASE 1066 Argument error conditional " )
   TEST_LINE( .T. .AND. 1                     , 1                                         )
   TEST_LINE( .T. .AND. 1.567                 , 1.567                                     )
   TEST_LINE( .T. .AND. lcString              , "HELLO"                                   )
   TEST_LINE( .T. .AND. SToD("")              , SToD("        ")                          )
   TEST_LINE( .T. .AND. NIL                   , NIL                                       )
   TEST_LINE( .T. .AND. {}                    , "{.[0].}"                                 )
   TEST_LINE( .T. .AND. {|| NIL }             , "{||...}"                                 )
   TEST_LINE( .F. .AND. 1                     , .F.                                       )
   TEST_LINE( .F. .AND. 1.567                 , .F.                                       )
   TEST_LINE( .F. .AND. lcString              , .F.                                       )
   TEST_LINE( .F. .AND. SToD("")              , .F.                                       )
   TEST_LINE( .F. .AND. NIL                   , .F.                                       )
   TEST_LINE( .F. .AND. {}                    , .F.                                       )
   TEST_LINE( .F. .AND. {|| NIL }             , .F.                                       )
   TEST_LINE( 1 .AND. .F.                     , .F.                                       )
   TEST_LINE( 1.567 .AND. .F.                 , .F.                                       )
   TEST_LINE( lcString .AND. .F.              , .F.                                       )

   /* With the shortcut optimalization *OFF* (/z switch) */

   TEST_LINE( 1 .OR. 2                        , "E BASE 1066 Argument error conditional " )
   TEST_LINE( .F. .OR. 2                      , 2                                         )
   TEST_LINE( .F. .OR. 1.678                  , 1.678                                     )
   TEST_LINE( .F. .OR. lcString               , "HELLO"                                   )
   TEST_LINE( .T. .OR. 2                      , .T.                                       )
   TEST_LINE( .T. .OR. 1.678                  , .T.                                       )
   TEST_LINE( .T. .OR. lcString               , .T.                                       )
   TEST_LINE( 1 .OR. .F.                      , 1                                         )
   TEST_LINE( 1.0 .OR. .F.                    , 1.0                                       )
   TEST_LINE( lcString .OR. .F.               , "HELLO"                                   )

   ELSE

   TEST_LINE( 1 .AND. 2                       , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( NIL .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( lcString .AND. lcString         , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. 1                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. 1.567                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. lcString              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. SToD("")              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {}                    , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {|| NIL }             , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1.567                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. lcString              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. SToD("")              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. {}                    , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. {|| NIL }             , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( 1 .AND. .F.                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( 1.567 .AND. .F.                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( lcString .AND. .F.              , "E BASE 1078 Argument error .AND. F:S"    )

   TEST_LINE( 1 .OR. 2                        , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. 2                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. 1.678                  , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .F. .OR. lcString               , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. 2                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. 1.678                  , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( .T. .OR. lcString               , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( 1 .OR. .F.                      , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( 1.0 .OR. .F.                    , "E BASE 1079 Argument error .OR. F:S"     )
   TEST_LINE( lcString .OR. .F.               , "E BASE 1079 Argument error .OR. F:S"     )

   ENDIF

   TEST_LINE( .NOT. .T.                       , .F.                                       )
   TEST_LINE( .NOT. .F.                       , .T.                                       )
   TEST_LINE( .NOT. 1                         , "E BASE 1077 Argument error .NOT. F:S"    )

   TEST_LINE( iif( "A", ":T:", ":F:" )        , "E BASE 1066 Argument error conditional " )
   TEST_LINE( iif( .T., ":T:", ":F:" )        , ":T:"                                     )
   TEST_LINE( iif( .F., ":T:", ":F:" )        , ":F:"                                     )

   TEST_LINE( lcString++                      , "E BASE 1086 Argument error ++ F:S"       )
   TEST_LINE( lcString--                      , "E BASE 1087 Argument error -- F:S"       )

   TEST_LINE( mxNotHere                       , "E BASE 1003 Variable does not exist MXNOTHERE F:R" )

   TEST_LINE( laArray[ 0 ]                    , "E BASE 1132 Bound error array access "           )
   TEST_LINE( laArray[ 0 ] := 1               , "E BASE 1133 Bound error array assign "           )
   TEST_LINE( laArray[ 1000 ]                 , "E BASE 1132 Bound error array access "           )
   TEST_LINE( laArray[ 1000 ] := 1            , "E BASE 1133 Bound error array assign "           )
   TEST_LINE( laArray[ -1 ]                   , "E BASE 1132 Bound error array access "           )
   TEST_LINE( laArray[ -1 ] := 1              , "E BASE 1133 Bound error array assign "           )
   TEST_LINE( laArray[ "1" ]                  , "E BASE 1068 Argument error array access F:S"     )
   TEST_LINE( laArray[ "1" ] := 1             , "E BASE 1069 Argument error array assign "        )

   TEST_LINE( lcString > 1                    , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( lcString >= 1                   , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( lcString <> 1                   , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( lcString == 1                   , "E BASE 1070 Argument error == F:S"               )
   TEST_LINE( loObject == loObject            , .T.                                               )
   TEST_LINE( loObject == ErrorNew()          , .F.                                               )
   TEST_LINE( loObject == TBColumnNew()       , .F.                                               )
   TEST_LINE( laArray == laArray              , .T.                                               )
   TEST_LINE( {} == {}                        , .F.                                               )
   TEST_LINE( {|| NIL } == {|| NIL }          , "E BASE 1070 Argument error == F:S"               )
   TEST_LINE( lcString = 1                    , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( lcString < 1                    , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( lcString <= 1                   , "E BASE 1074 Argument error <= F:S"               )

/* NOTE: TEST_CALL() should be used here, since CA-Cl*pper can't preprocess
         the TEST_LINE() variation properly. */
// TEST_LINE( ("NOTHERE")->NOFIELD            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
// TEST_LINE( (mcString)->NOFIELD             , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
// TEST_LINE( ({})->NOFIELD                   , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
// TEST_LINE( ({|| NIL })->NOFIELD            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
// TEST_LINE( (.T.)->NOFIELD                  , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_CALL( '("NOTHERE")->NOFIELD', {|| ("NOTHERE")->NOFIELD }, "E BASE 1002 Alias does not exist NOTHERE F:R" )
   TEST_CALL( '(mcString)->NOFIELD' , {|| (mcString)->NOFIELD } , "E BASE 1002 Alias does not exist HELLO F:R"   )
   TEST_CALL( '({})->NOFIELD'       , {|| ({})->NOFIELD }       , "E BASE 1065 Argument error & F:S"             )
   TEST_CALL( '({|| NIL })->NOFIELD', {|| ({|| NIL })->NOFIELD }, "E BASE 1065 Argument error & F:S"             )
   TEST_CALL( '(.T.)->NOFIELD'      , {|| (.T.)->NOFIELD }      , "E BASE 1065 Argument error & F:S"             )
   TEST_CALL( '(NIL)->NOFIELD'      , {|| (NIL)->NOFIELD }      , "E BASE 1065 Argument error & F:S"             )
   TEST_CALL( '("NOTHERE")->(Eof())', {|| ("NOTHERE")->(Eof()) }, .T.                                            )
   TEST_CALL( '(mcString)->(Eof())' , {|| (mcString)->(Eof()) } , .T.                                            )
   TEST_CALL( '({})->(Eof())'       , {|| ({})->(Eof()) }       , .T.                                            )
   TEST_CALL( '({|| NIL })->(Eof())', {|| ({|| NIL })->(Eof()) }, .T.                                            )
   TEST_CALL( '(.T.)->(Eof())'      , {|| (.T.)->(Eof()) }      , .T.                                            )
   TEST_CALL( '(.F.)->(Eof())'      , {|| (.F.)->(Eof()) }      , .T.                                            )
   TEST_CALL( '(NIL)->(Eof())'      , {|| (NIL)->(Eof()) }      , .T.                                            )
   TEST_LINE( NOTHERE->NOFIELD                , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( 200->NOFIELD                    , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( 200->("NOFIELD")                , "NOFIELD"                                         )
   TEST_LINE( 200->(NIL)                      , NIL                                               )
   TEST_LINE( 200->(1)                        , 1                                                 )
   TEST_LINE( 200->(1.5)                      , 1.5                                               )
   TEST_LINE( 200->({})                       , "{.[0].}"                                         )
   TEST_LINE( 200->({|| NIL })                , "{||...}"                                         )
   TEST_LINE( 200->(.T.)                      , .T.                                               )

   TEST_LINE( loObject:hello                  , "E BASE 1004 No exported method HELLO F:S"        )
   TEST_LINE( loObject:hello := 1             , "E BASE 1005 No exported variable HELLO F:S"      )

   /* LEN() */

   TEST_LINE( Len( NIL )                      , "E BASE 1111 Argument error LEN F:S"   )
   TEST_LINE( Len( 123 )                      , "E BASE 1111 Argument error LEN F:S"   )
   TEST_LINE( Len( "" )                       , 0                                      )
   TEST_LINE( Len( "123" )                    , 3                                      )
   TEST_LINE( Len( laArray )                  , 1                                      )
#ifdef __HARBOUR__
   TEST_LINE( Len( Space( 3000000000 ) )      , 3000000000                             )
#else
   TEST_LINE( Len( Space( 40000 ) )           , 40000                                  )
#endif

   /* EMPTY() */

   TEST_LINE( Empty( @lcString              ) , .T.                                    ) /* Bug in CA-Cl*pper ? */
   TEST_LINE( Empty( @lcStringE             ) , .T.                                    )
   TEST_LINE( Empty( @lnIntP                ) , .T.                                    ) /* Bug in CA-Cl*pper ? */
   TEST_LINE( Empty( @lnIntZ                ) , .T.                                    )
   TEST_LINE( Empty( "Hallo"                ) , .F.                                    )
   TEST_LINE( Empty( ""                     ) , .T.                                    )
   TEST_LINE( Empty( "  "                   ) , .T.                                    )
   TEST_LINE( Empty( " "+Chr(0)             ) , .F.                                    )
   TEST_LINE( Empty( " "+Chr(13)+Chr(9)     ) , .T.                                    )
   TEST_LINE( Empty( "  A"                  ) , .F.                                    )
   TEST_LINE( Empty( " x "                  ) , .F.                                    )
   TEST_LINE( Empty( " x"+Chr(0)            ) , .F.                                    )
   TEST_LINE( Empty( " "+Chr(13)+"x"+Chr(9) ) , .F.                                    )
   TEST_LINE( Empty( 0                      ) , .T.                                    )
   TEST_LINE( Empty( -0                     ) , .T.                                    )
   TEST_LINE( Empty( 0.0                    ) , .T.                                    )
   TEST_LINE( Empty( 70000-70000            ) , .T.                                    )
   TEST_LINE( Empty( 1.5*1.5-2.25           ) , .T.                                    )
   TEST_LINE( Empty( 10                     ) , .F.                                    )
   TEST_LINE( Empty( 10.0                   ) , .F.                                    )
   TEST_LINE( Empty( 70000+70000            ) , .F.                                    )
   TEST_LINE( Empty( 1.5*1.5*2.25           ) , .F.                                    )
   TEST_LINE( Empty( SToD("18241010")       ) , .F.                                    )
   TEST_LINE( Empty( SToD("18250231")       ) , .T.                                    )
   TEST_LINE( Empty( SToD("99999999")       ) , .T.                                    )
   TEST_LINE( Empty( SToD("        ")       ) , .T.                                    )
   TEST_LINE( Empty( SToD("")               ) , .T.                                    )
   TEST_LINE( Empty( .T.                    ) , .F.                                    )
   TEST_LINE( Empty( .F.                    ) , .T.                                    )
   TEST_LINE( Empty( NIL                    ) , .T.                                    )
   TEST_LINE( Empty( {1}                    ) , .F.                                    )
   TEST_LINE( Empty( {}                     ) , .T.                                    )
   TEST_LINE( Empty( {0}                    ) , .F.                                    )
   TEST_LINE( Empty( {|x|x+x}               ) , .F.                                    )

   /* ABS() */

   TEST_LINE( Abs("A")                        , "E BASE 1089 Argument error ABS F:S"   )
   TEST_LINE( Abs(0)                          , 0                                      )
   TEST_LINE( Abs(10)                         , 10                                     )
   TEST_LINE( Abs(-10)                        , 10                                     )
   TEST_LINE( Abs(0.1)                        , 0.1                                    )
   TEST_LINE( Abs(10.5)                       , 10.5                                   )
   TEST_LINE( Abs(-10.7)                      , 10.7                                   )
   TEST_LINE( Abs(100000)                     , 100000                                 )
   TEST_LINE( Abs(-100000)                    , 100000                                 )

   /* EXP() */

   TEST_LINE( Exp("A")                        , "E BASE 1096 Argument error EXP F:S"   )
   TEST_LINE( Exp(0)                          , 1.00                                   )
   TEST_LINE( Round(Exp(1),2)                 , 2.72                                   )
   TEST_LINE( Str(Exp(1),20,10)               , "        2.7182818285"                 )
   TEST_LINE( Round(Exp(10),2)                , 22026.47                               )
   TEST_LINE( Str(Exp(10),20,10)              , "    22026.4657948067"                 )

   /* ROUND() */

   TEST_LINE( Round(NIL, 0)                   , "E BASE 1094 Argument error ROUND F:S" )
   TEST_LINE( Round(0, NIL)                   , "E BASE 1094 Argument error ROUND F:S" )
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
   TEST_LINE( Round(50, 0)                    , 50               )
   TEST_LINE( Round(50, 1)                    , 50.0             )
   TEST_LINE( Round(50, 2)                    , 50.00            )
   TEST_LINE( Round(50, -1)                   , 50               )
   TEST_LINE( Round(50, -2)                   , 100              )
   TEST_LINE( Round(10.50, 0)                 , 11               )
   TEST_LINE( Round(10.50, -1)                , 10               )

   /* AT() */

   TEST_LINE( At("", "")                      , 1                )
   TEST_LINE( At("", "ABCDEF")                , 1                )
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
   TEST_LINE( Replicate("XXX", 2000000000)    , "E BASE 1234 String overflow REPLICATE F:S" )
#else
   TEST_LINE( Replicate("XXX", 30000)         , "E BASE 1234 String overflow REPLICATE F:S" )
#endif

   /* SUBSTR() */

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
   TEST_LINE( SubStr("abcdef", -2, -1)        , ""               )
   TEST_LINE( SubStr("abcdef", -2, 0)         , ""               )
   TEST_LINE( SubStr("abcdef", -2, 1)         , "e"              )
   TEST_LINE( SubStr("abcdef", -2, 7)         , "ef"             )
   TEST_LINE( SubStr("abcdef", -2)            , "ef"             )
   TEST_LINE( SubStr("abcdef", 10, -1)        , ""               )
   TEST_LINE( SubStr("abcdef", 10, 0)         , ""               )
   TEST_LINE( SubStr("abcdef", 10, 1)         , ""               )
   TEST_LINE( SubStr("abcdef", 10, 7)         , ""               )
   TEST_LINE( SubStr("abcdef", 10)            , ""               )
   TEST_LINE( SubStr("abcdef", -10, -1)       , ""               )
   TEST_LINE( SubStr("abcdef", -10, 0)        , ""               )
   TEST_LINE( SubStr("abcdef", -10, 1)        , "a"              )
   TEST_LINE( SubStr("abcdef", -10, 7)        , "abcdef"         )
   TEST_LINE( SubStr("abcdef", -10, 15)       , "abcdef"         )
   TEST_LINE( SubStr("abcdef", -10)           , "abcdef"         )
   TEST_LINE( SubStr("ab" + Chr(0) + "def", 2, 3) , "b" + Chr(0) + "d"   )

   /* LEFT() */

   TEST_LINE( Left("abcdef", -10)             , ""               )
   TEST_LINE( Left("abcdef", -2)              , ""               )
   TEST_LINE( Left("abcdef", 0)               , ""               )
   TEST_LINE( Left("abcdef", 2)               , "ab"             )
   TEST_LINE( Left("abcdef", 10)              , "abcdef"         )
   TEST_LINE( Left("ab" + Chr(0) + "def", 5)  , "ab" + Chr(0) + "de" )

   /* RIGHT() */

   TEST_LINE( Right("abcdef", -10)            , ""               )
   TEST_LINE( Right("abcdef", -2)             , ""               )
   TEST_LINE( Right("abcdef", 0)              , ""               )
   TEST_LINE( Right("abcdef", 2)              , "ef"             )
   TEST_LINE( Right("abcdef", 10)             , "abcdef"         )
   TEST_LINE( Right("ab" + Chr(0) + "def", 5) , "b" + Chr(0) + "def" )

   /* PADR() */

   TEST_LINE( PadR(NIL, 5)                    , ""               )
   TEST_LINE( PadR(.T., 5)                    , ""               )
   TEST_LINE( PadR(10, 5)                     , "10   "          )
   TEST_LINE( PadR(Year(SToD("19800101")), 5) , "1980 "          )
   TEST_LINE( PadR(Day(SToD("19800101")), 5)  , "1    "          )
   TEST_LINE( PadR("abcdef", -5)              , ""               )
   TEST_LINE( PadR("abcdef", 0)               , ""               )
   TEST_LINE( PadR("abcdef", 5)               , "abcde"          )
   TEST_LINE( PadR("abcdef", 10)              , "abcdef    "     )
   TEST_LINE( PadR("abcdef", 10, "1")         , "abcdef1111"     )
   TEST_LINE( PadR("abcdef", 10, "12")        , "abcdef1111"     )

   /* PADL() */

   TEST_LINE( PadL(NIL, 5)                    , ""               )
   TEST_LINE( PadL(.T., 5)                    , ""               )
   TEST_LINE( PadL(10, 5)                     , "   10"          )
   TEST_LINE( PadL(Year(SToD("19800101")), 5) , " 1980"          )
   TEST_LINE( PadL(Day(SToD("19800101")), 5)  , "    1"          )
   TEST_LINE( PadL("abcdef", -5)              , ""               )
   TEST_LINE( PadL("abcdef", 0)               , ""               )
   TEST_LINE( PadL("abcdef", 5)               , "abcde"          ) /* QUESTION: CA-Cl*pper "bug", should return: "bcdef" ? */
   TEST_LINE( PadL("abcdef", 10)              , "    abcdef"     )
   TEST_LINE( PadL("abcdef", 10, "1")         , "1111abcdef"     )
   TEST_LINE( PadL("abcdef", 10, "12")        , "1111abcdef"     )

   /* PADC() */

   TEST_LINE( PadC(NIL, 5)                    , ""               )
   TEST_LINE( PadC(.T., 5)                    , ""               )
   TEST_LINE( PadC(10, 5)                     , " 10  "          )
   TEST_LINE( PadC(Year(SToD("19800101")), 5) , "1980 "          )
   TEST_LINE( PadC(Day(SToD("19800101")), 5)  , "  1  "          )
   TEST_LINE( PadC("abcdef", -5)              , ""               )
   TEST_LINE( PadC("abcdef", 0)               , ""               )
   TEST_LINE( PadC("abcdef", 2)               , "ab"             ) /* QUESTION: CA-Cl*pper "bug", should return: "cd" ? */
   TEST_LINE( PadC("abcdef", 5)               , "abcde"          )
   TEST_LINE( PadC("abcdef", 10)              , "  abcdef  "     )
   TEST_LINE( PadC("abcdef", 10, "1")         , "11abcdef11"     )
   TEST_LINE( PadC("abcdef", 10, "12")        , "11abcdef11"     )

   /* STUFF() */

   TEST_LINE( Stuff("ABCDEF", 0, 0, NIL)      , ""               )
   TEST_LINE( Stuff("ABCDEF", 0, 0, "xyz")    , "xyzABCDEF"      )
   TEST_LINE( Stuff("ABCDEF", 1, 0, "xyz")    , "xyzABCDEF"      )
   TEST_LINE( Stuff("ABCDEF", 2, 0, "xyz")    , "AxyzBCDEF"      )
   TEST_LINE( Stuff("ABCDEF", 2, 3, "xyz")    , "AxyzEF"         )
   TEST_LINE( Stuff("ABCDEF", 2, 2, "")       , "ADEF"           )
   TEST_LINE( Stuff("ABCDEF", 2, 1, "xyz")    , "AxyzCDEF"       )
   TEST_LINE( Stuff("ABCDEF", 2, 4, "xyz")    , "AxyzF"          )
   TEST_LINE( Stuff("ABCDEF", 2, 10, "xyz")   , "Axyz"           )

#ifdef __HARBOUR__

   /* __COLORINDEX() */

   TEST_LINE( __ColorIndex()                  , ""               )
   TEST_LINE( __ColorIndex("", -1)            , ""               )
   TEST_LINE( __ColorIndex("", 0)             , ""               )
   TEST_LINE( __ColorIndex("W/R", -1)         , ""               )
   TEST_LINE( __ColorIndex("W/R", 0)          , "W/R"            )
   TEST_LINE( __ColorIndex("W/R", 1)          , ""               )
   TEST_LINE( __ColorIndex("W/R", 2)          , ""               )
   TEST_LINE( __ColorIndex("W/R,GR/0", 0)     , "W/R"            )
   TEST_LINE( __ColorIndex("W/R,GR/0", 1)     , "GR/0"           )
   TEST_LINE( __ColorIndex("W/R,GR/0", 2)     , ""               )
   TEST_LINE( __ColorIndex("W/R,GR/0", 3)     , ""               )
   TEST_LINE( __ColorIndex("W/R, GR/0", 0)    , "W/R"            )
   TEST_LINE( __ColorIndex("W/R, GR/0", 1)    , "GR/0"           )
   TEST_LINE( __ColorIndex("W/R, GR/0", 2)    , ""               )
   TEST_LINE( __ColorIndex("W/R, GR/0", 3)    , ""               )
   TEST_LINE( __ColorIndex("W/R,GR/0 ", 0)    , "W/R"            )
   TEST_LINE( __ColorIndex("W/R,GR/0 ", 1)    , "GR/0"           )
   TEST_LINE( __ColorIndex("W/R,GR/0 ", 2)    , ""               )
   TEST_LINE( __ColorIndex("W/R, GR/0 ", 0)   , "W/R"            )
   TEST_LINE( __ColorIndex("W/R, GR/0 ", 1)   , "GR/0"           )
   TEST_LINE( __ColorIndex("W/R, GR/0 ", 2)   , ""               )
   TEST_LINE( __ColorIndex("W/R, GR/0 ,", 0)  , "W/R"            )
   TEST_LINE( __ColorIndex("W/R, GR/0 ,", 1)  , "GR/0"           )
   TEST_LINE( __ColorIndex("W/R, GR/0 ,", 2)  , ""               )
   TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 0) , "W/R"            )
   TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 1) , "GR/0"           )
   TEST_LINE( __ColorIndex(" W/R, GR/0 ,", 2) , ""               )
   TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 0), "W/R"            )
   TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 1), "GR/0"           )
   TEST_LINE( __ColorIndex(" W/R , GR/0 ,", 2), ""               )
   TEST_LINE( __ColorIndex(" W/R ,   ,", 1)   , ""               )
   TEST_LINE( __ColorIndex(" W/R ,,", 1)      , ""               )
   TEST_LINE( __ColorIndex(",,", 0)           , ""               )
   TEST_LINE( __ColorIndex(",,", 1)           , ""               )
   TEST_LINE( __ColorIndex(",,", 2)           , ""               )
   TEST_LINE( __ColorIndex(",  ,", 2)         , ""               )

#endif

   /* STR() */

   TEST_LINE( Str(5000000000.0)               , "5000000000.0"   )
   TEST_LINE( Str(5000000000)                 , " 5000000000"    )
   TEST_LINE( Str(-5000000000.0)              , "         -5000000000.0" )
   TEST_LINE( Str(-5000000000)                , "         -5000000000" )
   TEST_LINE( Str(10)                         , "        10"     )
   TEST_LINE( Str(10.0)                       , "        10.0"   )
   TEST_LINE( Str(10.00)                      , "        10.00"  )
   TEST_LINE( Str(100000)                     , "    100000"     )
   TEST_LINE( Str(-10)                        , "       -10"     )
   TEST_LINE( Str(-10.0)                      , "       -10.0"   )
   TEST_LINE( Str(-10.00)                     , "       -10.00"  )
   TEST_LINE( Str(-100000)                    , "   -100000"     )
   TEST_LINE( Str(10, 5)                      , "   10"          )
   TEST_LINE( Str(10.0, 5)                    , "   10"          )
   TEST_LINE( Str(10.00, 5)                   , "   10"          )
   TEST_LINE( Str(100000, 5)                  , "*****"          )
   TEST_LINE( Str(100000, 8)                  , "  100000"       )
   TEST_LINE( Str(-10, 5)                     , "  -10"          )
   TEST_LINE( Str(-10.0, 5)                   , "  -10"          )
   TEST_LINE( Str(-10.00, 5)                  , "  -10"          )
   TEST_LINE( Str(-100000, 5)                 , "*****"          )
   TEST_LINE( Str(-100000, 6)                 , "******"         )
   TEST_LINE( Str(-100000, 8)                 , " -100000"       )
   TEST_LINE( Str(10, -5)                     , "        10"     )
   TEST_LINE( Str(10.0, -5)                   , "        10"     )
   TEST_LINE( Str(10.00, -5)                  , "        10"     )
   TEST_LINE( Str(100000, -5)                 , "    100000"     )
   TEST_LINE( Str(100000, -8)                 , "    100000"     )
   TEST_LINE( Str(-10, -5)                    , "       -10"     )
   TEST_LINE( Str(-10.0, -5)                  , "       -10"     )
   TEST_LINE( Str(-10.00, -5)                 , "       -10"     )
   TEST_LINE( Str(-100000, -5)                , "   -100000"     )
   TEST_LINE( Str(-100000, -6)                , "   -100000"     )
   TEST_LINE( Str(-100000, -8)                , "   -100000"     )
   TEST_LINE( Str(10, 5, 0)                   , "   10"          )
   TEST_LINE( Str(10.0, 5, 0)                 , "   10"          )
   TEST_LINE( Str(10.50, 5, 0)                , "   11"          )
   TEST_LINE( Str(100000, 5, 0)               , "*****"          )
   TEST_LINE( Str(-10, 5, 0)                  , "  -10"          )
   TEST_LINE( Str(-10.0, 5, 0)                , "  -10"          )
   TEST_LINE( Str(-10.00, 5, 0)               , "  -10"          )
   TEST_LINE( Str(-100000, 5, 0)              , "*****"          )
   TEST_LINE( Str(-100000, 6, 0)              , "******"         )
   TEST_LINE( Str(-100000, 8, 0)              , " -100000"       )
   TEST_LINE( Str(10, 5, 1)                   , " 10.0"          )
   TEST_LINE( Str(10.0, 5, 1)                 , " 10.0"          )
   TEST_LINE( Str(10.50, 5, 1)                , " 10.5"          )
   TEST_LINE( Str(100000, 5, 1)               , "*****"          )
   TEST_LINE( Str(-10, 5, 1)                  , "-10.0"          )
   TEST_LINE( Str(-10.0, 5, 1)                , "-10.0"          )
   TEST_LINE( Str(-10.00, 5, 1)               , "-10.0"          )
   TEST_LINE( Str(-100000, 5, 1)              , "*****"          )
   TEST_LINE( Str(-100000, 6, 1)              , "******"         )
   TEST_LINE( Str(-100000, 8, 1)              , "********"       )
   TEST_LINE( Str(10, 5, -1)                  , "   10"          )
   TEST_LINE( Str(10.0, 5, -1)                , "   10"          )
   TEST_LINE( Str(10.50, 5, -1)               , "   11"          )
   TEST_LINE( Str(100000, 5, -1)              , "*****"          )
   TEST_LINE( Str(-10, 5, -1)                 , "  -10"          )
   TEST_LINE( Str(-10.0, 5, -1)               , "  -10"          )
   TEST_LINE( Str(-10.00, 5, -1)              , "  -10"          )
   TEST_LINE( Str(-100000, 5, -1)             , "*****"          )
   TEST_LINE( Str(-100000, 6, -1)             , "******"         )
   TEST_LINE( Str(-100000, 8, -1)             , " -100000"       )

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
   TEST_LINE( Str(Val("1")                )   , "1"                            )
   TEST_LINE( Str(Val("15")               )   , "15"                           )
   TEST_LINE( Str(Val("200")              )   , "200"                          )
   TEST_LINE( Str(Val("15.0")             )   , "15.0"                         )
   TEST_LINE( Str(Val("15.00")            )   , "15.00"                        )
   TEST_LINE( Str(Year(SToD("19990905"))  )   , " 1999"                        )
   TEST_LINE( Str(Month(SToD("19990905")) )   , "  9"                          )
   TEST_LINE( Str(Day(SToD("19990905"))   )   , "  5"                          )
   TEST_LINE( Str(10                      )   , "        10"                   )
   TEST_LINE( Str(15.0                    )   , "        15.0"                 )
   TEST_LINE( Str(10.1                    )   , "        10.1"                 )
   TEST_LINE( Str(15.00                   )   , "        15.00"                )
   TEST_LINE( Str(Log(0)                  )   , "***********************"      )
   TEST_LINE( Str(100.2 * 200.12          )   , "     20052.024"               )
   TEST_LINE( Str(100.20 * 200.12         )   , "     20052.0240"              )
   TEST_LINE( Str(1000.2 * 200.12         )   , "    200160.024"               )
   TEST_LINE( Str(100/1000                )   , "         0.10"                )
   TEST_LINE( Str(100/100000              )   , "         0.00"                )
   TEST_LINE( Str(10 * 10                 )   , "       100"                   )
   TEST_LINE( Str(100 / 10                )   , "        10"                   )
   TEST_LINE( Str(1234567890 * 1234567890 )   , " 1524157875019052000"         )

   /* STRZERO() */

   TEST_LINE( StrZero(10)                     , "0000000010"     )
   TEST_LINE( StrZero(10.0)                   , "0000000010.0"   )
   TEST_LINE( StrZero(10.00)                  , "0000000010.00"  )
   TEST_LINE( StrZero(100000)                 , "0000100000"     )
   TEST_LINE( StrZero(-10)                    , "-000000010"     )
   TEST_LINE( StrZero(-10.0)                  , "-000000010.0"   )
   TEST_LINE( StrZero(-10.00)                 , "-000000010.00"  )
   TEST_LINE( StrZero(-100000)                , "-000100000"     )
   TEST_LINE( StrZero(10, 5)                  , "00010"          )
   TEST_LINE( StrZero(10.0, 5)                , "00010"          )
   TEST_LINE( StrZero(10.00, 5)               , "00010"          )
   TEST_LINE( StrZero(100000, 5)              , "*****"          )
   TEST_LINE( StrZero(100000, 8)              , "00100000"       )
   TEST_LINE( StrZero(-10, 5)                 , "-0010"          )
   TEST_LINE( StrZero(-10.0, 5)               , "-0010"          )
   TEST_LINE( StrZero(-10.00, 5)              , "-0010"          )
   TEST_LINE( StrZero(-100000, 5)             , "*****"          )
   TEST_LINE( StrZero(-100000, 6)             , "******"         )
   TEST_LINE( StrZero(-100000, 8)             , "-0100000"       )
   TEST_LINE( StrZero(10, -5)                 , "0000000010"     )
   TEST_LINE( StrZero(10.0, -5)               , "0000000010"     )
   TEST_LINE( StrZero(10.00, -5)              , "0000000010"     )
   TEST_LINE( StrZero(100000, -5)             , "0000100000"     )
   TEST_LINE( StrZero(100000, -8)             , "0000100000"     )
   TEST_LINE( StrZero(-10, -5)                , "-000000010"     )
   TEST_LINE( StrZero(-10.0, -5)              , "-000000010"     )
   TEST_LINE( StrZero(-10.00, -5)             , "-000000010"     )
   TEST_LINE( StrZero(-100000, -5)            , "-000100000"     )
   TEST_LINE( StrZero(-100000, -6)            , "-000100000"     )
   TEST_LINE( StrZero(-100000, -8)            , "-000100000"     )
   TEST_LINE( StrZero(10, 5, 0)               , "00010"          )
   TEST_LINE( StrZero(10.0, 5, 0)             , "00010"          )
   TEST_LINE( StrZero(10.50, 5, 0)            , "00011"          )
   TEST_LINE( StrZero(100000, 5, 0)           , "*****"          )
   TEST_LINE( StrZero(-10, 5, 0)              , "-0010"          )
   TEST_LINE( StrZero(-10.0, 5, 0)            , "-0010"          )
   TEST_LINE( StrZero(-10.00, 5, 0)           , "-0010"          )
   TEST_LINE( StrZero(-100000, 5, 0)          , "*****"          )
   TEST_LINE( StrZero(-100000, 6, 0)          , "******"         )
   TEST_LINE( StrZero(-100000, 8, 0)          , "-0100000"       )
   TEST_LINE( StrZero(10, 5, 1)               , "010.0"          )
   TEST_LINE( StrZero(10.0, 5, 1)             , "010.0"          )
   TEST_LINE( StrZero(10.50, 5, 1)            , "010.5"          )
   TEST_LINE( StrZero(100000, 5, 1)           , "*****"          )
   TEST_LINE( StrZero(-10, 5, 1)              , "-10.0"          )
   TEST_LINE( StrZero(-10.0, 5, 1)            , "-10.0"          )
   TEST_LINE( StrZero(-10.00, 5, 1)           , "-10.0"          )
   TEST_LINE( StrZero(-100000, 5, 1)          , "*****"          )
   TEST_LINE( StrZero(-100000, 6, 1)          , "******"         )
   TEST_LINE( StrZero(-100000, 8, 1)          , "********"       )
   TEST_LINE( StrZero(10, 5, -1)              , "00010"          )
   TEST_LINE( StrZero(10.0, 5, -1)            , "00010"          )
   TEST_LINE( StrZero(10.50, 5, -1)           , "00011"          )
   TEST_LINE( StrZero(100000, 5, -1)          , "*****"          )
   TEST_LINE( StrZero(-10, 5, -1)             , "-0010"          )
   TEST_LINE( StrZero(-10.0, 5, -1)           , "-0010"          )
   TEST_LINE( StrZero(-10.00, 5, -1)          , "-0010"          )
   TEST_LINE( StrZero(-100000, 5, -1)         , "*****"          )
   TEST_LINE( StrZero(-100000, 6, -1)         , "******"         )
   TEST_LINE( StrZero(-100000, 8, -1)         , "-0100000"       )

   /* TRANSFORM() */

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

   SET DATE ANSI
   SET CENTURY ON

   TEST_LINE( Transform( SToD("19901214") , "99/99/9999" ) , "1990.12.14"                  )
   TEST_LINE( Transform( SToD("19901202") , "99.99.9999" ) , "1990.12.02"                  )
   TEST_LINE( Transform( SToD("")         , "99/99/9999" ) , "    .  .  "                  )
   TEST_LINE( Transform( SToD("19901202") , "99/99/99"   ) , "1990.12.02"                  )
   TEST_LINE( Transform( SToD("19901214") , "99-99-99"   ) , "1990.12.14"                  )
   TEST_LINE( Transform( SToD("20040430") , "99.99.99"   ) , "2004.04.30"                  )
   TEST_LINE( Transform( SToD("")         , "99/99/99"   ) , "    .  .  "                  )
   TEST_LINE( Transform( SToD("19920101") , "THISWRNG"   ) , "1992.01.01"                  )
   TEST_LINE( Transform( SToD("19350605") , "999/99/9"   ) , "1935.06.05"                  )
   TEST_LINE( Transform( SToD("19101112") , "9#-9#/##"   ) , "1910.11.12"                  )
   TEST_LINE( Transform( SToD("19920101") , ""           ) , "1992.01.01"                  )
   TEST_LINE( Transform( SToD("19920101") , "DO THIS "   ) , "1992.01.01"                  )
   TEST_LINE( Transform( SToD("19920102") , "@E"         ) , "02/01/1992"                  ) /* Bug in CA-Cl*pper, it returns: "2.91901.02" */
   TEST_LINE( Transform( 1234             , "@D 9999"    ) , "1234.00.0 "                  )
   TEST_LINE( Transform( 1234             , "@BD 9999"   ) , "1234.00.0 "                  )

   SET CENTURY OFF

   TEST_LINE( Transform( SToD("19901214") , "99/99/9999" ) , "90.12.14"                    )
   TEST_LINE( Transform( SToD("19901202") , "99.99.9999" ) , "90.12.02"                    )
   TEST_LINE( Transform( SToD("")         , "99/99/9999" ) , "  .  .  "                    )
   TEST_LINE( Transform( SToD("19901202") , "99/99/99"   ) , "90.12.02"                    )
   TEST_LINE( Transform( SToD("19901214") , "99-99-99"   ) , "90.12.14"                    )
   TEST_LINE( Transform( SToD("20040430") , "99.99.99"   ) , "04.04.30"                    )
   TEST_LINE( Transform( SToD("")         , "99/99/99"   ) , "  .  .  "                    )
   TEST_LINE( Transform( SToD("19920101") , "THISWRNG"   ) , "92.01.01"                    )
   TEST_LINE( Transform( SToD("19350605") , "999/99/9"   ) , "35.06.05"                    )
   TEST_LINE( Transform( SToD("19101112") , "9#-9#/##"   ) , "10.11.12"                    )
   TEST_LINE( Transform( SToD("19920101") , ""           ) , "92.01.01"                    )
   TEST_LINE( Transform( SToD("19920101") , "DO THIS "   ) , "92.01.01"                    )
   TEST_LINE( Transform( SToD("19920102") , "@E"         ) , "02/01/92"                    ) /* Bug in CA-Cl*pper, it returns: "01.92.02" */
   TEST_LINE( Transform( 1234             , "@D 9999"    ) , "**.**.* "                    )
   TEST_LINE( Transform( 1234             , "@BD 9999"   ) , "**.**.* "                    )

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

   /* DESCEND() */

   TEST_LINE( Descend()                       , NIL                                                 ) /* Bug in CA-Cl*pper, it returns undefined trash */
   TEST_LINE( Descend( NIL )                  , NIL                                                 )
   TEST_LINE( Descend( { "A", "B" } )         , NIL                                                 )
   TEST_LINE( Descend( @lcString )            , NIL                                                 )
   TEST_LINE( Descend( lcString )             , "¸»´´±"                                             )
   TEST_LINE( Descend( lcString )             , "¸»´´±"                                             )
   TEST_LINE( Descend( Descend( lcString ) )  , "HELLO"                                             )
   TEST_LINE( Descend( .F. )                  , .T.                                                 )
   TEST_LINE( Descend( .T. )                  , .F.                                                 )
   TEST_LINE( Descend( 0 )                    , 0.00                                                )
   TEST_LINE( Descend( 1 )                    , -1.00                                               )
   TEST_LINE( Descend( -1 )                   , 1.00                                                )
   TEST_LINE( Descend( Descend( 256 ) )       , 256.00                                              )
   TEST_LINE( Descend( 2.0 )                  , -2.00                                               )
   TEST_LINE( Descend( 2.5 )                  , -2.50                                               )
   TEST_LINE( Descend( -100.35 )              , 100.35                                              )
   TEST_LINE( Str(Descend( -740.354 ))        , "       740.35"                                     )
   TEST_LINE( Str(Descend( -740.359 ))        , "       740.36"                                     )
   TEST_LINE( Str(Descend( -740.354 ), 15, 5) , "      740.35400"                                   )
   TEST_LINE( Str(Descend( -740.359 ), 15, 5) , "      740.35900"                                   )
   TEST_LINE( Descend( 100000 )               , -100000.00                                          )
   TEST_LINE( Descend( -100000 )              , 100000.00                                           )
   TEST_LINE( Descend( "" )                   , ""                                                  )
   TEST_LINE( Descend( Chr(0) )               , ""+Chr(0)+""                                        )
   TEST_LINE( Descend( Chr(0) + "Hello" )     , ""+Chr(0)+"¸›””‘"                                   )
   TEST_LINE( Descend( "Hello"+Chr(0)+"wo" )  , "¸›””‘"+Chr(0)+"‰‘"                                 )
   TEST_LINE( Descend( SToD( "" ) )           , 5231808                                             )
   TEST_LINE( Descend( SToD( "01000101" ) )   , 3474223                                             )
   TEST_LINE( Descend( SToD( "19801220" ) )   , 2787214                                             )

   /* Show results, return ERRORLEVEL and exit */

   TEST_END()

   RETURN NIL

#define TEST_RESULT_COL1_WIDTH  1
#define TEST_RESULT_COL2_WIDTH  4
#define TEST_RESULT_COL3_WIDTH  30
#define TEST_RESULT_COL4_WIDTH  55
#define TEST_RESULT_COL5_WIDTH  40

STATIC FUNCTION TEST_BEGIN( cParam )
   LOCAL cOs := OS()

   IF "OS/2" $ cOs .OR. ;
      "DOS"  $ cOs
      s_cNewLine := Chr( 13 ) + Chr( 10 )
   ELSE
      s_cNewLine := Chr( 10 )
   ENDIF

   s_lShowAll := "/ALL" $ Upper( cParam )

   /* Detect presence of shortcutting optimalization */

   s_lShortcut := .T.
   IF .T. .OR. Eval( {|| s_lShortcut := .F. } )
      /* Do nothing */
   ENDIF

   /* Decide about output filename */

   DO CASE
   CASE "HARBOUR" $ Upper( Version() )     ; s_cFileName := "rtl_test.hb"
   CASE "CLIPPER (R)" $ Upper( Version() ) .AND. ;
        "5.3" $ Version()                  ; s_cFileName := "rtl_test.c53"
   CASE "CLIPPER (R)" $ Upper( Version() ) ; s_cFileName := "rtl_test.c5x"
   ENDCASE

   s_nFhnd := 1 /* FHND_STDOUT */
   s_cFileName := "(stdout)"

   s_nCount := 0
   s_nPass := 0
   s_nFail := 0

   fWrite( s_nFhnd, "      Version: " + Version() + s_cNewLine +;
                    "           OS: " + OS() + s_cNewLine +;
                    "   Date, Time: " + DToS( Date() ) + " " + Time() + s_cNewLine +;
                    "       Output: " + s_cFileName + s_cNewLine +;
                    "Shortcut opt.: " + iif( s_lShortcut, "ON", "OFF" ) + s_cNewLine +;
                    "     Switches: " + cParam + s_cNewLine +;
                    "===========================================================================" + s_cNewLine )

   fWrite( s_nFhnd, PadL( "R", TEST_RESULT_COL1_WIDTH ) + " " +;
                    PadL( "No", TEST_RESULT_COL2_WIDTH ) + ". " +;
                    PadR( "TestCall()", TEST_RESULT_COL3_WIDTH ) + " -> " +;
                    PadR( "Result", TEST_RESULT_COL4_WIDTH ) + " | " +;
                    PadR( "Expected", TEST_RESULT_COL5_WIDTH ) + s_cNewLine +;
                    "---------------------------------------------------------------------------" + s_cNewLine )

   RETURN NIL

STATIC FUNCTION TEST_CALL( cBlock, bBlock, xResultExpected )
   LOCAL xResult
   LOCAL oError
   LOCAL bOldError
   LOCAL lFailed

   s_nCount++

   IF ValType( cBlock ) == "C"

      bOldError := ErrorBlock( {|oError| Break( oError ) } )

      BEGIN SEQUENCE
         xResult := Eval( bBlock )
      RECOVER USING oError
         xResult := ErrorMessage( oError )
      END SEQUENCE

      ErrorBlock( bOldError )

      IF !( ValType( xResult ) == ValType( xResultExpected ) )
         IF ValType( xResultExpected) == "C" .AND. ValType( xResult ) $ "ABM"
            lFailed := !( XToStr( xResult ) == xResultExpected )
         ELSE
            lFailed := .T.
         ENDIF
      ELSE
         lFailed := !( xResult == xResultExpected )
      ENDIF

   ELSE

      lFailed := .T.
      cBlock := "!! Preprocessor error. Test skipped !!"
      xResult := NIL

   ENDIF

   IF s_lShowAll .OR. lFailed

      fWrite( s_nFhnd, PadR( iif( lFailed, "!", " " ), TEST_RESULT_COL1_WIDTH ) + " " +;
                       Str( s_nCount, TEST_RESULT_COL2_WIDTH ) + ". " +;
                       PadR( cBlock, TEST_RESULT_COL3_WIDTH ) + " -> " +;
                       PadR( XToStr( xResult ), TEST_RESULT_COL4_WIDTH ) + " | " +;
                       PadR( XToStr( xResultExpected ), TEST_RESULT_COL5_WIDTH ) )

      fWrite( s_nFhnd, s_cNewLine )
   ENDIF

   IF lFailed
      s_nFail++
   ELSE
      s_nPass++
   ENDIF

   RETURN NIL

STATIC FUNCTION TEST_OPT_Z()
   RETURN s_lShortCut

STATIC FUNCTION TEST_END()

   fWrite( s_nFhnd, "===========================================================================" + s_cNewLine +;
                    "Test calls passed: " + Str( s_nPass ) + s_cNewLine +;
                    "Test calls failed: " + Str( s_nFail ) + s_cNewLine +;
                    "                   ----------" + s_cNewLine +;
                    "            Total: " + Str( s_nPass + s_nFail ) + s_cNewLine +;
                    s_cNewLine )

   IF s_nFail != 0
      IF "CLIPPER (R)" $ Upper( Version() )
         fWrite( s_nFhnd, "WARNING ! Failures detected using CA-Clipper." + s_cNewLine +;
                          "Please fix those expected results which are not bugs in CA-Clipper itself." + s_cNewLine )
      ELSE
         fWrite( s_nFhnd, "WARNING ! Failures detected" + s_cNewLine )
      ENDIF
   ENDIF

   ErrorLevel( iif( s_nFail != 0, 1, 0 ) )

   RETURN NIL

STATIC FUNCTION XToStr( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C" ; RETURN '"' + StrTran( xValue, Chr(0), '"+Chr(0)+"' ) + '"'
   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN 'SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN '{||...}'
   CASE cType == "A" ; RETURN '{.[' + LTrim( Str( Len( xValue ) ) ) + '].}'
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
   ENDCASE

   RETURN ""

STATIC FUNCTION ErrorMessage( oError )
   LOCAL cMessage := ""

   IF ValType( oError:severity ) == "N"
      DO CASE
      CASE oError:severity == ES_WHOCARES     ; cMessage += "M "
      CASE oError:severity == ES_WARNING      ; cMessage += "W "
      CASE oError:severity == ES_ERROR        ; cMessage += "E "
      CASE oError:severity == ES_CATASTROPHIC ; cMessage += "C "
      ENDCASE
   ENDIF
   IF ValType( oError:subsystem ) == "C"
      cMessage += oError:subsystem() + " "
   ENDIF
   IF ValType( oError:subCode ) == "N"
      cMessage += LTrim( Str( oError:subCode ) ) + " "
   ENDIF
   IF ValType( oError:description ) == "C"
      cMessage += oError:description + " "
   ENDIF
   IF !Empty( oError:operation )
      cMessage += oError:operation + " "
   ENDIF
   IF !Empty( oError:filename )
      cMessage += oError:filename + " "
   ENDIF

   IF oError:canDefault .OR. ;
      oError:canRetry .OR. ;
      oError:canSubstitute

      cMessage += "F:"
      IF oError:canDefault
         cMessage += "D"
      ENDIF
      IF oError:canRetry
         cMessage += "R"
      ENDIF
      IF oError:canSubstitute
         cMessage += "S"
      ENDIF
   ENDIF

   RETURN cMessage

#ifndef __HARBOUR__
#ifndef __XPP__

STATIC FUNCTION SToD( cDate )
   LOCAL cOldDateFormat
   LOCAL dDate

   IF ValType( cDate ) == "C"
      cOldDateFormat := Set( _SET_DATEFORMAT, "yyyy/mm/dd" )

      dDate := CToD( SubStr( cDate, 1, 4 ) + "/" +;
                     SubStr( cDate, 5, 2 ) + "/" +;
                     SubStr( cDate, 7, 2 ) )

      Set( _SET_DATEFORMAT, cOldDateFormat )
   ELSE
      dDate := CToD( "" )
   ENDIF

   RETURN dDate

#endif
#endif

