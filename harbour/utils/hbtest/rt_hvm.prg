/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (HVM)
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

FUNCTION Main_HVM()
   LOCAL nA, nB, nC

/* NOTE: CA-Cl*pper PP fails on these
   TEST_LINE( "1" .AND. "2"                   , "E BASE 1066 Argument error conditional " )
   TEST_LINE( "1" .AND. .F.                   , .F.                                       )
   TEST_LINE( "A" > 1                         , "E BASE 1075 Argument error > F:S"                )
*/

   /* VALTYPE() */

   TEST_LINE( ValType(  scString  )           , "C"   )
   TEST_LINE( ValType(  scStringE )           , "C"   )
   TEST_LINE( ValType(  scStringZ )           , "C"   )
   TEST_LINE( ValType(  snIntZ    )           , "N"   )
   TEST_LINE( ValType(  snDoubleZ )           , "N"   )
   TEST_LINE( ValType(  snIntP    )           , "N"   )
   TEST_LINE( ValType(  snLongP   )           , "N"   )
   TEST_LINE( ValType(  snDoubleP )           , "N"   )
   TEST_LINE( ValType(  snIntN    )           , "N"   )
   TEST_LINE( ValType(  snLongN   )           , "N"   )
   TEST_LINE( ValType(  snDoubleN )           , "N"   )
   TEST_LINE( ValType(  snDoubleI )           , "N"   )
   TEST_LINE( ValType(  sdDateE   )           , "D"   )
   TEST_LINE( ValType(  slFalse   )           , "L"   )
   TEST_LINE( ValType(  slTrue    )           , "L"   )
   TEST_LINE( ValType(  soObject  )           , "O"   )
   TEST_LINE( ValType(  suNIL     )           , "U"   )
   TEST_LINE( ValType(  sbBlock   )           , "B"   )
   TEST_LINE( ValType(  saArray   )           , "A"   )
   TEST_LINE( ValType( { 1, 2, 3 } )          , "A"   )
   TEST_LINE( ValType( w_TEST->TYPE_C )       , "C"   )
   TEST_LINE( ValType( w_TEST->TYPE_D )       , "D"   )
   TEST_LINE( ValType( w_TEST->TYPE_M )       , "M"   )
   TEST_LINE( ValType( w_TEST->TYPE_N_I )     , "N"   )
   TEST_LINE( ValType( w_TEST->TYPE_N_D )     , "N"   )
   TEST_LINE( ValType( w_TEST->TYPE_L )       , "L"   )
#ifdef __HARBOUR__
   TEST_LINE( ValType( @scString  )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @scStringE )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @scStringZ )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snIntZ    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snDoubleZ )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snIntP    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snLongP   )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snDoubleP )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snIntN    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snLongN   )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snDoubleN )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @snDoubleI )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @sdDateE   )           , "D"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @slFalse   )           , "L"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @slTrue    )           , "L"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @soObject  )           , "O"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @suNIL     )           , "U"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @sbBlock   )           , "B"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @saArray   )           , "A"   ) /* Bug in CA-Cl*pper, it will return "U" */
#endif
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
#ifdef __HARBOUR__
   TEST_LINE( ValType( @mcString  )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mcStringE )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mcStringZ )           , "C"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnIntZ    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnDoubleZ )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnIntP    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnLongP   )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnDoubleP )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnIntN    )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnLongN   )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnDoubleN )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mnDoubleI )           , "N"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mdDateE   )           , "D"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mlFalse   )           , "L"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mlTrue    )           , "L"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @moObject  )           , "O"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @muNIL     )           , "U"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @mbBlock   )           , "B"   ) /* Bug in CA-Cl*pper, it will return "U" */
   TEST_LINE( ValType( @maArray   )           , "A"   ) /* Bug in CA-Cl*pper, it will return "U" */
#endif

   /* TYPE() */

#ifndef __XPP__
   TEST_LINE( Type( NIL )                     , "E BASE 1121 Argument error TYPE F:S" )
   TEST_LINE( Type( 100 )                     , "E BASE 1121 Argument error TYPE F:S" )
   TEST_LINE( Type( {} )                      , "E BASE 1121 Argument error TYPE F:S" )
#endif
   TEST_LINE( Type( "w_TEST->TYPE_C" )        , "C"   )
   TEST_LINE( Type( "w_TEST->TYPE_D" )        , "D"   )
   TEST_LINE( Type( "w_TEST->TYPE_M" )        , "M"   )
   TEST_LINE( Type( "w_TEST->TYPE_N_I" )      , "N"   )
   TEST_LINE( Type( "w_TEST->TYPE_N_D" )      , "N"   )
   TEST_LINE( Type( "w_TEST->TYPE_L" )        , "L"   )
   TEST_LINE( Type( "mxNotHere"  )            , "U"   )
   TEST_LINE( Type( "mcString"  )             , "C"   )
   TEST_LINE( Type( "mcStringE" )             , "C"   )
   TEST_LINE( Type( "mcStringZ" )             , "C"   )
   TEST_LINE( Type( "mnIntZ"    )             , "N"   )
   TEST_LINE( Type( "mnDoubleZ" )             , "N"   )
   TEST_LINE( Type( "mnIntP"    )             , "N"   )
   TEST_LINE( Type( "mnLongP"   )             , "N"   )
   TEST_LINE( Type( "mnDoubleP" )             , "N"   )
   TEST_LINE( Type( "mnIntN"    )             , "N"   )
   TEST_LINE( Type( "mnLongN"   )             , "N"   )
   TEST_LINE( Type( "mnDoubleN" )             , "N"   )
   TEST_LINE( Type( "mnDoubleI" )             , "N"   )
   TEST_LINE( Type( "mdDateE"   )             , "D"   )
   TEST_LINE( Type( "mlFalse"   )             , "L"   )
   TEST_LINE( Type( "mlTrue"    )             , "L"   )
   TEST_LINE( Type( "moObject"  )             , "O"   )
   TEST_LINE( Type( "muNIL"     )             , "U"   )
   TEST_LINE( Type( "mbBlock"   )             , "B"   )
   TEST_LINE( Type( "maArray"   )             , "A"   )

   /* Special internal messages */

/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( NIL:className                   , "NIL"       )
#endif
#endif                                                      )
#ifndef __XPP__
   TEST_LINE( "":className                    , "CHARACTER" )
   TEST_LINE( 0:className                     , "NUMERIC"   )
   TEST_LINE( SToD( "" ):className            , "DATE"      )
   TEST_LINE( .F.:className                   , "LOGICAL"   )
   TEST_LINE( {|| NIL }:className             , "BLOCK"     )
   TEST_LINE( {}:className                    , "ARRAY"     )
   TEST_LINE( ErrorNew():className            , "ERROR"     )
#endif
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( NIL:classH                      , 0           )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( "":classH                       , 0           )
   TEST_LINE( 0:classH                        , 0           )
   TEST_LINE( SToD( "" ):classH               , 0           )
   TEST_LINE( .F.:classH                      , 0           )
   TEST_LINE( {|| NIL }:classH                , 0           )
   TEST_LINE( {}:classH                       , 0           )
   TEST_LINE( ErrorNew():classH > 0           , .T.         )
#endif

/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( suNIL:className                 , "NIL"       )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( scString:className              , "CHARACTER" )
   TEST_LINE( snIntP:className                , "NUMERIC"   )
   TEST_LINE( sdDateE:className               , "DATE"      )
   TEST_LINE( slFalse:className               , "LOGICAL"   )
   TEST_LINE( sbBlock:className               , "BLOCK"     )
   TEST_LINE( saArray:className               , "ARRAY"     )
   TEST_LINE( soObject:className              , "ERROR"     )
#endif
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( suNIL:classH                    , 0           )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( scString:classH                 , 0           )
   TEST_LINE( snIntP:classH                   , 0           )
   TEST_LINE( sdDateE:classH                  , 0           )
   TEST_LINE( slFalse:classH                  , 0           )
   TEST_LINE( sbBlock:classH                  , 0           )
   TEST_LINE( saArray:classH                  , 0           )
   TEST_LINE( soObject:classH > 0             , .T.         )
#endif

   /* (operators) */

   /* <= */

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
   TEST_LINE( ""               <= "AAA"            , .T.                                               )
   TEST_LINE( "AAA"            <= ""               , .T.                                               )
   TEST_LINE( "AAA"            <= "AA"             , .T.                                               )
   TEST_LINE( "AAA"            <= Chr(255)         , .T.                                               )
   TEST_LINE( Chr(150)         <= Chr(255)         , .T.                                               )
   TEST_LINE( "A"              <= "a"              , .T.                                               )
   TEST_LINE( "A"              <= "Z"              , .T.                                               )
   TEST_LINE( "Z"              <= " "              , .F.                                               )
   TEST_LINE( Chr(0)           <= " "              , .T.                                               )
   TEST_LINE( "Hallo"          <= "Hello"          , .T.                                               )
   TEST_LINE( "Hello"          <= "Hello"          , .T.                                               )
   TEST_LINE( "Hell"           <= "Hello"          , .T.                                               )
   TEST_LINE( "Hellow"         <= "Hello"          , .T.                                               )
   TEST_LINE( "J"              <= "Hello"          , .F.                                               )
   TEST_LINE( ""               <= "Hello"          , .T.                                               )
   TEST_LINE( "J"              <= ""               , .T.                                               )
   TEST_LINE( ""               <= ""               , .T.                                               )

   /* < */

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
   TEST_LINE( ""               <  "AAA"            , .T.                                               )
   TEST_LINE( "AAA"            <  ""               , .F.                                               )
   TEST_LINE( "AAA"            <  "AA"             , .F.                                               )
   TEST_LINE( "AAA"            <  Chr(255)         , .T.                                               )
   TEST_LINE( Chr(150)         <  Chr(255)         , .T.                                               )
   TEST_LINE( "A"              <  "a"              , .T.                                               )
   TEST_LINE( "A"              <  "Z"              , .T.                                               )
   TEST_LINE( "Z"              <  "A"              , .F.                                               )
   TEST_LINE( Chr(0)           <  " "              , .T.                                               )
   TEST_LINE( "Hallo"          <  "Hello"          , .T.                                               )
   TEST_LINE( "Hello"          <  "Hello"          , .F.                                               )
   TEST_LINE( "Hell"           <  "Hello"          , .T.                                               )
   TEST_LINE( "Hellow"         <  "Hello"          , .F.                                               )
   TEST_LINE( "J"              <  "Hello"          , .F.                                               )
   TEST_LINE( ""               <  "Hello"          , .T.                                               )
   TEST_LINE( "J"              <  ""               , .F.                                               )
   TEST_LINE( ""               <  ""               , .F.                                               )

   /* >= */

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
   TEST_LINE( ""               >= "AAA"            , .F.                                               )
   TEST_LINE( "AAA"            >= ""               , .T.                                               )
   TEST_LINE( "AAA"            >= "AA"             , .T.                                               )
   TEST_LINE( "AAA"            >= Chr(255)         , .F.                                               )
   TEST_LINE( Chr(150)         >= Chr(255)         , .F.                                               )
   TEST_LINE( "A"              >= "a"              , .F.                                               )
   TEST_LINE( "A"              >= "Z"              , .F.                                               )
   TEST_LINE( "Z"              >= "A"              , .T.                                               )
   TEST_LINE( Chr(0)           >= " "              , .F.                                               )
   TEST_LINE( "Hallo"          >= "Hello"          , .F.                                               )
   TEST_LINE( "Hello"          >= "Hello"          , .T.                                               )
   TEST_LINE( "Hell"           >= "Hello"          , .F.                                               )
   TEST_LINE( "Hellow"         >= "Hello"          , .T.                                               )
   TEST_LINE( "J"              >= "Hello"          , .T.                                               )
   TEST_LINE( ""               >= "Hello"          , .F.                                               )
   TEST_LINE( "J"              >= ""               , .T.                                               )
   TEST_LINE( ""               >= ""               , .T.                                               )

   /* > */

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
   TEST_LINE( ""               >  "AAA"            , .F.                                               )
   TEST_LINE( "AAA"            >  ""               , .F.                                               )
   TEST_LINE( "AAA"            >  "AA"             , .F.                                               )
   TEST_LINE( "AAA"            >  Chr(255)         , .F.                                               )
   TEST_LINE( Chr(150)         >  Chr(255)         , .F.                                               )
   TEST_LINE( "A"              >  "a"              , .F.                                               )
   TEST_LINE( "A"              >  "Z"              , .F.                                               )
   TEST_LINE( "Z"              >  "A"              , .T.                                               )
   TEST_LINE( Chr(0)           >  " "              , .F.                                               )
   TEST_LINE( "Hallo"          >  "Hello"          , .F.                                               )
   TEST_LINE( "Hello"          >  "Hello"          , .F.                                               )
   TEST_LINE( "Hell"           >  "Hello"          , .F.                                               )
   TEST_LINE( "Hellow"         >  "Hello"          , .F.                                               )
   TEST_LINE( "J"              >  "Hello"          , .T.                                               )
   TEST_LINE( ""               >  "Hello"          , .F.                                               )
   TEST_LINE( "J"              >  ""               , .F.                                               )
   TEST_LINE( ""               >  ""               , .F.                                               )

   /* =, == */

   SET EXACT ON
   TEST_LINE( "123" = "123  "                 , .T.                                               )
   TEST_LINE( " 123" = "123"                  , .F.                                               )
   TEST_LINE( "123" = "12345"                 , .F.                                               )
   TEST_LINE( "12345" = "123"                 , .F.                                               )
   TEST_LINE( "123" = ""                      , .F.                                               )
   TEST_LINE( "" = "123"                      , .F.                                               )
   TEST_LINE( "A" == "A"                      , .T.                                               )
   TEST_LINE( "Z" == "A"                      , .F.                                               )
   TEST_LINE( "A" == "A "                     , .F.                                               )
   TEST_LINE( "AA" == "A"                     , .F.                                               )
   SET EXACT OFF
   TEST_LINE( "123" = "123  "                 , .F.                                               )
   TEST_LINE( " 123" = "123"                  , .F.                                               )
   TEST_LINE( "123" = "12345"                 , .F.                                               )
   TEST_LINE( "12345" = "123"                 , .T.                                               )
   TEST_LINE( "123" = ""                      , .T.                                               )
   TEST_LINE( "" = "123"                      , .F.                                               )
   TEST_LINE( "A" == "A"                      , .T.                                               )
   TEST_LINE( "Z" == "A"                      , .F.                                               )
   TEST_LINE( "A" == "A "                     , .F.                                               )
   TEST_LINE( "AA" == "A"                     , .F.                                               )
   TEST_LINE( "Hallo"          == "Hello"     , .F.                                               )
   TEST_LINE( "Hello"          == "Hello"     , .T.                                               )
   TEST_LINE( "Hell"           == "Hello"     , .F.                                               )
   TEST_LINE( "Hellow"         == "Hello"     , .F.                                               )
   TEST_LINE( "J"              == "Hello"     , .F.                                               )
   TEST_LINE( ""               == "Hello"     , .F.                                               )
   TEST_LINE( "J"              == ""          , .F.                                               )
   TEST_LINE( ""               == ""          , .T.                                               )

   TEST_LINE( scString  = scString            , .T.                                               )
   TEST_LINE( scString  = scStringE           , .T.                                               )
   TEST_LINE( scString  = scStringZ           , .F.                                               )
   TEST_LINE( scStringE = scString            , .F.                                               )
   TEST_LINE( scStringE = scStringE           , .T.                                               )
   TEST_LINE( scStringE = scStringZ           , .F.                                               )
   TEST_LINE( scStringZ = scString            , .F.                                               )
   TEST_LINE( scStringZ = scStringE           , .T.                                               )
   TEST_LINE( scStringZ = scStringZ           , .T.                                               )

   /* != */

   SET EXACT ON
   TEST_LINE( "123" != "123  "                , .F.                                               )
   TEST_LINE( " 123" != "123"                 , .T.                                               )
   TEST_LINE( "123" != "12345"                , .T.                                               )
   TEST_LINE( "12345" != "123"                , .T.                                               )
   TEST_LINE( "123" != ""                     , .T.                                               )
   TEST_LINE( "" != "123"                     , .T.                                               )
   TEST_LINE( "A" != "A"                      , .F.                                               )
   TEST_LINE( "Z" != "A"                      , .T.                                               )
   TEST_LINE( "A" != "A "                     , .F.                                               )
   TEST_LINE( "AA" != "A"                     , .T.                                               )
   SET EXACT OFF
   TEST_LINE( "123" != "123  "                , .T.                                               )
   TEST_LINE( " 123" != "123"                 , .T.                                               )
   TEST_LINE( "123" != "12345"                , .T.                                               )
   TEST_LINE( "12345" != "123"                , .F.                                               )
   TEST_LINE( "123" != ""                     , .F.                                               )
   TEST_LINE( "" != "123"                     , .T.                                               )
   TEST_LINE( "A" != "A"                      , .F.                                               )
   TEST_LINE( "Z" != "A"                      , .T.                                               )
   TEST_LINE( "A" != "A "                     , .T.                                               )
   TEST_LINE( "AA" != "A"                     , .F.                                               )
   TEST_LINE( "Hallo"          != "Hello"     , .T.                                               )
   TEST_LINE( "Hello"          != "Hello"     , .F.                                               )
   TEST_LINE( "Hell"           != "Hello"     , .T.                                               )
   TEST_LINE( "Hellow"         != "Hello"     , .F.                                               )
   TEST_LINE( "J"              != "Hello"     , .T.                                               )
   TEST_LINE( ""               != "Hello"     , .T.                                               )
   TEST_LINE( "J"              != ""          , .F.                                               )
   TEST_LINE( ""               != ""          , .F.                                               )

   TEST_LINE( scString  != scString           , .F.                                               )
   TEST_LINE( scString  != scStringE          , .F.                                               )
   TEST_LINE( scString  != scStringZ          , .T.                                               )
   TEST_LINE( scStringE != scString           , .T.                                               )
   TEST_LINE( scStringE != scStringE          , .F.                                               )
   TEST_LINE( scStringE != scStringZ          , .T.                                               )
   TEST_LINE( scStringZ != scString           , .T.                                               )
   TEST_LINE( scStringZ != scStringE          , .F.                                               )
   TEST_LINE( scStringZ != scStringZ          , .F.                                               )

   /* == special */

   TEST_LINE( NIL == NIL                      , .T.                                               )
   TEST_LINE( 1 == NIL                        , .F.                                               )
   TEST_LINE( NIL == 1                        , .F.                                               )
   TEST_LINE( "" == NIL                       , .F.                                               )
   TEST_LINE( NIL == ""                       , .F.                                               )
   TEST_LINE( 1 == suNIL                      , .F.                                               )
   TEST_LINE( suNIL == 1                      , .F.                                               )
   TEST_LINE( "" == suNIL                     , .F.                                               )
   TEST_LINE( suNIL == ""                     , .F.                                               )
   TEST_LINE( scString == NIL                 , .F.                                               )
   TEST_LINE( scString == 1                   , "E BASE 1070 Argument error == F:S"               )
   TEST_LINE( soObject == ""                  , "E BASE 1070 Argument error == F:S"               )
   TEST_LINE( soObject == soObject            , .T.                                               )
   TEST_LINE( soObject == ErrorNew()          , .F.                                               )
   TEST_LINE( ErrorNew() == ErrorNew()        , .F.                                               )
   TEST_LINE( soObject == TBColumnNew()       , .F.                                               )
   TEST_LINE( saArray == saArray              , .T.                                               )
   TEST_LINE( {} == {}                        , .F.                                               )
   TEST_LINE( {|| NIL } == {|| NIL }          , "E BASE 1070 Argument error == F:S"               )

   /* = special */

   TEST_LINE( NIL = NIL                       , .T.                                               )
   TEST_LINE( scString = NIL                  , .F.                                               )
   TEST_LINE( scString = 1                    , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( soObject = ""                   , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( soObject = soObject             , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( soObject = ErrorNew()           , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( ErrorNew() = ErrorNew()         , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( soObject = TBColumnNew()        , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( saArray = saArray               , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( {} = {}                         , "E BASE 1071 Argument error = F:S"                )
   TEST_LINE( {|| NIL } = {|| NIL }           , "E BASE 1071 Argument error = F:S"                )

   /* != special */

   TEST_LINE( NIL != NIL                      , .F.                                               )
   TEST_LINE( scString != NIL                 , .T.                                               )
   TEST_LINE( scString != 1                   , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( soObject != ""                  , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( soObject != soObject            , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( soObject != ErrorNew()          , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( ErrorNew() != ErrorNew()        , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( soObject != TBColumnNew()       , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( saArray != saArray              , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( {} != {}                        , "E BASE 1072 Argument error <> F:S"               )
   TEST_LINE( {|| NIL } != {|| NIL }          , "E BASE 1072 Argument error <> F:S"               )

   /* < special */

   TEST_LINE( NIL < NIL                       , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( scString < NIL                  , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( scString < 1                    , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( soObject < ""                   , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( soObject < soObject             , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( soObject < ErrorNew()           , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( ErrorNew() < ErrorNew()         , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( soObject < TBColumnNew()        , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( saArray < saArray               , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( {} < {}                         , "E BASE 1073 Argument error < F:S"                )
   TEST_LINE( {|| NIL } < {|| NIL }           , "E BASE 1073 Argument error < F:S"                )

   /* <= special */

   TEST_LINE( NIL <= NIL                      , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( scString <= NIL                 , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( scString <= 1                   , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( soObject <= ""                  , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( soObject <= soObject            , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( soObject <= ErrorNew()          , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( ErrorNew() <= ErrorNew()        , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( soObject <= TBColumnNew()       , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( saArray <= saArray              , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( {} <= {}                        , "E BASE 1074 Argument error <= F:S"               )
   TEST_LINE( {|| NIL } <= {|| NIL }          , "E BASE 1074 Argument error <= F:S"               )

   /* > special */

   TEST_LINE( NIL > NIL                       , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( scString > NIL                  , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( scString > 1                    , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( soObject > ""                   , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( soObject > soObject             , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( soObject > ErrorNew()           , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( ErrorNew() > ErrorNew()         , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( soObject > TBColumnNew()        , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( saArray > saArray               , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( {} > {}                         , "E BASE 1075 Argument error > F:S"                )
   TEST_LINE( {|| NIL } > {|| NIL }           , "E BASE 1075 Argument error > F:S"                )

   /* >= special */

   TEST_LINE( NIL >= NIL                      , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( scString >= NIL                 , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( scString >= 1                   , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( soObject >= ""                  , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( soObject >= soObject            , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( soObject >= ErrorNew()          , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( ErrorNew() >= ErrorNew()        , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( soObject >= TBColumnNew()       , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( saArray >= saArray              , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( {} >= {}                        , "E BASE 1076 Argument error >= F:S"               )
   TEST_LINE( {|| NIL } >= {|| NIL }          , "E BASE 1076 Argument error >= F:S"               )

   // NOTE: These are compiler tests.
   //       The expressions have to be written with no separators!
   TEST_LINE( mnIntP==10.OR.mnIntP==0         , .T.                                               )
   TEST_LINE( mnIntP==10.AND.mnLongP==0       , .F.                                               )

   TEST_LINE( NIL + 1                         , "E BASE 1081 Argument error + F:S" )
   TEST_LINE( NIL - 1                         , "E BASE 1082 Argument error - F:S" )

   TEST_LINE( scString + NIL                  , "E BASE 1081 Argument error + F:S" )
   TEST_LINE( scString - NIL                  , "E BASE 1082 Argument error - F:S" )

   TEST_LINE( 1 + NIL                         , "E BASE 1081 Argument error + F:S" )
   TEST_LINE( 1 - NIL                         , "E BASE 1082 Argument error - F:S" )

   TEST_LINE( "A" - "B"                       , "AB"                               )
   TEST_LINE( "A  " - "B"                     , "AB  "                             )
   TEST_LINE( "A  " - "B "                    , "AB   "                            )
   TEST_LINE( "A  " - " B"                    , "A B  "                            )
   TEST_LINE( "   " - "B "                    , "B    "                            )

   TEST_LINE( 1 / 0                           , "E BASE 1340 Zero divisor / F:S"   )
   TEST_LINE( 1 / NIL                         , "E BASE 1084 Argument error / F:S" )
   TEST_LINE( 1 * NIL                         , "E BASE 1083 Argument error * F:S" )
   TEST_LINE( 1 ** NIL                        , "E BASE 1088 Argument error ^ F:S" )
   TEST_LINE( 1 ^ NIL                         , "E BASE 1088 Argument error ^ F:S" )
   TEST_LINE( 1 % 0                           , "E BASE 1341 Zero divisor % F:S"   )
   TEST_LINE( 1 % NIL                         , "E BASE 1085 Argument error % F:S" )

   TEST_LINE( Str( 1 / 0   )                  , "E BASE 1340 Zero divisor / F:S"   )
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
   TEST_LINE( Str( 1 % 0   )                  , "E BASE 1341 Zero divisor % F:S"   )
   TEST_LINE( Str( 2 % 4   )                  , "         2"                       )
   TEST_LINE( Str( 4 % 2   )                  , "         0"                       )
   TEST_LINE( Str( 4 % 2.0 )                  , "         0.00"                    )
   TEST_LINE( Str( 2 % 4.0 )                  , "         2.00"                    )

   TEST_LINE( Str(  3 %  3 )                   , "         0"                      )
   TEST_LINE( Str(  3 %  2 )                   , "         1"                      )
   TEST_LINE( Str(  3 %  1 )                   , "         0"                      )
   TEST_LINE( Str(  3 %  0 )                   , "E BASE 1341 Zero divisor % F:S"  )
   TEST_LINE( Str(  3 % -1 )                   , "         0"                      )
   TEST_LINE( Str(  3 % -2 )                   , "         1"                      )
   TEST_LINE( Str(  3 % -3 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  3 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  2 )                   , "        -1"                      )
   TEST_LINE( Str( -3 %  1 )                   , "         0"                      )
   TEST_LINE( Str( -3 %  0 )                   , "E BASE 1341 Zero divisor % F:S"  )
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
   TEST_LINE( -("1")                          , "E BASE 1080 Argument error - F:S" )

   TEST_LINE( "AA" $ 1                        , "E BASE 1109 Argument error $ F:S" )
   TEST_LINE( scString $ 1                    , "E BASE 1109 Argument error $ F:S" )
   TEST_LINE( 1 $ "AA"                        , "E BASE 1109 Argument error $ F:S" )

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

   TEST_LINE( 1 .AND. 2                       , "E BASE 1066 Argument error conditional " )
   TEST_LINE( NIL .AND. NIL                   , "E BASE 1066 Argument error conditional " )
   TEST_LINE( scString .AND. scString         , "E BASE 1066 Argument error conditional " )
   TEST_LINE( .T. .AND. 1                     , 1                                         )
   TEST_LINE( .T. .AND. 1.567                 , 1.567                                     )
   TEST_LINE( .T. .AND. scString              , "HELLO"                                   )
   TEST_LINE( .T. .AND. SToD("")              , SToD("        ")                          )
   TEST_LINE( .T. .AND. NIL                   , NIL                                       )
   TEST_LINE( .T. .AND. {}                    , "{.[0].}"                                 )
   TEST_LINE( .T. .AND. {|| NIL }             , "{||...}"                                 )
   TEST_LINE( .F. .AND. 1                     , .F.                                       )
   TEST_LINE( .F. .AND. 1.567                 , .F.                                       )
   TEST_LINE( .F. .AND. scString              , .F.                                       )
   TEST_LINE( .F. .AND. SToD("")              , .F.                                       )
   TEST_LINE( .F. .AND. NIL                   , .F.                                       )
   TEST_LINE( .F. .AND. {}                    , .F.                                       )
   TEST_LINE( .F. .AND. {|| NIL }             , .F.                                       )
   TEST_LINE( 1 .AND. .F.                     , .F.                                       )
   TEST_LINE( 1.567 .AND. .F.                 , .F.                                       )
   TEST_LINE( scString .AND. .F.              , .F.                                       )

   TEST_LINE( 1 .OR. 2                        , "E BASE 1066 Argument error conditional " )
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
   TEST_LINE( .T. .AND. SToD("")              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. NIL                   , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {}                    , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .T. .AND. {|| NIL }             , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1                     , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. 1.567                 , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. scString              , "E BASE 1078 Argument error .AND. F:S"    )
   TEST_LINE( .F. .AND. SToD("")              , "E BASE 1078 Argument error .AND. F:S"    )
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
   TEST_LINE( .NOT. 1                         , "E BASE 1077 Argument error .NOT. F:S"    )

#ifndef __HARBOUR__ // this error is reported at compile time
#ifndef __XPP__ // this error is reported at compile time
   TEST_LINE( iif( "A", ":T:", ":F:" )        , "E BASE 1066 Argument error conditional " )
#endif
#endif
   TEST_LINE( iif( .T., ":T:", ":F:" )        , ":T:"                                     )
   TEST_LINE( iif( .F., ":T:", ":F:" )        , ":F:"                                     )

   TEST_LINE( scString++                      , "E BASE 1086 Argument error ++ F:S"       )
   TEST_LINE( scString--                      , "E BASE 1087 Argument error -- F:S"       )

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
   TEST_LINE( saArray[ "1" ]                  , "E BASE 1068 Argument error array access F:S"     )
   TEST_LINE( saArray[ "1" ] := 1             , "E BASE 1069 Argument error array assign "        )
#endif

   /* Alias */

   TEST_LINE( ("NOTHERE")->NOFIELD            , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
   TEST_LINE( (mcString)->NOFIELD             , "E BASE 1002 Alias does not exist HELLO F:R"      )
   TEST_Line( ({})->NOFIELD                   , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( ({|| NIL })->NOFIELD            , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( (.T.)->NOFIELD                  , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( (.F.)->NOFIELD                  , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( (NIL)->NOFIELD                  , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( (2)->NOFIELD                    , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( (2.5)->NOFIELD                  , "E BASE 1003 Variable does not exist NOFIELD F:R" )
   TEST_LINE( (SToD(""))->NOFIELD             , "E BASE 1065 Argument error & F:S"                )
   TEST_LINE( (ErrorNew())->NOFIELD           , "E BASE 1065 Argument error & F:S"                )

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
   TEST_LINE( (SToD(""))->(Eof())             , .T.                                               )
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
   TEST_LINE( NOTHERE->(SToD(""))             , "E BASE 1002 Alias does not exist NOTHERE F:R"    )
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
   TEST_LINE( 200->(SToD(""))                 , SToD("        ")                                  )
   TEST_LINE( 200->(ErrorNew())               , "ERROR Object"                                    )

   TEST_LINE( soObject:hello                  , "E BASE 1004 No exported method HELLO F:S"        )
   TEST_LINE( soObject:hello := 1             , "E BASE 1005 No exported variable HELLO F:S"      )

   /* LEN() */

   TEST_LINE( Len( NIL )                      , "E BASE 1111 Argument error LEN F:S"   )
   TEST_LINE( Len( 123 )                      , "E BASE 1111 Argument error LEN F:S"   )
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
   TEST_LINE( RTSTR( 5000000000 )                     , " 11  5000000000"                    )
   TEST_LINE( RTSTR( 50000000000 )                    , " 12  50000000000"                   )
   TEST_LINE( RTSTR( 500000000000 )                   , " 13  500000000000"                  )
   TEST_LINE( RTSTR( 500000000000.0 )                 , " 14 500000000000.0"                 )
   TEST_LINE( RTSTR( 5000000000000 )                  , " 14  5000000000000"                 )
   TEST_LINE( RTSTR( 50000000000000 )                 , " 15  50000000000000"                )
   TEST_LINE( RTSTR( 500000000000000 )                , " 16  500000000000000"               )
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
