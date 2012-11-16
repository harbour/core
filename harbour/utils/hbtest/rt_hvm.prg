/*
 * $Id$
 */

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

PROCEDURE Main_HVM()

   /* ValType() */

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
   IF TEST_DBFAvail()
   TEST_LINE( ValType( w_TEST->TYPE_C )       , "C"   )
   TEST_LINE( ValType( w_TEST->TYPE_D )       , "D"   )
   TEST_LINE( ValType( w_TEST->TYPE_M )       , "M"   )
   TEST_LINE( ValType( w_TEST->TYPE_N_I )     , "N"   )
   TEST_LINE( ValType( w_TEST->TYPE_N_D )     , "N"   )
   TEST_LINE( ValType( w_TEST->TYPE_L )       , "L"   )
   ENDIF
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

   /* Type() */

#ifndef __XPP__
   TEST_LINE( Type( NIL )                     , "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:U:NIL F:S"     )
   TEST_LINE( Type( 100 )                     , "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:N:100 F:S"     )
   TEST_LINE( Type( {} )                      , "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:A:{.[0].} F:S" )
#endif
   IF TEST_DBFAvail()
   TEST_LINE( Type( "w_TEST->TYPE_C" )        , "C"   )
   TEST_LINE( Type( "w_TEST->TYPE_D" )        , "D"   )
   TEST_LINE( Type( "w_TEST->TYPE_M" )        , "M"   )
   TEST_LINE( Type( "w_TEST->TYPE_N_I" )      , "N"   )
   TEST_LINE( Type( "w_TEST->TYPE_N_D" )      , "N"   )
   TEST_LINE( Type( "w_TEST->TYPE_L" )        , "L"   )
   ENDIF
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
   TEST_LINE( NIL:className()                 , "NIL"       )
#endif
#endif                                                      )
#ifndef __XPP__
   TEST_LINE( "":className()                  , "CHARACTER" )
   TEST_LINE( 0:className()                   , "NUMERIC"   )
   TEST_LINE( hb_SToD( "" ):className()       , "DATE"      )
   TEST_LINE( .F.:className()                 , "LOGICAL"   )
   TEST_LINE( {|| NIL }:className()           , "BLOCK"     )
   TEST_LINE( {}:className()                  , "ARRAY"     )
#endif
   TEST_LINE( ErrorNew():className()          , "ERROR"     )
   TEST_LINE( ErrorNew():className            , "ERROR"     )
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( NIL:classH()                    , 0           )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( "":classH()                     , 0           )
   TEST_LINE( 0:classH()                      , 0           )
   TEST_LINE( hb_SToD( "" ):classH()          , 0           )
   TEST_LINE( .F.:classH()                    , 0           )
   TEST_LINE( {|| NIL }:classH()              , 0           )
   TEST_LINE( {}:classH()                     , 0           )
#endif
   TEST_LINE( ErrorNew():classH() > 0         , .T.         )
   TEST_LINE( ErrorNew():classH > 0           , .T.         )

/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( suNIL:className()               , "NIL"       )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( scString:className()            , "CHARACTER" )
   TEST_LINE( snIntP:className()              , "NUMERIC"   )
   TEST_LINE( sdDateE:className()             , "DATE"      )
   TEST_LINE( slFalse:className()             , "LOGICAL"   )
   TEST_LINE( sbBlock:className()             , "BLOCK"     )
   TEST_LINE( saArray:className()             , "ARRAY"     )
#endif
   TEST_LINE( soObject:className()            , "ERROR"     )
   TEST_LINE( soObject:className              , "ERROR"     )
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   TEST_LINE( suNIL:classH()                  , 0           )
#endif
#endif
#ifndef __XPP__
   TEST_LINE( scString:classH()               , 0           )
   TEST_LINE( snIntP:classH()                 , 0           )
   TEST_LINE( sdDateE:classH()                , 0           )
   TEST_LINE( slFalse:classH()                , 0           )
   TEST_LINE( sbBlock:classH()                , 0           )
   TEST_LINE( saArray:classH()                , 0           )
#endif
   TEST_LINE( soObject:classH() > 0           , .T.         )
   TEST_LINE( soObject:classH > 0             , .T.         )

   /* (operators) */

   /* <= */

   TEST_LINE( 2                   <= 1                   , .F.                                               )
   TEST_LINE( 1                   <= 2                   , .T.                                               )
   TEST_LINE( 2.0                 <= 2                   , .T.                                               )
   TEST_LINE( 2                   <= 2.0                 , .T.                                               )
   TEST_LINE( 2.5                 <= 3.7                 , .T.                                               )
   TEST_LINE( 3.7                 <= 2.5                 , .F.                                               )
   TEST_LINE( 10                  <= 10.50               , .T.                                               )
   TEST_LINE( .F.                 <= .F.                 , .T.                                               )
   TEST_LINE( .T.                 <= .F.                 , .F.                                               )
   TEST_LINE( .F.                 <= .T.                 , .T.                                               )
   TEST_LINE( hb_SToD("")         <= hb_SToD("")         , .T.                                               )
   TEST_LINE( hb_SToD("")         <= hb_SToD("19800101") , .T.                                               )
   TEST_LINE( hb_SToD("19800101") <= hb_SToD("")         , .F.                                               )
   TEST_LINE( ""                  <= "AAA"               , .T.                                               )
   TEST_LINE( "AAA"               <= ""                  , .T.                                               )
   TEST_LINE( "AAA"               <= "AA"                , .T.                                               )
   TEST_LINE( "AAA"               <= Chr(255)            , .T.                                               )
   TEST_LINE( Chr(150)            <= Chr(255)            , .T.                                               )
   TEST_LINE( "A"                 <= "a"                 , .T.                                               )
   TEST_LINE( "A"                 <= "Z"                 , .T.                                               )
   TEST_LINE( "Z"                 <= " "                 , .F.                                               )
   TEST_LINE( Chr(0)              <= " "                 , .T.                                               )
   TEST_LINE( "Hallo"             <= "Hello"             , .T.                                               )
   TEST_LINE( "Hello"             <= "Hello"             , .T.                                               )
   TEST_LINE( "Hell"              <= "Hello"             , .T.                                               )
   TEST_LINE( "Hellow"            <= "Hello"             , .T.                                               )
   TEST_LINE( "J"                 <= "Hello"             , .F.                                               )
   TEST_LINE( ""                  <= "Hello"             , .T.                                               )
   TEST_LINE( "J"                 <= ""                  , .T.                                               )
   TEST_LINE( ""                  <= ""                  , .T.                                               )

   /* < */

   TEST_LINE( 2                   <  1                   , .F.                                               )
   TEST_LINE( 1                   <  2                   , .T.                                               )
   TEST_LINE( 2.0                 <  2                   , .F.                                               )
   TEST_LINE( 2                   <  2.0                 , .F.                                               )
   TEST_LINE( 2.5                 <  3.7                 , .T.                                               )
   TEST_LINE( 3.7                 <  2.5                 , .F.                                               )
   TEST_LINE( 10.50               <  10                  , .F.                                               )
   TEST_LINE( .F.                 <  .F.                 , .F.                                               )
   TEST_LINE( .T.                 <  .F.                 , .F.                                               )
   TEST_LINE( .F.                 <  .T.                 , .T.                                               )
   TEST_LINE( hb_SToD("")         <  hb_SToD("")         , .F.                                               )
   TEST_LINE( hb_SToD("")         <  hb_SToD("19800101") , .T.                                               )
   TEST_LINE( hb_SToD("19800101") <  hb_SToD("")         , .F.                                               )
   TEST_LINE( ""                  <  "AAA"               , .T.                                               )
   TEST_LINE( "AAA"               <  ""                  , .F.                                               )
   TEST_LINE( "AAA"               <  "AA"                , .F.                                               )
   TEST_LINE( "AAA"               <  Chr(255)            , .T.                                               )
   TEST_LINE( Chr(150)            <  Chr(255)            , .T.                                               )
   TEST_LINE( "A"                 <  "a"                 , .T.                                               )
   TEST_LINE( "A"                 <  "Z"                 , .T.                                               )
   TEST_LINE( "Z"                 <  "A"                 , .F.                                               )
   TEST_LINE( Chr(0)              <  " "                 , .T.                                               )
   TEST_LINE( "Hallo"             <  "Hello"             , .T.                                               )
   TEST_LINE( "Hello"             <  "Hello"             , .F.                                               )
   TEST_LINE( "Hell"              <  "Hello"             , .T.                                               )
   TEST_LINE( "Hellow"            <  "Hello"             , .F.                                               )
   TEST_LINE( "J"                 <  "Hello"             , .F.                                               )
   TEST_LINE( ""                  <  "Hello"             , .T.                                               )
   TEST_LINE( "J"                 <  ""                  , .F.                                               )
   TEST_LINE( ""                  <  ""                  , .F.                                               )

   /* >= */

   TEST_LINE( 2                   >= 1                   , .T.                                               )
   TEST_LINE( 1                   >= 2                   , .F.                                               )
   TEST_LINE( 2.0                 >= 2                   , .T.                                               )
   TEST_LINE( 2                   >= 2.0                 , .T.                                               )
   TEST_LINE( 2.5                 >= 3.7                 , .F.                                               )
   TEST_LINE( 3.7                 >= 2.5                 , .T.                                               )
   TEST_LINE( 10.50               >= 10                  , .T.                                               )
   TEST_LINE( .F.                 >= .F.                 , .T.                                               )
   TEST_LINE( .T.                 >= .F.                 , .T.                                               )
   TEST_LINE( .F.                 >= .T.                 , .F.                                               )
   TEST_LINE( hb_SToD("")         >= hb_SToD("")         , .T.                                               )
   TEST_LINE( hb_SToD("")         >= hb_SToD("19800101") , .F.                                               )
   TEST_LINE( hb_SToD("19800101") >= hb_SToD("")         , .T.                                               )
   TEST_LINE( ""                  >= "AAA"               , .F.                                               )
   TEST_LINE( "AAA"               >= ""                  , .T.                                               )
   TEST_LINE( "AAA"               >= "AA"                , .T.                                               )
   TEST_LINE( "AAA"               >= Chr(255)            , .F.                                               )
   TEST_LINE( Chr(150)            >= Chr(255)            , .F.                                               )
   TEST_LINE( "A"                 >= "a"                 , .F.                                               )
   TEST_LINE( "A"                 >= "Z"                 , .F.                                               )
   TEST_LINE( "Z"                 >= "A"                 , .T.                                               )
   TEST_LINE( Chr(0)              >= " "                 , .F.                                               )
   TEST_LINE( "Hallo"             >= "Hello"             , .F.                                               )
   TEST_LINE( "Hello"             >= "Hello"             , .T.                                               )
   TEST_LINE( "Hell"              >= "Hello"             , .F.                                               )
   TEST_LINE( "Hellow"            >= "Hello"             , .T.                                               )
   TEST_LINE( "J"                 >= "Hello"             , .T.                                               )
   TEST_LINE( ""                  >= "Hello"             , .F.                                               )
   TEST_LINE( "J"                 >= ""                  , .T.                                               )
   TEST_LINE( ""                  >= ""                  , .T.                                               )

   /* > */

   TEST_LINE( 2                   >  1                   , .T.                                               )
   TEST_LINE( 1                   >  2                   , .F.                                               )
   TEST_LINE( 2.0                 >  2                   , .F.                                               )
   TEST_LINE( 2                   >  2.0                 , .F.                                               )
   TEST_LINE( 2.5                 >  3.7                 , .F.                                               )
   TEST_LINE( 3.7                 >  2.5                 , .T.                                               )
   TEST_LINE( 10.50               >  10                  , .T.                                               )
   TEST_LINE( .F.                 >  .F.                 , .F.                                               )
   TEST_LINE( .T.                 >  .F.                 , .T.                                               )
   TEST_LINE( .F.                 >  .T.                 , .F.                                               )
   TEST_LINE( hb_SToD("")         >  hb_SToD("")         , .F.                                               )
   TEST_LINE( hb_SToD("")         >  hb_SToD("19800101") , .F.                                               )
   TEST_LINE( hb_SToD("19800101") >  hb_SToD("")         , .T.                                               )
   TEST_LINE( ""                  >  "AAA"               , .F.                                               )
   TEST_LINE( "AAA"               >  ""                  , .F.                                               )
   TEST_LINE( "AAA"               >  "AA"                , .F.                                               )
   TEST_LINE( "AAA"               >  Chr(255)            , .F.                                               )
   TEST_LINE( Chr(150)            >  Chr(255)            , .F.                                               )
   TEST_LINE( "A"                 >  "a"                 , .F.                                               )
   TEST_LINE( "A"                 >  "Z"                 , .F.                                               )
   TEST_LINE( "Z"                 >  "A"                 , .T.                                               )
   TEST_LINE( Chr(0)              >  " "                 , .F.                                               )
   TEST_LINE( "Hallo"             >  "Hello"             , .F.                                               )
   TEST_LINE( "Hello"             >  "Hello"             , .F.                                               )
   TEST_LINE( "Hell"              >  "Hello"             , .F.                                               )
   TEST_LINE( "Hellow"            >  "Hello"             , .F.                                               )
   TEST_LINE( "J"                 >  "Hello"             , .T.                                               )
   TEST_LINE( ""                  >  "Hello"             , .F.                                               )
   TEST_LINE( "J"                 >  ""                  , .F.                                               )
   TEST_LINE( ""                  >  ""                  , .F.                                               )

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
   TEST_LINE( "A" == 1                        , "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString == 1                   , "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject == ""                  , "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject == soObject            , .T.                                               )
   TEST_LINE( soObject == ErrorNew()          , .F.                                               )
   TEST_LINE( ErrorNew() == ErrorNew()        , .F.                                               )
   TEST_LINE( soObject == TBColumnNew()       , .F.                                               )
   TEST_LINE( saArray == saArray              , .T.                                               )
   TEST_LINE( {} == {}                        , .F.                                               )
   TEST_LINE( {|| NIL } == {|| NIL }          , "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* = special */

   TEST_LINE( NIL = NIL                       , .T.                                               )
   TEST_LINE( scString = NIL                  , .F.                                               )
   TEST_LINE( "A" = 1                         , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString = 1                    , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject = ""                   , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject = soObject             , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject = ErrorNew()           , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() = ErrorNew()         , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject = TBColumnNew()        , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray = saArray               , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} = {}                         , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } = {|| NIL }           , "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* != special */

   TEST_LINE( NIL != NIL                      , .F.                                               )
   TEST_LINE( scString != NIL                 , .T.                                               )
   TEST_LINE( "A" != 1                        , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString != 1                   , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject != ""                  , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject != soObject            , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject != ErrorNew()          , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() != ErrorNew()        , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject != TBColumnNew()       , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray != saArray              , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} != {}                        , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } != {|| NIL }          , "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* < special */

   TEST_LINE( NIL < NIL                       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( scString < NIL                  , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:HELLO;U:NIL F:S" )
   TEST_LINE( "A" < 1                         , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString < 1                    , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject < ""                   , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject < soObject             , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject < ErrorNew()           , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() < ErrorNew()         , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject < TBColumnNew()        , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray < saArray               , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} < {}                         , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } < {|| NIL }           , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* <= special */

   TEST_LINE( NIL <= NIL                      , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( scString <= NIL                 , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:HELLO;U:NIL F:S" )
   TEST_LINE( "A" <= 1                        , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString <= 1                   , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject <= ""                  , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject <= soObject            , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject <= ErrorNew()          , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() <= ErrorNew()        , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject <= TBColumnNew()       , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray <= saArray              , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} <= {}                        , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } <= {|| NIL }          , "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* > special */

   TEST_LINE( NIL > NIL                       , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( scString > NIL                  , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:HELLO;U:NIL F:S" )
   TEST_LINE( "A" > 1                         , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString > 1                    , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject > ""                   , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject > soObject             , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject > ErrorNew()           , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() > ErrorNew()         , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject > TBColumnNew()        , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray > saArray               , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} > {}                         , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } > {|| NIL }           , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   /* >= special */

   TEST_LINE( NIL >= NIL                      , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( scString >= NIL                 , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:HELLO;U:NIL F:S" )
   TEST_LINE( "A" >= 1                        , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:A;N:1 F:S" )
   TEST_LINE( scString >= 1                   , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:HELLO;N:1 F:S" )
   TEST_LINE( soObject >= ""                  , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;C: F:S" )
   TEST_LINE( soObject >= soObject            , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject >= ErrorNew()          , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( ErrorNew() >= ErrorNew()        , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S" )
   TEST_LINE( soObject >= TBColumnNew()       , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S" )
   TEST_LINE( saArray >= saArray              , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S" )
   TEST_LINE( {} >= {}                        , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S" )
   TEST_LINE( {|| NIL } >= {|| NIL }          , "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S" )

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
