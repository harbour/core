/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (array)
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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

STATIC scString
STATIC scStringM
STATIC scStringE
STATIC scStringZ
STATIC scStringW
STATIC snIntZ
STATIC snDoubleZ
STATIC snIntP
STATIC snIntP1
STATIC snLongP
STATIC snDoubleP
STATIC snIntN
STATIC snLongN
STATIC snDoubleN
STATIC snDoubleI
STATIC sdDate
STATIC sdDateE
STATIC slFalse
STATIC slTrue
STATIC soObject
STATIC suNIL
STATIC sbBlock
STATIC sbBlockC
STATIC saArray
STATIC saAllTypes

MEMVAR mxNotHere /* Please don't declare this variable, since it's used to test undeclared MEMVAR situations. */
MEMVAR mcLongerNameThen10Chars
MEMVAR mcString
MEMVAR mcStringE
MEMVAR mcStringZ
MEMVAR mcStringW
MEMVAR mnIntZ
MEMVAR mnDoubleZ
MEMVAR mnIntP
MEMVAR mnLongP
MEMVAR mnDoubleP
MEMVAR mnDoubleI
MEMVAR mnIntN
MEMVAR mnLongN
MEMVAR mnDoubleN
MEMVAR mdDate
MEMVAR mdDateE
MEMVAR mlFalse
MEMVAR mlTrue
MEMVAR moObject
MEMVAR muNIL
MEMVAR mbBlock
MEMVAR mbBlockC
MEMVAR maArray

INIT PROCEDURE RT_InitStatics()

   /* NOTE: Some basic values we may need for some tests.
            ( passing by reference, avoid preprocessor bugs, etc. ) */

   scString  := "HELLO"
   scStringM := "Hello"
   scStringE := ""
   scStringZ := "A" + Chr( 0 ) + "B"
   scStringW := Chr(13)+Chr(10)+Chr(141)+Chr(10)+Chr(9)
   snIntZ    := 0
   snDoubleZ := 0.0
   snIntP    := 10
   snIntP1   := 65
   snLongP   := 100000
   snDoubleP := 10.567 /* Use different number of decimals than the default */
   snIntN    := -10
   snLongN   := -100000
   snDoubleN := -10.567 /* Use different number of decimals than the default */
   snDoubleI := 0   //Log( 0 )
   sdDate    := SToD( "19840325" )
   sdDateE   := SToD( "" )
   slFalse   := .F.
   slTrue    := .T.
   soObject  := ErrorNew()
   suNIL     := NIL
   sbBlock   := {|| NIL }
   sbBlockC  := {|| "(string)" }
   saArray   := { 9898 }

   saAllTypes := {;
      scString  ,;
      scStringE ,;
      scStringZ ,;
      snIntZ    ,;
      snDoubleZ ,;
      snIntP    ,;
      snLongP   ,;
      snDoubleP ,;
      snIntN    ,;
      snLongN   ,;
      snDoubleN ,;
      snDoubleI ,;
      sdDateE   ,;
      slFalse   ,;
      slTrue    ,;
      soObject  ,;
      suNIL     ,;
      sbBlock   ,;
      sbBlockC  ,;
      saArray   }

   RETURN

FUNCTION Main_ARRAY()

   /* ARRAY function error conditions. */

   TEST_LINE( aCopy()                         , NIL                                        )
   TEST_LINE( aCopy({}, "C")                  , NIL                                        )
   TEST_LINE( aCopy("C", {})                  , NIL                                        )
   TEST_LINE( aCopy({}, {})                   , "{.[0].}"                                  )
   TEST_LINE( aCopy({}, ErrorNew())           , "ERROR Object"                             )
   TEST_LINE( aCopy(ErrorNew(), {})           , "{.[0].}"                                  )
   TEST_LINE( aClone()                        , NIL                                        )
   TEST_LINE( aClone( NIL )                   , NIL                                        )
   TEST_LINE( aClone( {} )                    , "{.[0].}"                                  )
   TEST_LINE( aClone( ErrorNew() )            , NIL                                        )
   TEST_LINE( aEval()                         , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aEval( NIL )                    , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aEval( {} )                     , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aEval( {}, NIL )                , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aEval( {}, {|| NIL } )          , "{.[0].}"                                  )
   TEST_LINE( aEval( ErrorNew(), {|| NIL } )  , "ERROR Object"                             )
   TEST_LINE( aScan()                         , 0                                          )
   TEST_LINE( aScan( NIL )                    , 0                                          )
   TEST_LINE( aScan( "A" )                    , 0                                          )
   TEST_LINE( aScan( {} )                     , 0                                          )
   TEST_LINE( aScan( {}, "" )                 , 0                                          )
   TEST_LINE( aScan( ErrorNew(), "NOT_FOUND") , 0                                          )
   TEST_LINE( aSort()                         , NIL                                        )
   TEST_LINE( aSort(10)                       , NIL                                        )
   TEST_LINE( aSort({})                       , "{.[0].}"                                  )
   TEST_LINE( aSort(ErrorNew())               , NIL                                        )
   TEST_LINE( aFill()                         , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aFill( NIL )                    , "E BASE 2017 Argument error AEVAL "        )
   TEST_LINE( aFill( {} )                     , "{.[0].}"                                  )
   TEST_LINE( aFill( {}, 1 )                  , "{.[0].}"                                  )
   TEST_LINE( aFill( ErrorNew() )             , "ERROR Object"                             )
   TEST_LINE( aFill( ErrorNew(), 1 )          , "ERROR Object"                             )
   TEST_LINE( aDel()                          , NIL                                        )
   TEST_LINE( aDel( NIL )                     , NIL                                        )
   TEST_LINE( aDel( { 1 } )                   , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, 100 )              , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, 1 )                , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, -1 )               , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( aDel( { 1 }, NIL )              , "{.[1].}"                                  )
   TEST_LINE( aDel( ErrorNew() )              , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), 100 )         , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), 1 )           , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), -1 )          , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( aDel( ErrorNew(), NIL )         , "ERROR Object"                             )
   TEST_LINE( aIns()                          , NIL                                        )
   TEST_LINE( aIns( NIL )                     , NIL                                        )
   TEST_LINE( aIns( { 1 } )                   , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, 100 )              , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, 1 )                , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, -1 )               , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( aIns( { 1 }, NIL )              , "{.[1].}"                                  )
   TEST_LINE( aIns( ErrorNew() )              , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), 100 )         , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), 1 )           , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), -1 )          , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( aIns( ErrorNew(), NIL )         , "ERROR Object"                             )
   TEST_LINE( aTail()                         , NIL                                        )
   TEST_LINE( aTail( NIL )                    , NIL                                        )
   TEST_LINE( aTail( "" )                     , NIL                                        )
   TEST_LINE( aTail( {} )                     , NIL                                        )
   TEST_LINE( aTail( { 1, 2 } )               , 2                                          )
   TEST_LINE( aTail( ErrorNew() )             , NIL                                        )
   TEST_LINE( aSize()                         , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( NIL )                    , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( {} )                     , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( ErrorNew() )             , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( NIL, 0 )                 , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( {}, 0 )                  , "{.[0].}"                                  )
   TEST_LINE( aSize( ErrorNew(), 0 )          , "ERROR Object"                             )
   TEST_LINE( aSize( NIL, 1 )                 , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( {}, 1 )                  , "{.[1].}"                                  )
   TEST_LINE( aSize( { 1, 2 }, 1 )            , "{.[1].}"                                  )
   TEST_LINE( aSize( { 1, "AAAA" }, 1 )       , "{.[1].}"                                  )
   TEST_LINE( aSize( { "BBB", "AAAA" }, 0 )   , "{.[0].}"                                  )
   TEST_LINE( aSize( ErrorNew(), 1 )          , "ERROR Object"                             )
   TEST_LINE( aSize( NIL, -1 )                , "E BASE 2023 Argument error ASIZE "        )
   TEST_LINE( aSize( {}, -1 )                 , "{.[0].}"                                  )
   TEST_LINE( aSize( { 1 }, -1 )              , "{.[0].}"                                  )
#ifdef __HARBOUR__
   TEST_LINE( aSize( { 1 }, 5000 )            , "{.[5000].}"                               )
#else
   TEST_LINE( aSize( { 1 }, 5000 )            , "{.[1].}"                                  )
#endif
   TEST_LINE( aSize( ErrorNew(), -1 )         , "ERROR Object"                             )
   TEST_LINE( aSize( ErrorNew(), 100 )        , "ERROR Object"                             )
   TEST_LINE( aAdd( NIL, NIL )                , "E BASE 1123 Argument error AADD F:S"      )
   TEST_LINE( aAdd( {}, NIL )                 , NIL                                        )
   TEST_LINE( aAdd( {}, "A" )                 , "A"                                        )
   TEST_LINE( aAdd( ErrorNew(), NIL )         , NIL                                        )
   TEST_LINE( aAdd( ErrorNew(), "A" )         , "A"                                        )
   TEST_LINE( Array()                         , NIL                                        )
   TEST_LINE( Array( 0 )                      , "{.[0].}"                                  )
#ifdef __HARBOUR__
   TEST_LINE( Array( 5000 )                   , "{.[5000].}"                               )
#else
   TEST_LINE( Array( 5000 )                   , "E BASE 1131 Bound error array dimension " )
#endif
   TEST_LINE( Array( 1 )                      , "{.[1].}"                                  )
   TEST_LINE( Array( -1 )                     , "E BASE 1131 Bound error array dimension " )
   TEST_LINE( Array( 1, 0, -10 )              , "E BASE 1131 Bound error array dimension " )
   TEST_LINE( Array( 1, 0, "A" )              , NIL                                        )
   TEST_LINE( Array( 1, 0, 2 )                , "{.[1].}"                                  )
   TEST_LINE( Array( 4, 3, 2 )                , "{.[4].}"                                  )
   TEST_LINE( Array( 0, 3, 2 )                , "{.[0].}"                                  )

   /* AFILL() */

   TEST_LINE( TAStr(aFill(TANew(),"X")       ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",NIL,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",NIL, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",NIL, 3)) , "XXX......."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",NIL,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  0)   ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  0,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  0, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  0, 3)) , "XXX......."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  0,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  1)   ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  1,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  1, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  1, 3)) , "XXX......."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  1,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  3)   ) , "..XXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  3,-2)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  3, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  3, 3)) , "..XXX....."     )
   TEST_LINE( TAStr(aFill(TANew(),"X",  3,20)) , "..XXXXXXXX"     )
   TEST_LINE( TAStr(aFill(TANew(),"X", -1)   ) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", -1,-2)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", -1, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", -1, 3)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", -1,20)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", 21)   ) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", 21,-2)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", 21, 0)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", 21, 3)) , ".........."     )
   TEST_LINE( TAStr(aFill(TANew(),"X", 21,20)) , ".........."     )

   /* ACOPY() */

   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1        )) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  0    )) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  3    )) , "ABC......."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1, 20    )) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3        )) , "CDEFGHIJ.."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  0    )) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  3    )) , "CDE......."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3, 20    )) , "CDEFGHIJ.."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21        )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  0    )) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  3    )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21, 20    )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,NIL,  1)) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  0,  1)) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  3,  0)) , "ABC......."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  3,  2)) , ".ABC......"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  3,  8)) , ".......ABC"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1,  3, 20)) , ".........A"     ) /* Strange in CA-Cl*pper, it should return: ".........." */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  1, 20,  1)) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,NIL,  3)) , "..CDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  0,  3)) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  3,  0)) , "CDE......."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  3,  2)) , ".CDE......"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  3,  8)) , ".......CDE"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3,  3, 20)) , ".........C"     ) /* Strange in CA-Cl*pper, it should return: ".........." */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(),  3, 20,  3)) , "..CDEFGHIJ"     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,NIL, 21)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  0, 21)) , ".........."     )
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  3,  0)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  3,  2)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".J........", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  3,  8)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".......J..", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21,  3, 20)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   TEST_LINE( TAStr(aCopy(TARng(),TANew(), 21, 20, 21)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */

   /* ASORT() */

   TEST_LINE( TAStr(aSort(TARRv(),,,{||NIL})) , "ABCDEFGHIJ"     ) /* Bug/Feature in CA-Cl*pper, it will return: "IHGFEDCBAJ" */
   TEST_LINE( TAStr(aSort(TARRv()))           , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),NIL,NIL))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),NIL, -2))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),NIL,  0))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),NIL,  3))   , "HIJGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(),NIL, 20))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(), -5    ))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), -5, -2))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), -5,  0))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), -5,  3))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), -5, 20))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(),  0    ))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),  0, -2))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),  0,  0))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),  0,  3))   , "HIJGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(),  0, 20))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(aSort(TARRv(),  5    ))   , "JIHGABCDEF"     )
#ifdef __HARBOUR__
   TEST_LINE( TAStr(aSort(TARRv(),  5, -2))   , "JIHGABCDEF"     ) /* CA-Cl*pper will crash or GPF on that line. */
#endif
   TEST_LINE( TAStr(aSort(TARRv(),  5,  0))   , "JIHGABCDEF"     )
   TEST_LINE( TAStr(aSort(TARRv(),  5,  3))   , "JIHGDEFCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(),  5, 20))   , "JIHGABCDEF"     )
   TEST_LINE( TAStr(aSort(TARRv(), 20    ))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), 20, -2))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), 20,  0))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), 20,  3))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(aSort(TARRv(), 20, 20))   , "JIHGFEDCBA"     )

   /* ASCAN() */

   TEST_LINE( aScan()                         , 0           )
   TEST_LINE( aScan( NIL )                    , 0           )
   TEST_LINE( aScan( "A" )                    , 0           )
   TEST_LINE( aScan( "A", "A" )               , 0           )
   TEST_LINE( aScan( "A", {|| .F. } )         , 0           )
   TEST_LINE( aScan( {1,2,3}, {|x| NIL } )    , 0           )
   TEST_LINE( aScan( saAllTypes, scString   ) , 1           )
#ifdef __HARBOUR__
   TEST_LINE( aScan( @saAllTypes, scString )  , 1           ) /* Bug in CA-Cl*pper, it will return 0 */
   TEST_LINE( aScan( saAllTypes, @scString )  , 1           ) /* Bug in CA-Cl*pper, it will return 0 */
#endif
   TEST_LINE( aScan( saAllTypes, scStringE  ) , 1           )
   TEST_LINE( aScan( saAllTypes, scStringZ  ) , 3           )
   TEST_LINE( aScan( saAllTypes, snIntZ     ) , 4           )
   TEST_LINE( aScan( saAllTypes, snDoubleZ  ) , 4           )
   TEST_LINE( aScan( saAllTypes, snIntP     ) , 6           )
   TEST_LINE( aScan( saAllTypes, snLongP    ) , 7           )
   TEST_LINE( aScan( saAllTypes, snDoubleP  ) , 8           )
   TEST_LINE( aScan( saAllTypes, snIntN     ) , 9           )
   TEST_LINE( aScan( saAllTypes, snLongN    ) , 10          )
   TEST_LINE( aScan( saAllTypes, snDoubleN  ) , 11          )
   TEST_LINE( aScan( saAllTypes, snDoubleI  ) , 4           )
   TEST_LINE( aScan( saAllTypes, sdDateE    ) , 13          )
   TEST_LINE( aScan( saAllTypes, slFalse    ) , 14          )
   TEST_LINE( aScan( saAllTypes, slTrue     ) , 15          )
   TEST_LINE( aScan( saAllTypes, soObject   ) , 0           )
   TEST_LINE( aScan( saAllTypes, suNIL      ) , 17          )
   TEST_LINE( aScan( saAllTypes, sbBlock    ) , 0           )
   TEST_LINE( aScan( saAllTypes, sbBlockC   ) , 0           )
   TEST_LINE( aScan( saAllTypes, saArray    ) , 0           )
   SET EXACT ON
   TEST_LINE( aScan( saAllTypes, scString   ) , 1           )
   TEST_LINE( aScan( saAllTypes, scStringE  ) , 2           )
   TEST_LINE( aScan( saAllTypes, scStringZ  ) , 3           )
   SET EXACT OFF

   RETURN NIL

STATIC FUNCTION TANew( cChar, nLen )
   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   IF cChar == NIL
      cChar := "."
   ENDIF

   aArray := Array( nLen )

   /* Intentionally not using aFill() here, since this function is
      involved in testing aFill() itself. */
   FOR tmp := 1 TO nLen
      aArray[ tmp ] := cChar
   NEXT

   RETURN aArray

STATIC FUNCTION TARng( nLen )
   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   aArray := Array( nLen )

   FOR tmp := 1 TO nLen
      aArray[ tmp ] := Chr( Asc( "A" ) + tmp - 1 )
   NEXT

   RETURN aArray

STATIC FUNCTION TARRv( nLen )
   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   aArray := Array( nLen )

   FOR tmp := 1 TO nLen
      aArray[ tmp ] := Chr( Asc( "A" ) + nLen - tmp )
   NEXT

   RETURN aArray

STATIC FUNCTION TAStr( aArray )
   LOCAL cString := ""
   LOCAL tmp

   FOR tmp := 1 TO Len( aArray )
      cString += aArray[ tmp ]
   NEXT

   RETURN cString
