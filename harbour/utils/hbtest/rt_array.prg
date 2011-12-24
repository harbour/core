/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (array)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.hu)
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

PROCEDURE Main_ARRAY()

   /* ARRAY function error conditions. */

#ifndef __XPP__
   TEST_LINE( ACopy()                         , NIL                                        )
#endif
   TEST_LINE( ACopy({}, "C")                  , NIL                                        )
   TEST_LINE( ACopy("C", {})                  , NIL                                        )
   TEST_LINE( ACopy({}, {})                   , "{.[0].}"                                  )
   TEST_LINE( ACopy({}, ErrorNew())           , "ERROR Object"                             )
   TEST_LINE( ACopy(ErrorNew(), {})           , "{.[0].}"                                  )
#ifndef __XPP__
   TEST_LINE( AClone()                        , NIL                                        )
#endif
   TEST_LINE( AClone( NIL )                   , NIL                                        )
   TEST_LINE( AClone( {} )                    , "{.[0].}"                                  )
   TEST_LINE( AClone( ErrorNew() )            , NIL                                        )
#ifndef __XPP__
   TEST_LINE( AEval()                         , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 " )
   TEST_LINE( AEval( NIL )                    , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:1:U:NIL " )
   TEST_LINE( AEval( {} )                     , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:1:A:{.[0].} " )
#endif
   TEST_LINE( AEval( {}, NIL )                , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:2:A:{.[0].};U:NIL " )
   TEST_LINE( AEval( {}, {|| NIL } )          , "{.[0].}"                                  )
   TEST_LINE( AEval( ErrorNew(), {|| NIL } )  , "ERROR Object"                             )
#ifndef __XPP__
   TEST_LINE( AScan()                         , 0                                          )
   TEST_LINE( AScan( NIL )                    , 0                                          )
   TEST_LINE( AScan( "A" )                    , 0                                          )
   TEST_LINE( AScan( {} )                     , 0                                          )
#endif
   TEST_LINE( AScan( {}, "" )                 , 0                                          )
   TEST_LINE( AScan( ErrorNew(), "NOT_FOUND") , 0                                          )
#ifndef __XPP__
   TEST_LINE( ASort()                         , NIL                                        )
#endif
   TEST_LINE( ASort(10)                       , NIL                                        )
   TEST_LINE( ASort({})                       , "{.[0].}"                                  )
   TEST_LINE( ASort(ErrorNew())               , NIL                                        )
#ifdef HB_CLP_STRICT
#ifndef __XPP__
   TEST_LINE( AFill()                         , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:4:U:NIL;B:{||...};U:NIL;U:NIL " )
#endif
   TEST_LINE( AFill( NIL )                    , "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:4:U:NIL;B:{||...};U:NIL;U:NIL " )
#else
#ifndef __XPP__
   TEST_LINE( AFill()                         , "E 1 BASE 6004 Argument error (AFILL) OS:0 #:0 " )
#endif
   TEST_LINE( AFill( NIL )                    , "E 1 BASE 6004 Argument error (AFILL) OS:0 #:0 A:1:U:NIL " )
#endif
   TEST_LINE( AFill( {} )                     , "{.[0].}"                                  )
   TEST_LINE( AFill( {}, 1 )                  , "{.[0].}"                                  )
   TEST_LINE( AFill( ErrorNew() )             , "ERROR Object"                             )
   TEST_LINE( AFill( ErrorNew(), 1 )          , "ERROR Object"                             )
#ifndef __XPP__
   TEST_LINE( ADel()                          , NIL                                        )
   TEST_LINE( ADel( NIL )                     , NIL                                        )
   TEST_LINE( ADel( { 1 } )                   , "{.[1].}"                                  )
#endif
   TEST_LINE( ADel( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( ADel( { 1 }, 100 )              , "{.[1].}"                                  )
   TEST_LINE( ADel( { 1 }, 1 )                , "{.[1].}"                                  )
   TEST_LINE( ADel( { 1 }, -1 )               , "{.[1].}"                                  )
   TEST_LINE( ADel( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( ADel( { 1 }, NIL )              , "{.[1].}"                                  )
#ifndef __XPP__
   TEST_LINE( ADel( ErrorNew() )              , "ERROR Object"                             )
#endif
   TEST_LINE( ADel( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( ADel( ErrorNew(), 100 )         , "ERROR Object"                             )
   TEST_LINE( ADel( ErrorNew(), 1 )           , "ERROR Object"                             )
   TEST_LINE( ADel( ErrorNew(), -1 )          , "ERROR Object"                             )
   TEST_LINE( ADel( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( ADel( ErrorNew(), NIL )         , "ERROR Object"                             )
#ifndef __XPP__
   TEST_LINE( AIns()                          , NIL                                        )
   TEST_LINE( AIns( NIL )                     , NIL                                        )
   TEST_LINE( AIns( { 1 } )                   , "{.[1].}"                                  )
#endif
   TEST_LINE( AIns( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( AIns( { 1 }, 100 )              , "{.[1].}"                                  )
   TEST_LINE( AIns( { 1 }, 1 )                , "{.[1].}"                                  )
   TEST_LINE( AIns( { 1 }, -1 )               , "{.[1].}"                                  )
   TEST_LINE( AIns( { 1 }, 0 )                , "{.[1].}"                                  )
   TEST_LINE( AIns( { 1 }, NIL )              , "{.[1].}"                                  )
#ifndef __XPP__
   TEST_LINE( AIns( ErrorNew() )              , "ERROR Object"                             )
#endif
   TEST_LINE( AIns( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( AIns( ErrorNew(), 100 )         , "ERROR Object"                             )
   TEST_LINE( AIns( ErrorNew(), 1 )           , "ERROR Object"                             )
   TEST_LINE( AIns( ErrorNew(), -1 )          , "ERROR Object"                             )
   TEST_LINE( AIns( ErrorNew(), 0 )           , "ERROR Object"                             )
   TEST_LINE( AIns( ErrorNew(), NIL )         , "ERROR Object"                             )
#ifndef __XPP__
   TEST_LINE( ATail()                         , NIL                                        )
#endif
   TEST_LINE( ATail( NIL )                    , NIL                                        )
   TEST_LINE( ATail( "" )                     , NIL                                        )
   TEST_LINE( ATail( {} )                     , NIL                                        )
   TEST_LINE( ATail( { 1, 2 } )               , 2                                          )
   TEST_LINE( ATail( ErrorNew() )             , NIL                                        )
#ifndef __XPP__
#ifdef HB_COMPAT_C53
   TEST_LINE( ASize()                         , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
   TEST_LINE( ASize( NIL )                    , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
   TEST_LINE( ASize( {} )                     , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
   TEST_LINE( ASize( ErrorNew() )             , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
#else
   TEST_LINE( ASize()                         , NIL                                        )
   TEST_LINE( ASize( NIL )                    , NIL                                        )
   TEST_LINE( ASize( {} )                     , NIL                                        )
   TEST_LINE( ASize( ErrorNew() )             , NIL                                        )
#endif
#endif
#ifdef HB_COMPAT_C53
   TEST_LINE( ASize( NIL, 0 )                 , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
   TEST_LINE( ASize( NIL, 1 )                 , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
   TEST_LINE( ASize( NIL, -1 )                , "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 " )
#else
   TEST_LINE( ASize( NIL, 0 )                 , NIL                                        )
   TEST_LINE( ASize( NIL, 1 )                 , NIL                                        )
   TEST_LINE( ASize( NIL, -1 )                , NIL                                        )
#endif
   TEST_LINE( ASize( {}, 0 )                  , "{.[0].}"                                  )
   TEST_LINE( ASize( ErrorNew(), 0 )          , "ERROR Object"                             )
   TEST_LINE( ASize( {}, 1 )                  , "{.[1].}"                                  )
   TEST_LINE( ASize( { 1, 2 }, 1 )            , "{.[1].}"                                  )
   TEST_LINE( ASize( { 1, "AAAA" }, 1 )       , "{.[1].}"                                  )
   TEST_LINE( ASize( { "BBB", "AAAA" }, 0 )   , "{.[0].}"                                  )
   TEST_LINE( ASize( ErrorNew(), 1 )          , "ERROR Object"                             )
   TEST_LINE( ASize( {}, -1 )                 , "{.[0].}"                                  )
   TEST_LINE( ASize( { 1 }, -1 )              , "{.[0].}"                                  )
#ifdef __HARBOUR__
   TEST_LINE( ASize( { 1 }, 5000 )            , "{.[5000].}"                               )
#else
   TEST_LINE( ASize( { 1 }, 5000 )            , "{.[1].}"                                  )
#endif
   TEST_LINE( ASize( ErrorNew(), -1 )         , "ERROR Object"                             )
   TEST_LINE( ASize( ErrorNew(), 100 )        , "ERROR Object"                             )
   TEST_LINE( AAdd( NIL, NIL )                , "E 1 BASE 1123 Argument error (AADD) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( AAdd( {}, NIL )                 , NIL                                        )
   TEST_LINE( AAdd( {}, "A" )                 , "A"                                        )
   TEST_LINE( AAdd( ErrorNew(), NIL )         , NIL                                        )
   TEST_LINE( AAdd( ErrorNew(), "A" )         , "A"                                        )
#ifndef __XPP__
   TEST_LINE( Array()                         , NIL                                        )
#endif
   TEST_LINE( Array( 0 )                      , "{.[0].}"                                  )
#ifdef __HARBOUR__
   TEST_LINE( Array( 5000 )                   , "{.[5000].}"                               )
#else
   TEST_LINE( Array( 5000 )                   , "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 " )
#endif
   TEST_LINE( Array( 1 )                      , "{.[1].}"                                  )
   TEST_LINE( Array( -1 )                     , "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 " )
   TEST_LINE( Array( 1, 0, -10 )              , "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 " )
   TEST_LINE( Array( 1, 0, "A" )              , NIL                                        )
   TEST_LINE( Array( 1, 0, 2 )                , "{.[1].}"                                  )
   TEST_LINE( Array( 4, 3, 2 )                , "{.[4].}"                                  )
   TEST_LINE( Array( 0, 3, 2 )                , "{.[0].}"                                  )

   /* AFill() */

   TEST_LINE( TAStr(AFill(TANew(),"X")       ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",NIL,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",NIL, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",NIL, 3)) , "XXX......."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",NIL,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  0)   ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  0,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  0, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  0, 3)) , "XXX......."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  0,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  1)   ) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  1,-2)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  1, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  1, 3)) , "XXX......."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  1,20)) , "XXXXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  3)   ) , "..XXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  3,-2)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  3, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  3, 3)) , "..XXX....."     )
   TEST_LINE( TAStr(AFill(TANew(),"X",  3,20)) , "..XXXXXXXX"     )
   TEST_LINE( TAStr(AFill(TANew(),"X", -1)   ) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", -1,-2)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", -1, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", -1, 3)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", -1,20)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", 21)   ) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", 21,-2)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", 21, 0)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", 21, 3)) , ".........."     )
   TEST_LINE( TAStr(AFill(TANew(),"X", 21,20)) , ".........."     )

   /* ACopy() */

   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1        )) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  0    )) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  3    )) , "ABC......."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1, 20    )) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3        )) , "CDEFGHIJ.."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  0    )) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  3    )) , "CDE......."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3, 20    )) , "CDEFGHIJ.."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21        )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  0    )) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  3    )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21, 20    )) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,NIL,  1)) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  0,  1)) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  3,  0)) , "ABC......."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  3,  2)) , ".ABC......"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  3,  8)) , ".......ABC"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1,  3, 20)) , ".........A"     ) /* Strange in CA-Cl*pper, it should return: ".........." */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  1, 20,  1)) , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,NIL,  3)) , "..CDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  0,  3)) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  3,  0)) , "CDE......."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  3,  2)) , ".CDE......"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  3,  8)) , ".......CDE"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3,  3, 20)) , ".........C"     ) /* Strange in CA-Cl*pper, it should return: ".........." */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(),  3, 20,  3)) , "..CDEFGHIJ"     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,NIL, 21)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  0, 21)) , ".........."     )
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  3,  0)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  3,  2)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".J........", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  3,  8)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".......J..", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21,  3, 20)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   TEST_LINE( TAStr(ACopy(TARng(),TANew(), 21, 20, 21)) , ".........."     ) /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */

   /* ASort() */

   /* Different results in Harbour and CA-Cl*pper due to different
      sorting algorithms. Anyhow the results pattern should match.
      [vszakats] */
#ifdef __HARBOUR__
   TEST_LINE( TAStr(ASort(TARRv(),,,{||NIL})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||hb_SToD()})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"0"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"1"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"2"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"a"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"A"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||""}))  , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"z"})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||.T.})) , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||.F.})) , "FEIDGCHBJA"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||2}))   , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||1}))   , "DCBAEFIHGJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||0}))   , "FEIDGCHBJA"     )
#else
   TEST_LINE( TAStr(ASort(TARRv(),,,{||NIL})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||hb_SToD()})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"0"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"1"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"2"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"a"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"A"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||""}))  , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||"z"})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||.T.})) , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||.F.})) , "DCEABJIHFG"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||2}))   , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||1}))   , "IHGFEDCBAJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),,,{||0}))   , "DCEABJIHFG"     )
#endif
   TEST_LINE( TAStr(ASort(TARRv()))           , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),NIL,NIL))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),NIL, -2))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),NIL,  0))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),NIL,  3))   , "HIJGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(),NIL, 20))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(), -5    ))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), -5, -2))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), -5,  0))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), -5,  3))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), -5, 20))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(),  0    ))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),  0, -2))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),  0,  0))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),  0,  3))   , "HIJGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(),  0, 20))   , "ABCDEFGHIJ"     )
   TEST_LINE( TAStr(ASort(TARRv(),  5    ))   , "JIHGABCDEF"     )
#ifdef __HARBOUR__
   TEST_LINE( TAStr(ASort(TARRv(),  5, -2))   , "JIHGABCDEF"     ) /* CA-Cl*pper will crash or GPF on that line. */
#endif
   TEST_LINE( TAStr(ASort(TARRv(),  5,  0))   , "JIHGABCDEF"     )
   TEST_LINE( TAStr(ASort(TARRv(),  5,  3))   , "JIHGDEFCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(),  5, 20))   , "JIHGABCDEF"     )
   TEST_LINE( TAStr(ASort(TARRv(), 20    ))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), 20, -2))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), 20,  0))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), 20,  3))   , "JIHGFEDCBA"     )
   TEST_LINE( TAStr(ASort(TARRv(), 20, 20))   , "JIHGFEDCBA"     )

   /* AScan() */

#ifndef __XPP__
   TEST_LINE( AScan()                         , 0           )
   TEST_LINE( AScan( NIL )                    , 0           )
   TEST_LINE( AScan( "A" )                    , 0           )
#endif
   TEST_LINE( AScan( "A", "A" )               , 0           )
   TEST_LINE( AScan( "A", {|| .F. } )         , 0           )
   TEST_LINE( AScan( {1,2,3}, {|| NIL } )     , 0           )
   TEST_LINE( AScan( saAllTypes, scString   ) , 1           )
#ifdef __HARBOUR__
   TEST_LINE( AScan( @saAllTypes, scString )  , 1           ) /* Bug in CA-Cl*pper, it will return 0 */
   TEST_LINE( AScan( saAllTypes, @scString )  , 1           ) /* Bug in CA-Cl*pper, it will return 0 */
#endif
   TEST_LINE( AScan( saAllTypes, scStringE  ) , 1           )
   TEST_LINE( AScan( saAllTypes, scStringZ  ) , 3           )
   TEST_LINE( AScan( saAllTypes, snIntZ     ) , 4           )
   TEST_LINE( AScan( saAllTypes, snDoubleZ  ) , 4           )
   TEST_LINE( AScan( saAllTypes, snIntP     ) , 6           )
   TEST_LINE( AScan( saAllTypes, snLongP    ) , 7           )
   TEST_LINE( AScan( saAllTypes, snDoubleP  ) , 8           )
   TEST_LINE( AScan( saAllTypes, snIntN     ) , 9           )
   TEST_LINE( AScan( saAllTypes, snLongN    ) , 10          )
   TEST_LINE( AScan( saAllTypes, snDoubleN  ) , 11          )
   TEST_LINE( AScan( saAllTypes, snDoubleI  ) , 4           )
   TEST_LINE( AScan( saAllTypes, sdDateE    ) , 13          )
   TEST_LINE( AScan( saAllTypes, slFalse    ) , 14          )
   TEST_LINE( AScan( saAllTypes, slTrue     ) , 15          )
   TEST_LINE( AScan( saAllTypes, soObject   ) , 0           )
   TEST_LINE( AScan( saAllTypes, suNIL      ) , 17          )
   TEST_LINE( AScan( saAllTypes, sbBlock    ) , 0           )
   TEST_LINE( AScan( saAllTypes, sbBlockC   ) , 0           )
   TEST_LINE( AScan( saAllTypes, saArray    ) , 0           )
   SET EXACT ON
   TEST_LINE( AScan( saAllTypes, scString   ) , 1           )
   TEST_LINE( AScan( saAllTypes, scStringE  ) , 2           )
   TEST_LINE( AScan( saAllTypes, scStringZ  ) , 3           )
   SET EXACT OFF

   TEST_LINE( TAEVSM()                        , "N10N 9N 8N 7N 6N 5N 4N 3N 2N 1         0" ) /* Bug in CA-Cl*pper 5.x */
   TEST_LINE( TASOSM1()                       , "NN 5NN 4NN 3NN 2NN 1NN 0NN 0NN 0NN 0NN 0NN 0NN 0         0{  }" )
   TEST_LINE( TASOSM2()                       , "NN 5NN 4NN 3NN 2NN 1NN 0NN 0NN 0NN 0NN 0         0{  }" )

   RETURN

STATIC FUNCTION TAEVSM()

   LOCAL cString := ""
   LOCAL aArray := Array( 10 )

   AFill( aArray, 0 )
   AEval( aArray, {| x | cString += ValType( x ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ) } )

   RETURN cString + Str( Len( aArray ) )

STATIC FUNCTION TASOSM1()

   LOCAL cString := ""
   LOCAL aArray := { 1, 2, 3, 4, 5 }

   ASort( aArray, NIL, NIL, {| x, y | cString += ValType( x ) + ValType( y ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ), x > y } )

   RETURN cString + Str( Len( aArray ) ) + XToStrX( aArray )

STATIC FUNCTION TASOSM2()

   LOCAL cString := ""
   LOCAL aArray := { 1, 2, 3, 4, 5 }

   ASort( aArray, NIL, NIL, {| x, y | cString += ValType( x ) + ValType( y ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ) } )

   RETURN cString + Str( Len( aArray ) ) + XToStrX( aArray )

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

   /* Intentionally not using AFill() here, since this function is
      involved in testing AFill() itself. */
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
   LOCAL nLen := Len( aArray )

   FOR tmp := 1 TO nLen
      cString += aArray[ tmp ]
   NEXT

   RETURN cString

/* Don't change the position of this #include. */
#include "rt_init.ch"
