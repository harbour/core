/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (date)
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

FUNCTION Main_DATE()
   LOCAL cDate := "1999/11/25"

   /* YEAR() */

   TEST_LINE( Year(NIL)                       , "E BASE 1112 Argument error YEAR F:S"  )
   TEST_LINE( Year(100)                       , "E BASE 1112 Argument error YEAR F:S"  )
#ifdef __HARBOUR__
   TEST_LINE( Year(@sdDate)                   , 1984                                   ) /* Bug in CA-Cl*pper, it returns: "E BASE 1112 Argument error YEAR F:S" */
#endif
   TEST_LINE( Year(sdDate)                    , 1984                                   )
   TEST_LINE( Year(sdDateE)                   , 0                                      )
   TEST_LINE( Str(Year(SToD("19990905")))     , " 1999"                                )

   /* MONTH() */

   TEST_LINE( Month(NIL)                      , "E BASE 1113 Argument error MONTH F:S" )
   TEST_LINE( Month(100)                      , "E BASE 1113 Argument error MONTH F:S" )
#ifdef __HARBOUR__
   TEST_LINE( Month(@sdDate)                  , 3                                      ) /* Bug in CA-Cl*pper, it returns: "E BASE 1113 Argument error MONTH F:S" */
#endif
   TEST_LINE( Month(sdDate)                   , 3                                      )
   TEST_LINE( Month(sdDateE)                  , 0                                      )
   TEST_LINE( Str(Month(SToD("19990905")))    , "  9"                                  )

   /* DAY() */

   TEST_LINE( Day(NIL)                        , "E BASE 1114 Argument error DAY F:S"   )
   TEST_LINE( Day(100)                        , "E BASE 1114 Argument error DAY F:S"   )
#ifdef __HARBOUR__
   TEST_LINE( Day(@sdDate)                    , 25                                     ) /* Bug in CA-Cl*pper, it returns: "E BASE 1114 Argument error DAY F:S" */
#endif
   TEST_LINE( Day(sdDate)                     , 25                                     )
   TEST_LINE( Day(sdDateE)                    , 0                                      )
   TEST_LINE( Str(Day(SToD("19990905")))      , "  5"                                  )

   /* TIME() */

   TEST_LINE( Len(Time())                     , 8                                      )

   /* DOW() */

   TEST_LINE( Dow(NIL)                        , "E BASE 1115 Argument error DOW F:S"   )
   TEST_LINE( Dow(100)                        , "E BASE 1115 Argument error DOW F:S"   )
#ifdef __HARBOUR__
   TEST_LINE( Dow(@sdDate)                    , 1                                      ) /* Bug in CA-Cl*pper, it returns: "E BASE 1115 Argument error DOW F:S" */
#endif
   TEST_LINE( Dow(sdDate)                     , 1                                      )
   TEST_LINE( Dow(sdDateE)                    , 0                                      )
   TEST_LINE( Dow(SToD("20000222"))           , 3                                      )
   TEST_LINE( Dow(SToD("20000223"))           , 4                                      )
   TEST_LINE( Dow(SToD("20000224"))           , 5                                      )
   TEST_LINE( Dow(SToD("20000225"))           , 6                                      )
   TEST_LINE( Dow(SToD("20000226"))           , 7                                      )
   TEST_LINE( Dow(SToD("20000227"))           , 1                                      )
   TEST_LINE( Dow(SToD("20000228"))           , 2                                      )
   TEST_LINE( Dow(SToD("20000229"))           , 3                                      )
   TEST_LINE( Dow(SToD("20000230"))           , 0                                      )
   TEST_LINE( Dow(SToD("20000231"))           , 0                                      )
   TEST_LINE( Dow(SToD("20000301"))           , 4                                      )

   /* CMONTH() */

   TEST_LINE( CMonth(NIL)                     , "E BASE 1116 Argument error CMONTH F:S" )
   TEST_LINE( CMonth(100)                     , "E BASE 1116 Argument error CMONTH F:S" )
#ifdef __HARBOUR__
   TEST_LINE( CMonth(@sdDate)                 , "March"                                 ) /* Bug in CA-Cl*pper, it returns: "E BASE 1116 Argument error CMONTH F:S" */
#endif
   TEST_LINE( CMonth(sdDate)                  , "March"                                 )
   TEST_LINE( CMonth(sdDateE)                 , ""                                      )
   TEST_LINE( CMonth(SToD("19990101"))        , "January"                               )
   TEST_LINE( CMonth(SToD("19990201"))        , "February"                              )
   TEST_LINE( CMonth(SToD("19990301"))        , "March"                                 )
   TEST_LINE( CMonth(SToD("19990401"))        , "April"                                 )
   TEST_LINE( CMonth(SToD("19990501"))        , "May"                                   )
   TEST_LINE( CMonth(SToD("19990601"))        , "June"                                  )
   TEST_LINE( CMonth(SToD("19990701"))        , "July"                                  )
   TEST_LINE( CMonth(SToD("19990801"))        , "August"                                )
   TEST_LINE( CMonth(SToD("19990901"))        , "September"                             )
   TEST_LINE( CMonth(SToD("19991001"))        , "October"                               )
   TEST_LINE( CMonth(SToD("19991101"))        , "November"                              )
   TEST_LINE( CMonth(SToD("19991201"))        , "December"                              )

   /* CDOW() */

   TEST_LINE( CDow(NIL)                       , "E BASE 1117 Argument error CDOW F:S"  )
   TEST_LINE( CDow(100)                       , "E BASE 1117 Argument error CDOW F:S"  )
#ifdef __HARBOUR__
   TEST_LINE( CDow(@sdDate)                   , "Sunday"                               ) /* Bug in CA-Cl*pper, it returns: "E BASE 1117 Argument error CDOW F:S" */
#endif
   TEST_LINE( CDow(sdDate)                    , "Sunday"                               )
   TEST_LINE( CDow(sdDateE)                   , ""                                     )
   TEST_LINE( CDow(SToD("20000222"))          , "Tuesday"                              )
   TEST_LINE( CDow(SToD("20000223"))          , "Wednesday"                            )
   TEST_LINE( CDow(SToD("20000224"))          , "Thursday"                             )
   TEST_LINE( CDow(SToD("20000225"))          , "Friday"                               )
   TEST_LINE( CDow(SToD("20000226"))          , "Saturday"                             )
   TEST_LINE( CDow(SToD("20000227"))          , "Sunday"                               )
   TEST_LINE( CDow(SToD("20000228"))          , "Monday"                               )
   TEST_LINE( CDow(SToD("20000229"))          , "Tuesday"                              )
   TEST_LINE( CDow(SToD("20000230"))          , ""                                     )
   TEST_LINE( CDow(SToD("20000231"))          , ""                                     )
   TEST_LINE( CDow(SToD("20000301"))          , "Wednesday"                            )

   /* DTOC() */

   TEST_LINE( DToC(NIL)                       , "E BASE 1118 Argument error DTOC F:S"  )
   TEST_LINE( DToC(100)                       , "E BASE 1118 Argument error DTOC F:S"  )
   TEST_LINE( DToC("")                        , "E BASE 1118 Argument error DTOC F:S"  )
#ifdef __HARBOUR__
   TEST_LINE( DToC(@sdDate)                   , "1984.03.25"                           ) /* Bug in CA-Cl*pper, it returns: "E BASE 1118 Argument error DTOC F:S" */
#endif
   TEST_LINE( DToC(sdDate)                    , "1984.03.25"                           )
   TEST_LINE( DToC(sdDateE)                   , "    .  .  "                           )

   /* CTOD() */

   TEST_LINE( CToD(NIL)                       , "E BASE 1119 Argument error CTOD F:S"  )
   TEST_LINE( CToD(100)                       , "E BASE 1119 Argument error CTOD F:S"  )
   TEST_LINE( CToD("")                        , SToD("        ")                       )
#ifdef __HARBOUR__
   TEST_LINE( CToD(@cDate)                    , SToD("19991125")                       ) /* Bug in CA-Cl*pper, it returns: "E BASE 1119 Argument error CTOD F:S" */
#endif
   TEST_LINE( CToD(cDate)                     , SToD("19991125")                       )
   TEST_LINE( CToD("1999/11/25/10")           , SToD("19991125")                       )

   /* DTOS() */

   TEST_LINE( DToS(NIL)                       , "E BASE 1120 Argument error DTOS F:S"  )
   TEST_LINE( DToS(100)                       , "E BASE 1120 Argument error DTOS F:S"  )
#ifdef __HARBOUR__
   TEST_LINE( DToS(@sdDate)                   , "19840325"                             ) /* Bug in CA-Cl*pper, it returns: "E BASE 1120 Argument error DTOS F:S" */
#endif
   TEST_LINE( DToS(sdDate)                    , "19840325"                             )
   TEST_LINE( DToS(sdDateE)                   , "        "                             )

   RETURN NIL
