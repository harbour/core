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

/* TRANSFORM() tests written by Eddie Runia <eddie@runia.comu> */

/* NOTE: Always compile with /n switches */
/* TODO: Add checks for string parameters with embedded NUL character */
/* TODO: Add test cases for other string functions */
/* TODO: Incorporate tests from test/working/string*.prg */

#translate TEST_LINE(<x>, <result>) => TEST_CALL(<(x)>, {|| <x> }, <result>)

STATIC snPass
STATIC snFail
STATIC scFileName
STATIC snFhnd
STATIC scNewLine
STATIC snCount
STATIC slShowFailOnly

FUNCTION Main( cPar1 )

   IF cPar1 == NIL
      cPar1 := ""
   ENDIF

   /* Initialize test */

   TEST_BEGIN( cPar1 )

   /* ROUND() */

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
   TEST_LINE( PadC("abcdef", -5)              , ""               )
   TEST_LINE( PadC("abcdef", 0)               , ""               )
   TEST_LINE( PadC("abcdef", 2)               , "ab"             ) /* QUESTION: CA-Cl*pper "bug", should return: "cd" ? */
   TEST_LINE( PadC("abcdef", 5)               , "abcde"          )
   TEST_LINE( PadC("abcdef", 10)              , "  abcdef  "     )
   TEST_LINE( PadC("abcdef", 10, "1")         , "11abcdef11"     )
   TEST_LINE( PadC("abcdef", 10, "12")        , "11abcdef11"     )

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
   TEST_LINE( Transform( SToD("19920102") , "@E"         ) , "02/01/1992"                  ) /* BUG in Clipper */
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
   TEST_LINE( Transform( SToD("19920102") , "@E"         ) , "02/01/92"                    ) /* BUG in Clipper */
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
   TEST_LINE( Transform( 41        , "@0 9999"     )       , "  41"                        )
   TEST_LINE( Transform( 0         , "@0 9999"     )       , "   0"                        )
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
   TEST_LINE( Transform( -12       , "@( 99"       )       , "(*)"                         ) /* BUG Fix */
   TEST_LINE( Transform( 12        , "@( 99"       )       , "12"                          )
   TEST_LINE( Transform( 1         , ""            )       , "         1"                  )
   TEST_LINE( Transform( 32768     , ""            )       , "     32768"                  )
   TEST_LINE( Transform( -20       , ""            )       , "       -20"                  )
   TEST_LINE( Transform( 1048576   , ""            )       , "   1048576"                  )
   TEST_LINE( Transform( 21.65     , ""            )       , "        21.65"               )
   TEST_LINE( Transform( -3.33     , ""            )       , "        -3.33"               )
   TEST_LINE( Transform( -1234     , "@( 9999"     )       , "(***)"                       ) /* BUG Fix */
   TEST_LINE( Transform( -1234     , "@B 9999"     )       , "****"                        )
   TEST_LINE( Transform( -1234     , "@B( 9999"    )       , "(***)"                       ) /* BUG Fix */
   TEST_LINE( Transform( 1234      , "@E 9,999.99" )       , "1.234,00"                    )
   TEST_LINE( Transform( 12.2      , "@E 9,999.99" )       , "   12,20"                    )
   TEST_LINE( Transform( -1234     , "@X 9999"     )       , "1234 DB"                     )
   TEST_LINE( Transform( -1234     , "@BX 9999"    )       , "1234 DB"                     )
   TEST_LINE( Transform( 1234      , "@B 9999"     )       , "1234"                        )
   TEST_LINE( Transform( 1234      , "@BX 9999"    )       , "1234"                        )
   TEST_LINE( Transform( 0         , "@Z 9999"     )       , "    "                        )
   TEST_LINE( Transform( 0         , "@BZ 9999"    )       , "    "                        )
   TEST_LINE( Transform( 2334      , "Xxxxx: #####")       , "Xxxxx:  2334"                )

   /* Show results, return ERRORLEVEL and exit */

   TEST_END()

   RETURN NIL

STATIC FUNCTION TEST_BEGIN( cParam )
   LOCAL cOs := OS()

   IF "OS/2" $ cOs .OR. ;
      "DOS"  $ cOs
      scNewLine := Chr(13) + Chr(10)
   ELSE
      scNewLine := Chr(10)
   ENDIF

   slShowFailOnly := "/FAIL" $ Upper( cParam )

/*
#ifdef __HARBOUR__
     scFileName := "rtl_test.hb"
#else
     scFileName := "rtl_test.cl"
#endif
*/

   snFhnd := 1 /* FHND_STDOUT */
   scFileName := "(stdout)"

   snCount := 0
   snPass := 0
   snFail := 0

   fWrite(snFhnd, "   Version: " + Version() + scNewLine +;
                  "        OS: " + OS() + scNewLine +;
                  "Date, Time: " + DToS(Date()) + " " + Time() + scNewLine +;
                  "    Output: " + scFileName + scNewLine +;
                  "  Switches: " + cParam + scNewLine +;
                  "===========================================================================" + scNewLine +;
                  scNewLine)

   fWrite(snFhnd, PadL("No", 4) + ". " +;
                  PadR("TestCall()", 35) + " -> " +;
                  PadR("Result", 15) + " | " +;
                  PadR("Expected", 15) +;
                  " [! *FAIL* !]" + scNewLine)
   fWrite(snFhnd, "---------------------------------------------------------------------------" + scNewLine)

   RETURN NIL

STATIC FUNCTION TEST_CALL(cBlock, bBlock, xResultExpected)
   LOCAL xResult := Eval(bBlock)

   snCount++

   IF !slShowFailOnly .OR. !( xResult == xResultExpected )

      fWrite(snFhnd, Str(snCount, 4) + ". " +;
                     PadR(StrTran(cBlock, Chr(0), "."), 35) + " -> " +;
                     PadR('"' + StrTran(XToStr(xResult), Chr(0), ".") + '"', 15) + " | " +;
                     PadR('"' + StrTran(XToStr(xResultExpected), Chr(0), ".") + '"', 15))

      IF !( xResult == xResultExpected )
         fWrite(snFhnd, " ! *FAIL* !" )
      ENDIF

      fWrite(snFhnd, scNewLine)
   ENDIF

   IF xResult == xResultExpected
      snPass++
   ELSE
      snFail++
   ENDIF

   RETURN NIL

STATIC FUNCTION TEST_END()

   fWrite(snFhnd, scNewLine +;
                  "===========================================================================" + scNewLine +;
                  "Test calls passed: " + Str(snPass) + scNewLine +;
                  "Test calls failed: " + Str(snFail) + scNewLine +;
                  scNewLine)

   IF snFail != 0
      IF "CLIPPER" $ Upper(Version())
         fWrite(snFhnd, "WARNING ! Failures detected using Clipper." + scNewLine +;
                        "Please fix the expected result list, if this is not a bug in Clipper itself." + scNewLine)
      ELSE
         fWrite(snFhnd, "WARNING ! Failures detected" + scNewLine)
      ENDIF
   ENDIF

   ErrorLevel(iif(snFail != 0, 1, 0))

   RETURN NIL

STATIC FUNCTION XToStr(xValue)
   LOCAL cType := ValType(xValue)

   DO CASE
   CASE cType == "C" ; RETURN xValue
   CASE cType == "N" ; RETURN LTrim(Str(xValue))
   CASE cType == "D" ; RETURN DToC(xValue)
   CASE cType == "L" ; RETURN iif(xValue, ".T.", ".F.")
   CASE cType == "O" ; RETURN xValue:className + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...}"
   CASE cType == "A" ; RETURN "{...}"
   CASE cType == "M" ; RETURN xValue
   ENDCASE

   RETURN ""

#ifndef __HARBOUR__
#ifndef __XPP__

FUNCTION SToD( cDate )
   LOCAL cOldDateFormat := Set(_SET_DATEFORMAT, "dd/mm/yyyy")
   LOCAL dDate

   Set(_SET_DATEFORMAT, "yyyy/mm/dd")

   dDate := CToD( SubStr( cDate, 1, 4 ) + "/" +;
                  SubStr( cDate, 5, 2 ) + "/" +;
                  SubStr( cDate, 7, 2 ) )

   Set(_SET_DATEFORMAT, cOldDateFormat)

   RETURN dDate

#endif
#endif

