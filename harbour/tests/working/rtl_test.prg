/* $Id$

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

/* TODO: Add checks for string parameters with embedded NUL character */
/* TODO: Add test cases for other string functions */
/* TODO: Incorporate tests from TEST\WORKING\STRING*.PRG
/* QUESTION: Can Harbour compile and run this (PadR(), Chr(), fWrite(), StrTran()) ? */

FUNCTION Main()

#define NUL                             Chr(0)

SHOW_TEST('At("", "")'                , Str(At("", ""))             ) // 1
SHOW_TEST('At("", "ABCDEF")'          , Str(At("", "ABCDEF"))       ) // 1
SHOW_TEST('At("ABCDEF", "")'          , Str(At("ABCDEF", ""))       ) // 0
SHOW_TEST('At("AB", "AB")'            , Str(At("AB", "AB"))         ) // 1
SHOW_TEST('At("AB", "AAB")'           , Str(At("AB", "AAB"))        ) // 2
SHOW_TEST('At("A", "ABCDEF")'         , Str(At("A", "ABCDEF"))      ) // 1
SHOW_TEST('At("F", "ABCDEF")'         , Str(At("F", "ABCDEF"))      ) // 6
SHOW_TEST('At("D", "ABCDEF")'         , Str(At("D", "ABCDEF"))      ) // 4
SHOW_TEST('At("X", "ABCDEF")'         , Str(At("X", "ABCDEF"))      ) // 0
SHOW_TEST('At("AB", "ABCDEF")'        , Str(At("AB", "ABCDEF"))     ) // 1
SHOW_TEST('At("AA", "ABCDEF")'        , Str(At("AA", "ABCDEF"))     ) // 0
SHOW_TEST('At("ABCDEF", "ABCDEF")'    , Str(At("ABCDEF", "ABCDEF")) ) // 1
SHOW_TEST('At("BCDEF", "ABCDEF")'     , Str(At("BCDEF", "ABCDEF"))  ) // 2
SHOW_TEST('At("BCDEFG", "ABCDEF")'    , Str(At("BCDEFG", "ABCDEF")) ) // 0
SHOW_TEST('At("ABCDEFG", "ABCDEF")'   , Str(At("ABCDEFG", "ABCDEF"))) // 0
SHOW_TEST('At("FI", "ABCDEF")'        , Str(At("FI", "ABCDEF"))     ) // 0

SHOW_TEST('SubStr("abcdef", 0, -1)'   , SubStr("abcdef", 0, -1)   ) // ""
SHOW_TEST('SubStr("abcdef", 0, 0)'    , SubStr("abcdef", 0, 0)    ) // ""
SHOW_TEST('SubStr("abcdef", 0, 1)'    , SubStr("abcdef", 0, 1)    ) // "a"
SHOW_TEST('SubStr("abcdef", 0, 7)'    , SubStr("abcdef", 0, 7)    ) // "abcdef"
SHOW_TEST('SubStr("abcdef", 0)'       , SubStr("abcdef", 0)       ) // "abcdef"
SHOW_TEST('SubStr("abcdef", 2, -1)'   , SubStr("abcdef", 2, -1)   ) // ""
SHOW_TEST('SubStr("abcdef", 2, 0)'    , SubStr("abcdef", 2, 0)    ) // ""
SHOW_TEST('SubStr("abcdef", 2, 1)'    , SubStr("abcdef", 2, 1)    ) // "b"
SHOW_TEST('SubStr("abcdef", 2, 7)'    , SubStr("abcdef", 2, 7)    ) // "bcdef"
SHOW_TEST('SubStr("abcdef", 2)'       , SubStr("abcdef", 2)       ) // "bcdef"
SHOW_TEST('SubStr("abcdef", -2, -1)'  , SubStr("abcdef", -2, -1)  ) // ""
SHOW_TEST('SubStr("abcdef", -2, 0)'   , SubStr("abcdef", -2, 0)   ) // ""
SHOW_TEST('SubStr("abcdef", -2, 1)'   , SubStr("abcdef", -2, 1)   ) // "e"
SHOW_TEST('SubStr("abcdef", -2, 7)'   , SubStr("abcdef", -2, 7)   ) // "ef"
SHOW_TEST('SubStr("abcdef", -2)'      , SubStr("abcdef", -2)      ) // "ef"
SHOW_TEST('SubStr("abcdef", 10, -1)'  , SubStr("abcdef", 10, -1)  ) // ""
SHOW_TEST('SubStr("abcdef", 10, 0)'   , SubStr("abcdef", 10, 0)   ) // ""
SHOW_TEST('SubStr("abcdef", 10, 1)'   , SubStr("abcdef", 10, 1)   ) // ""
SHOW_TEST('SubStr("abcdef", 10, 7)'   , SubStr("abcdef", 10, 7)   ) // ""
SHOW_TEST('SubStr("abcdef", 10)'      , SubStr("abcdef", 10)      ) // ""
SHOW_TEST('SubStr("abcdef", -10, -1)' , SubStr("abcdef", -10, -1) ) // ""
SHOW_TEST('SubStr("abcdef", -10, 0)'  , SubStr("abcdef", -10, 0)  ) // ""
SHOW_TEST('SubStr("abcdef", -10, 1)'  , SubStr("abcdef", -10, 1)  ) // "a"
SHOW_TEST('SubStr("abcdef", -10, 7)'  , SubStr("abcdef", -10, 7)  ) // "abcdef"
SHOW_TEST('SubStr("abcdef", -10, 15)' , SubStr("abcdef", -10, 15) ) // "abcdef"
SHOW_TEST('SubStr("abcdef", -10)'     , SubStr("abcdef", -10)     ) // "abcdef"

SHOW_LINE()

SHOW_TEST('Left("abcdef", -10)'       , Left("abcdef", -10)       ) // ""
SHOW_TEST('Left("abcdef", -2)'        , Left("abcdef", -2)        ) // ""
SHOW_TEST('Left("abcdef", 0)'         , Left("abcdef", 0)         ) // ""
SHOW_TEST('Left("abcdef", 2)'         , Left("abcdef", 2)         ) // "ab"
SHOW_TEST('Left("abcdef", 10)'        , Left("abcdef", 10)        ) // "abcdef"

SHOW_LINE()

SHOW_TEST('Right("abcdef", -10)'      , Right("abcdef", -10)      ) // ""
SHOW_TEST('Right("abcdef", -2)'       , Right("abcdef", -2)       ) // ""
SHOW_TEST('Right("abcdef", 0)'        , Right("abcdef", 0)        ) // ""
SHOW_TEST('Right("abcdef", 2)'        , Right("abcdef", 2)        ) // "ef"
SHOW_TEST('Right("abcdef", 10)'       , Right("abcdef", 10)       ) // "abcdef"

SHOW_LINE()

SHOW_TEST('PadR("abcdef", -5)'        , PadR("abcdef", -5)        ) // ""
SHOW_TEST('PadR("abcdef", 0)'         , PadR("abcdef", 0)         ) // ""
SHOW_TEST('PadR("abcdef", 5)'         , PadR("abcdef", 5)         ) // "abcde"
SHOW_TEST('PadR("abcdef", 10)'        , PadR("abcdef", 10)        ) // "abcdef    "
SHOW_TEST('PadR("abcdef", 10, "1")'   , PadR("abcdef", 10, "1")   ) // "abcdef1111"
SHOW_TEST('PadR("abcdef", 10, "12")'  , PadR("abcdef", 10, "12")  ) // "abcdef1111"

SHOW_LINE()

SHOW_TEST('PadL("abcdef", -5)'        , PadL("abcdef", -5)        ) // ""
SHOW_TEST('PadL("abcdef", 0)'         , PadL("abcdef", 0)         ) // ""
SHOW_TEST('PadL("abcdef", 5)'         , PadL("abcdef", 5)         ) // "abcde" /* QUESTION: CA-Clipper "bug", should return: "bcdef" ? */
SHOW_TEST('PadL("abcdef", 10)'        , PadL("abcdef", 10)        ) // "    abcdef"
SHOW_TEST('PadL("abcdef", 10, "1")'   , PadL("abcdef", 10, "1")   ) // "1111abcdef"
SHOW_TEST('PadL("abcdef", 10, "12")'  , PadL("abcdef", 10, "12")  ) // "1111abcdef"

SHOW_LINE()

SHOW_TEST('PadC("abcdef", -5)'        , PadC("abcdef", -5)        ) // ""
SHOW_TEST('PadC("abcdef", 0)'         , PadC("abcdef", 0)         ) // ""
SHOW_TEST('PadC("abcdef", 2)'         , PadC("abcdef", 2)         ) // "ab" /* QUESTION: CA-Clipper "bug", should return: "cd" ? */
SHOW_TEST('PadC("abcdef", 5)'         , PadC("abcdef", 5)         ) // "abcde"
SHOW_TEST('PadC("abcdef", 10)'        , PadC("abcdef", 10)        ) // "  abcdef  "
SHOW_TEST('PadC("abcdef", 10, "1")'   , PadC("abcdef", 10, "1")   ) // "11abcdef11"
SHOW_TEST('PadC("abcdef", 10, "12")'  , PadC("abcdef", 10, "12")  ) // "11abcdef11"

SHOW_LINE()

/* TODO: These could be more complete */

SHOW_TEST('SubStr("ab" + NUL + "def", 2, 3)', SubStr("ab" + NUL + "def", 2, 3) )
SHOW_TEST('Left("ab" + NUL + "def", 5)', Left("ab" + NUL + "def", 5) )
SHOW_TEST('Right("ab" + NUL + "def", 5)', Right("ab" + NUL + "def", 5) )

RETURN NIL

STATIC FUNCTION SHOW_TEST(cExpr, cResult)
     RETURN fWrite(1, PadR(StrTran(cExpr, Chr(0), "."), 40) + " -> " + '"' + StrTran(cResult, Chr(0), ".") + '"' + Chr(13) + Chr(10))

STATIC FUNCTION SHOW_LINE()
     RETURN fWrite(1, Chr(13) + Chr(10))
