//
// $Id$
//

#include "set.ch"

function main
local cStr := "   " + CHR (0) + "ABC" + CHR (0) + "   "
local cTest, nI, nJ, crlf := CHR(13)+CHR(10)

   // Test string copying.
   cTest := cStr
   OUTSTD (crlf)
   OUTSTD (cStr)
   OUTSTD (crlf)
   OUTSTD (cTest)
   OUTSTD (crlf)
   OUTSTD (crlf)

   // Test string concatenation.
   cTest += cStr
   OUTSTD (crlf)
   OUTSTD (cStr)
   OUTSTD (crlf)
   OUTSTD (cTest)
   OUTSTD (crlf)
   OUTSTD (crlf)

   // Test the string comparison operators in the HVM.
   // Note: SET (_SET_EXACT) defaults to .F.
   TestStr ()
   OUTSTD (crlf)
   OUTSTD (crlf)
   SET (_SET_EXACT, .T.)
   TestStr ()
   OUTSTD (crlf)
return nil

function TestStr ()
   OUTSTD ("EXACT ")
   IF SET (_SET_EXACT)
      OUTSTD ("ON")
   ELSE
      OUTSTD ("OFF")
   ENDIF
   StrTest ("ABC", "")
   StrTest ("ABC", " ")
   StrTest ("ABC", "ABC")
   StrTest ("ABC", "ABCD")
   StrTest ("ABC", "ABC ")
   StrTest ("ABC", "DEF")
   StrTest ("ABC", "DEFG")
   StrTest ("ABCD", "DEF")
return nil

function StrTest (Str1, Str2)

   OUTSTD (CHR(13)+CHR(10)+CHR(10))
   OUTSTD ("'")
   OUTSTD (Str1)
   OUTSTD ("', '")
   OUTSTD (Str2)
   OUTSTD ("' == ")
   OUTSTD (Str1 == Str2)
   OUTSTD (" = ")
   OUTSTD (Str1 = Str2)
   OUTSTD (", != ")
   OUTSTD (Str1 != Str2)
   OUTSTD (", < ")
   OUTSTD (Str1 < Str2)
   OUTSTD (", <= ")
   OUTSTD (Str1 <= Str2)
   OUTSTD (", > ")
   OUTSTD (Str1 > Str2)
   OUTSTD (", >= ")
   OUTSTD (Str1 >= Str2)

   OUTSTD (CHR(13)+CHR(10))
   OUTSTD ("'")
   OUTSTD (Str2)
   OUTSTD ("', '")
   OUTSTD (Str1)
   OUTSTD ("' == ")
   OUTSTD (Str2 == Str1)
   OUTSTD (" = ")
   OUTSTD (Str2 = Str1)
   OUTSTD (", != ")
   OUTSTD (Str2 != Str1)
   OUTSTD (", < ")
   OUTSTD (Str2 < Str1)
   OUTSTD (", <= ")
   OUTSTD (Str2 <= Str1)
   OUTSTD (", > ")
   OUTSTD (Str2 > Str1)
   OUTSTD (", >= ")
   OUTSTD (Str2 >= Str1)

return nil
