/*
 * $Id$
 */

function main()

   local aStruct := { { "CHARACTER", "C", 25, 0 }, ;
		      { "NUMERIC",   "N",  8, 0 }, ;
		      { "DOUBLE",    "N",  8, 2 }, ;
		      { "DATE",      "D",  8, 0 }, ;
                      { "LOGICAL",   "L",  1, 0 }, ;
                      { "MEMO1",     "M", 10, 0 }, ;
                      { "MEMO2",     "M", 10, 0 } }

   dbCreate( "testdbf", aStruct )
   dbUseArea(,, "testdbf" )
   ? "[" + FIELD->MEMO1 + "]"
   ? "[" + FIELD->MEMO2 + "]"
   ? "-"
   FIELD->MEMO1 := "Hello world!"
   FIELD->MEMO2 := "Harbour power"
   ? "[" + FIELD->MEMO1 + "]"
   ? "[" + FIELD->MEMO2 + "]"
   dbAppend()
   FIELD->MEMO1 := "111"
   FIELD->MEMO2 := "222"
   ? "[" + FIELD->MEMO1 + "]"
   ? "[" + FIELD->MEMO2 + "]"

   FIELD->NUMERIC := 90
   FIELD->DOUBLE := 120.138

   ? "[" + Str(FIELD->DOUBLE) + "]"
   ? "[" + Str(FIELD->NUMERIC) + "]"

return nil
