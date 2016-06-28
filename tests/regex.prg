// Test for regular expression functions
// Giancarlo Niccolai

PROCEDURE Main()

   LOCAL regex
   LOCAL aMatch
   LOCAL cStr, nRow := 2, nCol
   LOCAL aSource := { ;
      "First date to match: 2001-3-21", ;
      "2002-12/2", ;
      "Another can be 1999/5/12, and succeeds", ;
      "Could be 1999/534/12, but this will fail" }

   CLS

   @ 0, 15 SAY "Regular expression scan tests"

   /* Standard regex to get the ISO date format:
    * ([0-9]{4}): exactly four digits (year); it is in brackets,
    *    this means that we want it back as a group
    * [-/]: one bar or a minus
    * ([0-9]{1,2}): one or two digits
    */
   regex := hb_regexComp( "([0-9]{4})[-/]([0-9]{1,2})[-/]([0-9]{1,2})" )

   FOR EACH cStr IN aSource

      @ nRow++, 5 SAY "String is '" + cStr + "'"

      IF ! Empty( aMatch := hb_regex( regex, cStr ) )
         @ nRow++, 10 SAY "Matched: " + aMatch[ 1 ] + " ( Year: " + aMatch[ 2 ] + ", Month: " + ;
            aMatch[ 3 ] + ", Day: " + aMatch[ 4 ] + ")"
      ELSE
         @ nRow++, 10 SAY "Match FAILED!"
      ENDIF

      nRow++
   NEXT

   cStr := "searching 'regex' here:"
   @ nRow++, 5 SAY "A test of a regex compiled on the fly; " + cStr

   IF Empty( aMatch := hb_regex( "(.*)regex(.*)", cStr ) )
      @ nRow++, 10 SAY "NOT FOUND!"
   ELSE
      @ nRow++, 10 SAY "Found (Before: <<" + aMatch[ 2 ] + ">>, After: <<" + aMatch[ 3 ] + ">>)"
   ENDIF

   nRow++

   cStr := "A str; with: separators :; here "
   @ nRow++, 5 SAY "Split test; splitting '" + cStr + "' by ':|;'"

   IF Empty( aMatch := hb_regexSplit( ":|;", cStr ) )
      @ nRow++, 10 SAY "Test failed"
   ELSE
      nCol := 10
      FOR EACH cStr IN aMatch
         @ nRow, nCol SAY cStr + "/"
         nCol += Len( cStr ) + 1
      NEXT
      nRow++
   ENDIF

   cStr := "A string without separators"
   @ nRow++, 5 SAY "Split test; splitting '" + cStr + "' by ':|;'"

   IF Empty( aMatch := hb_regexSplit( ":|;", cStr ) )
      @ nRow++, 10 SAY "Test failed"
   ELSE
      nCol := 10
      FOR EACH cStr IN aMatch
         @ nRow, nCol SAY cStr + "/"
         nCol += Len( cStr ) + 1
      NEXT
      nRow++
   ENDIF

   cStr := "Test for hb_regexAtX()"
   @ nRow++, 5 SAY "hb_regexAtX() test; scanning '" + cStr + "' by 'hb_reg(.x)'"

   IF Empty( aMatch := hb_regexAtX( "hb_reg(.x)", cStr ) )
      @ nRow++, 10 SAY "Test failed"
   ELSE
      nCol := 15
      FOR EACH cStr in aMatch
         @ nRow++, nCol SAY "FOUND: '" + cStr[ 1 ] + "'" + ;
            " Start: " + hb_ntos( cStr[ 2 ] ) + ;
            " End: " + hb_ntos( cStr[ 3 ] )
      NEXT
      nRow++
   ENDIF

   RETURN
