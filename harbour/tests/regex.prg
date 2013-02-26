/*
 * $Id$
 */

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
   /*
   * Standard regex to get the ISO date format:
   * ([0-9]{4}): exactly four digits (year); it is in brackets,
   *    this means that we want it back as a group
   * [-/]: one bar or a minus
   * ([0-9]{1,2}): one or two digits
   */
   regex := hb_regexComp( "([0-9]{4})[-/]([0-9]{1,2})[-/]([0-9]{1,2})" )

   FOR EACH cStr IN aSource
      @ nRow, 5 SAY "String is '" + cStr + "'"
      nRow++
      aMatch := hb_regex( regex, cStr )
      IF ! Empty( aMatch )
         @ nRow, 10 SAY "Matched: " + aMatch[ 1 ] + " ( Year: " + aMatch[ 2 ] + ", Month: " + ;
            aMatch[ 3 ] + ", Day: " + aMatch[ 4 ] + ")"
      ELSE
         @ nRow, 10 SAY "Match FAILED!"
      ENDIF
      nRow += 2
   NEXT

   cStr := "searching 'regex' here:"
   @ nRow, 5 SAY "A test of a regex compiled on the fly; " + cStr
   aMatch := hb_regex( "(.*)regex(.*)", cStr )
   nRow++
   IF Empty( aMatch )
      @ nRow, 10 SAY "NOT FOUND!"
   ELSE
      @ nRow, 10 SAY "Found (Before: <<" + aMatch[ 2 ] + ">>, After: <<" + aMatch[ 3 ] + ">>)"
   ENDIF

   nRow += 2

   cStr := "A str; with: separators :; here "
   @ nRow, 5 SAY "Split test; splitting '" + cStr + "' by ':|;'"
   nRow++
   aMatch := hb_regexSplit( ":|;", cStr )
   IF Empty( aMatch )
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
   @ nRow, 5 SAY "Split test; splitting '" + cStr + "' by ':|;'"
   nRow++
   aMatch := hb_regexSplit( ":|;", cStr )
   IF Empty( aMatch )
      @ nRow++, 10 SAY "Test failed"
   ELSE
      nCol := 10
      FOR EACH cStr IN aMatch
         @ nRow, nCol SAY cStr + "/"
         nCol += Len( cStr ) + 1
      NEXT
      nRow++
   ENDIF

   cStr := "Test for RegexAtX()"
   @ nRow, 5 SAY "RegexAtX() test; scanning '" + cStr + "' by 'Reg(.x)'"
   nRow++
   aMatch := hb_regexAtX( "Reg(.x)", cStr )
   IF Empty( aMatch )
      @ nRow++, 10 SAY "Test failed"
   ELSE
      nCol := 15
      FOR EACH cStr in aMatch
         @ nRow, nCol SAY "FOUND: '" + cStr[ 1 ] + "' Start: " + hb_ntos( cStr[ 2 ] ) + ;
            " End: " + hb_ntos( cStr[ 3 ] )
         nRow++
      NEXT
      nRow++
   ENDIF

   @ nRow, 1
   @ MaxRow(), 25 SAY "Press a key to continue"
   Inkey( 0 )

   RETURN
