/*
 * $Id$
 */

/* Test code for the harbour profiler API and the profile reporting classes */

#include "inkey.ch"

PROCEDURE Main()

   LOCAL oProfile := HBProfile():new()
   LOCAL oGet     := GetNew()
   LOCAL n

   // Turn on profiling.
   __setProfiler( .T. )

   // Make sure we've got something to see timewise.
   DrawScreen( "Doing nothing for a couple of seconds" )
   DoNothingForTwoSeconds()

   // Make sure we've got something to see callwise.
   FOR n := 1 TO 500
      CallMe500Times()
   NEXT

   // Generate some object oriented (oriented? <g>) entries.
   FOR n := 1 TO 500
      oGet:row := 0
   NEXT

   // Take a profile snapshot.
   oProfile:gather()

   // Report on calls greater than 0
   DrawScreen( "All methods/functions called one or more times" )
   MemoEdit( HBProfileReportToString():new( oProfile:callSort() ):generate( {| o | o:nCalls > 0 } ), 1, , , , .F. )

   // Sorted by name
   DrawScreen( "All methods/functions called one or more times, sorted by name" )
   MemoEdit( HBProfileReportToString():new( oProfile:nameSort() ):generate( {| o | o:nCalls > 0 } ), 1, , , , .F. )

   // Sorted by time
   DrawScreen( "All methods/functions taking measurable time, sorted by time" )
   MemoEdit( HBProfileReportToString():new( oProfile:timeSort() ):generate( {| o | o:nTicks > 0 } ), 1, , , , .F. )

   // TBrowse all calls greater than 0
   DrawScreen( "TBrowse all methods/functions called one or more times" )
   Browser( HBProfileReportToTBrowse():new( oProfile:callSort() ):generate( {| o | o:nCalls > 0 }, 1 ) )

   // Some closing stats
   DrawScreen( "Totals" )
   @ 2, 0 SAY "  Total Calls: " + Str( oProfile:totalCalls() )
   @ 3, 0 SAY "  Total Ticks: " + Str( oProfile:totalTicks() )
   @ 4, 0 SAY "Total Seconds: " + Str( oProfile:totalSeconds() )

   RETURN

STATIC FUNCTION DrawScreen( cTitle )

   Scroll()

   @ 0, 0 SAY PadR( cTitle, MaxCol() + 1 ) COLOR "N/W"

   RETURN NIL

FUNCTION DoNothingForTwoSeconds()

   Inkey( 2 )

   RETURN NIL

FUNCTION CallMe500Times()

   RETURN NIL

STATIC FUNCTION Browser( oBrowse )

   LOCAL lBrowsing := .T.
   LOCAL nKey

   DO WHILE lBrowsing

      oBrowse:forceStable()

      nKey := Inkey( 0 )

      DO CASE

      CASE nKey == K_ESC
         lBrowsing := .F.

      CASE nKey == K_DOWN
         oBrowse:down()

      CASE nKey == K_UP
         oBrowse:up()

      CASE nKey == K_LEFT
         oBrowse:Left()

      CASE nKey == K_RIGHT
         oBrowse:Right()

      CASE nKey == K_PGDN
         oBrowse:pageDown()

      CASE nKey == K_PGUP
         oBrowse:pageUp()

         // And so on.... (not really necessary for this test)

      ENDCASE

   ENDDO

   RETURN NIL
