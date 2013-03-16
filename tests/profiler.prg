/* Test code for the harbour profiler API and the profile reporting classes */

#include "inkey.ch"

PROCEDURE Main()

   LOCAL oProfile := HBProfile():new()
   LOCAL n

   // Turn on profiling.
   __SetProfiler( .T. )

   // Make sure we've got something to see timewise.
   DrawScreen( "Doing nothing for a couple of seconds" )
   DoNothingForTwoSeconds()

   // Make sure we've got something to see callwise.
   FOR n := 1 TO 500
      CallMe500Times()
   NEXT

   // Take a profile snapshot.
   oProfile:gather()

   // Report on calls greater than 0
   DrawScreen( "All methods/functions called one or more times" )
   MemoEdit( HBProfileReportToString():new( oProfile:callSort() ):generate( {| o | o:nCalls > 0 } ), 1,,,, .F. )

   // Sorted by name
   DrawScreen( "All methods/functions called one or more times, sorted by name" )
   MemoEdit( HBProfileReportToString():new( oProfile:nameSort() ):generate( {| o | o:nCalls > 0 } ), 1,,,, .F. )

   // Sorted by time
   DrawScreen( "All methods/functions taking measurable time, sorted by time" )
   MemoEdit( HBProfileReportToString():new( oProfile:timeSort() ):generate( {| o | o:nTicks > 0 } ), 1,,,, .F. )

   // TBrowse all calls greater than 0
   DrawScreen( "TBrowse all methods/functions called one or more times" )
   Browser( HBProfileReportToTBrowse():new( oProfile:callSort() ):generate( {| o | o:nCalls > 0 }, 1 ) )

   // Some closing stats
   DrawScreen( "Totals" )
   @ 2, 0 SAY "  Total Calls: " + Str( oProfile:totalCalls() )
   @ 3, 0 SAY "  Total Ticks: " + Str( oProfile:totalTicks() )
   @ 4, 0 SAY "Total Seconds: " + Str( oProfile:totalSeconds() )

   RETURN

STATIC PROCEDURE DrawScreen( cTitle )

   CLS

   @ 0, 0 SAY PadR( cTitle, MaxCol() + 1 ) COLOR "N/W"

   RETURN

PROCEDURE DoNothingForTwoSeconds()

   Inkey( 2 )

   RETURN

PROCEDURE CallMe500Times()

   RETURN

STATIC PROCEDURE Browser( oBrowse )

   LOCAL lBrowsing := .T.

   DO WHILE lBrowsing

      oBrowse:forceStable()

      SWITCH Inkey( 0 )
      CASE K_ESC
         lBrowsing := .F.
         EXIT
      CASE K_DOWN
         oBrowse:down()
         EXIT
      CASE K_UP
         oBrowse:up()
         EXIT
      CASE K_LEFT
         oBrowse:left()
         EXIT
      CASE K_RIGHT
         oBrowse:right()
         EXIT
      CASE K_PGDN
         oBrowse:pageDown()
         EXIT
      CASE K_PGUP
         oBrowse:pageUp()
         EXIT

      // And so on.... (not really necessary for this test)

      ENDSWITCH

   ENDDO

   RETURN
