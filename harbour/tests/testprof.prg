/*
 * $Id$
 */

/* Test code for the harbour profiler API and the profile reporting classes */

#include "inkey.ch"

Function Main()
Local oProfile := HBProfile():new()
Local oGet     := GetNew()
Local n

   // Turn on profiling.
   __setProfiler( .T. )

   // Make sure we've got something to see timewise.
   DrawScreen( "Doing nothing for a couple of seconds" )
   DoNothingForTwoSeconds()

   // Make sure we've got something to see callwise.
   For n := 1 To 500
      CallMe500Times()
   Next

   // Generate some object oriented (oriented? <g>) entries.
   For n := 1 To 500
      oGet:row := 0
   Next

   // Take a profile snapshot.
   oProfile:gather()

   // Report on calls greater than 0
   DrawScreen( "All methods/functions called one or more times" )
   memoedit( HBProfileReportToString():new( oProfile:callSort() ):generate( {|o| o:nCalls > 0 } ), 1,,,, .F. )

   // Sorted by name
   DrawScreen( "All methods/functions called one or more times, sorted by name" )
   memoedit( HBProfileReportToString():new( oProfile:nameSort() ):generate( {|o| o:nCalls > 0 } ), 1,,,, .F. )

   // Sorted by time
   DrawScreen( "All methods/functions taking measurable time, sorted by time" )
   memoedit( HBProfileReportToString():new( oProfile:timeSort() ):generate( {|o| o:nTicks > 0 } ), 1,,,, .F. )

   // TBrowse all calls greater than 0
   DrawScreen( "TBrowse all methods/functions called one or more times" )
   Browser( HBProfileReportToTBrowse():new( oProfile:callSort() ):generate( {|o| o:nCalls > 0 }, 1 ) )

   // Some closing stats
   DrawScreen( "Totals" )
   @ 2, 0 Say "  Total Calls: " + str( oProfile:totalCalls() )
   @ 3, 0 Say "  Total Ticks: " + str( oProfile:totalTicks() )
   @ 4, 0 Say "Total Seconds: " + str( oProfile:totalSeconds() )

Return( NIL )

Static Function DrawScreen( cTitle )

   scroll()

   @ 0, 0 Say padr( cTitle, maxcol() + 1 ) Color "n/w"

Return( NIL )

Function DoNothingForTwoSeconds()

   inkey( 2 )

Return( NIL )

Function CallMe500Times()
Return( NIL )

Static Function Browser( oBrowse )
Local lBrowsing := .T.
Local nKey

   Do While lBrowsing
   
      oBrowse:forceStable()
      
      nKey := inkey( 0 )
      
      Do Case
      
         Case nKey == K_ESC
	    lBrowsing := .F.
	    
	 Case nKey == K_DOWN
	    oBrowse:down()
	    
	 Case nKey == K_UP
	    oBrowse:up()
	    
	 Case nKey == K_LEFT
	    oBrowse:left()
	    
	 Case nKey == K_RIGHT
	    oBrowse:right()
	    
	 Case nKey == K_PGDN
	    oBrowse:pageDown()
	    
	 Case nKey == K_PGUP
	    oBrowse:pageUp()
	    
	 // And so on.... (not really necessary for this test)
	    
      EndCase
      
   EndDo

Return( NIL )
