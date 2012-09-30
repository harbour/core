/*
 * $Id$
 */

// Test routine
// Invoke by running SLEEP 1.0 to sleep 1.0 seconds

PROCEDURE Main( nSleep )

   ? "Time is now: " + Time()
   FT_SLEEP( Val( nSleep ) )
   ? "Time is now: " + Time()

   RETURN
