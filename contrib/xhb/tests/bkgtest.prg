/*
 * This test demonstrates usage of BACKGROUND functions that are an extension of IDLE functions;
 * this is a variant of idle functions that runs only on idle state (as Inkey( 0 ) does)
 * Background functions run on every vm execution. PLEASE BE CAREFULLY using these functions.
 * Inside background functions you have to think that them are executed on EVERY VM call.
 *
 * (c) 2003 - Francesco Saverio Giudice <info@fsgiudice.com>
 */

#require "xhb"

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL lActive := .T.
   LOCAL nSpeed  := 500
   LOCAL n

   LOCAL nId1 := hb_idleAdd( {|| IdleFunc( 10, "On Idle - From Block" ) } )
   LOCAL nId2 := hb_idleAdd( { @IdleFunc(), 11, "On Idle - From Array" } )
   LOCAL nId3 := hb_backgroundAdd( @CheckFunc(), 1000, .F. )  // This task is defined but not active
   LOCAL nId4 := hb_backgroundAdd( @TimerFunc(), 1000 )
   LOCAL nId5 := hb_backgroundAdd( {|| Counter1Func() } )
   LOCAL nId6 := hb_backgroundAdd( @Counter2Func(), nSpeed )
   LOCAL nId7 := hb_backgroundAdd( @Counter3Func(), 100 )

   hb_backgroundAdd( @Ticker(), 100 )

   SetColor( "w+/b" )
   CLS
   SetCursor( SC_NONE )

   hb_DispOutAt( 1, 0, PadC( "Harbour - Background and Idle Function Test.", MaxCol() + 1 ) )

   DispInfo( "Background Tasks defined but not running." )
   hb_idleSleep( 3 )

   DispInfo( "Now I'll run a single task manually" )
   hb_idleSleep( 3 )

   DispInfo( "Counter in action" )
   FOR n := 1 TO 10000
      hb_backgroundRun( nId5 )
   NEXT
   hb_DispOutAt( 3, 5, "In lines 10 and 11, two different idle functions" )
   hb_DispOutAt( 4, 5, "will make some text to flash." )
   DispInfo( "Now you will see idle functions running until you press any key" )
   Inkey( 0 )

   DispInfo( "Now manually force to run all background functions" )
   hb_idleSleep( 3 )
   DispInfo( "Background functions running manually" )
   FOR n := 1 TO 100000
      hb_backgroundRunForced()  // Runs background tasks also if SET BACKGROUND TASKS is OFF
      IF n % 1000 == 0 .AND. Inkey() == K_ESC
         EXIT
      ENDIF
   NEXT
   DispInfo( "Now you will see idle functions running until you press any key" )
   Inkey( 0 )

   DispInfo( "Now I set on background tasks" )
   hb_idleSleep( 3 )

// SET BACKGROUND TASK ON

   hb_DispOutAt( 5, 5, "Background functions show timer, ticker, counters and check time elapsed." )
   hb_DispOutAt( 6, 5, "After 40 seconds this program will be forcely quitted." )
   DispInfo( "Program in action with active background tasks" )

   // Now I make check time on
   hb_backgroundActive( nId3, .T. )
   hb_DispOutAt( 18, 10, "Main program counter:" )
   n := 0
   DO WHILE .T.
      /* NOTE: we must pull these manually under Harbour. Maybe better to hook it with hb_idleAdd() */
      hb_backgroundRunForced()
      hb_DispOutAt( 18, 32, Str( ++n, 10 ) )
      IF n % 1000 == 0 .AND. Inkey() == K_ESC
         EXIT
      ENDIF
      IF n % 20000 == 0
         lActive := ! lActive
         hb_backgroundActive( nId7, lActive )
         hb_DispOutAt( 19, 60, "Count3 " + iif( lActive, "ON ", "OFF" ) )
      ENDIF
      IF n % 60000 == 0
         nSpeed := iif( nSpeed == 500, 0, 500 )
         hb_backgroundTime( nId6, nSpeed )
         hb_DispOutAt( 20, 60, "Count2 Time to " + Str( nSpeed, 3 ) )
      ENDIF
      IF n == 130000
         hb_backgroundDel( nId5 )
         hb_DispOutAt( 16, 60, "Count1: DELETED    " )
      ENDIF
   ENDDO

   RETURN

STATIC PROCEDURE DispInfo( cMsg )

   hb_DispOutAt( 23, 0, PadC( cMsg, MaxCol() + 1 ) )

   RETURN

STATIC PROCEDURE IdleFunc( nRow, cStr )

   hb_DispOutAt( nRow, 10, cStr )
   hb_idleSleep( 0.3 )
   hb_DispOutAt( nRow, 10, Space( 69 ) )

   RETURN

STATIC PROCEDURE CheckFunc()

   STATIC s_nSeconds
   STATIC s_nElapsed

   hb_default( @s_nSeconds, Seconds() )

   s_nElapsed := Int( Seconds() - s_nSeconds )
   hb_DispOutAt( 19, 10, "Seconds Elapsed: " + Str( s_nElapsed ) )
   IF s_nElapsed > 40
      DispInfo( "Time elapsed! Quitting. Press any key to exit (note idle running meanwhile)" )
      Inkey( 0 )
      QUIT
   ENDIF

   RETURN

STATIC PROCEDURE TimerFunc()

   hb_DispOutAt( 15, 60, "Time: " + Time() )

   RETURN

STATIC PROCEDURE Counter1Func()

   STATIC s_nCount := 0

   hb_DispOutAt( 16, 60, "Count1: " + Str( s_nCount++ ) )

   RETURN

STATIC PROCEDURE Counter2Func()

   STATIC s_nCount := 0

   hb_DispOutAt( 17, 60, "Count2: " + Str( s_nCount++ ) )

   RETURN

STATIC PROCEDURE Counter3Func()

   STATIC s_nCount := 0

   hb_DispOutAt( 18, 60, "Count3: " + Str( s_nCount++ ) )

   RETURN

STATIC PROCEDURE Ticker()

   STATIC s_nPos := 1
   STATIC s_cText := "This is a sample text. You can press ESC in any moment to exit.    Please note the different speed of counters.    "

   hb_DispOutAt( 22, 0, PadR( SubStr( s_cText, s_nPos ) + Left( s_cText, s_nPos - 1 ), MaxCol() + 1 ) )

   IF ++s_nPos > Len( s_cText )
      s_nPos := 1
   ENDIF

   RETURN
