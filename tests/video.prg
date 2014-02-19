/*
 * Harbour video test code
 * Program originally by Brian Dukes <bdukes@yellowthingy.co.uk>
 * Redirect the output of this program to a file.
 */

#include "box.ch"
#ifdef __HARBOUR__
   #include "hbgtinfo.ch"
#endif

#ifdef __CLIP__
   #xtranslate hb_SecondsCPU( [<x>] ) => SecondsCPU( [<x>] )
#endif
#ifdef FlagShip
   #xtranslate hb_SecondsCPU( [<x>] ) => SecondsCPU( [<x>] )
#endif
#ifdef __XHARBOUR__
   #xtranslate hb_SecondsCPU( [<x>] ) => SecondsCPU( [<x>] )
   #undef __HARBOUR__
#else
   #ifndef __HARBOUR__
      #xtranslate hb_SecondsCPU( [<x>] ) => Seconds( [<x>] )
   #endif
#endif

#ifndef __HARBOUR__
   #xtranslate hb_eol() => ( Chr( 13 ) + Chr( 10 ) )
#endif

#ifdef FlagShip

STATIC s_nDispCount := 0

#xtranslate DispBegin() => iif( ( ++s_nDispCount ) == 1, DispBegin( NIL ), )
#xtranslate DispEnd()   => iif( s_nDispCount > 0 .AND. ( --s_nDispCount ) == 0, DispEnd( NIL ), )

#endif

PROCEDURE Main( xUtf, xCompatBuf )

   LOCAL aResult := {}

#ifdef __HARBOUR__
   hb_cdpSelect( iif( ! Empty( xUtf ), "UTF8", "EN" ) )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )
   hb_gtInfo( HB_GTI_COMPATBUFFER, ! Empty( xCompatBuf ) )
#endif

   Initialise()

   // Perform tests
   AAdd( aResult, StaticText() )
   AAdd( aResult, WindowBounce() )
   AAdd( aResult, ColorBoxes() )

   // Display results
   Summary( aResult )

   RETURN

// initialise the screen

STATIC PROCEDURE Initialise()

   SetColor( "W+/BG" )
#ifdef __HARBOUR__
   DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( hb_UTF8ToStrBox( "░" ), 9 ), "BG/B" )
#else
   DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( Chr( 176 ), 9 ), "BG/B" )
#endif

   RETURN

// repeatedly display a string in the same position
// this test determines how well the Screen i/o subsystem is
// caching screen writes.

STATIC FUNCTION StaticText()

   LOCAL r := MaxRow() / 2
   LOCAL str := Version()
   LOCAL c
   LOCAL i
   LOCAL nEnd
   LOCAL nStart := hb_SecondsCPU()

   str := "Hello World - From " + Left( str, At( " ", str ) - 1 )
   c   := ( MaxCol() - Len( str ) ) / 2

   FOR i := 1 TO 5000
      @ r, c SAY str
   NEXT

   nEnd := hb_SecondsCPU()

   RETURN PadR( "StaticText:", 14 ) + ;
      "Iterations=5000, Time=" + hb_ntos( nEnd - nStart ) + " secs, " + ;
      "Average FPS=" + hb_ntos( Round( 5000 / ( nEnd - nStart ), 0 ) )

// Bounce a window around the screen a few thousand times
// timing the duration, and determining the average FPS

STATIC FUNCTION WindowBounce()

   LOCAL nBoxes  := Min( MaxRow(), MaxCol() - 7 ) - 6 /* keep the box in bounds */
   LOCAL x       := Array( NBOXES )
   LOCAL y       := Array( NBOXES )
   LOCAL dx      := Array( NBOXES )
   LOCAL dy      := Array( NBOXES )
   LOCAL clr     := Array( NBOXES )
   LOCAL scr     := Array( NBOXES )
   LOCAL nFrames := 0
   LOCAL nStart
   LOCAL nEnd
   LOCAL i
   LOCAL aCol := { ;
      "N", "B", "G", "BG", "R", "RB", "GR", "W", ;
      "N*", "B*", "G*", "BG*", "R*", "RB*", "GR*", "W*" }

   // initialise boxes
   FOR i := 1 TO nBoxes
      x[ i ]   := i
      y[ i ]   := i - 1
      dx[ i ]  := -1
      dy[ i ]  := 1
      clr[ i ] := "W+/" + aCol[ ( i - 1 ) % 16 + 1 ]
   NEXT

   nStart := hb_SecondsCPU()
   DispBegin()

   DO WHILE nFrames < 5000

      FOR i := 1 TO nBoxes
         scr[ i ] := SaveScreen( x[ i ], y[ i ], x[ i ] + 6, y[ i ] + 12 )
#ifdef HB_B_SINGLE_UNI
         @ x[ i ], y[ i ], x[ i ] + 6, y[ i ] + 12 BOX HB_B_SINGLE_UNI + " " COLOR clr[ i ]
#else
         @ x[ i ], y[ i ], x[ i ] + 6, y[ i ] + 12 BOX B_SINGLE + " " COLOR clr[ i ]
#endif
      NEXT

      DispEnd()
      DispBegin()

      FOR i := nBoxes TO 1 STEP -1
         // remove boxes from screen
         RestScreen( x[ i ], y[ i ], x[ i ] + 6, y[ i ] + 12, scr[ i ] )

         // move
         x[ i ] += dx[ i ]
         y[ i ] += dy[ i ]
         IF x[ i ] <= 0 .OR. x[ i ] + 6 >= MaxRow()
            dx[ i ] := -dx[ i ]
         ENDIF
         IF y[ i ] <= 0 .OR. y[ i ] + 12 >= MaxCol()
            dy[ i ] := -dy[ i ]
         ENDIF
      NEXT

      ++nFrames
   ENDDO

   DispEnd()
   nEnd := hb_SecondsCPU()

   RETURN PadR( "WindowBounce:", 14 ) + ;
      "Iterations=" + hb_ntos( nFrames ) + ", " + ;
      "Time=" + hb_ntos( nEnd - nStart ) + " secs, " + ;
      "Average FPS=" + hb_ntos( Round( nFrames / ( nEnd - nStart ), 0 ) )

// Display color boxes, repeatedly, this will determine
// how efficiently the screen I/O subsystem is caching the
// DispBegin()'s and DispEnd()'s

STATIC FUNCTION ColorBoxes()

   LOCAL nFrames := 0
   LOCAL nStart
   LOCAL nEnd
   LOCAL i
   LOCAL nDir := 1
   LOCAL nDepth := 0
   LOCAL aCol := { ;
      "N", "B", "G", "BG", "R", "RB", "GR", "W", ;
      "N*", "B*", "G*", "BG*", "R*", "RB*", "GR*", "W*" }

   nStart := hb_SecondsCPU()
   // display boxes to screen

   DO WHILE nFrames < 5000
      IF nDir == 1
         DispBegin()
      ELSE
         DispEnd()
      ENDIF

      nDepth += nDir

      IF nDepth > 4 .OR. nDepth < 1
         nDir := -nDir
      ENDIF
      i := nFrames % 16 + 1
      DispBox( 5, 10, MaxRow() - 5, MaxCol() - 10, ;
         Replicate( Chr( i + 64 ), 9 ), ;
         "W+/" + aCol[ i ] )
      ++nFrames
   ENDDO

   // remove any nested dispbegins()
   DO WHILE nDepth > 0
      DispEnd()
      nDepth--
   ENDDO

   nEnd := hb_SecondsCPU()

   RETURN PadR( "ColorBoxes:", 14 ) + ;
      "Iterations=" + hb_ntos( nFrames ) + ", " + ;
      "Time=" + hb_ntos( nEnd - nStart ) + " secs, " + ;
      "Average FPS=" + hb_ntos( Round( nFrames / ( nEnd - nStart ), 0 ) )

// display results

STATIC PROCEDURE Summary( aResult )

   LOCAL i

   CLS
#ifdef __HARBOUR__
   ? ;
      "codepage:", hb_cdpSelect(), ;
      "box CP:", hb_gtInfo( HB_GTI_BOXCP ), ;
      "compat buffer:", hb_gtInfo( HB_GTI_COMPATBUFFER )
#endif
   ? "Resolution:", hb_ntos( MaxRow() + 1 ), "x", hb_ntos( MaxCol() + 1 ), Version()
   FOR i := 1 TO Len( aResult )
      ? aResult[ i ]
   NEXT
   ?
   WAIT

   RETURN
