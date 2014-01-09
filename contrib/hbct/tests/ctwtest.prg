/*
 * Harbour Project source code
 * http://harbour-project.org/
 *
 * Clipper Tool III like window system test program
 * Donated to the public domain on 2006-02-11 by Przemyslaw Czerpak
 */

#ifdef __HARBOUR__
#require "hbct"
#endif

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL aWin := Array( 9 ), y, x, i, k, lFlag := .F., lBoard := .T.

   SetBlink( .F. )
   WBoard( 5, 5, 20, 75 )
   WMode( .T., .T., .T., .T. )
   WSetShadow( 7 )
   SetClearA( 10 * 16 + 14 )
   SetClearB( 35 )
   DispBox( 0, 0, MaxRow(), MaxCol(), Replicate( "#", 9 ), NToColor( 10 * 16 + 14 ) )
   SetPos( 0, 0 )
   ? "GT driver:", hb_gtVersion()
   ? hb_gtVersion( 1 )
   ?
   ? "ESC - quit "
   ? "0 - select window 0 (base screen) "
   ? "1-9 select window 1-9 "
   ? "C - close window "
   ? "Q - clear screen "
   ? "P - print text at window 0 "
   ? "B - board switch "
   ? "INS - cursor shape "
   ? "DEL - hide cursor "
   ? "arrows - window move "

   SetClearB( 61 )
   FOR i := 1 TO Len( aWin )
      y := i + 2
      x := i * 4 + 10
      SetColor( NToColor( i * 16 + 15 ) + ",W+/B*" )
      WSetShadow( i % 8 )
      aWin[ i ] := WOpen( y, x, y + 10, x + 20 )
      WBox()

      @ -1, 0 SAY "TITLE " + hb_ntos( aWin[ i ] )
      ? hb_ntos( Row()        ) + ":" + hb_ntos( Col()        ), "/", hb_ntos( MaxRow()         ) + ":" + hb_ntos( MaxCol()         ), ""
      ? hb_ntos( WRow()       ) + ":" + hb_ntos( WCol()       ), "/", hb_ntos( MaxRow( .T. )    ) + ":" + hb_ntos( MaxCol( .T. )    ), ""
      ? hb_ntos( WFRow()      ) + ":" + hb_ntos( WFCol()      ), "/", hb_ntos( WFLastRow()      ) + ":" + hb_ntos( WFLastCol()      ), ""
      ? hb_ntos( WFRow( .T. ) ) + ":" + hb_ntos( WFCol( .T. ) ), "/", hb_ntos( WFLastRow( .T. ) ) + ":" + hb_ntos( WFLastCol( .T. ) ), ""
      ? "window:", hb_ntos( aWin[ i ] ), ""
      SetCursor( Int( i % 5 ) )

   NEXT

   dspcord()
   DO WHILE .T.
      k := Inkey( 0, INKEY_ALL )
      DO CASE
      CASE k == K_ESC
         EXIT
      CASE k >= hb_keyCode( "1" ) .AND. k <= hb_keyCode( "9" )
         WSelect( aWin[ k - hb_keyCode( "0" ) ] )
      CASE k == hb_keyCode( "0" )
         WSelect( 0 )
      CASE k == hb_keyCode( "C" ) .OR. k == hb_keyCode( "c" )
         WClose()
      CASE k == hb_keyCode( "Q" ) .OR. k == hb_keyCode( "q" )
         CLS
      CASE k == hb_keyCode( "B" ) .OR. k == hb_keyCode( "b" )
         IF lBoard
            WBoard( 0, 0, MaxRow( .T. ) - 1, MaxCol( .T. ) )
         ELSE
            WBoard( 5, 5, 20, 75 )
         ENDIF
         lBoard := ! lBoard
      CASE k == hb_keyCode( "P" ) .OR. k == hb_keyCode( "P" )
         y := WFRow()
         x := WFCol()
         i := WSelect()
         WSelect( 0 )
         @ y, x SAY "THIS IS WINDOW 0 OUTPUT"
         WSelect( i )
      CASE k == K_INS
         lFlag := ! lFlag
         SetCursor( iif( lFlag, 3, 1 ) )
      CASE k == K_DEL
         SetCursor( SC_NONE )
      CASE k == K_LEFT
         WMove( WRow(), WCol() - 1 )
      CASE k == K_RIGHT
         WMove( WRow(), WCol() + 1 )
      CASE k == K_UP
         WMove( WRow() - 1, WCol() )
      CASE k == K_DOWN
         WMove( WRow() + 1, WCol() )
      ENDCASE
      dspcord()
   ENDDO

   RETURN

STATIC PROCEDURE dspcord()

   LOCAL mr := MRow(), mc := MCol(), r := WRow(), c := WCol(), w := WSelect()

   WSelect( 0 )
   @ MaxRow(), 0 SAY PadR( "WPOS(" + hb_ntos( r ) + "," + hb_ntos( c ) + ")" + ;
      iif( MPresent(), "MPOS(" + hb_ntos( mr ) + "," + hb_ntos( mc ) + ")", "" ), MaxCol() + 1 )
   WSelect( w )

   RETURN
