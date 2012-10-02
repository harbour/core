/*
 * $Id$
 */

/*
 * Harbour Project source code
 * http://harbour-project.org/
 *
 * Clipper Tool III like window system test program
 * Donated to the public domain on 2006-02-11 by Przemyslaw Czerpak
 */

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL aWin := Array( 9 ), y, x, i, k, lFlag := .F. , lBoard := .T.

   SetBlink( .F. )
   wboard( 5, 5, 20, 75 )
   wmode( .T. , .T. , .T. , .T. )
   wsetshadow( 7 )
   setclearA( 10 * 16 + 14 )
   setclearB( 35 )
   DispBox( 0, 0, MaxRow(), MaxCol(), replicate( "#", 9 ), ntocolor( 10 * 16 + 14 ) )
   SetPos( 0, 0 )
   ? "GT driver: " + hb_gtVersion()
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

   setclearB( 61 )
   FOR i := 1 TO Len( aWin )
      y := i + 2
      x := i * 4 + 10
      SetColor( ntocolor( i * 16 + 15 ) + ",W+/B*" )
      wsetshadow( i % 8 )
      aWin[ i ] := wopen( y, x, y + 10, x + 20 )
      wbox()

      @ - 1, 0 SAY "TITLE " + hb_ntos( aWin[ i ] )
      ? hb_ntos( Row() ) + ":" + hb_ntos( Col() ), "/", hb_ntos( MaxRow() ) + ":" + hb_ntos( MaxCol() ), ""
      ? hb_ntos( wrow() ) + ":" + hb_ntos( wcol() ), "/", hb_ntos( MaxRow( .T. ) ) + ":" + hb_ntos( MaxCol( .T. ) ), ""
      ? hb_ntos( wfrow() ) + ":" + hb_ntos( wfcol() ), "/", ;
         hb_ntos( wflastrow() ) + ":" + hb_ntos( wflastcol() ), ""
      ? hb_ntos( wfrow( .T. ) ) + ":" + hb_ntos( wfcol( .T. ) ), "/", ;
         hb_ntos( wflastrow( .T. ) ) + ":" + hb_ntos( wflastcol( .T. ) ), ""
      ? "window:", hb_ntos( aWin[ i ] ), ""
      SetCursor( Int( i % 5 ) )

   NEXT

   dspcord()
   WHILE .T.
      k := Inkey( 0, INKEY_ALL )
      IF k == K_ESC
         EXIT
      ELSEIF k >= Asc( "1" ) .AND. k <= Asc( "9" )
         wselect( aWin[ k - Asc( "0" ) ] )
      ELSEIF k == Asc( "0" )
         wselect( 0 )
      ELSEIF k == Asc( "C" ) .OR. k == Asc( "c" )
         wclose()
      ELSEIF k == Asc( "Q" ) .OR. k == Asc( "q" )
         CLS
      ELSEIF k == Asc( "B" ) .OR. k == Asc( "b" )
         IF lBoard
            wboard( 0, 0, MaxRow( .T. ) - 1, MaxCol( .T. ) )
         ELSE
            wboard( 5, 5, 20, 75 )
         ENDIF
         lBoard := !lBoard
      ELSEIF k == Asc( "P" ) .OR. k == Asc( "P" )
         y := wfrow()
         x := wfcol()
         i := wselect()
         wselect( 0 )
         @ y, x SAY "THIS IS WINDOW 0 OUTPUT"
         wselect( i )
      ELSEIF k == K_INS
         lFlag := !lFlag
         SetCursor( iif( lFlag, 3, 1 ) )
      ELSEIF k == K_DEL
         SetCursor( SC_NONE )
      ELSEIF k == K_LEFT
         wmove( wrow(), wcol() - 1 )
      ELSEIF k == K_RIGHT
         wmove( wrow(), wcol() + 1 )
      ELSEIF k == K_UP
         wmove( wrow() - 1, wcol() )
      ELSEIF k == K_DOWN
         wmove( wrow() + 1, wcol() )
      ENDIF
      dspcord()
   ENDDO

   RETURN

STATIC PROC dspcord()

   LOCAL mr := MRow(), mc := MCol(), r := wrow(), c := wcol(), w := wselect()

   wselect( 0 )
   @ MaxRow(), 0 SAY PadR( "WPOS(" + LTrim( Str( r ) ) + "," + LTrim( Str( c ) ) + ")" + ;
      iif( MPresent(), "MPOS(" + LTrim( Str( mr ) ) + "," + LTrim( Str( mc ) ) + ")", "" ), MaxCol() + 1 )
   wselect( w )

   RETURN
