/*
 * $Id$
 */

/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   This is a simple Clipper program runs simple GET and BROWSE dialogs
   each on a pseudo-window with brief help message on the bottom of
   the screen.

   Note that this Clipper program uses ZNEWWINDOW() and ZREVWINDOW() to
   open and close every pseudo-windows respectively.
*/

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"

STATIC s_zwin := {}
STATIC s_cStdColor := "N/W,N/GR*,,,N/W*"

PROCEDURE Main()

   LOCAL i, j

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   SET SCOREBOARD OFF
   SetColor( s_cStdColor )
   SetCursor( SC_NONE )
   CLS
   @ 0, 0 SAY PadC( "This is the Main Window", MaxCol() + 1 )

   // screen background
   DispBegin()
   FOR i := 1 TO MaxRow() - 1
      FOR j := 0 TO MaxCol()
         hb_DispOutAtBox( i, j, hb_UTF8ToStrBox( "▒" ) )
      NEXT
   NEXT
   DispEnd()

   lboxmessage( "Welcome to our test program." + hb_eol() + ;
      "This program will show typical GET and BROWSE dialogs " + ;
      "with brief help on the bottom of the screen." )
   xGet1()
   xBrowse1()
   lboxmessage( "That's all folks" )

   // restore state
   SetCursor( SC_NORMAL )

   RETURN // main

PROCEDURE xGet1()

   LOCAL cName := PadR( "Name", 20 )
   LOCAL cAddr := PadR( "Address", 25 )
   LOCAL cPhone := PadR( "Phone", 15 )
   LOCAL cFax  := PadR( "Fax", 15 )
   LOCAL lDone := .F.
   LOCAL getlist := {}
   LOCAL oldCurs := SetCursor( SC_NORMAL )

   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), 10, 20, 22, 59, "Some Window" )

#if 0
   @ 21, 21 SAY "Inside the window" COLOR "R/W"
   @ 23, 0  SAY "Outside the window" COLOR "R/W"
#endif

   DO WHILE ! lDone
      @ 12, 22 SAY "Name    : " GET cName  PICT "@!K" WHEN lMessage( "Please enter your name" )
      @ 14, 22 SAY "Address : " GET cAddr  PICT "@!K" WHEN lMessage( "Please enter your address" )
      @ 16, 22 SAY "Phone   : " GET cPhone PICT "@K"  WHEN lMessage( "Please enter your phone number" )
      @ 18, 22 SAY "Fax     : " GET cFax   PICT "@K"  WHEN lMessage( "Please enter your fax number" )
      READ

      lMessage( "" )
      lDone := lyesno( "Done?" )
   ENDDO

   zrevwindow()

   SetCursor( oldCurs )

   RETURN // xGet1()

/* the following is adapted from WVTGUI.PRG by Pritpal Bedi
   for illustration purposes only */

FUNCTION xBrowse1()

   LOCAL nKey, bBlock, oBrowse, i
   LOCAL lEnd    := .F.
   LOCAL info_
   LOCAL nTop    :=  6
   LOCAL nLeft   :=  3
   LOCAL nBottom := MaxRow() - 2
   LOCAL nRight  := MaxCol() - 3
   LOCAL nCursor := SetCursor( SC_NONE )

   USE "..\..\..\tests\test" NEW
   IF NetErr()
      RETURN NIL
   ENDIF
   info_ := dbStruct()

   SetColor( "N/W*,N/GR*,,,N/W* " )
   oBrowse := TBrowseNew( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )

   oBrowse:ColSep        := hb_UTF8ToStrBox( "│" )
   oBrowse:HeadSep       := hb_UTF8ToStrBox( "─" )
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip, oBrowse ) }

   FOR i := 1 TO Len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i, 1 ], bBlock ) )
   NEXT

   oBrowse:configure()

   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTop, nLeft, nBottom, nRight, "test.dbf" )

   WHILE ! lEnd
      oBrowse:ForceStable()

      lMessage( "Record #" + hb_ntos( RecNo() ) )

      nKey := Inkey( 0 )

      DO CASE
      CASE nKey == K_ESC .OR. nKey == K_ENTER
         lEnd := lYesNo( "Done?" )
      CASE nKey == K_DOWN
         oBrowse:Down()
      CASE nKey == K_UP
         oBrowse:Up()
      CASE nKey == K_LEFT
         oBrowse:Left()
      CASE nKey == K_RIGHT
         oBrowse:Right()
      CASE nKey == K_PGDN
         oBrowse:pageDown()
      CASE nKey == K_PGUP
         oBrowse:pageUp()
      CASE nKey == K_CTRL_PGUP
         oBrowse:goTop()
      CASE nKey == K_CTRL_PGDN
         oBrowse:goBottom()
      CASE nKey == K_HOME
         oBrowse:home()
      CASE nKey == K_END
         oBrowse:end()
      CASE nKey == K_CTRL_LEFT
         oBrowse:panLeft()
      CASE nKey == K_CTRL_RIGHT
         oBrowse:panRight()
      CASE nKey == K_CTRL_HOME
         oBrowse:panHome()
      CASE nKey == K_CTRL_END
         oBrowse:panEnd()
      ENDCASE
   ENDDO

   lMessage( "" )

   zrevwindow()

   // restore state
   SetCursor( nCursor )

   dbCloseArea()

   RETURN NIL

//

STATIC FUNCTION DbSkipBlock( n, oTbr )

   LOCAL nSkipped := 0

   HB_SYMBOL_UNUSED( oTbr )

   IF n == 0
      dbSkip( 0 )
   ELSEIF n > 0
      DO WHILE nSkipped != n .AND. TBNext( oTbr )
         nSkipped++
      ENDDO
   ELSE
      DO WHILE nSkipped != n .AND. TBPrev( oTbr )
         nSkipped--
      ENDDO
   ENDIF

   RETURN  nSkipped

//

STATIC FUNCTION TBNext( oTbr )

   LOCAL nSaveRecNum := RecNo()
   LOCAL lMoved := .T.

   HB_SYMBOL_UNUSED( oTbr )

   IF Eof()
      lMoved := .F.
   ELSE
      dbSkip( 1 )
      IF Eof()
         lMoved := .F.
         dbGoto( nSaveRecNum )
      ENDIF
   ENDIF

   RETURN lMoved

//

STATIC FUNCTION TBPrev( oTbr )

   LOCAL nSaveRecNum := RecNo()
   LOCAL lMoved := .T.

   HB_SYMBOL_UNUSED( oTbr )

   dbSkip( -1 )
   IF Bof()
      dbGoto( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

//

STATIC FUNCTION VouBlockField( i )

   RETURN  {|| FieldGet( i ) }

// supporting functions ***************************

// displays a message on maxrow() and returns .T.
FUNCTION lMessage( cMsg )

   LOCAL cOldColor := SetColor( s_cStdColor )

   @ MaxRow(), 0 SAY PadC( cMsg, MaxCol() + 1 )
   SetColor( cOldColor )

   RETURN .T.

// display cmsg with Yes/No option, returns .T. if Yes selected
FUNCTION lYesNo( cMsg )

   LOCAL nTopLine, ;
      nLeft := 5, ;
      nBotLine := MaxRow() - 2, ;
      nRight := MaxCol() - 5
   LOCAL nChoice, nWidth
   LOCAL oldCurs := SetCursor( SC_NONE )
   LOCAL oldColor := SetColor( s_cStdColor )

   hb_default( @cMsg, "Please Confirm" )

   cmsg := " " + AllTrim( cmsg ) + " "
   nWidth := Max( Len( cmsg ), Len( "Yes" ) )
   nTopLine := nBotLine - 2 - 1

   nLeft := Max( nLeft, ( ( nRight + nLeft ) * .5 ) - ( nWidth * .5 ) - 1 )
   nRight := nLeft + nWidth + 1

   // open window
   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTopLine, nLeft, nBotLine, nRight, cMsg )

   @ nTopLine + 1, nLeft + 1 PROMPT PadR( "Yes", nWidth )
   @ nTopLine + 2, nLeft + 1 PROMPT PadR( "No", nWidth )
   MENU TO nChoice

   // close window
   zrevwindow()

   SetCursor( oldCurs )
   SetColor( oldColor )

   RETURN nChoice == 1

FUNCTION lBoxMessage( cMsg, cTitle )

   LOCAL nTopLine, ;
      nLeft := 5, ;
      nBotLine := MaxRow() - 2, ;
      nRight := MaxCol() - 5
   LOCAL nwidth, nmaxwidth, i, nNumLines, cAline
   LOCAL oldCurs := SetCursor( SC_NONE )
   LOCAL oldColor := SetColor( s_cStdColor )

   hb_default( @cTitle, "Info" )

   cmsg := AllTrim( cmsg )
   nNumLines := MLCount( cmsg, ( nright - nleft ) - 1 )
   nWidth := iif( nNumLines < 2, Len( cmsg ), nRight - nLeft - 1 )
   nTopLine := nBotLine - nNumLines - 1
   IF nTopLine < 0            // too many lines to display
      nNumLines += nTopLine
      nTopLine := 0
   ENDIF

   nMaxWidth := 0
   FOR i := 1 TO nNumLines
      nMaxWidth := Max( nMaxWidth, Len( RTrim( MemoLine( cmsg, nwidth, i ) ) ) )
   NEXT

   nLeft := Max( nLeft, Int( ( ( nRight + nLeft ) / 2 ) - ( nMaxWidth / 2 ) - 1 ) )
   nRight := nLeft + nMaxWidth + 1

   // open window
   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTopLine, nLeft, nBotLine, nRight, cTitle )
   DispBegin()
   FOR i := 1 TO nNumLines
      cAline := MemoLine( cMsg, nWidth, i )
      hb_DispOutAt( nTopLine + i, nLeft + 1, PadC( AllTrim( cAline ), nMaxWidth ) )
   NEXT
   DispEnd()

   Inkey( 0 )

   // close window
   zrevwindow()

   SetCursor( oldCurs )
   SetColor( oldColor )

   RETURN .T.

// Draw a new window on screen and register it in window list
// wtype       : Window border type, eg. "┌─┐│┘─└│"
// r1,c1,r2,c2 : coordinates
// Return      : Numeric id of the new window

FUNCTION ZNEWWINDOW( wtype, r1, c1, r2, c2, ctitle, ccolor )

   LOCAL i := Len( s_zwin )
   LOCAL cScreen := SaveScreen( r1, c1, r2, c2 )
   LOCAL cOldColor := SetColor()
   LOCAL nrow := Row(), ncol := Col()

   hb_default( @ctitle, "" )
   hb_default( @ccolor, s_cStdColor )
   SetColor( ccolor )

   AAdd( s_zwin, { i + 1, r1, c1, r2, c2, cScreen, ctitle, nrow, ncol, coldcolor } )

   SetColor( ccolor )

   hb_Scroll( r1, c1, r2, c2 )

   // GTWVW doesn't need box or textual title
   hb_DispBox( r1, c1, r2, c2, wtype )
   IF ! Empty( ctitle )
      cTitle := " " + AllTrim( ctitle ) + " "
      hb_DispOutAt( r1, nCeiling( ( c2 + c1 - Len( cTitle ) ) / 2 ), cTitle )
   ENDIF

   SetColor( cOldColor )

   RETURN i + 1

// Closes the last window and remove it from window list
FUNCTION ZREVWINDOW()

   LOCAL i := Len( s_zwin )

   IF i == 0
      // no window to close
      RETURN NIL
   ENDIF

   // restore states
   RestScreen( s_zwin[ i ][ 2 ], s_zwin[ i ][ 3 ], s_zwin[ i ][ 4 ], s_zwin[ i ][ 5 ], s_zwin[ i ][ 6 ] )
   SetPos( s_zwin[ i ][ 8 ], s_zwin[ i ][ 9 ] )
   SetColor( s_zwin[ i ][ 10 ] )

   // remove window from list
   hb_ADel( s_zwin, i, .T. )

   RETURN NIL

FUNCTION nCeiling( nNumber )

   LOCAL nTemp

   nTemp := nNumber - Int( nNumber )  // right of dec point
   IF nTemp > 0
      nNumber := Int( nNumber ) + 1
   ELSE
      nNumber := Int( nNumber )
   ENDIF

   RETURN nNumber
