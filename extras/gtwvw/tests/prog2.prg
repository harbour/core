/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   To present prettier displays we may want to use pseudo-GUI objects.
   This, however, is not as easy as the previous example.
   Once you understand how GTWVW draws these objects, you can put
   pseudo-GUI objects onto each window, one by one.

   Notes: GTWVW now also has native Windows Controls. See other samples.
*/

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"

#ifdef __HARBOUR__
#define __GTWVW__
#endif

STATIC s_zwin := {}
STATIC s_cStdColor := "N/W,N/GR*,,,N/W*"

#ifdef __GTWVW__
STATIC s_amiscobjlist := {}      // x misc object list (actually: list of codeblocks)
#endif

PROCEDURE Main()

   LOCAL i, j
#ifdef __GTWVW__
   LOCAL lMainCoord
   LOCAL nMaxRow
   LOCAL nMaxCol
#endif

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

#ifdef __GTWVW__
   lMainCoord := wvw_SetMainCoord( .T. )
   nMaxRow := MaxRow()
   nMaxCol := MaxCol()

   wvw_SetFont( , "Lucida Console", 16, - 8 )
   wvw_SetCodepage( , 255 )
   wvw_sbCreate()
#endif

   SET SCOREBOARD OFF
   SetColor( s_cStdColor )
   SetCursor( SC_NONE )
   CLS
   @ 0, 0 SAY PadC( "This is the Main Window", MaxCol() + 1 )

   // screen background
#ifndef __GTWVW__
   DispBegin()
   FOR i := 1 TO MaxRow() - 1
      FOR j := 0 TO MaxCol()
         hb_DispOutAtBox( i, j, hb_UTF8ToStrBox( "▒" ) )
      NEXT
   NEXT
   DispEnd()
#else
   ResetMiscObjects( 0 )   // make sure we start with no GUI objects
   AddMiscObjects( 0, {| nWindow | wvw_DrawImage( nWindow, 1, 0, nmaxrow, nmaxcol, "vouch1.bmp" ) } )
#endif

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
#ifdef __GTWVW__
   LOCAL nWin
   MEMVAR __temp__
#endif

#ifdef __GTWVW__
   nWin := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), 10, 20, 22, 59, "Some Window" )

   AddMiscObjects( nWin, {| nWindow | __temp__ := nWindow, AEval( GetList, {| oGet | wvw_DrawBoxGet( __temp__, oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )
#else
   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), 10, 20, 22, 59, "Some Window" )
#endif

#if 0
   @ 21, 21 say "Inside the window" color "R/W"
   @ 23, 0  say "Outside the window" color "R/W"
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

/* the following is adapted from wvtgui.prg by Pritpal Bedi
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
   LOCAL nWin

#ifdef __GTWVW__
   LOCAL aColumnsSep, tmp
#endif

   USE "..\..\..\tests\test" NEW
   IF NetErr()
      RETURN NIL
   ENDIF
   info_ := dbStruct()

   SetColor( "N/W*,N/GR*,,,N/W* " )
   oBrowse := TBrowseNew( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )

#ifndef __GTWVW__
   oBrowse:ColSep        := hb_UTF8ToStrBox( "│" )
   oBrowse:HeadSep       := hb_UTF8ToStrBox( "═" )
#else
   oBrowse:ColSep        := "  "     // we'll draw a line between these spaces
   oBrowse:HeadSep       := "__"
#endif
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip, oBrowse ) }

   FOR i := 1 TO Len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i, 1 ], bBlock ) )
   NEXT

   oBrowse:configure()

   nWin := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTop, nLeft, nBottom, nRight, "test.dbf" )

#ifdef __GTWVW__
   wvw_SetPen( 0, 0, RGB( 210, 1210, 210 ) )

   aColumnsSep := Array( oBrowse:colCount )
   FOR EACH tmp IN aColumnsSep
      tmp := oBrowse:getColumn( tmp:__enumIndex() ):colSep
   NEXT

   AddMiscObjects( nWin, {| nWindow | wvw_DrawBoxRecessed( nWindow, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 ) } )
   AddMiscObjects( nWin, {| nWindow | wvw_DrawGridHorz( nWindow, oBrowse:nTop + 3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   AddMiscObjects( nWin, {| nWindow | wvw_DrawGridVert( nWindow, oBrowse:nTop, oBrowse:nBottom, aColumnsSep, Len( aColumnsSep ) ) } )
#endif

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

   dbSkip( -1 )
   IF Bof()
      dbGoto( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

//

STATIC FUNCTION VouBlockField( i )

   RETURN {|| FieldGet( i ) }

// supporting functions ***************************

// displays a message on MaxRow() and returns .T.
FUNCTION lMessage( cMsg )

#ifndef __GTWVW__

   LOCAL cOldColor := SetColor( s_cStdColor )

   @ MaxRow(), 0 SAY PadC( cMsg, MaxCol() + 1 )
   SetColor( cOldColor )

#else

   // displays a message on status bar of Main Window and returns .T.
   wvw_sbSetText( 0, 0, cMsg )

#endif

   RETURN .T.

FUNCTION lYesNo( cMsg )

   // display cmsg with Yes/No option, returns .T. if Yes selected
   LOCAL nTopLine, ;
      nLeft := 5, ;
      nBotLine := MaxRow() - 2, ;
      nRight := MaxCol() - 5
   LOCAL nChoice, nWidth, nWinNum
   LOCAL oldCurs := SetCursor( SC_NONE )
   LOCAL oldColor := SetColor( s_cStdColor )

   hb_default( @cMsg, "Please Confirm" )

   cmsg := " " + AllTrim( cmsg ) + " "
   nWidth := Max( Len( cmsg ), Len( "Yes" ) )
   nTopLine := nBotLine - 2 - 1

   nLeft := Max( nLeft, ( ( nRight + nLeft ) * .5 ) - ( nWidth * .5 ) - 1 )
   nRight := nLeft + nWidth + 1

   // open window
   nWinNum := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTopLine, nLeft, nBotLine, nRight, cMsg )

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
   LOCAL nwidth, nmaxwidth, i, nNumLines, cAline, nWinNum
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
   nWinNum := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTopLine, nLeft, nBotLine, nRight, cTitle )
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

#ifdef __GTWVW__
   wvw_nOpenWindow( ctitle, r1, c1, r2, c2 )
   ResetMiscObjects( NIL )   // make sure we start with no GUI objects
#endif

   AAdd( s_zwin, { i + 1, r1, c1, r2, c2, cScreen, ctitle, nrow, ncol, coldcolor } )

   SetColor( ccolor )

   hb_Scroll( r1, c1, r2, c2 )

#ifndef __GTWVW__
   // GTWVW doesn't need box or textual title
   hb_DispBox( r1, c1, r2, c2, wtype )
   IF ! Empty( ctitle )
      cTitle := " " + AllTrim( ctitle ) + " "
      hb_DispOutAt( r1, nCeiling( ( c2 + c1 - Len( cTitle ) ) / 2 ), cTitle )
   ENDIF
#endif

   SetColor( cOldColor )

   RETURN i + 1

FUNCTION ZREVWINDOW()

   // Closes the last window and remove it from window list
   LOCAL i := Len( s_zwin )

   IF i == 0
      // no window to close
      RETURN NIL
   ENDIF

#ifdef __GTWVW__
   ResetMiscObjects( NIL )   // clear all GUI objects, if any
   wvw_lCloseWindow()
#endif

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

#ifdef __GTWVW__

//
//      WVW_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
// WARNING: it now receives only nWinNum parameter
//

FUNCTION WVW_Paint( nWinNum )

   IF Len( s_amiscobjlist ) >= nWinNum + 1
      AEval( s_amiscobjlist[ nWinNum + 1 ], {| e | Eval( e, nWinNum ) } )
   ENDIF

   RETURN 0

FUNCTION ResetMiscObjects( nWinNum )

   hb_default( @nWinNum, wvw_nNumWindows() - 1 )

   DO WHILE Len( s_amiscobjlist ) < nWinNum + 1
      AAdd( s_amiscobjlist, {} )
   ENDDO
   s_amiscobjlist[ nWinNum + 1 ] := {}

   RETURN .T.

FUNCTION AddMiscObjects( nWinNum, bAction )

   AAdd( s_amiscobjlist[ nWinNum + 1 ], bAction )

   RETURN .T.

#endif
