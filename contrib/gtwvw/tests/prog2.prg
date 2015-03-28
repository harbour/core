/* Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   To present prettier displays we may want to use pseudo-GUI objects.
   This, however, is not as easy as the previous example.
   Once you understand how GTWVW draws these objects, you can put
   pseudo-GUI objects onto each window, one by one.

   Notes: GTWVW now also has native Windows Controls. See other samples. */

#require "gtwvw"
#require "hbtest"

#include "dbstruct.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define _STD_COLOR_  "N/W,N/GR*,,,N/W*"

STATIC s_zwin := {}
STATIC s_amiscobjlist := {}      // misc object list (actually: list of codeblocks)

PROCEDURE Main()

   LOCAL nMaxRow
   LOCAL nMaxCol

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   wvw_SetMainCoord( .T. )
   nMaxRow := MaxRow()
   nMaxCol := MaxCol()

   wvw_SetFont( , "Lucida Console", 16, -8 )
   wvw_SetCodepage( , 255 )
   wvw_sbCreate()

   Set( _SET_SCOREBOARD, .F. )
   SetColor( _STD_COLOR_ )
   SetCursor( SC_NONE )
   CLS
   @ 0, 0 SAY PadC( "This is the Main Window", MaxCol() + 1 )

   // screen background
   ResetMiscObjects( 0 )   // make sure we start with no GUI objects
   AddMiscObjects( 0, {| nWindow | wvw_DrawImage( nWindow, 1, 0, nmaxrow, nmaxcol, hb_DirBase() + "vouch1.bmp" ) } )

   lboxmessage( ;
      "Welcome to our test program." + hb_eol() + ;
      "This program will show typical GET and BROWSE dialogs " + ;
      "with brief help on the bottom of the screen." )
   xGet1()
   xBrowse1()
   lboxmessage( "That's all folks" )

   RETURN

STATIC PROCEDURE xGet1()

   LOCAL cName  := PadR( "Name", 20 )
   LOCAL cAddr  := PadR( "Address", 25 )
   LOCAL cPhone := PadR( "Phone", 15 )
   LOCAL cFax   := PadR( "Fax", 15 )
   LOCAL cEmail := PadR( "E-mail", 15 )

   LOCAL nWin := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), 10, 20, 22, 59, "Some Window" )
   LOCAL GetList := {}

   MEMVAR __temp__

   AddMiscObjects( nWin, {| nWindow | __temp__ := nWindow, AEval( GetList, {| oGet | wvw_DrawBoxGet( __temp__, oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   DO WHILE .T.
      @ 12, 22 SAY "Name    :" GET cName  PICTURE "@!K" WHEN lMessage( "Please enter your name" )
      @ 14, 22 SAY "Address :" GET cAddr  PICTURE "@!K" WHEN lMessage( "Please enter your address" )
      @ 16, 22 SAY "Phone   :" GET cPhone PICTURE "@K"  WHEN lMessage( "Please enter your phone number" )
      @ 18, 22 SAY "Fax     :" GET cFax   PICTURE "@K"  WHEN lMessage( "Please enter your fax number" )
      @  8, 22 SAY "Email   :" GET cEmail PICTURE "@K"  WHEN lMessage( "Please enter your email address" )
      READ

      lMessage( "" )

      IF lyesno( "Done?" )
         EXIT
      ENDIF
   ENDDO

   zrevwindow()

   RETURN

/* the following is adapted from wvtgui.prg by Pritpal Bedi
   for illustration purposes only */

STATIC PROCEDURE xBrowse1()

   LOCAL nKey, oBrowse, i
   LOCAL lEnd    := .F.
   LOCAL nTop    :=  6
   LOCAL nLeft   :=  3
   LOCAL nBottom := MaxRow() - 2
   LOCAL nRight  := MaxCol() - 3
   LOCAL nCursor := SetCursor( SC_NONE )
   LOCAL nWin

   LOCAL aColumnsSep, tmp

   IF ! hbtest_Table()
      RETURN
   ENDIF

   SetColor( "N/W*,N/GR*,,,N/W* " )
   oBrowse := TBrowseNew( nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 )

   oBrowse:ColSep        := "  "     // we'll draw a line between these spaces
   oBrowse:HeadSep       := "__"
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip ) }

   FOR EACH i IN dbStruct()
      oBrowse:AddColumn( TBColumnNew( i[ DBS_NAME ], FieldBlock( i[ DBS_NAME ] ) ) )
   NEXT

   oBrowse:configure()

   nWin := znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTop, nLeft, nBottom, nRight, "Test table" )

   wvw_SetPen( 0, 0, WIN_RGB( 210, 210, 210 ) )

   aColumnsSep := Array( oBrowse:colCount )
   FOR EACH tmp IN aColumnsSep
      tmp := oBrowse:getColumn( tmp:__enumIndex() ):colSep
   NEXT

   AddMiscObjects( nWin, {| nWindow | wvw_DrawBoxRecessed( nWindow, nTop + 1, nLeft + 1, nBottom - 1, nRight - 1 ) } )
   AddMiscObjects( nWin, {| nWindow | wvw_DrawGridHorz( nWindow, oBrowse:nTop + 3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   AddMiscObjects( nWin, {| nWindow | wvw_DrawGridVert( nWindow, oBrowse:nTop, oBrowse:nBottom, aColumnsSep, Len( aColumnsSep ) ) } )

   DO WHILE ! lEnd
      oBrowse:ForceStable()

      lMessage( "Record #" + hb_ntos( RecNo() ) )

      nKey := Inkey( 0 )

      DO CASE
      CASE nKey == K_ESC .OR. nKey == K_ENTER
         lEnd := lYesNo( "Done?" )
      CASE nKey == K_DOWN       ; oBrowse:Down()
      CASE nKey == K_UP         ; oBrowse:Up()
      CASE nKey == K_LEFT       ; oBrowse:Left()
      CASE nKey == K_RIGHT      ; oBrowse:Right()
      CASE nKey == K_PGDN       ; oBrowse:pageDown()
      CASE nKey == K_PGUP       ; oBrowse:pageUp()
      CASE nKey == K_CTRL_PGUP  ; oBrowse:goTop()
      CASE nKey == K_CTRL_PGDN  ; oBrowse:goBottom()
      CASE nKey == K_HOME       ; oBrowse:home()
      CASE nKey == K_END        ; oBrowse:end()
      CASE nKey == K_CTRL_LEFT  ; oBrowse:panLeft()
      CASE nKey == K_CTRL_RIGHT ; oBrowse:panRight()
      CASE nKey == K_CTRL_HOME  ; oBrowse:panHome()
      CASE nKey == K_CTRL_END   ; oBrowse:panEnd()
      ENDCASE
   ENDDO

   lMessage( "" )

   zrevwindow()

   // restore state
   SetCursor( nCursor )

   dbCloseArea()

   RETURN

//

STATIC FUNCTION DbSkipBlock( n )

   LOCAL nSkipped := 0

   IF n == 0
      dbSkip( 0 )
   ELSEIF n > 0
      DO WHILE nSkipped != n .AND. TBNext()
         nSkipped++
      ENDDO
   ELSE
      DO WHILE nSkipped != n .AND. TBPrev()
         nSkipped--
      ENDDO
   ENDIF

   RETURN nSkipped

//

STATIC FUNCTION TBNext()

   LOCAL nSaveRecNum := RecNo()
   LOCAL lMoved := .T.

   IF Eof()
      lMoved := .F.
   ELSE
      dbSkip()
      IF Eof()
         dbGoto( nSaveRecNum )
         lMoved := .F.
      ENDIF
   ENDIF

   RETURN lMoved

//

STATIC FUNCTION TBPrev()

   LOCAL nSaveRecNum := RecNo()
   LOCAL lMoved := .T.

   dbSkip( -1 )
   IF Bof()
      dbGoto( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

// supporting functions

// displays a message on MaxRow() and returns .T.
STATIC FUNCTION lMessage( cMsg )

   // displays a message on status bar of Main Window and returns .T.
   wvw_sbSetText( 0, 0, cMsg )

   RETURN .T.

// display cmsg with Yes/No option, returns .T. if Yes selected
STATIC FUNCTION lYesNo( cMsg )

   LOCAL nTopLine, ;
      nLeft := 5, ;
      nBotLine := MaxRow() - 2, ;
      nRight := MaxCol() - 5
   LOCAL nChoice, nWidth
   LOCAL oldCurs := SetCursor( SC_NONE )
   LOCAL oldColor := SetColor( _STD_COLOR_ )

   cmsg := " " + AllTrim( hb_defaultValue( cMsg, "Please Confirm" ) ) + " "
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

STATIC PROCEDURE lBoxMessage( cMsg, cTitle )

   LOCAL nTopLine, ;
      nLeft := 5, ;
      nBotLine := MaxRow() - 2, ;
      nRight := MaxCol() - 5
   LOCAL nwidth, nmaxwidth, i, nNumLines, cAline
   LOCAL oldCurs := SetCursor( SC_NONE )
   LOCAL oldColor := SetColor( _STD_COLOR_ )

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
   znewwindow( hb_UTF8ToStrBox( "┌─┐│┘─└│" ), nTopLine, nLeft, nBotLine, nRight, hb_defaultValue( cTitle, "Info" ) )
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

   RETURN

// Draw a new window on screen and register it in window list
// wtype       : Window border type, eg. "┌─┐│┘─└│"
// r1,c1,r2,c2 : coordinates
// Return      : Numeric id of the new window
STATIC FUNCTION ZNEWWINDOW( wtype, r1, c1, r2, c2, ctitle, ccolor )

   LOCAL i := Len( s_zwin )
   LOCAL cScreen := SaveScreen( r1, c1, r2, c2 )
   LOCAL cOldColor := SetColor()
   LOCAL nrow := Row(), ncol := Col()

   hb_default( @ctitle, "" )
   hb_default( @ccolor, _STD_COLOR_ )

   SetColor( ccolor )

   wvw_nOpenWindow( ctitle, r1, c1, r2, c2 )
   ResetMiscObjects()   // make sure we start with no GUI objects

   AAdd( s_zwin, { i + 1, r1, c1, r2, c2, cScreen, ctitle, nrow, ncol, coldcolor } )

   SetColor( ccolor )

   hb_Scroll( r1, c1, r2, c2 )

   HB_SYMBOL_UNUSED( wtype )

   SetColor( cOldColor )

   RETURN i + 1

// Closes the last window and remove it from window list
STATIC PROCEDURE ZREVWINDOW()

   LOCAL i := Len( s_zwin )

   IF i == 0
      RETURN  // no window to close
   ENDIF

   ResetMiscObjects()   // clear all GUI objects, if any
   wvw_lCloseWindow()

   // restore states
   RestScreen( s_zwin[ i ][ 2 ], s_zwin[ i ][ 3 ], s_zwin[ i ][ 4 ], s_zwin[ i ][ 5 ], s_zwin[ i ][ 6 ] )
   SetPos( s_zwin[ i ][ 8 ], s_zwin[ i ][ 9 ] )
   SetColor( s_zwin[ i ][ 10 ] )

   // remove window from list
   hb_ADel( s_zwin, i, .T. )

   RETURN

// WVW_Paint() must be a FUNCTION in your application
// as it is called when Window gets WM_PAINT message.
// WARNING: it now receives only nWinNum parameter

FUNCTION WVW_Paint( nWinNum )  /* must be a public function */

   IF nWinNum + 1 <= Len( s_amiscobjlist )
      AEval( s_amiscobjlist[ nWinNum + 1 ], {| e | Eval( e, nWinNum ) } )
   ENDIF

   RETURN 0

STATIC PROCEDURE ResetMiscObjects( nWinNum )

   hb_default( @nWinNum, wvw_nNumWindows() - 1 )

   DO WHILE Len( s_amiscobjlist ) < nWinNum + 1
      AAdd( s_amiscobjlist, {} )
   ENDDO
   s_amiscobjlist[ nWinNum + 1 ] := {}

   RETURN

STATIC PROCEDURE AddMiscObjects( nWinNum, bAction )

   AAdd( s_amiscobjlist[ nWinNum + 1 ], bAction )

   RETURN
