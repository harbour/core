//         Test/Demo Program for
//          GTWVW GUI Interface
//      with multiple window support
//         a work in progress by
//  Budyanto Dj. <budyanto@centrin.net.id>
//               based on:
//
//              wvtgui.prg,
//      GTWVT Console GUI Interface
// by Pritpal Bedi <pritpal@vouchcac.com>
//
// parts of this program are copyrights of their respective owners

#require "gtwvw"
#require "hbtest"

#include "dbstruct.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"
#include "hbver.ch"

#define WVW_MAXWINDOWS    20             // ! must match with hbgtwvw.h
#define WVW_DEFAULT_MENUKEYEVENT  1024   // ! must match with hbgtwvw.h

// for Button Types: // 2004-03-03
#define _BUTTON_NORMAL 0        // normal button
#define _BUTTON_FLAT   1        // 'transparent', raised when mouseover
#define _BUTTON_NONE   2        // no sign even when mouseover or clicked
#define _BUTTON_HARD   3        // no recessed when pressed

// menu actions
#define IDM_DEMO_GET     101
#define IDM_DEMO_BROWSE  102
#define IDM_DEMO_CONSOLE 103
// #define IDM_DEMO_COLOR   104
#define IDM_DEMO_EXIT    199

#define IDM_TOOLBAR_RESET  501
#define IDM_TOOLBAR_DELETE 502
#define IDM_TOOLBAR_ENABLE  503
#define IDM_TOOLBAR_DISABLE  504

#define IDM_WINDOW_SPACING_INCREASE  201
#define IDM_WINDOW_SPACING_DECREASE  202
#define IDM_WINDOW_SPACING_DEFAULT   203

#define IDM_HELP_HELP    301
#define IDM_HELP_INFO    302

// menu action from toolbar only:
#define IDM_NETCONNECT    401
#define IDM_NETDISCONNECT 402
#define IDM_BACK          403
#define IDM_FORWARD       404
#define IDM_COPY          405
#define IDM_PASTE         406


STATIC s_amiscobjlist := {}      // misc object list (actually: list of codeblocks)
STATIC s_afontinfo := {}         // current font info

//

STATIC s_amouseobjlist := {}
STATIC s_ncurkey := 0
STATIC s_nkeyrepeater
STATIC s_nrepeatrate  := 0.1
STATIC s_nrepeatdelay := 0.5

//

PROCEDURE Main()

   LOCAL nCurWindow
   LOCAL hMenu, hPopupmenu, hSubMenu
   LOCAL cLabel := "This is the Main Window"
   LOCAL nCursor
   LOCAL kF1, kF2, kF3
   LOCAL kF9, kF10, kF11
   LOCAL oMouse
   LOCAL nKeyStd

   ErrorBlock( {| e | MyError( e ) } )

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   Set( _SET_EVENTMASK, INKEY_ALL )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_SCOREBOARD, .F. )
   // wvw_SetPaintRefresh( 0 )
   wvw_SetVertCaret( .T. )
   wvw_pbSetFont( , "Tahoma", 14 )
   nCursor := SetCursor( SC_NONE )

   IF SetDefaultWindowSize()
      ldebug( "Successfully setDefaultWindowSize()" )
   ELSE
      ldebug( "Cannot setDefaultWindowSize()" )
   ENDIF

   IF ! Empty( wvw_sbCreate() ) .AND. ;
      wvw_sbAddPart( , "99:99:99" ) > 0
      wvw_SetTimer( , 1000 )
   ENDIF

   s_afontinfo := wvw_GetFontInfo()

   hb_gtInfo( HB_GTI_INKEYFILTER, {| nkey | nAfterInkey( nkey ) } )
   wvw_SetMouseMove( , .T. )                           // required by wvwmouse
   kF1 := SetKey( K_F1, {|| xHelp() } )
   kF2 := SetKey( K_F2, {|| xDebugInfo() } )
   kF3 := SetKey( K_F3, {|| Demo_Console() } )

   kF9 := SetKey( K_F9, {|| wvw_SetLineSpacing( , wvw_SetLineSpacing() - 2 ) } )
   kF10 := SetKey( K_F10, {|| wvw_SetLineSpacing( , wvw_SetLineSpacing() + 2 ) } )
   kF11 := SetKey( K_F11, {|| wvw_SetDefLineSpacing( wvw_SetLineSpacing() ) } )

   // start menu definitions

   hMenu := wvw_CreateMenu()
   hPopupMenu := wvw_CreateMenu()
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_DEMO_GET, "~GET demo"  )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_DEMO_BROWSE, "~BROWSE demo" )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_DEMO_CONSOLE, "~CONSOLE demo (F3)" )
#if 0
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_DEMO_COLOR, "~COLOR demo" )
#endif
   wvw_AppendMenu( hPopupMenu )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_DEMO_EXIT, "E~xit"  )
   wvw_AppendMenu( hMenu, WIN_MF_ENABLED, hPopupMenu, "~Demos",  )

   hSubMenu := wvw_CreateMenu()
   wvw_AppendMenu( hSubMenu, WIN_MF_ENABLED, 900, "Sub~1"  )
   wvw_AppendMenu( hSubMenu, WIN_MF_ENABLED, 901, "Sub~2"  )

   hPopupMenu := wvw_CreateMenu()
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_TOOLBAR_ENABLE,  "~Enable Toolbar" )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_TOOLBAR_DISABLE, "~Disable Toolbar" )
   wvw_AppendMenu( hPopupMenu )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_TOOLBAR_RESET,  "~Reset Toolbar" )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_TOOLBAR_DELETE, "~Delete Toolbar" )
   wvw_AppendMenu( hPopupMenu )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, hSubMenu, "~Submenus" )
   wvw_AppendMenu( hMenu, WIN_MF_ENABLED, hPopupMenu, "~Toolbar",  )

   hPopupMenu := wvw_CreateMenu()
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_WINDOW_SPACING_DECREASE, "~Decrease Line Spacing (F9)" )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_WINDOW_SPACING_INCREASE, "~Increase Line Spacing (F10)" )
   wvw_AppendMenu( hPopupMenu )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_WINDOW_SPACING_DEFAULT,  "~Set As Default Line Spacing (F11)" )
   wvw_AppendMenu( hMenu, WIN_MF_ENABLED, hPopupMenu, "~Window",  )

   hPopupMenu := wvw_CreateMenu()
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_HELP_HELP, "~Help (F1)"  )
   wvw_AppendMenu( hPopupMenu )
   wvw_AppendMenu( hPopupMenu, WIN_MF_ENABLED, IDM_HELP_INFO, "~Info (F2)"  )
   wvw_AppendMenu( hMenu, WIN_MF_ENABLED, hPopupMenu, "~Help",  )

   wvw_SetMenu( , hMenu )

   // end menu definitions

   nCurWindow := wvw_nNumWindows() - 1  // == 0, Main Window

   CreateToolbar( nCurWindow )

   ResetMiscObjects( nCurWindow )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawLabel( nWindow, 1, 40, cLabel, 6,, WIN_RGB( 255, 255, 255 ), WIN_RGB( 198, 198, 198 ), "Arial", s_afontinfo[ 2 ], , , , , .T., .T. ) } )

   wvwm_ResetMouseObjects( nCurWindow )
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New( "Info!", MaxRow() - 2, 67, , , {|| xDebugInfo() } ) )

   oMouse := WVWMouseButton():New( "Flat", MaxRow() - 2, 67 - 11, , , {|| lboxmessage( "flat" ) }, 1 )
   oMouse:cImage := hb_DirBase() + "vouch1.gif"
   oMouse:cCaption := ""
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   oMouse := WVWMouseButton():New( "None", MaxRow() - 2, 67 - 11 - 11, , , {|| lboxmessage( "none" ) }, 2 )
   oMouse:Enable( .T. )
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New( "Hard", MaxRow() - 2, 67 - 11 - 11 - 11, , , {|| lboxmessage( "hard" ) }, 3 ) )
   oMouse := WVWMouseButton():New( "Disabled", MaxRow() - 2, 67 - 11 - 11 - 11 - 11, , , {|| xDebugInfo() } )
   oMouse:Enable( .F. )
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   oMouse := WVWMouseButton():New( "Tight", MaxRow() - 2, 67 - 11 - 11 - 11 - 11 - 11, , , {|| lboxmessage( "tight" ) } )
   oMouse:lTight := .T.
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   // 2007-05-25 the real pushbutton, easier and better looking. Nothing to do with wvwmouse.prg.
   wvw_pbCreate( nCurWindow, MaxRow() - 4, 67 - 11 - 11 - 11 - 11 - 11, MaxRow() - 4, 67 + 9 - 11 - 11 - 11 - 11 - 11, "native", , {|| lboxmessage( "native pushbutton" ) } )

   SetColor( "N/W,N/GR*,,,N/W*" )
   CLS
   @ 0, 0 SAY "This is line 0"
   @ 1, 0 SAY "This is line 1"
   @ MaxRow() - 1, 0 SAY "This is line " + hb_ntos( MaxRow() - 1 )
   @ MaxRow(), 0 SAY "This is line " + hb_ntos( MaxRow() )

   DO WHILE ( nKeyStd := hb_keyStd( Inkey( 0 ) ) ) != K_ESC
      // experiment with different paintrefresh interval:
      DO CASE
      CASE nKeyStd == hb_keyCode( "<" )
         wvw_SetPaintRefresh( Int( wvw_SetPaintRefresh() / 2 ) )
         Alert( wvw_SetPaintRefresh() )
      CASE nKeyStd == hb_keyCode( ">" )
         wvw_SetPaintRefresh( Int( wvw_SetPaintRefresh() * 2 ) )
         Alert( wvw_SetPaintRefresh() )
      CASE nKeyStd == hb_keyCode( "0" )
         wvw_SetPaintRefresh( 0 )
         Alert( wvw_SetPaintRefresh() )
      OTHERWISE
         // do nothing. Inkey() has been handled by nAfterInket()
      ENDCASE
   ENDDO

   lboxmessage( "Thanks for trying this program." + hb_eol() + "Good bye!" )

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetKey( K_F11, kF11 )
   SetKey( K_F10, kF10 )
   SetKey( K_F9, kF9 )

   // SetKey( K_F4, kF4 )
   SetKey( K_F3, kF3 )
   SetKey( K_F2, kF2 )
   SetKey( K_F1, kF1 )
   SetCursor( nCursor )

   RETURN

// disables all Menu Items of window nWinNum
STATIC PROCEDURE xDisableMenus( nWinNum, nNumItem )

   LOCAL i
   LOCAL hMenu := wvw_GetMenu( nWinNum )

   FOR i := 0 TO nNumItem - 1
      wvw_EnableMenuItem( hMenu, i, WIN_MF_BYPOSITION + WIN_MF_GRAYED )
   NEXT

   RETURN

// enables all Menu Items of window nWinNum
STATIC PROCEDURE xEnableMenus( nWinNum, nNumItem )

   LOCAL i
   LOCAL hMenu := wvw_GetMenu( nWinNum )

   FOR i := 0 TO nNumItem - 1
      wvw_EnableMenuItem( hMenu, i, WIN_MF_BYPOSITION + WIN_MF_ENABLED )
   NEXT
   wvw_DrawMenuBar( nWinNum )  // to force redraw of menu

   RETURN

//

STATIC PROCEDURE Demo_Console( nTop, nLeft, nBottom, nRight )

   LOCAL cWinName, nCurWindow
   LOCAL nCursor
   LOCAL cColor
   LOCAL nKeyStd
   LOCAL lMouseMove
   LOCAL lEchoing := .F.

   hb_default( @nTop, 2 )
   hb_default( @nLeft, 2 )
   hb_default( @nBottom, nTop + 10 )
   hb_default( @nRight, nLeft + 45 )

   cWinName := "Typewriter (Win#" + hb_ntos( wvw_nNumWindows() ) + "); <Ctrl+W>: New Window; <Esc>: Exit"

   IF ( nCurWindow := wvw_nOpenWindow( cWinName, nTop, nLeft, nBottom, nRight ) ) == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN
   ENDIF

   nCursor := SetCursor( SC_NORMAL )
   cColor := SetColor( "W+/N" )
   lMouseMove := wvw_SetMouseMove( , .F. )

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   // --- begin typewriter mode ---
   CLS
   ?? "Press <Ctrl+E> to toggle between echoing what you type to previous window"
   ?
   CLEAR TYPEAHEAD
   DO WHILE ( nKeyStd := hb_keyStd( Inkey( 0 ) ) ) != K_ESC
      IF nKeyStd == K_ENTER
         ?? hb_keyChar( nKeyStd ) + Chr( 10 )
         IF lEchoing
            // write the same thing to previous window
            wvw_nSetCurWindow( nCurWindow - 1 )
            ?? hb_keyChar( nKeyStd ) + Chr( 10 )
            wvw_nSetCurWindow( nCurWindow )
         ENDIF
      ELSEIF nKeyStd == K_CTRL_W
         // Recursively call (another) typewriter, bigger one
         Demo_Console( nTop + 2, nLeft + 2, nBottom + 4, nRight + 6 )
      ELSEIF nKeyStd == K_CTRL_E
         // toggle echoing output to prev window
         lEchoing := ! lEchoing
      ELSE
         // any other char goes here
         ?? hb_keyChar( nKeyStd )
         IF lEchoing
            // write the same thing to previous window
            wvw_nSetCurWindow( nCurWindow - 1 )
            ?? hb_keyChar( nKeyStd )
            wvw_nSetCurWindow( nCurWindow )
         ENDIF
      ENDIF
   ENDDO

   // --- end typewriter mode ---

   // epilogue
   wvw_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetCursor( nCursor )
   SetColor( cColor )
   wvw_SetMouseMove( , lMouseMove )

   RETURN

//

STATIC PROCEDURE Demo_Get()

   LOCAL nCurWindow, GetList := {}

   LOCAL cLabel  := "This is the GET Demo Window"
   LOCAL nTop    := 4
   LOCAL nLeft   := 4
   LOCAL nBottom := 21
   LOCAL nRight  := 75
   LOCAL nColGet := 8
   LOCAL get_1   := hb_SToD()
   LOCAL get_2   := PadR( "Pritpal Bedi", 35 )
   LOCAL get_3   := PadR( "60, New Professor Colony", 35 )
   LOCAL get_4   := PadR( "Ludhiana, INDIA", 35 )
   LOCAL get_5   := PadR( hb_Version( HB_VERSION_URL_BASE ), 35 )
   LOCAL get_6   := 20000
   LOCAL nCursor := SetCursor( SC_NORMAL )

   MEMVAR __temp__

   IF ( nCurWindow := wvw_nOpenWindow( "GET Demo", nTop, nLeft, nBottom, nRight ) ) == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN
   ENDIF

   wvw_SetIcon( , hb_DirBase() + "vr_1.ico" )

   ResetMiscObjects( nCurWindow )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawLabel( nWindow, 1, nRight - nLeft, cLabel, 2,, WIN_RGB( 255, 255, 255 ), WIN_RGB( 198, 198, 198 ), "Arial", s_afontinfo[ 2 ], , , , , .T., .T. ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawBoxRecessed( nWindow, 7 - nTop, 61 - nLeft, 13 - nTop, 70 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawBoxGroup( nWindow, 15 - nTop, 59 - nLeft, 18 - nTop, 72 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawBoxGroup( nWindow, 5 - nTop, 6 - nLeft, 19 - nTop, 44 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawImage( nWindow, 8 - nTop, 62 - nLeft, 12 - nTop, 69 - nLeft, hb_DirBase() + "vouch1.bmp" ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawBoxRecessed( nWindow, 7 - nTop, 48 - nLeft, 13 - nTop, 55 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | __temp__ := nWindow, AEval( GetList, {| oGet | wvw_DrawBoxGet( __temp__, oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   wvwm_ResetMouseObjects( nCurWindow )

#if 0
   /* we now use native push button */
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New( "Info", MaxRow() - 1, MaxCol() - 15, , , {|| xDebugInfo() }  ))
#endif

   wvw_pbCreate( nCurWindow, MaxRow() - 1, MaxCol() - 15, MaxRow() - 1, MaxCol() - 5, "Info", , {|| xDebugInfo() } )

   CLS

   @  6 - nTop, nColGet - nLeft SAY "< Date >"
   @  9 - nTop, nColGet - nLeft SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12 - nTop, nColGet - nLeft SAY "<" + PadC( "Address", 33 ) + ">"
   @ 16 - nTop, 61 - nLeft      SAY "< Salary >"

   @  7 - nTop, nColGet - nLeft GET get_1
   @ 10 - nTop, nColGet - nLeft GET get_2 // VALID VouChoice() < 7
   @ 13 - nTop, nColGet - nLeft GET get_3
   @ 15 - nTop, nColGet - nLeft GET get_4
   @ 17 - nTop, nColGet - nLeft GET get_5
   @ 17 - nTop, 61 - nLeft      GET get_6 PICTURE "@Z 9999999.99"

   READ

   // epilogue
   // lboxmessage( "Thanks for trying the GET Demo!" )
   wvw_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetCursor( nCursor )

   RETURN

//

STATIC PROCEDURE DEMO_Browse()

   LOCAL nKey, oBrowse, i
   LOCAL lEnd    := .F.
   LOCAL nTop    := 3             // pls notice that this is relative to PARENT window!
   LOCAL nLeft   := 3             // pls notice that this is relative to PARENT window!
   LOCAL nBottom := MaxRow() - 1  // pls notice that this is relative to PARENT window!
   LOCAL nRight  := MaxCol() - 3  // pls notice that this is relative to PARENT window!
   LOCAL cColor

   LOCAL nCurWindow

   LOCAL nHScrollBar, nVScrollBar

   LOCAL aColumnsSep, tmp

   IF ( nCurWindow := wvw_nOpenWindow( "BROWSE Demo", nTop, nLeft, nBottom, nRight ) ) == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN
   ENDIF

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   cColor := SetColor( "N/W" )
   CLS
   SetColor( "N/W*,N/GR*,,,N/W*" )

   IF ! hbtest_Table()
      wvw_lCloseWindow()
      RETURN
   ENDIF

   INDEX ON FIELD->LAST TO test1.ntx  // 2004-07-07

   oBrowse := TBrowseNew( 3, 2, MaxRow() - 4, MaxCol() - 3 )

   oBrowse:ColSep        := "  "
   oBrowse:HeadSep       := "__"
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip ) }

   FOR EACH i IN dbStruct()
      oBrowse:AddColumn( TBColumnNew( i[ DBS_NAME ], FieldBlock( i[ DBS_NAME ] ) ) )
   NEXT

   oBrowse:configure()

   wvw_SetPen( WIN_PS_SOLID, 0, WIN_RGB( 210, 210, 210 ) )
   wvw_SetIcon( , hb_DirBase() + "dia_excl.ico" )

   aColumnsSep := Array( oBrowse:colCount )
   FOR EACH tmp IN aColumnsSep
      tmp := oBrowse:getColumn( tmp:__enumIndex() ):colSep
   NEXT

   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawBoxRecessed( nWindow, oBrowse:nTop, oBrowse:nLeft, oBrowse:nBottom, oBrowse:nRight ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawGridHorz( nWindow, oBrowse:nTop + 3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   AddMiscObjects( nCurWindow, {| nWindow | wvw_DrawGridVert( nWindow, oBrowse:nTop, oBrowse:nBottom, aColumnsSep, Len( aColumnsSep ) ) } )

#if 0
   /* we now use native push button */
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info", MaxRow(), MaxCol() - 15, , , {|| xDebugInfo() } ))
#endif
   wvw_pbCreate( nCurWindow, MaxRow() - 1, MaxCol() - 15, MaxRow() - 1, MaxCol() - 5, "Info", , {|| xDebugInfo() } )

   nHScrollBar := wvw_xbCreate( nCurWindow, 0, oBrowse:nBottom + 1, oBrowse:nLeft, oBrowse:nRight - oBrowse:nLeft + 1, /*aBlock*/ {| nWinNum, nXBid, nXBmsg, nXBpos | HB_SYMBOL_UNUSED( nXBpos ), HXBscroller( oBrowse, nWinNum, nXBid, nXBmsg ) }, /*aOffset*/ )
   nVScrollBar := wvw_xbCreate( nCurWindow, 1, oBrowse:nTop, oBrowse:nRight + 1, oBrowse:nBottom - oBrowse:nTop + 1, /*aBlock*/ {| nWinNum, nXBid, nXBmsg, nXBpos | HB_SYMBOL_UNUSED( nXBpos ), VXBscroller( oBrowse, nWinNum, nXBid, nXBmsg ) }, /*aOffset*/ )

   hb_DispOutAt( nTop + 1 - nTop, nleft - nleft, PadC( "Test table", nRight - nLeft + 1 ), "W+/W" )

   oBrowse:ForceStable()
   RefreshHXB( oBrowse, nCurWindow, nHScrollBar )  // 2004-07-04
   RefreshVXB( oBrowse, nCurWindow, nVScrollBar )  // 2004-07-04

   DO WHILE ! lEnd
      nKey := hb_keyStd( Inkey( 0 ) )

      DO CASE
      CASE nKey == K_ESC .OR. nKey == K_ENTER
         lEnd := .T.
         LOOP

      CASE nKey == K_DOWN
         oBrowse:Down()

      CASE nKey == K_MWBACKWARD
         oBrowse:Down()  // simple scroll down

      CASE nKey == K_UP
         oBrowse:Up()

      CASE nKey == K_MWFORWARD
         oBrowse:Up()  // simple scroll up

      CASE nKey == K_LEFT
         IF oBrowse:colPos == 1
            LOOP
         ENDIF
         oBrowse:Left()

      CASE nKey == K_RIGHT
         IF oBrowse:colPos == oBrowse:colCount
            LOOP
         ENDIF
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

      OTHERWISE
         // other keys, including mouse events do not need ForceStable
         LOOP

      ENDCASE

      oBrowse:ForceStable()

      // refresh the scrollbars due to keyboard navigation
      RefreshHXB( oBrowse, nCurWindow, nHScrollBar )  // 2004-07-04
      RefreshVXB( oBrowse, nCurWindow, nVScrollBar )  // 2004-07-04

   ENDDO

   dbCloseArea()

   // epilogue
   // lboxmessage("Thanks for trying the BROWSE Demo!")
   wvw_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )

   wvw_SetPen( 0 )

   SetColor( cColor )

   RETURN

/* generic Vertical Scrollbar handler for tbrowse */
STATIC PROCEDURE VXBscroller( oBrowse, nWinNum, XBid, XBmsg )

   LOCAL nOldWin
   LOCAL lNeedStabilize

#if 0
   // if we can't handle non topmost window we must return right away
   IF nWinNum != wvw_nNumWindows() - 1
      RETURN
   ENDIF
#endif

   nOldWin := wvw_nSetCurWindow( nWinNum )

   lNeedStabilize := .F.
   DO WHILE .T.  // dummy loop
      DO CASE
      CASE XBmsg == 0  // SB_LINEUP
         IF ordKeyNo() == 1
            EXIT
         ENDIF
         oBrowse:up()
      CASE XBmsg == 1  // SB_LINEDOWN
         IF ordKeyNo() == ordKeyCount()
            EXIT
         ENDIF
         oBrowse:down()
      CASE XBmsg == 2  // SB_PAGEUP
         IF ordKeyNo() == 1
            EXIT
         ENDIF
         oBrowse:pageup()
      CASE XBmsg == 3  // SB_PAGEDOWN
         IF ordKeyNo() == ordKeyCount()
            EXIT
         ENDIF
         oBrowse:pagedown()
      OTHERWISE
         // ignore
         lNeedStabilize := .F.
         EXIT
      ENDCASE
      lNeedStabilize := .T.
      EXIT
   ENDDO

   IF lNeedStabilize
      oBrowse:ForceStable()
      RefreshVXB( oBrowse, nWinNum, XBid )
   ENDIF

   wvw_nSetCurWindow( nOldWin )

   RETURN

/* generic Horizontal Scrollbar handler for tbrowse */
STATIC PROCEDURE HXBscroller( oBrowse, nWinNum, XBid, XBmsg )

   LOCAL nOldWin
   LOCAL lNeedStabilize

#if 0
   // if we can't handle non topmost window we must return right away
   IF nWinNum != wvw_nNumWindows() - 1
      RETURN
   ENDIF
#endif

   nOldWin := wvw_nSetCurWindow( nWinNum )

   lNeedStabilize := .F.
   DO WHILE .T.  // dummy loop
      DO CASE
      CASE XBmsg == 0  // SB_LINELEFT
         IF oBrowse:colPos == 1
            EXIT
         ENDIF
         oBrowse:Left()
      CASE XBmsg == 1  // SB_LINERIGHT
         IF oBrowse:colpos == oBrowse:colCount
            EXIT
         ENDIF
         oBrowse:Right()
      CASE XBmsg == 2  // SB_PAGELEFT
         IF oBrowse:colPos == 1
            EXIT
         ENDIF
         oBrowse:panleft()
      CASE XBmsg == 3  // SB_PAGERIGHT
         IF oBrowse:colpos == oBrowse:colCount
            EXIT
         ENDIF
         oBrowse:panright()
      OTHERWISE
         // ignore
         lNeedStabilize := .F.
         EXIT
      ENDCASE
      lNeedStabilize := .T.
      EXIT
   ENDDO
   IF lNeedStabilize
      oBrowse:ForceStable()
      RefreshHXB( oBrowse, nWinNum, XBid )
   ENDIF

   wvw_nSetCurWindow( nOldWin )

   RETURN

/* 2004-07-04 notes:

   0 <= nPage <= ( nMax - nMin + 1 )
   nPage :: pagesize

   nMin <= nPos <= ( nMax - Max( nPage - 1, 0 ) )
 */
STATIC PROCEDURE RefreshVXB( oBrowse, nWinNum, XBid )

   LOCAL nMin, nMax, nPage, nPos
   LOCAL nRatio

   // recalc the pos
   IF ordKeyCount() < 30000
      nMin := 1
      nMax := ordKeyCount()
      nPage := oBrowse:RowCount       // ordKeyCount()
      nPos := ordKeyNo() - oBrowse:RowPos + 1  // ordKeyCount()
   ELSE
      nRatio := ordKeyCount() / 10
      DO WHILE nRatio > 30000
         nRatio := nRatio / 10
      ENDDO

      nMin := 1
      nMax := Round( ordKeyCount() / nRatio, 0 )
      nPage := Round( oBrowse:RowCount / nRatio, 0 )       // ordKeyCount()
      nPos := Round( ( ordKeyNo() - oBrowse:RowPos + 1 ) / nRatio, 0 )// ordKeyCount()
   ENDIF

   wvw_xbUpdate( nWinNum, XBid, nPos, nPage, nMin, nMax )

   RETURN

// recalc the pos
STATIC PROCEDURE RefreshHXB( oBrowse, nWinNum, XBid )

   LOCAL nMin := 1
   LOCAL nMax := oBrowse:ColCount
   LOCAL nPage := oBrowse:RightVisible - oBrowse:LeftVisible + 1
   LOCAL nPos := iif( oBrowse:RightVisible == oBrowse:ColCount, nMax, oBrowse:LeftVisible )

   wvw_xbUpdate( nWinNum, XBid, nPos, nPage, nMin, nMax )

   RETURN

STATIC FUNCTION DbSkipBlock( n )

   LOCAL nSkipped := 0

   DO CASE
   CASE n == 0
      dbSkip( 0 )

   CASE n > 0
      DO WHILE nSkipped != n .AND. TBNext()
         nSkipped++
      ENDDO
   OTHERWISE
      DO WHILE nSkipped != n .AND. TBPrev()
         nSkipped--
      ENDDO
   ENDCASE

   RETURN nSkipped

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

STATIC FUNCTION TBPrev()

   LOCAL nSaveRecNum := RecNo()
   LOCAL lMoved := .T.

   dbSkip( -1 )

   IF Bof()
      dbGoto( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

// WVW_Paint() must be a FUNCTION in your application
// as it is called when Window gets WM_PAINT message.
//
// 2004-03-30, was: FUNCTION WVW_Paint( hWnd, msg, wParam, lParam, nWinNum )
// 2004-04-08, was: FUNCTION WVW_Paint( nWinNum, nrow1, ncol1, nrow2, ncol2 )

FUNCTION WVW_Paint( nWinNum )  /* must be a public function */

#if 0
   ldebug( ;
      "WVW_Paint:" + hb_eol() + ;
      "hWnd: " + hb_ntos( hWnd ) + hb_eol() + ;
      "nWinNum: " + hb_ntos( nWinNum ) )
#endif

   IF Len( s_amiscobjlist ) >= nWinNum + 1
      AEval( s_amiscobjlist[ nWinNum + 1 ], {| e | Eval( e, nWinNum ) } )
   ENDIF

   wvwm_paint( nWinNum )

   RETURN 0

// WVW_SetFocus() must be a FUNCTION in your application
// needs to process messages sent through WM_SETFOCUS message
// received by the window.
//
#if 0
PROCEDURE WVW_SetFocus( hWnd, nWinNum )  /* must be a public function */

   STATIC s_nGotFocus := 0

   IF nWinNum == 0
      RETURN
   ENDIF
   s_nGotFocus++
   @ 0, 0 SAY s_nGotFocus
   IF s_nGotFocus % 3 == 0
      Alert( "Got focus " + hb_ntos( s_nGotFocus ) + "th times" )
   ENDIF

   RETURN

// WVW_KillFocus() must be a FUNCTION in your application
// needs to process messages sent through WM_KILLFOCUS message
// received by the window.

PROCEDURE WVW_KillFocus( hWnd )  /* must be a public function */
   RETURN
#endif

PROCEDURE WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )  /* must be a public function */

   HB_SYMBOL_UNUSED( nWinNum )
   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( message )
   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )

   // this function is called every certain interval, by GTWVW gtwndproc
   wvw_sbSetText( 0, 1, Time() )

   RETURN

STATIC PROCEDURE CreateToolbar( nWinNum )

   // for toolbar:
   LOCAL nSysBitmap := 1     // 0:none 1:small 2:large
   LOCAL lDisplayText := .F. // text will be displayed as tooltip instead

   wvw_tbDestroy( nWinNum )

   IF ! lYesNo( "would you like to use default toolbar setting?" )
      nSysBitmap := Alert( "Select toolbar button size", { "Small", "Big" } )
      nSysBitmap := iif( nSysBitmap == 0, 1, nSysBitmap )
      lDisplayText := Alert( "Display text in toolbar?", { "Yes", "No" } ) == 1
   ENDIF

   IF Empty( wvw_tbCreate( nWinNum, lDisplayText, , nSysBitmap ) )
      lboxmessage( "FAILED create toolbar" )
      RETURN
   ENDIF

   /* system bitmaps use constants in commctrl.h */

   /* using system view bitmaps */
   wvw_tbAddButton( nWinNum, IDM_NETCONNECT,    9  /*VIEW_NETCONNECT*/, "Connect", 2 )
   wvw_tbAddButton( nWinNum, IDM_NETDISCONNECT, 10 /*VIEW_NETDISCONNECT*/, "Dis", 2 )
   wvw_tbAddButton( nWinNum )  // separator

   /* using system history bitmaps */
   wvw_tbAddButton( nWinNum, IDM_BACK,          0 /*HIST_BACK*/,   "Back", 3 )
   wvw_tbAddButton( nWinNum, IDM_FORWARD,       1 /*HIST_FORWARD*/, "Forward", 3 )
   wvw_tbAddButton( nWinNum )  // separator

   /* using custom bitmaps */
   wvw_tbAddButton( nWinNum, IDM_DEMO_BROWSE, hb_DirBase() + "def2.bmp", "Browse", 0 )
   wvw_tbAddButton( nWinNum, IDM_DEMO_GET,    hb_DirBase() + "vouch1.bmp",  "Get", 0 )
   wvw_tbAddButton( nWinNum )  // separator

   /* using system standard bitmaps */
   wvw_tbAddButton( nWinNum, IDM_COPY,    1 /*STD_COPY*/, "Copy", 1 )
   wvw_tbAddButton( nWinNum, IDM_PASTE,   2 /*STD_PASTE*/, "Paste",  1 )
   wvw_tbAddButton( nWinNum )  // separator

   wvw_tbAddButton( nWinNum, IDM_HELP_INFO, 10 /*STD_PROPERTIES*/, "Info", 1 )
   wvw_tbAddButton( nWinNum, IDM_HELP_HELP, 11 /*STD_HELP*/, "Help", 1 )

   RETURN

STATIC PROCEDURE xDisableToolbar( nWinNum )

   LOCAL i

   FOR i := 0 TO wvw_tbButtonCount( nWinNum ) - 1
      wvw_tbEnableButton( nWinNum, i, .F. )
   NEXT

   RETURN

STATIC PROCEDURE xEnableToolbar( nWinNum )

   LOCAL i

   FOR i := 0 TO wvw_tbButtonCount( nWinNum ) - 1
      wvw_tbEnableButton( nWinNum, i, .T. )
   NEXT

   RETURN

//

STATIC PROCEDURE ResetMiscObjects( nWinNum )

   DO WHILE Len( s_amiscobjlist ) < nWinNum + 1
      AAdd( s_amiscobjlist, {} )
   ENDDO
   s_amiscobjlist[ nWinNum + 1 ] := {}

   RETURN

STATIC PROCEDURE AddMiscObjects( nWinNum, bAction )

   AAdd( s_amiscobjlist[ nWinNum + 1 ], bAction )

   RETURN

// Inkey() handler

/* this is for use with SetInkeyAfterBlock() */

STATIC FUNCTION nAfterInkey( nKey )

   // check if nkey is:
   // (1) menu command, or
   // (2) mouse button action
   LOCAL bAction
   LOCAL nKeyStd := hb_keyStd( nKey )

   IF nKey == WVW_DEFAULT_MENUKEYEVENT
      // MenuKeyEvent
      RETURN nMenuChecker( wvw_GetLastMenuEvent() )
      // was: ELSEIF AScan( { K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE }, nKeyStd ) > 0
   ELSEIF AScan( { K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE, K_MMLEFTDOWN, K_LDBLCLK }, nKeyStd ) > 0
      // MouseEvent
      RETURN wvwm_nMouseChecker( nkey )
   ELSEIF ( bAction := SetKey( nKey ) ) != NIL .OR. ;
          ( bAction := SetKey( nKeyStd ) ) != NIL
      Eval( bAction, ProcName(), ProcLine(), ReadVar() )
      RETURN 0
   ENDIF

   RETURN nKey

// MENU handler

STATIC FUNCTION nMenuChecker( nMenuEvent )

   LOCAL nkey := 0

   xDisableMenus( 0, 4 )
   // xDisableToolbar( 0 )

   SWITCH nMenuEvent
   CASE IDM_DEMO_GET
      // lboxmessage( "Demo GET" )
      Demo_Get()
      EXIT
   CASE IDM_DEMO_BROWSE
      // lboxmessage( "Demo BROWSE" )
      Demo_Browse()
      EXIT
   CASE IDM_DEMO_CONSOLE
      // lboxmessage( "Demo CONSOLE" )
      Demo_Console()
      EXIT
#if 0
   CASE IDM_DEMO_COLOR
      // lboxmessage( "Demo COLOR" )
      Demo_Color()
      EXIT
#endif
   CASE IDM_DEMO_EXIT
      // lboxmessage( "should EXIT!" )
      nkey := K_ESC
      EXIT

   CASE IDM_WINDOW_SPACING_INCREASE
      wvw_SetLineSpacing( , wvw_SetLineSpacing() + 2 )
      EXIT
   CASE IDM_WINDOW_SPACING_DECREASE
      wvw_SetLineSpacing( , wvw_SetLineSpacing() - 2 )
      EXIT
   CASE IDM_WINDOW_SPACING_DEFAULT
      wvw_SetDefLineSpacing( wvw_SetLineSpacing() )
      EXIT
   CASE IDM_TOOLBAR_ENABLE
      xEnableToolbar( 0 )
      EXIT
   CASE IDM_TOOLBAR_DISABLE
      xDisableToolbar( 0 )
      EXIT
   CASE IDM_TOOLBAR_RESET
      CreateToolbar( 0 )
      EXIT
   CASE IDM_TOOLBAR_DELETE
      wvw_tbDestroy( 0 )
      EXIT
   CASE IDM_HELP_HELP
      xHelp()
      EXIT
   CASE IDM_HELP_INFO
      xDebugInfo()
      EXIT
   OTHERWISE
      lboxmessage( "Sorry, unrecognized menu option: " + hb_ntos( nMenuEvent ) )
   ENDSWITCH

   // xEnableToolbar( 0 )
   xEnableMenus( 0, 4 )

   RETURN nkey

// Misc

STATIC FUNCTION lBoxMessage( cMsg, cTitle )

   wapi_MessageBox( wvw_GetWindowHandle(), cMsg, hb_defaultValue( cTitle, "Info" ), WIN_MB_OK + WIN_MB_ICONINFORMATION + WIN_MB_SYSTEMMODAL )

   RETURN .T.

STATIC FUNCTION lYesNo( cMsg, cTitle )
   RETURN wapi_MessageBox( wvw_GetWindowHandle(), cMsg, hb_defaultValue( cTitle, "Confirmation" ), WIN_MB_YESNO + WIN_MB_ICONQUESTION + WIN_MB_SYSTEMMODAL ) == WIN_IDYES

STATIC FUNCTION lDebug( cMsg )
   RETURN lBoxMessage( cMsg, "Debug" )

STATIC PROCEDURE xDebugInfo()

   MSetPos( MaxRow(), MaxCol() )

#if 0
   SetMouse( .T., MaxRow(), MaxCol() )

   wvw_SetMousePos( wvw_nNumWindows() - 1, MaxRow(), MaxCol() )
#endif

   lboxmessage( "GTWVW test/demo" + hb_eol() + ;
      "Budyanto Dj. <budyanto@centrin.net.id>" + hb_eol() + ;
      hb_eol() + ;
      "Topmost Window is Window #" + hb_ntos( wvw_nNumWindows() - 1 ) + hb_eol() + ;
      "Current Window is Window #" + hb_ntos( wvw_nSetCurWindow() ) + hb_eol() + ;
      "MaxRow(): " + hb_ntos( MaxRow() ) + ", MaxCol(): " + hb_ntos( MaxCol() ) + hb_eol() + ;
      "Row(): " + hb_ntos( Row() ) + ", Col(): " + hb_ntos( Col() ) + hb_eol() + ;
      "WVW_RowOfs(): " + hb_ntos( wvw_nRowOfs() ) + ", WVW_ColOfs(): " + hb_ntos( wvw_nColOfs() ) + hb_eol() + ;
      "Line Spacing: " + hb_ntos( wvw_SetLineSpacing() ) + hb_eol() + ;
      "Default Line Spacing: " + hb_ntos( wvw_SetDefLineSpacing() ) + hb_eol() + ;
      hb_eol() + ;
      "Font Face: '" + s_aFontInfo[ 1 ] + "'" + hb_eol() + ;
      "Font Height: " + hb_ntos( s_aFontInfo[ 2 ] ) + hb_eol() + ;
      "Font Width: " + hb_ntos( s_aFontInfo[ 3 ] ) + hb_eol() + ;
      hb_eol() + ;
      "BTW, mouse pointer now sits on MaxRow(),MaxCol(), doesn't it?" )

   RETURN

STATIC PROCEDURE xHelp()

   lboxmessage( "GTWVW test/demo" + hb_eol() + ;
      "Budyanto Dj. <budyanto@centrin.net.id>" + hb_eol() + ;
      hb_eol() + ;
      "Hotkeys (available in any window):" + hb_eol() + ;
      "F1 : Help" + hb_eol() + ;
      "F2 : Info on current window" + hb_eol() + ;
      "F3 : Open a new window of a pseudo-console typewriter" + hb_eol() + ;
      "F9 : Decrease Line Spacing" + hb_eol() + ;
      "F10: Increase Line Spacing" + hb_eol() + ;
      "F11: Set as Default Line Spacing" + hb_eol() + ;
      hb_eol() + ;
      "Test/demo available:" + hb_eol() + ;
      "GET: a simple GET/READ session" + hb_eol() + ;
      "BROWSE: a simple TBROWSE session" + hb_eol() + ;
      hb_eol() + ;
      "CONSOLE: a simple CONSOLE session" + hb_eol() + ;
      "You are interacting in a typewriter mode." + hb_eol() + ;
      "Press <Ctrl+W> to open a new, bigger window." + hb_eol() + ;
      "Press <Esc> to exit)" + hb_eol() + ;
      hb_eol() + ;
      "Maximum number of windows opened: " + hb_ntos( WVW_MAXWINDOWS ) + hb_eol() + ;
      hb_eol() + ;
      "Other info:" + hb_eol() + ;
      "Window repainting is checked at 100msec interval" )

   RETURN

/* Modified from SetDefaultWindowSize() sample from Peter Rees */
/* Note: width < 0 appears better, but mouse caption will look bad */

STATIC FUNCTION SetDefaultWindowSize()

   LOCAL Result, ScreenWidth

   SetMode( 25, 80 )

   screenWidth := wvw_GetScreenWidth()
   DO CASE
   CASE screenWidth >= 1024
      Result := wvw_SetFont( , "Terminal", 20, 10 )
   CASE screenWidth >= 800
      Result := wvw_SetFont( , iif( hb_osIsWinNT(), "Lucida Console", "System" ), 16, -8 )
   OTHERWISE
      Result := wvw_SetFont( , "Terminal", 12, 6 )
   ENDCASE

   IF Result
      wvw_SetCodepage( , 255 )  // #define OEM_CHARSET 255 - from wingdi.h
      CLS
   ENDIF

   RETURN Result

// ERROR handler

STATIC PROCEDURE MyError( e )

   LOCAL i := 1

   LOCAL cErr := "Runtime error" + hb_eol() + ;
      hb_eol() + ;
      "Gencode: " + hb_ntos( e:GenCode ) + hb_eol() + ;
      "Desc: " + e:Description +  + hb_eol() + ;
      "Sub-system: " + hb_ntos( e:SubCode ) + hb_eol() + ;
      hb_eol() + ;
      "Call trace:" + hb_eol() + ;
      hb_eol()

   DO WHILE ! Empty( ProcName( ++i ) )
      cErr += RTrim( ProcName( i ) ) + "(" + hb_ntos( ProcLine( i ) ) + ")" + hb_eol()
   ENDDO

   // ? cErr  // Calls quit
   ldebug( cErr )
   hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
   QUIT

   RETURN

/* Pseudo mouse object in GTWVW GUI
   copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   This is a sample of implementation of a pseudo GUI object in GTWVW,
   using GTWVW GUI primitives.

   Example on how to use it is given in wvwtest9.prg.
   eg. WVWMouseObject():New( "Button1", MaxRow() - 1, 10, , , {|| Tone( 660, 3 ) } )

   NOTES:
   This is just a sample. You may not want to see it as a 'model'.
   There are many other ways to handle pseudo GUI objects in GTWVW, just
   use your imagination :-) */

#include "hbclass.ch"

#define _DEFAULT_CAPTION_FONT    "Tahoma"
#define _DEFAULT_CAPTION_HEIGHT  16

// Cl*pper color constants
#define _IDX_BLACK               0
#define _IDX_BLUE                1
#define _IDX_GREEN               2
#define _IDX_CYAN                3
#define _IDX_RED                 4
#define _IDX_MAGENTA             5
#define _IDX_BROWN               6
#define _IDX_WHITE               7
#define _IDX_LIGHT_GRAY          8
#define _IDX_BRIGHT_BLUE         9
#define _IDX_BRIGHT_GREEN        10
#define _IDX_BRIGHT_CYAN         11
#define _IDX_BRIGHT_RED          12
#define _IDX_BRIGHT_MAGENTA      13
#define _IDX_YELLOW              14
#define _IDX_BRIGHT_WHITE        15

// mouse object types
#define _MOBJECT_BUTTON  0      // mouse button
#define _MOBJECT_HSCROLL 1      // horiz scrollbar: OBSOLETE, NOT USED HERE
#define _MOBJECT_VSCROLL 2      // horiz scrollbar: OBSOLETE, NOT USED HERE

// WVWMouseButton

CREATE CLASS WVWMouseButton STATIC

#if 0
   VAR nId         /* TODO */       // mouse object id
   VAR nHotKey     /* TODO */       // hotkey associated with this object
#endif
   VAR nWinId                       // 2004-03-03, parent window's number
   VAR lVisible                     // is the object visible
   VAR lEnable                      // 2004-03-03, is the object enable
   VAR lTight                       // allow tight neighboring
   VAR nType                        // 2004-03-03, appearance of this button
   VAR nRow1, nCol1, nRow2, nCol2   // mouse object region

   VAR bClickBlock                  // executed on Left Click
   VAR bPressBlock                  // executed on Left Press

   VAR lRepeatPress                 // repeat Left Press when pressed during mouse over?

   VAR cCaption
   VAR cCaptionFont                 // font name for caption
   VAR nCaptionHeight               // height of font for caption, if NIL use current wvw_GetFontInfo()
   VAR cImage                       // 2004-03-25, image file name

   VAR cNormalColor    // button normal color, pls use single color, eg "W"
   VAR cPressedColor   // button pressed color, pls use single color, eg "B"

   // private DATA, should be protected
   VAR lPressed                     // is it being pressed by Left Button?
   VAR lHover                       // 2004-03-03, is mouse over the button?

   // METHODS
   METHOD New( cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId )

#if 0
   METHOD nGetId() INLINE ::nId            /* TODO */
   METHOD SetHotKey( nKey )                /* TODO */
   METHOD nGetHotKey() INLINE ::nHotKey    /* TODO */
#endif

   METHOD Enable( lEnable )

   METHOD SetClickBlock( bBlock )
   METHOD GetClickBlock() INLINE ::bClickBlock

   METHOD SetPressBlock( bBlock )
   METHOD GetPressBlock() INLINE ::bPressBlock

   METHOD SetRepeatPress( lRepeat )
   METHOD GetRepeatPress() INLINE ::lRepeatPress

   METHOD SetCaption( cCaption )

   METHOD OnClick()
   METHOD OnPress()          // K_LBUTTONDOWN occurs over this mouse object
   METHOD OnRelease()        // K_LBUTTONUP occurs over this mouse object
   METHOD OnReleaseOut()     // K_LBUTTONUP occurs outside of this mouse object
   METHOD OnMouseOut()       // mouse is moved from over the button outside
   METHOD OnMouseOver()    // TODO

   METHOD Draw( nWinNum )

ENDCLASS

METHOD New( cCaption, nRow1, nCol1, nRow2, nCol2, bClickBlock, nType, lDraw, nWinId ) CLASS WVWMouseButton

   hb_default( @cCaption, "" )  // 2004-03-25, was: "Button"

   hb_default( @nRow1, 0 )
   hb_default( @nCol1, 0 )
   hb_default( @nRow2, nRow1 )
   hb_default( @nCol2, nCol1 + Max( 10, Len( cCaption ) + 2 ) - 1 )

#if 0  // TODO
   ::nId := iif( Empty( s_amouseobjlist ), 1, ATail( s_amouseobjlist ):nGetId() + 1 )
   ::nHotKey := NIL
#endif
   ::nWinId := hb_defaultValue( nWinId, wvw_nNumWindows() - 1 )  // 2004-03-03

   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   ::bClickBlock    := iif( HB_ISEVALITEM( bClickBlock ), bClickBlock, NIL )
   ::bPressBlock    := NIL

   ::lRepeatPress   := .F.
   ::lPressed       := .F.
   ::lHover         := .F.  // 2004-03-03

   ::cCaption       := cCaption
   ::cCaptionFont   := _DEFAULT_CAPTION_FONT
   ::nCaptionHeight := _DEFAULT_CAPTION_HEIGHT

   ::cImage         := NIL  // 2004-03-25

   // TODO: pls use current color
   ::cNormalColor   := "W"
   ::cPressedColor  := "W"

   ::lVisible := .T.
   ::lEnable  := .T.
   ::lTight   := .F.
   ::nType    := hb_defaultValue( nType, _BUTTON_NORMAL )

   IF hb_defaultValue( lDraw, .T. )  // 2004-03-04
      ::Draw( ::nWinId )
   ENDIF

   RETURN Self

METHOD Enable( lEnable ) CLASS WVWMouseButton

   ::lEnable := lEnable
   ::draw()

   RETURN Self

METHOD SetClickBlock( bBlock ) CLASS WVWMouseButton

   ::bClickBlock := bBlock

   RETURN Self

METHOD SetPressBlock( bBlock ) CLASS WVWMouseButton

   ::bPressBlock := bBlock

   RETURN Self

METHOD SetRepeatPress( lRepeat ) CLASS WVWMouseButton

   ::lRepeatPress := lRepeat

   RETURN Self

METHOD SetCaption( cCaption ) CLASS WVWMouseButton

   ::cCaption := cCaption

   RETURN Self

METHOD OnPress() CLASS WVWMouseButton

   // this is called when LEFT mouse button is pressed on the object
   LOCAL lWasPressed
   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   lWasPressed := ::lPressed
   ::lPressed := .T.
   ::Draw()

   IF ::lRepeatPress  // .AND. ::lPressed
      IF ! lWasPressed
         xKeyRepeater( .T. )  // init it
      ENDIF
      wvwm_SetKeyRepeater( .T. )  // activate key repeater
   ENDIF

   IF HB_ISEVALITEM( ::bPressBlock )
      Eval( ::bPressBlock )
   ENDIF

   RETURN Self

METHOD OnClick() CLASS WVWMouseButton

   // this is called when LEFT mouse button is clicked on the object
   // normally (or should it be restricted to be?) called from ::OnRelease()
   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   IF HB_ISEVALITEM( ::bClickBlock )
      Eval( ::bClickBlock )
   ENDIF

   RETURN Self

METHOD OnRelease() CLASS WVWMouseButton

   LOCAL lWasPressed := ::lPressed

   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   ::lPressed := .F.
   ::Draw()

   IF ::lRepeatPress // .AND. ::lPressed
      wvwm_SetKeyRepeater( .F. )   // deactivate key repeater
   ENDIF

   IF lWasPressed
      ::OnClick()
   ENDIF

   RETURN Self

METHOD OnReleaseOut() CLASS WVWMouseButton

   // left button is released outside of mouse region
   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   ::lPressed := .F.
   ::Draw()

   // NOTE: no need to do SetKeyRepeater( .F. ),
   //       because it was already handled by onMouseOut

   RETURN Self

// 2004-03-03

METHOD OnMouseOut() CLASS WVWMouseButton

   // mouse is moved from over the button outside
   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   IF ::lRepeatPress .AND. ::lPressed
      wvwm_SetKeyRepeater( .F. )  // stop key repeater
   ENDIF

   ::lHover := .F.
   ::Draw()

   RETURN Self

// 2004-03-03

METHOD OnMouseOver() CLASS WVWMouseButton

   // mouse is moved to over the button from outside
   IF ! ::lEnable  // 2004-03-03
      RETURN Self
   ENDIF

   IF ::lRepeatPress .AND. ::lPressed
      wvwm_SetKeyRepeater( .T. )  // activate key repeater
   ENDIF

   ::lHover := .T.
   ::Draw()

   RETURN Self

METHOD DRAW( nWinNum ) CLASS WVWMouseButton

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lMouseOver := ::lHover // 2004-03-03,was: ( MRow() >= ::nrow1 .AND. MRow() <= ::nrow2 .AND. MCol() >= ::ncol1 .AND. MCol() <= ::ncol2 )
   LOCAL lPressed := ::lPressed .AND. lMouseOver
   LOCAL aFontInfo := iif( ::nCaptionHeight == NIL, wvw_GetFontInfo( nWinNum ), NIL )
   LOCAL nLabelColor := iif( lPressed, WIN_RGB( 96, 96, 96 ), WIN_RGB( 0, 0, 0 ) )
   LOCAL lUseImage := HB_ISSTRING( ::cImage )  // 2004-03-25

   IF ! ::lVisible .OR. ::nType == _BUTTON_NONE
      SetCursor( nOldCursor )  // 2004-03-03
      RETURN Self
   ENDIF

   IF ::nrow1 > ::nrow2 .OR. ::ncol1 > ::ncol2
      SetCursor( nOldCursor )  // 2004-03-03
      RETURN Self
   ENDIF

   hb_default( @nWinNum, ::nWinId )

   IF lPressed  // ::lPressed
      IF ::nType != _BUTTON_HARD
         wvw_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, win_CreateBrush( WIN_BS_HATCHED,, WIN_HS_FDIAGONAL ), ::lTight, .T. )
         wvw_DrawBoxRecessed( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )  // wvw
      ELSE
         wvw_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, wvw_GetRGBColor( hb_ColorToN( ::cNormalColor ) ), ::lTight )
         wvw_DrawBoxRaised(   nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
      ENDIF

      IF lUseImage .AND. ::nType != _BUTTON_NONE
         IF ! wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
            wapi_MessageBox( , "Button failed wvw_DrawImage( " + ::cImage + " )" )
         ENDIF
      ENDIF

      IF ! Empty( ::cCaption )
         wvw_DrawLabel( nWinNum, ::nRow1, _nCeiling( ( ::nCol2 + ::nCol1 ) / 2 ), ::cCaption, 6, , nLabelColor, WIN_RGB( 198, 198, 198 ), ::cCaptionFont, iif( HB_ISARRAY( afontinfo ), afontinfo[ 2 ], ::nCaptionHeight ), 0, , , , .F., .F. )
      ENDIF
   ELSE
      IF lMouseOver .OR. ::nType == _BUTTON_NORMAL .OR. ::nType == _BUTTON_HARD
         wvw_FillRectangle(   nWinNum, ::nrow1, ::nCol1, ::nrow2, ::nCol2, wvw_GetRGBColor( hb_ColorToN( ::cNormalColor ) ), ::lTight )
         wvw_DrawBoxRaised( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::lTight )
      ELSE
         // must undraw the box. ideally GTWVW has this function
         wvw_DrawBoxGroup( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2 )
      ENDIF

      IF lUseImage .AND. ::nType != _BUTTON_NONE
         IF ! wvw_DrawImage( nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ::cImage, ::lTight )
            wapi_MessageBox( , "Button failed wvw_DrawImage( " + ::cImage + " )" )
         ENDIF
      ENDIF

      IF ! ::lEnable
         nLabelColor := WIN_RGB( 96, 96, 96 )
      ELSEIF lMouseOver
         nLabelColor := WIN_RGB( 255, 0, 0 )
      ENDIF

      IF ! Empty( ::cCaption )
         wvw_DrawLabel( nWinNum, ::nRow1, _nCeiling( ( ::nCol2 + ::nCol1 ) / 2 ), ::cCaption, 6, , nLabelColor, WIN_RGB( 198, 198, 198 ), ::cCaptionFont, iif( HB_ISARRAY( afontinfo ), afontinfo[ 2 ], ::nCaptionHeight ), 0, , , , .F., .F. )
      ENDIF
   ENDIF
   SetCursor( nOldCursor )

   RETURN Self

// interface functions

STATIC PROCEDURE wvwm_paint( nWinNum )

   // normally called by WVW_Paint()
   // redraw every mouse object in window nWinNum
   IF Len( s_amouseobjlist ) >= nWinNum + 1
      AEval( s_amouseobjlist[ nWinNum + 1 ], {| o | o[ 2 ]:draw( nWinNum ) } )
   ENDIF

   RETURN

// clears all mouse objects from window nWinNum
STATIC PROCEDURE wvwm_ResetMouseObjects( nWinNum )

   DO WHILE Len( s_amouseobjlist ) < nWinNum + 1
      AAdd( s_amouseobjlist, {} )
   ENDDO
   s_amouseobjlist[ nWinNum + 1 ] := {}

   RETURN

STATIC PROCEDURE wvwm_AddMouseObjects( nWinNum, oMouse, nObjType )

   // adds a mouse object oMouse into window nWinNum
   AAdd( s_amouseobjlist[ nWinNum + 1 ], { hb_defaultValue( nObjType, _MOBJECT_BUTTON ), oMouse } )

   RETURN

STATIC FUNCTION wvwm_SetKeyRepeater( lSet )

   // returns .T. if KeyRepeater is active
   // if lSet is supplied, KeyRepeater is enable/disable accordingly
   LOCAL lWasSet := ( s_nkeyrepeater != NIL )

   IF HB_ISLOGICAL( lSet )
      IF lSet
         IF ! lWasSet
            s_nkeyrepeater := hb_idleAdd( {|| xKeyRepeater() } )
         ENDIF
      ELSE
         IF lWasSet
            hb_idleDel( s_nkeyrepeater )
            s_nkeyrepeater := NIL
         ENDIF
      ENDIF
   ENDIF

   RETURN lWasSet

STATIC FUNCTION nButtonChecker( nkey, oMouseObj )

   LOCAL nrow := MRow(), ncol := MCol()

   IF nrow >= oMouseObj:nrow1 .AND. nrow <= oMouseObj:nrow2 .AND. ncol >= oMouseObj:ncol1 .AND. ncol <= oMouseObj:ncol2
      // cursor is over current mouse object area

      IF oMouseObj:lHover
         DO CASE
         CASE nkey == K_LDBLCLK
            // currently button not handle this events,
            // so we will treat it as single key press
            oMouseObj:OnPress()
         CASE nkey == K_LBUTTONDOWN
            oMouseObj:OnPress()
         CASE nkey == K_LBUTTONUP
            oMouseObj:OnRelease()
         ENDCASE
      ELSE
         // user has just moved the cursor into over this button
         oMouseObj:OnMouseOver()
      ENDIF
   ELSE
      // cursor is somewhere outside of current mouse object area

      IF oMouseObj:lHover
         // user has just moved the cursor out of this button
         oMouseObj:OnMouseOut()
      ELSE
         DO CASE
         CASE nkey == K_LBUTTONUP
            IF oMouseObj:lPressed
               oMouseObj:OnReleaseOut()
            ENDIF
         ENDCASE
      ENDIF
   ENDIF

   RETURN nkey

STATIC FUNCTION nScrollChecker( nKey, cType, oMouseObj )

   // cType == "H" or "V"

   HB_SYMBOL_UNUSED( cType )

   nButtonChecker( nkey, oMouseObj:oFirstButton )
   nButtonChecker( nkey, oMouseObj:oRail1Button )
   nButtonChecker( nkey, oMouseObj:oMidButton )
   nButtonChecker( nkey, oMouseObj:oRail2Button )
   nButtonChecker( nkey, oMouseObj:oSecondButton )

   RETURN nKey

/* HANDLING MULTIPLE MOUSE OBJECTS */
/* called by SETKEYAFTERBLOCK() function */

STATIC FUNCTION wvwm_nMouseChecker( nkey )

   // check mouse events in relations with registered mouse objects
   // always return inkey codes as if nothing happens here
   // (so as to allow GET do something about it)
   // NOTE: only cares about current (last) window
   LOCAL i, oMouseObj
   LOCAL nCurWindow

   nCurWindow := wvw_nNumWindows() - 1

   IF Len( s_amouseobjlist ) < nCurWindow + 1
      RETURN nkey
   ENDIF

   s_ncurkey := nkey  // 2004-03-03

   FOR EACH i IN s_amouseobjlist[ nCurWindow + 1 ]

      oMouseObj := i[ 2 ]

      SWITCH i[ 1 ]
      CASE _MOBJECT_BUTTON  ; nButtonChecker( nkey, oMouseObj ) ; EXIT
      CASE _MOBJECT_HSCROLL ; nScrollChecker( nkey, "H", oMouseObj ) ; EXIT
      CASE _MOBJECT_VSCROLL ; nScrollChecker( nkey, "V", oMouseObj ) ; EXIT
      OTHERWISE  // runtime error!
      ENDSWITCH
   NEXT

   s_ncurkey := 0  // 2004-03-03

   RETURN nKey

STATIC PROCEDURE xKeyRepeater( lInit )

   STATIC s_nLastValidCheck := 0
   STATIC s_lFirstRepeat := .T.

   LOCAL nNow
   LOCAL nRepeatInterval

   IF hb_defaultValue( lInit, .F. )
      // simply init the locally static var
      s_lFirstRepeat := .T.
      s_nLastValidCheck := Seconds()
      RETURN
   ENDIF

   nRepeatInterval := iif( s_lFirstRepeat, s_nrepeatdelay, s_nrepeatrate )

   nNow := Seconds()
   IF nNow - s_nLastValidCheck < nRepeatInterval  // s_nrepeatrate
      RETURN  // not yet
   ENDIF

   IF ! MLeftDown()
      RETURN  // mouse is not pressed
   ENDIF

   // mouse is down long enough since last valid check
   // let's repeat it
   hb_keyPut( K_LBUTTONDOWN )

   // to be more precise
   s_nLastValidCheck := Seconds()   // nNow

   // next repeat will be quicker
   s_lFirstRepeat := .F.

   RETURN

STATIC FUNCTION _nCeiling( nNumber )
   RETURN Int( nNumber ) + iif( ( nNumber - Int( nNumber ) ) > 0, 1, 0 )
