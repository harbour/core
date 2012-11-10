/*
 * $Id$
 */

//
//
//                      Test/Demo Program for
//                       GTWVW GUI Interface
//                   with multiple window support
//                      a work in progress by
//               Budyanto Dj. <budyanto@centrin.net.id>
//                            based on:
//
//                           WVTGUI.PRG,
//                   GTWVT Console GUI Interface
//              by Pritpal Bedi <pritpal@vouchcac.com>
//
//                              GTWVT
//                by Peter Rees <peter@rees.co.nz>
//
//   parts of this program are copyrights of their respective owners
//
//

/*
   Compile/Link info:
   You may use 'hbmk2 wvwtest9.hbp' to build this program.
 */

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

#define MF_INSERT                     0
#define MF_CHANGE                   128
#define MF_APPEND                   256
#define MF_DELETE                   512
#define MF_REMOVE                  4096

#define MF_BYCOMMAND                  0
#define MF_BYPOSITION              1024

#define MF_SEPARATOR               2048

#define MF_ENABLED                    0
#define MF_GRAYED                     1
#define MF_DISABLED                   2

#define MF_UNCHECKED                  0
#define MF_CHECKED                    8
#define MF_USECHECKBITMAPS          512

#define MF_STRING                     0
#define MF_BITMAP                     4
#define MF_OWNERDRAW                256

#define MF_POPUP                     16
#define MF_MENUBARBREAK              32
#define MF_MENUBREAK                 64

#define MF_UNHILITE                   0
#define MF_HILITE                   128

#define MB_OK                                 0
#define MB_OKCANCEL                           1
#define MB_ABORTRETRYIGNORE                   2
#define MB_YESNOCANCEL                        3
#define MB_YESNO                              4
#define MB_RETRYCANCEL                        5
#define MB_CANCELTRYCONTINUE                  6


#define MB_ICONHAND                          16
#define MB_ICONQUESTION                      32
#define MB_ICONEXCLAMATION                   48
#define MB_ICONASTERISK                      64

#define MB_USERICON                         128
#define MB_ICONWARNING              MB_ICONEXCLAMATION
#define MB_ICONERROR                MB_ICONHAND

#define MB_ICONINFORMATION          MB_ICONASTERISK
#define MB_ICONSTOP                 MB_ICONHAND

#define MB_DEFBUTTON1                         0
#define MB_DEFBUTTON2                       256
#define MB_DEFBUTTON3                       512
#define MB_DEFBUTTON4                       768

#define MB_APPLMODAL                          0
#define MB_SYSTEMMODAL                     4096
#define MB_TASKMODAL                       8192
#define MB_HELP                           16384 // Help Button

#define MB_NOFOCUS                        32768
#define MB_SETFOREGROUND                  65536
#define MB_DEFAULT_DESKTOP_ONLY          131072

#define MB_TOPMOST                       262144
#define MB_RIGHT                         524288
#define MB_RTLREADING                   1048576

#define IDOK                1
#define IDCANCEL            2
#define IDABORT             3
#define IDRETRY             4
#define IDIGNORE            5
#define IDYES               6
#define IDNO                7
#define IDCLOSE             8
#define IDHELP              9

#define IDTRYAGAIN          10

#define WVW_MAXWINDOWS    20             // ! must match with HBGTWVW.H
#define WVW_DEFAULT_MENUKEYEVENT  1024   // ! must match with HBGTWVW.H

// 20040303: !!! copied from WVWMOUSE.PRG pls create an include file
// mouse object types //20040303
#define _MOBJECT_BUTTON  0      // mouse button
#define _MOBJECT_HSCROLL 1      // horiz scrollbar  //obsolete, not used
#define _MOBJECT_VSCROLL 2      // horiz scrollbar  //obsolete, not used

// for Button Types: //20040303
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


STATIC s_amiscobjlist := {}      // x misc object list (actually: list of codeblocks)
STATIC s_afontinfo := {}         // x current font info

//

PROCEDURE Main()

   LOCAL nCurWindow
   LOCAL hWnd, hMenu, hPopupmenu, hPopupmenu2
   LOCAL cLabel := "This is the Main Window"
   LOCAL nMaxRow, nMaxCol
   LOCAL nCursor
   LOCAL kF1, kF2, kF3
   LOCAL kF9, kF10, kF11
   LOCAL oMouse
   LOCAL ch

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   SET( _SET_EVENTMASK, INKEY_ALL )

   SET DATE ANSI
   SET CENTURY ON
   SET SCOREBOARD OFF
   // wvw_SetPaintRefresh( 0 )
   wvw_SetVertCaret( .T. )
   wvw_pbSetFont( , "Tahoma", 14 )
   nCursor := SetCursor( SC_NONE )

   IF ! SetDefaultWindowSize()
      ldebug( "Cannot setDefaultWindowSize()" )
   ELSE
      ldebug( "Successfully setDefaultWindowSize()" )
   ENDIF
   nMaxRow := MaxRow(); nMaxCol := MaxCol()

   IF wvw_SBcreate() > 0 .AND. ;
         wvw_SBaddPart( , "99:99:99" ) > 0
      wvw_SetTimer( , 1000 )
   ENDIF

   s_afontinfo := WVW_getfontinfo()

   hb_gtInfo( HB_GTI_INKEYFILTER, {| nkey | nAfterInkey( nkey ) } )
   WVW_SETMOUSEMOVE( , .T. )                           // required by wvwmouse
   kF1 := SetKey( K_F1, {|| xHelp() } )
   kF2 := SetKey( K_F2, {|| xDebugInfo() } )
   kF3 := SetKey( K_F3, {|| Demo_Console() } )

   kF9 := SetKey( K_F9, {|| WVW_SetLineSpacing( NIL, WVW_SetLineSpacing() - 2 ) } )
   kF10 := SetKey( K_F10, {|| WVW_SetLineSpacing( NIL, WVW_SetLineSpacing() + 2 ) } )
   kF11 := SetKey( K_F11, {|| WVW_SetDefLineSpacing( WVW_SetLineSpacing() ) } )

   // start menu definitions *************************************

   hWnd := WVW_GETWINDOWHANDLE()
   hMenu := WVW_CreateMenu()
   hPopupMenu := WVW_CreateMenu()
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_GET, "~GET demo"  )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_BROWSE, "~BROWSE demo" )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_CONSOLE, "~CONSOLE demo (F3)" )
   // WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_COLOR, "~COLOR demo" )
   WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_EXIT, "E~xit"  )
   WVW_AppendMenu( hMenu, MF_ENABLED + MF_POPUP, hPopupMenu, "~Demos",  )

   hPopupMenu := WVW_CreateMenu()
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_ENABLE,  "~Enable Toolbar" )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_DISABLE, "~Disable Toolbar" )
   WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_RESET,  "~Reset Toolbar" )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_DELETE, "~Delete Toolbar" )
   WVW_AppendMenu( hMenu, MF_ENABLED + MF_POPUP, hPopupMenu, "~Toolbar",  )

   hPopupMenu := WVW_CreateMenu()
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_DECREASE, "~Decrease Line Spacing (F9)" )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_INCREASE, "~Increase Line Spacing (F10)" )
   WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_DEFAULT,  "~Set As Default Line Spacing (F11)" )
   WVW_AppendMenu( hMenu, MF_ENABLED + MF_POPUP, hPopupMenu, "~Window",  )

   hPopupMenu := WVW_CreateMenu()
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_HELP_HELP, "~Help (F1)"  )
   WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
   WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_HELP_INFO, "~Info (F2)"  )
   WVW_AppendMenu( hMenu, MF_ENABLED + MF_POPUP, hPopupMenu, "~Help",  )

   WVW_SetMenu( , hMenu )

   // end menu definitions *************************************

   nCurWindow := WVW_nNumWindows() - 1 // == 0, Main Window

   CreateToolbar( nCurWindow )

   ResetMiscObjects( nCurWindow )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawLabel( nWindow, 1, 40, cLabel, 6,, rgb( 255, 255, 255 ), rgb( 198, 198, 198 ), "Arial", s_afontinfo[ 2 ], , , , , .T., .T. ) } )

   wvwm_ResetMouseObjects( nCurWindow )
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New( "Info!", MaxRow() - 2, 67, , , {|| xDebugInfo() } ) )

   oMouse := WVWMouseButton():New( "Flat",   MaxRow() - 2, 67 - 11, , , {|| lboxmessage( "flat" ) }, 1, NIL )
   oMouse:cImage := "vouch1.gif"
   oMouse:cCaption := ""
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   oMouse := WVWMouseButton():New( "None",   MaxRow() - 2, 67 - 11 - 11, , , {|| lboxmessage( "none" ) }, 2, NIL )
   oMouse:Enable( .T. )
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New( "Hard",   MaxRow() - 2, 67 - 11 - 11 - 11, , , {|| lboxmessage( "hard" ) }, 3, NIL ) )
   oMouse := WVWMouseButton():New( "Disabled",   MaxRow() - 2, 67 - 11 - 11 - 11 - 11, , , {|| xDebugInfo() } )
   oMouse:Enable( .F. )
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   oMouse := WVWMouseButton():New( "Tight",   MaxRow() - 2, 67 - 11 - 11 - 11 - 11 - 11, , , {|| lboxmessage( "tight" ) } )
   oMouse:lTight := .T.
   wvwm_AddMouseObjects( nCurWindow, oMouse )

   // 20070525 the real pushbutton, easier and better looking. Nothing to do with wvwmouse.prg.
   WVW_PBcreate( nCurWindow, MaxRow() - 4, 67 - 11 - 11 - 11 - 11 - 11, MaxRow() - 4, 67 + 9 - 11 - 11 - 11 - 11 - 11, "native", NIL, {|| lboxmessage( "native pushbutton" ) }, NIL )

   SetColor( "N/W,N/GR*,,,N/W*" )
   CLS
   @ 0, 0 SAY "This is line 0"
   @ 1, 0 SAY "This is line 1"
   @ MaxRow() - 1, 0 SAY "This is line " + hb_ntos( MaxRow() - 1 )
   @ MaxRow(), 0 SAY "This is line " + hb_ntos( MaxRow() )

   DO WHILE ( ch := Inkey( 0 ) ) != K_ESC
      // experiment with different paintrefresh interval:
#if 0
      DO CASE
      CASE ch == hb_keyCode( "<" )
         wvw_setPaintRefresh( Int( wvw_setPaintRefresh() / 2 ) )
         Alert( wvw_setPaintRefresh() )
      CASE ch == hb_keyCode( ">" )
         wvw_setPaintRefresh( Int( wvw_setPaintRefresh() * 2 ) )
         Alert( wvw_setPaintRefresh() )
      CASE ch == hb_keyCode( "0" )
         wvw_setPaintRefresh( 0 )
         Alert( wvw_setPaintRefresh() )
      OTHERWISE
         // do nothing. inkey() has been handled by nAfterInket()
      ENDCASE
#endif
   ENDDO

   lboxmessage( "Thanks for trying this program." + hb_eol() + ;
      "Good bye!" )

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

   RETURN  // main()

//

STATIC PROCEDURE xDisableMenus( nWinNum, nNumItem )

   // disables all Menu Items of window nWinNum
   LOCAL i
   LOCAL hMenu := WVW_GetMenu( nWinNum )
   FOR i := 0 TO nNumItem - 1
      WVW_EnableMenuItem( hMenu, i, MF_BYPOSITION + MF_GRAYED )
   NEXT

   RETURN

STATIC PROCEDURE xEnableMenus( nWinNum, nNumItem )

   // enables all Menu Items of window nWinNum
   LOCAL i
   LOCAL hMenu := WVW_GetMenu( nWinNum )
   FOR i := 0 TO nNumItem - 1
      WVW_EnableMenuItem( hMenu, i, MF_BYPOSITION + MF_ENABLED )
   NEXT
   WVW_DrawMenuBar( nWinNum )   // to force redraw of menu

   RETURN

//

PROCEDURE Demo_Console( nTop, nLeft, nBottom, nRight )

   LOCAL cWinName, nCurWindow
   LOCAL nMaxrow, nMaxCol
   LOCAL nCursor
   LOCAL cColor
   LOCAL ch
   LOCAL lMouseMove
   LOCAL lEchoing := .F.

   hb_default( @nTop, 2 )
   hb_default( @nLeft, 2 )
   hb_default( @nBottom, nTop + 10 )
   hb_default( @nRight, nLeft + 45 )

   cWinName := "Typewriter (Win#" + hb_ntos( WVW_nNumWindows() ) + "); CtrlW: New Window; ESC: Exit"

   // x init window
   nCurWindow := WVW_nOpenWindow( cWinName, nTop, nLeft, nBottom, nRight )
   IF nCurWindow == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN
   ENDIF

   nCursor := SetCursor( SC_NORMAL )
   cColor := SetColor( "W+/N" )
   lMouseMove := WVW_SETMOUSEMOVE( , .F. )
   nMaxrow := MaxRow(); nMaxcol := MaxCol()

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   // *********** begin typewriter mode *************
   CLS
   ?? "Press Ctrl+E to toggle between echoing what you type to previous window"
   ?
   DO WHILE Inkey() != 0; ENDDO  // clear typeahead
   ch := Inkey( 0 )
   DO WHILE ch != K_ESC
      IF ch == K_ENTER
         ?? hb_keyChar( ch ) + Chr( 10 )
         IF lEchoing
            // write the same thing to previous window
            WVW_nSetCurWindow( nCurWindow - 1 )
            ?? hb_keyChar( ch ) + Chr( 10 )
            WVW_nSetCurWindow( nCurWindow )
         ENDIF
      ELSEIF ch == K_CTRL_W
         // Recursively call (another) typewriter, bigger one
         Demo_Console( nTop + 2, nLeft + 2, nBottom + 4, nRight + 6 )
      ELSEIF ch == K_CTRL_E
         // toggle echoing output to prev window
         lEchoing := ! lEchoing
      ELSE
         // any other char goes here
         ?? hb_keyChar( ch )
         IF lEchoing
            // write the same thing to previous window
            WVW_nSetCurWindow( nCurWindow - 1 )
            ?? hb_keyChar( ch )
            WVW_nSetCurWindow( nCurWindow )
         ENDIF
      ENDIF
      ch := Inkey( 0 )
   ENDDO

   // *********** end typewriter mode ***************

   // epilogue
   WVW_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetCursor( nCursor )
   SetColor( cColor )
   WVW_SETMOUSEMOVE( , lMouseMove )

   RETURN // Demo_Console()

//

PROCEDURE Demo_Get()

   LOCAL nCurWindow, getlist := {}
   LOCAL cLabel := "This is the GET Demo Window"
   LOCAL nTop    := 4
   LOCAL nLeft    := 4
   LOCAL nBottom    := 21
   LOCAL nRight    := 75
   LOCAL nColGet := 8
   LOCAL get_1   := SToD()
   LOCAL get_2   := PadR( "Pritpal Bedi", 35 )
   LOCAL get_3   := PadR( "60, New Professor Colony", 35 )
   LOCAL get_4   := PadR( "Ludhiana, INDIA", 35 )
   LOCAL get_5   := PadR( "http://www.vouchcac.com", 35 )
   LOCAL get_6   := 20000
   LOCAL nCursor := SetCursor( SC_NORMAL )
   MEMVAR x

   // x init window
   nCurWindow := WVW_nOpenWindow( "GET Demo", nTop, nLeft, nBottom, nRight )
   IF nCurWindow == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN
   ENDIF

   WVW_SetIcon( , "vr_1.ico" )

   ResetMiscObjects( nCurWindow )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawLabel( nWindow, 1, nRight - nLeft, cLabel, 2,, rgb( 255, 255, 255 ), rgb( 198, 198, 198 ), "Arial", s_afontinfo[ 2 ], , , , , .T., .T. ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawBoxRecessed( nWindow, 7 - nTop, 61 - nLeft, 13 - nTop, 70 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawBoxGroup( nWindow, 15 - nTop, 59 - nLeft, 18 - nTop, 72 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawBoxGroup( nWindow, 5 - nTop, 6 - nLeft, 19 - nTop, 44 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawImage( nWindow, 8 - nTop, 62 - nLeft, 12 - nTop, 69 - nLeft, "vouch1.bmp" ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawBoxRecessed( nWindow, 7 - nTop, 48 - nLeft, 13 - nTop, 55 - nLeft ) } )
   AddMiscObjects( nCurWindow, {| nWindow | x := nWindow, AEval( GetList, {| oGet | WVW_DrawBoxGet( x, oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   wvwm_ResetMouseObjects( nCurWindow )

   /* we now use native push button
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info", maxrow() - 1, maxcol() - 15, , , {|| xDebugInfo() } ))
   */
   WVW_PBcreate( nCurWindow, MaxRow() - 1, MaxCol() - 15, MaxRow() - 1, MaxCol() - 5, "Info", NIL, {|| xDebugInfo() }, NIL )

   CLS

   @  6 - nTop, nColGet - nLeft SAY "< Date >"
   @  9 - nTop, nColGet - nLeft SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12 - nTop, nColGet - nLeft SAY "<" + PadC( "Address", 33 ) + ">"
   @ 16 - nTop, 61 - nLeft      SAY "< Salary >"

   @  7 - nTop, nColGet - nLeft GET get_1
   @ 10 - nTop, nColGet - nLeft GET get_2 // VALID ( VouChoice() < 7 )
   @ 13 - nTop, nColGet - nLeft GET get_3
   @ 15 - nTop, nColGet - nLeft GET get_4
   @ 17 - nTop, nColGet - nLeft GET get_5
   @ 17 - nTop, 61 - nLeft      GET get_6 PICTURE "@Z 9999999.99"

   READ

   // epilogue
   // lboxmessage( "Thanks for trying the GET Demo!" )
   WVW_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetCursor( nCursor )

   RETURN

//

FUNCTION DEMO_Browse()

   LOCAL nKey, bBlock, oBrowse, i
   LOCAL lEnd    := .F.
   LOCAL info_   := {}             // WVW_nOpenWindow() has not been performed, so...
   LOCAL nTop    :=  3             // pls notice that this is relative to PARENT window!
   LOCAL nLeft   :=  3             // pls notice that this is relative to PARENT window!
   LOCAL nBottom := MaxRow() - 2   // pls notice that this is relative to PARENT window!
   LOCAL nRight  := MaxCol() - 3   // pls notice that this is relative to PARENT window!
   LOCAL cColor
   LOCAL nMaxRow, nMaxCol

   LOCAL nStyle := 0
   LOCAL nCurWindow

   LOCAL oMouse, nHScrollBar, nVScrollBar

   LOCAL aColumnsSep, tmp

   // x init window
   nCurWindow := WVW_nOpenWindow( "BROWSE Demo", nTop, nLeft, nBottom, nRight )
   IF nCurWindow == 0
      lboxmessage( "Failed Opening new window!" )
      RETURN NIL
   ENDIF
   nMaxRow := MaxRow(); nMaxCol := MaxCol()

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   cColor := SetColor( "N/W" )
   CLS
   SetColor( "N/W*,N/GR*,,,N/W* " )

   USE "..\..\..\tests\test" NEW
   IF NetErr()
      WVW_lCloseWindow()
      RETURN NIL
   ENDIF

   INDEX ON FIELD->LAST TO TEST1  // 20040707

   info_ := dbStruct()

   oBrowse := TBrowseNew( 3, 2, MaxRow() - 3, MaxCol() - 3 )

   oBrowse:ColSep        := "  "
   oBrowse:HeadSep       := "__"
   oBrowse:GoTopBlock    := {|| dbGoTop() }
   oBrowse:GoBottomBlock := {|| dbGoBottom() }
   oBrowse:SkipBlock     := {| nSkip | dbSkipBlock( nSkip, oBrowse ) }

   FOR i := 1 TO Len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i, 1 ], bBlock ) )
   NEXT

   oBrowse:configure()

   WVW_SetPen( nStyle, 0, rgb( 210, 1210, 210 ) )
   WVW_SetIcon( , "dia_excl.ico" )

   aColumnsSep := Array( oBrowse:colCount )
   FOR EACH tmp IN aColumnsSep
      tmp := oBrowse:getColumn( tmp:__enumIndex() ):colSep
   NEXT

   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawBoxRecessed( nWindow, oBrowse:nTop, oBrowse:nLeft, oBrowse:nBottom, oBrowse:nRight ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawGridHorz( nWindow, oBrowse:nTop + 3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   AddMiscObjects( nCurWindow, {| nWindow | WVW_DrawGridVert( nWindow, oBrowse:nTop, oBrowse:nBottom, aColumnsSep, Len( aColumnsSep ) ) } )

   /* we now use native push button
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info", maxrow(), maxcol() - 15, , , {|| xDebugInfo() } ))
   */
   WVW_PBcreate( nCurWindow, MaxRow(), MaxCol() - 15, MaxRow(), MaxCol() - 5, "Info", NIL, {|| xDebugInfo() }, NIL )

   nHScrollBar := wvw_xbCreate( nCurWindow, 0, oBrowse:nBottom + 1, oBrowse:nLeft, oBrowse:nRight - oBrowse:nLeft + 1, /*aBlock*/ {| nWinNum, nXBid, nXBmsg, nXBpos | HXBscroller( oBrowse, nWinNum, nXBid, nXBmsg ) }, /*aOffset*/ NIL )
   nVScrollBar := wvw_xbCreate( nCurWindow, 1, oBrowse:nTop, oBrowse:nRight + 1, oBrowse:nBottom - oBrowse:nTop + 1, /*aBlock*/ {| nWinNum, nXBid, nXBmsg, nXBpos | VXBscroller( oBrowse, nWinNum, nXBid, nXBmsg ) }, /*aOffset*/ NIL )

   hb_DispOutAt( nTop + 1 - nTop, nleft - nleft, PadC( hb_CurDrive() + ":" + hb_ps() + CurDir() + hb_ps() + "test.dbf", nRight - nLeft + 1 ), "W+/W" )

   oBrowse:ForceStable()
   RefreshHXB( oBrowse, nCurWindow, nHScrollBar ) // 20040704
   RefreshVXB( oBrowse, nCurWindow, nVScrollBar ) // 20040704

   WHILE ! lEnd
      nKey := Inkey( 0 )

      DO CASE
      CASE nKey == K_ESC .OR. nKey == K_ENTER
         lEnd := .T.
         LOOP

      CASE nKey == K_DOWN
         oBrowse:Down()

      CASE nKey == K_MWBACKWARD
         oBrowse:Down() // simple scroll down

      CASE nKey == K_UP
         oBrowse:Up()

      CASE nKey == K_MWFORWARD
         oBrowse:Up()   // simple scroll up

      CASE nKey == K_LEFT
         IF oBrowse:colPos == 1; loop; ENDIF
         oBrowse:Left()

      CASE nKey == K_RIGHT
         IF oBrowse:colPos == oBrowse:colCount; loop; ENDIF
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
      RefreshHXB( oBrowse, nCurWindow, nHScrollBar ) // 20040704
      RefreshVXB( oBrowse, nCurWindow, nVScrollBar ) // 20040704

   ENDDO

   dbCloseArea()

   // epilogue
   // lboxmessage("Thanks for trying the BROWSE Demo!")
   WVW_lCloseWindow()

   // restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )

   WVW_SetPen( 0 )

   SetColor( cColor )
   // SetCursor( nCursor )

   RETURN NIL // DEMO_Browse()

/* generic Vertical Scrollbar handler for tbrowse */

FUNCTION VXBscroller( oBrowse, nWinNum, XBid, XBmsg )

   LOCAL nOldWin
   LOCAL lNeedStabilize

   // if we can't handle non topmost window we must return right away
   // if nWinNum != wvw_nNumWindows()-1 ; return ; endif

   nOldWin := wvw_nSetCurWindow( nWinNum )

   lNeedStabilize := .F.
   DO WHILE .T.  // dummy loop
      DO CASE
      CASE XBmsg == 0 // SB_LINEUP
         IF ordKeyNo() == 1; exit; ENDIF
         oBrowse:up()
      CASE XBmsg == 1 // SB_LINEDOWN
         IF ordKeyNo() == ordKeyCount(); exit; ENDIF
         oBrowse:down()
      CASE XBmsg == 2 // SB_PAGEUP
         IF ordKeyNo() == 1; exit; ENDIF
         oBrowse:pageup()
      CASE XBmsg == 3 // SB_PAGEDOWN
         IF ordKeyNo() == ordKeyCount(); exit; ENDIF
         oBrowse:pagedown()
      OTHERWISE
         // ignore
         lNeedStabilize := .F.
         EXIT
      ENDCASE
      lNeedStabilize := .T.
      EXIT
   ENDDO // dummy loop

   IF lNeedStabilize
      oBrowse:ForceStable()
      RefreshVXB( oBrowse, nWinNum, XBid )
   ENDIF

   wvw_nSetCurWindow( nOldWin )

   RETURN NIL

/* generic Horizontal Scrollbar handler for tbrowse */

FUNCTION HXBscroller( oBrowse, nWinNum, XBid, XBmsg )

   LOCAL nOldWin
   LOCAL lNeedStabilize

   // if we can't handle non topmost window we must return right away
   // if nWinNum != wvw_nNumWindows()-1 ; return ; endif

   nOldWin := wvw_nSetCurWindow( nWinNum )

   lNeedStabilize := .F.
   DO WHILE .T.  // dummy loop
      DO CASE
      CASE XBmsg == 0 // SB_LINELEFT
         IF oBrowse:colPos == 1; exit; ENDIF
         oBrowse:Left()
      CASE XBmsg == 1 // SB_LINERIGHT
         IF oBrowse:colpos == oBrowse:colCount; exit; ENDIF
         oBrowse:Right()
      CASE XBmsg == 2 // SB_PAGELEFT
         IF oBrowse:colPos == 1; exit; ENDIF
         oBrowse:panleft()
      CASE XBmsg == 3 // SB_PAGERIGHT
         IF oBrowse:colpos == oBrowse:colCount; exit; ENDIF
         oBrowse:panright()
      OTHERWISE
         // ignore
         lNeedStabilize := .F.
         EXIT
      ENDCASE
      lNeedStabilize := .T.
      EXIT
   ENDDO // dummy loop
   IF lNeedStabilize
      oBrowse:ForceStable()
      RefreshHXB( oBrowse, nWinNum, XBid )
   ENDIF

   wvw_nSetCurWindow( nOldWin )

   RETURN NIL

/**
20040704 notes:

0 <= nPage <= (nMax - nMin + 1)
nPage :: pagesize

nMin <= nPos <= (nMax - max(nPage-1, 0))
**/

STATIC FUNCTION RefreshVXB( oBrowse, nWinNum, XBid )

   LOCAL nMin, nMax, nPage, nPos
   LOCAL nRatio

   // recalc the pos
   IF ordKeyCount() < 30000
      nRatio := 1
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

   WVW_XBupdate( nWinNum, XBid, nPos, nPage, nMin, nMax )

   RETURN NIL

STATIC FUNCTION RefreshHXB( oBrowse, nWinNum, XBid )

   LOCAL nMin, nMax, nPage, nPos
   LOCAL nRatio

   // recalc the pos
   nMin := 1
   nMax := oBrowse:ColCount
   nPage := oBrowse:RightVisible - oBrowse:LeftVisible + 1
   nPos := iif( oBrowse:RightVisible == oBrowse:ColCount, nMax, oBrowse:LeftVisible )

   WVW_XBupdate( nWinNum, XBid, nPos, nPage, nMin, nMax )

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

   dbSkip( - 1 )

   IF Bof()
      dbGoto( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

//

STATIC FUNCTION VouBlockField( i )

   RETURN {|| FieldGet( i ) }

//
//      WVW_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//
// 20040330,was: FUNCTION WVW_Paint( hWnd, msg, wParam, lParam, nWinNum )
// 20040408,was: FUNCTION WVW_Paint( nWinNum, nrow1, ncol1, nrow2, ncol2 )

FUNCTION WVW_Paint( nWinNum )

   // ldebug( "WVW_Paint:" + hb_eol() +;
   //        "hWnd = " + hb_ntos( hWnd ) + hb_eol() +;
   //        "nWinNum = " + hb_ntos( nWinNum ) )
   IF Len( s_amiscobjlist ) >= nWinNum + 1
      AEval( s_amiscobjlist[ nWinNum + 1 ], {| e | Eval( e, nWinNum ) } )
   ENDIF

   wvwm_paint( nWinNum )

   RETURN 0

//
//      WVW_SetFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_SETFOCUS message
//      received by the window.
//
#if 0
FUNCTION WVW_SetFocus( hWnd, nWinNum )

   STATIC s_nGotFocus := 0

   IF nWinNum == 0
      RETURN NIL
   ENDIF
   s_nGotFocus++
   @ 0, 0 SAY s_nGotFocus
   IF s_nGotFocus % 3 == 0
      Alert( "Got focus " + Transform( s_nGotFocus, "9999" ) + "th times" )
   ENDIF

   RETURN NIL
#endif

//
//      WVW_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//
// FUNCTION WVW_KillFocus( hWnd )
// RETURN NIL

FUNCTION WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )

   // this function is called every certain interval, by GTWVW gtwndproc
   WVW_SBsetText( 0, 1, Time() )

   RETURN NIL

FUNCTION CreateToolbar( nWinNum )

   // for toolbar:
   LOCAL nSysBitmap := 1     // 0:none 1:small 2:large
   LOCAL lDisplayText := .F. // text will be displayed as tooltip instead
   LOCAL hWndTB
   LOCAL ldefault

   wvw_tbdestroy( nWinNum )

   ldefault := lYesNo( "would you like to use default toolbar setting?" )

   IF ! ldefault
      nSysBitmap := Alert( "Select toolbar button size", { "Small", "Big" } )
      nSysBitmap := iif( nSysBitmap == 0, 1, nSysBitmap )
      lDisplayText := Alert( "Display text in toolbar?", { "Yes", "No" } ) == 1
   ENDIF

   hWndTB := wvw_tbcreate( nWinNum, lDisplayText, NIL, nSysBitmap )

   IF hWndTB == 0
      lboxmessage( "FAILED create toolbar" )
      RETURN NIL
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
   wvw_tbAddButton( nWinNum, IDM_DEMO_BROWSE, "def2.bmp", "Browse", 0 )
   wvw_tbAddButton( nWinNum, IDM_DEMO_GET,    "vouch1.bmp",  "Get", 0 )
   wvw_tbAddButton( nWinNum )  // separator

   /* using system standard bitmaps */
   wvw_tbAddButton( nWinNum, IDM_COPY,    1 /*STD_COPY*/, "Copy", 1 )
   wvw_tbAddButton( nWinNum, IDM_PASTE,   2 /*STD_PASTE*/, "Paste",  1 )
   wvw_tbAddButton( nWinNum )  // separator

   wvw_tbAddButton( nWinNum, IDM_HELP_INFO, 10 /*STD_PROPERTIES*/, "Info", 1 )
   wvw_tbAddButton( nWinNum, IDM_HELP_HELP, 11 /*STD_HELP*/, "Help", 1 )

   RETURN NIL

FUNCTION xDisableToolbar( nWinNum )

   LOCAL i

   FOR i := 0 TO wvw_tbButtonCount( nWinNum ) - 1
      WVW_TBEnableButton( nWinNum, i, .F. )
   NEXT

   RETURN NIL

FUNCTION xEnableToolbar( nWinNum )

   LOCAL i

   FOR i := 0 TO wvw_tbButtonCount( nWinNum ) - 1
      WVW_TBEnableButton( nWinNum, i, .T. )
   NEXT

   RETURN NIL

//

FUNCTION ResetMiscObjects( nWinNum )

   DO WHILE Len( s_amiscobjlist ) < nWinNum + 1
      AAdd( s_amiscobjlist, {} )
   ENDDO
   s_amiscobjlist[ nWinNum + 1 ] := {}

   RETURN .T.

FUNCTION AddMiscObjects( nWinNum, bAction )

   AAdd( s_amiscobjlist[ nWinNum + 1 ], bAction )

   RETURN .T.

// inkey() handler **************************************

/* this is for use with SETINKEYAFTERBLOCK() */

FUNCTION nAfterInkey( nkey )

   // check if nkey is:
   // (1) menu command, or
   // (2) mouse button action
   LOCAL bAction
   IF nkey == WVW_DEFAULT_MENUKEYEVENT
      // MenuKeyEvent
      RETURN nMenuChecker( WVW_GETLASTMENUEVENT() )
      // was: elseif ASCAN({K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE}, nKey) > 0
   ELSEIF AScan( { K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE, K_MMLEFTDOWN, ;
         K_LDBLCLK }, nKey ) > 0
      // MouseEvent
      RETURN wvwm_nMouseChecker( nkey )
   ELSEIF ( bAction := SetKey( nKey ) ) != NIL
      Eval( bAction, ProcName(), ProcLine(), ReadVar() )
      RETURN 0
   ENDIF

   RETURN nkey // nAfterInkey(nkey)

// MENU handler **************************************

FUNCTION nMenuChecker( nMenuEvent )

   LOCAL nkey := 0

   xDisableMenus( 0, 4 )
   // xDisableToolbar( 0 )

   DO CASE
   CASE nMenuEvent == IDM_DEMO_GET
      // lboxmessage( "Demo GET" )
      Demo_Get()
   CASE nMenuEvent == IDM_DEMO_BROWSE
      // lboxmessage( "Demo BROWSE" )
      Demo_Browse()
   CASE nMenuEvent == IDM_DEMO_CONSOLE
      // lboxmessage( "Demo CONSOLE" )
      Demo_Console()
// CASE nMenuEvent == IDM_DEMO_COLOR
//    // lboxmessage( "Demo COLOR" )
//    Demo_Color()
   CASE nMenuEvent == IDM_DEMO_EXIT
      // lboxmessage( "should EXIT!" )
      nkey := K_ESC

   CASE nMenuEvent == IDM_WINDOW_SPACING_INCREASE
      WVW_SetLineSpacing( NIL, WVW_SetLineSpacing() + 2 )
   CASE nMenuEvent == IDM_WINDOW_SPACING_DECREASE
      WVW_SetLineSpacing( NIL, WVW_SetLineSpacing() - 2 )
   CASE nMenuEvent == IDM_WINDOW_SPACING_DEFAULT
      WVW_SetDefLineSpacing( WVW_SetLineSpacing() )

   CASE nMenuEvent == IDM_TOOLBAR_ENABLE
      xEnableToolbar( 0 )
   CASE nMenuEvent == IDM_TOOLBAR_DISABLE
      xDisableToolbar( 0 )
   CASE nMenuEvent == IDM_TOOLBAR_RESET
      CreateToolbar( 0 )
   CASE nMenuEvent == IDM_TOOLBAR_DELETE
      WVW_TBdestroy( 0 )

   CASE nMenuEvent == IDM_HELP_HELP
      xHelp()
   CASE nMenuEvent == IDM_HELP_INFO
      xDebugInfo()
   OTHERWISE
      lboxmessage( "Sorry, unknown menu option" )
   ENDCASE

   // xEnableToolbar( 0 )
   xEnableMenus( 0, 4 )

   RETURN nkey // nMenuChecker()

// MISCELLANEOUS *******************************************************

FUNCTION lBoxMessage( cMsg, cTitle )

   hb_default( @cTitle, "Info" )
   win_messagebox( WVW_GETWINDOWHANDLE(), cMsg, cTitle, MB_OK + MB_ICONINFORMATION + MB_SYSTEMMODAL )

   RETURN .T.

FUNCTION lYesNo( cMsg, cTitle )

   hb_default( @cTitle, "Konfirmasi" )

   RETURN win_messagebox( WVW_GETWINDOWHANDLE(), cMsg, cTitle, MB_YESNO + MB_ICONQUESTION + MB_SYSTEMMODAL ) == IDYES

FUNCTION lDebug( cMsg )

   RETURN lBoxMessage( cMsg, "Debug" )

FUNCTION xDebugInfo()

   STATIC s_nfh := 0

   MSetPos( MaxRow(), MaxCol() )

   // SETMOUSE( .T., MaxRow(), MaxCol() )

   // WVW_SETMOUSEPOS( WVW_nNumWindows() - 1, MaxRow(), MaxCol() )

   lboxmessage( "GTWVW test/demo" + hb_eol() + ;
      "Budyanto Dj. <budyanto@centrin.net.id>" + hb_eol() + ;
      hb_eol() + ;
      "Topmost Window is Window #" + hb_ntos( wvw_nNumWindows() - 1 ) + hb_eol() + ;
      "Current Window is Window #" + hb_ntos( wvw_nSetCurWindow() ) + hb_eol() + ;
      "MaxRow() = " + hb_ntos( MaxRow() ) + ", MaxCol() = " + hb_ntos( MaxCol() ) + hb_eol() + ;
      "Row() = " + hb_ntos( Row() ) + ", Col() = " + hb_ntos( Col() ) + hb_eol() + ;
      "WVW_RowOfs() = " + hb_ntos( wvw_nrowofs() ) + ", WVW_ColOfs() = " + hb_ntos( wvw_ncolofs() ) + hb_eol() + ;
      "Line Spacing = " + hb_ntos( WVW_SetLineSpacing() ) + hb_eol() + ;
      "Default Line Spacing = " + hb_ntos( WVW_SetDefLineSpacing() ) + hb_eol() + ;
      hb_eol() + ;
      "Font Face = '" + s_aFontInfo[ 1 ] + "'" + hb_eol() + ;
      "Font Height = " + hb_ntos( s_aFontInfo[ 2 ] ) + hb_eol() + ;
      "Font Width = " + hb_ntos( s_aFontInfo[ 3 ] ) + hb_eol() + ;
      hb_eol() + ;
      "BTW, mouse pointer now sits on maxrow(),maxcol(), doesn't it?" )

   RETURN NIL

FUNCTION xHelp()

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
      "Press Ctrl+W to open a new, bigger window." + hb_eol() + ;
      "Press ESC to exit)" + hb_eol() + ;
      hb_eol() + ;
      "Maximum number of windows opened: " + hb_ntos( WVW_MAXWINDOWS ) + hb_eol() + ;
      hb_eol() + ;
      "Other info:" + hb_eol() + ;
      "Window repainting is checked at 100msec interval" )

   RETURN NIL

FUNCTION nCeiling( nNumber, nRoundDec )

   // WARNING!!! tidak bekerja untuk nRoundDec > 0
   // nRoundDec: 0=satuan, -1=puluhan, -2=ratusan, -3=ribuan
   LOCAL i
   LOCAL nTemp

   hb_default( @nRoundDec, 0 )  // SATUAN

   IF nRoundDec > 0
      nRoundDec := 0
   ENDIF

   // geser kanan
   FOR i := nRoundDec to ( 0 - 1 )   // artinya kalau SATUAN gak usah
      nNumber := nNumber / 10
   NEXT

   nTemp := nNumber - Int( nNumber )  // right of dec point
   IF nTemp > 0
      nNumber := Int( nNumber ) + 1
   ELSE
      nNumber := Int( nNumber )
   ENDIF

   // geser kiri
   FOR i := nRoundDec to ( 0 - 1 )   // artinya kalau SATUAN gak usah
      nNumber := nNumber * 10
   NEXT

   RETURN nNumber

/* Modified from SetDefaultWindowSize() sample from Peter Rees */
/* Note: width < 0 appears better, but mouse caption will look bad */

FUNCTION SetDefaultWindowSize()

   // x was: LOCAL Result:= SetMode(32,98), ScreenWidth
   LOCAL Result := .T., ScreenWidth
   SetMode( 25, 80 )
   IF Result
      screenWidth := Wvw_GetScreenWidth()
      DO CASE
      CASE screenWidth >= 1024
         Result := Wvw_SetFont( , "Terminal", 20, 10 )
      CASE screenWidth >= 800
         IF hb_osIsWinNT()
            Result := Wvw_SetFont( , "Lucida Console", 16, - 8 )
         ELSE
            Result := Wvw_SetFont( , "System", 16, - 8 )
         ENDIF
      OTHERWISE
         Result := Wvw_SetFont( , "Terminal", 12, 6 )
      ENDCASE
      IF Result
         Wvw_SetCodePage( , 255 )  // #define OEM_CHARSET 255 - from wingdi.h
         CLS
      ENDIF
   ENDIF

   RETURN Result

#if 0

STATIC FUNCTION isWinNT()

   RETURN lYesNo( "I am preparing the 'best' font for you..." + hb_eol() + ;
      "Sorry, is it Windows NT?" )

#endif

// ERROR handler *******************************************************

PROCEDURE ErrorSys()

   ErrorBlock( {| e | MyError( e ) } )

   RETURN

STATIC PROCEDURE MyError( e )

   LOCAL cTrace := "", i := 1, cErr

   cErr := "Runtime error" + hb_eol() + ;
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

PROCEDURE debugging( cMsg, nRow, nCol, nWinNum )

   ? cmsg + hb_ntos( nrow ) + ", " + hb_ntos( ncol )

   RETURN

PROCEDURE pause()

   Tone( 660, 2 )
   Inkey( 0 )

   RETURN

SET PROCEDURE TO "_wvwmous.prg"
