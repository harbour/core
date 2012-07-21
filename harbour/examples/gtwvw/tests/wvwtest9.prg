/*
 * $Id$
 */

//-------------------------------------------------------------------//
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
//-------------------------------------------------------------------//

/*
   Compile/Link info:
   This program requires WVWMOUSE.PRG.
   You may use 'hbmk2 wvwtest9.hbp' to build this program.

 */

#include "inkey.ch"
#include "common.ch"
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

#DEFINE CRLF chr(13)+chr(10)

#DEFINE WVW_MAXWINDOWS    20             //! must match with HBGTWVW.H
#DEFINE WVW_DEFAULT_MENUKEYEVENT  1024   //! must match with HBGTWVW.H

//20040303: !!! copied from WVWMOUSE.PRG pls create an include file
* mouse object types //20040303
#DEFINE _MOBJECT_BUTTON  0      //mouse button
#DEFINE _MOBJECT_HSCROLL 1      //horiz scrollbar  //obsolete, not used
#DEFINE _MOBJECT_VSCROLL 2      //horiz scrollbar  //obsolete, not used

* for Button Types: //20040303
#DEFINE _BUTTON_NORMAL 0        //normal button
#DEFINE _BUTTON_FLAT   1        //'transparent', raised when mouseover
#DEFINE _BUTTON_NONE   2        //no sign even when mouseover or clicked
#DEFINE _BUTTON_HARD   3        //no recessed when pressed

* menu actions
#DEFINE IDM_DEMO_GET     101
#DEFINE IDM_DEMO_BROWSE  102
#DEFINE IDM_DEMO_CONSOLE 103
//#DEFINE IDM_DEMO_COLOR   104
#DEFINE IDM_DEMO_EXIT    199

#DEFINE IDM_TOOLBAR_RESET  501
#DEFINE IDM_TOOLBAR_DELETE 502
#DEFINE IDM_TOOLBAR_ENABLE  503
#DEFINE IDM_TOOLBAR_DISABLE  504

#DEFINE IDM_WINDOW_SPACING_INCREASE  201
#DEFINE IDM_WINDOW_SPACING_DECREASE  202
#DEFINE IDM_WINDOW_SPACING_DEFAULT   203

#DEFINE IDM_HELP_HELP    301
#DEFINE IDM_HELP_INFO    302

* menu action from toolbar only:
#DEFINE IDM_NETCONNECT    401
#DEFINE IDM_NETDISCONNECT 402
#DEFINE IDM_BACK          403
#DEFINE IDM_FORWARD       404
#DEFINE IDM_COPY          405
#DEFINE IDM_PASTE         406


static s_amiscobjlist := {}      //x misc object list (actually: list of codeblocks)
static s_afontinfo := {}         //x current font info

//-------------------------------------------------------------------//
PROCEDURE Main()
local nCurWindow
local hWnd, hMenu, hPopupmenu, hPopupmenu2
local cLabel := "This is the Main Window"
local nMaxRow, nMaxCol
local nCursor
local kF1, kF2, kF3
local kF9, kF10, kF11
local oMouse
local ch

   SET( _SET_EVENTMASK, INKEY_ALL )

   SET DATE ANSI
   SET SCOREBOARD OFF
   //wvw_SetPaintRefresh(0)
   wvw_SetVertCaret(.t.)
   wvw_pbSetFont(, "Tahoma", 14)
   nCursor := setcursor(0)

   if !SetDefaultWindowSize()
      ldebug("Cannot setDefaultWindowSize()")
   else
      ldebug("Successfully setDefaultWindowSize()")
   endif
   nMaxRow := maxrow(); nMaxCol := maxcol()

   if wvw_SBcreate() > 0 .and.;
      wvw_SBaddPart(, "99:99:99") > 0
      wvw_SetTimer(, 1000)
   endif

   s_afontinfo := WVW_getfontinfo()

   hb_gtInfo( HB_GTI_INKEYFILTER, {|nkey| nAfterInkey(nkey) } )
   WVW_SETMOUSEMOVE(,.t.)                           //required by wvwmouse
   kF1 := SetKey( K_F1, {|| xHelp() } )
   kF2 := SetKey( K_F2, {|| xDebugInfo() } )
   kF3 := SetKey( K_F3, {|| Demo_Console() } )

   kF9 := SetKey( K_F9, {|| WVW_SetLineSpacing(NIL, WVW_SetLineSpacing()-2) } )
   kF10 := SetKey( K_F10, {|| WVW_SetLineSpacing(NIL, WVW_SetLineSpacing()+2) } )
   kF11 := SetKey( K_F11, {|| WVW_SetDefLineSpacing( WVW_SetLineSpacing() ) } )

   * start menu definitions *************************************

   hWnd := WVW_GETWINDOWHANDLE()
   hMenu := WVW_CreateMenu( )
      hPopupMenu := WVW_CreateMenu( )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_GET, "~GET demo"  )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_BROWSE, "~BROWSE demo" )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_CONSOLE, "~CONSOLE demo (F3)" )
      //WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_COLOR, "~COLOR demo" )
      WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_DEMO_EXIT, "E~xit"  )
   WVW_AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu , "~Demos",  )

      hPopupMenu := WVW_CreateMenu( )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_ENABLE,  "~Enable Toolbar" )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_DISABLE, "~Disable Toolbar" )
      WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_RESET,  "~Reset Toolbar" )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_TOOLBAR_DELETE, "~Delete Toolbar" )
   WVW_AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu , "~Toolbar",  )

      hPopupMenu := WVW_CreateMenu( )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_DECREASE, "~Decrease Line Spacing (F9)" )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_INCREASE, "~Increase Line Spacing (F10)" )
      WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_WINDOW_SPACING_DEFAULT,  "~Set As Default Line Spacing (F11)" )
   WVW_AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu , "~Window",  )

      hPopupMenu := WVW_CreateMenu( )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_HELP_HELP, "~Help (F1)"  )
      WVW_AppendMenu( hPopupMenu, MF_SEPARATOR )
      WVW_AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, IDM_HELP_INFO, "~Info (F2)"  )
   WVW_AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu , "~Help",  )

   WVW_SetMenu(, hMenu )

   * end menu definitions *************************************

   nCurWindow := WVW_nNumWindows()-1 // == 0, Main Window

   CreateToolbar(nCurWindow)

   ResetMiscObjects( nCurWindow )
     AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawLabel( nWindow, 1,40, cLabel,6,, rgb(255,255,255), rgb(198,198,198), 'Arial', s_afontinfo[2], , , , , .t., .t. ) } )

   wvwm_ResetMouseObjects( nCurWindow )
     wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info!",   maxrow()-2,67, , , {|| xDebugInfo() } ))

     oMouse := WVWMouseButton():New("Flat",   maxrow()-2,67-11, , , {|| lboxmessage("flat") }, 1, NIL )
     oMouse:cImage := "vouch1.gif"
     oMouse:cCaption := ""
     wvwm_AddMouseObjects( nCurWindow, oMouse )

     oMouse := WVWMouseButton():New("None",   maxrow()-2,67-11-11, , , {|| lboxmessage("none") }, 2, NIL )
     oMouse:Enable(.t.)
     wvwm_AddMouseObjects( nCurWindow, oMouse )

     wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Hard",   maxrow()-2,67-11-11-11, , , {|| lboxmessage("hard") }, 3, NIL ))
     oMouse := WVWMouseButton():New("Disabled",   maxrow()-2,67-11-11-11-11, , , {|| xDebugInfo() } )
     oMouse:Enable(.f.)
     wvwm_AddMouseObjects( nCurWindow, oMouse )

     oMouse := WVWMouseButton():New("Tight",   maxrow()-2,67-11-11-11-11-11, , , {|| lboxmessage("tight") } )
     oMouse:lTight := .t.
     wvwm_AddMouseObjects( nCurWindow, oMouse )

   * 20070525 the real pushbutton, easier and better looking. Nothing to do with wvwmouse.prg.
   WVW_PBcreate( nCurWindow, maxrow()-4,67-11-11-11-11-11, maxrow()-4, 67+9-11-11-11-11-11, "native", NIL, {||lboxmessage("native pushbutton")}, NIL)

   SetColor( 'N/W,N/GR*,,,N/W*' )
   CLS
   @ 0,0 say "This is line 0"
   @ 1,0 say "This is line 1"
   @ maxrow()-1,0 say "This is line " + alltrim(str(maxrow()-1))
   @ maxrow(),0 say "This is line " + alltrim(str(maxrow()))

   do while !((ch:=inkey(0))==K_ESC)
      /* experiment with different paintrefresh interval:
      do case
      case ch==asc("<")
         wvw_setPaintRefresh( INT(wvw_setPaintRefresh() / 2) )
         alert(wvw_setPaintRefresh())
      case ch==asc(">")
         wvw_setPaintRefresh( INT(wvw_setPaintRefresh() * 2) )
         alert(wvw_setPaintRefresh())
      case ch==asc("0")
         wvw_setPaintRefresh( 0 )
         alert(wvw_setPaintRefresh())
      otherwise
         * do nothing. inkey() has been handled by nAfterInket()
      endcase
      */
   enddo

   lboxmessage("Thanks for trying this program." + CRLF +;
               "Good bye!")

   * restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   SetKey( K_F11, kF11 )
   SetKey( K_F10, kF10 )
   SetKey( K_F9, kF9 )

   //SetKey( K_F4, kF4 )
   SetKey( K_F3, kF3 )
   SetKey( K_F2, kF2 )
   SetKey( K_F1, kF1 )
   setcursor(nCursor)

RETURN  //main()

//-------------------------------------------------------------------//

static procedure xDisableMenus(nWinNum, nNumItem)
* disables all Menu Items of window nWinNum
local i
local hMenu := WVW_GetMenu(nWinNum)
   for i := 0 to nNumItem-1
      WVW_EnableMenuItem(hMenu, i, MF_BYPOSITION + MF_GRAYED)
   next
return

static procedure xEnableMenus(nWinNum, nNumItem)
* enables all Menu Items of window nWinNum
local i
local hMenu := WVW_GetMenu(nWinNum)
   for i := 0 to nNumItem-1
      WVW_EnableMenuItem(hMenu, i, MF_BYPOSITION + MF_ENABLED)
   next
   WVW_DrawMenuBar(nWinNum)   //to force redraw of menu
return


//-------------------------------------------------------------------//
procedure Demo_Console(nTop, nLeft, nBottom, nRight)
local cWinName, nCurWindow
local nMaxrow, nMaxCol
local nCursor
local cColor
local ch
local lMouseMove
local lEchoing := .f.
   default nTop to 2
   default nLeft to 2
   default nBottom to nTop+10
   default nRight to nLeft+45

   cWinName := "Typewriter (Win#" + alltrim( str( WVW_nNumWindows() ) ) + "); CtrlW: New Window; ESC: Exit"

   //x init window
   nCurWindow := WVW_nOpenWindow(cWinName, nTop, nLeft, nBottom, nRight)
   if nCurWindow==0
      lboxmessage("Failed Opening new window!")
      return
   endif

   nCursor := setcursor(SC_NORMAL)
   cColor := setcolor("W+/N")
   lMouseMove := WVW_SETMOUSEMOVE(,.f.)
   nMaxrow := maxrow(); nMaxcol := maxcol()

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   ************ begin typewriter mode *************
   CLS
   ?? "Press Ctrl+E to toggle between echoing what you type to previous window"
   ?
   do while inkey()!=0; enddo  //clear typeahead
   ch := inkey(0)
   do while !(ch == K_ESC)
      if ch==K_ENTER
         ?? chr(ch)+chr(10)
         if lEchoing
            * write the same thing to previous window
            WVW_nSetCurWindow( nCurWindow-1 )
            ?? chr(ch)+chr(10)
            WVW_nSetCurWindow( nCurWindow )
         endif
      elseif ch==K_CTRL_W
         * Recursively call (another) typewriter, bigger one
         Demo_Console(nTop+2, nLeft+2, nBottom+4, nRight+6)
      elseif ch==K_CTRL_E
         * toggle echoing output to prev window
         lEchoing := !lEchoing
      else
         * any other char goes here
         ?? chr(ch)
         if lEchoing
            * write the same thing to previous window
            WVW_nSetCurWindow( nCurWindow-1 )
            ?? chr(ch)
            WVW_nSetCurWindow( nCurWindow )
         endif
      endif
      ch := inkey(0)
   enddo

   ************ end typewriter mode ***************

   * epilogue
   WVW_lCloseWindow()

   * restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   setcursor(nCursor)
   setcolor(cColor)
   WVW_SETMOUSEMOVE(,lMouseMove)
return //Demo_Console()


//-------------------------------------------------------------------//
PROCEDURE Demo_Get()
local nCurWindow, getlist := {}
local cLabel := "This is the GET Demo Window"
LOCAL nTop    := 4
LOCAL nLeft    := 4
LOCAL nBottom    := 21
LOCAL nRight    := 75
LOCAL nColGet := 8
LOCAL get_1   := SToD()
LOCAL get_2   := Pad( 'Pritpal Bedi', 35 )
LOCAL get_3   := Pad( '60, New Professor Colony', 35 )
LOCAL get_4   := Pad( 'Ludhiana, INDIA', 35 )
LOCAL get_5   := Pad( 'http://www.vouchcac.com', 35 )
LOCAL get_6   := 20000
local nCursor := setcursor(SC_NORMAL)
memvar x

   //x init window
   nCurWindow := WVW_nOpenWindow("GET Demo", nTop, nLeft, nBottom, nRight)
   if nCurWindow==0
      lboxmessage("Failed Opening new window!")
      return
   endif

   WVW_SetIcon(, 'vr_1.ico' )

   ResetMiscObjects( nCurWindow )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawLabel( nWindow, 1,nRight-nLeft, cLabel,2,, rgb(255,255,255), rgb(198,198,198), 'Arial', s_afontinfo[2], , , , , .t., .t. ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawBoxRecessed( nWindow, 7-nTop, 61-nLeft, 13-nTop, 70-nLeft ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawBoxGroup( nWindow, 15-nTop, 59-nLeft, 18-nTop, 72-nLeft ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawBoxGroup( nWindow, 5-nTop, 6-nLeft, 19-nTop, 44-nLeft ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawImage( nWindow, 8-nTop,62-nLeft,12-nTop,69-nLeft, 'vouch1.bmp' ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawBoxRecessed( nWindow, 7-nTop, 48-nLeft, 13-nTop, 55-nLeft ) } )
   AddMiscObjects( nCurWindow, {|nWindow| x:= nWindow, aEval( GetList, {|oGet| WVW_DrawBoxGet( x, oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   wvwm_ResetMouseObjects( nCurWindow )

   /* we now use native push button
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info",   maxrow()-1,maxcol()-15, , , {|| xDebugInfo() } ))
   */
   WVW_PBcreate( nCurWindow, maxrow()-1,maxcol()-15, maxrow()-1, maxcol()-5, "Info", NIL, {||xDebugInfo()}, NIL)

   CLS

   @  6-nTop, nColGet-nLeft SAY '< Date >'
   @  9-nTop, nColGet-nLeft SAY '<' + PadC( 'Name', 33 ) + '>'
   @ 12-nTop, nColGet-nLeft SAY '<' + PadC( 'Address', 33 ) + '>'
   @ 16-nTop, 61-nLeft      SAY '< Salary >'

   @  7-nTop, nColGet-nLeft GET get_1
   @ 10-nTop, nColGet-nLeft GET get_2 //VALID ( VouChoice() < 7 )
   @ 13-nTop, nColGet-nLeft GET get_3
   @ 15-nTop, nColGet-nLeft GET get_4
   @ 17-nTop, nColGet-nLeft GET get_5
   @ 17-nTop, 61-nLeft      GET get_6 PICTURE '@Z 9999999.99'

   READ

   * epilogue
   //lboxmessage("Thanks for trying the GET Demo!")
   WVW_lCloseWindow()

   * restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )
   setcursor(nCursor)

RETURN  //Demo_Get()


//-------------------------------------------------------------------//
FUNCTION DEMO_Browse()

   LOCAL nKey, bBlock, oBrowse , i
   LOCAL lEnd    := .f.
   LOCAL info_   := {}             //WVW_nOpenWindow() has not been performed, so...
   LOCAL nTop    :=  3             //pls notice that this is relative to PARENT window!
   LOCAL nLeft   :=  3             //pls notice that this is relative to PARENT window!
   LOCAL nBottom := maxrow() - 2   //pls notice that this is relative to PARENT window!
   LOCAL nRight  := maxcol() - 3   //pls notice that this is relative to PARENT window!
   LOCAL cColor
   local nMaxRow, nMaxCol

   local nStyle := 0
   local nCurWindow

   local oMouse, nHScrollBar, nVScrollBar

   local aColumnsSep, tmp

   //x init window
   nCurWindow := WVW_nOpenWindow("BROWSE Demo", nTop, nLeft, nBottom, nRight)
   if nCurWindow==0
      lboxmessage("Failed Opening new window!")
      return NIL
   endif
   nMaxRow := maxrow(); nMaxCol := maxcol()

   ResetMiscObjects( nCurWindow )
   wvwm_ResetMouseObjects( nCurWindow )

   cColor := SetColor( 'N/W' )
   CLS
   SetColor( 'N/W*,N/GR*,,,N/W* ' )

   USE '..\..\..\tests\TEST' NEW
   if NetErr()
      WVW_lCloseWindow()
      return nil
   endif

   index on FIELD->LAST to TEST1  //20040707

   info_:= DbStruct()

   oBrowse := TBrowseNew( 3, 2, maxrow()-3, maxcol()-3 )

   oBrowse:ColSep        := '  '
   oBrowse:HeadSep       := '__'
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i,1 ], bBlock ) )
   next

   oBrowse:configure()

   WVW_SetPen( nStyle, 0, rgb( 210,1210,210 ) )
   WVW_SetIcon(, 'DIA_EXCL.ico' )

   aColumnsSep := Array( oBrowse:colCount )
   FOR EACH tmp IN aColumnsSep
      tmp := oBrowse:getColumn( tmp:__enumIndex() ):colSep()
   NEXT

   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawBoxRecessed( nWindow, oBrowse:nTop, oBrowse:nLeft, oBrowse:nBottom, oBrowse:nRight ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawGridHorz( nWindow, oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   AddMiscObjects( nCurWindow, {|nWindow| WVW_DrawGridVert( nWindow, oBrowse:nTop, oBrowse:nBottom, aColumnsSep, len( aColumnsSep ) ) } )

   /* we now use native push button
   wvwm_AddMouseObjects( nCurWindow, WVWMouseButton():New("Info",   maxrow(),maxcol()-15, , , {|| xDebugInfo() } ))
   */
   WVW_PBcreate( nCurWindow, maxrow(),maxcol()-15, maxrow(), maxcol()-5, "Info", NIL, {||xDebugInfo()}, NIL)

   nHScrollBar := wvw_xbCreate( nCurWindow, 0, oBrowse:nBottom+1, oBrowse:nLeft, oBrowse:nRight-oBrowse:nLeft+1, /*aBlock*/ {|nWinNum, nXBid, nXBmsg, nXBpos| HXBscroller(oBrowse, nWinNum, nXBid, nXBmsg)}, /*aOffset*/ NIL)
   nVScrollBar := wvw_xbCreate( nCurWindow, 1, oBrowse:nTop, oBrowse:nRight+1, oBrowse:nBottom-oBrowse:nTop+1, /*aBlock*/ {|nWinNum, nXBid, nXBmsg, nXBpos| VXBscroller(oBrowse, nWinNum, nXBid, nXBmsg)}, /*aOffset*/ NIL)

   DispOutAt( nTop + 1-nTop, nleft-nleft, padc( hb_CurDrive()+':\'+CurDir()+'\'+'test.dbf', nRight - nLeft + 1 ), 'W+/W' )

   oBrowse:ForceStable()
   RefreshHXB(oBrowse, nCurWindow, nHScrollBar) //20040704
   RefreshVXB(oBrowse, nCurWindow, nVScrollBar) //20040704

   While !lEnd
      nKey := InKey( 0 )

      do case
      case nKey == K_ESC .or. nKey == K_ENTER
         lEnd := .t.
         loop

      case nKey == K_DOWN
         oBrowse:Down()

      case nKey == K_MWBACKWARD
         oBrowse:Down() //simple scroll down

      case nKey == K_UP
         oBrowse:Up()

      case nKey == K_MWFORWARD
         oBrowse:Up()   //simple scroll up

      case nKey == K_LEFT
         if oBrowse:colPos== 1; loop; endif
         oBrowse:Left()

      case nKey == K_RIGHT
         if oBrowse:colPos== oBrowse:colCount(); loop; endif
         oBrowse:Right()

      case nKey == K_PGDN
         oBrowse:pageDown()

      case nKey == K_PGUP
         oBrowse:pageUp()

      case nKey == K_CTRL_PGUP
         oBrowse:goTop()

      case nKey == K_CTRL_PGDN
         oBrowse:goBottom()

      case nKey == K_HOME
         oBrowse:home()

      case nKey == K_END
         oBrowse:end()

      case nKey == K_CTRL_LEFT
         oBrowse:panLeft()

      case nKey == K_CTRL_RIGHT
         oBrowse:panRight()

      case nKey == K_CTRL_HOME
         oBrowse:panHome()

      case nKey == K_CTRL_END
         oBrowse:panEnd()

      otherwise
         * other keys, including mouse events do not need ForceStable
         loop

      endcase

      oBrowse:ForceStable()

      //refresh the scrollbars due to keyboard navigation
      RefreshHXB(oBrowse, nCurWindow, nHScrollBar) //20040704
      RefreshVXB(oBrowse, nCurWindow, nVScrollBar) //20040704

   enddo

   DBCloseArea()

   * epilogue
   //lboxmessage("Thanks for trying the BROWSE Demo!")
   WVW_lCloseWindow()

   * restore state
   wvwm_ResetMouseObjects( nCurWindow )
   ResetMiscObjects( nCurWindow )

   WVW_SetPen( 0 )

   SetColor( cColor )
   //SetCursor( nCursor )

RETURN nil //DEMO_Browse()

/* generic Vertical Scrollbar handler for tbrowse */
function VXBscroller(oBrowse, nWinNum, XBid, XBmsg)
local nOldWin
local lNeedStabilize

//if we can't handle non topmost window we must return right away
//if nWinNum != wvw_nNumWindows()-1 ; return ; endif

   nOldWin := wvw_nSetCurWindow(nWinNum)

   lNeedStabilize := .f.
   do while .t.  //dummy loop
      do case
      case XBmsg == 0 //SB_LINEUP
         if ordKeyNo() == 1; exit; endif
         oBrowse:up()
      case XBmsg == 1 //SB_LINEDOWN
         if ordKeyNo() == ordKeyCount(); exit; endif
         oBrowse:down()
      case XBmsg == 2 //SB_PAGEUP
         if ordKeyNo() == 1; exit; endif
         oBrowse:pageup()
      case XBmsg == 3 //SB_PAGEDOWN
         if ordKeyNo() == ordKeyCount(); exit; endif
         oBrowse:pagedown()
      otherwise
         * ignore
         lNeedStabilize := .f.
         exit
      endcase
      lNeedStabilize := .t.
      exit
   enddo //dummy loop

   if lNeedStabilize
      oBrowse:ForceStable()
      RefreshVXB(oBrowse, nWinNum, XBid)
   endif

   wvw_nSetCurWindow(nOldWin)

return NIL

/* generic Horizontal Scrollbar handler for tbrowse */
function HXBscroller(oBrowse, nWinNum, XBid, XBmsg)
local nOldWin
local lNeedStabilize

//if we can't handle non topmost window we must return right away
//if nWinNum != wvw_nNumWindows()-1 ; return ; endif

   nOldWin := wvw_nSetCurWindow(nWinNum)

   lNeedStabilize := .f.
   do while .t.  //dummy loop
      do case
      case XBmsg == 0 //SB_LINELEFT
         if oBrowse:colPos== 1; exit; endif
         oBrowse:left()
      case XBmsg == 1 //SB_LINERIGHT
         if oBrowse:colpos == oBrowse:colCount(); exit; endif
         oBrowse:right()
      case XBmsg == 2 //SB_PAGELEFT
         if oBrowse:colPos== 1; exit; endif
         oBrowse:panleft()
      case XBmsg == 3 //SB_PAGERIGHT
         if oBrowse:colpos == oBrowse:colCount(); exit; endif
         oBrowse:panright()
      otherwise
         * ignore
         lNeedStabilize := .f.
         exit
      endcase
      lNeedStabilize := .t.
      exit
   enddo //dummy loop
   if lNeedStabilize
      oBrowse:ForceStable()
      RefreshHXB(oBrowse, nWinNum, XBid)
   endif

   wvw_nSetCurWindow(nOldWin)

return NIL

/**
20040704 notes:

0 <= nPage <= (nMax - nMin + 1)
nPage :: pagesize

nMin <= nPos <= (nMax - max(nPage-1, 0))
**/

static function RefreshVXB(oBrowse, nWinNum, XBid)
local nMin, nMax, nPage, nPos
local nRatio
   //recalc the pos
   if ordKeyCount() < 30000
      nRatio := 1
      nMin := 1
      nMax := ordKeyCount()
      nPage := oBrowse:RowCount()       // ordKeyCount()
      nPos := ordKeyNo()-oBrowse:RowPos+1  // ordKeyCount()
   else
      nRatio := ordKeyCount() / 10
      do while nRatio > 30000
         nRatio := nRatio / 10
      enddo

      nMin := 1
      nMax := round( ordKeyCount() / nRatio, 0)
      nPage := round(oBrowse:RowCount() / nRatio, 0)       // ordKeyCount()
      nPos := round( (ordKeyNo()-oBrowse:RowPos+1) / nRatio, 0 )// ordKeyCount()
   endif

   WVW_XBupdate(nWinNum, XBid, nPos, nPage, nMin, nMax)

return NIL

static function RefreshHXB(oBrowse, nWinNum, XBid)
local nMin, nMax, nPage, nPos
local nRatio
   //recalc the pos
   nMin := 1
   nMax := oBrowse:ColCount
   nPage := oBrowse:RightVisible - oBrowse:LeftVisible + 1
   nPos := iif(oBrowse:RightVisible==oBrowse:ColCount, nMax, oBrowse:LeftVisible)

   WVW_XBupdate(nWinNum, XBid, nPos, nPage, nMin, nMax)

return NIL



//-------------------------------------------------------------------//
STATIC FUNCTION DbSkipBlock( n, oTbr )

   LOCAL nSkipped := 0

   if n == 0
      DBSkip( 0 )

   elseif n > 0
      do while nSkipped != n .and. TBNext( oTbr )
         nSkipped++
      enddo
   else
      do while nSkipped != n .and. TBPrev( oTbr )
         nSkipped--
      enddo
   endif

RETURN  nSkipped

//-------------------------------------------------------------------//
STATIC FUNCTION TBNext( oTbr )

   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   if Eof()
      lMoved := .F.
   else
      DBSkip( 1 )
      if Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      endif
   endif

RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION TBPrev( oTbr )
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

RETURN lMoved
//-------------------------------------------------------------------//
STATIC FUNCTION VouBlockField( i )

RETURN  { || fieldget( i ) }

//-------------------------------------------------------------------//
//      WVW_Paint() must be a FUNCTION in your application
//      as it is called when Window gets WM_PAINT message.
//-------------------------------------------------------------------//
//20040330,was: FUNCTION WVW_Paint( hWnd, msg, wParam, lParam, nWinNum )
//20040408,was: FUNCTION WVW_Paint( nWinNum, nrow1, ncol1, nrow2, ncol2 )
FUNCTION WVW_Paint( nWinNum )
   //ldebug("WVW_Paint:" + CRLF +;
   //       "hWnd = " + alltrim(str(hWnd)) + CRLF +;
   //       "nWinNum = " + alltrim(str(nWinNum)))
   if len(s_amiscobjlist) >= nWinNum+1
      aeval( s_amiscobjlist[nWinNum+1], {|e| eval( e, nWinNum )} )
   endif

   wvwm_paint( nWinNum )
RETURN 0

//-------------------------------------------------------------------//
//      WVW_SetFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_SETFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
/*
FUNCTION WVW_SetFocus( hWnd, nWinNum )
static nGotFocus := 0
   if nWinNum==0
      return NIL
   endif
   nGotFocus++
   @ 0,0 say nGotFocus
   if nGotFocus % 3 == 0
      alert("Got focus " + tran(nGotFocus,"9999") + "th times")
   endif

RETURN nil
*/

//-------------------------------------------------------------------//
//      WVW_KillFocus() must be a FUNCTION in your application
//      needs to process messages sent through WM_KILLFOCUS message
//      received by the window.
//-------------------------------------------------------------------//
//FUNCTION WVW_KillFocus( hWnd )
//RETURN nil

function WVW_TIMER(nWinNum, hWnd, message, wParam, lParam)
* this function is called every certain interval, by GTWVW gtwndproc
   WVW_SBsetText(0, 1, time())
return NIL

function CreateToolbar(nWinNum)
//for toolbar:
local nSysBitmap := 1     //0:none 1:small 2:large
local lDisplayText := .f. //text will be displayed as tooltip instead
local hWndTB
local ldefault

   wvw_tbdestroy(nWinNum)

   ldefault := lYesNo("would you like to use default toolbar setting?")

   if !ldefault
      nSysBitmap := alert("Select toolbar button size", {"Small", "Big"})
      nSysBitmap := iif(nSysBitmap==0, 1, nSysBitmap)
      lDisplayText:= alert("Display text in toolbar?", {"Yes", "No"})==1
   endif

   hWndTB := wvw_tbcreate(nWinNum, lDisplayText, NIL, nSysBitmap)

   if hWndTB==0
      lboxmessage("FAILED create toolbar")
      return NIL
   endif

   /* system bitmaps use constants in commctrl.h */

   /* using system view bitmaps */
   wvw_tbAddButton(nWinNum, IDM_NETCONNECT,    9  /*VIEW_NETCONNECT*/, "Connect",2)
   wvw_tbAddButton(nWinNum, IDM_NETDISCONNECT, 10 /*VIEW_NETDISCONNECT*/, "Dis",2)
   wvw_tbAddButton(nWinNum)  //separator

   /* using system history bitmaps */
   wvw_tbAddButton(nWinNum, IDM_BACK,          0 /*HIST_BACK*/,   "Back",3)
   wvw_tbAddButton(nWinNum, IDM_FORWARD,       1 /*HIST_FORWARD*/,"Forward",3)
   wvw_tbAddButton(nWinNum)  //separator

   /* using custom bitmaps */
   wvw_tbAddButton(nWinNum, IDM_DEMO_BROWSE, "def2.bmp","Browse",0)
   wvw_tbAddButton(nWinNum, IDM_DEMO_GET,    "vouch1.bmp",  "Get",0)
   wvw_tbAddButton(nWinNum)  //separator

   /* using system standard bitmaps */
   wvw_tbAddButton(nWinNum, IDM_COPY,    1 /*STD_COPY*/, "Copy", 1)
   wvw_tbAddButton(nWinNum, IDM_PASTE,   2 /*STD_PASTE*/,"Paste",  1)
   wvw_tbAddButton(nWinNum)  //separator

   wvw_tbAddButton(nWinNum, IDM_HELP_INFO, 10 /*STD_PROPERTIES*/,"Info",1)
   wvw_tbAddButton(nWinNum, IDM_HELP_HELP, 11 /*STD_HELP*/,"Help",1)
return NIL

function xDisableToolbar(nWinNum)
local i
  for i := 0 to wvw_tbButtonCount(nWinNum)-1
     WVW_TBEnableButton(nWinNum, i, .f.)
  next
return NIL

function xEnableToolbar(nWinNum)
local i
  for i := 0 to wvw_tbButtonCount(nWinNum)-1
     WVW_TBEnableButton(nWinNum, i, .t.)
  next
return NIL

//-------------------------------------------------------------------//

function ResetMiscObjects( nWinNum )
   do while len(s_amiscobjlist) < nWinNum+1
      aadd( s_amiscobjlist, {} )
   enddo
   s_amiscobjlist[ nWinNum+1 ] := {}
return .t.

function AddMiscObjects( nWinNum, bAction )
   aadd( s_amiscobjlist[ nWinNum+1 ], bAction )
return .t.


// inkey() handler **************************************

/* this is for use with SETINKEYAFTERBLOCK() */
function nAfterInkey(nkey)
* check if nkey is:
* (1) menu command, or
* (2) mouse button action
local bAction
  if nkey==WVW_DEFAULT_MENUKEYEVENT
     * MenuKeyEvent
     return nMenuChecker(WVW_GETLASTMENUEVENT())
  //was: elseif ASCAN({K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE}, nKey) > 0
  elseif ASCAN({K_LBUTTONDOWN, K_LBUTTONUP, K_MOUSEMOVE, K_MMLEFTDOWN,;
                K_LDBLCLK}, nKey) > 0
     * MouseEvent
     return wvwm_nMouseChecker(nkey)
  elseif (bAction := SETKEY(nKey)) != NIL
     eval(bAction, PROCNAME(), PROCLINE(), READVAR())
     return 0
  endif
return nkey //nAfterInkey(nkey)


// MENU handler **************************************

function nMenuChecker(nMenuEvent)
local nkey := 0
   xDisableMenus(0, 4)
   //xDisableToolbar(0)

   do case
   case nMenuEvent==IDM_DEMO_GET
      * lboxmessage("Demo GET")
      Demo_Get()
   case nMenuEvent==IDM_DEMO_BROWSE
      * lboxmessage("Demo BROWSE")
      Demo_Browse()
   case nMenuEvent==IDM_DEMO_CONSOLE
      * lboxmessage("Demo CONSOLE")
      Demo_Console()
   //case nMenuEvent==IDM_DEMO_COLOR
   //   * lboxmessage("Demo COLOR")
   //   Demo_Color()
   case nMenuEvent==IDM_DEMO_EXIT
      * lboxmessage("should EXIT!")
      nkey := K_ESC

   case nMenuEvent==IDM_WINDOW_SPACING_INCREASE
      WVW_SetLineSpacing(NIL, WVW_SetLineSpacing()+2)
   case nMenuEvent==IDM_WINDOW_SPACING_DECREASE
      WVW_SetLineSpacing(NIL, WVW_SetLineSpacing()-2)
   case nMenuEvent==IDM_WINDOW_SPACING_DEFAULT
      WVW_SetDefLineSpacing( WVW_SetLineSpacing() )

   case nMenuEvent==IDM_TOOLBAR_ENABLE
      xEnableToolbar(0)
   case nMenuEvent==IDM_TOOLBAR_DISABLE
      xDisableToolbar(0)
   case nMenuEvent==IDM_TOOLBAR_RESET
      CreateToolbar(0)
   case nMenuEvent==IDM_TOOLBAR_DELETE
      WVW_TBdestroy(0)

   case nMenuEvent==IDM_HELP_HELP
      xHelp()
   case nMenuEvent==IDM_HELP_INFO
      xDebugInfo()
   otherwise
      lboxmessage("Sorry, unknown menu option")
   endcase

   //xEnableToolbar(0)
   xEnableMenus(0, 4)
return nkey //nMenuChecker()

// MISCELLANEOUS *******************************************************

function lBoxMessage(cMsg, cTitle)
   default cTitle to "Info"
   win_messagebox(WVW_GETWINDOWHANDLE(), cMsg, cTitle, MB_OK + MB_ICONINFORMATION + MB_SYSTEMMODAL)
return .t.

function lYesNo(cMsg, cTitle)
   default cTitle to "Konfirmasi"

return win_messagebox(WVW_GETWINDOWHANDLE(), cMsg, cTitle, MB_YESNO + MB_ICONQUESTION + MB_SYSTEMMODAL) == IDYES

function lDebug(cMsg)
return lBoxMessage(cMsg, "Debug")

function xDebugInfo()
static s_nfh := 0
   MSETPOS(maxrow(), maxcol())

   //SETMOUSE(.t., maxrow(), maxcol())

   //WVW_SETMOUSEPOS(WVW_nNumWindows()-1, maxrow(), maxcol())

   lboxmessage("GTWVW test/demo" + CRLF +;
          "Budyanto Dj. <budyanto@centrin.net.id>" + CRLF+;
          CRLF +;
          "Topmost Window is Window #" + alltrim(str(wvw_nNumWindows()-1)) + CRLF +;
          "Current Window is Window #" + alltrim(str(wvw_nSetCurWindow())) + CRLF +;
          "MaxRow() = " + alltrim(str(maxrow())) + ", MaxCol() = " + alltrim(str(maxcol())) + CRLF +;
          "Row() = " + alltrim(str(row())) + ", Col() = " + alltrim(str(col())) + CRLF +;
          "WVW_RowOfs() = " + alltrim(str(wvw_nrowofs())) + ", WVW_ColOfs() = " + alltrim(str(wvw_ncolofs())) + CRLF +;
          "Line Spacing = " + alltrim(str(WVW_SetLineSpacing()))  + CRLF +;
          "Default Line Spacing = " + alltrim(str(WVW_SetDefLineSpacing()))  + CRLF +;
          CRLF +;
          "Font Face = '" + s_aFontInfo[1] + "'" + CRLF +;
          "Font Height = " + alltrim(str(s_aFontInfo[2])) + CRLF +;
          "Font Width = " + alltrim(str(s_aFontInfo[3])) + CRLF +;
          CRLF+;
          "BTW, mouse pointer now sits on maxrow(),maxcol(), doesn't it?")
return NIL

function xHelp()
   lboxmessage("GTWVW test/demo" + CRLF +;
          "Budyanto Dj. <budyanto@centrin.net.id>" + CRLF+;
          CRLF +;
          "Hotkeys (available in any window):" + CRLF+;
          "F1 : Help" + CRLF+;
          "F2 : Info on current window" + CRLF+;
          "F3 : Open a new window of a pseudo-console typewriter" + CRLF+;
          "F9 : Decrease Line Spacing" + CRLF+;
          "F10: Increase Line Spacing" + CRLF+;
          "F11: Set as Default Line Spacing" + CRLF+;
          CRLF+;
          "Test/demo available:" + CRLF+;
          "GET: a simple GET/READ session"+CRLF+;
          "BROWSE: a simple TBROWSE session"+CRLF+;
          CRLF+;
          "CONSOLE: a simple CONSOLE session" + CRLF+;
          "You are interacting in a typewriter mode." + CRLF+;
          "Press Ctrl+W to open a new, bigger window." + CRLF+;
          "Press ESC to exit)"+CRLF+;
          CRLF+;
          "Maximum number of windows opened: " + alltrim(str(WVW_MAXWINDOWS)) + CRLF+;
          CRLF+;
          "Other info:"+CRLF+;
          "Window repainting is checked at 100msec interval")
return NIL

function nCeiling(nNumber, nRoundDec)
* WARNING!!! tidak bekerja untuk nRoundDec > 0
* nRoundDec: 0=satuan, -1=puluhan, -2=ratusan, -3=ribuan
local i
local nTemp
   default nRoundDec to 0   //SATUAN
   if nRoundDec > 0
      nRoundDec := 0
   endif

   * geser kanan
   for i := nRoundDec to (0-1)   //artinya kalau SATUAN gak usah
      nNumber := nNumber / 10
   next

   nTemp := nNumber - INT(nNumber)  //right of dec point
   if nTemp>0
      nNumber := INT(nNumber) + 1
   else
      nNumber := INT(nNumber)
   endif

   * geser kiri
   for i := nRoundDec to (0-1)   //artinya kalau SATUAN gak usah
      nNumber := nNumber * 10
   next
return nNumber

/* Modified from SetDefaultWindowSize() sample from Peter Rees */
/* Note: width < 0 appears better, but mouse caption will look bad */
FUNCTION SetDefaultWindowSize()
  //x was: LOCAL Result:= SetMode(32,98), ScreenWidth
  LOCAL Result:= .t., ScreenWidth
  SetMode(25,80)
  IF Result
     screenWidth := Wvw_GetScreenWidth()
     DO CASE
     CASE screenWidth >= 1024
       Result:= Wvw_SetFont(,'Terminal',20,10)
     CASE screenWidth >= 800
       IF HB_OSISWINNT()
          Result:= Wvw_SetFont(,'Lucida Console',16,-8)
       ELSE
          Result:= Wvw_SetFont(,'System',16,-8)
       ENDIF
     OTHERWISE
        Result:= Wvw_SetFont(,'Terminal',12,6)
     ENDCASE
     IF Result
        Wvw_SetCodePage(,255)  // #define OEM_CHARSET 255 - from wingdi.h
        CLS
     ENDIF
  ENDIF
RETURN Result

//static function isWinNT()
//return lYesNo("I am preparing the 'best' font for you..." + CRLF+;
//              "Sorry, is it Windows NT?")


//ERROR handler *******************************************************

procedure ErrorSys()
ErrorBlock( { |e| MyError( e ) } )
return


static procedure MyError( e )
local cTrace := "", i := 1 , cErr

cErr := "Runtime error" + CRLF + ;
        CRLF + ;
        "Gencode: " + LTrim( Str( e:GenCode ) ) + CRLF + ;
        "Desc: " + e:Description +  + CRLF + ;
        "Sub-system: " + LTrim( Str( e:SubCode ) ) + CRLF + ;
        CRLF + ;
        "Call trace:" + CRLF + ;
        CRLF


do while ! Empty( ProcName( ++i ) )
    cErr += Trim( ProcName( i ) ) + "(" + Ltrim( Str( ProcLine( i ) ) ) + ")" + CRLF
enddo

//? cErr  // Calls quit
ldebug(cErr)
hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
quit

return

proc debugging(cMsg, nRow, nCol, nWinNum)
  ? cmsg + alltrim(str(nrow)) + ", " + alltrim(str(ncol))
return

proc pause
  tone(660,2)
  inkey(0)
return
