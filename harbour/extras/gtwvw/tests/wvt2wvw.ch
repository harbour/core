/*
 * $Id$
 */

/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   Mapping of gtwvt functions and their coresponding ones in gtwvw.

   WARNING:
   This mapping is made globally. Should you find any error(s) please let me know.
*/

MEMVAR _wvwtemp_

/********************************************************************/
/********************************************************************
PART-1: WINDOW INDEPENDENT (the same parameter list)
*********************************************************************/
/********************************************************************/

#xtranslate WVT_CREATEMENU         ([<vlist,...>])  => WVW_CREATEMENU         ([<vlist>])
#xtranslate WVT_CREATEPOPUPMENU    ([<vlist,...>])  => WVW_CREATEPOPUPMENU    ([<vlist>])
#xtranslate WVT_APPENDMENU         ([<vlist,...>])  => WVW_APPENDMENU         ([<vlist>])
#xtranslate WVT_DELETEMENU         ([<vlist,...>])  => WVW_DELETEMENU         ([<vlist>])
#xtranslate WVT_DESTROYMENU        ([<vlist,...>])  => WVW_DESTROYMENU        ([<vlist>])
#xtranslate WVT_ENABLEMENUITEM     ([<vlist,...>])  => WVW_ENABLEMENUITEM     ([<vlist>])
#xtranslate WVT_GETSCREENWIDTH     ([<vlist,...>])  => WVW_GETSCREENWIDTH     ([<vlist>])
#xtranslate WVT_GETSCREENHEIGHT    ([<vlist,...>])  => WVW_GETSCREENHEIGHT    ([<vlist>])
#xtranslate WVT_SETALTF4CLOSE      ([<vlist,...>])  => WVW_SETALTF4CLOSE      ([<vlist>])
#xtranslate WVT_GETRGBCOLOR        ([<vlist,...>])  => WVW_GETRGBCOLOR        ([<vlist>])
#xtranslate WVT_GETCLIPBOARD       ([<vlist,...>])  => WVW_GETCLIPBOARD       ([<vlist>])
#xtranslate WVT_SETCLIPBOARD       ([<vlist,...>])  => WVW_SETCLIPBOARD       ([<vlist>])
#xtranslate WVT_PASTEFROMCLIPBOARD ([<vlist,...>])  => WVW_PASTEFROMCLIPBOARD ([<vlist>])
#xtranslate WVT_KEYBOARD           ([<vlist,...>])  => WVW_KEYBOARD           ([<vlist>])
#xtranslate WVT_ISLBUTTONPRESSED   ([<vlist,...>])  => WVW_ISLBUTTONPRESSED   ([<vlist>])
#xtranslate WVT_GETPALETTE         ([<vlist,...>])  => WVW_GETPALETTE         ([<vlist>])
#xtranslate WVT_SETPALETTE         ([<vlist,...>])  => WVW_SETPALETTE         ([<vlist>])
#xtranslate WVT_CREATEFONT         ([<vlist,...>])  => WVW_CREATEFONT         ([<vlist>])
#xtranslate WVT_GETCURSORPOS       ([<vlist,...>])  => WVW_GETCURSORPOS       ([<vlist>])
#xtranslate WVT_CHOOSEFONT         ([<vlist,...>])  => WVW_CHOOSEFONT         ([<vlist>])
#xtranslate WVT_CHOOSECOLOR        ([<vlist,...>])  => WVW_CHOOSECOLOR        ([<vlist>])
#xtranslate WVT_LOADPICTURE        ([<vlist,...>])  => WVW_LOADPICTURE        ([<vlist>])
#xtranslate WVT_LOADFONT           ([<vlist,...>])  => WVW_LOADFONT           ([<vlist>])
#xtranslate WVT_LOADPEN            ([<vlist,...>])  => WVW_LOADPEN            ([<vlist>])
#xtranslate WVT_SETPEN             ([<vlist,...>])  => WVW_SETPEN             ([<vlist>])
#xtranslate WVT_SETBRUSH           ([<vlist,...>])  => WVW_SETBRUSH           ([<vlist>])

#xtranslate WVT_CREATEDIALOGDYNAMIC([<vlist,...>])  => WVW_CREATEDIALOGDYNAMIC([<vlist>])
#xtranslate WVT_CREATEDIALOGMODAL  ([<vlist,...>])  => WVW_CREATEDIALOGMODAL  ([<vlist>])
#xtranslate WVT__MAKEDLGTEMPLATE   ([<vlist,...>])  => WVW__MAKEDLGTEMPLATE   ([<vlist>])
#xtranslate WVT_LBADDSTRING        ([<vlist,...>])  => WVW_LBADDSTRING        ([<vlist>])
#xtranslate WVT_LBSETCURSEL        ([<vlist,...>])  => WVW_LBSETCURSEL        ([<vlist>])
#xtranslate WVT_CBADDSTRING        ([<vlist,...>])  => WVW_CBADDSTRING        ([<vlist>])
#xtranslate WVT_CBSETCURSEL        ([<vlist,...>])  => WVW_CBSETCURSEL        ([<vlist>])
#xtranslate WVT_DLGSETICON         ([<vlist,...>])  => WVW_DLGSETICON         ([<vlist>])


/********************************************************************
These functions do not exist in WVT.
*********************************************************************/
//WVT_SETVERTCARET       ([<vlist,...>])  => WVW_SETVERTCARET       ([<vlist>])
//WVT_SETDEFLINESPACING  ([<vlist,...>])  => WVW_SETDEFLINESPACING  ([<vlist>])

/********************************************************************/
/********************************************************************
PART-2: WINDOW DEPENDENT (additional nWinNum parameter)
*********************************************************************/
/********************************************************************/

/*
  Notes:

  nWinNum parameter passed as NIL will be translated by gtwvw into :
    IF ! MainCoordMode
       Current Window
    ELSE
       Topmost Window
    ENDIF

  Since gtwvt application can't be in MainCoordMode,
  the following approach makes these functions work on current window.

  You may want to replace NIL with your own function/variable by which you can decide
  which window to direct your output to.

*/

#xtranslate WVT_SETMENU             ([<vlist,...>])    =>  WVW_SETMENU             (NIL [, <vlist>])
#xtranslate WVT_SETPOPUPMENU        ([<vlist,...>])    =>  WVW_SETPOPUPMENU        (NIL [, <vlist>])
#xtranslate WVT_GETLASTMENUEVENT    ([<vlist,...>])    =>  WVW_GETLASTMENUEVENT    (NIL [, <vlist>])
#xtranslate WVT_SETMENUKEYEVENT     ([<vlist,...>])    =>  WVW_SETMENUKEYEVENT     (NIL [, <vlist>])
#xtranslate WVT_DRAWMENUBAR         ([<vlist,...>])    =>  WVW_DRAWMENUBAR         (NIL [, <vlist>])
#xtranslate WVT_SETWINDOWCENTRE     ([<vlist,...>])    =>  WVW_SETWINDOWCENTRE     (NIL [, <vlist>])
#xtranslate WVT_PROCESSMESSAGES     ([<vlist,...>])    =>  WVW_PROCESSMESSAGES     (NIL [, <vlist>])
#xtranslate WVT_GETTITLE            ([<vlist,...>])    =>  WVW_GETTITLE            (NIL [, <vlist>])
#xtranslate WVT_INVALIDATERECT      ([<vlist,...>])    =>  WVW_INVALIDATERECT      (NIL [, <vlist>])
#xtranslate WVT_CLIENTTOSCREEN      ([<vlist,...>])    =>  WVW_CLIENTTOSCREEN      (NIL [, <vlist>])
#xtranslate WVT_SETFONT             ([<vlist,...>])    =>  WVW_SETFONT             (NIL [, <vlist>])
#xtranslate WVT_SETICON             ([<vlist,...>])    =>  WVW_SETICON             (NIL [, <vlist>])
#xtranslate WVT_SETTITLE            ([<vlist,...>])    =>  WVW_SETTITLE            (NIL [, <vlist>])
#xtranslate WVT_SETWINDOWPOS        ([<vlist,...>])    =>  WVW_SETWINDOWPOS        (NIL [, <vlist>])
#xtranslate WVT_GETWINDOWHANDLE     ([<vlist,...>])    =>  WVW_GETWINDOWHANDLE     (NIL [, <vlist>])
#xtranslate WVT_SETCODEPAGE         ([<vlist,...>])    =>  WVW_SETCODEPAGE         (NIL [, <vlist>])
#xtranslate WVT_CENTERWINDOW        ([<vlist,...>])    =>  WVW_CENTERWINDOW        (NIL [, <vlist>])
#xtranslate WVT_SETMOUSEMOVE        ([<vlist,...>])    =>  WVW_SETMOUSEMOVE        (NIL [, <vlist>])
#xtranslate WVT_GETXYFROMROWCOL     ([<vlist,...>])    =>  WVW_GETXYFROMROWCOL     (NIL [, <vlist>])
#xtranslate WVT_GETFONTINFO         ([<vlist,...>])    =>  WVW_GETFONTINFO         (NIL [, <vlist>])
#xtranslate WVT_MINIMIZE            ([<vlist,...>])    =>  WVW_MINIMIZE            (NIL [, <vlist>])
#xtranslate WVT_MAXIMIZE            ([<vlist,...>])    =>  WVW_MAXIMIZE            (NIL [, <vlist>])
#xtranslate WVT_SETONTOP            ([<vlist,...>])    =>  WVW_SETONTOP            (NIL [, <vlist>])
#xtranslate WVT_SETASNORMAL         ([<vlist,...>])    =>  WVW_SETASNORMAL         (NIL [, <vlist>])
#xtranslate WVT_SAVESCREEN          ([<vlist,...>])    =>  WVW_SAVESCREEN          (NIL [, <vlist>])
#xtranslate WVT_RESTSCREEN          ([<vlist,...>])    =>  WVW_RESTSCREEN          (NIL [, <vlist>])
#xtranslate WVT_DRAWLABELOBJ        ([<vlist,...>])    =>  WVW_DRAWLABELOBJ        (NIL [, <vlist>])
#xtranslate WVT_DRAWTOOLBUTTONSTATE ([<vlist,...>])    =>  WVW_DRAWTOOLBUTTONSTATE (NIL [, <vlist>])
#xtranslate WVT_DRAWSCROLLBUTTON    ([<vlist,...>])    =>  WVW_DRAWSCROLLBUTTON    (NIL [, <vlist>])
#xtranslate WVT_DRAWSCROLLTHUMBVERT ([<vlist,...>])    =>  WVW_DRAWSCROLLTHUMBVERT (NIL [, <vlist>])
#xtranslate WVT_DRAWSCROLLTHUMBHORZ ([<vlist,...>])    =>  WVW_DRAWSCROLLTHUMBHORZ (NIL [, <vlist>])
#xtranslate WVT_DRAWSHADEDRECT      ([<vlist,...>])    =>  WVW_DRAWSHADEDRECT      (NIL [, <vlist>])
#xtranslate WVT_DRAWTEXTBOX         ([<vlist,...>])    =>  WVW_DRAWTEXTBOX         (NIL [, <vlist>])
#xtranslate WVT_DRAWPROGRESSBAR     ([<vlist,...>])    =>  WVW_DRAWPROGRESSBAR     (NIL [, <vlist>])
#xtranslate WVT_TRACKPOPUPMENU      ([<vlist,...>])    =>  WVW_TRACKPOPUPMENU      (NIL [, <vlist>])
#xtranslate WVT_GETMENU             ([<vlist,...>])    =>  WVW_GETMENU             (NIL [, <vlist>])
#xtranslate WVT_SHOWWINDOW          ([<vlist,...>])    =>  WVW_SHOWWINDOW          (NIL [, <vlist>])
#xtranslate WVT_UPDATEWINDOW        ([<vlist,...>])    =>  WVW_UPDATEWINDOW        (NIL [, <vlist>])
#xtranslate WVT_DRAWBOXGET          ([<vlist,...>])    =>  WVW_DRAWBOXGET          (NIL [, <vlist>])
#xtranslate WVT_DRAWBOXRAISED       ([<vlist,...>])    =>  WVW_DRAWBOXRAISED       (NIL [, <vlist>])
#xtranslate WVT_DRAWBOXRECESSED     ([<vlist,...>])    =>  WVW_DRAWBOXRECESSED     (NIL [, <vlist>])
#xtranslate WVT_DRAWBOXGROUP        ([<vlist,...>])    =>  WVW_DRAWBOXGROUP        (NIL [, <vlist>])
#xtranslate WVT_DRAWBOXGROUPRAISED  ([<vlist,...>])    =>  WVW_DRAWBOXGROUPRAISED  (NIL [, <vlist>])
#xtranslate WVT_DRAWIMAGE           ([<vlist,...>])    =>  WVW_DRAWIMAGE           (NIL [, <vlist>])
#xtranslate WVT_DRAWLABEL           ([<vlist,...>])    =>  WVW_DRAWLABEL           (NIL [, <vlist>])
#xtranslate WVT_DRAWOUTLINE         ([<vlist,...>])    =>  WVW_DRAWOUTLINE         (NIL [, <vlist>])
#xtranslate WVT_DRAWLINE            ([<vlist,...>])    =>  WVW_DRAWLINE            (NIL [, <vlist>])
#xtranslate WVT_DRAWELLIPSE         ([<vlist,...>])    =>  WVW_DRAWELLIPSE         (NIL [, <vlist>])
#xtranslate WVT_DRAWRECTANGLE       ([<vlist,...>])    =>  WVW_DRAWRECTANGLE       (NIL [, <vlist>])
#xtranslate WVT_FILLRECTANGLE       ([<vlist,...>])    =>  WVW_FILLRECTANGLE       (NIL [, <vlist>])
#xtranslate WVT_DRAWCOLORRECT       ([<vlist,...>])    =>  WVW_DRAWCOLORRECT       (NIL [, <vlist>])
#xtranslate WVT_DRAWROUNDRECT       ([<vlist,...>])    =>  WVW_DRAWROUNDRECT       (NIL [, <vlist>])
#xtranslate WVT_DRAWFOCUSRECT       ([<vlist,...>])    =>  WVW_DRAWFOCUSRECT       (NIL [, <vlist>])
#xtranslate WVT_DRAWGRIDHORZ        ([<vlist,...>])    =>  WVW_DRAWGRIDHORZ        (NIL [, <vlist>])
#xtranslate WVT_DRAWGRIDVERT        ([<vlist,...>])    =>  WVW_DRAWGRIDVERT        (NIL [, <vlist>])
#xtranslate WVT_DRAWBUTTON          ([<vlist,...>])    =>  WVW_DRAWBUTTON          (NIL [, <vlist>])

#xtranslate WVT_SETMOUSEPOS         ([<vlist,...>])    =>  WVW_SETMOUSEPOS         (NIL [, <vlist>])

/* in gtwvt no pending rect is reflected as {0,0,0,0}
   in gtwvw no pending rect is reflected as {y1,x1,y2,x2} where y1 > y2 or x1 > x2
   thus we need some temporary var to check this exception
*/
#xtranslate WVT_GETPAINTRECT        ([<vlist,...>])    =>  ( _wvwtemp_ := WVW_GETPAINTRECT( NIL [, <vlist>])  ;
                                                           , iif(_wvwtemp_\[1\] > _wvwtemp_\[3\] .OR. _wvwtemp_\[2\] > _wvwtemp_\[4\], ;
                                                                 { 0, 0, 0, 0 }, _wvwtemp_ ) )

#xtranslate WVT_SETPOINTER          ([<vlist,...>])    =>  WVW_SETPOINTER          (NIL [, <vlist>])
#xtranslate WVT_DRAWPICTURE         ([<vlist,...>])    =>  WVW_DRAWPICTURE         (NIL [, <vlist>])
#xtranslate WVT_DRAWLABELEX         ([<vlist,...>])    =>  WVW_DRAWLABELEX         (NIL [, <vlist>])
#xtranslate WVT_DRAWLINEEX          ([<vlist,...>])    =>  WVW_DRAWLINEEX          (NIL [, <vlist>])
#xtranslate WVT_DRAWOUTLINEEX       ([<vlist,...>])    =>  WVW_DRAWOUTLINEEX       (NIL [, <vlist>])
#xtranslate WVT_MESSAGEBOX          ([<vlist,...>])    =>  WVW_MESSAGEBOX          (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPACTIVE    ([<vlist,...>])    =>  WVW_SETTOOLTIPACTIVE    (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIP          ([<vlist,...>])    =>  WVW_SETTOOLTIP          (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPTEXT      ([<vlist,...>])    =>  WVW_SETTOOLTIPTEXT      (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPMARGIN    ([<vlist,...>])    =>  WVW_SETTOOLTIPMARGIN    (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPWIDTH     ([<vlist,...>])    =>  WVW_SETTOOLTIPWIDTH     (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPBKCOLOR   ([<vlist,...>])    =>  WVW_SETTOOLTIPBKCOLOR   (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPTEXTCOLOR ([<vlist,...>])    =>  WVW_SETTOOLTIPTEXTCOLOR (NIL [, <vlist>])
#xtranslate WVT_SETTOOLTIPTITLE     ([<vlist,...>])    =>  WVW_SETTOOLTIPTITLE     (NIL [, <vlist>])
#xtranslate WVT_GETTOOLTIPWIDTH     ([<vlist,...>])    =>  WVW_GETTOOLTIPWIDTH     (NIL [, <vlist>])
#xtranslate WVT_GETTOOLTIPBKCOLOR   ([<vlist,...>])    =>  WVW_GETTOOLTIPBKCOLOR   (NIL [, <vlist>])
#xtranslate WVT_GETTOOLTIPTEXTCOLOR ([<vlist,...>])    =>  WVW_GETTOOLTIPTEXTCOLOR (NIL [, <vlist>])

/********************************************************************
Timer functions. WARNING: WVT implementation is slightly different.
*********************************************************************/

#xtranslate WVT_SETTIMER   ([<vlist,...>])    =>  WVW_SETTIMER  (NIL [, <vlist>])
#xtranslate WVT_KILLTIMER  ([<vlist,...>])    =>  WVW_KILLTIMER (NIL [, <vlist>])

/********************************************************************
WVW_DRAWSTATUSBAR is for compatibility only.
Recommended to use WVW_SBxxxx functions instead.
*********************************************************************/
#xtranslate WVT_DRAWSTATUSBAR ([<vlist,...>])          =>  WVW_DRAWSTATUSBAR (NIL [, <vlist>])

/********************************************************************
Native Statusbar functions. Currently none in WVT.
WVT uses different approach (WVT_DRAWSTATUSBAR)
*********************************************************************/

//WVW_SBCREATE
//WVW_SBDESTROY
//WVW_SBADDPART
//WVW_SBREFRESH
//WVW_SBSETTEXT
//WVW_SBGETTEXT
//WVW_SBGETPARTS

/********************************************************************
Toolbar functions. Currently none in WVT.
WVT uses different approach.
*********************************************************************/

//WVW_TBCREATE
//WVW_TBADDBUTTON
//WVW_TBBUTTONCOUNT
//WVW_TBDELBUTTON
//WVW_TBENABLEBUTTON
//WVW_TBDESTROY

/********************************************************************
Scrollbar functions. Currently none in WVT.
WVT uses different approach.
*********************************************************************/

//WVW_XBCREATE
//WVW_XBDESTROY
//WVW_XBUPDATE
//WVW_XBENABLE


/********************************************************************
Line Spacing. Currently none in WVT.
*********************************************************************/
//WVW_SETLINESPACING

/********************************************************************/
/********************************************************************
PART-3: RESERVED FUNCTION NAMES ("callback" prg functions, called by gtwvw)
*********************************************************************/
/********************************************************************/

/*
  Notes:

  Generally, each function is supplied additional nWinNum parameter at the front.
  nWinNum is 0-based (with 0 being the Main Window).

  If you have single (main) window, no further change is needed.
  However, once you open a second window you you should decide what to do
  with nWinNum parameter in these callback functions.

  Typically your WVT_xxx function will need adjustment like below:

      FUNCTION WVT_xxx(...)
         LOCAL nOldWin := wvw_nsetcurwindow( nWinNum ) //<-- add this

         ...existing code...

         wvw_nsetcurwindow( nOldWin ) //<--add this
         RETURN NIL

  Although the above may be enough, each individual function may need careful review
  to make sure it follows gtwvw convention. For example, if you have multiple
  exit points in that function.

  IMPORTANT NOTES ON MainCoord Mode:

  Using wvw_nsetcurwindow() in MainCoord Mode may not be appropriate, because
  current window is reset to 0 upon returning from a standard GT function
  (DispOut(), QQout(), DevOut(), etc.). Remember that these functions
  may also be called indirectly through many other functions/commands
  (Alert(), Achoice(), GET, etc.).

  You may want to replace wvw_nsetcurwindow() with your own function in this case.

*/

#xtranslate FUNCTION WVT_PAINT([<vlist,...>]) => FUNCTION WVW_PAINT( nWinNum [,<vlist>] )
#xtranslate PROCEDURE WVT_PAINT([<vlist,...>]) => PROCEDURE WVW_PAINT( nWinNum [,<vlist>] )
#xtranslate WVT_PAINT([<vlist,...>]) => WVW_PAINT( NIL [,<vlist>] )

#xtranslate FUNCTION WVT_SETFOCUS([<vlist,...>])  => FUNCTION WVW_SETFOCUS( nWinNum [,<vlist>] )
#xtranslate PROCEDURE WVT_SETFOCUS([<vlist,...>])  => PROCEDURE WVW_SETFOCUS( nWinNum [,<vlist>] )
#xtranslate WVT_SETFOCUS([<vlist,...>]) => WVW_SETFOCUS( NIL [,<vlist>] )

#xtranslate FUNCTION WVT_KILLFOCUS([<vlist,...>]) => FUNCTION WVW_KILLFOCUS( nWinNum [,<vlist>] )
#xtranslate PROCEDURE WVT_KILLFOCUS([<vlist,...>]) => PROCEDURE WVW_KILLFOCUS( nWinNum [,<vlist>] )
#xtranslate WVT_KILLFOCUS([<vlist,...>]) => WVW_KILLFOCUS( NIL [,<vlist>] )

#xtranslate FUNCTION WVT_MOUSE([<vlist,...>]) => FUNCTION WVW_MOUSE( nWinNum [,<vlist>] )
#xtranslate PROCEDURE WVT_MOUSE([<vlist,...>]) => PROCEDURE WVW_MOUSE( nWinNum [,<vlist>] )
#xtranslate WVT_MOUSE([<vlist,...>]) => WVW_MOUSE( NIL [,<vlist>] )

#xtranslate FUNCTION WVT_TIMER() => FUNCTION WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )
#xtranslate PROCEDURE WVT_TIMER() => PROCEDURE WVW_TIMER( nWinNum, hWnd, message, wParam, lParam )
/* Currently WVT_TIMER is never called by GTWVT.
   There should never be any existing usage of this function.
*/

/********************************************************************
WVT_MENUSELECT does not exist in WVT.
*********************************************************************/
//WVT_MENUSELECT
