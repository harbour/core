/*
 * $Id$
 */

/*
 * Demo program to show how to make use of WVW_INPUTFOCUS feature
 * of GTWVW
 *
 * Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 *
 */

#include "inkey.ch"
#include "setcurs.ch"

//MessageBox() Flags (from winuser.h)
#define MB_OK                0
#define MB_OKCANCEL          1
#define MB_ABORTRETRYIGNORE  2
#define MB_YESNOCANCEL       3
#define MB_YESNO             4
#define MB_RETRYCANCEL       5

//MessageBox() Icons (from winuser.h)
#define MB_ICONHAND          16 //0x00000010L
#define MB_ICONQUESTION      32 //0x00000020L
#define MB_ICONEXCLAMATION   48 //0x00000030L
#define MB_ICONASTERISK      64 //0x00000040L

// icon indexes for standard bitmap (from commctrl.h)
#define STD_DELETE              5
#define STD_FILENEW             6

// icon indexes for standard view bitmap
#define VIEW_PARENTFOLDER       8

//our command ids
#define IDM_OPENWIN     100
#define IDM_CLOSEWIN    101
#define IDM_ARRANGEWIN  102

//maximum windows opened
#define _MAX_WINNUM  10

//array of codeblock
static s_akeyhandlers[_MAX_WINNUM]

proc main
local ch
   if !setmode(25,80)
      wvw_messagebox(0, "Cannot set to (25,80) screen", "Warning", MB_OK+MB_ICONEXCLAMATION)
   endif
   setcolor("W*/N+")
   CLS
   setcursor(SC_NONE)
   @ 0,0 say padc("This will demonstrate how to handle input on non-topmost window", maxcol()+1)
   @ 1,0 say padc("(Sorry that currently caret is shown on topmost window only)", maxcol()+1)

   CreateToolbar(0)
   CreateStatusbar(0)

   ch := inkey(0)
   do while ch!=K_ESC
      if ch==wvw_setMenuKeyEvent(0)
         MenuAction(0, wvw_GetLastMenuEvent(0))
      endif
      ch := inkey(0)
   enddo

   wvw_messagebox(0, "Thanks for trying this program", "Goodbye", MB_OK)

   * let toolbar and statusbar be autodestroyed
return //main

static function CreateToolbar(nWinNum)
//for toolbar:
local nSysBitmap := 1     //0:none 1:small 2:large
local lDisplayText := .f. //text will be displayed as tooltip instead
local hWndTB
local ldefault

   wvw_tbdestroy(nWinNum)   //just in case

   hWndTB := wvw_tbcreate(nWinNum, lDisplayText, NIL, nSysBitmap)

   if hWndTB==0
      wvw_messagebox(nWinNum, "FAILED to create toolbar", "Error", MB_OK+MB_ICONEXCLAMATION)
      return .f.
   endif

   /* using system std & view bitmaps */
   wvw_tbAddButton(nWinNum, IDM_OPENWIN,  STD_FILENEW, "Open a new typewriter window", 1 /*system std bitmap*/)
   wvw_tbAddButton(nWinNum, IDM_CLOSEWIN, STD_DELETE, "Close last window", 1 /*system std bitmap*/)
   wvw_tbAddButton(nWinNum, IDM_ARRANGEWIN, VIEW_PARENTFOLDER, "Reposition all windows", 2 /*system view bitmap*/)
return .t.  //CreateToolbar()

static function CreateStatusbar(nWinNum)
local hWndSB
local ldefault

   wvw_sbdestroy(nWinNum)   //just in case
   hWndSB := wvw_sbcreate(nWinNum)
   if hWndSB==0
      wvw_messagebox(nWinNum, "FAILED to create statusbar", "Error", MB_OK+MB_ICONEXCLAMATION)
      return .f.
   endif
return .t. //CreateStatusbar()

static function MenuAction(nWinNum, nCommand)
* Handle Menu/Toolbar actions

   do case
      case nCommand==IDM_OPENWIN
         OpenNewWindow()
      case nCommand==IDM_CLOSEWIN
         CloseLastWindow()
      case nCommand==IDM_ARRANGEWIN
         wvw_xReposWindow()
      otherwise
         wvw_messagebox(nWinNum, "Unknown menu command", "Internal Error", MB_OK+MB_ICONEXCLAMATION)
   endcase

return NIL

static function OpenNewWindow()
* opens a new typewriter window
local nWinNum := wvw_nNumWindows()
local ctitle, nrow1, ncol1, nrow2, ncol2
local ch

   if nWinNum > _MAX_WINNUM
      wvw_messagebox(nWinNum-1, "Sorry, I don't think you can handle that many of windows :-)",;
                                "Sorry", MB_OK+MB_ICONASTERISK)
      return .f.
   endif

   * prepare titles and coordinates
   ctitle := "Win #" + alltrim(str(nWinNum))
   nrow1  := 4 + (nWinNum-1)
   ncol1  := 1 + (nWinNum-1)*3
   nrow2  := WinMaxRow(0)-_MAX_WINNUM+1 + (nWinNum-1)
   ncol2  := WinMaxCol(0)-(_MAX_WINNUM+1)*3 + (nWinNum-1)*3

   * open a window whose parent is Main Window
   setcolor("W+/N")
   if wvw_nOpenWindow(ctitle, nrow1, ncol1, nrow2, ncol2, NIL, 0) != nWinNum
      * currently wvw_nOpenWindow() will always return sequentially numbered window
      wvw_messagebox(0, "Something horrible has happened, program aborted",;
                        "Internal Error", MB_OK+MB_ICONHAND)
      quit
   endif
   wvw_noclose(nWinNum)  //disable close button

   * assign the key handler for previous window
   if nWinNum > 1
      s_akeyhandlers[nWinNum-1] := {|n, ch| KeyHandler(n, ch)}
   endif

   * then echoing user input, until user press ESC
   setcursor(SC_NORMAL)
   ch := inkey(0)
   do while ch!=K_ESC
      typing(ch)
      ch := inkey(0)
   enddo

   * close current window
   wvw_lclosewindow()

   * release keyhandler for previous window, we're going back there
   if nWinNum > 1
      s_akeyhandlers[nWinNum-1] := NIL
   elseif nWinNum==1
      setcursor(SC_NONE)
   endif
return .t.//OpenNewWindow()

static function CloseLastWindow()
* closes the last window. If no window left, Main Window will be closed too.
* Closing is done indirectly by stuffing K_ESC into kbd buffer of the
* designated window.
local nWinNum := wvw_nNumWindows()-1
   wvw_nSetCurWindow(nWinNum)
   __keyboard(K_ESC)
return NIL //CloseLastWindow()

static function KeyHandler(nWinNum, ch)
local nOldWin := wvw_nSetCurWindow(nWinNum)
   typing(ch)
   wvw_nSetCurWindow(nOldWin)
return NIL //KeyHandler()

static function typing(ch)
   if ch>=0 .and. ch<=255
      ?? chr(ch)
      if ch==K_ENTER
         ?? chr(10)
      elseif ch==K_BS
         ?? " "+chr(K_BS)
      endif
   endif
return NIL

//from winuser.h
#define WM_COMMAND                      0x0111
#define WM_CHAR                         0x0102

/*
 * WVW_INPUTFOCUS() is a special, callback function
 * This function will be called by GTWVW everytime input occurs on
 * non-topmost window.
 * This includes Menu, Toolbar, Pushbutton, Scrollbar, or plain keyboard
 * and mouse inputs.
 *
 * Parameter passed to this function is somewhat 'raw', so their handling
 * is a bit tricky.
 *
 * This function should return .t. if it has handled the event,
 * otherwise return .f. to sign GTWVW that the input is considered invalid.
 *
 */

function WVW_INPUTFOCUS(nWinNum, hWnd, message, wParam, lParam)
local wParamLow := WVW_LOWORD(wParam)
local wParamHi := WVW_HIWORD(wParam)
local nCommand, ch
//local cdebug

   * did user perform a menu/toolbar action on Main Window?
   if message==WM_COMMAND .and. nWinNum==0  //menu,toolbar,pushbutton
      nCommand := wParamLow
      MenuAction(0, nCommand)
      return .t.
   endif

   * other types of input on main window is not handled here
   if nWinNum==0
      return .f.
   endif

   * now we handle input on other non-topmost windows

   * is it a pushbutton action?
   * (TODO: create a sample of pushbutton event here)

   do case
      case message==WM_CHAR
         ch := wParam
         eval( s_akeyhandlers[nWinNum], nWinNum, ch )
         return .t.
      otherwise
         * let it be ignored
         return .t.
   endcase

   /*
   cdebug := "Sorry we can't handle this event:" + ceoln() +;
             "nWinNum == " + alltrim(str(nWinNum)) + ceoln() +;
             "message == " + alltrim(str(message)) + ceoln() +;
             "wParam == " + alltrim(str(wParam)) + ceoln() +;
             "wParamLow == " + alltrim(str(wParamLow)) + ceoln() +;
             "wParamHi == " + alltrim(str(wParamHi))

   wvw_messagebox(0, cdebug, "Debug", MB_OK)
   */

return .f.//WVW_INPUTFOCUS()

//********************************************************************
// SUPPORTING FUNCTIONS
//********************************************************************

static function winMaxRow(nWinNum)
* returns maxrow() of window nWinNum
local nOldWin := wvw_nsetCurWindow(nWinNum)
local nmaxrow := maxrow()
   wvw_nSetCurWindow(nOldWin)
return nmaxrow

static function winMaxCol(nWinNum)
* returns maxCol() of window nWinNum
local nOldWin := wvw_nsetCurWindow(nWinNum)
local nmaxCol := maxCol()
   wvw_nSetCurWindow(nOldWin)
return nmaxCol

static function ceoln()
return chr(13)+chr(10)
