/*
 * $Id$
 */

/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   Mapping of gtwvt functions and their coresponding ones in gtwvw.

   header file to be included in your gtwvt program if you
   wish to link it with gtwvw.

   WARNING:
   This mapping is made globally. Should you find any error(s) please let me know.
*/

MEMVAR _wvwtemp_

/* PART-1: WINDOW INDEPENDENT (the same parameter list) */
/* ==================================================== */

#xtranslate Wvt_CreateMenu         ( [<vlist,...>] )  => wvw_CreateMenu         ( [<vlist>] )
#xtranslate Wvt_CreatePopupMenu    ( [<vlist,...>] )  => wvw_CreatePopupMenu    ( [<vlist>] )
#xtranslate Wvt_AppendMenu         ( [<vlist,...>] )  => wvw_AppendMenu         ( [<vlist>] )
#xtranslate Wvt_DeleteMenu         ( [<vlist,...>] )  => wvw_DeleteMenu         ( [<vlist>] )
#xtranslate Wvt_DestroyMenu        ( [<vlist,...>] )  => wvw_DestroyMenu        ( [<vlist>] )
#xtranslate Wvt_EnableMenuItem     ( [<vlist,...>] )  => wvw_EnableMenuItem     ( [<vlist>] )
#xtranslate Wvt_GetScreenWidth     ( [<vlist,...>] )  => wvw_GetScreenWidth     ( [<vlist>] )
#xtranslate Wvt_GetScreenHeight    ( [<vlist,...>] )  => wvw_GetScreenHeight    ( [<vlist>] )
#xtranslate Wvt_SetAltF4Close      ( [<vlist,...>] )  => wvw_SetAltF4Close      ( [<vlist>] )
#xtranslate Wvt_GetRGBColor        ( [<vlist,...>] )  => wvw_GetRGBColor        ( [<vlist>] )
#xtranslate Wvt_GetClipboard       ( [<vlist,...>] )  => wvw_GetClipboard       ( [<vlist>] )
#xtranslate Wvt_SetClipboard       ( [<vlist,...>] )  => wvw_SetClipboard       ( [<vlist>] )
#xtranslate Wvt_PasteFromClipboard ( [<vlist,...>] )  => wvw_PasteFromClipboard ( [<vlist>] )
#xtranslate Wvt_Keyboard           ( [<vlist,...>] )  => wvw_Keyboard           ( [<vlist>] )
#xtranslate Wvt_IsLButtonPressed   ( [<vlist,...>] )  => wvw_IsLButtonPressed   ( [<vlist>] )
#xtranslate Wvt_GetPalette         ( [<vlist,...>] )  => wvw_GetPalette         ( [<vlist>] )
#xtranslate Wvt_SetPalette         ( [<vlist,...>] )  => wvw_SetPalette         ( [<vlist>] )
#xtranslate Wvt_CreateFont         ( [<vlist,...>] )  => wvw_CreateFont         ( [<vlist>] )
#xtranslate Wvt_GetCursorPos       ( [<vlist,...>] )  => wvw_GetCursorPos       ( [<vlist>] )
#xtranslate Wvt_ChooseFont         ( [<vlist,...>] )  => wvw_ChooseFont         ( [<vlist>] )
#xtranslate Wvt_ChooseColor        ( [<vlist,...>] )  => wvw_ChooseColor        ( [<vlist>] )
#xtranslate Wvt_LoadPicture        ( [<vlist,...>] )  => wvw_LoadPicture        ( [<vlist>] )
#xtranslate Wvt_LoadFont           ( [<vlist,...>] )  => wvw_LoadFont           ( [<vlist>] )
#xtranslate Wvt_LoadPen            ( [<vlist,...>] )  => wvw_LoadPen            ( [<vlist>] )
#xtranslate Wvt_SetPen             ( [<vlist,...>] )  => wvw_SetPen             ( [<vlist>] )
#xtranslate Wvt_SetBrush           ( [<vlist,...>] )  => wvw_SetBrush           ( [<vlist>] )

#xtranslate Wvt_CreateDialogDynamic( [<vlist,...>] )  => wvw_CreateDialogDynamic( [<vlist>] )
#xtranslate Wvt_CreateDialogModal  ( [<vlist,...>] )  => wvw_CreateDialogModal  ( [<vlist>] )
#xtranslate Wvt__MakeDlgTemplate   ( [<vlist,...>] )  => wvw__MakeDlgTemplate   ( [<vlist>] )
#xtranslate Wvt_LBAddString        ( [<vlist,...>] )  => wvw_LBAddString        ( [<vlist>] )
#xtranslate Wvt_LBSetCurSel        ( [<vlist,...>] )  => wvw_LBSetCurSel        ( [<vlist>] )
#xtranslate Wvt_CBAddString        ( [<vlist,...>] )  => wvw_cbAddString        ( [<vlist>] )
#xtranslate Wvt_CBSetCurSel        ( [<vlist,...>] )  => wvw_CBSetCurSel        ( [<vlist>] )
#xtranslate Wvt_DlgSetIcon         ( [<vlist,...>] )  => wvw_DlgSetIcon         ( [<vlist>] )


/* These functions do not exist in WVT. */
//Wvt_SetVertCaret       ( [<vlist,...>] )  => wvw_SetVertCaret       ( [<vlist>] )
//Wvt_SetDefLineSpacing  ( [<vlist,...>] )  => wvw_SetDefLineSpacing  ( [<vlist>] )

/* PART-2: WINDOW DEPENDENT (additional nWinNum parameter) */
/* ======================================================= */

/*
  Notes:

  nWinNum parameter passed as NIL will be translated by gtwvw into :
    IF MainCoordMode
       Topmost Window
    ELSE
       Current Window
    ENDIF

  Since gtwvt application can't be in MainCoordMode,
  the following approach makes these functions work on current window.

  You may want to replace NIL with your own function/variable by which you can decide
  which window to direct your output to.

*/

#xtranslate Wvt_SetMenu             ( [<vlist,...>] )    =>  wvw_SetMenu             ( NIL [, <vlist>] )
#xtranslate Wvt_SetPopupMenu        ( [<vlist,...>] )    =>  wvw_SetPopupMenu        ( NIL [, <vlist>] )
#xtranslate Wvt_GetLastMenuEvent    ( [<vlist,...>] )    =>  wvw_GetLastMenuEvent    ( NIL [, <vlist>] )
#xtranslate Wvt_SetMenuKeyEvent     ( [<vlist,...>] )    =>  wvw_SetMenuKeyEvent     ( NIL [, <vlist>] )
#xtranslate Wvt_DrawMenuBar         ( [<vlist,...>] )    =>  wvw_DrawMenuBar         ( NIL [, <vlist>] )
#xtranslate WVT_SETWINDOWCENTRE     ( [<vlist,...>] )    =>  wvw_SetWindowCentre     ( NIL [, <vlist>] )
#xtranslate Wvt_ProcessMessages     ( [<vlist,...>] )    =>  wvw_ProcessMessages     ( NIL [, <vlist>] )
#xtranslate Wvt_GetTitle            ( [<vlist,...>] )    =>  wvw_GetTitle            ( NIL [, <vlist>] )
#xtranslate Wvt_InvalidateRect      ( [<vlist,...>] )    =>  wvw_InvalidateRect      ( NIL [, <vlist>] )
#xtranslate Wvt_ClientToScreen      ( [<vlist,...>] )    =>  wvw_ClientToScreen      ( NIL [, <vlist>] )
#xtranslate Wvt_SetFont             ( [<vlist,...>] )    =>  wvw_SetFont             ( NIL [, <vlist>] )
#xtranslate Wvt_SetIcon             ( [<vlist,...>] )    =>  wvw_SetIcon             ( NIL [, <vlist>] )
#xtranslate Wvt_SetTitle            ( [<vlist,...>] )    =>  wvw_SetTitle            ( NIL [, <vlist>] )
#xtranslate Wvt_SetWindowPos        ( [<vlist,...>] )    =>  wvw_SetWindowPos        ( NIL [, <vlist>] )
#xtranslate Wvt_GetWindowHandle     ( [<vlist,...>] )    =>  wvw_GetWindowHandle     ( NIL [, <vlist>] )
#xtranslate Wvt_SetCodepage         ( [<vlist,...>] )    =>  wvw_SetCodepage         ( NIL [, <vlist>] )
#xtranslate Wvt_CenterWindow        ( [<vlist,...>] )    =>  wvw_CenterWindow        ( NIL [, <vlist>] )
#xtranslate Wvt_SetMouseMove        ( [<vlist,...>] )    =>  wvw_SetMouseMove        ( NIL [, <vlist>] )
#xtranslate Wvt_GetXYFromRowCol     ( [<vlist,...>] )    =>  wvw_GetXYFromRowCol     ( NIL [, <vlist>] )
#xtranslate Wvt_GetFontInfo         ( [<vlist,...>] )    =>  wvw_GetFontInfo         ( NIL [, <vlist>] )
#xtranslate Wvt_Minimize            ( [<vlist,...>] )    =>  wvw_Minimize            ( NIL [, <vlist>] )
#xtranslate Wvt_Maximize            ( [<vlist,...>] )    =>  wvw_Maximize            ( NIL [, <vlist>] )
#xtranslate Wvt_SetOnTop            ( [<vlist,...>] )    =>  wvw_SetOnTop            ( NIL [, <vlist>] )
#xtranslate Wvt_SetAsNormal         ( [<vlist,...>] )    =>  wvw_SetAsNormal         ( NIL [, <vlist>] )
#xtranslate Wvt_SaveScreen          ( [<vlist,...>] )    =>  wvw_SaveScreen          ( NIL [, <vlist>] )
#xtranslate Wvt_RestScreen          ( [<vlist,...>] )    =>  wvw_RestScreen          ( NIL [, <vlist>] )
#xtranslate Wvt_DrawLabelObj        ( [<vlist,...>] )    =>  wvw_DrawLabelObj        ( NIL [, <vlist>] )
#xtranslate Wvt_DrawToolButtonState ( [<vlist,...>] )    =>  wvw_DrawToolButtonState ( NIL [, <vlist>] )
#xtranslate Wvt_DrawScrollButton    ( [<vlist,...>] )    =>  wvw_DrawScrollButton    ( NIL [, <vlist>] )
#xtranslate Wvt_DrawScrollThumbVert ( [<vlist,...>] )    =>  wvw_DrawScrollThumbVert ( NIL [, <vlist>] )
#xtranslate Wvt_DrawScrollThumbHorz ( [<vlist,...>] )    =>  wvw_DrawScrollThumbHorz ( NIL [, <vlist>] )
#xtranslate Wvt_DrawShadedRect      ( [<vlist,...>] )    =>  wvw_DrawShadedRect      ( NIL [, <vlist>] )
#xtranslate Wvt_DrawTextBox         ( [<vlist,...>] )    =>  wvw_DrawTextBox         ( NIL [, <vlist>] )
#xtranslate Wvt_DrawProgressBar     ( [<vlist,...>] )    =>  wvw_DrawProgressBar     ( NIL [, <vlist>] )
#xtranslate Wvt_TrackPopupMenu      ( [<vlist,...>] )    =>  wvw_TrackPopupMenu      ( NIL [, <vlist>] )
#xtranslate Wvt_GetMenu             ( [<vlist,...>] )    =>  wvw_GetMenu             ( NIL [, <vlist>] )
#xtranslate Wvt_ShowWindow          ( [<vlist,...>] )    =>  wvw_ShowWindow          ( NIL [, <vlist>] )
#xtranslate Wvt_UpdateWINDOW        ( [<vlist,...>] )    =>  wvw_UpdateWindow        ( NIL [, <vlist>] )
#xtranslate Wvt_DrawBoxGet          ( [<vlist,...>] )    =>  wvw_DrawBoxGet          ( NIL [, <vlist>] )
#xtranslate Wvt_DrawBoxRaised       ( [<vlist,...>] )    =>  wvw_DrawBoxRaised       ( NIL [, <vlist>] )
#xtranslate Wvt_DrawBoxRecessed     ( [<vlist,...>] )    =>  wvw_DrawBoxRecessed     ( NIL [, <vlist>] )
#xtranslate Wvt_DrawBoxGroup        ( [<vlist,...>] )    =>  wvw_DrawBoxGroup        ( NIL [, <vlist>] )
#xtranslate Wvt_DrawBoxGroupRaised  ( [<vlist,...>] )    =>  wvw_DrawBoxGroupRaised  ( NIL [, <vlist>] )
#xtranslate Wvt_DrawImage           ( [<vlist,...>] )    =>  wvw_DrawImage           ( NIL [, <vlist>] )
#xtranslate Wvt_DrawLabel           ( [<vlist,...>] )    =>  wvw_DrawLabel           ( NIL [, <vlist>] )
#xtranslate Wvt_DrawOutline         ( [<vlist,...>] )    =>  wvw_DrawOutline         ( NIL [, <vlist>] )
#xtranslate Wvt_DrawLine            ( [<vlist,...>] )    =>  wvw_DrawLine            ( NIL [, <vlist>] )
#xtranslate Wvt_DrawEllipse         ( [<vlist,...>] )    =>  wvw_DrawEllipse         ( NIL [, <vlist>] )
#xtranslate Wvt_DrawRectangle       ( [<vlist,...>] )    =>  wvw_DrawRectangle       ( NIL [, <vlist>] )
#xtranslate WVT_FILLRECTANGLE       ( [<vlist,...>] )    =>  wvw_FillRectangle       ( NIL [, <vlist>] )
#xtranslate Wvt_DrawColorRect       ( [<vlist,...>] )    =>  wvw_DrawColorRect       ( NIL [, <vlist>] )
#xtranslate Wvt_DrawRoundRect       ( [<vlist,...>] )    =>  wvw_DrawRoundRect       ( NIL [, <vlist>] )
#xtranslate Wvt_DrawFocusRect       ( [<vlist,...>] )    =>  wvw_DrawFocusRect       ( NIL [, <vlist>] )
#xtranslate Wvt_DrawGridHorz        ( [<vlist,...>] )    =>  wvw_DrawGridHorz        ( NIL [, <vlist>] )
#xtranslate Wvt_DrawGridVert        ( [<vlist,...>] )    =>  wvw_DrawGridVert        ( NIL [, <vlist>] )
#xtranslate Wvt_DrawButton          ( [<vlist,...>] )    =>  wvw_DrawButton          ( NIL [, <vlist>] )

#xtranslate Wvt_SetMousePos         ( [<vlist,...>] )    =>  wvw_SetMousePos         ( NIL [, <vlist>] )

/* in gtwvt no pending rect is reflected as {0,0,0,0}
   in gtwvw no pending rect is reflected as {y1,x1,y2,x2} where y1 > y2 or x1 > x2
   thus we need some temporary var to check this exception
*/
#xtranslate Wvt_GetPaintRect        ( [<vlist,...>] )    =>  ( _wvwtemp_ := wvw_GetPaintRect( NIL [, <vlist>] ), ;
                                                             iif( _wvwtemp_\[ 1 \] > _wvwtemp_\[ 3 \] .OR. _wvwtemp_\[ 2 \] > _wvwtemp_\[ 4 \], ;
                                                                  { 0, 0, 0, 0 }, _wvwtemp_ ) )

#xtranslate Wvt_SetPointer          ( [<vlist,...>] )    =>  wvw_SetPointer          ( NIL [, <vlist>] )
#xtranslate Wvt_DrawPicture         ( [<vlist,...>] )    =>  wvw_DrawPicture         ( NIL [, <vlist>] )
#xtranslate Wvt_DrawLabelEx         ( [<vlist,...>] )    =>  wvw_DrawLabelEx         ( NIL [, <vlist>] )
#xtranslate Wvt_DrawLineEx          ( [<vlist,...>] )    =>  wvw_DrawLineEx          ( NIL [, <vlist>] )
#xtranslate Wvt_DrawOutlineEx       ( [<vlist,...>] )    =>  wvw_DrawOutlineEx       ( NIL [, <vlist>] )
#xtranslate Wvt_MessageBox          ( [<vlist,...>] )    =>  wvw_MessageBox          ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipActive    ( [<vlist,...>] )    =>  wvw_SetToolTipActive    ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTip          ( [<vlist,...>] )    =>  wvw_SetToolTip          ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipText      ( [<vlist,...>] )    =>  wvw_SetToolTipText      ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipMargin    ( [<vlist,...>] )    =>  wvw_SetToolTipMargin    ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipWidth     ( [<vlist,...>] )    =>  wvw_SetToolTipWidth     ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_SetToolTipBkColor   ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipTextColor ( [<vlist,...>] )    =>  wvw_SetToolTipTextColor ( NIL [, <vlist>] )
#xtranslate Wvt_SetToolTipTitle     ( [<vlist,...>] )    =>  wvw_SetToolTipTitle     ( NIL [, <vlist>] )
#xtranslate Wvt_GetToolTipWidth     ( [<vlist,...>] )    =>  wvw_GetToolTipWidth     ( NIL [, <vlist>] )
#xtranslate Wvt_GetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_GetToolTipBkColor   ( NIL [, <vlist>] )
#xtranslate Wvt_GetToolTipTextColor ( [<vlist,...>] )    =>  wvw_GetToolTipTextColor ( NIL [, <vlist>] )

/* Timer functions. WARNING: WVT implementation is slightly different. */
#xtranslate Wvt_SetTimer   ( [<vlist,...>] )    =>  wvw_SetTimer  ( NIL [, <vlist>] )
#xtranslate Wvt_KillTimer  ( [<vlist,...>] )    =>  wvw_KillTimer ( NIL [, <vlist>] )

/* wvw_DrawStatusBar() is for compatibility only.
   Recommended to use wvw_sb*() functions instead. */
#xtranslate Wvt_DrawStatusBar ( [<vlist,...>] )          =>  wvw_DrawStatusBar ( NIL [, <vlist>] )

/* Native Statusbar functions. Currently none in WVT.
   WVT uses different approach (Wvt_DrawStatusBar) */
//wvw_sbCreate
//wvw_sbDestroy
//wvw_sbAddPart
//wvw_sbRefresh
//wvw_sbSetText
//wvw_sbGetText
//wvw_sbGetParts

/* Toolbar functions. Currently none in WVT. WVT uses different approach. */
//wvw_tbCreate
//wvw_tbAddButton
//wvw_tbButtonCount
//wvw_tbDelButton
//wvw_tbEnableButton
//wvw_tbDestroy

/* Scrollbar functions. Currently none in WVT. WVT uses different approach. */
//wvw_xbCreate
//wvw_xbDestroy
//wvw_xbUpdate
//wvw_xbEnable

/* Line Spacing. Currently none in WVT. */
//wvw_SetLineSpacing

/* PART-3: RESERVED FUNCTION NAMES ("callback" prg functions, called by gtwvw) */
/* =========================================================================== */

/*
  Notes:

  Generally, each function is supplied additional nWinNum parameter at the front.
  nWinNum is 0-based (with 0 being the Main Window).

  If you have single (main) window, no further change is needed.
  However, once you open a second window you you should decide what to do
  with nWinNum parameter in these callback functions.

  Typically your WVT_xxx function will need adjustment like below:

      FUNCTION WVT_xxx(...)
         LOCAL nOldWin := wvw_nSetCurWindow( nWinNum ) //<-- add this

         ...existing code...

         wvw_nSetCurWindow( nOldWin ) //<--add this
         RETURN NIL

  Although the above may be enough, each individual function may need careful review
  to make sure it follows gtwvw convention. For example, if you have multiple
  exit points in that function.

  IMPORTANT NOTES ON MainCoord Mode:

  Using wvw_nSetCurWindow() in MainCoord Mode may not be appropriate, because
  current window is reset to 0 upon returning from a standard GT function
  (DispOut(), QQOut(), DevOut(), etc.). Remember that these functions
  may also be called indirectly through many other functions/commands
  (Alert(), AChoice(), GET, etc.).

  You may want to replace wvw_nSetCurWindow() with your own function in this case.

*/

#xtranslate FUNCTION Wvt_Paint( [<vlist,...>] ) => FUNCTION wvw_Paint( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_Paint( [<vlist,...>] ) => PROCEDURE wvw_Paint( nWinNum [,<vlist>] )
#xtranslate Wvt_Paint( [<vlist,...>] ) => wvw_Paint( NIL [,<vlist>] )

#xtranslate FUNCTION Wvt_SetFocus( [<vlist,...>] )  => FUNCTION wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_SetFocus( [<vlist,...>] )  => PROCEDURE wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate Wvt_SetFocus( [<vlist,...>] ) => wvw_SetFocus( NIL [,<vlist>] )

#xtranslate FUNCTION Wvt_KillFocus( [<vlist,...>] ) => FUNCTION wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_KillFocus( [<vlist,...>] ) => PROCEDURE wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate Wvt_KillFocus( [<vlist,...>] ) => wvw_KillFocus( NIL [,<vlist>] )

#xtranslate FUNCTION Wvt_Mouse( [<vlist,...>] ) => FUNCTION wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_Mouse( [<vlist,...>] ) => PROCEDURE wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate Wvt_Mouse( [<vlist,...>] ) => wvw_Mouse( NIL [,<vlist>] )

#xtranslate FUNCTION Wvt_Timer() => FUNCTION wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
#xtranslate PROCEDURE Wvt_Timer() => PROCEDURE wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
/* Currently Wvt_Timer() is never called by GTWVT.
   There should never be any existing usage of this function.
*/

/* Wvt_MenuSelect does not exist in WVT. */
//Wvt_MenuSelect
