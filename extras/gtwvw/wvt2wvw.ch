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

#xtranslate Wvt_SetMenu             ( [<vlist,...>] )    =>  wvw_SetMenu             ( [, <vlist>] )
#xtranslate Wvt_SetPopupMenu        ( [<vlist,...>] )    =>  wvw_SetPopupMenu        ( [, <vlist>] )
#xtranslate Wvt_GetLastMenuEvent    ( [<vlist,...>] )    =>  wvw_GetLastMenuEvent    ( [, <vlist>] )
#xtranslate Wvt_SetMenuKeyEvent     ( [<vlist,...>] )    =>  wvw_SetMenuKeyEvent     ( [, <vlist>] )
#xtranslate Wvt_DrawMenuBar         ( [<vlist,...>] )    =>  wvw_DrawMenuBar         ( [, <vlist>] )
#xtranslate WVT_SETWINDOWCENTRE     ( [<vlist,...>] )    =>  wvw_SetWindowCentre     ( [, <vlist>] )
#xtranslate Wvt_ProcessMessages     ( [<vlist,...>] )    =>  wvw_ProcessMessages     ( [, <vlist>] )
#xtranslate Wvt_GetTitle            ( [<vlist,...>] )    =>  wvw_GetTitle            ( [, <vlist>] )
#xtranslate Wvt_InvalidateRect      ( [<vlist,...>] )    =>  wvw_InvalidateRect      ( [, <vlist>] )
#xtranslate Wvt_ClientToScreen      ( [<vlist,...>] )    =>  wvw_ClientToScreen      ( [, <vlist>] )
#xtranslate Wvt_SetFont             ( [<vlist,...>] )    =>  wvw_SetFont             ( [, <vlist>] )
#xtranslate Wvt_SetIcon             ( [<vlist,...>] )    =>  wvw_SetIcon             ( [, <vlist>] )
#xtranslate Wvt_SetTitle            ( [<vlist,...>] )    =>  wvw_SetTitle            ( [, <vlist>] )
#xtranslate Wvt_SetWindowPos        ( [<vlist,...>] )    =>  wvw_SetWindowPos        ( [, <vlist>] )
#xtranslate Wvt_GetWindowHandle     ( [<vlist,...>] )    =>  wvw_GetWindowHandle     ( [, <vlist>] )
#xtranslate Wvt_SetCodepage         ( [<vlist,...>] )    =>  wvw_SetCodepage         ( [, <vlist>] )
#xtranslate Wvt_CenterWindow        ( [<vlist,...>] )    =>  wvw_CenterWindow        ( [, <vlist>] )
#xtranslate Wvt_SetMouseMove        ( [<vlist,...>] )    =>  wvw_SetMouseMove        ( [, <vlist>] )
#xtranslate Wvt_GetXYFromRowCol     ( [<vlist,...>] )    =>  wvw_GetXYFromRowCol     ( [, <vlist>] )
#xtranslate Wvt_GetFontInfo         ( [<vlist,...>] )    =>  wvw_GetFontInfo         ( [, <vlist>] )
#xtranslate Wvt_Minimize            ( [<vlist,...>] )    =>  wvw_Minimize            ( [, <vlist>] )
#xtranslate Wvt_Maximize            ( [<vlist,...>] )    =>  wvw_Maximize            ( [, <vlist>] )
#xtranslate Wvt_SetOnTop            ( [<vlist,...>] )    =>  wvw_SetOnTop            ( [, <vlist>] )
#xtranslate Wvt_SetAsNormal         ( [<vlist,...>] )    =>  wvw_SetAsNormal         ( [, <vlist>] )
#xtranslate Wvt_SaveScreen          ( [<vlist,...>] )    =>  wvw_SaveScreen          ( [, <vlist>] )
#xtranslate Wvt_RestScreen          ( [<vlist,...>] )    =>  wvw_RestScreen          ( [, <vlist>] )
#xtranslate Wvt_DrawLabelObj        ( [<vlist,...>] )    =>  wvw_DrawLabelObj        ( [, <vlist>] )
#xtranslate Wvt_DrawToolButtonState ( [<vlist,...>] )    =>  wvw_DrawToolButtonState ( [, <vlist>] )
#xtranslate Wvt_DrawScrollButton    ( [<vlist,...>] )    =>  wvw_DrawScrollButton    ( [, <vlist>] )
#xtranslate Wvt_DrawScrollThumbVert ( [<vlist,...>] )    =>  wvw_DrawScrollThumbVert ( [, <vlist>] )
#xtranslate Wvt_DrawScrollThumbHorz ( [<vlist,...>] )    =>  wvw_DrawScrollThumbHorz ( [, <vlist>] )
#xtranslate Wvt_DrawShadedRect      ( [<vlist,...>] )    =>  wvw_DrawShadedRect      ( [, <vlist>] )
#xtranslate Wvt_DrawTextBox         ( [<vlist,...>] )    =>  wvw_DrawTextBox         ( [, <vlist>] )
#xtranslate Wvt_DrawProgressBar     ( [<vlist,...>] )    =>  wvw_DrawProgressBar     ( [, <vlist>] )
#xtranslate Wvt_TrackPopupMenu      ( [<vlist,...>] )    =>  wvw_TrackPopupMenu      ( [, <vlist>] )
#xtranslate Wvt_GetMenu             ( [<vlist,...>] )    =>  wvw_GetMenu             ( [, <vlist>] )
#xtranslate Wvt_ShowWindow          ( [<vlist,...>] )    =>  wvw_ShowWindow          ( [, <vlist>] )
#xtranslate Wvt_UpdateWINDOW        ( [<vlist,...>] )    =>  wvw_UpdateWindow        ( [, <vlist>] )
#xtranslate Wvt_DrawBoxGet          ( [<vlist,...>] )    =>  wvw_DrawBoxGet          ( [, <vlist>] )
#xtranslate Wvt_DrawBoxRaised       ( [<vlist,...>] )    =>  wvw_DrawBoxRaised       ( [, <vlist>] )
#xtranslate Wvt_DrawBoxRecessed     ( [<vlist,...>] )    =>  wvw_DrawBoxRecessed     ( [, <vlist>] )
#xtranslate Wvt_DrawBoxGroup        ( [<vlist,...>] )    =>  wvw_DrawBoxGroup        ( [, <vlist>] )
#xtranslate Wvt_DrawBoxGroupRaised  ( [<vlist,...>] )    =>  wvw_DrawBoxGroupRaised  ( [, <vlist>] )
#xtranslate Wvt_DrawImage           ( [<vlist,...>] )    =>  wvw_DrawImage           ( [, <vlist>] )
#xtranslate Wvt_DrawLabel           ( [<vlist,...>] )    =>  wvw_DrawLabel           ( [, <vlist>] )
#xtranslate Wvt_DrawOutline         ( [<vlist,...>] )    =>  wvw_DrawOutline         ( [, <vlist>] )
#xtranslate Wvt_DrawLine            ( [<vlist,...>] )    =>  wvw_DrawLine            ( [, <vlist>] )
#xtranslate Wvt_DrawEllipse         ( [<vlist,...>] )    =>  wvw_DrawEllipse         ( [, <vlist>] )
#xtranslate Wvt_DrawRectangle       ( [<vlist,...>] )    =>  wvw_DrawRectangle       ( [, <vlist>] )
#xtranslate WVT_FILLRECTANGLE       ( [<vlist,...>] )    =>  wvw_FillRectangle       ( [, <vlist>] )
#xtranslate Wvt_DrawColorRect       ( [<vlist,...>] )    =>  wvw_DrawColorRect       ( [, <vlist>] )
#xtranslate Wvt_DrawRoundRect       ( [<vlist,...>] )    =>  wvw_DrawRoundRect       ( [, <vlist>] )
#xtranslate Wvt_DrawFocusRect       ( [<vlist,...>] )    =>  wvw_DrawFocusRect       ( [, <vlist>] )
#xtranslate Wvt_DrawGridHorz        ( [<vlist,...>] )    =>  wvw_DrawGridHorz        ( [, <vlist>] )
#xtranslate Wvt_DrawGridVert        ( [<vlist,...>] )    =>  wvw_DrawGridVert        ( [, <vlist>] )
#xtranslate Wvt_DrawButton          ( [<vlist,...>] )    =>  wvw_DrawButton          ( [, <vlist>] )

#xtranslate Wvt_SetMousePos         ( [<vlist,...>] )    =>  wvw_SetMousePos         ( [, <vlist>] )

/* in gtwvt no pending rect is reflected as {0,0,0,0}
   in gtwvw no pending rect is reflected as {y1,x1,y2,x2} where y1 > y2 or x1 > x2
   thus we need some temporary var to check this exception
*/
#xtranslate Wvt_GetPaintRect        ( [<vlist,...>] )    =>  ( _wvwtemp_ := wvw_GetPaintRect( [, <vlist>] ), ;
                                                             iif( _wvwtemp_\[ 1 \] > _wvwtemp_\[ 3 \] .OR. _wvwtemp_\[ 2 \] > _wvwtemp_\[ 4 \], ;
                                                                  { 0, 0, 0, 0 }, _wvwtemp_ ) )

#xtranslate Wvt_SetPointer          ( [<vlist,...>] )    =>  wvw_SetPointer          ( [, <vlist>] )
#xtranslate Wvt_DrawPicture         ( [<vlist,...>] )    =>  wvw_DrawPicture         ( [, <vlist>] )
#xtranslate Wvt_DrawLabelEx         ( [<vlist,...>] )    =>  wvw_DrawLabelEx         ( [, <vlist>] )
#xtranslate Wvt_DrawLineEx          ( [<vlist,...>] )    =>  wvw_DrawLineEx          ( [, <vlist>] )
#xtranslate Wvt_DrawOutlineEx       ( [<vlist,...>] )    =>  wvw_DrawOutlineEx       ( [, <vlist>] )
#xtranslate Wvt_MessageBox          ( [<vlist,...>] )    =>  wvw_MessageBox          ( [, <vlist>] )
#xtranslate Wvt_SetToolTipActive    ( [<vlist,...>] )    =>  wvw_SetToolTipActive    ( [, <vlist>] )
#xtranslate Wvt_SetToolTip          ( [<vlist,...>] )    =>  wvw_SetToolTip          ( [, <vlist>] )
#xtranslate Wvt_SetToolTipText      ( [<vlist,...>] )    =>  wvw_SetToolTipText      ( [, <vlist>] )
#xtranslate Wvt_SetToolTipMargin    ( [<vlist,...>] )    =>  wvw_SetToolTipMargin    ( [, <vlist>] )
#xtranslate Wvt_SetToolTipWidth     ( [<vlist,...>] )    =>  wvw_SetToolTipWidth     ( [, <vlist>] )
#xtranslate Wvt_SetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_SetToolTipBkColor   ( [, <vlist>] )
#xtranslate Wvt_SetToolTipTextColor ( [<vlist,...>] )    =>  wvw_SetToolTipTextColor ( [, <vlist>] )
#xtranslate Wvt_SetToolTipTitle     ( [<vlist,...>] )    =>  wvw_SetToolTipTitle     ( [, <vlist>] )
#xtranslate Wvt_GetToolTipWidth     ( [<vlist,...>] )    =>  wvw_GetToolTipWidth     ( [, <vlist>] )
#xtranslate Wvt_GetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_GetToolTipBkColor   ( [, <vlist>] )
#xtranslate Wvt_GetToolTipTextColor ( [<vlist,...>] )    =>  wvw_GetToolTipTextColor ( [, <vlist>] )

/* Timer functions. WARNING: WVT implementation is slightly different. */
#xtranslate Wvt_SetTimer   ( [<vlist,...>] )    =>  wvw_SetTimer  ( [, <vlist>] )
#xtranslate Wvt_KillTimer  ( [<vlist,...>] )    =>  wvw_KillTimer ( [, <vlist>] )

/* wvw_DrawStatusBar() is for compatibility only.
   Recommended to use wvw_sb*() functions instead. */
#xtranslate Wvt_DrawStatusBar ( [<vlist,...>] )          =>  wvw_DrawStatusBar ( [, <vlist>] )

/* Native Statusbar functions. Currently none in WVT.
   WVT uses different approach (Wvt_DrawStatusBar) */
//wvw_sbCreate()
//wvw_sbDestroy()
//wvw_sbAddPart()
//wvw_sbRefresh()
//wvw_sbSetText()
//wvw_sbGetText()
//wvw_sbGetParts()

/* Toolbar functions. Currently none in WVT. WVT uses different approach. */
//wvw_tbCreate()
//wvw_tbAddButton()
//wvw_tbButtonCount()
//wvw_tbDelButton()
//wvw_tbEnableButton()
//wvw_tbDestroy()

/* Scrollbar functions. Currently none in WVT. WVT uses different approach. */
//wvw_xbCreate()
//wvw_xbDestroy()
//wvw_xbUpdate()
//wvw_xbEnable()

/* Line Spacing. Currently none in WVT. */
//wvw_SetLineSpacing()

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
         LOCAL nOldWin := wvw_nSetCurWindow( nWinNum )  // <-- add this

         ...existing code...

         wvw_nSetCurWindow( nOldWin )  // <--add this
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
#xtranslate Wvt_Paint( [<vlist,...>] ) => wvw_Paint( [,<vlist>] )

#xtranslate FUNCTION Wvt_SetFocus( [<vlist,...>] )  => FUNCTION wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_SetFocus( [<vlist,...>] )  => PROCEDURE wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate Wvt_SetFocus( [<vlist,...>] ) => wvw_SetFocus( [,<vlist>] )

#xtranslate FUNCTION Wvt_KillFocus( [<vlist,...>] ) => FUNCTION wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_KillFocus( [<vlist,...>] ) => PROCEDURE wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate Wvt_KillFocus( [<vlist,...>] ) => wvw_KillFocus( [,<vlist>] )

#xtranslate FUNCTION Wvt_Mouse( [<vlist,...>] ) => FUNCTION wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate PROCEDURE Wvt_Mouse( [<vlist,...>] ) => PROCEDURE wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate Wvt_Mouse( [<vlist,...>] ) => wvw_Mouse( [,<vlist>] )

#xtranslate FUNCTION Wvt_Timer() => FUNCTION wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
#xtranslate PROCEDURE Wvt_Timer() => PROCEDURE wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
/* Currently Wvt_Timer() is never called by GTWVT.
   There should never be any existing usage of this function. */

/* Wvt_MenuSelect() does not exist in WVT. */
