/* Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
   Mapping of GTWVT functions and their coresponding ones in GTWVW.
   Header file to be included in your GTWVT program if you wish to link it with GTWVW. */

/* PART-1: WINDOW INDEPENDENT (the same parameter list) */

#xtranslate wvt_CreateMenu         ( [<vlist,...>] )  => wvw_CreateMenu         ( [<vlist>] )
#xtranslate wvt_CreatePopupMenu    ( [<vlist,...>] )  => wvw_CreatePopupMenu    ( [<vlist>] )
#xtranslate wvt_AppendMenu         ( [<vlist,...>] )  => wvw_AppendMenu         ( [<vlist>] )
#xtranslate wvt_DeleteMenu         ( [<vlist,...>] )  => wvw_DeleteMenu         ( [<vlist>] )
#xtranslate wvt_DestroyMenu        ( [<vlist,...>] )  => wvw_DestroyMenu        ( [<vlist>] )
#xtranslate wvt_EnableMenuItem     ( [<vlist,...>] )  => wvw_EnableMenuItem     ( [<vlist>] )
#xtranslate wvt_GetScreenWidth     ( [<vlist,...>] )  => wvw_GetScreenWidth     ( [<vlist>] )
#xtranslate wvt_GetScreenHeight    ( [<vlist,...>] )  => wvw_GetScreenHeight    ( [<vlist>] )
#xtranslate wvt_SetAltF4Close      ( [<vlist,...>] )  => wvw_SetAltF4Close      ( [<vlist>] )
#xtranslate wvt_GetRGBColor        ( [<vlist,...>] )  => wvw_GetRGBColor        ( [<vlist>] )
#xtranslate wvt_GetClipboard       ( [<vlist,...>] )  => wvw_GetClipboard       ( [<vlist>] )
#xtranslate wvt_SetClipboard       ( [<vlist,...>] )  => wvw_SetClipboard       ( [<vlist>] )
#xtranslate wvt_PasteFromClipboard ( [<vlist,...>] )  => wvw_PasteFromClipboard ( [<vlist>] )
#xtranslate wvt_Keyboard           ( [<vlist,...>] )  => wvw_Keyboard           ( [<vlist>] )
#xtranslate wvt_IsLButtonPressed   ( [<vlist,...>] )  => wvw_IsLButtonPressed   ( [<vlist>] )
#xtranslate wvt_GetPalette         ( [<vlist,...>] )  => wvw_GetPalette         ( [<vlist>] )
#xtranslate wvt_SetPalette         ( [<vlist,...>] )  => wvw_SetPalette         ( [<vlist>] )
#xtranslate wvt_CreateFont         ( [<vlist,...>] )  => wvw_CreateFont         ( [<vlist>] )
#xtranslate wvt_GetCursorPos       ( [<vlist,...>] )  => wvw_GetCursorPos       ( [<vlist>] )
#xtranslate wvt_ChooseFont         ( [<vlist,...>] )  => wvw_ChooseFont         ( [<vlist>] )
#xtranslate wvt_ChooseColor        ( [<vlist,...>] )  => wvw_ChooseColor        ( [<vlist>] )
#xtranslate wvt_LoadPicture        ( [<vlist,...>] )  => wvw_LoadPicture        ( [<vlist>] )
#xtranslate wvt_LoadFont           ( [<vlist,...>] )  => wvw_LoadFont           ( [<vlist>] )
#xtranslate wvt_LoadPen            ( [<vlist,...>] )  => wvw_LoadPen            ( [<vlist>] )
#xtranslate wvt_SetPen             ( [<vlist,...>] )  => wvw_SetPen             ( [<vlist>] )
#xtranslate wvt_SetBrush           ( [<vlist,...>] )  => wvw_SetBrush           ( [<vlist>] )

#xtranslate wvt_CreateDialogDynamic( [<vlist,...>] )  => wvw_CreateDialogDynamic( [<vlist>] )
#xtranslate wvt_CreateDialogModal  ( [<vlist,...>] )  => wvw_CreateDialogModal  ( [<vlist>] )
#xtranslate wvt__MakeDlgTemplate   ( [<vlist,...>] )  => wvw__MakeDlgTemplate   ( [<vlist>] )
#xtranslate wvt_lbAddString        ( [<vlist,...>] )  => wvw_lbAddString        ( [<vlist>] )
#xtranslate wvt_lbSetCurSel        ( [<vlist,...>] )  => wvw_lbSetCurSel        ( [<vlist>] )
#xtranslate wvt_cbAddString        ( [<vlist,...>] )  => wvw_cbAddString        ( [<vlist>] )
#xtranslate wvt_cbSetCurSel        ( [<vlist,...>] )  => wvw_cbSetCurSel        ( [<vlist>] )
#xtranslate wvt_DlgSetIcon         ( [<vlist,...>] )  => wvw_DlgSetIcon         ( [<vlist>] )

/* These functions do not exist in WVT. */
//wvt_SetVertCaret       ( [<vlist,...>] )  => wvw_SetVertCaret       ( [<vlist>] )
//wvt_SetDefLineSpacing  ( [<vlist,...>] )  => wvw_SetDefLineSpacing  ( [<vlist>] )

/* PART-2: WINDOW DEPENDENT (additional nWinNum parameter) */

/*
  Notes:

  nWinNum parameter passed as NIL will be translated by GTWVW into :
    IF MainCoordMode
       Topmost Window
    ELSE
       Current Window
    ENDIF

  Since GTWVT application can't be in MainCoordMode,
  the following approach makes these functions work on current window.

  You may want to replace NIL with your own function/variable by which you can decide
  which window to direct your output to.

 */

#xtranslate wvt_SetMenu             ( [<vlist,...>] )    =>  wvw_SetMenu             ( [, <vlist>] )
#xtranslate wvt_SetPopupMenu        ( [<vlist,...>] )    =>  wvw_SetPopupMenu        ( [, <vlist>] )
#xtranslate wvt_GetLastMenuEvent    ( [<vlist,...>] )    =>  wvw_GetLastMenuEvent    ( [, <vlist>] )
#xtranslate wvt_SetMenuKeyEvent     ( [<vlist,...>] )    =>  wvw_SetMenuKeyEvent     ( [, <vlist>] )
#xtranslate wvt_DrawMenuBar         ( [<vlist,...>] )    =>  wvw_DrawMenuBar         ( [, <vlist>] )
#xtranslate wvt_ProcessMessages     ( [<vlist,...>] )    =>  wvw_ProcessMessages     ( [, <vlist>] )
#xtranslate wvt_GetTitle            ( [<vlist,...>] )    =>  wvw_GetTitle            ( [, <vlist>] )
#xtranslate wvt_InvalidateRect      ( [<vlist,...>] )    =>  wvw_InvalidateRect      ( [, <vlist>] )
#xtranslate wvt_ClientToScreen      ( [<vlist,...>] )    =>  wvw_ClientToScreen      ( [, <vlist>] )
#xtranslate wvt_SetFont             ( [<vlist,...>] )    =>  wvw_SetFont             ( [, <vlist>] )
#xtranslate wvt_SetIcon             ( [<vlist,...>] )    =>  wvw_SetIcon             ( [, <vlist>] )
#xtranslate wvt_SetTitle            ( [<vlist,...>] )    =>  wvw_SetTitle            ( [, <vlist>] )
#xtranslate wvt_SetWindowPos        ( [<vlist,...>] )    =>  wvw_SetWindowPos        ( [, <vlist>] )
#xtranslate wvt_GetWindowHandle     ( [<vlist,...>] )    =>  wvw_GetWindowHandle     ( [, <vlist>] )
#xtranslate wvt_SetCodepage         ( [<vlist,...>] )    =>  wvw_SetCodepage         ( [, <vlist>] )
#xtranslate wvt_CenterWindow        ( [<vlist,...>] )    =>  wvw_CenterWindow        ( [, <vlist>] )
#xtranslate wvt_SetMouseMove        ( [<vlist,...>] )    =>  wvw_SetMouseMove        ( [, <vlist>] )
#xtranslate wvt_GetXYFromRowCol     ( [<vlist,...>] )    =>  wvw_GetXYFromRowCol     ( [, <vlist>] )
#xtranslate wvt_GetFontInfo         ( [<vlist,...>] )    =>  wvw_GetFontInfo         ( [, <vlist>] )
#xtranslate wvt_Minimize            ( [<vlist,...>] )    =>  wvw_Minimize            ( [, <vlist>] )
#xtranslate wvt_Maximize            ( [<vlist,...>] )    =>  wvw_Maximize            ( [, <vlist>] )
#xtranslate wvt_SetOnTop            ( [<vlist,...>] )    =>  wvw_SetOnTop            ( [, <vlist>] )
#xtranslate wvt_SetAsNormal         ( [<vlist,...>] )    =>  wvw_SetAsNormal         ( [, <vlist>] )
#xtranslate wvt_SaveScreen          ( [<vlist,...>] )    =>  wvw_SaveScreen          ( [, <vlist>] )
#xtranslate wvt_RestScreen          ( [<vlist,...>] )    =>  wvw_RestScreen          ( [, <vlist>] )
#xtranslate wvt_DrawLabelObj        ( [<vlist,...>] )    =>  wvw_DrawLabelObj        ( [, <vlist>] )
#xtranslate wvt_DrawToolButtonState ( [<vlist,...>] )    =>  wvw_DrawToolButtonState ( [, <vlist>] )
#xtranslate wvt_DrawScrollButton    ( [<vlist,...>] )    =>  wvw_DrawScrollButton    ( [, <vlist>] )
#xtranslate wvt_DrawScrollThumbVert ( [<vlist,...>] )    =>  wvw_DrawScrollThumbVert ( [, <vlist>] )
#xtranslate wvt_DrawScrollThumbHorz ( [<vlist,...>] )    =>  wvw_DrawScrollThumbHorz ( [, <vlist>] )
#xtranslate wvt_DrawShadedRect      ( [<vlist,...>] )    =>  wvw_DrawShadedRect      ( [, <vlist>] )
#xtranslate wvt_DrawTextBox         ( [<vlist,...>] )    =>  wvw_DrawTextBox         ( [, <vlist>] )
#xtranslate wvt_DrawProgressBar     ( [<vlist,...>] )    =>  wvw_DrawProgressBar     ( [, <vlist>] )
#xtranslate wvt_TrackPopupMenu      ( [<vlist,...>] )    =>  wvw_TrackPopupMenu      ( [, <vlist>] )
#xtranslate wvt_GetMenu             ( [<vlist,...>] )    =>  wvw_GetMenu             ( [, <vlist>] )
#xtranslate wvt_ShowWindow          ( [<vlist,...>] )    =>  wvw_ShowWindow          ( [, <vlist>] )
#xtranslate wvt_DrawBoxGet          ( [<vlist,...>] )    =>  wvw_DrawBoxGet          ( [, <vlist>] )
#xtranslate wvt_DrawBoxRaised       ( [<vlist,...>] )    =>  wvw_DrawBoxRaised       ( [, <vlist>] )
#xtranslate wvt_DrawBoxRecessed     ( [<vlist,...>] )    =>  wvw_DrawBoxRecessed     ( [, <vlist>] )
#xtranslate wvt_DrawBoxGroup        ( [<vlist,...>] )    =>  wvw_DrawBoxGroup        ( [, <vlist>] )
#xtranslate wvt_DrawBoxGroupRaised  ( [<vlist,...>] )    =>  wvw_DrawBoxGroupRaised  ( [, <vlist>] )
#xtranslate wvt_DrawImage           ( [<vlist,...>] )    =>  wvw_DrawImage           ( [, <vlist>] )
#xtranslate wvt_DrawLabel           ( [<vlist,...>] )    =>  wvw_DrawLabel           ( [, <vlist>] )
#xtranslate wvt_DrawOutline         ( [<vlist,...>] )    =>  wvw_DrawOutline         ( [, <vlist>] )
#xtranslate wvt_DrawLine            ( [<vlist,...>] )    =>  wvw_DrawLine            ( [, <vlist>] )
#xtranslate wvt_DrawEllipse         ( [<vlist,...>] )    =>  wvw_DrawEllipse         ( [, <vlist>] )
#xtranslate wvt_DrawRectangle       ( [<vlist,...>] )    =>  wvw_DrawRectangle       ( [, <vlist>] )
#xtranslate wvt_DrawColorRect       ( [<vlist,...>] )    =>  wvw_DrawColorRect       ( [, <vlist>] )
#xtranslate wvt_DrawRoundRect       ( [<vlist,...>] )    =>  wvw_DrawRoundRect       ( [, <vlist>] )
#xtranslate wvt_DrawFocusRect       ( [<vlist,...>] )    =>  wvw_DrawFocusRect       ( [, <vlist>] )
#xtranslate wvt_DrawGridHorz        ( [<vlist,...>] )    =>  wvw_DrawGridHorz        ( [, <vlist>] )
#xtranslate wvt_DrawGridVert        ( [<vlist,...>] )    =>  wvw_DrawGridVert        ( [, <vlist>] )
#xtranslate wvt_DrawButton          ( [<vlist,...>] )    =>  wvw_DrawButton          ( [, <vlist>] )

#xtranslate wvt_SetMousePos         ( [<vlist,...>] )    =>  wvw_SetMousePos         ( [, <vlist>] )

/* in GTWVT no pending rect is reflected as {0,0,0,0}
   in GTWVW no pending rect is reflected as {y1,x1,y2,x2} where y1 > y2 or x1 > x2
   thus we need some temporary var to check this exception
*/
#xtranslate wvt_GetPaintRect        ( [<vlist,...>] )    =>  Eval( {| _wvwtemp_ | _wvwtemp_ := wvw_GetPaintRect( [, <vlist>] ), ;
                                                             iif( _wvwtemp_\[ 1 \] > _wvwtemp_\[ 3 \] .OR. _wvwtemp_\[ 2 \] > _wvwtemp_\[ 4 \], ;
                                                                  { 0, 0, 0, 0 }, _wvwtemp_ ) } )

#xtranslate wvt_SetPointer          ( [<vlist,...>] )    =>  wvw_SetPointer          ( [, <vlist>] )
#xtranslate wvt_DrawPicture         ( [<vlist,...>] )    =>  wvw_DrawPicture         ( [, <vlist>] )
#xtranslate wvt_DrawLabelEx         ( [<vlist,...>] )    =>  wvw_DrawLabelEx         ( [, <vlist>] )
#xtranslate wvt_DrawLineEx          ( [<vlist,...>] )    =>  wvw_DrawLineEx          ( [, <vlist>] )
#xtranslate wvt_DrawOutlineEx       ( [<vlist,...>] )    =>  wvw_DrawOutlineEx       ( [, <vlist>] )
#xtranslate wvt_MessageBox          ( [<vlist,...>] )    =>  wvw_MessageBox          ( [, <vlist>] )
#xtranslate wvt_SetToolTipActive    ( [<vlist,...>] )    =>  wvw_SetToolTipActive    ( [, <vlist>] )
#xtranslate wvt_SetToolTip          ( [<vlist,...>] )    =>  wvw_SetToolTip          ( [, <vlist>] )
#xtranslate wvt_SetToolTipText      ( [<vlist,...>] )    =>  wvw_SetToolTipText      ( [, <vlist>] )
#xtranslate wvt_SetToolTipMargin    ( [<vlist,...>] )    =>  wvw_SetToolTipMargin    ( [, <vlist>] )
#xtranslate wvt_SetToolTipWidth     ( [<vlist,...>] )    =>  wvw_SetToolTipWidth     ( [, <vlist>] )
#xtranslate wvt_SetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_SetToolTipBkColor   ( [, <vlist>] )
#xtranslate wvt_SetToolTipTextColor ( [<vlist,...>] )    =>  wvw_SetToolTipTextColor ( [, <vlist>] )
#xtranslate wvt_SetToolTipTitle     ( [<vlist,...>] )    =>  wvw_SetToolTipTitle     ( [, <vlist>] )
#xtranslate wvt_GetToolTipWidth     ( [<vlist,...>] )    =>  wvw_GetToolTipWidth     ( [, <vlist>] )
#xtranslate wvt_GetToolTipBkColor   ( [<vlist,...>] )    =>  wvw_GetToolTipBkColor   ( [, <vlist>] )
#xtranslate wvt_GetToolTipTextColor ( [<vlist,...>] )    =>  wvw_GetToolTipTextColor ( [, <vlist>] )

/* Timer functions. WARNING: WVT implementation is slightly different. */
#xtranslate wvt_SetTimer   ( [<vlist,...>] )    =>  wvw_SetTimer  ( [, <vlist>] )
#xtranslate wvt_KillTimer  ( [<vlist,...>] )    =>  wvw_KillTimer ( [, <vlist>] )

/* wvw_DrawStatusBar() is for compatibility only.
   Recommended to use wvw_sb*() functions instead. */
#xtranslate wvt_DrawStatusBar ( [<vlist,...>] )          =>  wvw_DrawStatusBar ( [, <vlist>] )

/* Native Statusbar functions. Currently none in WVT.
   WVT uses different approach (wvt_DrawStatusBar) */
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

/* PART-3: RESERVED FUNCTION NAMES ("callback" prg functions, called by GTWVW) */

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
  to make sure it follows GTWVW convention. For example, if you have multiple
  exit points in that function.

  IMPORTANT NOTES ON MainCoord Mode:

  Using wvw_nSetCurWindow() in MainCoord Mode may not be appropriate, because
  current window is reset to 0 upon returning from a standard GT function
  (DispOut(), QQOut(), DevOut(), etc.). Remember that these functions
  may also be called indirectly through many other functions/commands
  (Alert(), AChoice(), GET, etc.).

  You may want to replace wvw_nSetCurWindow() with your own function in this case.
 */

#xtranslate FUNCTION wvt_Paint( [<vlist,...>] ) => FUNCTION wvw_Paint( nWinNum [,<vlist>] )
#xtranslate PROCEDURE wvt_Paint( [<vlist,...>] ) => PROCEDURE wvw_Paint( nWinNum [,<vlist>] )
#xtranslate wvt_Paint( [<vlist,...>] ) => wvw_Paint( [,<vlist>] )

#xtranslate FUNCTION wvt_SetFocus( [<vlist,...>] ) => FUNCTION wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE wvt_SetFocus( [<vlist,...>] ) => PROCEDURE wvw_SetFocus( nWinNum [,<vlist>] )
#xtranslate wvt_SetFocus( [<vlist,...>] ) => wvw_SetFocus( [,<vlist>] )

#xtranslate FUNCTION wvt_KillFocus( [<vlist,...>] ) => FUNCTION wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate PROCEDURE wvt_KillFocus( [<vlist,...>] ) => PROCEDURE wvw_KillFocus( nWinNum [,<vlist>] )
#xtranslate wvt_KillFocus( [<vlist,...>] ) => wvw_KillFocus( [,<vlist>] )

#xtranslate FUNCTION wvt_Mouse( [<vlist,...>] ) => FUNCTION wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate PROCEDURE wvt_Mouse( [<vlist,...>] ) => PROCEDURE wvw_Mouse( nWinNum [,<vlist>] )
#xtranslate wvt_Mouse( [<vlist,...>] ) => wvw_Mouse( [,<vlist>] )

#xtranslate FUNCTION wvt_Timer() => FUNCTION wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
#xtranslate PROCEDURE wvt_Timer() => PROCEDURE wvw_Timer( nWinNum, hWnd, message, wParam, lParam )
/* Currently wvt_Timer() is never called by GTWVT.
   There should never be any existing usage of this function. */

/* wvt_MenuSelect() does not exist in WVT. */
