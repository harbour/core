/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#include "hbgtinfo.ch"

#include "hbwin.ch"

FUNCTION wvw_GetClipboard()
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION wvw_SetClipboard( cData )

   IF HB_ISSTRING( cData )
      hb_gtInfo( HB_GTI_CLIPBOARDDATA, cData )
      RETURN .T.
   ENDIF

   RETURN .F.

PROCEDURE wvw_PasteFromClipboard()

   hb_gtInfo( HB_GTI_CLIPBOARDPASTE )

   RETURN

FUNCTION wvw_GetPalette()
   RETURN hb_gtInfo( HB_GTI_PALETTE )

/* <aPalette> = An array of 16 elements with RGB values */
PROCEDURE wvw_SetPalette( aPalette )

   IF ! HB_ISARRAY( aPalette )
      aPalette := AFill( Array( 16 ), 0 )
   ENDIF

   hb_gtInfo( HB_GTI_PALETTE, aPalette )

   RETURN

FUNCTION wvw_GetRGBColor( nColor )

   hb_default( @nColor, -1 )

   RETURN iif( nColor >= 0 .AND. nColor <= 15, hb_gtInfo( HB_GTI_PALETTE, nColor ), 0 )

FUNCTION wvw_BringToTop1( hWnd )

   IF wapi_IsIconic( hWnd )
      wapi_ShowWindow( hWnd, WIN_SW_RESTORE )
   ELSE
      wapi_BringWindowToTop( hWnd )  /* IE 5.5 related hack */
      wapi_SetForegroundWindow( hWnd )
   ENDIF

   RETURN .T.

FUNCTION wvw_LoadIcon( ncRes )
   RETURN wapi_LoadIcon( iif( HB_ISNUMERIC( ncRes ),, wapi_GetModuleHandle() ), ncRes )

FUNCTION wvw_LoadImage( hInstance, ncRes, ... )
   RETURN wapi_LoadImage( iif( HB_ISNUMERIC( ncRes ), wapi_GetModuleHandle(), hInstance ), ncRes, ... )

FUNCTION wvw_LoadBitmap( ncRes, lNULLInstance )
   RETURN wapi_LoadBitmap( iif( hb_defaultValue( lNULLInstance, .F. ),, wapi_GetModuleHandle() ), ncRes )

FUNCTION wvw_LoadBitmapEx( hInstance, ncRes )
   RETURN wapi_LoadBitmap( hb_defaultValue( hInstance, wapi_GetModuleHandle() ), ncRes )

FUNCTION wvw_DrawIcon( hDC, hIcon, x, y )
   RETURN wapi_DrawIcon( hDC, x, y, hIcon )

FUNCTION wvw_RedrawWindow( hWnd, nFlags )
   RETURN wapi_RedrawWindow( hWnd,,, nFlags )

FUNCTION wvw_GetIconSize( hIcon )

   LOCAL ii

   wapi_GetIconInfo( hIcon, @ii )

   RETURN { ii[ 2 ] * 2, ii[ 3 ] * 2 }

/* Removes System Menu of a window
   if lRemoveClose is .T., also removes the 'Close' command and 'X' button */
PROCEDURE wvw_NoSysMenu( nWin, lRemoveClose )

   LOCAL hWnd
   LOCAL hMenu

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) ) .AND. ;
      ! Empty( hMenu := wapi_GetSystemMenu( hWnd, .F. ) )

      wapi_DeleteMenu( hMenu, WIN_SC_MAXIMIZE, WIN_MF_BYCOMMAND )
      wapi_DeleteMenu( hMenu, WIN_SC_MINIMIZE, WIN_MF_BYCOMMAND )
      wapi_DeleteMenu( hMenu, WIN_SC_SIZE, WIN_MF_BYCOMMAND )
      wapi_DeleteMenu( hMenu, WIN_SC_MOVE, WIN_MF_BYCOMMAND )
      wapi_DeleteMenu( hMenu, WIN_SC_RESTORE, WIN_MF_BYCOMMAND )
      wapi_DeleteMenu( hMenu, WIN_SC_NEXTWINDOW, WIN_MF_BYCOMMAND )
      IF hb_defaultValue( lRemoveClose, .F. )
         wapi_DeleteMenu( hMenu, WIN_SC_CLOSE, WIN_MF_BYCOMMAND )
         wapi_DeleteMenu( hMenu, 0, WIN_MF_BYPOSITION )
      ENDIF
      wapi_DrawMenuBar( hWnd )
   ENDIF

   RETURN

/* Disable close 'X' button of a window */
PROCEDURE wvw_NoClose( nWin )

   LOCAL hWnd
   LOCAL hMenu

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) ) .AND. ;
      ! Empty( hMenu := wapi_GetSystemMenu( hWnd, .F. ) )

      wapi_DeleteMenu( hMenu, WIN_SC_CLOSE, WIN_MF_BYCOMMAND )
      wapi_DrawMenuBar( hWnd )
   ENDIF

   RETURN

PROCEDURE wvw_YesClose( nWin )

   LOCAL hWnd
   LOCAL hMenu

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) ) .AND. ;
      ! Empty( hMenu := wapi_GetSystemMenu( hWnd, .F. ) )

      wapi_AppendMenu( hMenu, WIN_SC_CLOSE, WIN_MF_BYCOMMAND, "" )
      wapi_DrawMenuBar( hWnd )
   ENDIF

   RETURN

/* Original implementation RTEd when passing an ID that
   was equal to WVW_ID_BASE_PUSHBUTTON or higher. */
FUNCTION wvw_AppendMenu( hMenu, nFlags, nMenuItemID, chCaption )

   IF HB_ISSTRING( chCaption )
      chCaption := StrTran( chCaption, "~", "&" )
   ENDIF

   RETURN wapi_AppendMenu( hMenu, nFlags, nMenuItemID, chCaption )

FUNCTION wvw_GetMenu( nWin )

   LOCAL hWnd := wvw_Get_hnd_Window( nWin )

   RETURN iif( Empty( hWnd ),, wapi_GetMenu( hWnd ) )

PROCEDURE wvw_DrawMenuBar( nWin )

   LOCAL hWnd

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      wapi_DrawMenuBar( hWnd )
   ENDIF

   RETURN

FUNCTION wvw_GetTitle( nWin )

   LOCAL hWnd

   RETURN iif( Empty( hWnd := wvw_Get_hnd_Window( nWin ) ), "", wapi_GetWindowText( hWnd ) )

/* Returns the System Menu of a window */
FUNCTION wvw_GetSystemMenu( nWin, lReset )

   LOCAL hWnd := wvw_Get_hnd_Window( nWin )

   RETURN iif( Empty( hWnd ),, wapi_GetSystemMenu( hWnd, lReset ) )

FUNCTION wvw_TrackPopupMenu( nWin, hMenu )

   LOCAL hWnd
   LOCAL xy

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN 0
   ENDIF

   xy := { => }

   wapi_GetCursorPos( @xy )

   RETURN wapi_TrackPopupMenu( hMenu, hb_bitOr( WIN_TPM_CENTERALIGN, WIN_TPM_RETURNCMD, WIN_TPM_RECURSE ), ;
                               xy[ "x" ], xy[ "y" ], 0, hWnd )

FUNCTION win_LoadIcon( ncIcon )

   IF HB_ISNUMERIC( ncIcon )
      RETURN wapi_LoadIcon( wapi_GetModuleHandle(), ncIcon )
   ENDIF

   RETURN wapi_LoadImage( , ncIcon, WIN_IMAGE_ICON, 0, 0, WIN_LR_LOADFROMFILE )

/* nSource: 0 ResourceIdByNumber
   nSource: 1 ResourceIdByName
   nSource: 2 ImageFromDiskFile */
FUNCTION win_LoadImage( ncImage, nSource )

   SWITCH hb_defaultValue( nSource, 0 )
   CASE 0
   CASE 1 ; RETURN wapi_LoadBitmap( wapi_GetModuleHandle(), ncImage )
   CASE 2 ; RETURN wapi_LoadImage( , ncImage, WIN_IMAGE_BITMAP, 0, 0, WIN_LR_LOADFROMFILE )
   ENDSWITCH

   RETURN NIL

PROCEDURE wvw_SetParent( nWin1, nWin2 )

   LOCAL hWnd1, hWnd2

   hb_default( @nWin2, 0 )

   IF nWin2 != 0 .AND. ;
      ! Empty( hWnd1 := wvw_Get_hnd_Window( nWin1 ) ) .AND. ;
      ! Empty( hWnd2 := wvw_Get_hnd_Window( nWin2 ) )

      wapi_SetParent( hWnd1, hWnd2 )
   ENDIF

   RETURN

FUNCTION wvw_MessageBox( nWin, ... )

   LOCAL hWnd

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN -1
   ENDIF

   RETURN wapi_MessageBox( hWnd, ... )

PROCEDURE wvw_ShowWindow( nWin, nCmdShow )

   LOCAL hWnd

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      wapi_ShowWindow( hWnd, hb_defaultValue( nCmdShow, WIN_SW_SHOWNORMAL ) )
   ENDIF

   RETURN

PROCEDURE wvw_UpdateWindow( nWin )

   LOCAL hWnd

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      wapi_UpdateWindow( hWnd )
   ENDIF

   RETURN

FUNCTION wvw_SetOnTop( nWin )

   LOCAL hWnd
   LOCAL rc

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN .F.
   ENDIF

   rc := { => }

   wapi_GetWindowRect( hWnd, @rc )

   RETURN wapi_SetWindowPos( hWnd, WIN_HWND_TOPMOST, ;
      rc[ "left" ], rc[ "top" ], 0, 0, hb_bitOr( WIN_SWP_NOSIZE, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) )

FUNCTION wvw_SetAsNormal( nWin )

   LOCAL hWnd
   LOCAL rc

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN .F.
   ENDIF

   rc := { => }

   wapi_GetWindowRect( hWnd, @rc )

   RETURN wapi_SetWindowPos( hWnd, WIN_HWND_NOTOPMOST, ;
      rc[ "left" ], rc[ "top" ], 0, 0, hb_bitOr( WIN_SWP_NOSIZE, WIN_SWP_NOMOVE, WIN_SWP_NOACTIVATE ) )

/* Get/Set window style
   NOTES: if window has controls (eg. pushbutton, scrollbar)
          you should include WS_CLIPCHILDREN in nStyle
   SIDE EFFECT:
         if window is hidden, applying nStyle here will cause it to show
   return previous style */
FUNCTION wvw_SetWinStyle( nWin, nStyle )

   LOCAL hWnd
   LOCAL nStyleOld

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN 0
   ENDIF

   IF HB_ISNUMERIC( nStyle )
      nStyleOld := wapi_SetWindowLongPtr( hWnd, WIN_GWL_STYLE, nStyle )
      wapi_SetWindowPos( hWnd,, 0, 0, 0, 0, hb_bitOr( WIN_SWP_NOMOVE, WIN_SWP_NOSIZE, WIN_SWP_NOZORDER, WIN_SWP_FRAMECHANGED ) )
      wapi_ShowWindow( hWnd, WIN_SW_SHOWNORMAL )
   ELSE
      nStyleOld := GetWindowLongPtr( hWnd, WIN_GWL_STYLE )
   ENDIF

   RETURN nStyleOld

/* Get/Set maximize button
   returns maximize box state prior to applying the new style
   NOTE: in order to enable MAXIMIZE button, app should have WVW_SIZE() callback function */
FUNCTION wvw_EnableMaximize( nWin, lEnable )

   LOCAL hWnd
   LOCAL nStyle
   LOCAL lEnableOld

   IF Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      RETURN .F.
   ENDIF

   nStyle := wapi_GetWindowLongPtr( hWnd, WIN_GWL_STYLE )

   lEnableOld := hb_bitAnd( nStyle, WIN_WS_MAXIMIZEBOX ) != 0

   IF HB_ISLOGICAL( lEnable ) .AND. lEnable != lEnableOld
      wapi_SetWindowLongPtr( hWnd, WIN_GWL_STYLE, iif( lEnable, hb_bitOr( nStyle, WIN_WS_MAXIMIZEBOX ), hb_bitAnd( nStyle, hb_bitNot( WIN_WS_MAXIMIZEBOX ) ) ) )
      wapi_SetWindowPos( hWnd,, 0, 0, 0, 0, hb_bitOr( WIN_SWP_NOMOVE, WIN_SWP_NOSIZE, WIN_SWP_NOZORDER, WIN_SWP_FRAMECHANGED ) )
      wapi_ShowWindow( hWnd, WIN_SW_SHOW )
   ENDIF

   RETURN lEnableOld

FUNCTION wvw_GetScreenWidth()
   RETURN wapi_GetSystemMetrics( WIN_SM_CXSCREEN )

FUNCTION wvw_GetScreenHeight()
   RETURN wapi_GetSystemMetrics( WIN_SM_CYSCREEN )

FUNCTION wvw_GetCursorPos()

   LOCAL xy

   wapi_GetCursorPos( @xy )

   RETURN xy

FUNCTION win_GetClientRect( hWnd )

   LOCAL rc

   wapi_GetClientRect( hWnd, @rc )

   RETURN rc

FUNCTION win_SetTimer( ... )
   RETURN ! Empty( wapi_SetTimer( ... ) )

FUNCTION win_InvalidateRect( w, e, l, t, r, b )
   RETURN wapi_InvalidateRect( w, iif( PCount() > 2, { l, t, r, b }, NIL ), ;
      iif( HB_ISLOGICAL( e ), e, hb_defaultValue( e, 1 ) != 0 ) )
