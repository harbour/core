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

/* wvw_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values */
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

/* removes System Menu of a window
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

FUNCTION wvw_GetMenu( nWin )

   LOCAL hWnd := wvw_Get_hnd_Window( nWin )

   RETURN iif( Empty( hWnd ),, wapi_GetMenu( hWnd ) )

PROCEDURE wvw_DrawMenuBar( nWin )

   LOCAL hWnd

   IF ! Empty( hWnd := wvw_Get_hnd_Window( nWin ) )
      wapi_DrawMenuBar( hWnd )
   ENDIF

   RETURN

/* returns the System Menu of a window */
FUNCTION wvw_GetSystemMenu( nWin, lReset )

   LOCAL hWnd := wvw_Get_hnd_Window( nWin )

   RETURN iif( Empty( hWnd ),, wapi_GetSystemMenu( hWnd, lReset ) )

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
