/*
 * Video subsystem for Windows using GUI windows instead of Console
 *
 *    Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * based on:
 *
 *    Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Direct WinApi Functions - Prefixed wvg_*() */

#include "gtwvg.h"

#include <windowsx.h>

#if ! defined( HB_OS_WIN_CE )
   #ifndef CB_GETCOMBOBOXINFO
   #define CB_GETCOMBOBOXINFO  0x0164
   #endif
#endif

HB_FUNC( WVG_DELETEOBJECT )
{
   hb_retl( DeleteObject( hbwapi_par_raw_HGDIOBJ( 1 ) ) );
}

HB_FUNC( WVG_SELECTOBJECT )
{
   hbwapi_ret_raw_HANDLE( SelectObject( hbwapi_par_raw_HDC( 1 ), hbwapi_par_raw_HGDIOBJ( 2 ) ) );
}

HB_FUNC( WVG_GETDC )
{
   hbwapi_ret_raw_HANDLE( GetDC( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WVG_RELEASEDC )
{
   hb_retl( ReleaseDC( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HDC( 2 ) ) );
}

HB_FUNC( WVG_CREATEBRUSH )
{
   LOGBRUSH lb;

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = hbwapi_par_COLORREF( 2 );
   lb.lbHatch = hb_parni( 3 );
#if ! defined( HB_OS_WIN_CE )
   hbwapi_ret_raw_HANDLE( CreateBrushIndirect( &lb ) );
#else
   hbwapi_ret_raw_HANDLE( CreateSolidBrush( lb.lbColor ) );
#endif
}

/* Menu manipulations */

HB_FUNC( WVG_SETMENU )
{
#if ! defined( HB_OS_WIN_CE )
   HWND hWnd = hbwapi_par_raw_HWND( 1 );

   HB_BOOL bSet = SetMenu( hWnd, hbwapi_par_raw_HMENU( 2 ) );

   #if 1
   RECT wi = { 0, 0, 0, 0 };
   RECT ci = { 0, 0, 0, 0 };
   int  height, width;

   GetWindowRect( hWnd, &wi );
   GetClientRect( hWnd, &ci );
   height = ci.bottom - ci.top;
   width  = ci.right - ci.left;

   width  += wi.right - wi.left - ci.right;
   height += wi.bottom - wi.top - ci.bottom;

   SetWindowPos( hWnd, NULL, wi.left, wi.top, width, height, SWP_NOZORDER );
   #endif

   hb_retl( bSet );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WVG_ISMENUITEMCHECKED )
{
   MENUITEMINFO lpmii;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
   lpmii.fMask  = MIIM_STATE;

   if( GetMenuItemInfo( hbwapi_par_raw_HMENU( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) )
      hb_retl( ( lpmii.fState & MFS_CHECKED ) != 0 );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVG_ISMENUITEMENABLED )  /* = grayed */
{
#if ! defined( HB_OS_WIN_CE )
   MENUITEMINFO lpmii;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
   lpmii.fMask  = MIIM_STATE;

   if( GetMenuItemInfo( hbwapi_par_raw_HMENU( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) )
      hb_retl( ( lpmii.fState & MFS_DISABLED /* equivalent to MFS_GRAYED */ ) == 0 );
   else
      hb_retl( HB_TRUE );
#else
   hb_retl( HB_TRUE );
#endif
}

HB_FUNC( WVG_SETMENUITEM )
{
   MENUITEMINFO lpmii;
   void *       hText = NULL;

   memset( &lpmii, 0, sizeof( lpmii ) );

   lpmii.cbSize = sizeof( lpmii );
#if ! defined( HB_OS_WIN_CE )
   if( hb_parl( 5 ) )
   {
      lpmii.fMask = MIIM_STRING;
      lpmii.dwTypeData = ( LPTSTR ) HB_PARSTR( 4, &hText, NULL );
   }
   else
      lpmii.fMask = MIIM_SUBMENU;
#else
   lpmii.fMask = MIIM_SUBMENU;
#endif

   hb_retl( SetMenuItemInfo( hbwapi_par_raw_HMENU( 1 ), ( UINT ) hb_parni( 2 ), TRUE, &lpmii ) );

   hb_strfree( hText );
}

/* TreeView Functions */

HB_FUNC( WVG_TREEVIEW_EXPAND )
{
   hb_retl( TreeView_Expand( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HTREEITEM( 2 ), ( hb_parl( 3 ) ? TVE_EXPAND : TVE_COLLAPSE ) ) );
}

#if 0
HB_FUNC( WVG_TREEVIEW_ISEXPANDED )
{
   hb_retl( TreeView_GetItemState( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HTREEITEM( 2 ), ( UINT ) TVIS_EXPANDED ) );
}
#endif

/* ListBox Functions */

HB_FUNC( WVG_LBGETTEXT )
{
   HWND hWnd = hbwapi_par_raw_HWND( 1 );
   int iIndex = hb_parni( 2 );
   int iLen = ListBox_GetTextLen( hWnd, iIndex );
   LPTSTR szText = ( LPTSTR ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) );

   ( void ) ListBox_GetText( hWnd, iIndex, szText );

   HB_RETSTRLEN( szText, iLen );
}

HB_FUNC( WVG_LBGETCURSEL )
{
   hb_retni( ListBox_GetCurSel( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WVG_LBSETCURSEL )
{
   hb_retni( ListBox_SetCurSel( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

/* Buttons */

HB_FUNC( WVG_BUTTON_GETCHECK )
{
   hb_retnl( Button_GetCheck( hbwapi_par_raw_HWND( 1 ) ) );
}

/* wvg_GetCurrentObject( hDC, nObjType ) */
HB_FUNC( WVG_GETCURRENTOBJECT )
{
   hbwapi_ret_raw_HANDLE( GetCurrentObject( hbwapi_par_raw_HDC( 1 ), hb_parni( 2 ) ) );
}

/* wvg_GetCurrentBrush( hDC ) */
HB_FUNC( WVG_GETCURRENTBRUSH )
{
   hbwapi_ret_raw_HANDLE( GetCurrentObject( hbwapi_par_raw_HDC( 1 ), OBJ_BRUSH ) );
}

/* wvg_GetCurrentFont( hDC ) */
HB_FUNC( WVG_GETCURRENTFONT )
{
   hbwapi_ret_raw_HANDLE( GetCurrentObject( hbwapi_par_raw_HDC( 1 ), OBJ_FONT ) );
}

/* wvg_SetLayeredWindowAttributes( hWnd, nRGB, nOpacityFactor [0-255] ) */
HB_FUNC( WVG_SETLAYEREDWINDOWATTRIBUTES )
{
   HINSTANCE h = GetModuleHandle( TEXT( "user32.dll" ) );

   if( h )
   {
      wvtSetLayeredWindowAttributes pfnLayered = ( wvtSetLayeredWindowAttributes ) HB_WINAPI_GETPROCADDRESS( h, "SetLayeredWindowAttributes" );

      if( pfnLayered )
      {
         HWND     hWnd = hbwapi_par_raw_HWND( 1 );
         COLORREF cr   = hbwapi_par_COLORREF_def( 2, RGB( 255, 255, 255 ) );

         SetWindowLong( hWnd, GWL_EXSTYLE, GetWindowLong( hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );

         if( pfnLayered( hWnd, cr, ( BYTE ) hb_parni( 3 ), /*LWA_COLORKEY |*/ LWA_ALPHA ) == 0 )
         {
            /* Just to supress warning */
         }
      }
      FreeLibrary( h );
   }
}

HB_FUNC( WVG_SENDTOOLBARMESSAGE )
{
   HWND hTB = hbwapi_par_raw_HWND( 1 );

   switch( hbwapi_par_INT( 2 ) )
   {
      case TB_ADDBITMAP:
      {
         TBADDBITMAP tbab;

         tbab.hInst = NULL;
#if ( _WIN32_IE >= 0x0500 )
         tbab.nID = ( UINT_PTR ) hbwapi_par_raw_HBITMAP( 3 );
#else
         tbab.nID = ( UINT ) hbwapi_par_raw_HBITMAP( 3 );
#endif
         hbwapi_ret_NI( ( int ) SendMessage( hTB, TB_ADDBITMAP, ( WPARAM ) 1, ( LPARAM ) &tbab ) );
         break;
      }
      case TB_ADDBUTTONS:
      {
         TBBUTTON tbb;

         tbb.iBitmap   = hbwapi_par_INT( 3 );
         tbb.idCommand = hbwapi_par_INT( 4 );
         tbb.fsState   = TBSTATE_ENABLED;
         tbb.fsStyle   = TBSTYLE_BUTTON;
         tbb.dwData    = 0;
         tbb.iString   = hbwapi_par_INT( 5 );

         hbwapi_ret_L( SendMessage( hTB, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb ) );
         break;
      }
      case TB_ADDSTRING:
      {
         void * hCaption;
         hbwapi_ret_NI( ( int ) SendMessage( hTB, TB_ADDSTRING, 0, ( LPARAM ) HB_PARSTR( 3, &hCaption, NULL ) ) );
         hb_strfree( hCaption );
         break;
      }
      case TB_AUTOSIZE:
         SendMessage( hTB, TB_AUTOSIZE, 0, 0 );
         break;
      case TB_BUTTONCOUNT:
         break;
      case TB_BUTTONSTRUCTSIZE:
         SendMessage( hTB, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );
         break;
      case TB_CHANGEBITMAP:
      case TB_CHECKBUTTON:
      case TB_COMMANDTOINDEX:
      case TB_DELETEBUTTON:
      case TB_ENABLEBUTTON:
      case TB_GETBITMAP:
      case TB_GETBITMAPFLAGS:
      case TB_GETBUTTON:
      case TB_GETBUTTONINFO:
      case TB_GETBUTTONSIZE:
      case TB_GETBUTTONTEXT:
      case TB_GETDISABLEDIMAGELIST:
      case TB_GETIMAGELIST:
      case TB_GETITEMRECT:
      case TB_GETRECT:
      case TB_GETROWS:
      case TB_GETSTATE:
      case TB_GETSTYLE:
      case TB_GETTEXTROWS:
      case TB_GETTOOLTIPS:
      case TB_HIDEBUTTON:
      case TB_HITTEST:
      case TB_INDETERMINATE:
      case TB_INSERTBUTTON:
      case TB_ISBUTTONCHECKED:
      case TB_ISBUTTONENABLED:
      case TB_ISBUTTONHIDDEN:
      case TB_ISBUTTONHIGHLIGHTED:
      case TB_ISBUTTONINDETERMINATE:
      case TB_ISBUTTONPRESSED:
      case TB_LOADIMAGES:
      case TB_PRESSBUTTON:
      case TB_REPLACEBITMAP:
         break;
      case TB_SETBITMAPSIZE:
         SendMessage( hTB, TB_SETBITMAPSIZE, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONINFO:
         break;
      case TB_SETBUTTONSIZE:
         SendMessage( hTB, TB_SETBUTTONSIZE, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETBUTTONWIDTH:
         SendMessage( hTB, TB_SETBUTTONWIDTH, 0,
                      ( LPARAM ) MAKELONG( hbwapi_par_INT( 3 ), hbwapi_par_INT( 4 ) ) );
         break;
      case TB_SETIMAGELIST:
         SendMessage( hTB, TB_SETIMAGELIST, 0, ( LPARAM ) hbwapi_par_raw_HIMAGELIST( 3 ) );
         break;
      case TB_SETINDENT:
         SendMessage( hTB, TB_SETINDENT, ( WPARAM ) hbwapi_par_INT( 3 ), 0 );
         break;
      case TB_SETMAXTEXTROWS:
         SendMessage( hTB, TB_SETMAXTEXTROWS, ( WPARAM ) hbwapi_par_INT( 2 ), 0 );
         break;
      case TB_SETPARENT:
      case TB_SETROWS:
      case TB_SETSTATE:
      case TB_SETSTYLE:
      case TB_SETTOOLTIPS:
      case TB_SETCMDID:
      case TB_SETDISABLEDIMAGELIST:
      case TB_SETDRAWTEXTFLAGS:
         break;

      #if 0
      case TB_TRANSLATEACCELERATOR:
      case TB_SETPRESSEDIMAGELIST:
      case TB_SETWINDOWTHEME:
      case TB_GETIDEALSIZE:
      case TB_GETIMAGELISTCOUNT:
      case TB_GETMETRICS:
      case TB_GETPRESSEDIMAGELIST:
      case TB_GETSTRING:
      case TB_SETLISTGAP:
      case TB_GETITEMDROPDOWNRECT:
      case TB_SETHOTITEM2:
      case TB_SETMETRICS:
         break;
      #endif

#if ! defined( HB_OS_WIN_CE )
      case TB_SETPADDING:
         SendMessage( hTB, TB_SETPADDING, 0,
                      ( LPARAM ) MAKELPARAM( hbwapi_par_INT( 2 ), hbwapi_par_INT( 3 ) ) );
         break;
      case TB_MARKBUTTON:
         SendMessage( hTB, TB_MARKBUTTON, ( WPARAM ) hbwapi_par_INT( 3 ), ( LPARAM ) MAKELONG( hb_parl( 4 ), 0 ) );
         break;
      case TB_SETINSERTMARK:
      case TB_SETINSERTMARKCOLOR:
      case TB_SETCOLORSCHEME:
      case TB_SETEXTENDEDSTYLE:
      case TB_SETHOTIMAGELIST:
      case TB_SETHOTITEM:
      case TB_INSERTMARKHITTEST:
      case TB_MAPACCELERATOR:
      case TB_MOVEBUTTON:
      case TB_GETINSERTMARK:
         break;
      case TB_GETCOLORSCHEME:
      {
         PHB_ITEM    info = hb_itemArrayNew( 2 );
         COLORSCHEME colorScheme;

         memset( &colorScheme, 0, sizeof( colorScheme ) );

         colorScheme.dwSize = sizeof( colorScheme );
         SendMessage( hTB, TB_GETCOLORSCHEME, 0, ( LPARAM ) &colorScheme );

         hb_arraySetNInt( info, 1, colorScheme.clrBtnHighlight );
         hb_arraySetNInt( info, 2, colorScheme.clrBtnShadow );
         hb_itemReturnRelease( info );
         break;
      }
      case TB_CUSTOMIZE:
      case TB_GETANCHORHIGHLIGHT:
      case TB_GETEXTENDEDSTYLE:
      case TB_GETHOTIMAGELIST:
      case TB_GETINSERTMARKCOLOR:
      case TB_GETHOTITEM:
      case TB_GETOBJECT:
      case TB_GETUNICODEFORMAT:
      case TB_GETMAXSIZE:
      case TB_SAVERESTORE:
      case TB_SETANCHORHIGHLIGHT:
      case TB_SETUNICODEFORMAT:
         break;
#endif
   }
}

HB_FUNC( WVG_SENDEDITCONTROLMESSAGE )
{
   HWND hED = hbwapi_par_raw_HWND( 1 );

   switch( hbwapi_par_INT( 2 ) )
   {
      case EM_GETSEL:
      {
         DWORD min = 0;
         DWORD max = 0;
         SendMessage( hED, EM_GETSEL, ( WPARAM ) &min, ( LPARAM ) &max );
         break;
      }
   }
}

HB_FUNC( WVG_SENDCBMESSAGE )
{
   HWND   hCB   = hbwapi_par_raw_HWND( 1 );
   void * hText = NULL;

   switch( hbwapi_par_INT( 2 ) )
   {
      case CB_ADDSTRING:
         hb_retnint( SendMessage( hCB, CB_ADDSTRING, 0, ( LPARAM ) ( LPCTSTR ) HB_PARSTR( 3, &hText, NULL ) ) );
         break;
      case CB_DELETESTRING:
         hb_retnint( SendMessage( hCB, CB_DELETESTRING, hb_parni( 3 ), 0 ) );
         break;
#if defined( CB_DIR )
      case CB_DIR:
         hb_retnint( SendMessage( hCB, CB_DIR, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
#endif
      case CB_FINDSTRING:
         hb_retnint( SendMessage( hCB, CB_FINDSTRING, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
      case CB_FINDSTRINGEXACT:
         hb_retnint( SendMessage( hCB, CB_FINDSTRINGEXACT, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
#if defined( CB_GETCOMBOBOXINFO )
      case CB_GETCOMBOBOXINFO:
      {
#if ! defined( HB_OS_WIN_CE )
         COMBOBOXINFO cbi;

         memset( &cbi, 0, sizeof( cbi ) );
         cbi.cbSize = sizeof( cbi );

         if( GetComboBoxInfo( hCB, &cbi ) )
         {
            PHB_ITEM pCbi = hb_itemArrayNew( 6 );
            PHB_ITEM pRc1 = hb_itemArrayNew( 4 );
            PHB_ITEM pRc2 = hb_itemArrayNew( 4 );

            hb_arraySetNI( pRc1, 1, cbi.rcItem.left );
            hb_arraySetNI( pRc1, 2, cbi.rcItem.top );
            hb_arraySetNI( pRc1, 3, cbi.rcItem.right );
            hb_arraySetNI( pRc1, 4, cbi.rcItem.bottom );

            hb_arraySet( pCbi, 1, pRc1 );

            hb_arraySetNI( pRc2, 1, cbi.rcButton.left );
            hb_arraySetNI( pRc2, 2, cbi.rcButton.top );
            hb_arraySetNI( pRc2, 3, cbi.rcButton.right );
            hb_arraySetNI( pRc2, 4, cbi.rcButton.bottom );

            hb_arraySet( pCbi, 2, pRc2 );

            hb_arraySetNInt( pCbi, 3, cbi.stateButton );
            hbwapi_arraySet_HANDLE( pCbi, 4, cbi.hwndCombo );
            hbwapi_arraySet_HANDLE( pCbi, 5, cbi.hwndItem );
            hbwapi_arraySet_HANDLE( pCbi, 6, cbi.hwndList );

            hb_itemReturnRelease( pCbi );
            hb_itemRelease( pRc1 );
            hb_itemRelease( pRc2 );
         }
#endif
         break;
      }
#endif
      case CB_GETCOUNT:
         hb_retnint( SendMessage( hCB, CB_GETCOUNT, 0, 0 ) );
         break;
#if defined( CB_GETCUEBANNER )
      case CB_GETCUEBANNER:
         break;
#endif
      case CB_GETCURSEL:
         hb_retnint( SendMessage( hCB, CB_GETCURSEL, 0, 0 ) );
         break;
      case CB_GETDROPPEDCONTROLRECT:
      {
         RECT     rc;
         PHB_ITEM pRect = hb_itemArrayNew( 4 );

         SendMessage( hCB, CB_GETDROPPEDCONTROLRECT, 0, ( LPARAM ) &rc );

         hb_arraySetNI( pRect, 1, rc.left );
         hb_arraySetNI( pRect, 2, rc.top );
         hb_arraySetNI( pRect, 3, rc.right );
         hb_arraySetNI( pRect, 4, rc.bottom );

         hb_itemReturnRelease( pRect );
         break;
      }
      case CB_GETDROPPEDSTATE:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDSTATE, 0, 0 ) );
         break;
      case CB_GETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_GETDROPPEDWIDTH, 0, 0 ) );
         break;
      case CB_GETEDITSEL:
      {
         DWORD    range = ( DWORD ) SendMessage( hCB, CB_GETEDITSEL, 0, 0 );
         PHB_ITEM pRng  = hb_itemArrayNew( 2 );

         hb_arraySetNI( pRng, 1, LOWORD( range ) );
         hb_arraySetNI( pRng, 1, HIWORD( range ) );
         hb_itemReturnRelease( pRng );

         break;
      }
      case CB_GETEXTENDEDUI:
         hb_retnint( SendMessage( hCB, CB_GETEXTENDEDUI, 0, 0 ) );
         break;
      case CB_GETHORIZONTALEXTENT:
         hb_retnint( SendMessage( hCB, CB_GETHORIZONTALEXTENT, 0, 0 ) );
         break;
      case CB_GETITEMDATA:
         hb_retnint( SendMessage( hCB, CB_GETITEMDATA, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_GETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_GETITEMHEIGHT, 0, 0 ) );
         break;
      case CB_GETLBTEXT:
      {
         HB_ISIZ iSize = SendMessage( hCB, CB_GETLBTEXTLEN, ( WPARAM ) hb_parnint( 3 ), 0 );
         LPTSTR  text  = ( LPTSTR ) hb_xgrab( ( iSize + 1 ) * sizeof( TCHAR ) );
         SendMessage( hCB, CB_GETLBTEXT, iSize, ( LPARAM ) text );
         HB_RETSTRLEN( text, iSize );
         hb_xfree( text );
         break;
      }
      case CB_GETLBTEXTLEN:
         hb_retnint( SendMessage( hCB, CB_GETLBTEXTLEN, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_GETLOCALE:
#if defined( CB_GETMINVISIBLE )
      case CB_GETMINVISIBLE:
         hb_retnint( SendMessage( hCB, CB_GETMINVISIBLE, 0, 0 ) );
         break;
#endif
      case CB_GETTOPINDEX:
         hb_retnint( SendMessage( hCB, CB_GETTOPINDEX, 0, 0 ) );
         break;
      case CB_INITSTORAGE:
         break;
      case CB_INSERTSTRING:
         hb_retnint( SendMessage( hCB, CB_INSERTSTRING, ( WPARAM ) hb_parnint( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
      case CB_LIMITTEXT:
         SendMessage( hCB, CB_LIMITTEXT, hb_parni( 3 ), 0 );
         break;
      case CB_RESETCONTENT:
         SendMessage( hCB, CB_RESETCONTENT, 0, 0 );
         break;
      case CB_SELECTSTRING:
         hb_retnint( SendMessage( hCB, CB_SELECTSTRING, ( WPARAM ) hb_parnint( 3 ), ( LPARAM ) HB_PARSTR( 4, &hText, NULL ) ) );
         break;
#if defined( CB_SETCUEBANNER )
      case CB_SETCUEBANNER:
         break;
#endif
      case CB_SETCURSEL:
         hb_retnint( SendMessage( hCB, CB_SETCURSEL, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETDROPPEDWIDTH:
         hb_retnint( SendMessage( hCB, CB_SETDROPPEDWIDTH, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETEDITSEL:
         break;
      case CB_SETEXTENDEDUI:
         SendMessage( hCB, CB_SETEXTENDEDUI, hb_parl( 3 ), 0 );
         break;
      case CB_SETHORIZONTALEXTENT:
         SendMessage( hCB, CB_SETHORIZONTALEXTENT, hb_parl( 3 ), 0 );
         break;
      case CB_SETITEMDATA:
         SendMessage( hCB, CB_SETITEMDATA, hb_parl( 3 ), ( LPARAM ) hb_parnint( 4 ) );
         break;
      case CB_SETITEMHEIGHT:
         hb_retnint( SendMessage( hCB, CB_SETITEMHEIGHT, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
      case CB_SETLOCALE:
         hb_retnint( SendMessage( hCB, CB_SETLOCALE, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
#if defined( CB_SETMINVISIBLE )
      case CB_SETMINVISIBLE:
         hb_retl( ( HB_BOOL ) SendMessage( hCB, CB_SETMINVISIBLE, ( WPARAM ) hb_parnint( 3 ), 0 ) );
         break;
#endif
      case CB_SETTOPINDEX:
         hb_retl( SendMessage( hCB, CB_SETTOPINDEX, ( WPARAM ) hb_parnint( 3 ), 0 ) ? FALSE : TRUE );
         break;
      case CB_SHOWDROPDOWN:
         SendMessage( hCB, CB_SHOWDROPDOWN, hb_parl( 3 ), 0 );
         break;
   }

   hb_strfree( hText );
}
