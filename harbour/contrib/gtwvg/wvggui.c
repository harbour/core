/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
 *    Adopted to new GT API
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *
 * See COPYING for licensing terms.
 *
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * Individual authors:
 * (C) 2003-2004 Giancarlo Niccolai <gc at niccolai dot ws>
 *         Standard xplatform GT Info system,
 *         Graphical object system and event system.
 *         GTINFO() And GTO_* implementation.
 *
 * (C) 2004 Mauricio Abre <maurifull@datafull.com>
 *         Cross-GT, multiplatform Graphics API
 *
 */

#include "wvggui.h"

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER   ( &SuperTable )
#define HB_GTID_PTR  ( &s_GtId )

#define HB_GTWVT_GET( p )  ( ( PHB_GTWVT ) HB_GTLOCAL( p ) )

static HB_CRITICAL_NEW( s_wvtMtx );
#define HB_WVT_LOCK()      hb_threadEnterCriticalSection( &s_wvtMtx )
#define HB_WVT_UNLOCK()    hb_threadLeaveCriticalSection( &s_wvtMtx )

static PHB_GTWVT s_wvtWindows[ WVT_MAX_WINDOWS ];
static int       s_wvtCount = 0;

static const TCHAR s_szClassName[] = TEXT( "Harbour_WVGGUI_Class" );

static const int K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G,
   K_CTRL_H, K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N,
   K_CTRL_O, K_CTRL_P, K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U,
   K_CTRL_V, K_CTRL_W, K_CTRL_X, K_CTRL_Y, K_CTRL_Z
};


static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

/*-*/

static void hb_gt_wvt_RegisterClass( HINSTANCE hInstance )
{
   WNDCLASS wndclass;

   memset( &wndclass, 0, sizeof( wndclass ) );
   wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
   wndclass.lpfnWndProc   = hb_gt_wvt_WndProc;
/* wndclass.cbClsExtra    = 0; */
/* wndclass.cbWndExtra    = 0; */
   wndclass.hInstance     = hInstance;
/* wndclass.hIcon         = NULL; */
   wndclass.hCursor       = LoadCursor( NULL, IDC_ARROW );
   wndclass.hbrBackground = NULL; /*( HBRUSH ) ( COLOR_BTNFACE + 1 ); */
/* wndclass.lpszMenuName  = NULL; */
   wndclass.lpszClassName = s_szClassName;

   if( ! RegisterClass( &wndclass ) )
   {
      int iError = GetLastError();
      if( iError != 1410 )
      {
         hb_errInternal( 10001, "Failed to register WGU window class", NULL, NULL );
      }
   }
}

static PHB_GTWVT hb_gt_wvt_Find( HWND hWnd )
{
   int iCount = s_wvtCount, iPos = 0;
   PHB_GTWVT pWVT = NULL;

   HB_WVT_LOCK();

   while( iCount && iPos < WVT_MAX_WINDOWS )
   {
      if( s_wvtWindows[ iPos ] )
      {
         if( s_wvtWindows[ iPos ]->hWnd == hWnd )
         {
            pWVT = s_wvtWindows[ iPos ];
            break;
         }
         --iCount;
      }
      ++iPos;
   }

   HB_WVT_UNLOCK();

   return pWVT;
}

static HB_BOOL hb_gt_wvt_Alloc( PHB_GTWVT pWVT )
{
   HB_BOOL fOK = HB_FALSE;

   HB_WVT_LOCK();

   if( s_wvtCount < WVT_MAX_WINDOWS )
   {
      int iPos = 0;
      do
      {
         if( s_wvtWindows[ iPos ] == NULL )
         {
            s_wvtWindows[ iPos ] = pWVT;
            pWVT->iHandle = iPos;
            if( ++s_wvtCount == 1 )
               hb_gt_wvt_RegisterClass( pWVT->hInstance );
            fOK = HB_TRUE;
            break;
         }
         ++iPos;
      }
      while( iPos < WVT_MAX_WINDOWS );
   }

   HB_WVT_UNLOCK();

   return fOK;
}

static void hb_gt_wvt_Free( PHB_GTWVT pWVT )
{
   HB_WVT_LOCK();

   s_wvtWindows[ pWVT->iHandle ] = NULL;

   if( --s_wvtCount == 0 )
   {
      if( pWVT->hInstance )
         UnregisterClass( s_szClassName, pWVT->hInstance );
   }

   HB_WVT_UNLOCK();

   if( pWVT->hWindowTitle )
      hb_strfree( pWVT->hWindowTitle );

   /* Detach PRG callback */

   hb_itemRelease( pWVT->pPP->pParentGT );
   hb_xfree( pWVT->pPP );

   if( pWVT->hWnd )
      DestroyWindow( pWVT->hWnd );

   if( pWVT->hIcon && pWVT->bIconToFree )
      DestroyIcon( pWVT->hIcon );

   hb_xfree( pWVT );
}

static PHB_GTWVT hb_gt_wvt_New( PHB_GT pGT, HINSTANCE hInstance, int iCmdShow )
{
   PHB_GTWVT pWVT;

   pWVT = ( PHB_GTWVT ) hb_xgrab( sizeof( HB_GTWVT ) );
   memset( pWVT, 0, sizeof( HB_GTWVT ) );
   pWVT->pGT               = pGT;

   if( ! hb_gt_wvt_Alloc( pWVT ) )
   {
      hb_xfree( pWVT );
      return NULL;
   }

   pWVT->hInstance         = ( HINSTANCE ) hInstance;
   pWVT->iCmdShow          = iCmdShow;

   pWVT->ROWS              = WVT_DEFAULT_ROWS;
   pWVT->COLS              = WVT_DEFAULT_COLS;

   /* THESE are the default font parameters, if not changed by user */
   pWVT->PTEXTSIZE.x       = WVT_DEFAULT_FONT_WIDTH;
   pWVT->PTEXTSIZE.y       = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWidth         = WVT_DEFAULT_FONT_WIDTH;
   pWVT->fontHeight        = WVT_DEFAULT_FONT_HEIGHT;
   pWVT->fontWeight        = FW_NORMAL;
   pWVT->fontQuality       = DEFAULT_QUALITY;
   hb_strncpy( pWVT->fontFace, WVT_DEFAULT_FONT_NAME, sizeof( pWVT->fontFace ) - 1 );

   pWVT->MousePos.x        = 0;
   pWVT->MousePos.y        = 0;
   pWVT->MouseMove         = HB_TRUE;
   pWVT->hWnd              = NULL;
   pWVT->keyPointerIn      = 0;
   pWVT->keyPointerOut     = 0;
   pWVT->keyLast           = 0;

   pWVT->CentreWindow      = HB_TRUE;         /* Default is to always display window in centre of screen */
   pWVT->CodePage          = OEM_CHARSET;     /* GetACP(); - set code page to default system */

   pWVT->Win9X             = hb_iswin9x();

   pWVT->IgnoreWM_SYSCHAR  = HB_FALSE;

   pWVT->bResizable        = HB_TRUE;
   pWVT->bClosable         = HB_TRUE;

   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_cmdargARGVN( 0 ) );
      PHB_ITEM  pItem = hb_itemPutC( NULL, pFileName->szName );

      pWVT->lpWindowTitle = HB_ITEMGETSTR( pItem, &pWVT->hWindowTitle, NULL );

      hb_itemRelease( pItem );
      hb_xfree( pFileName );
   }

   pWVT->pPP               = ( HB_GT_PARAMS * ) hb_xgrab( sizeof( HB_GT_PARAMS ) );
   pWVT->pPP->style        = WS_THICKFRAME|WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX|WS_MAXIMIZEBOX;
   pWVT->pPP->exStyle      = 0;
   pWVT->pPP->x            = 0;
   pWVT->pPP->y            = 0;
   pWVT->pPP->width        = 600;
   pWVT->pPP->height       = 400;
   pWVT->pPP->pParentGT    = NULL;
   pWVT->pPP->bVisible     = HB_FALSE;
   pWVT->pPP->bConfigured  = HB_FALSE;
   pWVT->pPP->bRowCols     = HB_FALSE;
   pWVT->pPP->iWndType     = 0;

   pWVT->hostCDP    = hb_vmCDP();
#if defined( UNICODE )
   pWVT->inCDP      = hb_vmCDP();
#else
   {
      int i;
      for( i = 0; i < 256; ++i )
         pWVT->chrTransTbl[ i ] = pWVT->keyTransTbl[ i ] = ( HB_BYTE ) i;
   }
#endif
   pWVT->bResizing         = HB_FALSE;
   pWVT->width             = 600;
   pWVT->height            = 400;

   return pWVT;
}

static int hb_gt_wvt_FireEvent( PHB_GTWVT pWVT, int nEvent, PHB_ITEM pParams )
{
   int nResult = 0; /* Unhandled */

   if( pWVT->pGT->pNotifierBlock )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pEvent = hb_itemPutNI( NULL, nEvent );

         nResult = hb_itemGetNI( hb_vmEvalBlockV( ( PHB_ITEM ) pWVT->pGT->pNotifierBlock, 2, pEvent, pParams ) );

         hb_itemRelease( pEvent );

         hb_vmRequestRestore();
      }
   }
   hb_itemRelease( pParams );

   return nResult;
}

static void hb_gt_wvt_FireMenuEvent( PHB_GTWVT pWVT, int iMode, int menuIndex )
{
   PHB_ITEM pEvParams = hb_itemNew( NULL );

   hb_arrayNew( pEvParams, 2 );
   hb_arraySetNI( pEvParams, 1, iMode );
   hb_arraySetNI( pEvParams, 2, menuIndex );

   hb_gt_wvt_FireEvent( pWVT, HB_GTE_MENU, pEvParams );
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_gt_wvt_AddCharToInputQueue( PHB_GTWVT pWVT, int iKey )
{
   int iPos = pWVT->keyPointerIn;

   if( iKey == K_MOUSEMOVE || iKey == K_NCMOUSEMOVE )
   {
      /* Clipper strips repeated mouse movemnt - let's do the same */
      if( pWVT->keyLast == iKey && pWVT->keyPointerIn != pWVT->keyPointerOut )
         return;
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   pWVT->Keys[ iPos ] = pWVT->keyLast = iKey;
   if( ++iPos >= WVT_CHAR_QUEUE_SIZE )
      iPos = 0;
   if( iPos != pWVT->keyPointerOut )
      pWVT->keyPointerIn = iPos;

   #if 0
   /* Fire event to be trapped by the application */
   {
      PHB_ITEM pEvParams = hb_itemNew( NULL );
      hb_itemPutNI( pEvParams, iKey );
      hb_gt_wvt_FireEvent( pWVT, HB_GTE_KEYBOARD, pEvParams );
   }
   #endif
}

static HB_BOOL hb_gt_wvt_GetCharFromInputQueue( PHB_GTWVT pWVT, int * iKey )
{
   if( pWVT->keyPointerOut != pWVT->keyPointerIn )
   {
      *iKey = pWVT->Keys[ pWVT->keyPointerOut ];
      if( ++pWVT->keyPointerOut >= WVT_CHAR_QUEUE_SIZE )
      {
         pWVT->keyPointerOut = 0;
      }
      return HB_TRUE;
   }

   *iKey = 0;
   return HB_FALSE;
}

static void hb_gt_wvt_TranslateKey( PHB_GTWVT pWVT, int key, int shiftkey, int altkey, int controlkey )
{
   int nVirtKey = GetKeyState( VK_MENU );

   if( nVirtKey & 0x8000 ) /* alt + key */
   {
      hb_gt_wvt_AddCharToInputQueue( pWVT, altkey );
   }
   else
   {
      nVirtKey = GetKeyState( VK_CONTROL );
      if( nVirtKey & 0x8000 ) /* control + key */
      {
         hb_gt_wvt_AddCharToInputQueue( pWVT, controlkey );
      }
      else
      {
         nVirtKey = GetKeyState( VK_SHIFT );
         if( nVirtKey & 0x8000 ) /* shift + key */
            hb_gt_wvt_AddCharToInputQueue( pWVT, shiftkey );
         else /* just key */
            hb_gt_wvt_AddCharToInputQueue( pWVT, key );
      }
   }
}

static int hb_gt_wvt_key_ansi_to_oem( int c )
{
   BYTE pszAnsi[ 2 ];
   BYTE pszOem[ 2 ];

   pszAnsi[ 0 ] = ( BYTE ) c;
   pszAnsi[ 1 ] = 0;
   CharToOemBuffA( ( LPCSTR ) pszAnsi, ( LPSTR ) pszOem, 1 );

   return *pszOem;
}

static int hb_gt_wvt_SizeChanged( PHB_GTWVT pWVT )
{
   PHB_ITEM pEvParams = hb_itemNew( NULL );
   RECT rc;

   GetClientRect( pWVT->hWnd, &rc );

   hb_arrayNew( pEvParams, 4 );
   hb_arraySetNI( pEvParams, 1, pWVT->width );
   hb_arraySetNI( pEvParams, 2, pWVT->height );
   hb_arraySetNI( pEvParams, 3, rc.right - rc.left );
   hb_arraySetNI( pEvParams, 4, rc.bottom - rc.top );

   hb_gt_wvt_AddCharToInputQueue( pWVT, HB_K_RESIZE );

   hb_gt_wvt_FireEvent( pWVT, HB_GTE_RESIZED, pEvParams );

   pWVT->width  = rc.right - rc.left;
   pWVT->height = rc.bottom - rc.top;

   return 0;
}

static void hb_gt_wvt_MouseEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy;
   SHORT keyCode = 0;
   SHORT keyState;

   HB_SYMBOL_UNUSED( wParam );
   if( ! pWVT->MouseMove && ( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ) )
      return;

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
         keyCode = K_LBUTTONDOWN;
         break;

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_RBUTTONUP:
         keyCode = K_RBUTTONUP;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_MBUTTONDOWN:
         keyCode = K_MBUTTONDOWN;
         break;

      case WM_MBUTTONUP:
         keyCode = K_MBUTTONUP;
         break;

      case WM_MBUTTONDBLCLK:
         keyCode = K_MDBLCLK;
         break;

      case WM_MOUSEMOVE:
         keyState = ( SHORT ) wParam;

         switch( keyState )
         {
            case MK_LBUTTON:
               keyCode = K_MMLEFTDOWN;
               break;
            case MK_RBUTTON:
               keyCode = K_MMRIGHTDOWN;
               break;
            case MK_MBUTTON:
               keyCode = K_MMMIDDLEDOWN;
               break;
            default:
               keyCode = K_MOUSEMOVE;
         }
#if 0
         if( ! pWVT->bTracking )
         {
            TRACKMOUSEEVENT tmi;
            tmi.cbSize      = sizeof( TRACKMOUSEEVENT );
            tmi.dwFlags     = TME_LEAVE | TME_HOVER;
            tmi.hwndTrack   = pWVT->hWnd;
            tmi.dwHoverTime = 1;
            pWVT->bTracking = _TrackMouseEvent( &tmi );
         }
#endif
         break;
      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );
         keyCode = keyState > 0 ? K_MWFORWARD : K_MWBACKWARD;
         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( keyCode != 0 )
   {
      PHB_ITEM pEvParams = hb_itemNew( NULL );

      hb_arrayNew( pEvParams, 6 );
      hb_arraySetNL( pEvParams, 1, message );
      hb_arraySetNI( pEvParams, 2, keyCode );
      hb_arraySetNI( pEvParams, 3, xy.x );
      hb_arraySetNI( pEvParams, 4, xy.y );
      hb_arraySetNI( pEvParams, 5, xy.x );
      hb_arraySetNI( pEvParams, 6, xy.y );

      hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );
   }
}

static HB_BOOL hb_gt_wvt_KeyEvent( PHB_GTWVT pWVT, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;

         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;

         switch( wParam )
         {
            case VK_LEFT:
               hb_gt_wvt_TranslateKey( pWVT, K_LEFT , K_SH_LEFT , K_ALT_LEFT , K_CTRL_LEFT  );
               break;
            case VK_RIGHT:
               hb_gt_wvt_TranslateKey( pWVT, K_RIGHT, K_SH_RIGHT, K_ALT_RIGHT, K_CTRL_RIGHT );
               break;
            case VK_UP:
               hb_gt_wvt_TranslateKey( pWVT, K_UP   , K_SH_UP   , K_ALT_UP   , K_CTRL_UP    );
               break;
            case VK_DOWN:
               hb_gt_wvt_TranslateKey( pWVT, K_DOWN , K_SH_DOWN , K_ALT_DOWN , K_CTRL_DOWN  );
               break;
            case VK_HOME:
               hb_gt_wvt_TranslateKey( pWVT, K_HOME , K_SH_HOME , K_ALT_HOME , K_CTRL_HOME  );
               break;
            case VK_END:
               hb_gt_wvt_TranslateKey( pWVT, K_END  , K_SH_END  , K_ALT_END  , K_CTRL_END   );
               break;
            case VK_DELETE:
               hb_gt_wvt_TranslateKey( pWVT, K_DEL  , K_SH_DEL  , K_ALT_DEL  , K_CTRL_DEL   );
               break;
            case VK_INSERT:
               hb_gt_wvt_TranslateKey( pWVT, K_INS  , K_SH_INS  , K_ALT_INS  , K_CTRL_INS   );
               break;
            case VK_PRIOR:
               hb_gt_wvt_TranslateKey( pWVT, K_PGUP , K_SH_PGUP , K_ALT_PGUP , K_CTRL_PGUP  );
               break;
            case VK_NEXT:
               hb_gt_wvt_TranslateKey( pWVT, K_PGDN , K_SH_PGDN , K_ALT_PGDN , K_CTRL_PGDN  );
               break;

            case VK_F1:
               hb_gt_wvt_TranslateKey( pWVT, K_F1   , K_SH_F1, K_ALT_F1   , K_CTRL_F1    );
               break;
            case VK_F2:
               hb_gt_wvt_TranslateKey( pWVT, K_F2   , K_SH_F2, K_ALT_F2   , K_CTRL_F2    );
               break;
            case VK_F3:
               hb_gt_wvt_TranslateKey( pWVT, K_F3   , K_SH_F3, K_ALT_F3   , K_CTRL_F3    );
               break;
            case VK_F4:
#if 0
               if( pWVT->AltF4Close && bAlt )
                  return DefWindowProc( pWVT->hWnd, message, wParam, lParam ) != 0;
#endif
               hb_gt_wvt_TranslateKey( pWVT, K_F4   , K_SH_F4, K_ALT_F4   , K_CTRL_F4    );
               break;
            case VK_F5:
               hb_gt_wvt_TranslateKey( pWVT, K_F5   , K_SH_F5, K_ALT_F5   , K_CTRL_F5    );
               break;
            case VK_F6:
               hb_gt_wvt_TranslateKey( pWVT, K_F6   , K_SH_F6, K_ALT_F6   , K_CTRL_F6    );
               break;
            case VK_F7:
               hb_gt_wvt_TranslateKey( pWVT, K_F7   , K_SH_F7, K_ALT_F7   , K_CTRL_F7    );
               break;
            case VK_F8:
               hb_gt_wvt_TranslateKey( pWVT, K_F8   , K_SH_F8, K_ALT_F8   , K_CTRL_F8    );
               break;
            case VK_F9:
               hb_gt_wvt_TranslateKey( pWVT, K_F9   , K_SH_F9, K_ALT_F9   , K_CTRL_F9    );
               break;
            case VK_F10:
               hb_gt_wvt_TranslateKey( pWVT, K_F10  , K_SH_F10,K_ALT_F10  , K_CTRL_F10   );
               break;
            case VK_F11:
               hb_gt_wvt_TranslateKey( pWVT, K_F11  , K_SH_F11,K_ALT_F11  , K_CTRL_F11   );
               break;
            case VK_F12:
               hb_gt_wvt_TranslateKey( pWVT, K_F12  , K_SH_F12,K_ALT_F12  , K_CTRL_F12   );
               break;
            default:
            {
               HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
               HB_BOOL bShift    = GetKeyState( VK_SHIFT ) & 0x8000;
               int  iScanCode = HIWORD( lParam ) & 0xFF;

               if( bCtrl && iScanCode == 76 ) /* CTRL_VK_NUMPAD5 */
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, KP_CTRL_5 );
               }
               else if( bCtrl && wParam == VK_TAB ) /* K_CTRL_TAB */
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, bShift ? K_CTRL_SH_TAB : K_CTRL_TAB );
               }
               else if( iScanCode == 70 )                                  /* Ctrl_Break key OR Scroll Lock Key */
               {
                  if( bCtrl )                                              /* Not scroll lock */
                  {
                     hb_gt_wvt_AddCharToInputQueue( pWVT, HB_BREAK_FLAG ); /* Pretend Alt+C pressed */
                     pWVT->IgnoreWM_SYSCHAR = HB_TRUE;
                  }
                  else
                  {
                     DefWindowProc( pWVT->hWnd, message, wParam, lParam );   /* Let windows handle ScrollLock */
                  }
               }
               else if( bCtrl && iScanCode == 53 && bShift )
               {
                  hb_gt_wvt_AddCharToInputQueue( pWVT, K_CTRL_QUESTION );
               }
               else if( ( bAlt || bCtrl ) && (
                        wParam == VK_MULTIPLY || wParam == VK_ADD ||
                        wParam == VK_SUBTRACT || wParam == VK_DIVIDE ) )
               {
                  if( bAlt )
                     pWVT->IgnoreWM_SYSCHAR = HB_TRUE;

                  switch( wParam )
                  {
                     case VK_MULTIPLY:
                        hb_gt_wvt_TranslateKey( pWVT, '*', '*', KP_ALT_ASTERISK, KP_CTRL_ASTERISK );
                        break;
                     case VK_ADD:
                        hb_gt_wvt_TranslateKey( pWVT, '+', '+', KP_ALT_PLUS, KP_CTRL_PLUS );
                        break;
                     case VK_SUBTRACT:
                        hb_gt_wvt_TranslateKey( pWVT, '-', '-', KP_ALT_MINUS, KP_CTRL_MINUS );
                        break;
                     case VK_DIVIDE:
                        hb_gt_wvt_TranslateKey( pWVT, '/', '/', KP_ALT_SLASH, KP_CTRL_SLASH );
                        break;
                  }
               }
               else if( pWVT->EnableShortCuts )
               {
                  return DefWindowProc( pWVT->hWnd, message, wParam, lParam ) != 0;
               }
            }
         }
         break;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int  iScanCode = HIWORD( lParam ) & 0xFF;
         int  c = ( int ) wParam;

         if( ! pWVT->IgnoreWM_SYSCHAR )
         {
            if( bCtrl && iScanCode == 28 )  /* K_CTRL_RETURN */
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, K_CTRL_RETURN );
            }
            else if( bCtrl && ( c >= 1 && c <= 26 ) )  /* K_CTRL_A - Z */
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, K_Ctrl[ c - 1 ] );
            }
            else
            {
               switch( c )
               {
                  /* handle special characters */
                  case VK_BACK:
                     hb_gt_wvt_TranslateKey( pWVT, K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                     break;
                  case VK_TAB:
                     hb_gt_wvt_TranslateKey( pWVT, K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                     break;
                  case VK_RETURN:
                     hb_gt_wvt_TranslateKey( pWVT, K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                     break;
                  case VK_ESCAPE:
                     hb_gt_wvt_AddCharToInputQueue( pWVT, K_ESC );
                     break;
                  default:
                     if( pWVT->inCDP )
                     {
#if defined( UNICODE )
                        c = hb_cdpGetChar( pWVT->inCDP, ( HB_WCHAR ) c );
#else
                        if( c > 0 && c <= 255 && pWVT->keyTransTbl[ c ] )
                           c = pWVT->keyTransTbl[ c ];
#endif
                     }
                     else if( pWVT->CodePage == OEM_CHARSET )
                        c = hb_gt_wvt_key_ansi_to_oem( c );
                     hb_gt_wvt_AddCharToInputQueue( pWVT, c );
                     break;
               }
            }
         }
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE; /* As Suggested by Peter */
         break;
      }

      case WM_SYSCHAR:
         if( ! pWVT->IgnoreWM_SYSCHAR )
         {
            int c, iScanCode = HIWORD( lParam ) & 0xFF;
            switch( iScanCode )
            {
               case  2:
                  c = K_ALT_1;
                  break;
               case  3:
                  c = K_ALT_2;
                  break;
               case  4:
                  c = K_ALT_3;
                  break;
               case  5:
                  c = K_ALT_4;
                  break;
               case  6:
                  c = K_ALT_5;
                  break;
               case  7:
                  c = K_ALT_6;
                  break;
               case  8:
                  c = K_ALT_7;
                  break;
               case  9:
                  c = K_ALT_8;
                  break;
               case 10:
                  c = K_ALT_9;
                  break;
               case 11:
                  c = K_ALT_0;
                  break;
               case 13:
                  c = K_ALT_EQUALS;
                  break;
               case 14:
                  c = K_ALT_BS;
                  break;
               case 16:
                  c = K_ALT_Q;
                  break;
               case 17:
                  c = K_ALT_W;
                  break;
               case 18:
                  c = K_ALT_E;
                  break;
               case 19:
                  c = K_ALT_R;
                  break;
               case 20:
                  c = K_ALT_T;
                  break;
               case 21:
                  c = K_ALT_Y;
                  break;
               case 22:
                  c = K_ALT_U;
                  break;
               case 23:
                  c = K_ALT_I;
                  break;
               case 24:
                  c = K_ALT_O;
                  break;
               case 25:
                  c = K_ALT_P;
                  break;
               case 30:
                  c = K_ALT_A;
                  break;
               case 31:
                  c = K_ALT_S;
                  break;
               case 32:
                  c = K_ALT_D;
                  break;
               case 33:
                  c = K_ALT_F;
                  break;
               case 34:
                  c = K_ALT_G;
                  break;
               case 35:
                  c = K_ALT_H;
                  break;
               case 36:
                  c = K_ALT_J;
                  break;
               case 37:
                  c = K_ALT_K;
                  break;
               case 38:
                  c = K_ALT_L;
                  break;
               case 44:
                  c = K_ALT_Z;
                  break;
               case 45:
                  c = K_ALT_X;
                  break;
               case 46:
                  c = K_ALT_C;
                  break;
               case 47:
                  c = K_ALT_V;
                  break;
               case 48:
                  c = K_ALT_B;
                  break;
               case 49:
                  c = K_ALT_N;
                  break;
               case 50:
                  c = K_ALT_M;
                  break;
               default:
                  c = ( int ) wParam;
                  break;
            }
            hb_gt_wvt_AddCharToInputQueue( pWVT, c );
         }
         pWVT->IgnoreWM_SYSCHAR = HB_FALSE;
   }

   return 0;
}

static LRESULT CALLBACK hb_gt_wvt_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_GTWVT pWVT = hb_gt_wvt_Find( hWnd );

   if( pWVT )
      switch( message )
      {
         case WM_CREATE:
            return 0;
         case WM_SETFOCUS:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );
            hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) hWnd );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) wParam );
            hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_SETFOCUS, pEvParams );
            return 0;
         }
         case WM_KILLFOCUS:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );
            hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) hWnd );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) wParam );
            hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_KILLFOCUS, pEvParams );
            return 0;
         }
         case WM_KEYDOWN:
         case WM_SYSKEYDOWN:
         case WM_CHAR:
         case WM_SYSCHAR:
            return hb_gt_wvt_KeyEvent( pWVT, message, wParam, lParam );
         case WM_RBUTTONDOWN:
         case WM_LBUTTONDOWN:
         case WM_RBUTTONUP:
         case WM_LBUTTONUP:
         case WM_RBUTTONDBLCLK:
         case WM_LBUTTONDBLCLK:
         case WM_MBUTTONDOWN:
         case WM_MBUTTONUP:
         case WM_MBUTTONDBLCLK:
         case WM_MOUSEMOVE:
         case WM_MOUSEWHEEL:
         case WM_NCMOUSEMOVE:
            hb_gt_wvt_MouseEvent( pWVT, message, wParam, lParam );
            return 0;
         case WM_QUERYENDSESSION: /* Closing down computer */
            hb_vmRequestQuit();
            return 0;
         case WM_ENTERIDLE:
            /* FSG - 12/05/2004 - Signal than i'm on idle */
            hb_idleState();
            return 0;
         /* Pritpal Bedi - 06 Jun 2008 */
         case WM_ACTIVATE:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );
            hb_gt_wvt_FireEvent( pWVT, ( LOWORD( wParam ) == WA_INACTIVE ? HB_GTE_KILLFOCUS : HB_GTE_SETFOCUS ), pEvParams );
            SendMessage( hWnd, WM_SIZE, 0, 0 );
            return 0;
         }
#if ! defined( HB_OS_WIN_CE )
         case WM_SIZING:
#endif
         case WM_SIZE:
            return hb_gt_wvt_SizeChanged( pWVT );
         case WM_SYSCOMMAND:
#if ! defined( HB_OS_WIN_CE )
            switch( wParam )
            {
               case SC_MAXIMIZE:
                  /* TODO */
                  break;
            }
#endif
            break;
         case WM_TIMER:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );
            hb_itemPutNI( pEvParams, ( int ) wParam  );
            hb_gt_wvt_FireEvent( pWVT, HB_GTE_TIMER, pEvParams );
            return 0;
         }
         case HB_MSG_NOTIFYICON:
            if( lParam == WM_RBUTTONUP )
            {
               NOTIFYICONDATA tnid;
               tnid.cbSize           = sizeof( NOTIFYICONDATA );
               tnid.hWnd             = hWnd;
               tnid.uID              = HB_ID_NOTIFYICON;
               tnid.uCallbackMessage = HB_MSG_NOTIFYICON;
               tnid.hIcon            = NULL;
               Shell_NotifyIcon( NIM_DELETE, &tnid );
               ShowWindow( hWnd, SW_RESTORE );
            }
            return 0;
         case WM_ENTERMENULOOP:
            hb_gt_wvt_FireMenuEvent( pWVT, 1, ( int ) wParam );
            return 0;
         case WM_EXITMENULOOP:
            hb_gt_wvt_FireMenuEvent( pWVT, 2, ( int ) wParam );
            return 0;
#if ! defined( HB_OS_WIN_CE )
         case WM_MOUSEHOVER:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 6 );
            hb_arraySetNI( pEvParams, 1, message );
            hb_arraySetNI( pEvParams, 2, 0 );
            hb_arraySetNI( pEvParams, 3, LOWORD( lParam ) );
            hb_arraySetNI( pEvParams, 4, HIWORD( lParam ) );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );
            return 0;
         }
         case WM_MOUSELEAVE:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 2 );
            hb_arraySetNI( pEvParams, 1, message );
            hb_arraySetNI( pEvParams, 2, 0 );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_MOUSE, pEvParams );

            pWVT->bTracking = HB_FALSE;
            break;
         }
#endif
         case WM_COMMAND:
            if( ( int ) lParam == 0 )
            {
               /* Menu command */
               if( HIWORD( wParam ) == 0 )
               {
                  hb_gt_wvt_FireMenuEvent( pWVT, 0, ( int ) LOWORD( wParam ) );
               }
            }
            else
            {
               PHB_ITEM pEvParams = hb_itemNew( NULL );
               int iLo, iHi;

               iLo = LOWORD( wParam );
               iHi = HIWORD( wParam );

               hb_arrayNew( pEvParams, 3 );
               hb_arraySetNI( pEvParams, 1, iHi );                              /* Notification Code  */
               hb_arraySetNI( pEvParams, 2, iLo );                              /* Control identifier */
               hb_arraySetNInt( pEvParams, 3, ( HB_PTRDIFF ) ( HWND ) lParam ); /* Controls hWnd      */

               hb_gt_wvt_FireEvent( pWVT, HB_GTE_COMMAND, pEvParams );
            }
            return 0;
         case WM_NOTIFY:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 2 );

            hb_arraySetNI( pEvParams, 1, ( int ) wParam );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_NOTIFY, pEvParams );
            break;
         }
         case WM_CLOSE:  /* Clicked 'X' on system menu */
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );
            if( hb_gt_wvt_FireEvent( pWVT, HB_GTE_CLOSE, pEvParams ) == 0 )
            {
               hb_gt_wvt_AddCharToInputQueue( pWVT, 27 );      /* post ESCape */
            }
            return 0;
         }
         case WM_QUIT:
         case WM_DESTROY:
            return 0;
         case WM_CTLCOLORLISTBOX:
         case WM_CTLCOLORMSGBOX:
         case WM_CTLCOLOREDIT:
         case WM_CTLCOLORBTN:
         case WM_CTLCOLORDLG:
         case WM_CTLCOLORSCROLLBAR:
         case WM_CTLCOLORSTATIC:
         {
            int iResult;
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 2 );

            hb_arraySetNInt( pEvParams, 1, ( HB_PTRDIFF ) wParam );
            hb_arraySetNInt( pEvParams, 2, ( HB_PTRDIFF ) lParam );

            iResult = hb_gt_wvt_FireEvent( pWVT, HB_GTE_CTLCOLOR, pEvParams );

            if( iResult == 0 )
               break;
            else
               return iResult;
         }
         case WM_HSCROLL:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );

            hb_arraySetNL( pEvParams, 1, ( long ) LOWORD( wParam ) );
            hb_arraySetNL( pEvParams, 2, ( long ) HIWORD( wParam ) );
            hb_arraySetNInt( pEvParams, 3, ( HB_MAXINT ) ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_HSCROLL, pEvParams );
            return 0;
         }
         case WM_VSCROLL:
         {
            PHB_ITEM pEvParams = hb_itemNew( NULL );

            hb_arrayNew( pEvParams, 3 );

            hb_arraySetNL( pEvParams, 1, ( long ) LOWORD( wParam ) );
            hb_arraySetNL( pEvParams, 2, ( long ) HIWORD( wParam ) );
            hb_arraySetNInt( pEvParams, 3, ( HB_MAXINT ) ( HB_PTRDIFF ) lParam );

            hb_gt_wvt_FireEvent( pWVT, HB_GTE_VSCROLL, pEvParams );
            return 0;
         }
      }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

static WPARAM hb_gt_wvt_ProcessMessages( PHB_GTWVT pWVT )
{
   MSG msg;

   HB_SYMBOL_UNUSED( pWVT );
   while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
   {
      TranslateMessage( &msg );
      DispatchMessage( &msg );
   }
   return msg.wParam;
}

static HWND hb_gt_wvt_CreateWindow( PHB_GTWVT pWVT )
{
   HWND hWnd, hWndParent;

   hWndParent = NULL;

   if( pWVT->pPP->bConfigured )
   {
      PHB_GT pGTp = hb_gt_ItemBase( pWVT->pPP->pParentGT );
      if( pGTp )
      {
         PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
         hWndParent = pWVTp->hWnd;
         hb_gt_BaseFree( pGTp );

         if( hWndParent )
         {
            RECT rc;
            POINT pt;

            GetClientRect( hWndParent, &rc );
            pt.x = rc.left + pWVT->pPP->x;
            pt.y = rc.top  + pWVT->pPP->y;

            ClientToScreen( hWndParent, &pt );

            pWVT->pPP->x = pt.x;
            pWVT->pPP->y = pt.y;
         }
      }
      if( pWVT->pPP->y < 0 )
         pWVT->CentreWindow = HB_TRUE;
   }

   hWnd = CreateWindowEx(
      pWVT->pPP->exStyle,                                   /* extended style */
      s_szClassName,                                        /* classname      */
      pWVT->lpWindowTitle,                                  /* window name    */
      pWVT->pPP->style,                                     /* style          */
      pWVT->pPP->x,                                         /* x              */
      pWVT->pPP->y,                                         /* y              */
      pWVT->pPP->width,                                     /* width          */
      pWVT->pPP->height,                                    /* height         */
      hWndParent,                                           /* window parent  */
      NULL,                                                 /* menu           */
      pWVT->hInstance,                                      /* instance       */
      NULL );                                               /* lpParam        */

   ShowWindow( pWVT->hWnd, pWVT->pPP->bVisible ? SW_SHOWNORMAL : SW_HIDE );
   UpdateWindow( pWVT->hWnd );

   return hWnd;
}

static HB_BOOL hb_gt_wvt_CreateConsoleWindow( PHB_GTWVT pWVT )
{
   if( ! pWVT->hWnd )
   {
      RECT rc = { 0, 0, 0, 0 };

      pWVT->hWnd = hb_gt_wvt_CreateWindow( pWVT );
      if( ! pWVT->hWnd )
         hb_errInternal( 10001, "Failed to create WVT window", NULL, NULL );

      GetClientRect( pWVT->hWnd, &rc );
      pWVT->width = rc.right - rc.left;
      pWVT->height = rc.bottom - rc.top;

      /* Set icon */
      if( pWVT->hIcon )
      {
         SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon );
         SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon );
      }
   }

   return HB_TRUE;
}

/* ********************************************************************** */
/*
 * GT Specific Functions
 */
/* ********************************************************************** */

static void hb_gt_wvt_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HANDLE    hInstance;
   int       iCmdShow;
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Init(%p,%p,%p,%p)", pGT, ( void * ) ( HB_PTRDIFF ) hFilenoStdin, ( void * ) ( HB_PTRDIFF ) hFilenoStdout, ( void * ) ( HB_PTRDIFF ) hFilenoStderr ) );

   if( ! hb_winmainArgGet( &hInstance, NULL, &iCmdShow ) )
      hb_errInternal( 10001, "It's not a GUI program", NULL, NULL );

   pWVT = hb_gt_wvt_New( pGT, ( HINSTANCE ) hInstance, iCmdShow );
   if( ! pWVT )
      hb_errInternal( 10001, "Maximum number of WVT windows reached, cannot create another one", NULL, NULL );

   HB_GTLOCAL( pGT ) = ( void * ) pWVT;

   /* SUPER GT initialization */
   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, pWVT->ROWS, pWVT->COLS );
   HB_GTSELF_SEMICOLD( pGT );

   #if 0
   hb_gt_wvt_CreateConsoleWindow( pWVT );
   hb_gt_wvt_ProcessMessages( pWVT );
   #endif
}

/* ********************************************************************** */

static void hb_gt_wvt_Exit( PHB_GT pGT )
{
   PHB_GTWVT pWVT;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Exit(%p)", pGT ) );

   pWVT = HB_GTWVT_GET( pGT );
   HB_GTSUPER_EXIT( pGT );

   if( pWVT )
   {
      hb_gt_wvt_Free( pWVT );
   }
}

/* ********************************************************************** */

static int hb_gt_wvt_ReadKey( PHB_GT pGT, int iEventMask )
{
   PHB_GTWVT pWVT;
   int  c = 0;
   HB_BOOL fKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_ReadKey(%p,%d)", pGT, iEventMask ) );

   HB_SYMBOL_UNUSED( iEventMask ); /* we ignore the eventmask! */

   pWVT = HB_GTWVT_GET( pGT );

   if( pWVT->hWnd ) /* Is the window already open */
      hb_gt_wvt_ProcessMessages( pWVT );

   fKey = hb_gt_wvt_GetCharFromInputQueue( pWVT, &c );

   return fKey ? c : 0;
}

/* ********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_wvt_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   HB_SYMBOL_UNUSED( pGT );

   hb_gt_winapi_tone( dFrequency, dDuration );
}

/* ********************************************************************** */

static HB_BOOL hb_gt_wvt_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   PHB_GTWVT pWVT;
   int iVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_wvt_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   pWVT = HB_GTWVT_GET( pGT );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISGRAPHIC:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;
      case HB_GTI_ISUNICODE:
#if defined( UNICODE )
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
#else
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
#endif
         break;
      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_INPUT_HANDLE ) );
         break;
      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_OUTPUT_HANDLE ) );
         break;
      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult,
                              ( HB_PTRDIFF ) GetStdHandle( STD_ERROR_HANDLE ) );
         break;
      case HB_GTI_SETFONT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
         {
            PHB_ITEM pSome;

            pSome = hb_arrayGetItemPtr( pInfo->pNewVal, 1 );
            if( hb_itemType( pSome ) & HB_IT_STRING )
            {
               hb_strncpy( pWVT->fontFace, hb_itemGetCPtr( pSome ), sizeof( pWVT->fontFace ) - 1 );
            }
            pSome = hb_arrayGetItemPtr( pInfo->pNewVal, 2 );
            if( hb_itemType( pSome ) & HB_IT_NUMERIC )
            {
               pWVT->fontHeight = hb_arrayGetNI( pInfo->pNewVal, 2 );
            }
            pSome = hb_arrayGetItemPtr( pInfo->pNewVal, 3 );
            if( hb_itemType( pSome ) & HB_IT_NUMERIC )
            {
               pWVT->fontWidth = hb_arrayGetNI( pInfo->pNewVal, 3 );
            }
            pSome = hb_arrayGetItemPtr( pInfo->pNewVal, 4 );
            if( hb_itemType( pSome ) & HB_IT_NUMERIC )
            {
               pWVT->fontWeight = hb_arrayGetNI( pInfo->pNewVal, 4 );
            }
            pSome = hb_arrayGetItemPtr( pInfo->pNewVal, 5 );
            if( hb_itemType( pSome ) & HB_IT_NUMERIC )
            {
               pWVT->fontQuality = hb_arrayGetNI( pInfo->pNewVal, 5 );
            }
         }
         break;
      case HB_GTI_FONTSIZE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontHeight );

         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 )
         {
            pWVT->fontHeight = iVal;
         }
         break;
      case HB_GTI_FONTWIDTH:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWidth );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontWidth = hb_itemGetNI( pInfo->pNewVal );

         break;
      case HB_GTI_FONTNAME:
         pInfo->pResult = hb_itemPutC( pInfo->pResult, pWVT->fontFace );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_strncpy( pWVT->fontFace, hb_itemGetCPtr( pInfo->pNewVal ), sizeof( pWVT->fontFace ) - 1 );
         }
         break;
      case HB_GTI_FONTWEIGHT:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontWeight );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontWeight = hb_itemGetNI( pInfo->pNewVal );

         break;
      case HB_GTI_FONTQUALITY:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->fontQuality );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            pWVT->fontQuality = hb_itemGetNI( pInfo->pNewVal );

         break;
      case HB_GTI_SCREENHEIGHT:
      {
         RECT rc;
         GetClientRect( pWVT->hWnd, &rc );

         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rc.bottom - rc.top );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && pWVT->hWnd )
         {
            MoveWindow( pWVT->hWnd, 0, 0, rc.right - rc.left, iVal, TRUE );
         }
         break;
      }
      case HB_GTI_SCREENWIDTH:
      {
         RECT rc;
         GetClientRect( pWVT->hWnd, &rc );

         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rc.right - rc.left );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && pWVT->hWnd )
         {
            MoveWindow( pWVT->hWnd, 0, 0, iVal, rc.bottom - rc.top, TRUE );
         }
         break;
      }
      case HB_GTI_DESKTOPWIDTH:
      {
         RECT rDesk;
         HWND hDesk;

         hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.right - rDesk.left );
         break;
      }
      case HB_GTI_DESKTOPHEIGHT:
      {
         RECT rDesk;
         HWND hDesk = GetDesktopWindow();
         GetWindowRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rDesk.bottom - rDesk.top );
         break;
      }
      case HB_GTI_DESKTOPCOLS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        ( rDesk.right - rDesk.left ) / pWVT->fontWidth );
         break;
      }
      case HB_GTI_DESKTOPROWS:
      {
         RECT rDesk;
         HWND hDesk;
         hDesk = GetDesktopWindow();
         GetClientRect( hDesk, &rDesk );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult,
                                        ( rDesk.bottom - rDesk.top ) / pWVT->fontHeight );
         break;
      }
      case HB_GTI_WINTITLE:
         pInfo->pResult = HB_ITEMPUTSTR( pInfo->pResult, pWVT->lpWindowTitle );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            hb_strfree( pWVT->hWindowTitle );
            pWVT->lpWindowTitle = HB_ITEMGETSTR( pInfo->pNewVal, &pWVT->hWindowTitle, NULL );
            if( pWVT->hWnd )
               SetWindowText( pWVT->hWnd, pWVT->lpWindowTitle );
         }
         break;
      case HB_GTI_CODEPAGE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, pWVT->CodePage );
         iVal = hb_itemGetNI( pInfo->pNewVal );
         if( iVal > 0 && iVal != pWVT->CodePage )
         {
            pWVT->CodePage = iVal;
         }
         break;
      case HB_GTI_ICONFILE:
         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING ) )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;
            void * hImageName;

            pWVT->bIconToFree = HB_TRUE;
            pWVT->hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL,
                                               HB_ITEMGETSTR( pInfo->pNewVal, &hImageName, NULL ),
                                               IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTSIZE );
            hb_strfree( hImageName );
            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hIcon );
         break;

      case HB_GTI_ICONRES:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;
            void * hIconName;

            pWVT->bIconToFree = HB_FALSE;
            pWVT->hIcon = LoadIcon( pWVT->hInstance,
                                    HB_ITEMGETSTR( pInfo->pNewVal, &hIconName, NULL ) );
            hb_strfree( hIconName );
            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         else if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
         {
            HICON hIconToFree = pWVT->bIconToFree ? pWVT->hIcon : NULL;

            pWVT->bIconToFree = HB_FALSE;
            pWVT->hIcon = LoadIcon( pWVT->hInstance,
                                    MAKEINTRESOURCE( ( HB_MAXINT )
                                         hb_itemGetNInt( pInfo->pNewVal ) ) );

            if( pWVT->hWnd )
            {
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_SMALL, ( LPARAM ) pWVT->hIcon ); /* Set Title Bar Icon */
               SendNotifyMessage( pWVT->hWnd, WM_SETICON, ICON_BIG  , ( LPARAM ) pWVT->hIcon ); /* Set Task List Icon */
            }

            if( hIconToFree )
               DestroyIcon( hIconToFree );
         }
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hIcon );
         break;
      case HB_GTI_VIEWMAXWIDTH:
      {
         RECT rc;
         GetClientRect( pWVT->hWnd, &rc );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rc.right - rc.left / pWVT->fontWidth );
         break;
      }
      case HB_GTI_VIEWMAXHEIGHT:
      {
         RECT rc;
         GetClientRect( pWVT->hWnd, &rc );
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, rc.bottom - rc.top / pWVT->fontHeight );
         break;
      }
      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_winapi_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_winapi_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;
      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
#if defined( UNICODE )
            hb_gt_winapi_setClipboard( CF_UNICODETEXT, pInfo->pNewVal );
#else
            hb_gt_winapi_setClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( pWVT->CodePage == OEM_CHARSET ?
                                       CF_OEMTEXT : CF_TEXT, pInfo->pResult );
#endif
         }
         break;
      case HB_GTI_CURSORBLINKRATE:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, GetCaretBlinkTime() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            SetCaretBlinkTime( hb_itemGetNI( pInfo->pNewVal ) );
         break;
      case HB_GTI_SCREENSIZE:
      {
         RECT rc;
         GetClientRect( pWVT->hWnd, &rc );

         if( ! pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );

         hb_arrayNew( pInfo->pResult, 2 );
         hb_arraySetNI( pInfo->pResult, 2, rc.bottom - rc.top );
         hb_arraySetNI( pInfo->pResult, 1, rc.right - rc.left );

         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) && hb_arrayLen( pInfo->pNewVal ) == 2 )
         {
            int iX, iY;
            iY = hb_arrayGetNI( pInfo->pNewVal, 2 );
            iX = hb_arrayGetNI( pInfo->pNewVal, 1 );
            if( iY > 0 )
            {
               MoveWindow( pWVT->hWnd, 0, 0, iX, iY, TRUE );
            }
         }
         break;
      }
      case HB_GTI_RESIZABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bResizable );
         if( pInfo->pNewVal )
         {
            HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bResizable )
            {
               pWVT->bResizable = bNewValue;
               if( pWVT->hWnd )
               {
#if ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) || defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
                  LONG style;
#else
                  LONG_PTR style;
#endif
                  if( pWVT->bResizable )
                     style = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME;
                  else
                     style = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_BORDER;

#if ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 || defined( HB_OS_WIN_CE ) ) || defined( __DMC__ ) ) && ! defined( HB_ARCH_64BIT )
                  SetWindowLong( pWVT->hWnd, GWL_STYLE, style );
#else
                  SetWindowLongPtr( pWVT->hWnd, GWL_STYLE, style );
#endif
#if defined( HB_OS_WIN_CE ) && ! defined( __MINGW32CE__ )
   #define SWP_DEFERERASE  0
#endif
                  SetWindowPos( pWVT->hWnd, NULL, 0, 0, 0, 0,
                                SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_DEFERERASE );
                  ShowWindow( pWVT->hWnd, SW_HIDE );
                  ShowWindow( pWVT->hWnd, SW_SHOWNORMAL );
               }
            }
         }
         break;
      case HB_GTI_CLOSABLE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->bClosable );
         if( pInfo->pNewVal )
         {
            HB_BOOL bNewValue = hb_itemGetL( pInfo->pNewVal );
            if( bNewValue != pWVT->bClosable )
            {
               if( pWVT->hWnd )
               {
                  HMENU hSysMenu = GetSystemMenu( pWVT->hWnd, FALSE );
                  if( hSysMenu )
                  {
                     EnableMenuItem( hSysMenu, SC_CLOSE, MF_BYCOMMAND | ( bNewValue ? MF_ENABLED : MF_GRAYED ) );
                     pWVT->bClosable = bNewValue;
                  }
               }
               else
                  pWVT->bClosable = bNewValue;
            }
         }
         break;
      case HB_GTI_SETPOS_XY:
      case HB_GTI_SETPOS_ROWCOL:
         if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC ) &&
                           ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC ) )
         {
            int x, y;
            RECT rect = { 0, 0, 0, 0 };
            GetWindowRect( pWVT->hWnd, &rect );

            if( iType == HB_GTI_SETPOS_ROWCOL )
            {
               y = hb_itemGetNI( pInfo->pNewVal ) * pWVT->fontHeight;
               x = hb_itemGetNI( pInfo->pNewVal2 ) * pWVT->fontWidth;
            }
            else
            {
               x = hb_itemGetNI( pInfo->pNewVal );
               y = hb_itemGetNI( pInfo->pNewVal2 );
            }
            hb_retl( SetWindowPos( pWVT->hWnd, NULL,
                                   x,
                                   y,
                                   rect.right - rect.left,
                                   rect.bottom - rect.top,
                                   SWP_NOSIZE | SWP_NOZORDER ) );
         }
         break;
      case HB_GTI_SPEC:
      {
         int iMessage = hb_itemGetNI( pInfo->pNewVal );
         switch( iMessage )
         {
            case HB_GTS_WINDOWHANDLE:
               if( pWVT->hWnd )
                  pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_PTRDIFF ) pWVT->hWnd );
               break;

            case HB_GTS_CENTERWINDOW:
               pInfo->pResult = hb_itemPutL( pInfo->pResult, pWVT->CentreWindow );
               if( pInfo->pNewVal2 )
               {
                  pWVT->CentreWindow = hb_arrayGetL( pInfo->pNewVal2, 1 );
                  if( hb_arrayGetL( pInfo->pNewVal2, 2 ) )
                  {
                     if( pWVT->hWnd )
                     {
                        RECT rDesk, rApp;
                        HWND hDesk;
                        int iLeft, iTop;

                        hDesk = GetDesktopWindow();
                        GetClientRect( hDesk, &rDesk );

                        GetWindowRect( pWVT->hWnd, &rApp );

                        iLeft = ( ( rDesk.right - rDesk.left ) - ( rApp.right - rApp.left ) ) / 2;
                        iTop  = ( ( rDesk.bottom - rDesk.top ) - ( rApp.bottom - rApp.top ) ) / 2;

                        MoveWindow( pWVT->hWnd, iLeft, iTop, ( rApp.right - rApp.left ), ( rApp.bottom - rApp.top ), FALSE );
                     }
                  }
               }
               break;
            case HB_GTS_PROCESSMESSAGES:
               if( pWVT->hWnd )
                  hb_gt_wvt_ProcessMessages( pWVT );
               break;
            case HB_GTS_KEYBOARD:
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                  hb_gt_wvt_AddCharToInputQueue( pWVT, hb_itemGetNI( pInfo->pNewVal2 ) );
               break;
            case HB_GTS_RESETWINDOW:
               if( pWVT->hWnd )
                  UpdateWindow( pWVT->hWnd );   /* hb_gt_wvt_ResetWindowSize( pWVT ); */
               break;
            case HB_GTS_SETTIMER:
               if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY && pWVT->hWnd )
                  SetTimer( pWVT->hWnd, hb_arrayGetNI( pInfo->pNewVal2, 1 ), hb_arrayGetNI( pInfo->pNewVal2, 2 ), NULL );
               break;
            case HB_GTS_KILLTIMER:
               if( pWVT->hWnd )
               {
                  if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                     KillTimer( pWVT->hWnd, hb_itemGetNI( pInfo->pNewVal2 ) );
               }
               break;
            case HB_GTS_SETPOSITION:
               if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY ) )
               {
                  RECT rect = { 0, 0, 0, 0 };
                  GetWindowRect( pWVT->hWnd, &rect );
                  hb_retl( SetWindowPos( pWVT->hWnd, NULL,
                                         hb_arrayGetNI( pInfo->pNewVal2, 1 ),
                                         hb_arrayGetNI( pInfo->pNewVal2, 2 ),
                                         rect.right - rect.left,
                                         rect.bottom - rect.top,
                                         SWP_NOSIZE | SWP_NOZORDER ) );
               }
               break;
            case HB_GTS_SHOWWINDOW:
               if( ! pWVT->hWnd )
               {
                  hb_gt_wvt_CreateConsoleWindow( pWVT );
                  hb_gt_wvt_ProcessMessages( pWVT );
               }
               if( pWVT->hWnd )
               {
                  if( hb_itemType( pInfo->pNewVal2 ) & HB_IT_NUMERIC )
                  {
                     ShowWindow( pWVT->hWnd, hb_itemGetNI( pInfo->pNewVal2 ) );
                     UpdateWindow( pWVT->hWnd );
                  }
               }
               break;
            case HB_GTS_UPDATEWINDOW:
               if( pWVT->hWnd )
                  UpdateWindow( pWVT->hWnd );
               break;
            case HB_GTS_SYSTRAYICON:
               if( pWVT->hWnd && ( hb_itemType( pInfo->pNewVal2 ) & HB_IT_ARRAY ) )
               {
                  int            mode = hb_arrayGetNI( pInfo->pNewVal2, 1 );
                  int            iIconType = hb_arrayGetNI( pInfo->pNewVal2, 2 );
                  HICON          hIcon = 0;
                  NOTIFYICONDATA tnid;
                  void * hIconName;

                  if( iIconType == 0 )
                  {
                     hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL,
                                             HB_ARRAYGETSTR( pInfo->pNewVal2, 3, &hIconName, NULL ),
                                                     IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
                     hb_strfree( hIconName );
                  }
                  else if( iIconType == 1 )
                  {
                     hIcon = LoadIcon( pWVT->hInstance, HB_ARRAYGETSTR( pInfo->pNewVal2, 3, &hIconName, NULL ) );
                     hb_strfree( hIconName );
                  }
                  else if( iIconType == 2 )
                  {
                     hIcon = LoadIcon( pWVT->hInstance,
                                          MAKEINTRESOURCE( ( HB_MAXINT )
                                                      hb_arrayGetNInt( pInfo->pNewVal2, 3 ) ) );
                  }
                  tnid.cbSize           = sizeof( NOTIFYICONDATA );
                  tnid.hWnd             = pWVT->hWnd;
                  tnid.uID              = HB_ID_NOTIFYICON;
                  tnid.uFlags           = NIF_MESSAGE | NIF_ICON | NIF_TIP;
                  tnid.uCallbackMessage = HB_MSG_NOTIFYICON;
                  tnid.hIcon            = hIcon;

                  HB_STRNCPY( tnid.szTip, HB_ARRAYGETSTR( pInfo->pNewVal2, 4, &hIconName, NULL ), HB_SIZEOFARRAY( tnid.szTip ) - 1 );
                  hb_strfree( hIconName );

                  Shell_NotifyIcon( mode, &tnid );

                  if( hIcon )
                     DestroyIcon( hIcon );

                  ShowWindow( pWVT->hWnd, SW_HIDE );
               }
               break;

            case HB_GTS_WNDSTATE:
               if( pWVT->hWnd )
               {
                  int iSpec = hb_itemGetNI( pInfo->pNewVal2 );

                  switch( iSpec )
                  {
                     case HB_GTS_WS_SETONTOP:
                     {
                        RECT rect = { 0, 0, 0, 0 };
                        GetWindowRect( pWVT->hWnd, &rect );
                        hb_retl( SetWindowPos( pWVT->hWnd, HWND_TOPMOST,
                                               rect.left,
                                               rect.top,
                                               0,
                                               0,
                                               SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
                        break;
                     }
                     case HB_GTS_WS_SETASNORMAL:
                     {
                        RECT rect = { 0, 0, 0, 0 };
                        GetWindowRect( pWVT->hWnd, &rect );
                        hb_retl( SetWindowPos( pWVT->hWnd, HWND_NOTOPMOST,
                                               rect.left,
                                               rect.top,
                                               0,
                                               0,
                                               SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE ) );
                        break;
                     }

                     case HB_GTS_WS_MINIMIZED:
#if ! defined( HB_OS_WIN_CE )
                        SendNotifyMessage( pWVT->hWnd, WM_SYSCOMMAND, SC_MINIMIZE, 0 );
#endif
                        break;

                     case HB_GTS_WS_MAXIMIZED:
                        if( pWVT->bResizable )
                        {
#if ! defined( HB_OS_WIN_CE )
                           SendNotifyMessage( pWVT->hWnd, WM_SYSCOMMAND, SC_MAXIMIZE, 0 );
#endif
                        }
                        else
                           ShowWindow( pWVT->hWnd, SW_RESTORE );
                        break;

                     case HB_GTS_WS_HIDDEN:
                        ShowWindow( pWVT->hWnd, SW_HIDE );
                        break;

                     case HB_GTS_WS_NORMAL:
                        ShowWindow( pWVT->hWnd, SW_SHOWNORMAL );
                        break;
                  }
               }
               break;
            case HB_GTS_FACTOR:
               if( pWVT->hWnd )
               {
#if ( _WIN32_WINNT >= 0x0500 )
                  if( pWVT->pfnLayered )
                  {
                     SetWindowLong( pWVT->hWnd,
                                    GWL_EXSTYLE,
                                    GetWindowLong( pWVT->hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );

                     pWVT->pfnLayered( pWVT->hWnd,
                                       RGB( 255, 255, 255 ),
                                       ( BYTE ) hb_itemGetNI( pInfo->pNewVal2 ),
                                       /*LWA_COLORKEY|*/ LWA_ALPHA );
                  }
#endif
               }
               break;
         }
         break;
      }
      case HB_GTI_PRESPARAMS:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY )
         {
            HB_SIZE iParam = hb_arrayLen( pInfo->pNewVal );
            if( iParam == HB_GTI_PP_SIZE )
            {
               PHB_ITEM pSome;

               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_EXSTYLE );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->exStyle = ( DWORD ) hb_itemGetNInt( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_STYLE );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->style = ( DWORD ) hb_itemGetNInt( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_X );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->x = hb_itemGetNI( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_Y );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->y = hb_itemGetNI( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_WIDTH );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->width = hb_itemGetNI( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_HEIGHT );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->height = hb_itemGetNI( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_PARENT );
               if( hb_itemType( pSome ) & HB_IT_POINTER )
               {
                  pWVT->pPP->pParentGT = hb_itemNew( hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_PARENT ) );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_VISIBLE );
               if( hb_itemType( pSome ) & HB_IT_LOGICAL )
               {
                  pWVT->pPP->bVisible = hb_itemGetL( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_ROWCOLS );
               if( hb_itemType( pSome ) & HB_IT_LOGICAL )
               {
                  pWVT->pPP->bRowCols = hb_itemGetL( pSome );
               }
               pSome = hb_arrayGetItemPtr( pInfo->pNewVal, HB_GTI_PP_WNDTYPE );
               if( hb_itemType( pSome ) & HB_IT_NUMERIC )
               {
                  pWVT->pPP->iWndType = hb_itemGetNI( pSome );
               }

               /* Flag that caller configured itself */
               pWVT->pPP->bConfigured = HB_TRUE;
               pWVT->CentreWindow = HB_FALSE;
            }
         }
         /* Only possible when it is WvgDialog() Window */
         if( pWVT->hWnd && pWVT->pPP->bConfigured )
         {
            MoveWindow( pWVT->hWnd, pWVT->pPP->x, pWVT->pPP->y, pWVT->pPP->width, pWVT->pPP->height, TRUE );
         }
         break;
      case HB_GTI_ENABLE:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            EnableWindow( pWVTp->hWnd, TRUE );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      case HB_GTI_DISABLE:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            EnableWindow( pWVTp->hWnd, FALSE );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      case HB_GTI_SETFOCUS:
      {
         PHB_GT pGTp = hb_gt_ItemBase( pInfo->pNewVal );
         if( pGTp )
         {
            PHB_GTWVT pWVTp = HB_GTWVT_GET( pGTp );
            SetFocus( pWVTp->hWnd );
            hb_gt_BaseFree( pGTp );
         }
         break;
      }
      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }
   return HB_TRUE;
}

/* ********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init = hb_gt_wvt_Init;
   pFuncTable->Exit = hb_gt_wvt_Exit;
   pFuncTable->Tone = hb_gt_wvt_Tone;
   pFuncTable->Info = hb_gt_wvt_Info;

   pFuncTable->ReadKey = hb_gt_wvt_ReadKey;

   return HB_TRUE;
}

/* ********************************************************************** */

#include "hbgtreg.h"

/* ********************************************************************** */
