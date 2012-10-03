/*
 * $Id$
 */

/*
 * Video subsystem for Win32 using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw draw functions
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004/01/26 08:14:07 vouchcac Exp =
 *
 * Harbour Project source code:
 * Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * See doc/license.txt for licensing terms.
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

#include "hbgtwvw.h"

HB_FUNC( WVW_YESCLOSE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HMENU  hMenu = GetSystemMenu( pWindowData->hWnd, FALSE);

   if (hMenu)
   {
      AppendMenu( hMenu, SC_CLOSE, MF_BYCOMMAND, "");
      DrawMenuBar( pWindowData->hWnd );
   }
}
HB_FUNC( WIN_SENDMESSAGE )
{
   char *cText = NULL;

   if( HB_ISBYREF( 4 ) )
   {
      cText = ( char* ) hb_xgrab( hb_parcsiz( 4 ) );
      hb_xmemcpy( cText, hb_parcx( 4 ), hb_parcsiz( 4 ) );
   }

   hb_retnl( ( ULONG ) SendMessage( ( HWND ) HB_PARHANDLE( 1 ),
                                    ( UINT ) hb_parni( 2 ),
                                    ( HB_ISNIL( 3 ) ? 0 : ( WPARAM ) hb_parnl( 3 ) ),
                                    ( HB_ISNIL( 4 ) ? 0 : ( HB_ISBYREF( 4 ) ? ( LPARAM ) ( LPSTR ) cText :
                                       ( HB_ISCHAR( 4 ) ? ( LPARAM )( LPSTR ) hb_parcx( 4 ) :
                                           ( LPARAM ) hb_parnl( 4 ) ) ) ) )
           );

   if ( HB_ISBYREF( 4 ) )
   {
      hb_storclen( cText, hb_parcsiz( 4 ), 4 );
      hb_xfree( cText );
   }
}



HB_FUNC( WIN_SENDDLGITEMMESSAGE )
{
   char     *cText;
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );

   if( pText )
   {
      cText = ( char* ) hb_xgrab( hb_itemGetCLen( pText ) + 1 );
      hb_xmemcpy( cText, hb_itemGetCPtr( pText ), hb_itemGetCLen( pText ) + 1 );
   }
   else
   {
      cText = NULL;
   }

   hb_retnl( ( LONG ) SendDlgItemMessage( ( HWND ) HB_PARHANDLE( 1 ) ,
                                          ( int  ) hb_parni( 2 ) ,
                                          ( UINT ) hb_parni( 3 ) ,
                                          ( HB_ISNIL( 4 ) ? 0 : ( WPARAM ) hb_parnl( 4 ) ),
                                          ( cText ? ( LPARAM ) cText : ( LPARAM ) hb_parnl( 5 ) )
                                        )
           );

  if( pText )
  {
     hb_storclen( cText, hb_itemGetCLen( pText ), 5 ) ;
  }

  if( cText )
  {
     hb_xfree( cText );
  }
}

/*
 *
 *  WIN_SetTimer( hWnd, nIdentifier, nTimeOut )
 */

HB_FUNC( WIN_SETTIMER )
{
   hb_retl( SetTimer( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL ) );
}



HB_FUNC( WIN_SETFOCUS )
{
   SetFocus( ( HWND ) HB_PARHANDLE( 1 ) );
}



HB_FUNC( WIN_SETTEXTCOLOR )
{
   hb_retnl( ( ULONG ) SetTextColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}



HB_FUNC( WIN_SETBKCOLOR )
{
   hb_retnl( ( ULONG ) SetBkColor( ( HDC ) HB_PARHANDLE( 1 ), ( COLORREF ) hb_parnl( 2 ) ) );
}



HB_FUNC( WVW_SETBKMODE )
{
   hb_retni( ( int ) SetBkMode( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}



HB_FUNC( WIN_GETSTOCKOBJECT )
{
   hb_retnl( ( ULONG ) GetStockObject( hb_parnl( 1 ) ) );
}



HB_FUNC( WIN_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}



HB_FUNC( WIN_SELECTOBJECT )
{
   hb_retnl( ( ULONG ) SelectObject( ( HDC ) HB_PARHANDLE( 1 ), ( HGDIOBJ ) HB_PARHANDLE( 2 ) ) );
}



HB_FUNC( WIN_MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}



HB_FUNC( WIN_GETDIALOGBASEUNITS )
{
   hb_retnl( ( LONG ) GetDialogBaseUnits() ) ;
}






HB_FUNC( WIN_SETDLGITEMTEXT )
{
   SetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parc( 3 ) );
}



HB_FUNC( WIN_GETDLGITEMTEXT )
{
   USHORT iLen = (USHORT) SendMessage( (HWND)GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) , (UINT)WM_GETTEXTLENGTH, (WPARAM)0, (LPARAM)0 ) + 1 ;
   char *cText = ( char* ) hb_xgrab( iLen +1  );

   GetDlgItemText( ( HWND ) HB_PARHANDLE( 1 ),
                   hb_parni( 2 ),
                   ( LPTSTR ) cText,
                   iLen
                 );

   hb_retc( cText );
   hb_xfree( cText );
}



HB_FUNC( WIN_CHECKDLGBUTTON )
{
   hb_retl( CheckDlgButton( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ),
                            HB_ISNUM( 3 ) ? hb_parni( 3 ) : ( UINT ) hb_parl( 3 ) ) );
}



HB_FUNC( WIN_ISDLGBUTTONCHECKED )
{
   hb_retni( IsDlgButtonChecked( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) ) ;
}



HB_FUNC( WIN_CHECKRADIOBUTTON )
{
    hb_retl( CheckRadioButton( ( HWND ) HB_PARHANDLE( 1 ),
                                        hb_parni( 2 ),
                                        hb_parni( 3 ),
                                        hb_parni( 4 )
                              ) );
}



HB_FUNC( WIN_GETDLGITEM )
{
   hb_retnl( ( ULONG ) GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ) );
}



HB_FUNC( WIN_MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) HB_PARHANDLE( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), HB_ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) ) ;
}



HB_FUNC( WIN_INVALIDATERECT )
{
   InvalidateRect( ( HWND ) HB_PARHANDLE( 1 ), NULL, TRUE );
}

/*
 *
 *  Win_LoadIcon( ncIcon )
 */

HB_FUNC( WIN_LOADICON )
{
   HICON hIcon;

   if ( HB_ISNUM( 1 ) )
   {
      hIcon = LoadIcon( hb_getWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
   }
   else
   {
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 1 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
   }

   hb_retnl( ( ULONG ) hIcon ) ;
}

/*
 *
 *  Win_LoadImage( ncImage, nSource ) -> hImage
 *    nSource == 0 ResourceIdByNumber
 *    nSource == 0 ResourceIdByName
 *    nSource == 0 ImageFromDiskFile
 */

HB_FUNC( WIN_LOADIMAGE )
{
   HBITMAP hImage = NULL;
   int     iSource = hb_parni( 2 );

   switch ( iSource )
   {
      case 0:
      {
         hImage = LoadBitmap( hb_getWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 1 ) ) );
      }
      break;

      case 1:
      {
         hImage = LoadBitmap( hb_getWvwData()->hInstance, hb_parc( 1 ) );
      }
      break;

      case 2:
      {
         hImage = ( HBITMAP ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 1 ), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE );
      }
      break;
   }

   hb_retnl( ( ULONG ) hImage ) ;
}



HB_FUNC( WIN_GETCLIENTRECT )
{
   RECT     rc = { 0 };
   PHB_ITEM  info = hb_itemArrayNew(4);

   GetClientRect( ( HWND ) HB_PARHANDLE( 1 ), &rc );

   hb_arraySetNI( info, 1, rc.left   );
   hb_arraySetNI( info, 2, rc.top    );
   hb_arraySetNI( info, 3, rc.right  );
   hb_arraySetNI( info, 4, rc.bottom );

   hb_itemReturnRelease( info );
}

/*
 *
 *    Win_DrawImage( hdc, nLeft, nTop, nWidth, nHeight, cImage ) in Pixels
 */

/* sorry, not supported in GTWVW
HB_FUNC( WIN_DRAWIMAGE )
{
   hb_retl( hb_wvt_DrawImage( ( HDC ) hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                                   hb_parni( 4 ), hb_parni( 5 ), hb_parc( 6 ) ) );
}
*/



HB_FUNC( WIN_GETDC )
{
   HB_RETHANDLE( GetDC( ( HWND ) HB_PARHANDLE( 1 )  ) );
}



HB_FUNC( WIN_RELEASEDC )
{
   hb_retl( ReleaseDC( ( HWND ) HB_PARHANDLE( 1 ), ( HDC ) HB_PARHANDLE( 2 ) ) );
}



HB_FUNC( WVW_RECTANGLE )
{
   Rectangle( ( HDC ) HB_PARHANDLE( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}



HB_FUNC( WIN_CREATEBRUSH )
{
   LOGBRUSH lb = { 0 };

   lb.lbStyle = hb_parni( 1 );
   lb.lbColor = HB_ISNIL( 2 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = HB_ISNIL( 3 ) ? 0 : hb_parni( 3 );

   hb_retnl( ( ULONG ) CreateBrushIndirect( &lb ) );
}

/*
 *
 *   Win_DrawText( hDC, cText, aRect, nFormat )
 */

HB_FUNC( WIN_DRAWTEXT )
{
   RECT rc = { 0 };

   rc.left   = hb_parvni( 3,1 );
   rc.top    = hb_parvni( 3,2 );
   rc.right  = hb_parvni( 3,3 );
   rc.bottom = hb_parvni( 3,4 );

   hb_retl( DrawText( ( HDC ) HB_PARHANDLE( 1 ), hb_parc( 2 ), strlen( hb_parc( 2 ) ), &rc, hb_parni( 4 ) ) );
}


///////////////////////////////////////////////////////////////////
//                                                               //
// Adiciones a GtWVW desarrolladas por SOLUCIONES PERCEPTIVAS... //
//                                                               //
///////////////////////////////////////////////////////////////////

HB_FUNC( WVW_GBCREATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = ( USHORT )hb_parni( 4 ),
            usRight  = ( USHORT )hb_parni( 5 );
   LPCTSTR  lpszCaption = HB_ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = HB_ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = HB_ISNUM(7) ? (UINT) hb_parni(7) : 0;
   double   dStretch = !HB_ISNIL(10) ? hb_parnd(10) : 1;
   BOOL     bMap3Dcolors = HB_ISLOG(11) ? (BOOL) hb_parl(11) : FALSE;

   iOffTop    = !HB_ISNIL( 9 ) ? hb_parvni( 9,1 ) : -1 ;
   iOffLeft   = !HB_ISNIL( 9 ) ? hb_parvni( 9,2 ) : -1 ;
   iOffBottom = !HB_ISNIL( 9 ) ? hb_parvni( 9,3 ) : +1 ;
   iOffRight  = !HB_ISNIL( 9 ) ? hb_parvni( 9,4 ) : +1;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE );
   hb_retnl( (LONG) uiPBid );
}

// BS_TEXT | BS_GROUPBOX | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE
// BS_GROUPBOX | WS_GROUP | BS_TEXT | WS_OVERLAPPED

HB_FUNC( WVW_RBCREATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT uiPBid;
   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = ( USHORT )hb_parni( 4 ),
            usRight  = ( USHORT )hb_parni( 5 );
   LPCTSTR  lpszCaption = HB_ISCHAR(6) ? hb_parcx(6) : NULL;
   char   * szBitmap = HB_ISCHAR(7) ? (char*) hb_parcx(7) : NULL;
   UINT     uiBitmap = HB_ISNUM(7) ? (UINT) hb_parni(7) : 0;
   double   dStretch = !HB_ISNIL(10) ? hb_parnd(10) : 1;
   BOOL     bMap3Dcolors = HB_ISLOG(11) ? (BOOL) hb_parl(11) : FALSE;

   if (!HB_ISBLOCK(8))
   {
     hb_retnl(0);
     return;
   }

   iOffTop    = !HB_ISNIL( 9 ) ? hb_parvni( 9,1 ) : -2 ;
   iOffLeft   = !HB_ISNIL( 9 ) ? hb_parvni( 9,2 ) : -2 ;
   iOffBottom = !HB_ISNIL( 9 ) ? hb_parvni( 9,3 ) : +2 ;
   iOffRight  = !HB_ISNIL( 9 ) ? hb_parvni( 9,4 ) : +2;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_AUTORADIOBUTTON /*| WS_GROUP*/ );
   hb_retnl( (LONG) uiPBid );
}

HB_FUNC( WVW_SETCONTROLTEXT )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    return;
  }
  SetWindowText( hWndPB, hb_parcx(3) );
  hb_retl( TRUE );
}

HB_FUNC( WVW_PBVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    hb_retl( FALSE );
    return;
  }

  if ( bEnable )
  {
    iCmdShow = SW_SHOW;
  }
  else
  {
    iCmdShow = SW_HIDE;
  }
  hb_retl( ShowWindow(hWndPB, iCmdShow)==0 );
}

HB_FUNC( WVW_CBVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndCB = FindControlHandle(usWinNum, WVW_CONTROL_COMBOBOX, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (hWndCB)
  {
     if ( bEnable )
     {
       iCmdShow = SW_SHOW;
     }
     else
     {
       iCmdShow = SW_HIDE;
     }
     hb_retl( ShowWindow(hWndCB, iCmdShow)==0 );
  }
  else
  {
    hb_retl(FALSE);
  }
}

HB_FUNC( WVW_CXVISIBLE )
{
  UINT usWinNum = WVW_WHICH_WINDOW;
  UINT   uiCtrlId = HB_ISNIL(2) ? 0 : hb_parni(2);
  BOOL   bEnable  = HB_ISNIL(3) ? TRUE : hb_parl(3);
  byte   bStyle;
  HWND   hWndPB = FindControlHandle(usWinNum, WVW_CONTROL_PUSHBUTTON, uiCtrlId, &bStyle);
  int    iCmdShow;

  if (uiCtrlId==0 || hWndPB==NULL)
  {
    hb_retl( FALSE );
    return;
  }

  if ( bEnable )
  {
    iCmdShow = SW_SHOW;
  }
  else
  {
    iCmdShow = SW_HIDE;
  }
  hb_retl( ShowWindow(hWndPB, iCmdShow)==0 );
}

/*WVW_XBVisible( [nWinNum], nXBid, lShow )
 *show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *nWinNum better be NIL
 *nXBid is the handle of the scrolbar
 *lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 *returns .t. if successful
 */
HB_FUNC( WVW_XBVISIBLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   UINT uiXBid = (UINT) ( HB_ISNIL( 2 ) ? 0 : hb_parni( 2 ) );
   BOOL bShow = (BOOL) ( HB_ISLOG( 3 ) ? hb_parl( 3 ) : TRUE );
   byte bStyle;
   HWND hWndXB = uiXBid == 0 ? NULL : FindControlHandle(usWinNum, WVW_CONTROL_SCROLLBAR, uiXBid, &bStyle );

   if (uiXBid==0 || hWndXB==NULL)
   {
     hb_retl( FALSE );
     return;
   }

   hb_retl( ShowScrollBar( hWndXB, SB_CTL, bShow ) );
}




HB_FUNC( WVW_MOUSE_COL )
{
  WVW_DATA * pData =  hb_getWvwData( ) ;
  if ( hb_gt_wvw_GetMainCoordMode() )
  {
    hb_retni( hb_gt_wvwGetMouseX( pData->s_pWindows[ pData->s_usNumWindows-1 ] ) + hb_gt_wvwColOfs( pData->s_usNumWindows-1 ) );
  }
  else
  {
    hb_retni( hb_gt_wvwGetMouseX( pData->s_pWindows[ pData->s_usCurWindow ] ) );
  }
}



HB_FUNC( WVW_MOUSE_ROW )
{
  WVW_DATA * pData =  hb_getWvwData( ) ;
  if ( hb_gt_wvw_GetMainCoordMode() )
  {
    hb_retni( hb_gt_wvwGetMouseY( pData->s_pWindows[ pData->s_usNumWindows-1 ] ) + hb_gt_wvwRowOfs( pData->s_usNumWindows-1 ));
  }
  else
  {
    hb_retni( hb_gt_wvwGetMouseY( pData->s_pWindows[ pData->s_usCurWindow ] ) );
  }
}

HB_FUNC( SENDMESSAGE )
{

    hb_retnl( (LONG) SendMessage(
                       (HWND) HB_PARHANDLE(1),     // handle of destination window
                       (UINT) hb_parni( 2 ),    // message to send
                       (WPARAM) hb_parnl( 3 ),    // first message parameter
                       (HB_ISCHAR(4))? (LPARAM) hb_parc( 4 ) :
                          (LPARAM) hb_parnl( 4 ) // second message parameter
                     ) );
}

HB_FUNC( SETPARENT )
{
   UINT usWinNum = WVW_WHICH_WINDOW; // filho
   UINT usWinNum1 = ( HB_ISNIL( 2 ) ? ( hb_gt_wvw_GetMainCoordMode() ? ((hb_gt_wvw_GetNumWindows())-1) : hb_gt_wvw_GetCurWindow() ) : ((USHORT) hb_parni( 2 )) ); //pai
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WIN_DATA * pWindowData1 = hb_gt_wvw_GetWindowsData( usWinNum1 );
   HWND hWndParent = pWindowData->hWnd;
   HWND hWndParent1 = pWindowData1->hWnd;

   if ( usWinNum1 != 0)
      SetParent(hWndParent,hWndParent1);
}


HB_FUNC( BRINGTOTOP1 )
{
   HWND hWnd  = (HWND) HB_PARHANDLE( 1 ) ;
//   DWORD  ForegroundThreadID;
//DWORD  ThisThreadID;
//DWORD  timeout;

   if (IsIconic(hWnd) )
   {
      ShowWindow(hWnd,SW_RESTORE);
      hb_retl(TRUE);
      return;
   }
   BringWindowToTop(hWnd); // IE 5.5 related hack
   SetForegroundWindow(hWnd);
}

HB_FUNC( ISWINDOW )
{
   hb_retl(IsWindow((HWND) HB_PARHANDLE( 1 ) ) ) ;
}


HB_FUNC( ADDTOOLTIPEX ) // changed by MAG
{
//   HWND hWnd = (HWND) hb_parnl( 1 );
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WVW_DATA * pData =  hb_getWvwData( ) ;

   int iStyle = TTS_ALWAYSTIP;
   INITCOMMONCONTROLSEX icex = { 0 };
   TOOLINFO             ti = { 0 };

   /* Load the tooltip class from the DLL.
    */
   icex.dwSize = sizeof( icex );
   icex.dwICC  = ICC_BAR_CLASSES;

   if( !InitCommonControlsEx( &icex ) )
   {
   }

//   if ( lToolTipBalloon )
//   {
      iStyle = iStyle | TTS_BALLOON;
//   }

   if( !pData->hWndTT )
      pData->hWndTT = CreateWindow( TOOLTIPS_CLASS, (LPSTR) NULL, iStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                NULL, (HMENU) NULL, GetModuleHandle( NULL ), NULL );
   if( !pData->hWndTT )
   {
      hb_retnl( 0 );
      return;
   }
   ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
   ti.hwnd = pWindowData->hWnd;
   ti.uId = (UINT) hb_parnl( 2 );
   // ti.uId = (UINT) GetDlgItem( hWnd, hb_parni( 2 ) );
   ti.hinst = GetModuleHandle( NULL );
   ti.lpszText = (LPSTR) hb_parc( 3 );

   hb_retl( SendMessage( pData->hWndTT, TTM_ADDTOOL, 0, (LPARAM) (LPTOOLINFO) &ti) );
}





/*
 * CreateImagelist( array, cx, cy, nGrow, flags )
*/
HB_FUNC( CREATEIMAGELIST )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   UINT flags = ( HB_ISNIL(5) ) ? ILC_COLOR : hb_parni(5);
   HIMAGELIST himl;
   ULONG ul, ulLen = hb_arrayLen( pArray );
   HBITMAP hbmp;

   himl = ImageList_Create( hb_parni(2), hb_parni(3), flags,
                   ulLen, hb_parni(4) );

   for( ul=1; ul<=ulLen; ul++ )
   {
      hbmp = (HBITMAP)hb_arrayGetNL( pArray, ul );
      ImageList_Add( himl, hbmp, (HBITMAP) NULL );
      DeleteObject(hbmp);
   }

   HB_RETHANDLE( himl );
}

HB_FUNC( IMAGELIST_ADD )
{
   hb_retnl( ImageList_Add( (HIMAGELIST)HB_PARHANDLE(1), (HBITMAP)HB_PARHANDLE(2), (HBITMAP) NULL ) );
}

HB_FUNC( IMAGELIST_ADDMASKED )
{
   hb_retnl( ImageList_AddMasked( (HIMAGELIST)HB_PARHANDLE(1), (HBITMAP)HB_PARHANDLE(2), (COLORREF) hb_parnl(3) ) );
}


HB_FUNC( GETBITMAPSIZE )
{
   BITMAP  bitmap;
   PHB_ITEM aMetr = hb_itemArrayNew( 3 );

   GetObject( (HBITMAP) HB_PARHANDLE( 1 ), sizeof( BITMAP ), ( LPVOID ) &bitmap );

   hb_arraySetNL( aMetr, 1, bitmap.bmWidth );
   hb_arraySetNL( aMetr, 2, bitmap.bmHeight );
   hb_arraySetNL( aMetr, 3, bitmap.bmBitsPixel );

   hb_itemReturnRelease( aMetr );
}

HB_FUNC( GETICONSIZE )
{
   ICONINFO iinfo;
   PHB_ITEM aMetr = hb_itemArrayNew( 2 );

   GetIconInfo( (HICON) HB_PARHANDLE( 1 ), &iinfo );

   hb_arraySetNL( aMetr, 1, iinfo.xHotspot * 2 );
   hb_arraySetNL( aMetr, 2, iinfo.yHotspot * 2 );

   hb_itemReturnRelease( aMetr );
}


HB_FUNC( LOADIMAGE )
{
   if ( HB_ISNUM( 2 ) )
      hb_retnl( (LONG)
          LoadImage( hb_getWvwData()->hInstance,  //HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl( 1 ),    // handle of the instance that contains the image
                  (LPCTSTR)MAKEINTRESOURCE(hb_parnl(2)),          // name or identifier of image
                  (UINT) hb_parni(3),           // type of image
                  hb_parni(4),                  // desired width
                  hb_parni(5),                  // desired height
                  (UINT)hb_parni(6)             // load flags
     ) );

   else
      HB_RETHANDLE(
          LoadImage( (HINSTANCE)hb_parnl(1),    // handle of the instance that contains the image
                  (LPCTSTR)hb_parc(2),          // name or identifier of image
                  (UINT) hb_parni(3),           // type of image
                  hb_parni(4),                  // desired width
                  hb_parni(5),                  // desired height
                  (UINT)hb_parni(6)             // load flags
      ) );

}

HB_FUNC( LOADBITMAP )
{
   if( HB_ISNUM(1) )
   {
      if( !HB_ISNIL(2) && hb_parl(2) )
//               hb_retnl( (LONG) LoadBitmap( GetModuleHandle( NULL ),  MAKEINTRESOURCE(hb_parnl( 1 ) )) );
         HB_RETHANDLE( LoadBitmap( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), (LPCTSTR) hb_parnl( 1 ) ) );
   }
   else
     HB_RETHANDLE( LoadBitmap( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}

HB_FUNC( LOADBITMAPEX )
{
    HINSTANCE h = HB_ISNUM(1) ? (HINSTANCE) hb_parnl( 1 ) :GetModuleHandle( NULL ) ;
   if( HB_ISNUM(1) && HB_ISNUM(2) )
   {
      if( !HB_ISNIL(3) && hb_parl(3) )
//               hb_retnl( (LONG) LoadBitmap( h,  MAKEINTRESOURCE(hb_parnl( 2 ) )) );
         HB_RETHANDLE( LoadBitmap( h, (LPCTSTR) hb_parnl( 3 ) ) );
      else
         HB_RETHANDLE( LoadBitmap( (HINSTANCE) h, (LPCTSTR) hb_parnl( 2 ) ) );
   }
   else
      HB_RETHANDLE( LoadBitmap( h, (LPCTSTR) hb_parc( 2 ) ) );
}


HB_FUNC( OPENIMAGE )
{
   const char* cFileName = hb_parc(1);
   BOOL  lString = (HB_ISNIL(2))? 0 : hb_parl(2);
   int iFileSize;
   FILE* fp;
   // IPicture * pPic;
   LPPICTURE pPic;
   IStream * pStream;
   HGLOBAL hG;
   HBITMAP hBitmap = 0;

   if( lString )
   {
      iFileSize = hb_parclen( 1 );
      hG = GlobalAlloc( GPTR,iFileSize );
      if( !hG )
      {
         hb_retnl(0);
         return;
      }
      memcpy( (void*)hG, (void*)cFileName,iFileSize );
   }
   else
   {
      fp = fopen( cFileName,"rb" );
      if( !fp )
      {
         hb_retnl(0);
         return;
      }

      fseek( fp,0,SEEK_END );
      iFileSize = ftell( fp );
      hG = GlobalAlloc( GPTR,iFileSize );
      if( !hG )
      {
         fclose( fp );
         hb_retnl(0);
         return;
      }
      fseek( fp,0,SEEK_SET );
      fread( (void*)hG, 1, iFileSize, fp );
      fclose( fp );
   }

   CreateStreamOnHGlobal( hG,0,&pStream );

   if( !pStream )
   {
      GlobalFree( hG );
      hb_retnl(0);
      return;
   }

//#if defined(__cplusplus)
   //OleLoadPicture( pStream,0,0,&IID_IPicture,(void**)&pPic );
   //pStream->Release();
//#else
   OleLoadPicture( pStream,0,0,&IID_IPicture,(void**)&pPic );
   pStream->lpVtbl->Release( pStream );
//#endif

   GlobalFree( hG );

   if( !pPic )
   {
      hb_retnl(0);
      return;
   }

//#if defined(__cplusplus)
   //pPic->get_Handle( (OLE_HANDLE*)&hBitmap );
//#else
   pPic->lpVtbl->get_Handle( pPic, (OLE_HANDLE*)&hBitmap );
//#endif

   hb_retnl( (LONG) CopyImage( hBitmap,IMAGE_BITMAP,0,0,LR_COPYRETURNORG ) );

//#if defined(__cplusplus)
   //pPic->Release();
//#else
   pPic->lpVtbl->Release( pPic );
//#endif
}

HB_FUNC( OPENBITMAP )
{
   BITMAPFILEHEADER bmfh;
   BITMAPINFOHEADER bmih;
   LPBITMAPINFO lpbmi;
   DWORD dwRead;
   LPVOID lpvBits;
   HGLOBAL hmem1, hmem2;
   HBITMAP hbm;
   HDC hDC = (hb_pcount()>1 && !HB_ISNIL(2))? (HDC)HB_PARHANDLE(2):NULL;
   HANDLE hfbm = CreateFile( hb_parc( 1 ), GENERIC_READ, FILE_SHARE_READ,
                   (LPSECURITY_ATTRIBUTES) NULL, OPEN_EXISTING,
                   FILE_ATTRIBUTE_READONLY, (HANDLE) NULL );

   if( ( (long int)hfbm ) <= 0 )
   {
      HB_RETHANDLE(NULL);
      return;
   }
   /* Retrieve the BITMAPFILEHEADER structure. */
   ReadFile( hfbm, &bmfh, sizeof(BITMAPFILEHEADER), &dwRead, NULL );

   /* Retrieve the BITMAPFILEHEADER structure. */
   ReadFile( hfbm, &bmih, sizeof(BITMAPINFOHEADER), &dwRead, NULL );

   /* Allocate memory for the BITMAPINFO structure. */

   hmem1 = GlobalAlloc( GHND, sizeof(BITMAPINFOHEADER) +
             ((1<<bmih.biBitCount) * sizeof(RGBQUAD)));
   lpbmi = (LPBITMAPINFO)GlobalLock( hmem1 );

   /*  Load BITMAPINFOHEADER into the BITMAPINFO  structure. */
   lpbmi->bmiHeader.biSize = bmih.biSize;
   lpbmi->bmiHeader.biWidth = bmih.biWidth;
   lpbmi->bmiHeader.biHeight = bmih.biHeight;
   lpbmi->bmiHeader.biPlanes = bmih.biPlanes;

   lpbmi->bmiHeader.biBitCount = bmih.biBitCount;
   lpbmi->bmiHeader.biCompression = bmih.biCompression;
   lpbmi->bmiHeader.biSizeImage = bmih.biSizeImage;
   lpbmi->bmiHeader.biXPelsPerMeter = bmih.biXPelsPerMeter;
   lpbmi->bmiHeader.biYPelsPerMeter = bmih.biYPelsPerMeter;
   lpbmi->bmiHeader.biClrUsed = bmih.biClrUsed;
   lpbmi->bmiHeader.biClrImportant = bmih.biClrImportant;

   /*  Retrieve the color table.
    * 1 << bmih.biBitCount == 2 ^ bmih.biBitCount
   */
   switch(bmih.biBitCount)
   {
      case 1  :
      case 4  :
      case 8  :
         ReadFile(hfbm, lpbmi->bmiColors,
           ((1<<bmih.biBitCount) * sizeof(RGBQUAD)),
           &dwRead, (LPOVERLAPPED) NULL);
           break;

      case 16 :
      case 32 :
         if( bmih.biCompression == BI_BITFIELDS )
           ReadFile(hfbm, lpbmi->bmiColors,
             ( 3 * sizeof(RGBQUAD)),
             &dwRead, (LPOVERLAPPED) NULL);
         break;

      case 24 :
         break;
   }

   /* Allocate memory for the required number of  bytes. */
   hmem2 = GlobalAlloc( GHND, (bmfh.bfSize - bmfh.bfOffBits) );
   lpvBits = GlobalLock(hmem2);

   /* Retrieve the bitmap data. */

   ReadFile(hfbm, lpvBits, (bmfh.bfSize - bmfh.bfOffBits), &dwRead, NULL );

   if( !hDC )
      hDC = GetDC( 0 );

   /* Create a bitmap from the data stored in the .BMP file.  */
   hbm = CreateDIBitmap( hDC, &bmih, CBM_INIT, lpvBits, lpbmi, DIB_RGB_COLORS );

   if( hb_pcount() < 2 || HB_ISNIL(2) )
      ReleaseDC( 0, hDC );

   /* Unlock the global memory objects and close the .BMP file. */
   GlobalUnlock(hmem1);
   GlobalUnlock(hmem2);
   GlobalFree(hmem1);
   GlobalFree(hmem2);
   CloseHandle(hfbm);

   HB_RETHANDLE( hbm );
}


HB_FUNC( SETTEXTCOLOR )
{
   COLORREF crColor = SetTextColor(
              (HDC) HB_PARHANDLE( 1 ),      // handle of device context
              (COLORREF) hb_parnl( 2 )  // text color
            );
   hb_retnl( (LONG) crColor );
}

HB_FUNC( SETBKCOLOR )
{
   COLORREF crColor = SetBkColor(
              (HDC) HB_PARHANDLE( 1 ),      // handle of device context
              (COLORREF) hb_parnl( 2 )  // text color
            );
   hb_retnl( (LONG) crColor );
}

HB_FUNC( CREATESOLIDBRUSH )
{
   HB_RETHANDLE( CreateSolidBrush(
               (COLORREF) hb_parnl( 1 )   // brush color
             ) );
}

HB_FUNC( CREATEHATCHBRUSH )
{
   HB_RETHANDLE( CreateHatchBrush(
               hb_parni(1), (COLORREF) hb_parnl(2) ) );
}
HB_FUNC( RGB )
{
   hb_retnl( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ) ;
}

HB_FUNC( GETSYSCOLOR )
{
   hb_retnl( (LONG) GetSysColor( hb_parni( 1 ) ) );
}

HB_FUNC( REDRAWWINDOW )
{
   RedrawWindow(
    (HWND) HB_PARHANDLE( 1 ),  // handle of window
    NULL,                  // address of structure with update rectangle
    NULL,                  // handle of update region
    (UINT)hb_parni( 2 )    // array of redraw flags
   );
}

/* CreateFont( fontName, nWidth, hHeight [,fnWeight] [,fdwCharSet],
               [,fdwItalic] [,fdwUnderline] [,fdwStrikeOut]  )
*/
HB_FUNC( CREATEFONT )
{
   HFONT hFont;
   int fnWeight = ( HB_ISNIL(4) )? 0:hb_parni(4);
   DWORD fdwCharSet = ( HB_ISNIL(5) )? 0:hb_parnl(5);
   DWORD fdwItalic = ( HB_ISNIL(6) )? 0:hb_parnl(6);
   DWORD fdwUnderline = ( HB_ISNIL(7) )? 0:hb_parnl(7);
   DWORD fdwStrikeOut = ( HB_ISNIL(8) )? 0:hb_parnl(8);

   hFont = CreateFont(
    hb_parni( 3 ),      // logical height of font
    hb_parni( 2 ),      // logical average character width
    0,  // angle of escapement
    0,  // base-line orientation angle
    fnWeight,   // font weight
    fdwItalic,  // italic attribute flag
    fdwUnderline,       // underline attribute flag
    fdwStrikeOut,       // strikeout attribute flag
    fdwCharSet, // character set identifier
    0,  // output precision
    0,  // clipping precision
    0,  // output quality
    0,  // pitch and family
    (LPCTSTR) hb_parc( 1 )      // pointer to typeface name string
   );
   HB_RETHANDLE( hFont );
}


HB_FUNC( SELECTFONT )
{

   CHOOSEFONT cf;
   LOGFONT lf;
   HFONT hfont;
   PHB_ITEM pObj = ( HB_ISNIL(1) )? NULL:hb_param( 1, HB_IT_OBJECT );
   //PHB_ITEM temp1;
   PHB_ITEM aMetr = hb_itemArrayNew( 9 );

   cf.lStructSize = sizeof(CHOOSEFONT);
   cf.hwndOwner = (HWND)NULL;
   cf.hDC = (HDC)NULL;
   cf.lpLogFont = &lf;
   cf.iPointSize = 0;
   cf.Flags = CF_SCREENFONTS | ( (pObj)? CF_INITTOLOGFONTSTRUCT:0 );
   cf.rgbColors = RGB(0,0,0);
   cf.lCustData = 0L;
   cf.lpfnHook = (LPCFHOOKPROC)NULL;
   cf.lpTemplateName = (LPSTR)NULL;

   cf.hInstance = (HINSTANCE) NULL;
   cf.lpszStyle = (LPSTR)NULL;
   cf.nFontType = SCREEN_FONTTYPE;
   cf.nSizeMin = 0;
   cf.nSizeMax = 0;

   /* Display the CHOOSEFONT common-dialog box. */

   if( !ChooseFont(&cf) )
   {
      hb_itemRelease( aMetr );
      hb_ret();
      return;
   }

   /* Create a logical font based on the user's   */
   /* selection and return a handle identifying   */
   /* that font.                                  */

   hfont = CreateFontIndirect(cf.lpLogFont);

   hb_arraySetNInt( aMetr, 1, ( HB_PTRDIFF ) hfont );
   hb_arraySetC(  aMetr, 2, lf.lfFaceName );
   hb_arraySetNL( aMetr, 3, lf.lfWidth );
   hb_arraySetNL( aMetr, 4, lf.lfHeight );
   hb_arraySetNL( aMetr, 5, lf.lfWeight );
   hb_arraySetNI( aMetr, 6, lf.lfCharSet );
   hb_arraySetNI( aMetr, 7, lf.lfItalic );
   hb_arraySetNI( aMetr, 8, lf.lfUnderline );
   hb_arraySetNI( aMetr, 9, lf.lfStrikeOut );

   hb_itemReturnRelease( aMetr );

}

HB_FUNC( INVALIDATERECT )
{
   RECT rc;

   if( hb_pcount() > 2 )
   {
      rc.left = hb_parni( 3 );
      rc.top = hb_parni( 4 );
      rc.right = hb_parni( 5 );
      rc.bottom = hb_parni( 6 );
   }

   InvalidateRect(
    (HWND) HB_PARHANDLE( 1 ), // handle of window with changed update region
    ( hb_pcount() > 2 )? &rc:NULL,  // address of rectangle coordinates
    hb_parni( 2 ) // erase-background flag
   );
}

HB_FUNC(TOOLBARADDBUTTONS)
{

   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   HWND hWndCtrl = ( HWND ) HB_PARHANDLE( 2 ) ;
   /* HWND hToolTip = ( HWND ) hb_parnl( 5 ) ; */
   PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );
   int iButtons= hb_parni( 4 );
   TBBUTTON  *tb = ( struct _TBBUTTON * ) hb_xgrab( iButtons * sizeof( TBBUTTON ) );
   PHB_ITEM pTemp;
   //BOOL bSystem;

   ULONG ulCount;
   ULONG ulID;
   DWORD style = GetWindowLong( hWndCtrl, GWL_STYLE );
   USHORT  usOldHeight;

   SetWindowLong(hWndCtrl,GWL_STYLE,style|TBSTYLE_TOOLTIPS |TBSTYLE_FLAT);

   SendMessage( hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0L);
   usOldHeight = pWindowData->usTBHeight;
   for ( ulCount =0 ;  ( ulCount < hb_arrayLen( pArray ) ); ulCount++ )
   {

      pTemp = hb_arrayGetItemPtr( pArray , ulCount + 1 );
      ulID=hb_arrayGetNI( pTemp, 1 );
      //bSystem = hb_arrayGetL( pTemp, 9 );

//      if (bSystem)
//      if (ulID > 0 && ulID <  31 )
  //    {
         tb[ ulCount ].iBitmap   = ulID > 0 ? ( int ) ulID : -1;
//      }
//      else
//      {
//         tb[ ulCount ].iBitmap   = ulID > 0 ? ( int ) ulCount : -1;
//      }
      tb[ ulCount ].idCommand = hb_arrayGetNI( pTemp, 2 );
      tb[ ulCount ].fsState   = ( BYTE )hb_arrayGetNI( pTemp, 3 );
      tb[ ulCount ].fsStyle   = ( BYTE )hb_arrayGetNI( pTemp, 4 );
      tb[ ulCount ].dwData    = hb_arrayGetNI( pTemp, 5 );
      tb[ ulCount ].iString   = hb_arrayGetCLen( pTemp, 6 )  >0 ? ( int ) hb_arrayGetCPtr( pTemp, 6 ) : 0 ;

   }

   SendMessage( hWndCtrl, TB_ADDBUTTONS, (WPARAM) iButtons, (LPARAM) (LPTBBUTTON) tb);
   SendMessage( hWndCtrl, TB_AUTOSIZE, 0, 0 );
   hb_gt_wvwTBinitSize( pWindowData, hWndCtrl );

   if (pWindowData->usTBHeight != usOldHeight)
   {
     hb_gt_wvwResetWindow( usWinNum );
   }

   hb_xfree( tb );
}

HB_FUNC(SETBITMAPRESOURCEID)
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   TBADDBITMAP tbab;
   HBITMAP hBitmap  = (HBITMAP) HB_PARHANDLE( 3 ) ;
   UINT uiBitmap = (UINT) hb_parni( 4 );
   HWND hWndToolbar = pWindowData->hToolBar;
   int iNewBitmap;
   int iBitmapType = hb_parni( 2 );
   int iOffset;


   switch (iBitmapType)
   {
      case 0:
        iOffset = 0;
        break;
      case 1:
        iOffset = pWindowData->iStartStdBitmap;
        break;
      case 2:
        iOffset = pWindowData->iStartViewBitmap;
        break;
      case 3:
        iOffset = pWindowData->iStartHistBitmap;
        break;
      default:
        iOffset = 0;
        break;
   }

   if (iBitmapType==0)
   {


      tbab.hInst = NULL;
      tbab.nID   = (UINT) hBitmap;
      iNewBitmap = SendMessage(hWndToolbar, TB_ADDBITMAP, (WPARAM) 1, (WPARAM) &tbab);

   }
   else /* system bitmap */
   {
      iNewBitmap = (int) uiBitmap + iOffset;
   }
   hb_retni( iNewBitmap ) ;

}


HB_FUNC( DRAWICON )
{
   DrawIcon( (HDC)HB_PARHANDLE( 1 ), hb_parni( 3 ), hb_parni( 4 ), (HICON)HB_PARHANDLE( 2 ) );
}

HB_FUNC( LOADICON )
{
   if( HB_ISNUM(1) )
      HB_RETHANDLE( LoadIcon( NULL, (LPCTSTR) hb_parnl( 1 ) ) );
   else
      HB_RETHANDLE( LoadIcon( GetModuleHandle( NULL ), (LPCTSTR) hb_parc( 1 ) ) );
}
HB_FUNC( DRAWBITMAP )
{
   HDC hDC = (HDC) HB_PARHANDLE( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (HB_ISNIL(3))? SRCCOPY:hb_parnl(3);
   HBITMAP hBitmap = (HBITMAP) HB_PARHANDLE( 2 );
   BITMAP  bitmap;
   int nWidthDest = ( hb_pcount()>=5 && !HB_ISNIL(6) )? hb_parni(6):0;
   int nHeightDest = ( hb_pcount()>=6 && !HB_ISNIL(7) )? hb_parni(7):0;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   if( nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ))
   {
      StretchBlt( hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   }
   else
   {
      BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
   }

   DeleteDC( hDCmem );
}
HB_FUNC( WINDOW2BITMAP )
{
   HWND hWnd = (HWND) HB_PARHANDLE( 1 );
   BOOL lFull = ( HB_ISNIL(2) )? 0 : (BOOL)hb_parl(2);
   HDC hDC = ( lFull )? GetWindowDC( hWnd ) : GetDC( hWnd );
   HDC hDCmem = CreateCompatibleDC( hDC );
   HBITMAP hBitmap;
   RECT rc;

   if( lFull )
      GetWindowRect( hWnd, &rc );
   else
      GetClientRect( hWnd, &rc );

   hBitmap = CreateCompatibleBitmap( hDC, rc.right-rc.left, rc.bottom-rc.top );
   SelectObject( hDCmem, hBitmap );

   BitBlt( hDCmem, 0, 0, rc.right-rc.left, rc.bottom-rc.top, hDC, 0, 0, SRCCOPY );

   DeleteDC( hDCmem );
   DeleteDC( hDC );
   HB_RETHANDLE( hBitmap );
}



/* wvw_SetMaxBMCache([nMax])
   Get/Set maximum user-bitmap cache (default is 20, minimum is 1).
   Returns old setting of maximum user-bitmap cache.

   Description:
   To minimize bitmap loading operation, wvw_drawimage caches bitmap once
   it reads from disk.
   Ie., subsequent wvw_drawimage will use the bitmap from the memory.
   When the maximum number of cache is used, the least recently opened bitmap
   will be discarded from the cache.

   Remarks:
   There is no way to discard a specific bitmap from the cache.
   If you want to control bitmap caching manually, use wvw_loadpicture()
   instead.

   Example:
   wvw_SetMaxBMCache(1)  :: this will cache one bitmap only
   wvw_SetMaxBMCache(50) :: allows up to 50 bitmap stored in the cache
 */
HB_FUNC( WVW_SETMAXBMCACHE )
{
   WVW_DATA *  p = hb_getWvwData();
   UINT uiOldMaxBMcache = p->s_sApp->uiMaxBMcache;

   if ( ! HB_ISNIL( 1 ) )
   {
     p->s_sApp->uiMaxBMcache = (UINT) hb_parni( 1 );
   }

   hb_retni( uiOldMaxBMcache );
}

/* wvw_NumBMCache()
   Returns current number of user-bitmap cache.
 */
HB_FUNC( WVW_NUMBMCACHE )
{
   WVW_DATA *  p = hb_getWvwData();
   hb_retni( p->s_sApp->uiBMcache );
}










/*                                                                   */
/*               Miscellaneous xHarbour callable functions           */
/*               Budyanto Dj. <budyanto@centrin.net.id>              */
/*                                                                   */





/* TIMER                                                             */


/*WVW_SetTimer([nWinNum], nInterval)
 *set timer event for every nInterval millisec
 *(effective only if WVW_TIMER() function exists)
 *eg. it can be usefull to update clock on status bar
 *returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different*/
HB_FUNC (WVW_SETTIMER)
{
   WVW_DATA *  p = hb_getWvwData();
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if ( p->s_sApp->pSymWVW_TIMER )
   {
     SetTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+usWinNum, (UINT) hb_parni(2), NULL );

     hb_retl( TRUE );
   }
   else
   {
     hb_retl( FALSE );
   }
}

/*WVW_KillTimer([nWinNum])
 *kill the timer event handler for window nWinNum
 *returns .t. if successfull
 */
/*20040602: WARNING: WVT is slightly different */
HB_FUNC (WVW_KILLTIMER)
{
   WVW_DATA *  p = hb_getWvwData();
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if ( p->s_sApp->pSymWVW_TIMER )
   {
     KillTimer( pWindowData->hWnd, WVW_ID_BASE_TIMER+usWinNum );
     hb_retl( TRUE );
   }
   else
   {
     hb_retl( FALSE );
   }
}




/*WVW_GetPaintRect( nWinNum )   nWinNum is 0 based               */
/*returns array of paint pending rect {top, left, bottom, right} */
/*WARNING:                                                       */
/*unlike WVT, top maybe > bottom                                 */
/*            left maybe > right                                 */
/*in these cases, no paint request is pending                    */
/*(in WVT these is reflected in {0,0,0,0})                       */
HB_FUNC( WVW_GETPAINTRECT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   RECT   rPaintRect = pWindowData->rPaintPending;
   PHB_ITEM  info = hb_itemArrayNew(4);

   hb_arraySetNI( info, 1, rPaintRect.top );
   hb_arraySetNI( info, 2, rPaintRect.left );
   hb_arraySetNI( info, 3, rPaintRect.bottom  );
   hb_arraySetNI( info, 4, rPaintRect.right  );

   hb_itemReturnRelease( info );
}



HB_FUNC( WVW_SETPOINTER )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int     iCursor = hb_parni( 2 );
   HCURSOR hCursor;

   switch ( iCursor )
   {
   case 1:
      hCursor = LoadCursor( NULL, IDC_ARROW    );
      break;

   case 2:
      hCursor = LoadCursor( NULL, IDC_IBEAM    );
      break;

   case 3:
      hCursor = LoadCursor( NULL, IDC_WAIT     );
      break;

   case 4:
      hCursor = LoadCursor( NULL, IDC_CROSS    );
      break;

   case 5:
      hCursor = LoadCursor( NULL, IDC_UPARROW  );
      break;

   case 6:
      hCursor = LoadCursor( NULL, IDC_SIZE     );
      break;

   case 7:
      hCursor = LoadCursor( NULL, IDC_ICON     );
      break;

   case 8:
      hCursor = LoadCursor( NULL, IDC_SIZENWSE );
      break;

   case 9:
      hCursor = LoadCursor( NULL, IDC_SIZENESW );
      break;

   case 10:
      hCursor = LoadCursor( NULL, IDC_SIZEWE   );
      break;

   case 11:
      hCursor = LoadCursor( NULL, IDC_SIZENS   );
      break;

   case 12:
      hCursor = LoadCursor( NULL, IDC_SIZEALL  );
      break;

   case 13:
      hCursor = LoadCursor( NULL, IDC_NO       );
      break;

   case 14:
      hCursor = LoadCursor( NULL, IDC_HAND     );
      break;

   case 15:
      hCursor = LoadCursor( NULL, IDC_APPSTARTING );
      break;

   case 16:
      hCursor = LoadCursor( NULL, IDC_HELP     );
      break;

   default:
      hCursor = LoadCursor( NULL, IDC_ARROW    );
      break;
   }

   SetClassLongPtr( pWindowData->hWnd, GCLP_HCURSOR, ( LONG_PTR ) hCursor );
}




/*                                                                   */
/*   Wvw_LoadPicture( nSlot, cFilePic )                              */
/*                                                                   */
HB_FUNC( WVW_LOADPICTURE )
{
   WVW_DATA *  p = hb_getWvwData();
   IPicture * iPicture = hb_gt_wvwLoadPicture( hb_parcx( 2 ) );
   BOOL       bResult  = FALSE;
   int        iSlot    = hb_parni( 1 ) - 1 ;

   if ( iPicture )
   {
      if ( p->s_sApp->iPicture[ iSlot ] )
      {
         hb_gt_wvwDestroyPicture( p->s_sApp->iPicture[ iSlot ] );
      }

      p->s_sApp->iPicture[ iSlot ] = iPicture;
      bResult = TRUE;
   }
   hb_retl( bResult );
}


/*                                                                                                */
/* Wvw_LoadFont( nSlotFont, cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline, lStrikeout, */
/*               nCharSet, nQuality, nEscapement )                                                */
/*                                                                                                */
HB_FUNC( WVW_LOADFONT )
{
   WVW_DATA *  p = hb_getWvwData();
   UINT usWinNum = p->s_usNumWindows-1;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   LOGFONT  logfont;
   int      iSlot = hb_parni( 1 ) - 1;
   HFONT    hFont;

   logfont.lfEscapement     = ( HB_ISNIL( 11 ) ? 0 : ( hb_parni( 11 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( HB_ISNIL(  5 ) ? 0 : hb_parni( 5 ) );
   logfont.lfItalic         = ( HB_ISNIL(  6 ) ? 0 : ( BYTE )hb_parl(  6 ) );
   logfont.lfUnderline      = ( HB_ISNIL(  7 ) ? 0 : ( BYTE )hb_parl(  7 ) );
   logfont.lfStrikeOut      = ( HB_ISNIL(  8 ) ? 0 : ( BYTE )hb_parl(  8 ) );
   logfont.lfCharSet        = ( HB_ISNIL(  9 ) ? ( BYTE )pWindowData->CodePage : ( BYTE )hb_parni(  9 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( HB_ISNIL( 10 ) ? ( BYTE )DEFAULT_QUALITY : (BYTE)hb_parni( 10 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( HB_ISNIL(  3 ) ? pWindowData->fontHeight : hb_parni( 3 ) );
   logfont.lfWidth          = ( HB_ISNIL(  4 ) ? ( pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth ) : hb_parni( 4 ) );

   strcpy( logfont.lfFaceName, ( HB_ISNIL( 2 ) ? pWindowData->fontFace : hb_parcx( 2 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      if ( p->s_sApp->hUserFonts[ iSlot ] )
      {
         DeleteObject( (HFONT) p->s_sApp->hUserFonts[ iSlot ] );
      }
      p->s_sApp->hUserFonts[ iSlot ] = hFont;
   }
}


/*                                                                   */
/*  Wvw_LoadPen( nSlot, nStyle, nWidth, nRGBColor )                  */
/*                                                                   */
HB_FUNC( WVW_LOADPEN )
{
   WVW_DATA *  p = hb_getWvwData();
   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   int      iSlot = hb_parni( 1 ) - 1;

   iPenStyle = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
   iPenWidth = HB_ISNIL( 3 ) ? 0 : hb_parni( 3 );
   crColor   = HB_ISNIL( 4 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 4 );

   hPen      = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      if ( p->s_sApp->hUserPens[ iSlot ] )
      {
         DeleteObject( (HPEN) p->s_sApp->hUserPens[ iSlot ] );
      }
      p->s_sApp->hUserPens[ iSlot ] = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}




HB_FUNC( WVW_MESSAGEBOX )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   hb_retni( MessageBox( pWindowData->hWnd, hb_parcx( 2 ), hb_parcx( 3 ), HB_ISNIL( 4 ) ? MB_OK : hb_parni( 4 ) ) );
}



/*                    End of Drawing Primitives                      */

/*                                                                   */
/*              Utility Functions . A Natural Extension              */
/*                copied and modified from gtwvt                     */


/*                                                                      */
/*     Wvw_ChooseFont( cFontName, nHeight, nWidth, nWeight, nQuality, ; */
/*                                    lItalic, lUnderline, lStrikeout ) */
/*                                                                      */

HB_FUNC( WVW_CHOOSEFONT )
{

   CHOOSEFONT  cf = { 0 };
   LOGFONT     lf = { 0 };
   LONG        PointSize = 0;
   WVW_DATA *  p = hb_getWvwData();

   if ( ! HB_ISNIL( 2 ) )
   {
      PointSize = -MulDiv( ( LONG ) hb_parnl( 2 ), GetDeviceCaps( p->s_pWindows[ p->s_usNumWindows-1 ]->hdc, LOGPIXELSY ), 72 ) ;
   }

   lf.lfHeight         = PointSize;
   lf.lfWidth          = HB_ISNIL( 3 ) ? 0 : hb_parni( 3 );
   lf.lfWeight         = HB_ISNIL( 4 ) ? 0 : hb_parni( 4 );
   lf.lfItalic         = HB_ISNIL( 6 ) ? 0 : ( BYTE )hb_parl( 6 );
   lf.lfUnderline      = HB_ISNIL( 7 ) ? 0 : ( BYTE )hb_parl( 7 );
   lf.lfStrikeOut      = HB_ISNIL( 8 ) ? 0 : ( BYTE )hb_parl( 8 );
   lf.lfCharSet        = DEFAULT_CHARSET;
   lf.lfQuality        = HB_ISNIL( 5 ) ? DEFAULT_QUALITY : ( BYTE )hb_parni( 5 );
   lf.lfPitchAndFamily = FF_DONTCARE;
   if ( HB_ISCHAR( 1 ) )
   {
      strcpy( lf.lfFaceName, hb_parcx( 1 ) );
   }

   cf.lStructSize      = sizeof( CHOOSEFONT );
   cf.hwndOwner        = p->s_pWindows[ p->s_usNumWindows-1 ]->hWnd;
   cf.hDC              = ( HDC ) NULL;
   cf.lpLogFont        = &lf;
   cf.iPointSize       = 0;
   cf.Flags            = CF_SCREENFONTS | CF_EFFECTS | CF_SHOWHELP | CF_INITTOLOGFONTSTRUCT ;
   cf.rgbColors        = RGB( 0,0,0 );
   cf.lCustData        = 0L;
   cf.lpfnHook         = ( LPCFHOOKPROC ) NULL;
   cf.lpTemplateName   = ( LPSTR ) NULL;
   cf.hInstance        = ( HINSTANCE ) NULL;
   cf.lpszStyle        = ( LPSTR ) NULL;
   cf.nFontType        = SCREEN_FONTTYPE;
   cf.nSizeMin         = 0;
   cf.nSizeMax         = 0;

   if ( ChooseFont( &cf ) )
   {
      PointSize = -MulDiv( lf.lfHeight, 72, GetDeviceCaps( p->s_pWindows[ p->s_usNumWindows-1 ]->hdc, LOGPIXELSY ) ) ;

      hb_reta( 8 );

      hb_storvc( lf.lfFaceName      , -1, 1 );
      hb_storvnl( ( LONG ) PointSize, -1, 2 );
      hb_storvni( lf.lfWidth        , -1, 3 );
      hb_storvni( lf.lfWeight       , -1, 4 );
      hb_storvni( lf.lfQuality      , -1, 5 );
      hb_storvl( lf.lfItalic        , -1, 6 );
      hb_storvl( lf.lfUnderline     , -1, 7 );
      hb_storvl( lf.lfStrikeOut     , -1, 8 );
   }
   else
   {
      hb_reta( 8 );

      hb_storvc( ""         , -1, 1 );
      hb_storvnl( ( LONG ) 0, -1, 2 );
      hb_storvni( 0         , -1, 3 );
      hb_storvni( 0         , -1, 4 );
      hb_storvni( 0         , -1, 5 );
      hb_storvl( 0          , -1, 6 );
      hb_storvl( 0          , -1, 7 );
      hb_storvl( 0          , -1, 8 );
   }

   return ;
}


/*                                                                   */
/*    Wvw_ChooseColor( nRGBInit, aRGB16, nFlags ) => nRGBSelected    */
/*                                                                   */

HB_FUNC( WVW_CHOOSECOLOR )
{

   CHOOSECOLOR cc ;
   COLORREF    crCustClr[ 16 ] ;
   int         i ;
   WVW_DATA *  p = hb_getWvwData();

   for( i = 0 ; i < 16 ; i++ )
   {

     crCustClr[ i ] = ( HB_ISARRAY( 2 ) ? (COLORREF) hb_parvnl( 2, i+1 ) : GetSysColor( COLOR_BTNFACE ) ) ;
   }

   cc.lStructSize    = sizeof( CHOOSECOLOR ) ;
   cc.hwndOwner      = p->s_pWindows[ p->s_usNumWindows-1 ]->hWnd ;
   cc.rgbResult      = HB_ISNIL( 1 ) ?  0 : ( COLORREF ) hb_parnl( 1 ) ;
   cc.lpCustColors   = crCustClr ;

   cc.Flags         = ( WORD ) ( HB_ISNIL( 3 ) ? CC_ANYCOLOR | CC_RGBINIT | CC_FULLOPEN : hb_parnl( 3 ) );

   if ( ChooseColor( &cc ) )
   {
      hb_retnl( cc.rgbResult ) ;
   }
   else
   {
      hb_retnl( -1 );
   }
}



/*WVW_SETMOUSEPOS( nWinNum, nRow, nCol ) nWinNum is 0 based        */
/*WHAT'S the difference with GT_FUNC( mouse_SetPos ) ???           */
/*this func is able to position cursor on any window               */

/*NOTE: consider using 'standard' SETMOUSE() instead:     */
/*      SETMOUSE(.t., nRow, nCol)                                  */
/*      This will treat (nRow,nCol) according to current s_pWvwData->s_bMainCoordMode setting */

HB_FUNC( WVW_SETMOUSEPOS )
{
   POINT xy = { 0 };
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   USHORT   usRow    = ( USHORT )hb_parni( 2 ),
            usCol    = ( USHORT )hb_parni( 3 );

   if (hb_gt_wvw_GetMainCoordMode())
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usRow, &usCol, NULL, NULL);
   }

   xy = hb_gt_wvwGetXYFromColRow( pWindowData, usCol, usRow );

   if ( ClientToScreen( pWindowData->hWnd, &xy ) )
   {
      hb_retl( SetCursorPos( xy.x, xy.y + ( pWindowData->PTEXTSIZE.y / 2 ) ) );
   }
   else
   {
      hb_retl( FALSE );
   }
}



/*by bdj                                                                                */
/*none in gtwvt                                                                         */
/*    Wvw_FillRectangle( nWinNum, nTop, nLeft, nBottom, nRight, nRGBcolor/hBrush,       */
/*                       lTight, lUseBrush, aOffSet )                                   */
/*                                                                                      */
/*   if lTight, rect is drawn inside the character region                               */
/*   AND top and left lines are lower two pixel down to make room for above/left object */
/*   WARNING: gui object of this type subject to be overwritten by chars                */
/*   NOTE that these lines are to be overwritten by displayed char,                     */
/*        we are depending on the fact that gui object will be painted last             */
/*                                                                                      */
/*   if lUseBrush, nRGBcolor is treated as a BRUSH handle                               */
/*                                                                                      */

HB_FUNC( WVW_FILLRECTANGLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WVW_DATA *  p = hb_getWvwData();
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT xy = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = ( USHORT )hb_parni( 4 ),
            usRight  = ( USHORT )hb_parni( 5 );
   COLORREF crRGBcolor = ( HB_ISNIL( 6 ) ? 0 : hb_parnl( 6 ) );
   BOOL     bTight = ( HB_ISNIL( 7 ) ? FALSE : hb_parl( 7 ) );
   BOOL     bUseBrush = ( HB_ISNIL( 8 ) ? FALSE : hb_parl( 8 ) );
   LOGBRUSH lb = { 0 };
   HBRUSH   hBrush;
   RECT     xyRect = { 0 };

   if (hb_gt_wvw_GetMainCoordMode())
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   iOffTop    = !HB_ISNIL( 9 ) ? hb_parvni( 9,1 ) : 0 ;
   iOffLeft   = !HB_ISNIL( 9 ) ? hb_parvni( 9,2 ) : 0 ;
   iOffBottom = !HB_ISNIL( 9 ) ? hb_parvni( 9,3 ) : 0 ;
   iOffRight  = !HB_ISNIL( 9 ) ? hb_parvni( 9,4 ) : 0;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = bTight ? xy.y+2 : xy.y;
   iLeft   = bTight ? xy.x+2 : xy.x;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y   -= pWindowData->byLineSpacing;

   iBottom = xy.y-1;
   iRight  = xy.x-1;

   // Aplica OffSet
   iTop    += iOffTop   ;
   iLeft   += iOffLeft  ;
   iBottom += iOffBottom;
   iRight  += iOffRight ;

   xyRect.left = iLeft;
   xyRect.top  = iTop;
   xyRect.right= iRight+1;
   xyRect.bottom = iBottom+1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = crRGBcolor;
   lb.lbHatch = 0;

   hBrush     = !bUseBrush ? CreateBrushIndirect( &lb ) : (HBRUSH) HB_PARHANDLE(6);

   FillRect( pWindowData->hdc, &xyRect, hBrush );

   if (!bUseBrush)
   {
     SelectObject( p->s_pWindows[0]->hdc, (HBRUSH) p->s_sApp->OriginalBrush );
     DeleteObject( hBrush );
   }

   hb_retl( TRUE );
}




HB_FUNC( WVW_LBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_ADDSTRING, 0, ( LPARAM )( LPSTR ) hb_parcx( 3 ) );
}

HB_FUNC( WVW_LBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), LB_SETCURSEL, hb_parni( 3 ), 0 );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBADDSTRING )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_ADDSTRING, 0, ( LPARAM )( LPSTR ) hb_parcx( 3 ) );
}

/* WARNING!!! this function is not member of WVW_CB* group of functions */
HB_FUNC( WVW_CBSETCURSEL )
{
   SendMessage( GetDlgItem( ( HWND ) HB_PARHANDLE( 1 ), hb_parni( 2 ) ), CB_SETCURSEL, hb_parni( 3 ), 0 );
}

HB_FUNC( WVW_DLGSETICON )
{
   HICON hIcon;

   if ( HB_ISNUM( 2 ) )
   {
      hIcon = LoadIcon( hb_getWvwData()->hInstance, MAKEINTRESOURCE( hb_parni( 2 ) ) );
   }
   else
   {
      hIcon = ( HICON ) LoadImage( ( HINSTANCE ) NULL, hb_parc( 2 ), IMAGE_ICON, 0, 0, LR_LOADFROMFILE );
   }

   if ( hIcon )
   {
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_SMALL, ( LPARAM ) hIcon ); /* Set Title Bar ICON */
      SendMessage( ( HWND ) HB_PARHANDLE( 1 ), WM_SETICON, ICON_BIG,   ( LPARAM ) hIcon ); /* Set Task List Icon */
   }

   if ( hIcon )
   {
      hb_retnl( ( ULONG ) hIcon );
   }
}




/*                                                                   */
/*                      GUI Drawing Functions                        */
/*               Pritpal Bedi <pritpal@vouchcac.com>                 */
/*                                                                   */




/*                                                                   */
/*   Wvw_SetPen( nPenStyle, nWidth, nColor )                         */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide.
 */

HB_FUNC( WVW_SETPEN )
{

   int      iPenWidth, iPenStyle;
   COLORREF crColor;
   HPEN     hPen;
   WVW_DATA *  p = hb_getWvwData();

   if ( HB_ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   iPenStyle = hb_parni( 1 ) ;
   iPenWidth = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
   crColor   = HB_ISNIL( 3 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 3 );

   hPen = CreatePen( iPenStyle, iPenWidth, crColor );

   if ( hPen )
   {
      /* 20040923, was:
      if ( s_pWvwData->s_pWindows[usWinNum]->currentPen )
      {
         DeleteObject( (HPEN) s_pWvwData->s_pWindows[usWinNum]->currentPen );
      }
      s_pWvwData->s_pWindows[usWinNum]->currentPen = hPen;
      */

      if ( p->s_sApp->currentPen )
      {
         DeleteObject( (HPEN) p->s_sApp->currentPen );
      }

      p->s_sApp->currentPen = hPen;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}


/*                                                                   */
/*   Wvw_SetBrush( nStyle, nColor, [ nHatch ] )                      */
/*                                                                   */

/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide.
 */

HB_FUNC( WVW_SETBRUSH )
{

   HBRUSH   hBrush;
   LOGBRUSH lb = { 0 };
   WVW_DATA *  p = hb_getWvwData();

   if ( HB_ISNIL( 1 ) )
   {
      hb_retl( FALSE );
   }

   lb.lbStyle = hb_parnl( 1 );
   lb.lbColor = HB_ISNIL( 2 ) ? RGB( 0,0,0 ) : ( COLORREF ) hb_parnl( 2 ) ;
   lb.lbHatch = HB_ISNIL( 3 ) ? 0 : hb_parnl( 3 );

   hBrush     = CreateBrushIndirect( &lb );

   if ( hBrush )
   {
      /* 20040923,was:
      if ( s_pWvwData->s_pWindows[usWinNum]->currentBrush )
      {
         DeleteObject( (HBRUSH) s_pWvwData->s_pWindows[usWinNum]->currentBrush );
      }
      s_pWvwData->s_pWindows[usWinNum]->currentBrush = hBrush;
      */

      if ( p->s_sApp->currentBrush )
      {
         SelectObject( p->s_pWindows[0]->hdc, (HBRUSH) p->s_sApp->OriginalBrush );
         DeleteObject( (HBRUSH) p->s_sApp->currentBrush );
      }
      p->s_sApp->currentBrush = hBrush;

      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}


HB_FUNC( WVW__MAKEDLGTEMPLATE )
{
   WORD  *p, *pdlgtemplate ;
   WORD  nItems = (WORD)hb_parvni( 1, 4 ) ;
   int   i, nchar ;
   DWORD lStyle ;

   pdlgtemplate = p = ( PWORD ) LocalAlloc( LPTR, 65534 )  ;

    lStyle = hb_parvnl(1,3) ;

    *p++ = 1                        ;
    *p++ = 0xFFFF                   ;
    *p++ = LOWORD ( hb_parvnl(1,1) ) ;
    *p++ = HIWORD ( hb_parvnl(1,1) ) ;

    *p++ = LOWORD ( hb_parvnl(1,2) ) ;
    *p++ = HIWORD ( hb_parvnl(1,2) ) ;

    *p++ = LOWORD (lStyle)          ;
    *p++ = HIWORD (lStyle)          ;

    *p++ = (WORD)   nItems          ;
    *p++ = (short)  hb_parvni(1,5)   ;
    *p++ = (short)  hb_parvni(1,6)   ;
    *p++ = (short)  hb_parvni(1,7)   ;
    *p++ = (short)  hb_parvni(1,8)   ;
    *p++ = (short)  0               ;
    *p++ = (short)  0x00            ;

    if ( hb_parinfa( 1,11 ) == HB_IT_STRING )
    {
        nchar = nCopyAnsiToWideChar( p, TEXT( (char*) hb_parvcx( 1,11 ) ) ) ;
        p += nchar   ;
    }
    else
    {
      *p++ =0 ;
    }

    if ( ( lStyle & DS_SETFONT ) )
    {
      *p++ = (short) hb_parvni(1,12) ;
      *p++ = (short) hb_parvni(1,13) ;
      *p++ = (short) hb_parvni(1,14) ;

      nchar = nCopyAnsiToWideChar( p, TEXT( (char*) hb_parvcx(1,15) ) ) ;
      p += nchar ;
    } ;

   for ( i = 1 ; i <= nItems ; i++ ) {

      p = lpwAlign (p) ;

      *p++ = LOWORD ( hb_parvnl(2,i) ) ;
      *p++ = HIWORD ( hb_parvnl(2,i) ) ;

      *p++ = LOWORD ( hb_parvnl(3,i) ) ;
      *p++ = HIWORD ( hb_parvnl(3,i) ) ;

      *p++ = LOWORD ( hb_parvnl(4,i) ) ;
      *p++ = HIWORD ( hb_parvnl(4,i) ) ;

      *p++ = (short)  hb_parvni(5,i)   ;
      *p++ = (short)  hb_parvni(6,i)   ;
      *p++ = (short)  hb_parvni(7,i)   ;
      *p++ = (short)  hb_parvni(8,i)   ;

      *p++ = LOWORD ( hb_parvnl(9,i) ) ;
      *p++ = HIWORD ( hb_parvnl(9,i) ) ;

      if ( hb_parinfa( 10,i ) == HB_IT_STRING )
         {
         nchar = nCopyAnsiToWideChar( p, TEXT ( (char*) hb_parvcx( 10,i ) ) ) ;
         p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parvni(10,i) ;
         }

      if ( hb_parinfa( 11,i ) == HB_IT_STRING )
         {
         nchar = nCopyAnsiToWideChar( p, ( LPSTR ) hb_parvcx( 11,i ) ) ;
         p += nchar ;
         }
      else
         {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parvni(11,i) ;
         }

      *p++ = 0x00 ;
    } ;

    p = lpwAlign( p )  ;

    hb_retclen( ( LPSTR ) pdlgtemplate, ( ( ULONG ) p - ( ULONG ) pdlgtemplate ) ) ;

    LocalFree( LocalHandle( pdlgtemplate ) ) ;
}


HB_FUNC( WVW_GETCURSORPOS )
 {
    POINT    xy = { 0 };
    PHB_ITEM  info = hb_itemArrayNew(2);

    GetCursorPos( &xy );

    hb_arraySetNI( info, 1, xy.x );
    hb_arraySetNI( info, 2, xy.y );

    hb_itemReturnRelease( info );
  }









/* WVW_ShowWindow( [nWinNum], nCmdShow ) */
HB_FUNC( WVW_SHOWWINDOW )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int    iCmdShow = HB_ISNUM(2) ? hb_parni(2) : SW_SHOWNORMAL;

   ShowWindow( pWindowData->hWnd, iCmdShow );
}



/* WVW_UpdateWindow( [nWinNum] ) */
HB_FUNC( WVW_UPDATEWINDOW )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   UpdateWindow( pWindowData->hWnd );
}

/*
 *
 *                             Dialogs
 *          original work by Pritpal Bedi in WVTUTILS.C
 */

HB_FUNC( WVW_CREATEDIALOGDYNAMIC )
{
   PHB_ITEM pFirst = hb_param( 3,HB_IT_ANY );
   PHB_ITEM pFunc  = NULL ;
   PHB_DYNS pExecSym;
   WVW_DATA *  p = hb_getWvwData();
   HWND     hDlg = NULL;
   int      iIndex;
   int      iType = 0;
   int      iResource = hb_parni( 4 );

   /* check if we still have room for a new dialog */

   for ( iIndex = 0; iIndex < WVW_DLGML_MAX; iIndex++ )
   {
      if ( p->s_sApp->hDlgModeless[ iIndex ] == NULL )
      {
         break;
      }
   }

   if ( iIndex >= WVW_DLGML_MAX )
   {
      /* no more room */
      hb_retnl( (ULONG) NULL );
      return;
   }

   if ( HB_IS_BLOCK( pFirst ) )
   {

      /* pFunc is pointing to stored code block (later) */
      pFunc = hb_itemNew( pFirst );
      iType = 2;
   }
   else if( HB_IS_STRING( pFirst ) == HB_IT_STRING )
   {
      pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
      if ( pExecSym )
      {
         pFunc = ( PHB_ITEM ) pExecSym;
      }
      iType = 1;
   }

   {

      if ( HB_ISNUM( 3 ) )
      {
         hDlg = CreateDialogIndirect( hb_getWvwData()->hInstance,
                                      ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                        hb_parl( 2 ) ? p->s_pWindows[0]->hWnd : NULL,
                                      ( DLGPROC       ) hb_parnl( 3 ) );
      }
      else
      {

         switch ( iResource )
         {
            case 0:
            {
               hDlg = CreateDialog( hb_getWvwData()->hInstance,
                                                      hb_parc( 1 ),
                                                      hb_parl( 2 ) ? p->s_pWindows[0]->hWnd : NULL,
                                                      (DLGPROC) hb_gt_wvwDlgProcMLess );
            }
            break;

            case 1:
            {
               hDlg = CreateDialog( hb_getWvwData()->hInstance,
                                    MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                                      hb_parl( 2 ) ? p->s_pWindows[0]->hWnd : NULL,
                                                      (DLGPROC) hb_gt_wvwDlgProcMLess );
            }
            break;

            case 2:
            {
               hDlg = CreateDialogIndirect( hb_getWvwData()->hInstance,
                                            ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                              hb_parl( 2 ) ? p->s_pWindows[0]->hWnd : NULL,
                                                             (DLGPROC) hb_gt_wvwDlgProcMLess );
            }
            break;
         }
      }

      if ( hDlg )
      {
         p->s_sApp->hDlgModeless[ iIndex ] = hDlg;
         if ( pFunc )
         {

            /* if codeblock, store the codeblock and lock it there */
            if (HB_IS_BLOCK( pFirst ))
            {
               p->s_sApp->pcbFunc[ iIndex ] = pFunc;

            }

            p->s_sApp->pFunc[ iIndex ] = pFunc;
            p->s_sApp->iType[ iIndex ] = iType;
         }
         else
         {
            p->s_sApp->pFunc[ iIndex ] = NULL;
            p->s_sApp->iType[ iIndex ] = 0;
         }
         SendMessage( hDlg, WM_INITDIALOG, 0, 0 );
      }
      else
      {

         if (iType==2 && pFunc)
         {
            hb_itemRelease( pFunc );
         }

         p->s_sApp->hDlgModeless[ iIndex ] = NULL;
      }
   }

   hb_retnl( ( ULONG ) hDlg );
}



HB_FUNC( WVW_CREATEDIALOGMODAL )
{
   PHB_ITEM pFirst    = hb_param( 3,HB_IT_ANY );
   PHB_ITEM pFunc     = NULL ;
   PHB_DYNS pExecSym;
   WVW_DATA *  p = hb_getWvwData();
   int      iIndex;
   int      iResource = hb_parni( 4 );
   int      iResult   = 0;
   HWND     hParent   = HB_ISNIL( 5 ) ? p->s_pWindows[0]->hWnd : ( HWND ) HB_PARHANDLE( 5 );

   /* check if we still have room for a new dialog */
   for ( iIndex = 0; iIndex < WVW_DLGMD_MAX; iIndex++ )
   {
      if ( p->s_sApp->hDlgModal[ iIndex ] == NULL )
      {
         break;
      }
   }

   if ( iIndex >= WVW_DLGMD_MAX )
   {
      /* no more room */
      hb_retni( ( int ) NULL );
      return;
   }

   if ( HB_IS_BLOCK( pFirst ) )
   {
      /* pFunc is pointing to stored code block (later) */

      p->s_sApp->pcbFuncModal[ iIndex ] = hb_itemNew( pFirst );

      pFunc = p->s_sApp->pcbFuncModal[ iIndex ];
      p->s_sApp->pFuncModal[ iIndex ] = pFunc;
      p->s_sApp->iTypeModal[ iIndex ] = 2;
   }
   else if( HB_IS_STRING( pFirst ) == HB_IT_STRING )
   {
      pExecSym = hb_dynsymFindName( hb_itemGetCPtr( pFirst ) );
      if ( pExecSym )
      {
         pFunc = ( PHB_ITEM ) pExecSym;
      }
      p->s_sApp->pFuncModal[ iIndex ] = pFunc;
      p->s_sApp->iTypeModal[ iIndex ] = 1;
   }

   switch ( iResource )
   {
      case 0:
      {
         iResult = DialogBoxParam( hb_getWvwData()->hInstance,
                                                     hb_parc( 1 ),
                                                     hParent,
                                                     (DLGPROC) hb_gt_wvwDlgProcModal,
                                ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;

      case 1:
      {
         iResult = DialogBoxParam( hb_getWvwData()->hInstance,
                           MAKEINTRESOURCE( ( WORD ) hb_parni( 1 ) ),
                                                     hParent,
                                                     (DLGPROC) hb_gt_wvwDlgProcModal,
                                ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;

      case 2:
      {
         iResult = DialogBoxIndirectParam( hb_getWvwData()->hInstance,
                                           ( LPDLGTEMPLATE ) hb_parc( 1 ),
                                                             hParent,
                                                            (DLGPROC) hb_gt_wvwDlgProcModal,
                                        ( LPARAM ) ( DWORD ) iIndex+1 );
      }
      break;
   }

   hb_retni( iResult );
}
/* removed from GTWVT, so we remove it from here also. I really don;t like doing it... */
HB_FUNC( WVW_DELETEOBJECT )
{
   hb_retl( DeleteObject( ( HGDIOBJ ) HB_PARHANDLE( 1 ) ) );
}




HB_FUNC( WVW_SETONTOP )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   RECT rect = { 0 };

   GetWindowRect( pWindowData->hWnd, &rect );

   hb_retl( SetWindowPos( pWindowData->hWnd, HWND_TOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}



HB_FUNC( WVW_SETASNORMAL )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   RECT rect = { 0 };

   GetWindowRect( pWindowData->hWnd, &rect );

   hb_retl( SetWindowPos( pWindowData->hWnd, HWND_NOTOPMOST,
                          rect.left,
                          rect.top,
                          0,
                          0,
                          SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE ) );
}


/*                                                                   */
/*   aScr := Wvw_SaveScreen( nWinNum, nTop, nLeft, nBottom, nRight ) */
/*                                                                   */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC( WVW_SAVESCREEN )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   HBITMAP  hBmp, oldBmp;
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight, iWidth, iHeight;
   PHB_ITEM  info = hb_itemArrayNew(3);

   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = ( USHORT )hb_parni( 4 ),
            usRight  = ( USHORT )hb_parni( 5 );

   if (hb_gt_wvw_GetMainCoordMode())
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1;
   iHeight = iBottom - iTop + 1;

   hBmp    = CreateCompatibleBitmap( pWindowData->hdc, iWidth, iHeight ) ;

   oldBmp = (HBITMAP) SelectObject( pWindowData->hCompDC, hBmp );
   BitBlt( pWindowData->hCompDC, 0, 0, iWidth, iHeight, pWindowData->hdc, iLeft, iTop, SRCCOPY );
   SelectObject( pWindowData->hCompDC, oldBmp );

   hb_arraySetNI( info, 1, iWidth );
   hb_arraySetNI( info, 2, iHeight );
   hb_arraySetNInt( info, 3, ( HB_PTRDIFF ) hBmp );

   hb_itemReturnRelease( info );
}


/*                                                                     */
/*   Wvw_RestScreen( nWinNum, nTop, nLeft, nBottom, nRight, aScr, lDoNotDestroyBMP )*/
/*                                                                     */

/*TODO: reconsider, is it really needed? is it better to be handled by application?
 *      besides, with Windowing feature, it seems not needed anymore
 */

HB_FUNC( WVW_RESTSCREEN )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT   xy = { 0 };
   int     iTop, iLeft, iBottom, iRight, iWidth, iHeight;

   HBITMAP hBmp;

   BOOL    bResult = FALSE;
   BOOL    bDoNotDestroyBMP = HB_ISNIL( 7 ) ? FALSE : hb_parl( 7 );
   USHORT   usTop    = ( USHORT )hb_parni( 2 ),
            usLeft   = ( USHORT )hb_parni( 3 ),
            usBottom = ( USHORT )hb_parni( 4 ),
            usRight  = ( USHORT )hb_parni( 5 );
   if (hb_gt_wvw_GetMainCoordMode())
   {
     hb_wvw_HBFUNCPrologue(usWinNum, &usTop, &usLeft, &usBottom, &usRight);
   }

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop    = xy.y;
   iLeft   = xy.x;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   iBottom = xy.y-1;
   iRight  = xy.x-1;

   iWidth  = iRight - iLeft + 1 ;
   iHeight = iBottom - iTop + 1 ;

   hBmp    = (HBITMAP) SelectObject( pWindowData->hCompDC, ( HBITMAP ) hb_parvnl( 6,3 ) );
   if ( hBmp )
   {
      if ( ( iWidth == hb_parvni( 6,1 ) )  && ( iHeight == hb_parvni( 6,2 ) ) )
      {
         if ( BitBlt( pWindowData->hdc,
                      iLeft,
                      iTop,
                      iWidth,
                      iHeight,
                      pWindowData->hCompDC,
                      0,
                      0,
                      SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
      else
      {
         if ( StretchBlt( pWindowData->hdc,
                          iLeft,
                          iTop,
                          iWidth,
                          iHeight,
                          pWindowData->hCompDC,
                          0,
                          0,
                          hb_parvni( 6,1 ),
                          hb_parvni( 6,2 ),
                          SRCCOPY ) )
         {
            bResult = TRUE;
         }
      }
   }

   SelectObject( pWindowData->hCompDC, hBmp );

   if ( ! bDoNotDestroyBMP )
   {
      DeleteObject( ( HBITMAP ) hb_parvnl( 6,3 ) );
   }

   hb_retl( bResult );
}


/*                                                                     */
/* Wvw_CreateFont( cFontFace, nHeight, nWidth, nWeight, lItalic, lUnderline,*/
/*                 lStrikeout, nCharSet, nQuality, nEscapement )            */
/*                                                                          */
HB_FUNC( WVW_CREATEFONT )
{
   WVW_DATA *  p = hb_getWvwData();
   UINT usWinNum = p->s_usNumWindows-1;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );


   LOGFONT  logfont;
   HFONT    hFont;

   logfont.lfEscapement     = ( HB_ISNIL( 10 ) ? 0 : ( hb_parni( 10 ) * 10 ) );
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = ( HB_ISNIL(  4 ) ? 0 : hb_parni( 4 ) );
   logfont.lfItalic         = ( HB_ISNIL(  5 ) ? 0 : ( BYTE )hb_parl(  5 ) );
   logfont.lfUnderline      = ( HB_ISNIL(  6 ) ? 0 : ( BYTE )hb_parl(  6 ) );
   logfont.lfStrikeOut      = ( HB_ISNIL(  7 ) ? 0 : ( BYTE )hb_parl(  7 ) );
   logfont.lfCharSet        = ( HB_ISNIL(  8 ) ? ( BYTE )pWindowData->CodePage : (BYTE)hb_parni( 8 ) );
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( HB_ISNIL( 9 ) ? ( BYTE )DEFAULT_QUALITY : (BYTE)hb_parni( 9 ) );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = ( HB_ISNIL(  2 ) ? pWindowData->fontHeight : hb_parni( 2 ) );
   logfont.lfWidth          = ( HB_ISNIL(  3 ) ? ( pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth ) : hb_parni( 3 ) );

   strcpy( logfont.lfFaceName, ( HB_ISNIL( 1 ) ? pWindowData->fontFace : hb_parcx( 1 ) ) );

   hFont = CreateFontIndirect( &logfont );
   if ( hFont )
   {
      hb_retnl( ( ULONG ) hFont );
   }
   else
   {
      hb_retnl( 0 );
   }
}

HB_FUNC( WVW_GETKEYSTATE )
{
   hb_retni( GetKeyState( hb_parni( 1 ) ) ) ;
}

HB_FUNC( WVW_LOWORD )
{
   hb_retni( (int) ( hb_parnl( 1 ) & 0xFFFF ) );
}

HB_FUNC( WVW_HIWORD )
{
   hb_retni( (int) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}
