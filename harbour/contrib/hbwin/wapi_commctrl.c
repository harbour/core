/*
 * $Id$
 */

/*
 * Pritpal Bedi <pritpal@vouchcac.com> 14Feb2009
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*----------------------------------------------------------------------*/

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbwapi.h"
#include <commctrl.h>

/*----------------------------------------------------------------------*/
/*                      BEGIN - ImageList_* - API                       */
/*----------------------------------------------------------------------*/
/*
int ImageList_Add( HIMAGELIST himl, HBITMAP hbmImage, HBITMAP hbmMask );
*/
HB_FUNC( WAPI_IMAGELIST_ADD )
{
   wapi_ret_NI( ImageList_Add( wapi_par_HIMAGELIST( 1 ),
                               wapi_par_HBITMAP( 2 ),
                               ISNIL( 3 ) ? NULL : wapi_par_HBITMAP( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
int ImageList_AddMasked( HIMAGELIST himl, HBITMAP hbmImage, COLORREF crMask );
*/
HB_FUNC( WAPI_IMAGELIST_ADDMASKED )
{
   wapi_ret_NI( ImageList_AddMasked( wapi_par_HIMAGELIST( 1 ),
                                     wapi_par_HBITMAP( 2 ),
                                     wapi_par_COLORREF( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_BeginDrag( HIMAGELIST himlTrack, int iTrack, int dxHotspot, int dyHotspot );
*/
HB_FUNC( WAPI_IMAGELIST_BEGINDRAG )
{
   wapi_ret_L( ImageList_BeginDrag( wapi_par_HIMAGELIST( 1 ),
                                    wapi_par_INT( 2 ),
                                    wapi_par_INT( 3 ),
                                    wapi_par_INT( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/*
HRESULT ImageList_CoCreateInstance( REFCLSID rclsid, const IUnknown *punkOuter, REFIID riid, void **ppv );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_COCREATEINSTANCE )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Copy( HIMAGELIST himlDst, int iDst, HIMAGELIST himlSrc, int iSrc, UINT uFlags );
*/
#if (_WIN32_IE >= 0x0300)
HB_FUNC( WAPI_IMAGELIST_COPY )
{
   wapi_ret_L( ImageList_Copy( wapi_par_HIMAGELIST( 1 ),
                               wapi_par_INT( 2 ),
                               wapi_par_HIMAGELIST( 3 ),
                               wapi_par_INT( 4 ),
                               wapi_par_UINT( 5 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_Create( int cx, int cy, UINT flags, int cInitial, int cGrow );
*/
HB_FUNC( WAPI_IMAGELIST_CREATE )
{
   wapi_ret_HANDLE( ImageList_Create( wapi_par_INT( 1 ),
                                      wapi_par_INT( 2 ),
                                      wapi_par_UINT( 3 ),
                                      wapi_par_INT( 4 ),
                                      wapi_par_INT( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Destroy( HIMAGELIST himl );
*/
HB_FUNC( WAPI_IMAGELIST_DESTROY )
{
   wapi_ret_L( ImageList_Destroy( wapi_par_HIMAGELIST( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DragEnter( HWND hwndLock, int x, int y );
*/
HB_FUNC( WAPI_IMAGELIST_DRAGENTER )
{
   wapi_ret_L( ImageList_DragEnter( wapi_par_HWND( 1 ),
                                    wapi_par_INT( 2 ),
                                    wapi_par_INT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DragLeave( HWND hwndLock );
*/
HB_FUNC( WAPI_IMAGELIST_DRAGLEAVE )
{
   wapi_ret_L( ImageList_DragLeave( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DragMove( int x, int y );
*/
HB_FUNC( WAPI_IMAGELIST_DRAGMOVE )
{
   wapi_ret_L( ImageList_DragMove( wapi_par_INT( 1 ), wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DragShowNolock( BOOL fShow );
*/
HB_FUNC( WAPI_IMAGELIST_DRAGSHOWNOLOCK )
{
   wapi_ret_L( ImageList_DragShowNolock( hb_parl( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Draw( HIMAGELIST himl, int i, HDC hdcDst, int x, int y, UINT fStyle );
*/
HB_FUNC( WAPI_IMAGELIST_DRAW )
{
   wapi_ret_L( ImageList_Draw( wapi_par_HIMAGELIST( 1 ),
                               wapi_par_INT( 1 ),
                               wapi_par_HDC( 3 ),
                               wapi_par_INT( 4 ),
                               wapi_par_INT( 5 ),
                               wapi_par_UINT( 6 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DrawEx( HIMAGELIST himl, int i, HDC hdcDst, int x, int y, int dx, int dy,
                                          COLORREF rgbBk, COLORREF rgbFg, UINT fStyle );
*/
HB_FUNC( WAPI_IMAGELIST_DRAWEX )
{
   wapi_ret_L( ImageList_DrawEx( wapi_par_HIMAGELIST( 1 ),
                                 wapi_par_INT( 2 ),
                                 wapi_par_HDC( 3 ),
                                 wapi_par_INT( 4 ),
                                 wapi_par_INT( 5 ),
                                 wapi_par_INT( 6 ),
                                 wapi_par_INT( 7 ),
                                 wapi_par_COLORREF( 8 ),
                                 wapi_par_COLORREF( 9 ),
                                 wapi_par_UINT( 10 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_DrawIndirect( IMAGELISTDRAWPARAMS *pimldp );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_DRAWINDIRECT )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_Duplicate( HIMAGELIST himl );
*/
#if (_WIN32_IE >= 0x0400)
HB_FUNC( WAPI_IMAGELIST_DUPLICATE )
{
   wapi_ret_HANDLE( ImageList_Duplicate( wapi_par_HIMAGELIST( 1 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
VOID ImageList_EndDrag( VOID );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_ENDDRAG )
{
   ImageList_EndDrag();
}
#endif
/*----------------------------------------------------------------------*/
/*
COLORREF ImageList_GetBkColor( HIMAGELIST himl );
*/
HB_FUNC( WAPI_IMAGELIST_GETBKCOLOR )
{
   wapi_ret_COLORREF( ImageList_GetBkColor( wapi_par_HIMAGELIST( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_GetDragImage( POINT *ppt, POINT *pptHotspot );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_GETDRAGIMAGE )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
HICON ImageList_GetIcon( HIMAGELIST himl, int i, UINT flags );
*/
HB_FUNC( WAPI_IMAGELIST_GETICON )
{
   wapi_ret_HANDLE( ImageList_GetIcon( wapi_par_HIMAGELIST( 1 ),
                                       wapi_par_INT( 2 ),
                                       wapi_par_UINT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_GetIconSize( HIMAGELIST himl, int *cx, int *cy );
*/
HB_FUNC( WAPI_IMAGELIST_GETICONSIZE )
{
   int cx;
   int cy;

   if( ImageList_GetIconSize( wapi_par_HIMAGELIST( 1 ), &cx, &cy ) )
   {
      hb_storni( cx, 2 );
      hb_storni( cy, 3 );
      hb_retl( 1 );
   }
   else
   {
      hb_retl( 0 );
   }
}
/*----------------------------------------------------------------------*/
/*
int ImageList_GetImageCount( HIMAGELIST himl );
*/
HB_FUNC( WAPI_IMAGELIST_GETIMAGECOUNT )
{
   wapi_ret_NI( ImageList_GetImageCount( wapi_par_HIMAGELIST( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*                          T O R E V I E W

BOOL ImageList_GetImageInfo( HIMAGELIST himl, int i, IMAGEINFO *pImageInfo );
*/
HB_FUNC( WAPI_IMAGELIST_GETIMAGEINFO )
{
   IMAGEINFO ii;

   if( ImageList_GetImageInfo( wapi_par_HIMAGELIST( 1 ), wapi_par_INT( 2 ), &ii ) )
   {
      hb_retclen( ( char * ) &ii, sizeof( IMAGEINFO ) );
   }
}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_LoadImage( HINSTANCE hi, LPCTSTR lpbmp, int cx, int cGrow,
                                COLORREF crMask, UINT uType, UINT uFlags );
*/
HB_FUNC( WAPI_IMAGELIST_LOADIMAGE )
{
   LPCTSTR image;

   if( ISCHAR( 2 ) )
      image = ( LPCTSTR ) hb_parc( 2 );
   else
      image = ( LPCTSTR ) MAKEINTRESOURCE( wapi_par_INT( 2 ) );

   wapi_ret_HANDLE( ImageList_LoadImage( wapi_par_HINSTANCE( 1 ),
                                         image,
                                         wapi_par_INT( 3 ),
                                         wapi_par_INT( 4 ),
                                         wapi_par_COLORREF( 5 ),
                                         wapi_par_UINT( 6 ),
                                         wapi_par_UINT( 7 ) ) );
}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_Merge( HIMAGELIST himl1, int i1, HIMAGELIST himl2, int i2, int dx, int dy );
*/
HB_FUNC( WAPI_IMAGELIST_MERGE )
{
   wapi_ret_HANDLE( ImageList_Merge( wapi_par_HIMAGELIST( 1 ),
                                     wapi_par_INT( 2 ),
                                     wapi_par_HIMAGELIST( 3 ),
                                     wapi_par_INT( 4 ),
                                     wapi_par_INT( 5 ),
                                     wapi_par_INT( 6 ) ) );

}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_Read( LPSTREAM pstm );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_READ )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
HRESULT ImageList_ReadEx( DWORD dwFlags, LPSTREAM pstm, REFIID riid, void **ppv );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_READEX )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Remove( HIMAGELIST himl, int i );
*/
HB_FUNC( WAPI_IMAGELIST_REMOVE )
{
   wapi_ret_L( ImageList_Remove( wapi_par_HIMAGELIST( 1 ),
                                 wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Replace( HIMAGELIST himl, int i, HBITMAP hbmImage, HBITMAP hbmMask );
*/
HB_FUNC( WAPI_IMAGELIST_REPLACE )
{
   wapi_ret_L( ImageList_Replace( wapi_par_HIMAGELIST( 1 ),
                                  wapi_par_INT( 2 ),
                                  wapi_par_HBITMAP( 3 ),
                                  wapi_par_HBITMAP( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/*
int ImageList_ReplaceIcon( HIMAGELIST himl, int i, HICON hicon );
*/
HB_FUNC( WAPI_IMAGELIST_REPLACEICON )
{
   wapi_ret_NI( ImageList_ReplaceIcon( wapi_par_HIMAGELIST( 1 ),
                                       wapi_par_INT( 2 ),
                                       wapi_par_HICON( 3 ) ) );

}
/*----------------------------------------------------------------------*/
/*
COLORREF ImageList_SetBkColor( HIMAGELIST himl, COLORREF clrBk );
*/
HB_FUNC( WAPI_IMAGELIST_SETBKCOLOR )
{
   wapi_ret_COLORREF( ImageList_SetBkColor( wapi_par_HIMAGELIST( 1 ),
                                            wapi_par_COLORREF( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
int ImageList_SetColorTable( HIMAGELIST himl, int start, int len, RGBQUAD *prgb );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_SETCOLORTABLE )
{
   wapi_ret_NI( ImageList_SetColorTable( wapi_par_HIMAGELIST( 1 ),
                                         wapi_par_INT( 2 ),
                                         wapi_par_INT( 3 ),
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_SetDragCursorImage( HIMAGELIST himlDrag, int iDrag, int dxHotspot, int dyHotspot );
*/
HB_FUNC( WAPI_IMAGELIST_SETDRAGCURSORIMAGE )
{
   wapi_ret_L( ImageList_SetDragCursorImage( wapi_par_HIMAGELIST( 1 ),
                                             wapi_par_INT( 2 ),
                                             wapi_par_INT( 3 ),
                                             wapi_par_INT( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_SetIconSize( HIMAGELIST himl, int cx, int cy );
*/
HB_FUNC( WAPI_IMAGELIST_SETICONSIZE )
{
   wapi_ret_L( ImageList_SetIconSize( wapi_par_HIMAGELIST( 1 ),
                                      wapi_par_INT( 2 ),
                                      wapi_par_INT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_SetImageCount( HIMAGELIST himl, UINT uNewCount );
*/
#if (_WIN32_IE >= 0x0400)
HB_FUNC( WAPI_IMAGELIST_SETIMAGECOUNT )
{
   wapi_ret_L( ImageList_SetImageCount( wapi_par_HIMAGELIST( 1 ),
                                        wapi_par_UINT( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_SetOverlayImage( HIMAGELIST himl, int iImage, int iOverlay );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_SETOVERLAYIMAGE )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
BOOL ImageList_Write( HIMAGELIST himl, LPSTREAM pstm );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_WRITE )
{
}
#endif
/*----------------------------------------------------------------------*/
/*
HRESULT ImageList_WriteEx( HIMAGELIST himl, DWORD dwFlags, LPSTREAM pstm );
*/
#if 0
HB_FUNC( WAPI_IMAGELIST_WRITEEX )
{
}
#endif
/*----------------------------------------------------------------------*/
/*                            END - ImageList_* - API                   */
/*----------------------------------------------------------------------*/


/*----------------------------------------------------------------------*/
/*                           Tab Control Macros                         */
/*----------------------------------------------------------------------*/
/* Wapi_TabCtrl_InsertItem( hWndTab, nInsertPos, cText, iImageListIndex ) */

HB_FUNC( WAPI_TABCTRL_INSERTITEM )
{
   TC_ITEM item;
   LPTSTR szText = HB_TCHAR_CONVTO( hb_parc( 3 ) );

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = ISNIL( 4 ) ? -1 : wapi_par_INT( 4 );
   item.pszText = szText;

   wapi_ret_NI( TabCtrl_InsertItem( wapi_par_HWND( 1 ), wapi_par_INT( 3 ), &item ) );

   HB_TCHAR_FREE( szText );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_SETCURSEL )
{
   wapi_ret_NI( TabCtrl_SetCurSel( wapi_par_HWND( 1 ) , hb_parni( 2 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_GETCURSEL )
{
   wapi_ret_NI( TabCtrl_GetCurSel( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_GETITEM )
{
   TC_ITEM item;
   hb_retl( TabCtrl_GetItem( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), &item ) );

   /* assign  item to param 3 */
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_GETITEMCOUNT )
{
   wapi_ret_NI( TabCtrl_GetItemCount( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_GETITEMRECT )
{
   RECT rc;
   PHB_ITEM aRect = hb_itemArrayNew( 4 );
   PHB_ITEM temp;

   TabCtrl_GetItemRect( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), &rc );

   temp = hb_itemPutNL( NULL, rc.left );
   hb_arraySet( aRect, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, rc.top );
   hb_arraySet( aRect, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, rc.right );
   hb_arraySet( aRect, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, rc.bottom );
   hb_arraySet( aRect, 4, temp );
   hb_itemRelease( temp );

   hb_itemReturnRelease( aRect );
}
/*----------------------------------------------------------------------*/

HB_FUNC( WAPI_TABCTRL_GETROWCOUNT )
{
   wapi_ret_NI( TabCtrl_GetRowCount( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetImageList(hwnd) */
/* (HIMAGELIST)SNDMSG((hwnd), TCM_GETIMAGELIST, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_GETIMAGELIST )
{
   wapi_ret_NINT( ( LONG ) TabCtrl_GetImageList( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* #define TabCtrl_SetImageList(hwnd, himl) */
/* (HIMAGELIST)SNDMSG((hwnd), TCM_SETIMAGELIST, 0, (LPARAM)(HIMAGELIST)(himl)) */

HB_FUNC( WAPI_TABCTRL_SETIMAGELIST )
{
   wapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_SetImageList( wapi_par_HWND( 1 ),
                                                       wapi_par_HIMAGELIST( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* Wapi_TabCtrl_SetItem( hWndTab, nInsertPos, cText, iImageListIndex ) */
/* (BOOL)SNDMSG((hwnd), TCM_SETITEM, (WPARAM)(int)(iItem), (LPARAM)(TC_ITEM FAR*)(pitem)) */

HB_FUNC( WAPI_TABCTRL_SETITEM )
{
   TC_ITEM item;
   LPTSTR szText = HB_TCHAR_CONVTO( hb_parc( 3 ) );

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = ISNIL( 4 ) ? -1 : wapi_par_INT( 4 );
   item.pszText = szText;

   wapi_ret_L( TabCtrl_SetItem( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), &item ) );

   HB_TCHAR_FREE( szText );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_DeleteAllItems(hwnd) */
/* (BOOL)SNDMSG((hwnd), TCM_DELETEALLITEMS, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_DELETEALLITEMS )
{
   wapi_ret_L( TabCtrl_DeleteAllItems( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_DeleteItem(hwnd, i) */
/* (BOOL)SNDMSG((hwnd), TCM_DELETEITEM, (WPARAM)(int)(i), 0L) */

HB_FUNC( WAPI_TABCTRL_DELETEITEM )
{
   wapi_ret_L( TabCtrl_DeleteItem( wapi_par_HWND( 1 ), ( WPARAM ) wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_HitTest(hwndTC, pinfo) */
/* (int)SNDMSG((hwndTC), TCM_HITTEST, 0, (LPARAM)(TC_HITTESTINFO FAR*)(pinfo)) */

/* waiting for structures */

HB_FUNC( WAPI_TABCTRL_HITTEST )
{
   TCHITTESTINFO tcht;

   hb_parni( TabCtrl_HitTest( wapi_par_HWND( 1 ), &tcht ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetItemExtra(hwndTC, cb) */
/* (BOOL)SNDMSG((hwndTC), TCM_SETITEMEXTRA, (WPARAM)(cb), 0L) */

HB_FUNC( WAPI_TABCTRL_SETITEMEXTRA )
{
   wapi_ret_L( TabCtrl_SetItemExtra( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_AdjustRect(hwnd, bLarger, prc) */
/* (int)SNDMSG(hwnd, TCM_ADJUSTRECT, (WPARAM)(BOOL)(bLarger), (LPARAM)(RECT FAR *)prc) */

HB_FUNC( WAPI_TABCTRL_ADJUSTRECT )
{
   if( ISARRAY( 3 ) )
   {
      RECT rc;

      rc.left   = hb_parnl( 3, 1 );
      rc.top    = hb_parnl( 3, 2 );
      rc.right  = hb_parnl( 3, 3 );
      rc.bottom = hb_parnl( 3, 4 );

      TabCtrl_AdjustRect( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ), &rc );

      hb_stornl( rc.left  , 3, 1 );
      hb_stornl( rc.top   , 3, 2 );
      hb_stornl( rc.right , 3, 3 );
      hb_stornl( rc.bottom, 3, 4 );
   }
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetItemSize(hwnd, x, y) */
/* (DWORD)SNDMSG((hwnd), TCM_SETITEMSIZE, 0, MAKELPARAM(x,y)) */

HB_FUNC( WAPI_TABCTRL_SETITEMSIZE )
{
   wapi_ret_NINT( TabCtrl_SetItemSize( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), wapi_par_INT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_RemoveImage(hwnd, i) */
/* (void)SNDMSG((hwnd), TCM_REMOVEIMAGE, i, 0L) */

HB_FUNC( WAPI_TABCTRL_REMOVEIMAGE )
{
   TabCtrl_RemoveImage( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetPadding(hwnd,  cx, cy) */
/* (void)SNDMSG((hwnd), TCM_SETPADDING, 0, MAKELPARAM(cx, cy)) */

HB_FUNC( WAPI_TABCTRL_SETPADDING )
{
   TabCtrl_SetPadding( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), wapi_par_INT( 3 ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetToolTips(hwnd) */
/* (HWND)SNDMSG((hwnd), TCM_GETTOOLTIPS, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_GETTOOLTIPS )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_GetToolTips( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_NINT( -1 );
#endif
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetToolTips(hwnd, hwndTT) */
/* (void)SNDMSG((hwnd), TCM_SETTOOLTIPS, (WPARAM)(hwndTT), 0L) */

HB_FUNC( WAPI_TABCTRL_SETTOOLTIPS )
{
#if ! defined(HB_OS_WIN_CE)
   TabCtrl_SetToolTips( wapi_par_HWND( 1 ), wapi_par_HWND( 2 ) );
#endif
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetCurFocus(hwnd) */
/* (int)SNDMSG((hwnd), TCM_GETCURFOCUS, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETCURFOCUS )
{
   wapi_ret_NI( TabCtrl_GetCurFocus( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetCurFocus(hwnd, i) */
/* SNDMSG((hwnd),TCM_SETCURFOCUS, i, 0) */

HB_FUNC( WAPI_TABCTRL_SETCURFOCUS )
{
   TabCtrl_SetCurFocus( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetMinTabWidth(hwnd, x) */
/* (int)SNDMSG((hwnd), TCM_SETMINTABWIDTH, 0, x) */

HB_FUNC( WAPI_TABCTRL_SETMINTABWIDTH )
{
   /* wapi_ret_NI( TabCtrl_SetMinTabWidth( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) ) ); */
   wapi_ret_NI( ( int ) SendMessage( wapi_par_HWND( 1 ), TCM_SETMINTABWIDTH, ( WPARAM ) 0, ( LPARAM ) wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_DeselectAll(hwnd, fExcludeFocus) */
/* (void)SNDMSG((hwnd), TCM_DESELECTALL, fExcludeFocus, 0) */

HB_FUNC( WAPI_TABCTRL_DESELECTALL )
{
   /* TabCtrl_DeselectAll( wapi_par_HWND( 1 ), wapi_par_UINT( 2 ) ); */
   SendMessage( wapi_par_HWND( 1 ), TCM_DESELECTALL, ( WPARAM ) wapi_par_UINT( 2 ), ( LPARAM ) 0 );
}
/*----------------------------------------------------------------------*/

#if (_WIN32_IE >= 0x0400)

/* TabCtrl_HighlightItem(hwnd, i, fHighlight) */
/* (BOOL)SNDMSG((hwnd), TCM_HIGHLIGHTITEM, (WPARAM)(i), (LPARAM)MAKELONG (fHighlight, 0)) */

HB_FUNC( WAPI_TABCTRL_HIGHLIGHTITEM )
{
   wapi_ret_L( TabCtrl_HighlightItem( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), wapi_par_WORD( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetExtendedStyle(hwnd, dw) */
/* (DWORD)SNDMSG((hwnd), TCM_SETEXTENDEDSTYLE, 0, dw) */

HB_FUNC( WAPI_TABCTRL_SETEXTENDEDSTYLE )
{
   wapi_ret_NINT( TabCtrl_SetExtendedStyle( wapi_par_HWND( 1 ), wapi_par_DWORD( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetExtendedStyle(hwnd)
/* (DWORD)SNDMSG((hwnd), TCM_GETEXTENDEDSTYLE, 0, 0)

HB_FUNC( WAPI_TABCTRL_GETEXTENDEDSTYLE )
{
   wapi_ret_NINT( TabCtrl_GetExtendedStyle( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetUnicodeFormat(hwnd, fUnicode) */
/* (BOOL)SNDMSG((hwnd), TCM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0) */

HB_FUNC( WAPI_TABCTRL_SETUNICODEFORMAT )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_L( TabCtrl_SetUnicodeFormat( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ) ) );
#endif
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetUnicodeFormat(hwnd) */
/* (BOOL)SNDMSG((hwnd), TCM_GETUNICODEFORMAT, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETUNICODEFORMAT )
{
#if ! defined(HB_OS_WIN_CE)
   wapi_ret_L( TabCtrl_GetUnicodeFormat( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}

#endif

/*----------------------------------------------------------------------*/
/* not an API */

HB_FUNC( WAPI_TABCTRL_CREATE )
{
   HWND hwnd;
   HWND hbutton;
   LONG hFont;
   LONG style;
   style   = ISNIL( 6 ) ? 0 : ( LONG ) hb_parnl( 6 );
   hwnd    = ( HWND ) hb_parnl( 1 );
   hFont   = SendMessage( hwnd, WM_GETFONT, 0, 0 );
   hbutton = CreateWindowEx( 0, WC_TABCONTROL, NULL, style, hb_parni( 2 ), hb_parni( 3 ) , hb_parni( 4 ), hb_parni( 5 ), hwnd, NULL, GetModuleHandle( NULL ), NULL );
   SendMessage( hbutton, ( UINT ) WM_SETFONT, ( WPARAM ) hFont, 1 );
   hb_retnl( ( LONG ) hbutton );
}

/*----------------------------------------------------------------------*/
/* not an API */

HB_FUNC( WAPI_TABCTRL_ADDITEM )
{
   int     iCount = TabCtrl_GetItemCount( wapi_par_HWND( 1 ) );
   LPTSTR  szText = HB_TCHAR_CONVTO( hb_parc( 2 ) );
   TC_ITEM item;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = ISNIL( 3 ) ? -1 : wapi_par_INT( 3 );
   item.pszText = szText;

   wapi_ret_NI( TabCtrl_InsertItem( wapi_par_HWND( 1 ), iCount, &item ) );
   HB_TCHAR_FREE( szText );
}
/*----------------------------------------------------------------------*/
