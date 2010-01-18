/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (commctrl)
 *
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbwinuni.h"
#include "hbwapi.h"

#include <commctrl.h>

#if defined( __BORLANDC__ ) && !defined( HB_ARCH_64BIT )
    #undef MAKELONG
    #define MAKELONG(a,b) ((LONG)(((WORD)((DWORD_PTR)(a) & 0xffff)) | \
                          (((DWORD)((WORD)((DWORD_PTR)(b) & 0xffff))) << 16)))
#endif

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
                               wapi_par_HBITMAP( 3 ) ) );
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
#if ( _WIN32_IE >= 0x0300 )
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
#if ( _WIN32_IE >= 0x0400 )
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
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
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
      hb_retclen( ( char * ) &ii, sizeof( IMAGEINFO ) );
   else
      hb_retc_null();
}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_LoadImage( HINSTANCE hi, LPCTSTR lpbmp, int cx, int cGrow,
                                COLORREF crMask, UINT uType, UINT uFlags );
*/
HB_FUNC( WAPI_IMAGELIST_LOADIMAGE )
{
   LPCTSTR image;

   if( HB_ISCHAR( 2 ) )
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
#if ( _WIN32_IE >= 0x0400 )
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

   void * hText;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = HB_ISNUM( 4 ) ? wapi_par_INT( 4 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );

   wapi_ret_NI( TabCtrl_InsertItem( wapi_par_HWND( 1 ), wapi_par_INT( 3 ), &item ) );

   hb_strfree( hText );
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
   PHB_ITEM aRect = hb_itemArrayNew( 4 );
   RECT rc;

   ( void ) TabCtrl_GetItemRect( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), &rc );

   hb_arraySetNL( aRect, 1, rc.left );
   hb_arraySetNL( aRect, 2, rc.top );
   hb_arraySetNL( aRect, 3, rc.right );
   hb_arraySetNL( aRect, 4, rc.bottom );

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
   wapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_GetImageList( wapi_par_HWND( 1 ) ) );
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

   void * hText;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = HB_ISNUM( 4 ) ? wapi_par_INT( 4 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );

   wapi_ret_L( TabCtrl_SetItem( wapi_par_HWND( 1 ), wapi_par_INT( 2 ), &item ) );

   hb_strfree( hText );
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
   if( HB_ISARRAY( 3 ) )
   {
      RECT rc;

      rc.left   = hb_parvnl( 3, 1 );
      rc.top    = hb_parvnl( 3, 2 );
      rc.right  = hb_parvnl( 3, 3 );
      rc.bottom = hb_parvnl( 3, 4 );

      ( void ) TabCtrl_AdjustRect( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ), &rc );

      hb_storvnl( rc.left  , 3, 1 );
      hb_storvnl( rc.top   , 3, 2 );
      hb_storvnl( rc.right , 3, 3 );
      hb_storvnl( rc.bottom, 3, 4 );
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
#if ! defined( HB_OS_WIN_CE )
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
#if ! defined( HB_OS_WIN_CE )
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

#if ( _WIN32_IE >= 0x0400 )

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
/* TabCtrl_GetExtendedStyle(hwnd) */
/* (DWORD)SNDMSG((hwnd), TCM_GETEXTENDEDSTYLE, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETEXTENDEDSTYLE )
{
   wapi_ret_NINT( TabCtrl_GetExtendedStyle( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* TabCtrl_SetUnicodeFormat(hwnd, fUnicode) */
/* (BOOL)SNDMSG((hwnd), TCM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0) */

HB_FUNC( WAPI_TABCTRL_SETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_L( TabCtrl_SetUnicodeFormat( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
/*----------------------------------------------------------------------*/
/* TabCtrl_GetUnicodeFormat(hwnd) */
/* (BOOL)SNDMSG((hwnd), TCM_GETUNICODEFORMAT, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE )
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
   HWND hwnd = wapi_par_HWND( 1 );
   HWND hbutton = CreateWindowEx( 0, WC_TABCONTROL, NULL, ( LONG ) hb_parnl( 6 ) /* style */, hb_parni( 2 ), hb_parni( 3 ) , hb_parni( 4 ), hb_parni( 5 ), hwnd, NULL, GetModuleHandle( NULL ), NULL );
   SendMessage( hbutton, ( UINT ) WM_SETFONT, ( WPARAM ) ( HFONT ) SendMessage( hwnd, WM_GETFONT, 0, 0 ), 1 );
   wapi_ret_HANDLE( hbutton );
}

/*----------------------------------------------------------------------*/
/* not an API */

HB_FUNC( WAPI_TABCTRL_ADDITEM )
{
   int     iCount = TabCtrl_GetItemCount( wapi_par_HWND( 1 ) );
   TC_ITEM item;

   void * hText;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = HB_ISNUM( 3 ) ? wapi_par_INT( 3 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 2, &hText, NULL );

   wapi_ret_NI( TabCtrl_InsertItem( wapi_par_HWND( 1 ), iCount, &item ) );

   hb_strfree( hText );
}
/*----------------------------------------------------------------------*/
/*
 *                          TreeView_* Functions
 */
/*----------------------------------------------------------------------*/
/*
   HIMAGELIST TreeView_CreateDragImage( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_CREATEDRAGIMAGE )
{
   wapi_ret_HANDLE( TreeView_CreateDragImage( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_DeleteAllItems( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_DELETEALLITEMS )
{
   wapi_ret_L( TreeView_DeleteAllItems( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HWND TreeView_EditLabel( HWND hWndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_EDITLABEL )
{
   wapi_ret_HANDLE( TreeView_EditLabel( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_EndEditLabelNow( HWND hwndTV, BOOL b );
*/
HB_FUNC( WAPI_TREEVIEW_ENDEDITLABELNOW )
{
   wapi_ret_L( TreeView_EndEditLabelNow( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_EnsureVisible( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_ENSUREVISIBLE )
{
   wapi_ret_L( TreeView_EnsureVisible( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_Expand( HWND hwndTV, HTREEITEM hItem, UINT flag );
*/
HB_FUNC( WAPI_TREEVIEW_EXPAND )
{
   wapi_ret_L( TreeView_Expand( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_UINT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
... COLORREF TreeView_GetBkColor( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   wapi_ret_COLORREF( TreeView_GetBkColor( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
/*----------------------------------------------------------------------*/
/* IE 5.0
...UINT TreeView_GetCheckState( HWND hwndTV, HTREEITEM hItem );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_GETCHECKSTATE )
{
   wapi_ret_UINT( TreeView_GetCheckState( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetChild( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETCHILD )
{
   wapi_ret_HANDLE( TreeView_GetChild( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_GetCount( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETCOUNT )
{
   wapi_ret_UINT( TreeView_GetCount( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetDropHilight( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETDROPHILIGHT )
{
   wapi_ret_HANDLE( TreeView_GetDropHilight( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HWND TreeView_GetEditControl( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETEDITCONTROL )
{
   wapi_ret_HANDLE( TreeView_GetEditControl( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
/* Vista
...DWORD TreeView_GetExtendedStyle( HWND hwnd );
*/
HB_FUNC( WAPI_TREEVIEW_GETEXTENDEDSTYLE )
{
   wapi_ret_DWORD( TreeView_GetExtendedStyle( wapi_par_HWND( 1 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetFirstVisible( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETFIRSTVISIBLE )
{
   wapi_ret_HANDLE( TreeView_GetFirstVisible( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HIMAGELIST TreeView_GetImageList( HWND hwndTV, INT iImage );
*/
HB_FUNC( WAPI_TREEVIEW_GETIMAGELIST )
{
   wapi_ret_HANDLE( TreeView_GetImageList( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_GetIndent( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETINDENT )
{
   wapi_ret_UINT( TreeView_GetIndent( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/* IE 4.0
... COLORREF TreeView_GetInsertMarkColor( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_GETINSERTMARKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_COLORREF( TreeView_GetInsertMarkColor( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_GetISearchString( HWND hwndTV, LPTSTR lpsz );
*/
HB_FUNC( WAPI_TREEVIEW_GETISEARCHSTRING )
{
   /* wapi_ret_( TreeView_GetISearchString( wapi_par_HWND( 1 ), LPTSTR ) ); */
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_GetItem( HWND hwndTV, LPTVITEM pitem );
-- Version 4.71 or later --
...BOOL TreeView_GetItem( HWND hwndTV, LPTVITEMEX pitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETITEM )
{
   /* wapi_ret_( TreeView_GetItem( wapi_par_HWND( 1 ), LPTVITEM ) ); */
}
/*----------------------------------------------------------------------*/
/* IE 4.0
...int TreeView_GetItemHeight( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_GETITEMHEIGHT )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_NI( TreeView_GetItemHeight( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_NI( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
#if 0
/* Vista
... BOOL TreeView_GetItemPartRect( HWND hwnd, HTREEITEM hitem, RECT *prc, TVITEMPART *partid );
*/
HB_FUNC( WAPI_TREEVIEW_GETITEMPARTRECT )
{
   RECT       rc;
   TVITEMPART partid;

   wapi_ret_L( TreeView_GetItemPartRect( wapi_par_HWND( 1 ),, wapi_par_HANDLE( 2 ), &rc, &partid ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_GetItemRect( HWND hwndTV, HTREEITEM hitem, LPRECT prc, BOOL fItemRect );
*/
HB_FUNC( WAPI_TREEVIEW_GETITEMRECT )
{
   LPRECT prc = NULL;
   wapi_ret_L( TreeView_GetItemRect( wapi_par_HWND( 1 ), ( HTREEITEM ) wapi_par_HANDLE( 2 ), prc, wapi_par_BOOL( 4 ) ) );
}
/*----------------------------------------------------------------------*/
/* IE 5.0
...UINT TreeView_GetItemState( HWND hwndTV, HTREEITEM hItem, UINT stateMask );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_GETITEMSTATE )
{
   wapi_ret_UINT( TreeView_GetItemState( wapi_par_HWND( 1 ), ( HTREEITEM ) wapi_par_HANDLE( 2 ), wapi_par_UINT( 3 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/* IE 4.0
...HTREEITEM TreeView_GetLastVisible( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_GETLASTVISIBLE )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_HANDLE( TreeView_GetLastVisible( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_HANDLE( NULL );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...COLOREF TreeView_GetLineColor( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_GETLINECOLOR )
{
   wapi_ret_COLORREF( TreeView_GetLineColor( wapi_par_HWND( 1 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetNextItem( HWND hwndTV, HTREEITEM hitem, UINT flag );
*/
HB_FUNC( WAPI_TREEVIEW_GETNEXTITEM )
{
   wapi_ret_HANDLE( TreeView_GetNextItem( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_UINT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
/*
...HTREEITEM TreeView_GetNextSelected( HWND hwnd, TREEITEM *hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETNEXTSELECTED )
{
   wapi_ret_HANDLE( TreeView_GetNextSelected( wapi_par_HWND( 1 ), wapi_par_TREEITEM( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetNextSibling( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETNEXTSIBLING )
{
   wapi_ret_HANDLE( TreeView_GetNextSibling( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetNextVisible( HWND hwndTV, HTREEITEM hitem );
);
*/
HB_FUNC( WAPI_TREEVIEW_GETNEXTVISIBLE )
{
   wapi_ret_HANDLE( TreeView_GetNextVisible( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetParent( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETPARENT )
{
   wapi_ret_HANDLE( TreeView_GetParent( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetPrevSibling( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETPREVSIBLING )
{
   wapi_ret_HANDLE( TreeView_GetPrevSibling( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetPrevVisible( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_GETPREVVISIBLE )
{
   wapi_ret_HANDLE( TreeView_GetPrevVisible( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetRoot( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETROOT )
{
   wapi_ret_HANDLE( TreeView_GetRoot( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_GetScrollTime( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETSCROLLTIME )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   wapi_ret_UINT( TreeView_GetScrollTime( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_UINT( 0 );
#endif
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_GetSelection( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETSELECTION )
{
   wapi_ret_HANDLE( TreeView_GetSelection( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...COLORREF TreeView_GetTextColor( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_GETTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_COLORREF( TreeView_GetTextColor( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...HWND TreeView_GetToolTips( HWND hwndTV );
*/
#if ( _WIN32_IE >= 0x0300 )
HB_FUNC( WAPI_TREEVIEW_GETTOOLTIPS )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_HANDLE( TreeView_GetToolTips( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_HANDLE( NULL );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_GetUnicodeFormat( HWND hwnd );
*/
HB_FUNC( WAPI_TREEVIEW_GETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   wapi_ret_L( TreeView_GetUnicodeFormat( wapi_par_HWND( 1 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_GetVisibleCount( HWND hwndTV );
*/
HB_FUNC( WAPI_TREEVIEW_GETVISIBLECOUNT )
{
   wapi_ret_UINT( TreeView_GetVisibleCount( wapi_par_HWND( 1 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_HitTest( HWND hwndTV, LPTVHITTESTINFO lpht );
*/
HB_FUNC( WAPI_TREEVIEW_HITTEST )
{
   wapi_ret_HANDLE( TreeView_HitTest( wapi_par_HWND( 1 ), wapi_par_TVHITTESTINFO( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...HTREEITEM TreeView_InsertItem( HWND hwndTV, LPTVINSERTSTRUCT lpis );
*/
HB_FUNC( WAPI_TREEVIEW_INSERTITEM )
{
   wapi_ret_HANDLE( TreeView_InsertItem( wapi_par_HWND( 1 ), wapi_par_TVINSERTSTRUCT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
/*
...HTREEITEM TreeView_MapAccIDToHTREEITEM( HWND hwnd, UINT id );
*/
HB_FUNC( WAPI_TREEVIEW_MAPACCIDTOHTREEITEM )
{
   wapi_ret_HANDLE( TreeView_MapAccIDToHTREEITEM( wapi_par_HWND( 1 ), wapi_par_UINT( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
#if 0
/*
...UINT TreeView_MapHTREEITEMtoAccID( HWND hwnd, HTREEITEM htreeitem );
*/
HB_FUNC( WAPI_TREEVIEW_MAPHTREEITEMTOACCID )
{
   wapi_ret_UINT( TreeView_MapHTREEITEMtoAccID( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_Select( HWND hwndTV, HTREEITEM hitem, UINT flag );
*/
HB_FUNC( WAPI_TREEVIEW_SELECT )
{
   wapi_ret_L( TreeView_Select( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_UINT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SelectDropTarget( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_SELECTDROPTARGET )
{
   wapi_ret_L( TreeView_SelectDropTarget( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SelectItem( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_SELECTITEM )
{
   wapi_ret_L( TreeView_SelectItem( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SelectSetFirstVisible( HWND hwndTV, HTREEITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_SELECTSETFIRSTVISIBLE )
{
   wapi_ret_L( TreeView_SelectSetFirstVisible( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
/*
...LRESULT TreeView_SetAutoScrollInfo( HWND hwnd, UINT uPixPerSec, UINT uUpdateTime );
*/
HB_FUNC( WAPI_TREEVIEW_SETAUTOSCROLLINFO )
{
   wapi_ret_LRESULT( TreeView_SetAutoScrollInfo( wapi_par_HWND( 1 ), wapi_par_UINT( 2 ), wapi_par_UINT( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...COLORREF TreeView_SetBkColor( HWND hwndTV, COLORREF clrBk );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_SETBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_COLORREF( TreeView_SetBkColor( wapi_par_HWND( 1 ), wapi_par_COLORREF( 2 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_SetCheckState( HWND hwndTV, HTREEITEM hItem, BOOL fCheck );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_SETCHECKSTATE )
{
   /* wapi_ret_UINT( TreeView_SetCheckState( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_BOOL( 3 ) ) ); */
}
#endif
/*----------------------------------------------------------------------*/
#if 0
/*
...HRESULT TreeView_SetExtendedStyle( HWND hwnd, DWORD dw, UINT mask );
*/
HB_FUNC( WAPI_TREEVIEW_SETEXTENDEDSTYLE )
{
   wapi_ret_HRESULT( TreeView_SetExtendedStyle( wapi_par_HWND( 1 ), wapi_par_DWORD( 2 ), wapi_par_UINT( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...HIMAGELIST TreeView_SetImageList( HWND hwndTV, HIMAGELIST himl, INT iImage );
*/
HB_FUNC( WAPI_TREEVIEW_SETIMAGELIST )
{
   wapi_ret_HANDLE( TreeView_SetImageList( wapi_par_HWND( 1 ), wapi_par_HIMAGELIST( 2 ), wapi_par_INT( 3 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SetIndent( HWND hwndTV, INT indent );
*/
HB_FUNC( WAPI_TREEVIEW_SETINDENT )
{
   wapi_ret_L( TreeView_SetIndent( wapi_par_HWND( 1 ), wapi_par_INT( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SetInsertMark( HWND hwndTV, HTREEITEM htiInsert, BOOL fAfter );

*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_SETINSERTMARK )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_L( TreeView_SetInsertMark( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_BOOL( 3 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...COLORREF TreeView_SetInsertMarkColor( HWND hwndTV, COLORREF clrInsertMark );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_SETINSERTMARKCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_COLORREF( TreeView_SetInsertMarkColor( wapi_par_HWND( 1 ), wapi_par_COLORREF( 2 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SetItem( HWND hwndTV, LPTVITEM pitem );
- Version 4.71 or later -
...BOOL TreeView_SetItem( HWND hwndTV, LPTVITEMEX pitem );
*/
HB_FUNC( WAPI_TREEVIEW_SETITEM )
{
   wapi_ret_L( TreeView_SetItem( wapi_par_HWND( 1 ), wapi_par_TVITEM( 2 ) ) );
}
/*----------------------------------------------------------------------*/
/*
...int TreeView_SetItemHeight( HWND hwndTV, SHORT cyItem );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_SETITEMHEIGHT )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_NI( TreeView_SetItemHeight( wapi_par_HWND( 1 ), wapi_par_SHORT( 2 ) ) );
#else
   wapi_ret_NI( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_SetItemState( HWND hwndTV, HTREEITEM hItem, UINT state, UINT stateMask );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_SETITEMSTATE )
{
   /* wapi_ret_UINT( TreeView_SetItemState( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_UINT( 3 ), wapi_par_UINT( 4 ) ) ); */
}
#endif
/*----------------------------------------------------------------------*/
/*
...COLORREF TreeView_SetLineColor( HWND hwndTV, COLORREF clrLine );
*/
#if ( _WIN32_IE >= 0x0500 )
HB_FUNC( WAPI_TREEVIEW_SETLINECOLOR )
{
   wapi_ret_COLORREF( TreeView_SetLineColor( wapi_par_HWND( 1 ), wapi_par_COLORREF( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...UINT TreeView_SetScrollTime( HWND hwndTV, UINT uMaxScrollTime );
*/
HB_FUNC( WAPI_TREEVIEW_SETSCROLLTIME )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   wapi_ret_UINT( TreeView_SetScrollTime( wapi_par_HWND( 1 ), wapi_par_UINT( 2 ) ) );
#else
   wapi_ret_UINT( 0 );
#endif
}
/*----------------------------------------------------------------------*/
/*
...COLORREF TreeView_SetTextColor( HWND hwndTV, COLORREF clrText );
*/
#if ( _WIN32_IE >= 0x0400 )
HB_FUNC( WAPI_TREEVIEW_SETTEXTCOLOR )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_COLORREF( TreeView_SetTextColor( wapi_par_HWND( 1 ), wapi_par_COLORREF( 2 ) ) );
#else
   wapi_ret_COLORREF( 0 );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/*
...HWND TreeView_SetToolTips( HWND hwndTV, HWND hwndTooltip );
*/
#if ( _WIN32_IE >= 0x0300 )
HB_FUNC( WAPI_TREEVIEW_SETTOOLTIPS )
{
#if ! defined( HB_OS_WIN_CE )
   wapi_ret_HANDLE( TreeView_SetToolTips( wapi_par_HWND( 1 ), wapi_par_HWND( 2 ) ) );
#else
   wapi_ret_HANDLE( NULL );
#endif
}
#endif
/*----------------------------------------------------------------------*/
/* IE 4.0
...BOOL TreeView_SetUnicodeFormat( HWND hwnd, BOOL fUnicode );
*/
HB_FUNC( WAPI_TREEVIEW_SETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   wapi_ret_L( TreeView_SetUnicodeFormat( wapi_par_HWND( 1 ), wapi_par_BOOL( 2 ) ) );
#else
   wapi_ret_L( FALSE );
#endif
}
/*----------------------------------------------------------------------*/
#if 0
/*
...DWORD TreeView_ShowInfoTip( HWND hwnd, HITEM hitem );
*/
HB_FUNC( WAPI_TREEVIEW_SHOWINFOTIP )
{
   wapi_ret_DWORD( TreeView_ShowInfoTip( wapi_par_HWND( 1 ), wapi_par_HITEM( 2 ) ) );
}
#endif
/*----------------------------------------------------------------------*/
/*
...BOOL TreeView_SortChildren( HWND hwndTV, HTREEITEM hitem, BOOL fRecurse );
*/
HB_FUNC( WAPI_TREEVIEW_SORTCHILDREN )
{
   wapi_ret_L( TreeView_SortChildren( wapi_par_HWND( 1 ), wapi_par_HANDLE( 2 ), wapi_par_BOOL( 3 ) ) );
}
/*----------------------------------------------------------------------*/
#if 0
/*
...BOOL TreeView_SortChildrenCB( HWND hwndTV, LPTVSORTCB psort, BOOL fRecurse );
*/
HB_FUNC( WAPI_TREEVIEW_SORTCHILDRENCB )
{
   wapi_ret_L( TreeView_SortChildrenCB( wapi_par_HWND( 1 ), wapi_par_TVSORTCB( 2 ), wapi_par_BOOL( 3 ) ) );
}
#endif
