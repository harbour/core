/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (commctrl)
 *
 * Pritpal Bedi <pritpal@vouchcac.com> 14Feb2009
 * www - http://harbour-project.org
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbwapi.h"
#include "hbapiitm.h"

#include <commctrl.h>

#if defined( __BORLANDC__ ) && ! defined( HB_ARCH_64BIT )
    #undef MAKELONG
    #define MAKELONG( a, b )  ( ( LONG ) ( ( ( WORD ) ( ( DWORD_PTR ) ( a ) & 0xffff ) ) | \
                                           ( ( ( DWORD ) ( ( WORD ) ( ( DWORD_PTR ) ( b ) & 0xffff ) ) ) << 16 ) ) )
#endif

/*----------------------------------------------------------------------*/
/*                      BEGIN - ImageList_* - API                       */
/*----------------------------------------------------------------------*/
/*
   int ImageList_Add( HIMAGELIST himl, HBITMAP hbmImage, HBITMAP hbmMask );
 */
HB_FUNC( WAPI_IMAGELIST_ADD )
{
   hbwapi_ret_NI( ImageList_Add( hbwapi_par_raw_HIMAGELIST( 1 ),
                                 hbwapi_par_raw_HBITMAP( 2 ),
                                 hbwapi_par_raw_HBITMAP( 3 ) ) );
}

/*
   int ImageList_AddMasked( HIMAGELIST himl, HBITMAP hbmImage, COLORREF crMask );
 */
HB_FUNC( WAPI_IMAGELIST_ADDMASKED )
{
   hbwapi_ret_NI( ImageList_AddMasked( hbwapi_par_raw_HIMAGELIST( 1 ),
                                       hbwapi_par_raw_HBITMAP( 2 ),
                                       hbwapi_par_COLORREF( 3 ) ) );
}

/*
   BOOL ImageList_BeginDrag( HIMAGELIST himlTrack, int iTrack, int dxHotspot, int dyHotspot );
 */
HB_FUNC( WAPI_IMAGELIST_BEGINDRAG )
{
   hbwapi_ret_L( ImageList_BeginDrag( hbwapi_par_raw_HIMAGELIST( 1 ),
                                      hbwapi_par_INT( 2 ),
                                      hbwapi_par_INT( 3 ),
                                      hbwapi_par_INT( 4 ) ) );
}

/*
   HRESULT ImageList_CoCreateInstance( REFCLSID rclsid, const IUnknown *punkOuter, REFIID riid, void **ppv );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_COCREATEINSTANCE )
{
}
#endif

/*
   BOOL ImageList_Copy( HIMAGELIST himlDst, int iDst, HIMAGELIST himlSrc, int iSrc, UINT uFlags );
 */
HB_FUNC( WAPI_IMAGELIST_COPY )
{
#if ( _WIN32_IE >= 0x0300 )
   hbwapi_ret_L( ImageList_Copy( hbwapi_par_raw_HIMAGELIST( 1 ),
                                 hbwapi_par_INT( 2 ),
                                 hbwapi_par_raw_HIMAGELIST( 3 ),
                                 hbwapi_par_INT( 4 ),
                                 hbwapi_par_UINT( 5 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/*
   HIMAGELIST ImageList_Create( int cx, int cy, UINT flags, int cInitial, int cGrow );
 */
HB_FUNC( WAPI_IMAGELIST_CREATE )
{
   hbwapi_ret_raw_HANDLE( ImageList_Create( hbwapi_par_INT( 1 ),
                                            hbwapi_par_INT( 2 ),
                                            hbwapi_par_UINT( 3 ),
                                            hbwapi_par_INT( 4 ),
                                            hbwapi_par_INT( 5 ) ) );
}

/*
   BOOL ImageList_Destroy( HIMAGELIST himl );
 */
HB_FUNC( WAPI_IMAGELIST_DESTROY )
{
   hbwapi_ret_L( ImageList_Destroy( hbwapi_par_raw_HIMAGELIST( 1 ) ) );
}

/*
   BOOL ImageList_DragEnter( HWND hwndLock, int x, int y );
 */
HB_FUNC( WAPI_IMAGELIST_DRAGENTER )
{
   hbwapi_ret_L( ImageList_DragEnter( hbwapi_par_raw_HWND( 1 ),
                                      hbwapi_par_INT( 2 ),
                                      hbwapi_par_INT( 3 ) ) );
}

/*
   BOOL ImageList_DragLeave( HWND hwndLock );
 */
HB_FUNC( WAPI_IMAGELIST_DRAGLEAVE )
{
   hbwapi_ret_L( ImageList_DragLeave( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   BOOL ImageList_DragMove( int x, int y );
 */
HB_FUNC( WAPI_IMAGELIST_DRAGMOVE )
{
   hbwapi_ret_L( ImageList_DragMove( hbwapi_par_INT( 1 ),
                                     hbwapi_par_INT( 2 ) ) );
}

/*
   BOOL ImageList_DragShowNolock( BOOL fShow );
 */
HB_FUNC( WAPI_IMAGELIST_DRAGSHOWNOLOCK )
{
   hbwapi_ret_L( ImageList_DragShowNolock( hb_parl( 1 ) ) );
}

/*
   BOOL ImageList_Draw( HIMAGELIST himl, int i, HDC hdcDst, int x, int y, UINT fStyle );
 */
HB_FUNC( WAPI_IMAGELIST_DRAW )
{
   hbwapi_ret_L( ImageList_Draw( hbwapi_par_raw_HIMAGELIST( 1 ),
                                 hbwapi_par_INT( 1 ),
                                 hbwapi_par_raw_HDC( 3 ),
                                 hbwapi_par_INT( 4 ),
                                 hbwapi_par_INT( 5 ),
                                 hbwapi_par_UINT( 6 ) ) );
}

/*
   BOOL ImageList_DrawEx( HIMAGELIST himl, int i, HDC hdcDst, int x, int y, int dx, int dy,
                                          COLORREF rgbBk, COLORREF rgbFg, UINT fStyle );
 */
HB_FUNC( WAPI_IMAGELIST_DRAWEX )
{
   hbwapi_ret_L( ImageList_DrawEx( hbwapi_par_raw_HIMAGELIST( 1 ),
                                   hbwapi_par_INT( 2 ),
                                   hbwapi_par_raw_HDC( 3 ),
                                   hbwapi_par_INT( 4 ),
                                   hbwapi_par_INT( 5 ),
                                   hbwapi_par_INT( 6 ),
                                   hbwapi_par_INT( 7 ),
                                   hbwapi_par_COLORREF( 8 ),
                                   hbwapi_par_COLORREF( 9 ),
                                   hbwapi_par_UINT( 10 ) ) );
}

/*
   BOOL ImageList_DrawIndirect( IMAGELISTDRAWPARAMS *pimldp );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_DRAWINDIRECT )
{
}
#endif

/*
   HIMAGELIST ImageList_Duplicate( HIMAGELIST himl );
 */
HB_FUNC( WAPI_IMAGELIST_DUPLICATE )
{
#if ( _WIN32_IE >= 0x0400 )
   hbwapi_ret_raw_HANDLE( ImageList_Duplicate( hbwapi_par_raw_HIMAGELIST( 1 ) ) );
#else
   hbwapi_ret_raw_HANDLE( NULL );
#endif
}

/*
   VOID ImageList_EndDrag( VOID );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_ENDDRAG )
{
   ImageList_EndDrag();
}
#endif

/*
   COLORREF ImageList_GetBkColor( HIMAGELIST himl );
 */
HB_FUNC( WAPI_IMAGELIST_GETBKCOLOR )
{
   hbwapi_ret_COLORREF( ImageList_GetBkColor( hbwapi_par_raw_HIMAGELIST( 1 ) ) );
}

/*
   HIMAGELIST ImageList_GetDragImage( POINT *ppt, POINT *pptHotspot );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_GETDRAGIMAGE )
{
}
#endif

/*
   HICON ImageList_GetIcon( HIMAGELIST himl, int i, UINT flags );
 */
HB_FUNC( WAPI_IMAGELIST_GETICON )
{
   hbwapi_ret_raw_HANDLE( ImageList_GetIcon( hbwapi_par_raw_HIMAGELIST( 1 ),
                                             hbwapi_par_INT( 2 ),
                                             hbwapi_par_UINT( 3 ) ) );
}

/*
   BOOL ImageList_GetIconSize( HIMAGELIST himl, int *cx, int *cy );
 */
HB_FUNC( WAPI_IMAGELIST_GETICONSIZE )
{
   int cx;
   int cy;

   if( ImageList_GetIconSize( hbwapi_par_raw_HIMAGELIST( 1 ), &cx, &cy ) )
   {
      hb_storni( cx, 2 );
      hb_storni( cy, 3 );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/*
   int ImageList_GetImageCount( HIMAGELIST himl );
 */
HB_FUNC( WAPI_IMAGELIST_GETIMAGECOUNT )
{
   hbwapi_ret_NI( ImageList_GetImageCount( hbwapi_par_raw_HIMAGELIST( 1 ) ) );
}

/*                          T O R E V I E W
   BOOL ImageList_GetImageInfo( HIMAGELIST himl, int i, IMAGEINFO *pImageInfo );
 */
HB_FUNC( WAPI_IMAGELIST_GETIMAGEINFO )
{
   IMAGEINFO ii;

   if( ImageList_GetImageInfo( hbwapi_par_raw_HIMAGELIST( 1 ), hbwapi_par_INT( 2 ), &ii ) )
      hb_retclen( ( char * ) &ii, sizeof( IMAGEINFO ) );
   else
      hb_retc_null();
}

/*
   HIMAGELIST ImageList_LoadImage( HINSTANCE hi, LPCTSTR lpbmp, int cx, int cGrow,
                                COLORREF crMask, UINT uType, UINT uFlags );
 */
HB_FUNC( WAPI_IMAGELIST_LOADIMAGE )
{
#if ! defined( HB_OS_WIN_CE )
   LPCTSTR image;

   if( HB_ISCHAR( 2 ) )
      image = ( LPCTSTR ) hb_parc( 2 );
   else
      image = ( LPCTSTR ) MAKEINTRESOURCE( hbwapi_par_INT( 2 ) );

   hbwapi_ret_raw_HANDLE( ImageList_LoadImage( hbwapi_par_raw_HINSTANCE( 1 ),
                                               image,
                                               hbwapi_par_INT( 3 ),
                                               hbwapi_par_INT( 4 ),
                                               hbwapi_par_COLORREF( 5 ),
                                               hbwapi_par_UINT( 6 ),
                                               hbwapi_par_UINT( 7 ) ) );
#else
   hbwapi_ret_raw_HANDLE( NULL );
#endif
}

/*
   HIMAGELIST ImageList_Merge( HIMAGELIST himl1, int i1, HIMAGELIST himl2, int i2, int dx, int dy );
 */
HB_FUNC( WAPI_IMAGELIST_MERGE )
{
   hbwapi_ret_raw_HANDLE( ImageList_Merge( hbwapi_par_raw_HIMAGELIST( 1 ),
                                           hbwapi_par_INT( 2 ),
                                           hbwapi_par_raw_HIMAGELIST( 3 ),
                                           hbwapi_par_INT( 4 ),
                                           hbwapi_par_INT( 5 ),
                                           hbwapi_par_INT( 6 ) ) );

}

/*
   HIMAGELIST ImageList_Read( LPSTREAM pstm );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_READ )
{
}
#endif

/*
   HRESULT ImageList_ReadEx( DWORD dwFlags, LPSTREAM pstm, REFIID riid, void **ppv );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_READEX )
{
}
#endif

/*
   BOOL ImageList_Remove( HIMAGELIST himl, int i );
 */
HB_FUNC( WAPI_IMAGELIST_REMOVE )
{
   hbwapi_ret_L( ImageList_Remove( hbwapi_par_raw_HIMAGELIST( 1 ),
                                   hbwapi_par_INT( 2 ) ) );
}

/*
   BOOL ImageList_Replace( HIMAGELIST himl, int i, HBITMAP hbmImage, HBITMAP hbmMask );
 */
HB_FUNC( WAPI_IMAGELIST_REPLACE )
{
   hbwapi_ret_L( ImageList_Replace( hbwapi_par_raw_HIMAGELIST( 1 ),
                                    hbwapi_par_INT( 2 ),
                                    hbwapi_par_raw_HBITMAP( 3 ),
                                    hbwapi_par_raw_HBITMAP( 4 ) ) );
}

/*
   int ImageList_ReplaceIcon( HIMAGELIST himl, int i, HICON hicon );
 */
HB_FUNC( WAPI_IMAGELIST_REPLACEICON )
{
   hbwapi_ret_NI( ImageList_ReplaceIcon( hbwapi_par_raw_HIMAGELIST( 1 ),
                                         hbwapi_par_INT( 2 ),
                                         hbwapi_par_raw_HICON( 3 ) ) );

}

/*
   COLORREF ImageList_SetBkColor( HIMAGELIST himl, COLORREF clrBk );
 */
HB_FUNC( WAPI_IMAGELIST_SETBKCOLOR )
{
   hbwapi_ret_COLORREF( ImageList_SetBkColor( hbwapi_par_raw_HIMAGELIST( 1 ),
                                              hbwapi_par_COLORREF( 2 ) ) );
}

/*
   int ImageList_SetColorTable( HIMAGELIST himl, int start, int len, RGBQUAD *prgb );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_SETCOLORTABLE )
{
   hbwapi_ret_NI( ImageList_SetColorTable( hbwapi_par_raw_HIMAGELIST( 1 ),
                                           hbwapi_par_INT( 2 ),
                                           hbwapi_par_INT( 3 ),
                                           NULL ) );
}
#endif

/*
   BOOL ImageList_SetDragCursorImage( HIMAGELIST himlDrag, int iDrag, int dxHotspot, int dyHotspot );
 */
HB_FUNC( WAPI_IMAGELIST_SETDRAGCURSORIMAGE )
{
   hbwapi_ret_L( ImageList_SetDragCursorImage( hbwapi_par_raw_HIMAGELIST( 1 ),
                                               hbwapi_par_INT( 2 ),
                                               hbwapi_par_INT( 3 ),
                                               hbwapi_par_INT( 4 ) ) );
}

/*
   BOOL ImageList_SetIconSize( HIMAGELIST himl, int cx, int cy );
 */
HB_FUNC( WAPI_IMAGELIST_SETICONSIZE )
{
   hbwapi_ret_L( ImageList_SetIconSize( hbwapi_par_raw_HIMAGELIST( 1 ),
                                        hbwapi_par_INT( 2 ),
                                        hbwapi_par_INT( 3 ) ) );
}

/*
   BOOL ImageList_SetImageCount( HIMAGELIST himl, UINT uNewCount );
 */
HB_FUNC( WAPI_IMAGELIST_SETIMAGECOUNT )
{
#if ( _WIN32_IE >= 0x0400 )
   hbwapi_ret_L( ImageList_SetImageCount( hbwapi_par_raw_HIMAGELIST( 1 ),
                                          hbwapi_par_UINT( 2 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/*
   BOOL ImageList_SetOverlayImage( HIMAGELIST himl, int iImage, int iOverlay );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_SETOVERLAYIMAGE )
{
}
#endif

/*
   BOOL ImageList_Write( HIMAGELIST himl, LPSTREAM pstm );
 */
#if 0
HB_FUNC( WAPI_IMAGELIST_WRITE )
{
}
#endif

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
   item.iImage  = HB_ISNUM( 4 ) ? hbwapi_par_INT( 4 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );

   hbwapi_ret_NI( TabCtrl_InsertItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 3 ), &item ) );

   hb_strfree( hText );
}

HB_FUNC( WAPI_TABCTRL_SETCURSEL )
{
   hbwapi_ret_NI( TabCtrl_SetCurSel( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( WAPI_TABCTRL_GETCURSEL )
{
   hbwapi_ret_NI( TabCtrl_GetCurSel( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WAPI_TABCTRL_GETITEM )
{
   TC_ITEM item;

   hbwapi_ret_L( TabCtrl_GetItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), &item ) );

   /* assign item to param 3 */
}

HB_FUNC( WAPI_TABCTRL_GETITEMCOUNT )
{
   hbwapi_ret_NI( TabCtrl_GetItemCount( hbwapi_par_raw_HWND( 1 ) ) );
}

HB_FUNC( WAPI_TABCTRL_GETITEMRECT )
{
   PHB_ITEM aRect = hb_itemArrayNew( 4 );
   RECT rc;

   ( void ) TabCtrl_GetItemRect( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), &rc );

   hb_arraySetNL( aRect, 1, rc.left );
   hb_arraySetNL( aRect, 2, rc.top );
   hb_arraySetNL( aRect, 3, rc.right );
   hb_arraySetNL( aRect, 4, rc.bottom );

   hb_itemReturnRelease( aRect );
}

HB_FUNC( WAPI_TABCTRL_GETROWCOUNT )
{
   hbwapi_ret_NI( TabCtrl_GetRowCount( hbwapi_par_raw_HWND( 1 ) ) );
}

/* TabCtrl_GetImageList(hwnd) */
/* (HIMAGELIST)SNDMSG((hwnd), TCM_GETIMAGELIST, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_GETIMAGELIST )
{
   hbwapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_GetImageList( hbwapi_par_raw_HWND( 1 ) ) );
}

/* #define TabCtrl_SetImageList(hwnd, himl) */
/* (HIMAGELIST)SNDMSG((hwnd), TCM_SETIMAGELIST, 0, (LPARAM)(HIMAGELIST)(himl)) */

HB_FUNC( WAPI_TABCTRL_SETIMAGELIST )
{
   hbwapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_SetImageList( hbwapi_par_raw_HWND( 1 ),
                                                         hbwapi_par_raw_HIMAGELIST( 2 ) ) );
}

/* Wapi_TabCtrl_SetItem( hWndTab, nInsertPos, cText, iImageListIndex ) */
/* (BOOL)SNDMSG((hwnd), TCM_SETITEM, (WPARAM)(int)(iItem), (LPARAM)(TC_ITEM FAR*)(pitem)) */

HB_FUNC( WAPI_TABCTRL_SETITEM )
{
   TC_ITEM item;

   void * hText;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = HB_ISNUM( 4 ) ? hbwapi_par_INT( 4 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 3, &hText, NULL );

   hbwapi_ret_L( TabCtrl_SetItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), &item ) );

   hb_strfree( hText );
}

/* TabCtrl_DeleteAllItems(hwnd) */
/* (BOOL)SNDMSG((hwnd), TCM_DELETEALLITEMS, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_DELETEALLITEMS )
{
   hbwapi_ret_L( TabCtrl_DeleteAllItems( hbwapi_par_raw_HWND( 1 ) ) );
}

/* TabCtrl_DeleteItem(hwnd, i) */
/* (BOOL)SNDMSG((hwnd), TCM_DELETEITEM, (WPARAM)(int)(i), 0L) */

HB_FUNC( WAPI_TABCTRL_DELETEITEM )
{
   hbwapi_ret_L( TabCtrl_DeleteItem( hbwapi_par_raw_HWND( 1 ), ( WPARAM ) hbwapi_par_INT( 2 ) ) );
}

/* TabCtrl_HitTest(hwndTC, pinfo) */
/* (int)SNDMSG((hwndTC), TCM_HITTEST, 0, (LPARAM)(TC_HITTESTINFO FAR*)(pinfo)) */

/* waiting for structures */

HB_FUNC( WAPI_TABCTRL_HITTEST )
{
   TCHITTESTINFO tcht;

   hb_parni( TabCtrl_HitTest( hbwapi_par_raw_HWND( 1 ), &tcht ) );
}

/* TabCtrl_SetItemExtra(hwndTC, cb) */
/* (BOOL)SNDMSG((hwndTC), TCM_SETITEMEXTRA, (WPARAM)(cb), 0L) */

HB_FUNC( WAPI_TABCTRL_SETITEMEXTRA )
{
   hbwapi_ret_L( TabCtrl_SetItemExtra( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ) );
}

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

      ( void ) TabCtrl_AdjustRect( hbwapi_par_raw_HWND( 1 ), hbwapi_par_BOOL( 2 ), &rc );

      hb_storvnl( rc.left  , 3, 1 );
      hb_storvnl( rc.top   , 3, 2 );
      hb_storvnl( rc.right , 3, 3 );
      hb_storvnl( rc.bottom, 3, 4 );
   }
}

/* TabCtrl_SetItemSize(hwnd, x, y) */
/* (DWORD)SNDMSG((hwnd), TCM_SETITEMSIZE, 0, MAKELPARAM(x,y)) */

HB_FUNC( WAPI_TABCTRL_SETITEMSIZE )
{
   hbwapi_ret_NINT( TabCtrl_SetItemSize( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), hbwapi_par_INT( 3 ) ) );
}

/* TabCtrl_RemoveImage(hwnd, i) */
/* (void)SNDMSG((hwnd), TCM_REMOVEIMAGE, i, 0L) */

HB_FUNC( WAPI_TABCTRL_REMOVEIMAGE )
{
   TabCtrl_RemoveImage( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) );
}

/* TabCtrl_SetPadding(hwnd,  cx, cy) */
/* (void)SNDMSG((hwnd), TCM_SETPADDING, 0, MAKELPARAM(cx, cy)) */

HB_FUNC( WAPI_TABCTRL_SETPADDING )
{
   TabCtrl_SetPadding( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), hbwapi_par_INT( 3 ) );
}

/* TabCtrl_GetToolTips(hwnd) */
/* (HWND)SNDMSG((hwnd), TCM_GETTOOLTIPS, 0, 0L) */

HB_FUNC( WAPI_TABCTRL_GETTOOLTIPS )
{
#if ! defined( HB_OS_WIN_CE )
   hbwapi_ret_NINT( ( HB_PTRDIFF ) TabCtrl_GetToolTips( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_NINT( -1 );
#endif
}

/* TabCtrl_SetToolTips(hwnd, hwndTT) */
/* (void)SNDMSG((hwnd), TCM_SETTOOLTIPS, (WPARAM)(hwndTT), 0L) */

HB_FUNC( WAPI_TABCTRL_SETTOOLTIPS )
{
#if ! defined( HB_OS_WIN_CE )
   TabCtrl_SetToolTips( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HWND( 2 ) );
#endif
}

/* TabCtrl_GetCurFocus(hwnd) */
/* (int)SNDMSG((hwnd), TCM_GETCURFOCUS, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETCURFOCUS )
{
   hbwapi_ret_NI( TabCtrl_GetCurFocus( hbwapi_par_raw_HWND( 1 ) ) );
}

/* TabCtrl_SetCurFocus(hwnd, i) */
/* SNDMSG((hwnd),TCM_SETCURFOCUS, i, 0) */

HB_FUNC( WAPI_TABCTRL_SETCURFOCUS )
{
   TabCtrl_SetCurFocus( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) );
}

/* TabCtrl_SetMinTabWidth(hwnd, x) */
/* (int)SNDMSG((hwnd), TCM_SETMINTABWIDTH, 0, x) */

HB_FUNC( WAPI_TABCTRL_SETMINTABWIDTH )
{
   /* hbwapi_ret_NI( TabCtrl_SetMinTabWidth( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ) ); */
   hbwapi_ret_NI( ( int ) SendMessage( hbwapi_par_raw_HWND( 1 ), TCM_SETMINTABWIDTH, ( WPARAM ) 0, ( LPARAM ) hbwapi_par_INT( 2 ) ) );
}

/* TabCtrl_DeselectAll(hwnd, fExcludeFocus) */
/* (void)SNDMSG((hwnd), TCM_DESELECTALL, fExcludeFocus, 0) */

HB_FUNC( WAPI_TABCTRL_DESELECTALL )
{
   /* TabCtrl_DeselectAll( hbwapi_par_raw_HWND( 1 ), hbwapi_par_UINT( 2 ) ); */
   SendMessage( hbwapi_par_raw_HWND( 1 ), TCM_DESELECTALL, ( WPARAM ) hbwapi_par_UINT( 2 ), ( LPARAM ) 0 );
}

/* TabCtrl_HighlightItem(hwnd, i, fHighlight) */
/* (BOOL)SNDMSG((hwnd), TCM_HIGHLIGHTITEM, (WPARAM)(i), (LPARAM)MAKELONG (fHighlight, 0)) */

HB_FUNC( WAPI_TABCTRL_HIGHLIGHTITEM )
{
#if ( _WIN32_IE >= 0x0400 )
   hbwapi_ret_L( TabCtrl_HighlightItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ), hbwapi_par_WORD( 3 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/* TabCtrl_SetExtendedStyle(hwnd, dw) */
/* (DWORD)SNDMSG((hwnd), TCM_SETEXTENDEDSTYLE, 0, dw) */

HB_FUNC( WAPI_TABCTRL_SETEXTENDEDSTYLE )
{
#if ( _WIN32_IE >= 0x0400 )
   hbwapi_ret_NINT( TabCtrl_SetExtendedStyle( hbwapi_par_raw_HWND( 1 ), hbwapi_par_DWORD( 2 ) ) );
#else
   hbwapi_ret_NINT( 0 );
#endif
}

/* TabCtrl_GetExtendedStyle(hwnd) */
/* (DWORD)SNDMSG((hwnd), TCM_GETEXTENDEDSTYLE, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETEXTENDEDSTYLE )
{
#if ( _WIN32_IE >= 0x0400 )
   hbwapi_ret_NINT( TabCtrl_GetExtendedStyle( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_NINT( 0 );
#endif
}

/* TabCtrl_SetUnicodeFormat(hwnd, fUnicode) */
/* (BOOL)SNDMSG((hwnd), TCM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0) */

HB_FUNC( WAPI_TABCTRL_SETUNICODEFORMAT )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_L( TabCtrl_SetUnicodeFormat( hbwapi_par_raw_HWND( 1 ), hbwapi_par_BOOL( 2 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/* TabCtrl_GetUnicodeFormat(hwnd) */
/* (BOOL)SNDMSG((hwnd), TCM_GETUNICODEFORMAT, 0, 0) */

HB_FUNC( WAPI_TABCTRL_GETUNICODEFORMAT )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_L( TabCtrl_GetUnicodeFormat( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/* not an API */
HB_FUNC( WAPI_TABCTRL_CREATE )
{
   HWND hwnd = hbwapi_par_raw_HWND( 1 );
   HWND hbutton = CreateWindowEx( 0, WC_TABCONTROL, NULL, ( LONG ) hb_parnl( 6 ) /* style */, hb_parni( 2 ), hb_parni( 3 ) , hb_parni( 4 ), hb_parni( 5 ), hwnd, NULL, GetModuleHandle( NULL ), NULL );

   SendMessage( hbutton, ( UINT ) WM_SETFONT, ( WPARAM ) ( HFONT ) SendMessage( hwnd, WM_GETFONT, 0, 0 ), 1 );
   hbwapi_ret_raw_HANDLE( hbutton );
}

/* not an API */
HB_FUNC( WAPI_TABCTRL_ADDITEM )
{
   int     iCount = TabCtrl_GetItemCount( hbwapi_par_raw_HWND( 1 ) );
   TC_ITEM item;

   void * hText;

   item.mask    = TCIF_TEXT | TCIF_IMAGE;
   item.iImage  = HB_ISNUM( 3 ) ? hbwapi_par_INT( 3 ) : -1;
   item.pszText = ( LPTSTR ) HB_PARSTRDEF( 2, &hText, NULL );

   hbwapi_ret_NI( TabCtrl_InsertItem( hbwapi_par_raw_HWND( 1 ), iCount, &item ) );

   hb_strfree( hText );
}

/*----------------------------------------------------------------------*/
/*                          TreeView_* Functions                       */
/*----------------------------------------------------------------------*/

/*
   HIMAGELIST TreeView_CreateDragImage( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_CREATEDRAGIMAGE )
{
   hbwapi_ret_raw_HANDLE( TreeView_CreateDragImage( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...BOOL TreeView_DeleteAllItems( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_DELETEALLITEMS )
{
   hbwapi_ret_L( TreeView_DeleteAllItems( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...HWND TreeView_EditLabel( HWND hWndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_EDITLABEL )
{
   hbwapi_ret_raw_HANDLE( TreeView_EditLabel( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...BOOL TreeView_EndEditLabelNow( HWND hwndTV, BOOL b );
 */
HB_FUNC( WAPI_TREEVIEW_ENDEDITLABELNOW )
{
   hbwapi_ret_L( TreeView_EndEditLabelNow( hbwapi_par_raw_HWND( 1 ), hbwapi_par_BOOL( 2 ) ) );
}

/*
   ...BOOL TreeView_EnsureVisible( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_ENSUREVISIBLE )
{
   hbwapi_ret_L( TreeView_EnsureVisible( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...BOOL TreeView_Expand( HWND hwndTV, HTREEITEM hItem, UINT flag );
 */
HB_FUNC( WAPI_TREEVIEW_EXPAND )
{
   hbwapi_ret_L( TreeView_Expand( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_UINT( 2 ) ) );
}

/*
   ... COLORREF TreeView_GetBkColor( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETBKCOLOR )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   hbwapi_ret_COLORREF( TreeView_GetBkColor( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/* IE 5.0
   ...UINT TreeView_GetCheckState( HWND hwndTV, HTREEITEM hItem );
 */
HB_FUNC( WAPI_TREEVIEW_GETCHECKSTATE )
{
#if ( _WIN32_IE >= 0x0500 )
   hbwapi_ret_UINT( TreeView_GetCheckState( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
#else
   hbwapi_ret_UINT( 0 );
#endif
}

/*
   ...HTREEITEM TreeView_GetChild( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETCHILD )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetChild( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...UINT TreeView_GetCount( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETCOUNT )
{
   hbwapi_ret_UINT( TreeView_GetCount( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...HTREEITEM TreeView_GetDropHilight( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETDROPHILIGHT )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetDropHilight( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...HWND TreeView_GetEditControl( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETEDITCONTROL )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetEditControl( hbwapi_par_raw_HWND( 1 ) ) );
}

#if 0
/* Vista
   ...DWORD TreeView_GetExtendedStyle( HWND hwnd );
 */
HB_FUNC( WAPI_TREEVIEW_GETEXTENDEDSTYLE )
{
   hbwapi_ret_DWORD( TreeView_GetExtendedStyle( hbwapi_par_raw_HWND( 1 ) ) );
}
#endif

/*
   ...HTREEITEM TreeView_GetFirstVisible( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETFIRSTVISIBLE )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetFirstVisible( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...HIMAGELIST TreeView_GetImageList( HWND hwndTV, INT iImage );
 */
HB_FUNC( WAPI_TREEVIEW_GETIMAGELIST )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetImageList( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ) );
}

/*
   ...UINT TreeView_GetIndent( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETINDENT )
{
   hbwapi_ret_UINT( TreeView_GetIndent( hbwapi_par_raw_HWND( 1 ) ) );
}

/* IE 4.0
   ... COLORREF TreeView_GetInsertMarkColor( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETINSERTMARKCOLOR )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_COLORREF( TreeView_GetInsertMarkColor( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...BOOL TreeView_GetISearchString( HWND hwndTV, LPTSTR lpsz );
 */
HB_FUNC( WAPI_TREEVIEW_GETISEARCHSTRING )
{
   /* hbwapi_ret_( TreeView_GetISearchString( hbwapi_par_raw_HWND( 1 ), LPTSTR ) ); */
}

/*
   ...BOOL TreeView_GetItem( HWND hwndTV, LPTVITEM pitem );
   -- Version 4.71 or later --
   ...BOOL TreeView_GetItem( HWND hwndTV, LPTVITEMEX pitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETITEM )
{
   /* hbwapi_ret_( TreeView_GetItem( hbwapi_par_raw_HWND( 1 ), LPTVITEM ) ); */
}

/* IE 4.0
   ...int TreeView_GetItemHeight( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETITEMHEIGHT )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_NI( TreeView_GetItemHeight( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_NI( 0 );
#endif
}

#if 0
/* Vista
   ... BOOL TreeView_GetItemPartRect( HWND hwnd, HTREEITEM hitem, RECT *prc, TVITEMPART *partid );
 */
HB_FUNC( WAPI_TREEVIEW_GETITEMPARTRECT )
{
   RECT       rc;
   TVITEMPART partid;

   hbwapi_ret_L( TreeView_GetItemPartRect( hbwapi_par_raw_HWND( 1 ), , hbwapi_par_raw_HANDLE( 2 ), &rc, &partid ) );
}
#endif

/*
   ...BOOL TreeView_GetItemRect( HWND hwndTV, HTREEITEM hitem, LPRECT prc, BOOL fItemRect );
 */
HB_FUNC( WAPI_TREEVIEW_GETITEMRECT )
{
   LPRECT prc = NULL;

   hbwapi_ret_L( TreeView_GetItemRect( hbwapi_par_raw_HWND( 1 ), ( HTREEITEM ) hbwapi_par_raw_HANDLE( 2 ), prc, hbwapi_par_BOOL( 4 ) ) );
}

/* IE 5.0
   ...UINT TreeView_GetItemState( HWND hwndTV, HTREEITEM hItem, UINT stateMask );
 */
HB_FUNC( WAPI_TREEVIEW_GETITEMSTATE )
{
#if ( _WIN32_IE >= 0x0500 )
   hbwapi_ret_UINT( TreeView_GetItemState( hbwapi_par_raw_HWND( 1 ), ( HTREEITEM ) hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_UINT( 3 ) ) );
#else
   hbwapi_ret_UINT( 0 );
#endif
}

/* IE 4.0
   ...HTREEITEM TreeView_GetLastVisible( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETLASTVISIBLE )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_raw_HANDLE( TreeView_GetLastVisible( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_raw_HANDLE( NULL );
#endif
}

/*
   ...COLOREF TreeView_GetLineColor( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETLINECOLOR )
{
#if ( _WIN32_IE >= 0x0500 )
   hbwapi_ret_COLORREF( TreeView_GetLineColor( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...HTREEITEM TreeView_GetNextItem( HWND hwndTV, HTREEITEM hitem, UINT flag );
 */
HB_FUNC( WAPI_TREEVIEW_GETNEXTITEM )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetNextItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_UINT( 3 ) ) );
}

#if 0
/*
   ...HTREEITEM TreeView_GetNextSelected( HWND hwnd, TREEITEM *hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETNEXTSELECTED )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetNextSelected( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_TREEITEM( 2 ) ) );
}
#endif

/*
   ...HTREEITEM TreeView_GetNextSibling( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETNEXTSIBLING )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetNextSibling( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_GetNextVisible( HWND hwndTV, HTREEITEM hitem );
   );
 */
HB_FUNC( WAPI_TREEVIEW_GETNEXTVISIBLE )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetNextVisible( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_GetParent( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETPARENT )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetParent( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_GetPrevSibling( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETPREVSIBLING )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetPrevSibling( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_GetPrevVisible( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_GETPREVVISIBLE )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetPrevVisible( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_GetRoot( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETROOT )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetRoot( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...UINT TreeView_GetScrollTime( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETSCROLLTIME )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   hbwapi_ret_UINT( TreeView_GetScrollTime( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_UINT( 0 );
#endif
}

/*
   ...HTREEITEM TreeView_GetSelection( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETSELECTION )
{
   hbwapi_ret_raw_HANDLE( TreeView_GetSelection( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...COLORREF TreeView_GetTextColor( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETTEXTCOLOR )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_COLORREF( TreeView_GetTextColor( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...HWND TreeView_GetToolTips( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETTOOLTIPS )
{
#if ( _WIN32_IE >= 0x0300 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_raw_HANDLE( TreeView_GetToolTips( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_raw_HANDLE( NULL );
#endif
}

/*
   ...BOOL TreeView_GetUnicodeFormat( HWND hwnd );
 */
HB_FUNC( WAPI_TREEVIEW_GETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   hbwapi_ret_L( TreeView_GetUnicodeFormat( hbwapi_par_raw_HWND( 1 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/*
   ...UINT TreeView_GetVisibleCount( HWND hwndTV );
 */
HB_FUNC( WAPI_TREEVIEW_GETVISIBLECOUNT )
{
   hbwapi_ret_UINT( TreeView_GetVisibleCount( hbwapi_par_raw_HWND( 1 ) ) );
}

/*
   ...HTREEITEM TreeView_HitTest( HWND hwndTV, LPTVHITTESTINFO lpht );
 */
HB_FUNC( WAPI_TREEVIEW_HITTEST )
{
   hbwapi_ret_raw_HANDLE( TreeView_HitTest( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_TVHITTESTINFO( 2 ) ) );
}

/*
   ...HTREEITEM TreeView_InsertItem( HWND hwndTV, LPTVINSERTSTRUCT lpis );
 */
HB_FUNC( WAPI_TREEVIEW_INSERTITEM )
{
   hbwapi_ret_raw_HANDLE( TreeView_InsertItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_TVINSERTSTRUCT( 2 ) ) );
}

#if 0
/*
   ...HTREEITEM TreeView_MapAccIDToHTREEITEM( HWND hwnd, UINT id );
 */
HB_FUNC( WAPI_TREEVIEW_MAPACCIDTOHTREEITEM )
{
   hbwapi_ret_raw_HANDLE( TreeView_MapAccIDToHTREEITEM( hbwapi_par_raw_HWND( 1 ), hbwapi_par_UINT( 2 ) ) );
}
#endif

#if 0
/*
   ...UINT TreeView_MapHTREEITEMtoAccID( HWND hwnd, HTREEITEM htreeitem );
 */
HB_FUNC( WAPI_TREEVIEW_MAPHTREEITEMTOACCID )
{
   hbwapi_ret_UINT( TreeView_MapHTREEITEMtoAccID( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}
#endif

/*
   ...BOOL TreeView_Select( HWND hwndTV, HTREEITEM hitem, UINT flag );
 */
HB_FUNC( WAPI_TREEVIEW_SELECT )
{
   hbwapi_ret_L( TreeView_Select( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_UINT( 3 ) ) );
}

/*
   ...BOOL TreeView_SelectDropTarget( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_SELECTDROPTARGET )
{
   hbwapi_ret_L( TreeView_SelectDropTarget( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...BOOL TreeView_SelectItem( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_SELECTITEM )
{
   hbwapi_ret_L( TreeView_SelectItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

/*
   ...BOOL TreeView_SelectSetFirstVisible( HWND hwndTV, HTREEITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_SELECTSETFIRSTVISIBLE )
{
   hbwapi_ret_L( TreeView_SelectSetFirstVisible( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ) ) );
}

#if 0
/*
   ...LRESULT TreeView_SetAutoScrollInfo( HWND hwnd, UINT uPixPerSec, UINT uUpdateTime );
 */
HB_FUNC( WAPI_TREEVIEW_SETAUTOSCROLLINFO )
{
   hbwapi_ret_LRESULT( TreeView_SetAutoScrollInfo( hbwapi_par_raw_HWND( 1 ), hbwapi_par_UINT( 2 ), hbwapi_par_UINT( 2 ) ) );
}
#endif

/*
   ...COLORREF TreeView_SetBkColor( HWND hwndTV, COLORREF clrBk );
 */
HB_FUNC( WAPI_TREEVIEW_SETBKCOLOR )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_COLORREF( TreeView_SetBkColor( hbwapi_par_raw_HWND( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...UINT TreeView_SetCheckState( HWND hwndTV, HTREEITEM hItem, BOOL fCheck );
 */
HB_FUNC( WAPI_TREEVIEW_SETCHECKSTATE )
{
#if ( _WIN32_IE >= 0x0500 )
   /* hbwapi_ret_UINT( TreeView_SetCheckState( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_BOOL( 3 ) ) ); */
#else
   hbwapi_ret_UINT( 0 );
#endif
}

#if 0
/*
   ...HRESULT TreeView_SetExtendedStyle( HWND hwnd, DWORD dw, UINT mask );
 */
HB_FUNC( WAPI_TREEVIEW_SETEXTENDEDSTYLE )
{
   hbwapi_ret_HRESULT( TreeView_SetExtendedStyle( hbwapi_par_raw_HWND( 1 ), hbwapi_par_DWORD( 2 ), hbwapi_par_UINT( 2 ) ) );
}
#endif

/*
   ...HIMAGELIST TreeView_SetImageList( HWND hwndTV, HIMAGELIST himl, INT iImage );
 */
HB_FUNC( WAPI_TREEVIEW_SETIMAGELIST )
{
   hbwapi_ret_raw_HANDLE( TreeView_SetImageList( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HIMAGELIST( 2 ), hbwapi_par_INT( 3 ) ) );
}

/*
   ...BOOL TreeView_SetIndent( HWND hwndTV, INT indent );
 */
HB_FUNC( WAPI_TREEVIEW_SETINDENT )
{
   hbwapi_ret_L( TreeView_SetIndent( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ) );
}

/*
   ...BOOL TreeView_SetInsertMark( HWND hwndTV, HTREEITEM htiInsert, BOOL fAfter );

 */
HB_FUNC( WAPI_TREEVIEW_SETINSERTMARK )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_L( TreeView_SetInsertMark( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_BOOL( 3 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

/*
   ...COLORREF TreeView_SetInsertMarkColor( HWND hwndTV, COLORREF clrInsertMark );
 */
HB_FUNC( WAPI_TREEVIEW_SETINSERTMARKCOLOR )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_COLORREF( TreeView_SetInsertMarkColor( hbwapi_par_raw_HWND( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...BOOL TreeView_SetItem( HWND hwndTV, LPTVITEM pitem );
   - Version 4.71 or later -
   ...BOOL TreeView_SetItem( HWND hwndTV, LPTVITEMEX pitem );
 */
HB_FUNC( WAPI_TREEVIEW_SETITEM )
{
   hbwapi_ret_L( TreeView_SetItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_TVITEM( 2 ) ) );
}

/*
   ...int TreeView_SetItemHeight( HWND hwndTV, SHORT cyItem );
 */
HB_FUNC( WAPI_TREEVIEW_SETITEMHEIGHT )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_NI( TreeView_SetItemHeight( hbwapi_par_raw_HWND( 1 ), hbwapi_par_SHORT( 2 ) ) );
#else
   hbwapi_ret_NI( 0 );
#endif
}

/*
   ...UINT TreeView_SetItemState( HWND hwndTV, HTREEITEM hItem, UINT state, UINT stateMask );
 */
HB_FUNC( WAPI_TREEVIEW_SETITEMSTATE )
{
#if ( _WIN32_IE >= 0x0500 )
   /* hbwapi_ret_UINT( TreeView_SetItemState( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_UINT( 3 ), hbwapi_par_UINT( 4 ) ) ); */
#else
   hbwapi_ret_UINT( 0 );
#endif
}

/*
   ...COLORREF TreeView_SetLineColor( HWND hwndTV, COLORREF clrLine );
 */
HB_FUNC( WAPI_TREEVIEW_SETLINECOLOR )
{
#if ( _WIN32_IE >= 0x0500 )
   hbwapi_ret_COLORREF( TreeView_SetLineColor( hbwapi_par_raw_HWND( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...UINT TreeView_SetScrollTime( HWND hwndTV, UINT uMaxScrollTime );
 */
HB_FUNC( WAPI_TREEVIEW_SETSCROLLTIME )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   hbwapi_ret_UINT( TreeView_SetScrollTime( hbwapi_par_raw_HWND( 1 ), hbwapi_par_UINT( 2 ) ) );
#else
   hbwapi_ret_UINT( 0 );
#endif
}

/*
   ...COLORREF TreeView_SetTextColor( HWND hwndTV, COLORREF clrText );
 */
HB_FUNC( WAPI_TREEVIEW_SETTEXTCOLOR )
{
#if ( _WIN32_IE >= 0x0400 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_COLORREF( TreeView_SetTextColor( hbwapi_par_raw_HWND( 1 ), hbwapi_par_COLORREF( 2 ) ) );
#else
   hbwapi_ret_COLORREF( 0 );
#endif
}

/*
   ...HWND TreeView_SetToolTips( HWND hwndTV, HWND hwndTooltip );
 */
HB_FUNC( WAPI_TREEVIEW_SETTOOLTIPS )
{
#if ( _WIN32_IE >= 0x0300 ) && ! defined( HB_OS_WIN_CE )
   hbwapi_ret_raw_HANDLE( TreeView_SetToolTips( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HWND( 2 ) ) );
#else
   hbwapi_ret_raw_HANDLE( NULL );
#endif
}

/* IE 4.0
   ...BOOL TreeView_SetUnicodeFormat( HWND hwnd, BOOL fUnicode );
 */
HB_FUNC( WAPI_TREEVIEW_SETUNICODEFORMAT )
{
#if ! defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ ) && ! defined( __CYGWIN__ )
   hbwapi_ret_L( TreeView_SetUnicodeFormat( hbwapi_par_raw_HWND( 1 ), hbwapi_par_BOOL( 2 ) ) );
#else
   hbwapi_ret_L( FALSE );
#endif
}

#if 0
/*
   ...DWORD TreeView_ShowInfoTip( HWND hwnd, HITEM hitem );
 */
HB_FUNC( WAPI_TREEVIEW_SHOWINFOTIP )
{
   hbwapi_ret_DWORD( TreeView_ShowInfoTip( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HITEM( 2 ) ) );
}
#endif

/*
   ...BOOL TreeView_SortChildren( HWND hwndTV, HTREEITEM hitem, BOOL fRecurse );
 */
HB_FUNC( WAPI_TREEVIEW_SORTCHILDREN )
{
   hbwapi_ret_L( TreeView_SortChildren( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_HANDLE( 2 ), hbwapi_par_BOOL( 3 ) ) );
}

#if 0
/*
   ...BOOL TreeView_SortChildrenCB( HWND hwndTV, LPTVSORTCB psort, BOOL fRecurse );
 */
HB_FUNC( WAPI_TREEVIEW_SORTCHILDRENCB )
{
   hbwapi_ret_L( TreeView_SortChildrenCB( hbwapi_par_raw_HWND( 1 ), hbwapi_par_raw_TVSORTCB( 2 ), hbwapi_par_BOOL( 3 ) ) );
}
#endif
