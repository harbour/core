/*
 * $Id$
 */

/*
 *
 * Pritpal Bedi <pritpal@vouchcac.com> 14Feb2009
 * www - http://www.harbour.org
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
#include "hbwapi.h"
#include <commctrl.h>

/*----------------------------------------------------------------------//
                        BEGIN - ImageList_* - API
//----------------------------------------------------------------------*/
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
HB_FUNC( WAPI_IMAGELIST_COPY )
{
   wapi_ret_L( ImageList_Copy( wapi_par_HIMAGELIST( 1 ),
                               wapi_par_INT( 2 ),
                               wapi_par_HIMAGELIST( 3 ),
                               wapi_par_INT( 4 ),
                               wapi_par_UINT( 5 ) ) );
}
/*----------------------------------------------------------------------*/
/*
HIMAGELIST ImageList_Create( int cx, int cy, UINT flags, int cInitial, int cGrow );
*/
HB_FUNC( WAPI_IMAGELIST_CREATE )
{
   #if 0  /* Test */
   HIMAGELIST il;

   il = ImageList_Create( wapi_par_INT( 1 ),
                                      wapi_par_INT( 2 ),
                                      wapi_par_UINT( 3 ),
                                      wapi_par_INT( 4 ),
                                      wapi_par_INT( 5 ) );
   if( il )
   {
      wapi_ret_HANDLE( il );
   }
   #else
   wapi_ret_HANDLE( ImageList_Create( wapi_par_INT( 1 ),
                                      wapi_par_INT( 2 ),
                                      wapi_par_UINT( 3 ),
                                      wapi_par_INT( 4 ),
                                      wapi_par_INT( 5 ) ) );
   #endif
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
HB_FUNC( WAPI_IMAGELIST_DUPLICATE )
{
   wapi_ret_HANDLE( ImageList_Duplicate( wapi_par_HIMAGELIST( 1 ) ) );
}
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
   int cx ;
   int cy ;

   if ( ImageList_GetIconSize( wapi_par_HIMAGELIST( 1 ), &cx, &cy ) )
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
   IMAGEINFO ii ;

   if( ImageList_GetImageInfo( wapi_par_HIMAGELIST( 1 ), wapi_par_INT( 2 ), &ii ) )
   {
      hb_retclen( ( char* ) &ii, sizeof( IMAGEINFO ) );
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
HB_FUNC( WAPI_IMAGELIST_SETIMAGECOUNT )
{
   wapi_ret_L( ImageList_SetImageCount( wapi_par_HIMAGELIST( 1 ),
                                        wapi_par_UINT( 2 ) ) );
}
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
/*----------------------------------------------------------------------//
//                            END - ImageList_* - API
//----------------------------------------------------------------------*/
