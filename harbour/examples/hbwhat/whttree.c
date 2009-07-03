/*
 * $Id$
 */

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"

//-------------------------------------------------------------------------//

HB_FUNC( VWN_TVINSERTITEM )
{
   TV_INSERTSTRUCT is;

   is.hParent      = ( HTREEITEM ) HB_PARWH( 3 );
   is.hInsertAfter = TVI_LAST;

   #if (_WIN32_IE >= 0x0400) && !defined(_MSC_VER)
      is.DUMMYUNIONNAME.item.pszText = hb_parcx( 2 );
      is.DUMMYUNIONNAME.item.mask    = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE;
      is.DUMMYUNIONNAME.item.iImage  = hb_parnl( 4 );
      is.DUMMYUNIONNAME.item.iSelectedImage = hb_parnl( 4 );
   #else
      is.item.pszText = hb_parcx( 2 );
      is.item.mask    = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE;
      is.item.iImage  = hb_parnl( 4 );
      is.item.iSelectedImage = hb_parnl( 4 );
   #endif

   HB_RETWI( SendMessage( ( HWND ) HB_PARWH( 1 ), TVM_INSERTITEM, 0,
           ( LPARAM )( LPTV_INSERTSTRUCT )( &is ) ) );
}

//-------------------------------------------------------------------------//
HB_FUNC( VWN_TVDELETEITEM )
{
   ( void ) TreeView_DeleteItem( (HWND) HB_PARWH( 1 ), ( HTREEITEM ) HB_PARWH( 2 ) );
}

//-------------------------------------------------------------------------//

HB_FUNC( VWN_TVSETIMAGELIST ) // ( hWnd, hImageList, nType )
{
   HB_RETWH( TreeView_SetImageList( ( HWND ) HB_PARWH( 1 ),
            ( HIMAGELIST ) HB_PARWH( 2 ), hb_parnl( 3 ) ) );
}

//-------------------------------------------------------------------------//

HB_FUNC( VWN_TVGETSELTEXT ) // ( hWnd ) --> cText
{
   HWND hWnd = ( HWND ) HB_PARWH( 1 );
   HTREEITEM hItem = TreeView_GetSelection( hWnd );
   TV_ITEM tvi;
   BYTE buffer[ 100 ];
   if( hItem )
   {
      tvi.mask       = TVIF_TEXT;
      tvi.hItem      = hItem;
      tvi.pszText    = ( char *)buffer;
      tvi.cchTextMax = 100;
      ( void ) TreeView_GetItem( hWnd, &tvi );
      hb_retc( tvi.pszText );
   }
   else
      hb_retc_null();
}

//-------------------------------------------------------------------------//

HB_FUNC( VWN_TVGETSELECTED ) // ( hWnd ) --> hItem
{
   HB_RETWH( TreeView_GetSelection( ( HWND ) HB_PARWH( 1 ) ) );
}

//-------------------------------------------------------------------------//
