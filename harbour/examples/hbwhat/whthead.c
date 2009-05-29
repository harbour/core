/*
 * $Id$
 */

// Header control functions
// hbwhat

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400
#undef _WIN32_IE
#define _WIN32_IE      0x0500

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>

#include "hbapiitm.h"
#include "hbapi.h"

#ifndef HDM_SETBITMAPMARGIN
   #define HDM_SETBITMAPMARGIN          (HDM_FIRST + 20)
#endif
#ifndef HDM_GETBITMAPMARGIN
   #define HDM_GETBITMAPMARGIN          (HDM_FIRST + 21)
#endif
#ifndef HDM_SETFILTERCHANGETIMEOUT
   #define HDM_SETFILTERCHANGETIMEOUT   (HDM_FIRST + 22)
#endif
#ifndef HDM_EDITFILTER
   #define HDM_EDITFILTER               (HDM_FIRST + 23)
#endif
#ifndef HDM_CLEARFILTER
   #define HDM_CLEARFILTER              (HDM_FIRST + 24)
#endif

#ifndef Header_SetHotDivider
   #define Header_SetHotDivider(hwnd, fPos, dw) (int)SNDMSG((hwnd), HDM_SETHOTDIVIDER, (WPARAM)(fPos), (LPARAM)(dw))
#endif
#ifndef Header_SetBitmapMargin
   #define Header_SetBitmapMargin(hwnd, iWidth) (int)SNDMSG((hwnd), HDM_SETBITMAPMARGIN, (WPARAM)(iWidth), 0)
#endif
#ifndef Header_GetBitmapMargin
   #define Header_GetBitmapMargin(hwnd) (int)SNDMSG((hwnd), HDM_GETBITMAPMARGIN, 0, 0)
#endif
#ifndef Header_SetFilterChangeTimeout
   #define Header_SetFilterChangeTimeout(hwnd, i) (int)SNDMSG((hwnd), HDM_SETFILTERCHANGETIMEOUT, 0, (LPARAM)(i))
#endif
#ifndef Header_EditFilter
   #define Header_EditFilter(hwnd, i, fDiscardChanges) (int)SNDMSG((hwnd), HDM_EDITFILTER, (WPARAM)(i), MAKELPARAM(fDiscardChanges, 0))
#endif
#ifndef Header_ClearFilter
   #define Header_ClearFilter(hwnd, i)  (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)(i), 0)
#endif
#ifndef Header_ClearAllFilters
   #define Header_ClearAllFilters(hwnd) (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)-1, 0)
#endif

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );

#ifndef __WATCOMC__

//-----------------------------------------------------------------------------

// SYNTAX:
// Header_Create( nStyle, x, y, nWidth, nHeight, hWndParent, nId )

HB_FUNC( VWN_HEADER_CREATE )
{

   HB_RETWH( CreateWindow(  "SysHeader32"         ,
                                   ""                    ,
                                   (DWORD) hb_parnl( 1 ) ,
                                   hb_parni( 2 )         ,
                                   hb_parni( 3 )         ,
                                   hb_parni( 4 )         ,
                                   hb_parni( 5 )         ,
                                   (HWND) HB_PARWH( 6 )  ,
                                   (HMENU) HB_PARWH( 7 ) ,
                                   GetModuleHandle(NULL) ,
                                   NULL ) );
}


//-----------------------------------------------------------------------------
// Header_GetItemCount(hwndHD) (int)SNDMSG((hwndHD), HDM_GETITEMCOUNT, 0, 0L)

HB_FUNC( VWN_HEADER_GETITEMCOUNT )
{
   hb_retni( Header_GetItemCount( (HWND) HB_PARWH(1) ) );
}


//-----------------------------------------------------------------------------
// Header_InsertItem(hwndHD, i, phdi) (int)SNDMSG((hwndHD), HDM_INSERTITEM, (WPARAM)(int)(i), (LPARAM)(const HD_ITEM FAR*)(phdi))

HB_FUNC( VWN_HEADER_INSERTITEM )
{
   HDITEM *hdi = ( HDITEM *) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING )->item.asString.value ;
   hb_retni( Header_InsertItem( (HWND) HB_PARWH(1), hb_parni(2), hdi ) );
}


//-----------------------------------------------------------------------------
// Header_DeleteItem(hwndHD, i) (BOOL)SNDMSG((hwndHD), HDM_DELETEITEM, (WPARAM)(int)(i), 0L)

HB_FUNC( VWN_HEADER_DELETEITEM  )
{
   hb_retl( Header_DeleteItem( (HWND) HB_PARWH(1), hb_parni(2) ) );
}


//-----------------------------------------------------------------------------
// Header_GetItem(hwndHD, i, phdi) (BOOL)SNDMSG((hwndHD), HDM_GETITEM, (WPARAM)(int)(i), (LPARAM)(HD_ITEM FAR*)(phdi))

// SYNTAX
// structbuffer := Header_GetItem(hWnd,i) -> cBuff or NIL

HB_FUNC( VWN_HEADER_GETITEM )
{
   HDITEM hdi ;
   BOOL lRet = Header_GetItem( (HWND) HB_PARWH(1), hb_parni(2), &hdi );
   if ( lRet )
       hb_retclen( (char*) &hdi, sizeof(HDITEM) );
      //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) hdi, sizeof( HDITEM ) );

}


//-----------------------------------------------------------------------------
// Header_SetItem(hwndHD, i, phdi) (BOOL)SNDMSG((hwndHD), HDM_SETITEM, (WPARAM)(int)(i), (LPARAM)(const HD_ITEM FAR*)(phdi))

HB_FUNC( VWN_HEADER_SETITEM )
{
   HDITEM *hdi = ( HDITEM * ) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING )->item.asString.value ;
   hb_retl( Header_SetItem( (HWND) HB_PARWH(1), hb_parni(2), hdi ) );
}


//-----------------------------------------------------------------------------
// Header_CreateDragImage(hwnd, i) (HIMAGELIST)SNDMSG((hwnd), HDM_CREATEDRAGIMAGE, (WPARAM)(i), 0)

HB_FUNC( VWN_HEADER_CREATEDRAGIMAGE )
{

   HB_RETWH( Header_CreateDragImage( (HWND) HB_PARWH(1), hb_parni(2) ) );

}


//-----------------------------------------------------------------------------
// Header_GetOrderArray(hwnd, iCount, lpi) (BOOL)SNDMSG((hwnd), HDM_GETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))

// SYNTAX:
// aOrder := Header_GetOrderArray( hWnd )

HB_FUNC( VWN_HEADER_GETORDERARRAY )
{

  UINT iCount = Header_GetItemCount((HWND)HB_PARWH(1) );
  PHB_ITEM aInt ;
  INT *lpi = (INT*) hb_xgrab( iCount*sizeof(INT));
  BOOL lRet = Header_GetOrderArray((HWND) HB_PARWH(1), iCount, lpi )  ;
  UINT i;

  if ( lRet )
  {
      aInt  = hb_itemArrayNew(iCount );
      for ( i = 0; i<iCount ; i++)
         hb_arraySetNL( aInt, i+1, lpi[i] );

      hb_itemReturnRelease(aInt);
      hb_xfree( lpi );
  }
}


//-----------------------------------------------------------------------------
// Header_SetOrderArray(hwnd, iCount, lpi) (BOOL)SNDMSG((hwnd), HDM_SETORDERARRAY, (WPARAM)(iCount), (LPARAM)(lpi))
// lparam = int array of size HDM_GETITEMCOUNT
// the array specifies the order that all items should be displayed.
// e.g.  { 2, 0, 1}
// says the index 2 item should be shown in the 0ths position
//      index 0 should be shown in the 1st position
//      index 1 should be shown in the 2nd position

// SYNTAX:
// Header_SetOrderArray(hWnd,aOrder)


HB_FUNC( VWN_HEADER_SETORDERARRAY )
{
   UINT iCount ;
   INT *lpi    ;
   UINT i      ;

   if( hb_parinfo( 2 ) == HB_IT_ARRAY  )
      {

          iCount = hb_parinfa( 2, 0 );
          lpi = (INT*) hb_xgrab( iCount*sizeof(INT) );
          for ( i= 0 ; i<iCount ; i++ )
          {
             lpi[i] = hb_parni(2,i+1);
          }

          hb_retl( Header_SetOrderArray((HWND) HB_PARWH(1), iCount, lpi ) );
      }
   else
      hb_retl( 0 );

}


//-----------------------------------------------------------------------------
// Header_GetItemRect(hwnd, iItem, lprc) (BOOL)SNDMSG((hwnd), HDM_GETITEMRECT, (WPARAM)(iItem), (LPARAM)(lprc))


// SYNTAX:
// aRect:=Header_GetItemRect(hWnd,i)

HB_FUNC( VWN_HEADER_GETITEMRECT )
{
   RECT rc ;
   PHB_ITEM aRc ;

   if ( Header_GetItemRect((HWND) HB_PARWH(1), (WPARAM) hb_parnint(2), &rc ) )
   {
      aRc = Rect2Array( &rc );
      hb_itemReturn( aRc );
      hb_itemRelease( aRc );

   }

}


//-----------------------------------------------------------------------------
// Header_GetImageList(hwnd) (HIMAGELIST)SNDMSG((hwnd), HDM_GETIMAGELIST, 0, 0)


HB_FUNC( VWN_HEADER_GETIMAGELIST )
{

    HB_RETWH(Header_GetImageList((HWND) HB_PARWH(1) ) );

}


//-----------------------------------------------------------------------------
// Header_SetImageList(hwnd, himl) (HIMAGELIST)SNDMSG((hwnd), HDM_SETIMAGELIST, 0, (LPARAM)(himl))

HB_FUNC( VWN_HEADER_SETIMAGELIST )
{

   HB_RETWH( Header_SetImageList((HWND) HB_PARWH(1), (LPARAM) hb_parnint(2) ) );

}


//-----------------------------------------------------------------------------
// Header_OrderToIndex(hwnd, i) (int)SNDMSG((hwnd), HDM_ORDERTOINDEX, (WPARAM)(i), 0)

HB_FUNC( VWN_HEADER_ORDERTOINDEX )
{

   hb_retni( Header_OrderToIndex((HWND) HB_PARWH(1), hb_parni(2) ) );

}


//-----------------------------------------------------------------------------
// Header_SetHotDivider(hwnd, fPos, dw) (int)SNDMSG((hwnd), HDM_SETHOTDIVIDER, (WPARAM)(fPos), (LPARAM)(dw))
// convenience message for external dragdrop
// wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
//              position or the index of which divider to hotlight
// lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)

HB_FUNC( VWN_HEADER_SETHOTDIVIDER )
{

   hb_retni( Header_SetHotDivider((HWND) HB_PARWH(1), hb_parl(2), (LPARAM) hb_parnint(3)));

}


//-----------------------------------------------------------------------------
// Header_SetBitmapMargin(hwnd, iWidth) (int)SNDMSG((hwnd), HDM_SETBITMAPMARGIN, (WPARAM)(iWidth), 0)

HB_FUNC( VWN_HEADER_SETBITMAPMARGIN )
{

   hb_retni( Header_SetBitmapMargin( (HWND) HB_PARWH(1), hb_parni(2) ) );

}


//-----------------------------------------------------------------------------
// Header_GetBitmapMargin(hwnd) (int)SNDMSG((hwnd), HDM_GETBITMAPMARGIN, 0, 0)

HB_FUNC( VWN_HEADER_GETBITMAPMARGIN )
{

   hb_retni( Header_GetBitmapMargin( (HWND) HB_PARWH(1) ) );

}


//-----------------------------------------------------------------------------
// Header_SetUnicodeFormat(hwnd, fUnicode) (BOOL)SNDMSG((hwnd), HDM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)

HB_FUNC( VWN_HEADER_SETUNICODEFORMAT )
{

   hb_retl( Header_SetUnicodeFormat( (HWND) HB_PARWH(1), hb_parl( 2 ) ) );

}


//-----------------------------------------------------------------------------
// Header_GetUnicodeFormat(hwnd) (BOOL)SNDMSG((hwnd), HDM_GETUNICODEFORMAT, 0, 0)

HB_FUNC( VWN_HEADER_GETUNICODEFORMAT )
{

   hb_retl( Header_GetUnicodeFormat((HWND) HB_PARWH(1) ) );

}


//-----------------------------------------------------------------------------
// Header_SetFilterChangeTimeout(hwnd, i) (int)SNDMSG((hwnd), HDM_SETFILTERCHANGETIMEOUT, 0, (LPARAM)(i))

HB_FUNC( VWN_HEADER_SETFILTERCHANGETIMEOUT )
{

   hb_retni( Header_SetFilterChangeTimeout((HWND) HB_PARWH(1), hb_parni( 2 ) ) );

}


//-----------------------------------------------------------------------------
// Header_EditFilter(hwnd, i, fDiscardChanges) (int)SNDMSG((hwnd), HDM_EDITFILTER, (WPARAM)(i), MAKELPARAM(fDiscardChanges, 0))

HB_FUNC( VWN_HEADER_EDITFILTER )
{

   hb_retni( Header_EditFilter( (HWND) HB_PARWH(1), hb_parni( 2 ), hb_parl( 3 ) ) );

}


//-----------------------------------------------------------------------------
// Header_ClearAllFilters(hwnd) (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)-1, 0)

HB_FUNC( VWN_HEADER_CLEARALLFILTERS )
{

   hb_retni( Header_ClearAllFilters( (HWND) HB_PARWH(1) ) );

}


//-----------------------------------------------------------------------------
// Header_ClearFilter(hwnd, i) (int)SNDMSG((hwnd), HDM_CLEARFILTER, (WPARAM)(i), 0)
// Clear filter takes -1 as a column value to indicate that all
// the filter should be cleared.  When this happens you will
// only receive a single filter changed notification.

HB_FUNC( VWN_HEADER_CLEARFILTER )
{

   hb_retni( Header_ClearFilter( (HWND) HB_PARWH(1), hb_parni( 2 ) ) );

}


//-----------------------------------------------------------------------------
// Header_Layout(hwndHD, playout) (BOOL)SNDMSG((hwndHD), HDM_LAYOUT, 0, (LPARAM)(HD_LAYOUT FAR*)(playout))

HB_FUNC( VWN_HEADER_LAYOUT )
{
   HD_LAYOUT *hdLayout = ( HD_LAYOUT *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING )->item.asString.value ;
   hb_retl( Header_Layout( (HWND) HB_PARWH(1), hdLayout ) );
}


//--------- eof.
//

#endif
