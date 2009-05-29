/*
 * $Id$
 */

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400
#undef _WIN32_IE
#define _WIN32_IE      0x0400

#ifndef __WATCOMC__

#include "hbwhat.h"

#include <windows.h>
#include <commctrl.h>
#include <shlobj.h>

#include "hbapiitm.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_CREATE )
{
   HWND hwnd;
   HWND hbutton;
   LRESULT hFont;
   LONG style;
   style = ISNIL(6) ? 0 : (LONG) hb_parnl(6);
   hwnd = (HWND) HB_PARWH (1);
   hFont = SendMessage( hwnd, WM_GETFONT, 0, 0);
   hbutton = CreateWindowEx(0, WC_TABCONTROL, NULL , style, hb_parni(2), hb_parni(3) , hb_parni(4), hb_parni(5) , hwnd,NULL, GetModuleHandle(NULL) , NULL );
   SendMessage(hbutton,(UINT)WM_SETFONT, (WPARAM) hFont, 1 );
   HB_RETWH( hbutton );
}

//---------------------------------------------------------------------------//

// not an API

HB_FUNC( VWN_TABCTRL_ADDITEM )
{
   int iCount = TabCtrl_GetItemCount( (HWND) HB_PARWH(1) );
   TC_ITEM item;

   item.mask = TCIF_TEXT | TCIF_IMAGE;
   item.iImage = ISNIL(3) ? -1 : (LONG) hb_parnl(3);
   item.pszText = (LPSTR) hb_parcx(2);

   hb_retni( TabCtrl_InsertItem( (HWND) HB_PARWH(1), iCount, &item) );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_INSERTITEM )
{
   TC_ITEM item;
   item.mask = TCIF_TEXT | TCIF_IMAGE;
   item.iImage = ISNIL(4) ? -1 : (LONG) hb_parnl(4);
   item.pszText = (LPSTR) hb_parcx(2);
   hb_retni( TabCtrl_InsertItem( (HWND) HB_PARWH(1), (INT) hb_parni(3), &item) );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_SETCURSEL )
{
   hb_retni( TabCtrl_SetCurSel( (HWND) HB_PARWH(1) , hb_parni (2) ) );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_GETCURSEL )
{
   hb_retni ( TabCtrl_GetCurSel( (HWND) HB_PARWH (1) ) );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_GETITEM )
{
   TC_ITEM item;
   hb_retl(TabCtrl_GetItem( (HWND) HB_PARWH (1), (int) hb_parni(2) , &item ) );

   // assign  item to param 3
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_GETITEMCOUNT )
{
   hb_retni( TabCtrl_GetItemCount( (HWND) HB_PARWH(1) ) );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_GETITEMRECT )
{
   RECT rc;
   PHB_ITEM aRect = hb_itemArrayNew( 4 );

   ( void ) TabCtrl_GetItemRect((HWND) HB_PARWH (1), hb_parni(2), &rc);

   hb_arraySetNL( aRect, 1, rc.left );
   hb_arraySetNL( aRect, 2, rc.top );
   hb_arraySetNL( aRect, 3, rc.right );
   hb_arraySetNL( aRect, 4, rc.bottom );

   hb_itemReturnRelease( aRect );
}

//---------------------------------------------------------------------------//

HB_FUNC( VWN_TABCTRL_GETROWCOUNT )
{
   hb_retni( TabCtrl_GetRowCount( (HWND) HB_PARWH(1) ) );
}



//---------------------------------------------------------------------------//
// TabCtrl_GetImageList(hwnd)
// (HIMAGELIST)SNDMSG((hwnd), TCM_GETIMAGELIST, 0, 0L)

HB_FUNC( VWN_TABCTRL_GETIMAGELIST )
{
   HB_RETWH( TabCtrl_GetImageList( (HWND) HB_PARWH(1) ) );
}


//---------------------------------------------------------------------------//
// #define TabCtrl_SetImageList(hwnd, himl)
// (HIMAGELIST)SNDMSG((hwnd), TCM_SETIMAGELIST, 0, (LPARAM)(HIMAGELIST)(himl))

HB_FUNC( VWN_TABCTRL_SETIMAGELIST )
{
   HB_RETWH( TabCtrl_SetImageList( (HWND) HB_PARWH( 1 ),
                    (LPARAM)(HIMAGELIST) HB_PARWH( 2 ) ) );
}


//---------------------------------------------------------------------------//
// TabCtrl_SetItem(hwnd, iItem, pitem)
// (BOOL)SNDMSG((hwnd), TCM_SETITEM, (WPARAM)(int)(iItem), (LPARAM)(TC_ITEM FAR*)(pitem))

HB_FUNC( VWN_TABCTRL_SETITEM )
{
   TC_ITEM item;
   item.mask = TCIF_TEXT | TCIF_IMAGE;
   item.iImage = -1;
   item.pszText = (LPSTR) hb_parcx( 3 );
   hb_retl( TabCtrl_SetItem( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), &item) );
}

//---------------------------------------------------------------------------//
// TabCtrl_DeleteAllItems(hwnd)
// (BOOL)SNDMSG((hwnd), TCM_DELETEALLITEMS, 0, 0L)

HB_FUNC( VWN_TABCTRL_DELETEALLITEMS )
{
   hb_retl(TabCtrl_DeleteAllItems((HWND) HB_PARWH(1)));
}

//---------------------------------------------------------------------------//
// TabCtrl_DeleteItem(hwnd, i)
// (BOOL)SNDMSG((hwnd), TCM_DELETEITEM, (WPARAM)(int)(i), 0L)

HB_FUNC( VWN_TABCTRL_DELETEITEM )
{
   hb_retl(TabCtrl_DeleteItem((HWND) HB_PARWH(1), (WPARAM) hb_parnint(2)));
}

//---------------------------------------------------------------------------//
// TabCtrl_HitTest(hwndTC, pinfo)
// (int)SNDMSG((hwndTC), TCM_HITTEST, 0, (LPARAM)(TC_HITTESTINFO FAR*)(pinfo))

// waiting for structures

HB_FUNC( VWN_TABCTRL_HITTEST )
{
   TCHITTESTINFO tcht ;

   hb_parni( TabCtrl_HitTest( (HWND) HB_PARWH(1), &tcht ) );

   // assign to structure in param 2


}

//---------------------------------------------------------------------------//
// TabCtrl_SetItemExtra(hwndTC, cb)
// (BOOL)SNDMSG((hwndTC), TCM_SETITEMEXTRA, (WPARAM)(cb), 0L)

HB_FUNC( VWN_TABCTRL_SETITEMEXTRA )
{
   hb_retl( TabCtrl_SetItemExtra( (HWND) HB_PARWH(1), (int) hb_parni(2) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_AdjustRect(hwnd, bLarger, prc)
// (int)SNDMSG(hwnd, TCM_ADJUSTRECT, (WPARAM)(BOOL)(bLarger), (LPARAM)(RECT FAR *)prc)

HB_FUNC( VWN_TABCTRL_ADJUSTRECT )
{
  RECT rc;

  if ( ISARRAY(3) )
  {
     rc.left   = hb_parnl(3,1);
     rc.top    = hb_parnl(3,2);
     rc.right  = hb_parnl(3,3);
     rc.bottom = hb_parnl(3,4);

     ( void ) TabCtrl_AdjustRect( (HWND) HB_PARWH(1), (BOOL) hb_parl(2), &rc );

     hb_stornl( rc.left  , 3 ,1 );
     hb_stornl( rc.top   , 3, 2 );
     hb_stornl( rc.right , 3 ,3 );
     hb_stornl( rc.bottom, 3, 4 );
  }

}

//---------------------------------------------------------------------------//
// TabCtrl_SetItemSize(hwnd, x, y)
// (DWORD)SNDMSG((hwnd), TCM_SETITEMSIZE, 0, MAKELPARAM(x,y))

HB_FUNC( VWN_TABCTRL_SETITEMSIZE )
{
   hb_retnl( TabCtrl_SetItemSize( (HWND) HB_PARWH(1), (int) hb_parni(2), (int) hb_parni(3) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_RemoveImage(hwnd, i)
// (void)SNDMSG((hwnd), TCM_REMOVEIMAGE, i, 0L)

HB_FUNC( VWN_TABCTRL_REMOVEIMAGE )
{
  TabCtrl_RemoveImage( (HWND) HB_PARWH(1), (int) hb_parni(2) );
}

//---------------------------------------------------------------------------//
// TabCtrl_SetPadding(hwnd,  cx, cy)
// (void)SNDMSG((hwnd), TCM_SETPADDING, 0, MAKELPARAM(cx, cy))

HB_FUNC( VWN_TABCTRL_SETPADDING )
{
   TabCtrl_SetPadding( (HWND) HB_PARWH(1), (int) hb_parni(2), (int) hb_parni(3) );
}

//---------------------------------------------------------------------------//
// TabCtrl_GetToolTips(hwnd)
// (HWND)SNDMSG((hwnd), TCM_GETTOOLTIPS, 0, 0L)

HB_FUNC( VWN_TABCTRL_GETTOOLTIPS )
{
   HB_RETWH( TabCtrl_GetToolTips( (HWND) HB_PARWH( 1 ) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_SetToolTips(hwnd, hwndTT)
// (void)SNDMSG((hwnd), TCM_SETTOOLTIPS, (WPARAM)(hwndTT), 0L)

HB_FUNC( VWN_TABCTRL_SETTOOLTIPS )
{
   TabCtrl_SetToolTips( (HWND) HB_PARWH(1), (HWND) HB_PARWH(2) );
}

//---------------------------------------------------------------------------//
// TabCtrl_GetCurFocus(hwnd)
// (int)SNDMSG((hwnd), TCM_GETCURFOCUS, 0, 0)

HB_FUNC( VWN_TABCTRL_GETCURFOCUS )
{
   hb_retni( TabCtrl_GetCurFocus( (HWND) HB_PARWH(1) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_SetCurFocus(hwnd, i)
// SNDMSG((hwnd),TCM_SETCURFOCUS, i, 0)

HB_FUNC( VWN_TABCTRL_SETCURFOCUS )
{
  TabCtrl_SetCurFocus( (HWND) HB_PARWH(1), (int) hb_parni(2) );
  hb_ret();
}

//---------------------------------------------------------------------------//
// TabCtrl_SetMinTabWidth(hwnd, x)
// (int)SNDMSG((hwnd), TCM_SETMINTABWIDTH, 0, x)

HB_FUNC( VWN_TABCTRL_SETMINTABWIDTH )
{
   hb_retni( TabCtrl_SetMinTabWidth( (HWND) HB_PARWH(1), (int) hb_parni(2) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_DeselectAll(hwnd, fExcludeFocus)
// (void)SNDMSG((hwnd), TCM_DESELECTALL, fExcludeFocus, 0)

HB_FUNC( VWN_TABCTRL_DESELECTALL )
{
   TabCtrl_DeselectAll( (HWND) HB_PARWH(1), (UINT) hb_parni( 2 ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_HighlightItem(hwnd, i, fHighlight)
// (BOOL)SNDMSG((hwnd), TCM_HIGHLIGHTITEM, (WPARAM)(i), (LPARAM)MAKELONG (fHighlight, 0))

HB_FUNC( VWN_TABCTRL_HIGHLIGHTITEM )
{
   hb_retl( TabCtrl_HighlightItem( (HWND) HB_PARWH(1), (int) hb_parni(2), (WORD) hb_parni(3) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_SetExtendedStyle(hwnd, dw)
// (DWORD)SNDMSG((hwnd), TCM_SETEXTENDEDSTYLE, 0, dw)

HB_FUNC( VWN_TABCTRL_SETEXTENDEDSTYLE )
{
   hb_retnl( TabCtrl_SetExtendedStyle( (HWND) HB_PARWH(1), (DWORD) hb_parnl(2) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_GetExtendedStyle(hwnd)
// (DWORD)SNDMSG((hwnd), TCM_GETEXTENDEDSTYLE, 0, 0)

HB_FUNC( VWN_TABCTRL_GETEXTENDEDSTYLE )
{
   hb_retnl( TabCtrl_GetExtendedStyle( (HWND) HB_PARWH(1) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_SetUnicodeFormat(hwnd, fUnicode)
// (BOOL)SNDMSG((hwnd), TCM_SETUNICODEFORMAT, (WPARAM)(fUnicode), 0)

HB_FUNC( VWN_TABCTRL_SETUNICODEFORMAT )
{
   hb_retl( TabCtrl_SetUnicodeFormat( (HWND) HB_PARWH(1), hb_parl(2) ) );
}

//---------------------------------------------------------------------------//
// TabCtrl_GetUnicodeFormat(hwnd)
// (BOOL)SNDMSG((hwnd), TCM_GETUNICODEFORMAT, 0, 0)

HB_FUNC( VWN_TABCTRL_GETUNICODEFORMAT )
{
   hb_retl( TabCtrl_GetUnicodeFormat( (HWND) HB_PARWH(1) ) );
}

//---------------------------------------------------------------------------//

#endif
