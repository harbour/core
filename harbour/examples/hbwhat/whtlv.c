/*
 * $Id$
 */

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapiitm.h"
#include "hbapi.h"
#include "commctrl.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"


HB_FUNC( VWN_LISTVIEW_DELETEALLITEMS )
{
   ( void ) ListView_DeleteAllItems( (HWND) HB_PARWH(1) );
}

HB_FUNC( VWN_LISTVIEW_DELETECOLUMN )
{
   ( void ) ListView_DeleteColumn( (HWND) HB_PARWH(1), (INT) hb_parni(2) );
}

HB_FUNC( VWN_LISTVIEW_ENSUREVISIBLE )
{
   hb_retl(ListView_EnsureVisible( (HWND) HB_PARWH(1), hb_parni(2), hb_parl(3) ));
}

HB_FUNC( VWN_LISTVIEW_INSERTCOLUMN )
{
   LV_COLUMN *lvColumn = ( LV_COLUMN *) hb_parc( 3 );
   ( void ) ListView_InsertColumn( (HWND)HB_PARWH(1), hb_parni(2), lvColumn );
}

HB_FUNC( VWN_LISTVIEW_SETITEMCOUNT )
{
   ListView_SetItemCount( (HWND) HB_PARWH(1), hb_parnl(2) );
}

HB_FUNC( VWN_LISTVIEW_GETNEXTITEM )
{
   hb_retni( ListView_GetNextItem( (HWND) HB_PARWH(1), hb_parni(2), hb_parnl(3) ));
}

HB_FUNC( VWN_LISTVIEWNOTIFY )
{
   static PHB_DYNS pSymTest = 0 ;

   LPARAM lParam = (LPARAM) hb_parnint(2);
   LPNMHDR lpnmh = (LPNMHDR) lParam;
   LPCSTR cRet;
   PHB_ITEM pArray;
   PHB_ITEM DArray;
   LPSTR cAlias;
   switch(lpnmh->code)
   {
   case LVN_GETDISPINFO:
      {
      LV_DISPINFO *lpdi = (LV_DISPINFO *)lParam;
      // TCHAR szString[MAX_PATH];
      if(lpdi->item.iSubItem)
         {
         if(lpdi->item.mask & LVIF_TEXT)
            {
            if ( !pSymTest )
               pSymTest = hb_dynsymFind( "_WINLVGETDBINFO" );
            if ( pSymTest )
               {
               pArray = hb_param( 3, HB_IT_ARRAY );
               DArray = hb_param( 4, HB_IT_ARRAY );
               cAlias = hb_parcx(5);
               hb_vmPush( pSymTest ); //pSymTest->pSymbol );
               hb_vmPushNil();
               hb_vmPushLong( (LONG) lpdi->item.iItem );
               hb_vmPushLong( (LONG) lpdi->item.iSubItem );
               hb_vmPush( pArray );
               hb_vmPush( DArray );
               hb_vmPushString( cAlias,strlen(cAlias) );
               hb_vmPushInteger( hb_parni(6) );
               hb_vmDo( 6 );
               cRet = hb_itemGetCPtr( hb_param( -1, HB_IT_ANY ) );
               /* TOFIX: Unicode conversion, buffer allocation for item.pszText */
               hb_strncpy(lpdi->item.pszText, cRet, hb_itemGetCLen( hb_param( -1, HB_IT_ANY ) ) );
               }
            }
         }
      else
         {
         if(lpdi->item.mask & LVIF_TEXT)
            {
            if ( !pSymTest )
               pSymTest = hb_dynsymFind( "_WINLVGETDBINFO" );
            if ( pSymTest )
               {
               pArray = hb_param( 3, HB_IT_ARRAY );
               DArray = hb_param( 4, HB_IT_ARRAY );
               cAlias = hb_parcx(5);
               hb_vmPush( pSymTest ); //pSymTest->pSymbol );
               hb_vmPushNil();
               hb_vmPushLong( (LONG) lpdi->item.iItem );
               hb_vmPushLong( (LONG) lpdi->item.iSubItem );
               hb_vmPush( pArray );
               hb_vmPush( DArray );
               hb_vmPushString( cAlias,strlen(cAlias) );
               hb_vmPushInteger( hb_parni(6) );
               hb_vmDo( 6 );
               cRet = hb_itemGetCPtr( hb_param( -1, HB_IT_ANY ) );
               /* TOFIX: Unicode conversion, buffer allocation for item.pszText */
               hb_strncpy(lpdi->item.pszText, cRet, hb_itemGetCLen( hb_param( -1, HB_IT_ANY ) ) );
               }
            }
         if(lpdi->item.mask & LVIF_IMAGE)
            {
            lpdi->item.iImage = lpdi->item.iItem;
            }
         }
      }
      hb_retni(0);
   }
hb_retni(0);
}
