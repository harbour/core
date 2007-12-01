/*
 * $Id$
 */


/*
 * Some parts Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * with author's permission granted on 27 MAy 2002
   Last change:  WN   27 May 2002   10:37 am
 */



#define _WIN32_WINNT   0x0400
#define _WIN32_IE      0x0500

#include <windows.h>
#include <shlobj.h>
#include <commctrl.h>

#include <commdlg.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "item.api"

extern PHB_ITEM Rect2Array( RECT *rc  );

//-----------------------------------------------------------------------------

HB_FUNC( INITCOMMONCONTROLS )
{
  InitCommonControls() ;
}

//-----------------------------------------------------------------------------
// BOOL InitCommonControlsEx( LPINITCOMMONCONTROLSEX lpInitCtrls);

// SYNTAX
// InitCommnonControlsEx(nFlags)

HB_FUNC( INITCOMMONCONTROLSEX )
{
  INITCOMMONCONTROLSEX icc ;
  icc.dwSize = sizeof(INITCOMMONCONTROLSEX);
  icc.dwICC = hb_parnl( 1 );

  hb_retl( InitCommonControlsEx( &icc ) );
}

//----------------------------------------------------------------------------//
HB_FUNC( CREATESTATUSWINDOW )
{
  hb_retnl( (ULONG) CreateStatusWindow (
                                         hb_parnl(1),
                                         (LPCSTR) hb_parcx(2) ,
                                         (HWND) hb_parnl(3) ,
                                         (UINT) hb_parni(4)
                                       )
                                   ) ;
}


//----------------------------------------------------------------------------//

HB_FUNC( DRAWSTATUSTEXT )
{
    RECT rc ;

    rc.left   = hb_parnl( 2, 1 ) ;
    rc.top    = hb_parnl( 2, 2 ) ;
    rc.right  = hb_parnl( 2, 3 ) ;
    rc.bottom = hb_parnl( 2, 4 ) ;

    DrawStatusText(
                     (HDC) hb_parnl(1)  ,
                     (LPRECT) &rc       ,
                     (LPCTSTR) hb_parcx(3),
                     (UINT) hb_parni(4)
                  );

}

//----------------------------------------------------------------------------//

HB_FUNC( WRITESTATUSWINDOW )
{
   SendMessage( (HWND) hb_parnl( 1 ), SB_SETTEXT, hb_parni( 2 ), (LPARAM) hb_parcx( 3 ) );
}


HB_FUNC( STATUSBARGETRECT )
{
  RECT rc;
  PHB_ITEM aRect ;
  HWND hWnd = (HWND) hb_parnl(1);
  SendMessage ( hWnd, SB_GETRECT, hb_parnl(2), (LPARAM) &rc);
  aRect = Rect2Array( &rc  );
  _itemReturn( aRect );
  _itemRelease( aRect );
}


HB_FUNC( STATUSBARGETPARTS )
{
  RECT rc;
  PHB_ITEM aParts;
  HWND hWnd = (HWND) hb_parnl(1);
  SendMessage ( hWnd, SB_GETPARTS, hb_parni(2), (LPARAM) &rc);
  aParts = Rect2Array( &rc  );
  _itemReturn( aParts );
  _itemRelease( aParts );
}

//----------------------------------------------------------------------------//

// T.B.D.

/*
HB_FUNC( MENUHELP )
{

     LPINPUT lpwIDs

     MenuHelp(
               (UINT) hb_parni(1)  ,
               (WPARAM) hb_parni(2),
               (LPARAM)hb_parl(3)  ,
               (HMENU) hb_parnl(4) ,
               (HINSTANCE) hb_parnl(5),
               (HWND) hb_parnl(6),
               (LPUINT) lpwIDs
             );

lpwIDs
Address of an array that contains pairs of string resource identifiers
and menu handles. The function searches the array for the handle to the
selected menu and, if found, uses the corresponding resource identifier
to load the appropriate Help string.

}

   Last change:  WN   26 May 2002    1:28 pm
*/

//----------------------------------------------------------------------------//

/*
   CreateProgressBar( hParentWindow, nRange, x ,y, nWidth,nHeight,bBorder )
*/
HB_FUNC( CREATEPROGRESSBAR )
{
   HWND hPBar, hParentWindow = (HWND) hb_parnl(1);
   RECT rcClient;
   LONG ProgressBarStyle;
   BOOL bBorder = ISNIL(7) ? FALSE : hb_parl(7);
   int cyVScroll = ISNIL(6) ? GetSystemMetrics( SM_CYVSCROLL ): hb_parni(6) ;
   LONG nStyle = ISNIL(8) ? 0 : hb_parnl(8) ;

   int x1, y1, nwidth, nheight;
   if( hb_pcount() > 2 )
   {
      x1 = hb_parni( 3 );
      y1 = hb_parni( 4 );
      nwidth = hb_parni( 5 );
      nheight = cyVScroll;
   }
   else
   {
      GetClientRect( hParentWindow, &rcClient );
      x1 = rcClient.left;
      y1 = rcClient.bottom - cyVScroll;
      nwidth = rcClient.right;
      nheight = cyVScroll;
   }

   hPBar = CreateWindowEx( 0, PROGRESS_CLASS, (LPSTR) NULL,
              WS_CHILD | WS_VISIBLE | nStyle,    /* style  */
              x1,                       /* x */
              y1,                       /* y */
              nwidth, nheight,          /* nWidth, nHeight */
              hParentWindow,            /* parent window    */
              (HMENU) NULL,
              GetModuleHandle( NULL ), NULL );

   SendMessage( hPBar, PBM_SETRANGE, 0, MAKELPARAM( 0, hb_parni( 2 ) ) );
   SendMessage(hPBar, PBM_SETSTEP, (WPARAM) 1, 0);


  if( bBorder )
    {
    ProgressBarStyle = GetWindowLong(hPBar, GWL_EXSTYLE);
    ProgressBarStyle = ProgressBarStyle - WS_EX_STATICEDGE;
    SetWindowLong(hPBar, GWL_EXSTYLE, ProgressBarStyle);
    }

   hb_retnl( (LONG) hPBar );
}


//----------------------------------------------------------------------------//

/*
   UpdateProgressBar( hPBar )
*/
HB_FUNC( UPDATEPROGRESSBAR )
{
   SendMessage( (HWND) hb_parnl(1), PBM_STEPIT, 0, 0 );
}

//----------------------------------------------------------------------------//

HB_FUNC( SETPROGRESSBAR )
{
   SendMessage( (HWND) hb_parnl(1), PBM_SETPOS, (WPARAM) hb_parni(2), 0 );
}



/*


#define PBS_SMOOTH                 1
#define PBS_VERTICAL               4


#define PBM_SETRANGE            (WM_USER+1)
#define PBM_SETPOS              (WM_USER+2)
#define PBM_DELTAPOS            (WM_USER+3)
#define PBM_SETSTEP             (WM_USER+4)
#define PBM_STEPIT              (WM_USER+5)
#define PBM_SETRANGE32          (WM_USER+6)  // lParam = high, wParam = low
#define PBM_GETRANGE            (WM_USER+7)  // wParam = return (TRUE ? low : high). lParam = PPBRANGE or NULL
#define PBM_GETPOS              (WM_USER+8)
#define PBM_SETBARCOLOR         (WM_USER+9)             // lParam = bar color
#define PBM_SETBKCOLOR          CCM_SETBKCOLOR  // lParam = bkColor

*/
















//----------------------------------------------------------------------------


HB_FUNC( INITLISTVIEW )
{
   HWND hwnd;
   HWND hbutton;

   INITCOMMONCONTROLSEX  i;

   i.dwSize = sizeof(INITCOMMONCONTROLSEX);
   i.dwICC = ICC_DATE_CLASSES;
   InitCommonControlsEx(&i);

   hwnd = (HWND) hb_parnl (1);

   hbutton = CreateWindowEx(WS_EX_CLIENTEDGE,"SysListView32","",
   LVS_SINGLESEL | LVS_SHOWSELALWAYS | WS_CHILD | WS_TABSTOP | WS_VISIBLE | WS_BORDER | LVS_REPORT,
   hb_parni(3), hb_parni(4) , hb_parni(5), hb_parni(6) ,
   hwnd,(HMENU)hb_parni(2) , GetModuleHandle(NULL) , NULL ) ;

   SendMessage(hbutton,LVM_SETEXTENDEDLISTVIEWSTYLE, 0,LVS_EX_GRIDLINES | LVS_EX_FULLROWSELECT | LVS_EX_HEADERDRAGDROP );

   if ( hb_parni(8) != 0)
   {
      //SendMessage(hbutton,(UINT)WM_SETFONT,(WPARAM) PrepareFont ( hb_parcx(7) , (LPARAM) hb_parni(8)) , 1 ) ;
   }

   hb_retnl ( (LONG) hbutton );
}
//------------------------------------------------------------------------------------------
HB_FUNC( INITLISTVIEWCOLUMNS )
{
   PHB_ITEM wArray;
   PHB_ITEM hArray;
   char *caption;
   HWND hc;
   LV_COLUMN COL;
   int l9;
   int s;
   int vi;

   hc = (HWND) hb_parnl( 1 ) ;

   l9 = hb_parinfa( 2, 0 ) - 1 ;
   hArray = hb_param( 2, HB_IT_ARRAY );
   wArray = hb_param( 3, HB_IT_ARRAY );

   COL.mask=LVCF_FMT | LVCF_WIDTH | LVCF_TEXT |LVCF_SUBITEM;
   COL.fmt=LVCFMT_LEFT;

   for (s = 0 ; s<=l9 ; s=s+1 )
      {
      //...........
      //caption  = hb_itemGetCPtr ( hArray->item.asArray.value->pItems + s );
      //vi = hb_itemGetNI   ( wArray->item.asArray.value->pItems + s );

caption  = hb_itemGetCPtr ( hb_arrayGetItemPtr(hArray,s));
vi = hb_itemGetNI   ( hb_arrayGetItemPtr(wArray, s ));

      COL.cx=vi;
      COL.pszText=caption;
      COL.iSubItem=s;
      ListView_InsertColumn(hc,s,&COL);

      }

}
//------------------------------------------------------------------------------------------
HB_FUNC( ADDLISTVIEWITEMS )
{
   PHB_ITEM hArray;
   char *caption;
   LV_ITEM LI;
   HWND h;
   int l;
   int s;
   int c;

   h = (HWND) hb_parnl( 1 ) ;
   l = hb_parinfa( 2, 0 ) - 1 ;
   hArray = hb_param( 2, HB_IT_ARRAY );
   c = ListView_GetItemCount (h);
//   caption  = hb_itemGetCPtr ( hArray->item.asArray.value->pItems );

   LI.mask=LVIF_TEXT ;
   LI.state=0;
   LI.stateMask=0;
        LI.iImage=0;
        LI.iSubItem=0;
   LI.iItem=c;
   LI.pszText=caption;
   ListView_InsertItem(h,&LI);

   for (s = 1 ; s<=l ; s=s+1 )
   {
//      caption  = hb_itemGetCPtr ( hArray->item.asArray.value->pItems + s );
      ListView_SetItemText(h,c,s,caption);
   }
}
//------------------------------------------------------------------------------------------
HB_FUNC( LISTVIEW_SETCURSEL )
{
   ListView_SetItemState((HWND) hb_parnl (1), (WPARAM) hb_parni(2)-1 ,LVIS_FOCUSED | LVIS_SELECTED , LVIS_FOCUSED | LVIS_SELECTED );
}
//------------------------------------------------------------------------------------------
HB_FUNC( C_SETFOCUS )
{
   hb_retnl( (LONG) SetFocus( (HWND) hb_parnl( 1 ) ) );
}
//------------------------------------------------------------------------------------------
HB_FUNC( LISTVIEWDELETESTRING )
{
   SendMessage( (HWND) hb_parnl( 1 ),LVM_DELETEITEM , (WPARAM) hb_parni(2)-1, 0);
}
//------------------------------------------------------------------------------------------
HB_FUNC( LISTVIEWRESET )
{
   SendMessage( (HWND) hb_parnl( 1 ), LVM_DELETEALLITEMS , 0, 0 );
}
//------------------------------------------------------------------------------------------
HB_FUNC( LISTVIEW_GETFIRSTITEM )
{
   hb_retni ( ListView_GetNextItem( (HWND) hb_parnl( 1 )  , -1 ,LVNI_ALL | LVNI_SELECTED) + 1);
}
//------------------------------------------------------------------------------------------

