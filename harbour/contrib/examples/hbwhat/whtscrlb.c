/*
 * $Id$
 */

// hbwhat
// Scrollbar API functions

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>

#include "hbapi.h"


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI SetScrollPos( IN HWND hWnd, IN int nBar, IN int nPos, IN BOOL bRedraw);


HB_FUNC( VWN_SETSCROLLPOS )
{
   hb_retni( SetScrollPos( (HWND) HB_PARWH( 1 ),
                           hb_parni( 2 )       ,
                           hb_parni( 3 )       ,
                           hb_parl( 4 )
                         ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetScrollPos( IN HWND hWnd, IN int nBar);


HB_FUNC( VWN_GETSCROLLPOS )
{
   hb_retni( GetScrollPos( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetScrollRange( IN HWND hWnd, IN int nBar, IN int nMinPos, IN int nMaxPos, IN BOOL bRedraw);


HB_FUNC( VWN_SETSCROLLRANGE )
{
   hb_retl( SetScrollRange( (HWND) HB_PARWH( 1 ),
                            hb_parni( 2 )       ,
                            hb_parni( 3 )       ,
                            hb_parni( 4 )       ,
                            hb_parl( 5 )
                          ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetScrollRange( IN HWND hWnd, IN int nBar, OUT LPINT lpMinPos, OUT LPINT lpMaxPos);

// syntax
// GetScrollRange(hWnd,nSbar,@nMin,@nMax) -> lSuccess


HB_FUNC( VWN_GETSCROLLRANGE )
{
   LPINT lpMinPos = 0 ;
   LPINT lpMaxPos = 0 ;

   if ( GetScrollRange( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), lpMinPos, lpMaxPos ) )
   {
      if ( ISBYREF(3) && ISBYREF(4) )
      {
         hb_storni(3,*lpMinPos);
         hb_storni(4,*lpMaxPos);
         hb_retl(1);
      }
      else
        hb_retl(0);
   }
   else
     hb_retl(0);

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ShowScrollBar( IN HWND hWnd, IN int wBar, IN BOOL bShow);


HB_FUNC( VWN_SHOWSCROLLBAR )
{
   hb_retl( ShowScrollBar( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), hb_parl( 3 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EnableScrollBar( IN HWND hWnd, IN UINT wSBflags, IN UINT wArrows);


HB_FUNC( VWN_ENABLESCROLLBAR )
{
   hb_retl( EnableScrollBar( (HWND) HB_PARWH( 1 ),
                             (UINT) hb_parni( 2 ),
                             (UINT) hb_parni( 3 )
                           ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI SetScrollInfo( IN HWND, IN int, IN LPCSCROLLINFO, IN BOOL);

// syntax
// SetScrollInfo(hWnd,nSBarFlag,scrollinfo.value)-> lSuccess


HB_FUNC( VWN_SETSCROLLINFO )
{
   SCROLLINFO * scrollInfo =  (SCROLLINFO * ) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING )->item.asString.value;

   hb_retni( SetScrollInfo( (HWND) HB_PARWH( 1 ),
                            hb_parni( 2 )       ,
                            scrollInfo          ,
                            hb_parl( 4 )
                          ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetScrollInfo( IN HWND, IN int, IN OUT LPSCROLLINFO);

// syntax
// GetScrollInfo(hWnd,nSBarFlag) -> SCROLLINFO structure or NIL

HB_FUNC( VWN_GETSCROLLINFO )
{
   SCROLLINFO si ;
   si.cbSize = sizeof(SCROLLINFO);
   si.fMask  = SIF_ALL ;

   if ( GetScrollInfo( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), &si ) )
      hb_retclen( (char *) &si, sizeof( SCROLLINFO ) );

      // problem
      //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &si, sizeof( SCROLLINFO ) );

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetScrollBarInfo( IN HWND hwnd, IN LONG idObject, OUT PSCROLLBARINFO psbi );

// syntax
// GetScrollBarInfo(hWnd,nId) -> SCROLLBARINFO structure or NIL

// to be tested
#if (WINVER >= 0x0500)
HB_FUNC( VWN_GETSCROLLBARINFO )
{
   SCROLLBARINFO sbi     ;

   if ( GetScrollBarInfo( (HWND) HB_PARWH( 1 ), hb_parnl( 2 ), &sbi ) )
       hb_retclen( (char *) &sbi, sizeof( SCROLLBARINFO ) );

     // problem
     // hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &sbi, sizeof( SCROLLBARINFO ) );
}

#endif
