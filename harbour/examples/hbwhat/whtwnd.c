/*
 * $Id$
 */

#define HB_OS_WIN_USED

#include "hbwhat.h"

#include <windows.h>
#include <winuser.h>
#include "hbapiitm.h"
#include "hbapi.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );
extern BOOL Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM Size2Array( SIZE *siz  );
extern void Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void Rect2ArrayEx( RECT *pt  , PHB_ITEM aRect);
extern void Size2ArrayEx( SIZE *siz  ,  PHB_ITEM aSize);

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ISICONIC )
{
   hb_retl( IsIconic( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ISWINDOWVISIBLE )
{
   hb_retl( IsWindowVisible( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ISZOOMED )
{
   hb_retl( IsZoomed( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IsWindowUnicode( IN HWND hWnd);


HB_FUNC( VWN_ISWINDOWUNICODE )
{
   hb_retl( IsWindowUnicode( (HWND) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------

HB_FUNC( VWN_CLOSEWINDOW )
{
   hb_retl( CloseWindow( (HWND) HB_PARWH(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_FINDWINDOW )
{

   HB_RETWH( FindWindow( (LPCSTR) hb_parcx(1), HB_ISCHAR(2) ? hb_parcx(2):NULL ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI FindWindowExA( IN HWND, IN HWND, IN LPCSTR, IN LPCSTR);


HB_FUNC( VWN_FINDWINDOWEX )
{
   HB_RETWH( FindWindowEx( (HWND) HB_PARWH( 1 ) ,
                                  (HWND) HB_PARWH( 2 ) ,
                                  (LPCSTR) hb_parcx( 3 ),
                                  (LPCSTR) hb_parcx( 4 )
                                ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_ISCHILD )
{
   hb_retl( ( BOOL ) IsChild( (HWND) HB_PARWH(1), (HWND) HB_PARWH(2) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_UPDATEWINDOW )
{
  hb_retl( UpdateWindow( (HWND) HB_PARWH(1)) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETWINDOWLONG )
{
   hb_retnl( GetWindowLong( (HWND) HB_PARWH(1), hb_parni(2) ));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( (HWND) HB_PARWH(1), hb_parni(2), hb_parnl(3) ));
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_ENABLEWINDOW )
{
   EnableWindow( (HWND) HB_PARWH(1), hb_parl(2) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IsWindowEnabled( IN HWND hWnd);


HB_FUNC( VWN_ISWINDOWENABLED )
{
   hb_retl( IsWindowEnabled( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DESTROYWINDOW )
{
   hb_retl( DestroyWindow( (HWND) HB_PARWH( 1 )) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ISWINDOW )
{
    hb_retl( IsWindow( (HWND) HB_PARWH( 1 )) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SHOWWINDOW )
{
   hb_retl( ShowWindow( (HWND) HB_PARWH( 1 ), hb_parni(2) ));
}

//-----------------------------------------------------------------------------
HB_FUNC( VWN_MOVEWINDOW )
{
  hb_retl( MoveWindow(
                       (HWND) HB_PARWH(1),
                       hb_parni(2),
                       hb_parni(3),
                       hb_parni(4),
                       hb_parni(5),
                       (ISNIL(6) ? TRUE : hb_parl(6))
                      ));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DEFWINDOWPROC )
{
  HB_RETWI( DefWindowProc( (HWND) HB_PARWH(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DEFDLGPROC )
{
  HB_RETWI( DefDlgProc( (HWND) HB_PARWH(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DEFMDICHILDPROC )
{
  HB_RETWI( DefMDIChildProc( (HWND) HB_PARWH(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DEFFRAMEPROC )
{
  HB_RETWI( DefFrameProc( (HWND) HB_PARWH(1), (HWND) HB_PARWH(2), hb_parnl(3), hb_parnl(4), hb_parnl(5)));
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_CALLWINDOWPROC )
{
  HB_RETWI( CallWindowProc( (WNDPROC) HB_PARWH(1), (HWND) HB_PARWH(2), hb_parni(3), hb_parnl(4), hb_parnl(5)));
}

//-----------------------------------------------------------------------------

//WINUSERAPI BOOL WINAPI InvalidateRect(    IN HWND hWnd,    IN CONST RECT *lpRect,    IN BOOL bErase);
//SYNTAX INVALIDATERECT ( hWnd,aArray ) -> lSuccess

HB_FUNC( VWN_INVALIDATERECT )
{
   RECT rc;
   BOOL bRectOk ;

   bRectOk = ( HB_ISARRAY( 2 )  &&   Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) );

   hb_retl( InvalidateRect(
                           HB_ISNIL(1) ? NULL : (HWND) HB_PARWH( 1 )    ,  // handle of window with changed update region
                           bRectOk ? &rc : NULL ,  // address of rectangle coordinates
                           HB_ISLOG(3) ? hb_parl( 3 ) : TRUE         // erase-background flag
                          ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI RedrawWindow( IN HWND hWnd, IN CONST RECT *lprcUpdate, IN HRGN hrgnUpdate, IN UINT flags);

HB_FUNC( VWN_REDRAWWINDOW )
{
   RECT rc ;
   BOOL bRectOk ;

   bRectOk = ( HB_ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) );

   hb_retl( RedrawWindow(
                          (HWND) HB_PARWH( 1 )                     ,   // handle of window
                          bRectOk ? &rc : NULL                     ,   // address of structure with update rectangle
                          HB_ISNIL( 3 ) ? NULL : (HRGN) HB_PARWH( 3 ) ,   // handle of update region
                          hb_parni( 4 )                                // array of redraw flags
                         ) );

}


//-----------------------------------------------------------------------------
//WINUSERAPI BOOL WINAPI GetClientRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETCLIENTRECT( hWnd ) -> aRect

HB_FUNC( VWN_GETCLIENTRECT )
{
   RECT rc;

   PHB_ITEM aMetr ;
   GetClientRect( (HWND) HB_PARWH( 1 ), &rc );

   aMetr = Rect2Array( &rc  );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}

//-----------------------------------------------------------------------------

//WINUSERAPI BOOL WINAPI GetWindowRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETWINDOWRECT( hWnd ) -> aRect

HB_FUNC( VWN_GETWINDOWRECT )
{
   RECT rc;
   PHB_ITEM aMetr ;

   GetWindowRect( (HWND) HB_PARWH( 1 ),   &rc );
   aMetr = Rect2Array( &rc  );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ShowOwnedPopups( IN HWND hWnd, IN BOOL fShow);


HB_FUNC( VWN_SHOWOWNEDPOPUPS )
{
   hb_retl( ShowOwnedPopups( (HWND) HB_PARWH( 1 ), hb_parl( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI OpenIcon( IN HWND hWnd);


HB_FUNC( VWN_OPENICON )
{
   hb_retl( OpenIcon( (HWND) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HDWP WINAPI BeginDeferWindowPos( IN int nNumWindows);


HB_FUNC( VWN_BEGINDEFERWINDOWPOS )
{
   HB_RETWH( BeginDeferWindowPos( hb_parni( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HDWP WINAPI DeferWindowPos( IN HDWP hWinPosInfo, IN HWND hWnd, IN HWND hWndInsertAfter, IN int x, IN int y, IN int cx, IN int cy, IN UINT uFlags);


HB_FUNC( VWN_DEFERWINDOWPOS )
{
   HB_RETWH( DeferWindowPos( (HDWP) HB_PARWH( 1 ),
                                    (HWND) HB_PARWH( 2 ),
                                    (HWND) HB_PARWH( 3 ),
                                    hb_parni( 4 )       ,
                                    hb_parni( 5 )       ,
                                    hb_parni( 6 )       ,
                                    hb_parni( 7 )       ,
                                    (UINT) hb_parni( 8 )
                                  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EndDeferWindowPos( IN HDWP hWinPosInfo);


HB_FUNC( VWN_ENDDEFERWINDOWPOS )
{
   hb_retl( EndDeferWindowPos( (HDWP) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowPos( IN HWND hWnd, IN HWND hWndInsertAfter, IN int X, IN int Y, IN int cx, IN int cy, IN UINT uFlags);


HB_FUNC( VWN_SETWINDOWPOS )
{
   hb_retl( SetWindowPos( (HWND) HB_PARWH( 1 ),
                          (HWND) HB_PARWH( 2 ),
                          hb_parni( 3 )       ,
                          hb_parni( 4 )       ,
                          hb_parni( 5 )       ,
                          hb_parni( 6 )       ,
                          (UINT) hb_parni( 7 )
                        ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI SetFocus( IN HWND hWnd);


HB_FUNC( VWN_SETFOCUS )
{
   HB_RETWH( SetFocus( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetActiveWindow( VOID);


HB_FUNC( VWN_GETACTIVEWINDOW )
{
   HB_RETWH( GetActiveWindow(  ) );
}

//-----------------------------------------------------------------------------
// ok
// WINUSERAPI HWND WINAPI SetActiveWindow( IN HWND hWnd);


HB_FUNC( VWN_SETACTIVEWINDOW )
{
   HB_RETWH( SetActiveWindow( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// ok
// WINUSERAPI HWND WINAPI GetForegroundWindow( VOID);


HB_FUNC( VWN_GETFOREGROUNDWINDOW )
{
   HB_RETWH( GetForegroundWindow(  ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetFocus( VOID);


HB_FUNC( VWN_GETFOCUS )
{
   HB_RETWH( GetFocus(  ) );
}


//-----------------------------------------------------------------------------
// ok
// WINUSERAPI BOOL WINAPI SetForegroundWindow( IN HWND hWnd);

HB_FUNC( VWN_SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( (HWND) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AnyPopup( VOID);


HB_FUNC( VWN_ANYPOPUP )
{
   hb_retl( AnyPopup(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI BringWindowToTop( IN HWND hWnd);


HB_FUNC( VWN_BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( (HWND) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetClassNameA( IN HWND hWnd, OUT LPSTR lpClassName, IN int nMaxCount);


HB_FUNC( VWN_GETCLASSNAME )
{
   char *cText= (char*) hb_xgrab( MAX_PATH+1 );

   GetClassName( (HWND) HB_PARWH( 1 ),
                 (LPSTR) cText ,
                 MAX_PATH
                );

   hb_retc( cText);
   hb_xfree( cText );
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetTopWindow( IN HWND hWnd);


HB_FUNC( VWN_GETTOPWINDOW )
{
   HB_RETWH( GetTopWindow( (HWND) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScrollWindow( IN HWND hWnd, IN int XAmount, IN int YAmount, IN CONST RECT *lpRect, IN CONST RECT *lpClipRect);
//SYNTAX SCROLLWINDOW(hWnd,nXAmount,nYAmount,aRect1,aRect2) -> lSuccess

HB_FUNC( VWN_SCROLLWINDOW )
{
   RECT lpRect     ;
   RECT lpClipRect ;
   Array2Rect( hb_param( 4 , HB_IT_ARRAY ) , &lpRect );
   Array2Rect( hb_param( 5 , HB_IT_ARRAY ) , &lpClipRect );

   hb_retl( ScrollWindow( (HWND) HB_PARWH( 1 ),
                          hb_parni( 2 )       ,
                          hb_parni( 3 )       ,
                          &lpRect             ,
                          &lpClipRect
                        ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowTextA( IN HWND hWnd, IN LPCSTR lpString);


HB_FUNC( VWN_SETWINDOWTEXT )
{
   hb_retl( SetWindowText( (HWND) HB_PARWH( 1 ), (LPSTR) hb_parcx( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowTextA( IN HWND hWnd, OUT LPSTR lpString, IN int nMaxCount);

// modified API - returns the string !

HB_FUNC( VWN_GETWINDOWTEXT )
{
   int iLen = GetWindowTextLength( (HWND) HB_PARWH( 1 ) )  ;
   char *cText = (char*) hb_xgrab( iLen+1 );
   int iRet = GetWindowText( (HWND) HB_PARWH( 1 ) ,
                            (LPSTR) cText       ,
                             iLen+1
                           );

   hb_retclen( cText, iRet );
   hb_xfree( cText );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowTextLengthA( IN HWND hWnd);


HB_FUNC( VWN_GETWINDOWTEXTLENGTH )
{
   hb_retni( GetWindowTextLength( (HWND) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowContextHelpId( IN HWND, IN DWORD);


HB_FUNC( VWN_SETWINDOWCONTEXTHELPID )
{
   hb_retl( SetWindowContextHelpId( (HWND) HB_PARWH( 1 ), (DWORD) hb_parnl( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetWindowContextHelpId( IN HWND);


HB_FUNC( VWN_GETWINDOWCONTEXTHELPID )
{
   hb_retnl( (LONG) GetWindowContextHelpId( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetMenuContextHelpId( IN HMENU, IN DWORD);


HB_FUNC( VWN_SETMENUCONTEXTHELPID )
{
   hb_retl( SetMenuContextHelpId( (HMENU) HB_PARWH( 1 ), (DWORD) hb_parnl( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetMenuContextHelpId( IN HMENU);


HB_FUNC( VWN_GETMENUCONTEXTHELPID )
{
   hb_retnl( (LONG) GetMenuContextHelpId( (HMENU) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetWindow( IN HWND, IN UINT );


HB_FUNC( VWN_GETWINDOW )
{
   HB_RETWH( GetWindow( (HWND)HB_PARWH(1), (UINT) hb_parni( 2 ) ) );
}


 //-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ClientToScreen( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX CLIENTTOSCREEN( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20,60,80}
if CLIENTTOSCREEN(nWnd , @aArray)
endif
*/

HB_FUNC( VWN_CLIENTTOSCREEN )
{
   POINT Point ;
   PHB_ITEM pArray;
   pArray=  hb_param( 2 , HB_IT_ARRAY );
   if (Array2Point( pArray ,&Point  ) )
   {
      if (ClientToScreen( (HWND) HB_PARWH( 1 ), &Point ))
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
      else
         hb_retl( FALSE );

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScreenToClient( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX SCREENTOCLIENT( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20}
if SCREENTOCLIENT(nWnd , @aArray)
endif
*/

HB_FUNC( VWN_SCREENTOCLIENT )
{
   POINT Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if (Array2Point(pArray, &Point ) )
   {
      if( ScreenToClient( (HWND) HB_PARWH( 1 ), &Point ) >0)
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
      else
         hb_retl( FALSE );


}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI MapWindowPoints( IN HWND hWndFrom, IN HWND hWndTo, IN OUT LPPOINT lpPoints, IN UINT cPoints);
//SYNTAX MAPWINDOWPOINTS(nWndSource,nWndDest,@aPoint,nPoint) -> nP


HB_FUNC( VWN_MAPWINDOWPOINTS )
{
   POINT lpPoints ;
   PHB_ITEM pArray =hb_param( 3 , HB_IT_ARRAY );
   Array2Point( pArray ,&lpPoints );

   hb_retni( MapWindowPoints( (HWND) HB_PARWH( 1 ),
                              (HWND) HB_PARWH( 2 ),
                               &lpPoints            ,
                              (UINT) hb_parni( 4 )
                            ) );
          Point2ArrayEx( &lpPoints   , pArray );

}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI WindowFromPoint( IN POINT Point);
//SYNTAX WINDOWFROMPOINT(aPoint) -> nPoint


HB_FUNC( VWN_WINDOWFROMPOINT )
{
   POINT Point ;
   Array2Point( hb_param( 1 , HB_IT_ARRAY ), &Point );

   HB_RETWH( WindowFromPoint( Point ) );
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI ChildWindowFromPoint( IN HWND hWndParent, IN POINT Point);
//SYNTAX CHILDWINDOWFROMPOINT(nWnd,aPoint) -> nPoint


HB_FUNC( VWN_CHILDWINDOWFROMPOINT )
{
   POINT Point      ;

   Array2Point( hb_param( 2 , HB_IT_ARRAY ) ,&Point);

   HB_RETWH( ChildWindowFromPoint( (HWND) HB_PARWH( 1 ), Point ) );
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI ChildWindowFromPointEx( IN HWND, IN POINT, IN UINT);
//SYNTAX CHILDWINDOWFROMPOINTEX(nWnd,aPoint,nStyle) -> nPoint

HB_FUNC( VWN_CHILDWINDOWFROMPOINTEX )
{
   POINT PoInt ;

   Array2Point( hb_param( 2 , HB_IT_ARRAY ) ,&PoInt);

   HB_RETWH( ChildWindowFromPointEx( (HWND) HB_PARWH( 1 ),
                                            PoInt               ,
                                            (UINT) hb_parni( 3 )
                                          ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI GetWindowWord( IN HWND hWnd, IN int nIndex);


HB_FUNC( VWN_GETWINDOWWORD )
{
   hb_retni( GetWindowWord( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI SetWindowWord( IN HWND hWnd, IN int nIndex, IN WORD wNewWord);

HB_FUNC( VWN_SETWINDOWWORD )
{

   hb_retni( SetWindowWord( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), (WORD) hb_parni(3) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetDesktopWindow( VOID);

HB_FUNC( VWN_GETDESKTOPWINDOW )
{
   HB_RETWH( GetDesktopWindow(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetParent( IN HWND hWnd);

HB_FUNC( VWN_GETPARENT )
{
   HB_RETWH( GetParent( (HWND) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI SetParent( IN HWND hWndChild, IN HWND hWndNewParent);

HB_FUNC( VWN_SETPARENT )
{
   HB_RETWH( SetParent( (HWND) HB_PARWH( 1 ), (HWND) HB_PARWH( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI GetClassWord( IN HWND hWnd, IN int nIndex);


HB_FUNC( VWN_GETCLASSWORD )
{
   hb_retni( GetClassWord( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI SetClassWord( IN HWND hWnd, IN int nIndex, IN WORD wNewWord);

HB_FUNC( VWN_SETCLASSWORD )
{

   hb_retni( SetClassWord( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), (WORD) hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetClassLongA( IN HWND hWnd, IN int nIndex);

HB_FUNC( VWN_GETCLASSLONG )
{
   hb_retnl( (LONG) GetClassLong( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI SetClassLongA( IN HWND hWnd, IN int nIndex, IN LONG dwNewLong);

HB_FUNC( VWN_SETCLASSLONG )
{
   hb_retnl( (LONG) SetClassLong( (HWND) HB_PARWH( 1 ),
                                  hb_parni( 2 )       ,
                                  hb_parnl( 3 )
                                ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetAncestor( IN HWND hwnd, IN UINT gaFlags );
#if(WINVER >= 0x0500)

HB_FUNC( VWN_GETANCESTOR )
{
   HB_RETWH( GetAncestor( (HWND) HB_PARWH( 1 ), (UINT) hb_parni( 2 ) ) );
}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ShowWindowAsync( IN HWND hWnd, IN int nCmdShow);


HB_FUNC( VWN_SHOWWINDOWASYNC )
{
   hb_retl( ShowWindowAsync( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI FlashWindow( IN HWND hWnd, IN BOOL bInvert);
/*

FUNCTION Flash(hWnd,nTimes)

   LOCAL n
   DEFAULT nTimes TO 10
   For n := 1 To nTimes
       FlashWindow(hWnd,1)
       Sleep(500)
   Next
   FlashWindow(hWnd,0)

RETURN(nil)
*/


HB_FUNC( VWN_FLASHWINDOW )
{
   hb_retl( FlashWindow( (HWND) HB_PARWH( 1 ), hb_parl( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AnimateWindow( IN HWND hWnd, IN DWORD dwTime, IN DWORD dwFlags);
#if(WINVER >= 0x0500)
HB_FUNC( VWN_ANIMATEWINDOW )
{
   hb_retl( AnimateWindow( (HWND) HB_PARWH( 1 ) ,
                           (DWORD) hb_parnl( 2 ),
                           (DWORD) hb_parnl( 3 )
                         ) );
}
#endif
//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetWindowPlacement( IN HWND hWnd, OUT WINDOWPLACEMENT *lpwndpl);
#if(WINVER >= 0x0500)
HB_FUNC( VWN_GETWINDOWPLACEMENT )
{
   WINDOWPLACEMENT wndpl ;
   wndpl.length=sizeof(WINDOWPLACEMENT);
   if ( GetWindowPlacement( (HWND) HB_PARWH( 1 ), &wndpl ) )
      hb_retclen( ( char *) &wndpl, sizeof(WINDOWPLACEMENT) );

}

#endif
//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowPlacement( IN HWND hWnd, IN CONST WINDOWPLACEMENT *lpwndpl);
#if(WINVER >= 0x0500)

HB_FUNC( VWN_SETWINDOWPLACEMENT )
{
   WINDOWPLACEMENT * lpwndpl = (WINDOWPLACEMENT *) hb_parc( 2 ); //hb_param( 2,HB_IT_STRING )->item.asString.value;


   hb_retl( SetWindowPlacement( (HWND) HB_PARWH( 1 ), lpwndpl ) );
}

#endif
//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI SetWindowRgn( IN HWND hWnd, IN HRGN hRgn, IN BOOL bRedraw);

HB_FUNC( VWN_SETWINDOWRGN )
{
   hb_retni( SetWindowRgn( (HWND) HB_PARWH( 1 ),
                           (HRGN) HB_PARWH( 2 ),
                           hb_parl( 3 )
                         ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowRgn( IN HWND hWnd, IN HRGN hRgn);

HB_FUNC( VWN_GETWINDOWRGN )
{
   hb_retni( GetWindowRgn( (HWND) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetPropA( IN HWND hWnd, IN LPCSTR lpString, IN HANDLE hData);

HB_FUNC( VWN_SETPROP )
{
   hb_retl( SetProp( (HWND) HB_PARWH( 1 )  ,
                     (LPCSTR) hb_parcx( 2 ) ,
                     (HANDLE) HB_PARWH( 3 )
                   ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI GetPropA( IN HWND hWnd, IN LPCSTR lpString);

HB_FUNC( VWN_GETPROP )
{
   HB_RETWH( GetProp( (HWND) HB_PARWH( 1 ), (LPCSTR) hb_parcx( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI RemovePropA( IN HWND hWnd, IN LPCSTR lpString);

HB_FUNC( VWN_REMOVEPROP )
{
   HB_RETWH( RemoveProp( (HWND) HB_PARWH( 1 ), (LPCSTR) hb_parcx( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI EnumPropsExA( IN HWND hWnd, IN PROPENUMPROCEXA lpEnumFunc, IN LPARAM lParam);


//T.B.D.

/*

HB_FUNC( VWN_ENUMPROPSEX )
{
   PROPENUMPROCEXA lpEnumFunc ;

   // Your code goes here

   hb_retni( EnumPropsEx( (HWND) HB_PARWH( 1 )  ,
                          lpEnumFunc            ,
                          (LPARAM) hb_parnint( 3 )
                        ) );
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI EnumPropsA( IN HWND hWnd, IN PROPENUMPROCA lpEnumFunc);

/*

HB_FUNC( VWN_ENUMPROPS )
{
   PROPENUMPROCA lpEnumFunc ;

   // Your code goes here

   hb_retni( EnumProps( (HWND) HB_PARWH( 1 ), lpEnumFunc ) );
}

*/



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AdjustWindowRect( IN OUT LPRECT lpRect, IN DWORD dwStyle, IN BOOL bMenu);
//SYNTAX ADJUSTWINDOWRECT(@aArray,nStyle,lMenu) -> lSuccess
/*Example
aRect:={12,30,40,90}
if ADJUSTWINDOWRECT(@arect,2,.f.)
endif
*/

HB_FUNC( VWN_ADJUSTWINDOWRECT )
{
   RECT lpRect  ;

   PHB_ITEM pArray=hb_param(1,HB_IT_ARRAY);
   //PHB_ITEM pItem =hb_stackItemFromBase( 1 );

   if(Array2Rect(pArray,&lpRect))
   {
      if( AdjustWindowRect( &lpRect, (DWORD) hb_parnl( 2 ), hb_parl( 3 ) ) >0)
      {
         Rect2ArrayEx(&lpRect,pArray);
         hb_retl(TRUE);
      }
      else
         hb_retl(FALSE);
   }
      else
         hb_retl(FALSE);

}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AdjustWindowRectEx( IN OUT LPRECT lpRect, IN DWORD dwStyle, IN BOOL bMenu, IN DWORD dwExStyle);
//SYNTAX ADJUSTWINDOWRECT(@aArray,nStyle,lMenu,nStyleex) -> lSuccess
/*Example
aRect:={12,30,40,90}
if ADJUSTWINDOWRECTEX(@arect,2,.f.,3)
endif
*/



HB_FUNC( VWN_ADJUSTWINDOWRECTEX )
{
   RECT lpRect    ;
   BOOL bAjust;
   PHB_ITEM pArray=hb_param(1,HB_IT_ARRAY);
   //PHB_ITEM pItem =hb_stackItemFromBase( 1 );

   Array2Rect(pArray,&lpRect);

   bAjust = AdjustWindowRectEx( &lpRect               ,
                                (DWORD) hb_parnl( 2 ),
                                hb_parl( 3 )         ,
                                (DWORD) hb_parnl( 4 )
                              )  ;
   if (bAjust)
            Rect2ArrayEx(&lpRect,pArray );

   hb_retl(bAjust);

}


//-----------------------------------------------------------------------------
// WINUSERAPI LONG_PTR WINAPI GetWindowLongPtrA( HWND hWnd, int nIndex);


HB_FUNC( VWN_GETWINDOWLONGPTR )
{
   hb_retptr( ( void * ) GetWindowLongPtr( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrA( HWND hWnd, int nIndex, LONG_PTR dwNewLong);

HB_FUNC( VWN_SETWINDOWLONGPTR )
{
   hb_retnl( (LONG) SetWindowLongPtr( (HWND) HB_PARWH( 1 ),
                                      hb_parni( 2 )       ,
                                      (LONG_PTR) HB_PARWH( 3 )
                                    ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrA( IN HWND hWnd, IN int nIndex);
#ifndef __WATCOMC__
HB_FUNC( VWN_GETCLASSLONGPTR )
{
    HB_RETWI( GetClassLongPtr( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrA( IN HWND hWnd, IN int nIndex, IN LONG_PTR dwNewLong);

HB_FUNC( VWN_SETCLASSLONGPTR )
{
   HB_RETWI( SetClassLongPtr( (HWND) HB_PARWH( 1 ), hb_parni( 2 ), (LONG_PTR) hb_parnl(3) ) );
}
#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetWindowInfo( IN HWND hwnd, OUT PWINDOWINFO pwi );

#if (WINVER >= 0X0500)

HB_FUNC( VWN_GETWINDOWINFO )
{
   WINDOWINFO pwi ;

   if ( GetWindowInfo( (HWND) HB_PARWH( 1 ), &pwi ) )

      hb_retclen( (char *) &pwi, sizeof( WINDOWINFO) );

}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetTitleBarInfo( IN HWND hwnd, OUT PTITLEBARINFO pti );

#if (WINVER >= 0X0500)

HB_FUNC( VWN_GETTITLEBARINFO )
{
   TITLEBARINFO pti  ;

     if ( GetTitleBarInfo( (HWND) HB_PARWH( 1 ), &pti ) )

         hb_retclen( (char *) &pti, sizeof(TITLEBARINFO) );
}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EnumChildWindows( IN HWND hWndParent, IN WNDENUMPROC lpEnumFunc, IN LPARAM lParam);

//T.B.D

/*

HB_FUNC( VWN_ENUMCHILDWINDOWS )
{
   WNDENUMPROC lpEnumFunc ;

   // Your code goes here

   hb_retl( EnumChildWindows( (HWND) HB_PARWH( 1 )  ,
                              lpEnumFunc            ,
                              (LPARAM) hb_parnint( 3 )
                            ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EnumWindows( IN WNDENUMPROC lpEnumFunc, IN LPARAM lParam);

/*

HB_FUNC( VWN_ENUMWINDOWS )
{
   WNDENUMPROC lpEnumFunc ;

   // Your code goes here

   hb_retl( EnumWindows( lpEnumFunc, (LPARAM) hb_parnint( 2 ) ) );
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI RealGetWindowClassA( IN HWND hwnd, OUT LPSTR pszType, IN UINT cchType );

/*
HB_FUNC( VWN_REALGETWINDOWCLASS )
{
   hb_retni( RealGetWindowClass( (HWND) HB_PARWH( 1 ),
                                 (LPSTR) hb_parcx( 2 ),
                                 (UINT) hb_parni( 3 )
                               ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI RealChildWindowFromPoint( IN HWND hwndParent, IN POINT ptParentClientCoords );
//SYNTAX REALCHILDWINDOWFROMPOINT(nWnd, aPoint) ->nWnd

#if (WINVER >= 0X0500)

HB_FUNC( VWN_REALCHILDWINDOWFROMPOINT )
{
   POINT ptParentClientCoords ;
   Array2Point( hb_param( 2 , HB_IT_ARRAY) , &ptParentClientCoords );

   HB_RETWH( RealChildWindowFromPoint( (HWND) HB_PARWH( 1 ),
                                              ptParentClientCoords
                                            ) );

}


#endif

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetWindowExtEx( IN HDC, IN int, IN int, OUT LPSIZE);
// SYNTAX:
// SetWindowExtEx(nHdc,nX,nY) -> aOldSize, or NIL


HB_FUNC( VWN_SETWINDOWEXTEX )
{
   SIZE lpSize ;
   PHB_ITEM pArray;

   if( SetWindowExtEx( (HDC) HB_PARWH( 1 ),
                             hb_parni( 2 ),
                             hb_parni( 3 ),
                             &lpSize
                             ) > 0 )
     {

     pArray = Size2Array(&lpSize);
     hb_itemReturn( pArray );
     hb_itemRelease( pArray );

     }
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetWindowOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

// SYNTAX:
// SetWindowOrgEx(nHdc,nX,nY) -> aOldOrg, or NIL

HB_FUNC( VWN_SETWINDOWORGEX )
{
   POINT lpPoint ;
   PHB_ITEM pArray;

   if( SetWindowOrgEx( (HDC) HB_PARWH( 1 ),
                             hb_parni( 2 ),
                             hb_parni( 3 ),
                             &lpPoint
                             ) > 0 )

   {
     pArray = Point2Array(&lpPoint);
     hb_itemReturn( pArray );
     hb_itemRelease( pArray );
   }
}




//-----------------------------------------------------------------------------
/* usage:
  hwndClient:=CreateMdiClient(hWnd,; // frame window
                                 0,; // window menu
                               100,; // first child id
                                 0,; // left
                                 0,; // top
                               150,; // width
                               200)  // height
*/

HB_FUNC( VWN_CREATEMDICLIENT )
{
  HWND hwndClient;
  HWND hFrame = (HWND) HB_PARWH(1);
  CLIENTCREATESTRUCT clientCreate ;
  clientCreate.hWindowMenu  = (HMENU)HB_PARWH(2);
  clientCreate.idFirstChild = (INT)hb_parni(3);
  hwndClient = CreateWindowEx(WS_EX_CLIENTEDGE,"MDICLIENT", NULL,WS_CHILD|WS_CLIPSIBLINGS|WS_VISIBLE,hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), (HWND)hFrame,0,GetModuleHandle(NULL),&clientCreate);
  HB_RETWH( hwndClient );
}

//-----------------------------------------------------------------------------

// sets a member of the MINMAXINFO  structure

HB_FUNC( VWN_SETMINMAXINFO )
{
  MINMAXINFO *mmi = (MINMAXINFO *) HB_PARWH(1);
  POINT pt  ;

  pt.x = hb_parni( 3, 1 );
  pt.y = hb_parni( 3, 2 );

  switch (hb_parni(2)) {
    case 2:
      mmi->ptMaxSize = pt ;
      break;

    case 3:
      mmi->ptMaxPosition = pt ;
      break;

    case 4:
      mmi->ptMinTrackSize = pt ;
      break;

    case 5:
      mmi->ptMaxTrackSize = pt ;
      break;

  }

}


//-----------------------------------------------------------------------------
/*
BOOL AllowSetForegroundWindow( DWORD dwProcessId

usage:

#define ASFW_ANY    (-1)

AllowSetForegroundWindow( ASFW_ANY or GetCurrentProcessId() )

);
*/
/*
#if(WINVER >= 0x0500)


HB_FUNC( VWN_ALLOWSETFOREGROUNDWINDOW )
{
   hb_retl( AllowSetForegroundWindow ( (DWORD) hb_parnl( 1 ) ) );
}

#endif
*/


#ifndef ASFW_ANY
  #define ASFW_ANY    ((DWORD)-1)
#endif

HB_FUNC( VWN_ALLOWSETFOREGROUNDWINDOW )
{

   HINSTANCE h = LoadLibraryEx( "user32.dll", NULL, 0 );
   BOOL bASFWRet = (BOOL) FALSE ;
   DWORD dwProcessId = HB_ISNIL( 1 ) ? ASFW_ANY : (DWORD) hb_parnl( 1 );

   if( h )
   {
      typedef BOOL (WINAPI *xbAllowSetForegroundWindow)( DWORD dwProcessId );
      xbAllowSetForegroundWindow pfnASFW = (xbAllowSetForegroundWindow)
      GetProcAddress( h, "AllowSetForegroundWindow");

      if( pfnASFW )
      {
         bASFWRet = (BOOL) pfnASFW( dwProcessId );
      }

      FreeLibrary( h );
   }

   hb_retl( bASFWRet );
}

//-----------------------------------------------------------------------------
/*
BOOL LockSetForegroundWindow( UINT uLockCode
);
*/
/*
#if (WINVER >= 0X0500)

HB_FUNC( VWN_LOCKSETFOREGROUNDWINDOW )
{
   hb_retl( LockSetForegroundWindow( (UINT) hb_parnl( 1 ) ) );
}

#endif
*/
HB_FUNC( VWN_LOCKWINDOWUPDATE )
{
   hb_retl( LockWindowUpdate( (HWND) HB_PARWH( 1 ) ) );
}
