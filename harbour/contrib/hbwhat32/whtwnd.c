/*
 * $Id$
 */

#define HB_OS_WIN_32_USED

#include <windows.h>
#include <winuser.h>
#include "item.api"
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

HB_FUNC( ISICONIC )
{
   hb_retl( IsIconic( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( ISWINDOWVISIBLE )
{
   hb_retl( IsWindowVisible( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( ISZOOMED )
{
   hb_retl( IsZoomed( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IsWindowUnicode( IN HWND hWnd);


HB_FUNC( ISWINDOWUNICODE )
{
   hb_retl( IsWindowUnicode( (HWND) hb_parnl( 1 ) ) ) ;
}



//-----------------------------------------------------------------------------

HB_FUNC( CLOSEWINDOW )
{
   hb_retl( CloseWindow( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( FINDWINDOW )
{

   hb_retnl((ULONG) FindWindow( (LPCSTR) hb_parcx(1), ISCHAR(2) ? hb_parcx(2):NULL ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI FindWindowExA( IN HWND, IN HWND, IN LPCSTR, IN LPCSTR);


HB_FUNC( FINDWINDOWEX )
{
   hb_retnl( (LONG) FindWindowEx( (HWND) hb_parnl( 1 ) ,
                                  (HWND) hb_parnl( 2 ) ,
                                  (LPCSTR) hb_parcx( 3 ),
                                  (LPCSTR) hb_parcx( 4 )
                                ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( ISCHILD )
{
   hb_retl( ( BOOL ) IsChild( (HWND) hb_parnl(1), (HWND) hb_parnl(2) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( UPDATEWINDOW )
{
  hb_retl( UpdateWindow( (HWND) hb_parnl(1)) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( GETWINDOWLONG )
{
   hb_retnl( GetWindowLong( (HWND) hb_parnl(1), hb_parni(2) ));
}

//-----------------------------------------------------------------------------

HB_FUNC( SETWINDOWLONG )
{
   hb_retnl( SetWindowLong( (HWND) hb_parnl(1), hb_parni(2), hb_parnl(3) ));
}


//-----------------------------------------------------------------------------

HB_FUNC( ENABLEWINDOW )
{
   EnableWindow( (HWND) hb_parnl(1), hb_parl(2) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IsWindowEnabled( IN HWND hWnd);


HB_FUNC( ISWINDOWENABLED )
{
   hb_retl( IsWindowEnabled( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( DESTROYWINDOW )
{
   hb_retl( DestroyWindow( (HWND) hb_parnl( 1 )) );
}

//-----------------------------------------------------------------------------

HB_FUNC( ISWINDOW )
{
    hb_retl( IsWindow( (HWND) hb_parnl( 1 )) );
}

//-----------------------------------------------------------------------------

HB_FUNC( SHOWWINDOW )
{
   hb_retl( ShowWindow( (HWND) hb_parnl( 1 ), hb_parni(2) ));
}

//-----------------------------------------------------------------------------
HB_FUNC( MOVEWINDOW )
{
  hb_retl( MoveWindow(
                       (HWND) hb_parnl(1),
                       hb_parni(2),
                       hb_parni(3),
                       hb_parni(4),
                       hb_parni(5),
                       (ISNIL(6) ? TRUE : hb_parl(6))
                      ));
}

//-----------------------------------------------------------------------------

HB_FUNC( DEFWINDOWPROC )
{
  hb_retnl( DefWindowProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( DEFDLGPROC )
{
  hb_retnl( DefDlgProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( DEFMDICHILDPROC )
{
  hb_retnl( DefMDIChildProc( (HWND) hb_parnl(1), hb_parnl(2), hb_parnl(3), hb_parnl(4)));
}

//-----------------------------------------------------------------------------

HB_FUNC( DEFFRAMEPROC )
{
  hb_retnl( DefFrameProc( (HWND) hb_parnl(1), (HWND) hb_parnl(2), hb_parnl(3), hb_parnl(4), hb_parnl(5)));
}


//-----------------------------------------------------------------------------

HB_FUNC( CALLWINDOWPROC )
{
  hb_retnl( CallWindowProc( (WNDPROC) hb_parptr(1), (HWND) hb_parnl(2), hb_parni(3), hb_parnl(4), hb_parnl(5)));
}

//-----------------------------------------------------------------------------

//WINUSERAPI BOOL WINAPI InvalidateRect(    IN HWND hWnd,    IN CONST RECT *lpRect,    IN BOOL bErase);
//SYNTAX INVALIDATERECT ( hWnd,aArray ) -> lSuccess

HB_FUNC( INVALIDATERECT )
{
   RECT rc;
   BOOL bRectOk ;

   bRectOk = ( ISARRAY( 2 )  &&   Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) ) ;

   hb_retl( InvalidateRect(
                           ISNIL(1) ? NULL : (HWND) hb_parnl( 1 )    ,  // handle of window with changed update region
                           bRectOk ? &rc : NULL ,  // address of rectangle coordinates
                           ISLOG(3) ? hb_parl( 3 ) : TRUE         // erase-background flag
                          ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI RedrawWindow( IN HWND hWnd, IN CONST RECT *lprcUpdate, IN HRGN hrgnUpdate, IN UINT flags);

HB_FUNC( REDRAWWINDOW )
{
   RECT rc ;
   BOOL bRectOk ;

   bRectOk = ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ) ) ;

   hb_retl( RedrawWindow(
                          (HWND) hb_parnl( 1 )                     ,   // handle of window
                          bRectOk ? &rc : NULL                     ,   // address of structure with update rectangle
                          ISNIL( 3 ) ? NULL : (HRGN) hb_parnl( 3 ) ,   // handle of update region
                          hb_parni( 4 )                                // array of redraw flags
                         ) );

}


//-----------------------------------------------------------------------------
//WINUSERAPI BOOL WINAPI GetClientRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETCLIENTRECT( hWnd ) -> aRect

HB_FUNC( GETCLIENTRECT )
{
   RECT rc;

   PHB_ITEM aMetr ;
   GetClientRect( (HWND) hb_parnl( 1 ), &rc );

   aMetr = Rect2Array( &rc  );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}

//-----------------------------------------------------------------------------

//WINUSERAPI BOOL WINAPI GetWindowRect(    IN HWND hWnd,    OUT LPRECT lpRect);
//Syntax GETWINDOWRECT( hWnd ) -> aRect

HB_FUNC( GETWINDOWRECT )
{
   RECT rc;
   PHB_ITEM aMetr ;

   GetWindowRect( (HWND) hb_parnl( 1 ),   &rc );
   aMetr = Rect2Array( &rc  );

   _itemReturn( aMetr );
   _itemRelease( aMetr );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ShowOwnedPopups( IN HWND hWnd, IN BOOL fShow);


HB_FUNC( SHOWOWNEDPOPUPS )
{
   hb_retl( ShowOwnedPopups( (HWND) hb_parnl( 1 ), hb_parl( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI OpenIcon( IN HWND hWnd);


HB_FUNC( OPENICON )
{
   hb_retl( OpenIcon( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI HDWP WINAPI BeginDeferWindowPos( IN int nNumWindows);


HB_FUNC( BEGINDEFERWINDOWPOS )
{
   hb_retnl( (LONG) BeginDeferWindowPos( hb_parni( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HDWP WINAPI DeferWindowPos( IN HDWP hWinPosInfo, IN HWND hWnd, IN HWND hWndInsertAfter, IN int x, IN int y, IN int cx, IN int cy, IN UINT uFlags);


HB_FUNC( DEFERWINDOWPOS )
{
   hb_retnl( (LONG) DeferWindowPos( (HDWP) hb_parnl( 1 ),
                                    (HWND) hb_parnl( 2 ),
                                    (HWND) hb_parnl( 3 ),
                                    hb_parni( 4 )       ,
                                    hb_parni( 5 )       ,
                                    hb_parni( 6 )       ,
                                    hb_parni( 7 )       ,
                                    (UINT) hb_parni( 8 )
                                  ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EndDeferWindowPos( IN HDWP hWinPosInfo);


HB_FUNC( ENDDEFERWINDOWPOS )
{
   hb_retl( EndDeferWindowPos( (HDWP) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowPos( IN HWND hWnd, IN HWND hWndInsertAfter, IN int X, IN int Y, IN int cx, IN int cy, IN UINT uFlags);


HB_FUNC( SETWINDOWPOS )
{
   hb_retl( SetWindowPos( (HWND) hb_parnl( 1 ),
                          (HWND) hb_parnl( 2 ),
                          hb_parni( 3 )       ,
                          hb_parni( 4 )       ,
                          hb_parni( 5 )       ,
                          hb_parni( 6 )       ,
                          (UINT) hb_parni( 7 )
                        ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI SetFocus( IN HWND hWnd);


HB_FUNC( SETFOCUS )
{
   hb_retnl( (LONG) SetFocus( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetActiveWindow( VOID);


HB_FUNC( GETACTIVEWINDOW )
{
   hb_retnl( (LONG) GetActiveWindow(  ) ) ;
}

//-----------------------------------------------------------------------------
// ok
// WINUSERAPI HWND WINAPI SetActiveWindow( IN HWND hWnd);


HB_FUNC( SETACTIVEWINDOW )
{
   hb_retnl( (LONG) SetActiveWindow( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// ok
// WINUSERAPI HWND WINAPI GetForegroundWindow( VOID);


HB_FUNC( GETFOREGROUNDWINDOW )
{
   hb_retnl( (LONG) GetForegroundWindow(  ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetFocus( VOID);


HB_FUNC( GETFOCUS )
{
   hb_retnl( (LONG) GetFocus(  ) ) ;
}


//-----------------------------------------------------------------------------
// ok
// WINUSERAPI BOOL WINAPI SetForegroundWindow( IN HWND hWnd);

HB_FUNC( SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AnyPopup( VOID);


HB_FUNC( ANYPOPUP )
{
   hb_retl( AnyPopup(  ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI BringWindowToTop( IN HWND hWnd);


HB_FUNC( BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetClassNameA( IN HWND hWnd, OUT LPSTR lpClassName, IN int nMaxCount);


HB_FUNC( GETCLASSNAME )
{
   char *cText= (char*) hb_xgrab( MAX_PATH+1 );

   GetClassName( (HWND) hb_parnl( 1 ),
                 (LPSTR) cText ,
                 MAX_PATH
                ) ;

   hb_retc( cText);
   hb_xfree( cText ) ;
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetTopWindow( IN HWND hWnd);


HB_FUNC( GETTOPWINDOW )
{
   hb_retnl( (LONG) GetTopWindow( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScrollWindow( IN HWND hWnd, IN int XAmount, IN int YAmount, IN CONST RECT *lpRect, IN CONST RECT *lpClipRect);
//SYNTAX SCROLLWINDOW(hWnd,nXAmount,nYAmount,aRect1,aRect2) -> lSuccess

HB_FUNC( SCROLLWINDOW )
{
   RECT lpRect     ;
   RECT lpClipRect ;
   Array2Rect( hb_param( 4 , HB_IT_ARRAY ) , &lpRect ) ;
   Array2Rect( hb_param( 5 , HB_IT_ARRAY ) , &lpClipRect ) ;

   hb_retl( ScrollWindow( (HWND) hb_parnl( 1 ),
                          hb_parni( 2 )       ,
                          hb_parni( 3 )       ,
                          &lpRect             ,
                          &lpClipRect
                        ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowTextA( IN HWND hWnd, IN LPCSTR lpString);


HB_FUNC( SETWINDOWTEXT )
{
   hb_retl( SetWindowText( (HWND) hb_parnl( 1 ), (LPSTR) hb_parcx( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowTextA( IN HWND hWnd, OUT LPSTR lpString, IN int nMaxCount);

// modified API - returns the string !

HB_FUNC( GETWINDOWTEXT )
{
   int iLen = GetWindowTextLength( (HWND) hb_parnl( 1 ) )  ;
   char *cText = (char*) hb_xgrab( iLen+1 ) ;
   int iRet = GetWindowText( (HWND) hb_parnl( 1 ) ,
                            (LPSTR) cText       ,
                             iLen+1
                           ) ;

   hb_retclen( cText, iRet ) ;
   hb_xfree( cText ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowTextLengthA( IN HWND hWnd);


HB_FUNC( GETWINDOWTEXTLENGTH )
{
   hb_retni( GetWindowTextLength( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowContextHelpId( IN HWND, IN DWORD);


HB_FUNC( SETWINDOWCONTEXTHELPID )
{
   hb_retl( SetWindowContextHelpId( (HWND) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetWindowContextHelpId( IN HWND);


HB_FUNC( GETWINDOWCONTEXTHELPID )
{
   hb_retnl( (LONG) GetWindowContextHelpId( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetMenuContextHelpId( IN HMENU, IN DWORD);


HB_FUNC( SETMENUCONTEXTHELPID )
{
   hb_retl( SetMenuContextHelpId( (HMENU) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetMenuContextHelpId( IN HMENU);


HB_FUNC( GETMENUCONTEXTHELPID )
{
   hb_retnl( (LONG) GetMenuContextHelpId( (HMENU) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetWindow( IN HWND, IN UINT );


HB_FUNC( GETWINDOW )
{
   hb_retnl( (LONG) GetWindow( (HWND)hb_parnl(1), (UINT) hb_parni( 2 ) ) ) ;
}


 //-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ClientToScreen( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX CLIENTTOSCREEN( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20,60,80}
if CLIENTTOSCREEN(nWnd , @aArray)
endif
*/

HB_FUNC( CLIENTTOSCREEN )
{
   POINT Point ;
   PHB_ITEM pArray;
   pArray=  hb_param( 2 , HB_IT_ARRAY );
   if (Array2Point( pArray ,&Point  ) )
   {
      if (ClientToScreen( (HWND) hb_parnl( 1 ), &Point ))
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
         hb_retl( FALSE ) ;
   }
      else
         hb_retl( FALSE ) ;

}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScreenToClient( IN HWND hWnd, IN OUT LPPOINT lpPoint);
// SYNTAX SCREENTOCLIENT( nWhd , @aArray ) -> lSuccess
/*Example
aArray:={20,20}
if SCREENTOCLIENT(nWnd , @aArray)
endif
*/

HB_FUNC( SCREENTOCLIENT )
{
   POINT Point ;
   PHB_ITEM pArray = hb_param( 2 , HB_IT_ARRAY );

   if (Array2Point(pArray, &Point ) )
   {
      if( ScreenToClient( (HWND) hb_parnl( 1 ), &Point ) >0)
      {
          Point2ArrayEx( &Point   , pArray );
          hb_retl( TRUE ) ;
      }
      else
         hb_retl( FALSE ) ;
   }
      else
         hb_retl( FALSE ) ;


}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI MapWindowPoints( IN HWND hWndFrom, IN HWND hWndTo, IN OUT LPPOINT lpPoints, IN UINT cPoints);
//SYNTAX MAPWINDOWPOINTS(nWndSource,nWndDest,@aPoint,nPoint) -> nP


HB_FUNC( MAPWINDOWPOINTS )
{
   POINT lpPoints ;
   PHB_ITEM pArray =hb_param( 3 , HB_IT_ARRAY );
   Array2Point( pArray ,&lpPoints );

   hb_retni( MapWindowPoints( (HWND) hb_parnl( 1 ),
                              (HWND) hb_parnl( 2 ),
                               &lpPoints            ,
                              (UINT) hb_parni( 4 )
                            ) ) ;
          Point2ArrayEx( &lpPoints   , pArray );

}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI WindowFromPoint( IN POINT Point);
//SYNTAX WINDOWFROMPOINT(aPoint) -> nPoint


HB_FUNC( WINDOWFROMPOINT )
{
   POINT Point ;
   Array2Point( hb_param( 1 , HB_IT_ARRAY ), &Point ) ;

   hb_retnl( (LONG) WindowFromPoint( Point ) ) ;
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI ChildWindowFromPoint( IN HWND hWndParent, IN POINT Point);
//SYNTAX CHILDWINDOWFROMPOINT(nWnd,aPoint) -> nPoint


HB_FUNC( CHILDWINDOWFROMPOINT )
{
   POINT Point      ;

   Array2Point( hb_param( 2 , HB_IT_ARRAY ) ,&Point) ;

   hb_retnl( (LONG) ChildWindowFromPoint( (HWND) hb_parnl( 1 ), Point ) ) ;
}



//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI ChildWindowFromPointEx( IN HWND, IN POINT, IN UINT);
//SYNTAX CHILDWINDOWFROMPOINTEX(nWnd,aPoint,nStyle) -> nPoint

HB_FUNC( CHILDWINDOWFROMPOINTEX )
{
   POINT PoInt ;

   Array2Point( hb_param( 2 , HB_IT_ARRAY ) ,&PoInt) ;

   hb_retnl( (LONG) ChildWindowFromPointEx( (HWND) hb_parnl( 1 ),
                                            PoInt               ,
                                            (UINT) hb_parni( 3 )
                                          ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI GetWindowWord( IN HWND hWnd, IN int nIndex);


HB_FUNC( GETWINDOWWORD )
{
   hb_retni( GetWindowWord( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI SetWindowWord( IN HWND hWnd, IN int nIndex, IN WORD wNewWord);

HB_FUNC( SETWINDOWWORD )
{

   hb_retni( SetWindowWord( (HWND) hb_parnl( 1 ), hb_parni( 2 ), (WORD) hb_parni(3) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetDesktopWindow( VOID);

HB_FUNC( GETDESKTOPWINDOW )
{
   hb_retnl( (LONG) GetDesktopWindow(  ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetParent( IN HWND hWnd);

HB_FUNC( GETPARENT )
{
   hb_retnl( (LONG) GetParent( (HWND) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI SetParent( IN HWND hWndChild, IN HWND hWndNewParent);

HB_FUNC( SETPARENT )
{
   hb_retnl( (LONG) SetParent( (HWND) hb_parnl( 1 ), (HWND) hb_parnl( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI GetClassWord( IN HWND hWnd, IN int nIndex);


HB_FUNC( GETCLASSWORD )
{
   hb_retni( GetClassWord( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI WORD WINAPI SetClassWord( IN HWND hWnd, IN int nIndex, IN WORD wNewWord);

HB_FUNC( SETCLASSWORD )
{

   hb_retni( SetClassWord( (HWND) hb_parnl( 1 ), hb_parni( 2 ), (WORD) hb_parni( 3 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetClassLongA( IN HWND hWnd, IN int nIndex);

HB_FUNC( GETCLASSLONG )
{
   hb_retnl( (LONG) GetClassLong( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI SetClassLongA( IN HWND hWnd, IN int nIndex, IN LONG dwNewLong);

HB_FUNC( SETCLASSLONG )
{
   hb_retnl( (LONG) SetClassLong( (HWND) hb_parnl( 1 ),
                                  hb_parni( 2 )       ,
                                  hb_parnl( 3 )
                                ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI GetAncestor( IN HWND hwnd, IN UINT gaFlags );
#if(WINVER >= 0x0500)

HB_FUNC( GETANCESTOR )
{
   hb_retnl( (LONG) GetAncestor( (HWND) hb_parnl( 1 ), (UINT) hb_parni( 2 ) ) ) ;
}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ShowWindowAsync( IN HWND hWnd, IN int nCmdShow);


HB_FUNC( SHOWWINDOWASYNC )
{
   hb_retl( ShowWindowAsync( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
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


HB_FUNC( FLASHWINDOW )
{
   hb_retl( FlashWindow( (HWND) hb_parnl( 1 ), hb_parl( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AnimateWindow( IN HWND hWnd, IN DWORD dwTime, IN DWORD dwFlags);
#if(WINVER >= 0x0500)
HB_FUNC( ANIMATEWINDOW )
{
   hb_retl( AnimateWindow( (HWND) hb_parnl( 1 ) ,
                           (DWORD) hb_parnl( 2 ),
                           (DWORD) hb_parnl( 3 )
                         ) ) ;
}
#endif
//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetWindowPlacement( IN HWND hWnd, OUT WINDOWPLACEMENT *lpwndpl);
#if(WINVER >= 0x0500)
HB_FUNC( GETWINDOWPLACEMENT )
{
   WINDOWPLACEMENT wndpl ;
   wndpl.length=sizeof(WINDOWPLACEMENT);
   if ( GetWindowPlacement( (HWND) hb_parnl( 1 ), &wndpl ) )
      hb_retclen( ( char *) &wndpl, sizeof(WINDOWPLACEMENT) );

}

#endif
//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetWindowPlacement( IN HWND hWnd, IN CONST WINDOWPLACEMENT *lpwndpl);
#if(WINVER >= 0x0500)

HB_FUNC( SETWINDOWPLACEMENT )
{
   WINDOWPLACEMENT * lpwndpl = (WINDOWPLACEMENT *) hb_parc( 2 ); //hb_param( 2,HB_IT_STRING )->item.asString.value;


   hb_retl( SetWindowPlacement( (HWND) hb_parnl( 1 ), lpwndpl ) ) ;
}

#endif
//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI SetWindowRgn( IN HWND hWnd, IN HRGN hRgn, IN BOOL bRedraw);

HB_FUNC( SETWINDOWRGN )
{
   hb_retni( SetWindowRgn( (HWND) hb_parnl( 1 ),
                           (HRGN) hb_parnl( 2 ),
                           hb_parl( 3 )
                         ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetWindowRgn( IN HWND hWnd, IN HRGN hRgn);

HB_FUNC( GETWINDOWRGN )
{
   hb_retni( GetWindowRgn( (HWND) hb_parnl( 1 ), (HRGN) hb_parnl( 2 ) ) ) ;
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetPropA( IN HWND hWnd, IN LPCSTR lpString, IN HANDLE hData);

HB_FUNC( SETPROP )
{
   hb_retl( SetProp( (HWND) hb_parnl( 1 )  ,
                     (LPCSTR) hb_parcx( 2 ) ,
                     (HANDLE) hb_parnl( 3 )
                   ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI GetPropA( IN HWND hWnd, IN LPCSTR lpString);

HB_FUNC( GETPROP )
{
   hb_retnl( (LONG) GetProp( (HWND) hb_parnl( 1 ), (LPCSTR) hb_parcx( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI RemovePropA( IN HWND hWnd, IN LPCSTR lpString);

HB_FUNC( REMOVEPROP )
{
   hb_retnl( (LONG) RemoveProp( (HWND) hb_parnl( 1 ), (LPCSTR) hb_parcx( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI EnumPropsExA( IN HWND hWnd, IN PROPENUMPROCEXA lpEnumFunc, IN LPARAM lParam);


//T.B.D.

/*

HB_FUNC( ENUMPROPSEX )
{
   PROPENUMPROCEXA lpEnumFunc ;

   // Your code goes here

   hb_retni( EnumPropsEx( (HWND) hb_parnl( 1 )  ,
                          lpEnumFunc            ,
                          (LPARAM) hb_parnl( 3 )
                        ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI EnumPropsA( IN HWND hWnd, IN PROPENUMPROCA lpEnumFunc);

/*

HB_FUNC( ENUMPROPS )
{
   PROPENUMPROCA lpEnumFunc ;

   // Your code goes here

   hb_retni( EnumProps( (HWND) hb_parnl( 1 ), lpEnumFunc ) ) ;
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

HB_FUNC( ADJUSTWINDOWRECT )
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



HB_FUNC( ADJUSTWINDOWRECTEX )
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


HB_FUNC( GETWINDOWLONGPTR )
{
   hb_retptr( ( void * ) GetWindowLongPtr( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI LONG_PTR WINAPI SetWindowLongPtrA( HWND hWnd, int nIndex, LONG_PTR dwNewLong);

HB_FUNC( SETWINDOWLONGPTR )
{
   hb_retnl( (LONG) SetWindowLongPtr( (HWND) hb_parnl( 1 ),
                                      hb_parni( 2 )       ,
                                      ISPOINTER( 3 ) ? (LONG_PTR) hb_parptr( 3 ) : (LONG_PTR) hb_parnl( 3 )
                                    ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI ULONG_PTR WINAPI GetClassLongPtrA( IN HWND hWnd, IN int nIndex);
#ifndef __WATCOMC__
HB_FUNC( GETCLASSLONGPTR )
{
    hb_retnl((ULONG_PTR) GetClassLongPtr( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI ULONG_PTR WINAPI SetClassLongPtrA( IN HWND hWnd, IN int nIndex, IN LONG_PTR dwNewLong);

HB_FUNC( SETCLASSLONGPTR )
{
   hb_retnl( (ULONG_PTR) SetClassLongPtr( (HWND) hb_parnl( 1 ), hb_parni( 2 ), (LONG_PTR) hb_parnl(3) ) ) ;
}
#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetWindowInfo( IN HWND hwnd, OUT PWINDOWINFO pwi );

#if (WINVER >= 0X0500)

HB_FUNC( GETWINDOWINFO )
{
   WINDOWINFO pwi ;

   if ( GetWindowInfo( (HWND) hb_parnl( 1 ), &pwi ) )

      hb_retclen( (char *) &pwi, sizeof( WINDOWINFO) ) ;

}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetTitleBarInfo( IN HWND hwnd, OUT PTITLEBARINFO pti );

#if (WINVER >= 0X0500)

HB_FUNC( GETTITLEBARINFO )
{
   TITLEBARINFO pti  ;

     if ( GetTitleBarInfo( (HWND) hb_parnl( 1 ), &pti ) )

         hb_retclen( (char *) &pti, sizeof(TITLEBARINFO) );
}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EnumChildWindows( IN HWND hWndParent, IN WNDENUMPROC lpEnumFunc, IN LPARAM lParam);

//T.B.D

/*

HB_FUNC( ENUMCHILDWINDOWS )
{
   WNDENUMPROC lpEnumFunc ;

   // Your code goes here

   hb_retl( EnumChildWindows( (HWND) hb_parnl( 1 )  ,
                              lpEnumFunc            ,
                              (LPARAM) hb_parnl( 3 )
                            ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EnumWindows( IN WNDENUMPROC lpEnumFunc, IN LPARAM lParam);

/*

HB_FUNC( ENUMWINDOWS )
{
   WNDENUMPROC lpEnumFunc ;

   // Your code goes here

   hb_retl( EnumWindows( lpEnumFunc, (LPARAM) hb_parnl( 2 ) ) ) ;
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI RealGetWindowClassA( IN HWND hwnd, OUT LPSTR pszType, IN UINT cchType );

/*
HB_FUNC( REALGETWINDOWCLASS )
{
   hb_retni( RealGetWindowClass( (HWND) hb_parnl( 1 ),
                                 (LPSTR) hb_parcx( 2 ),
                                 (UINT) hb_parni( 3 )
                               ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI HWND WINAPI RealChildWindowFromPoint( IN HWND hwndParent, IN POINT ptParentClientCoords );
//SYNTAX REALCHILDWINDOWFROMPOINT(nWnd, aPoint) ->nWnd

#if (WINVER >= 0X0500)

HB_FUNC( REALCHILDWINDOWFROMPOINT )
{
   POINT ptParentClientCoords ;
   Array2Point( hb_param( 2 , HB_IT_ARRAY) , &ptParentClientCoords );

   hb_retnl( (LONG) RealChildWindowFromPoint( (HWND) hb_parnl( 1 ),
                                              ptParentClientCoords
                                            ) ) ;

}


#endif

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetWindowExtEx( IN HDC, IN int, IN int, OUT LPSIZE);
// SYNTAX:
// SetWindowExtEx(nHdc,nX,nY) -> aOldSize, or NIL


HB_FUNC( SETWINDOWEXTEX )
{
   SIZE lpSize ;
   PHB_ITEM pArray;

   if( SetWindowExtEx( (HDC) hb_parnl( 1 ),
                            hb_parni( 2 )      ,
                            hb_parni( 3 )      ,
                            &lpSize
                            ) >0)
     {

     pArray = Size2Array(&lpSize) ;
     _itemReturn( pArray );
     _itemRelease( pArray );

     }
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetWindowOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

// SYNTAX:
// SetWindowOrgEx(nHdc,nX,nY) -> aOldOrg, or NIL

HB_FUNC( SETWINDOWORGEX )
{
   POINT lpPoint ;
   PHB_ITEM pArray;

   if( SetWindowOrgEx( (HDC) hb_parnl( 1 ),
                            hb_parni( 2 )      ,
                            hb_parni( 3 )      ,
                            &lpPoint
                            ) >0)

   {
     pArray = Point2Array(&lpPoint) ;
     _itemReturn( pArray );
     _itemRelease( pArray );
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

HB_FUNC( CREATEMDICLIENT )
{
  HWND hwndClient;
  HWND hFrame = (HWND) hb_parnl(1);
  CLIENTCREATESTRUCT clientCreate ;
  clientCreate.hWindowMenu  = (HMENU)hb_parnl(2);
  clientCreate.idFirstChild = (INT)hb_parni(3);
  hwndClient = CreateWindowEx(WS_EX_CLIENTEDGE,"MDICLIENT", NULL,WS_CHILD|WS_CLIPSIBLINGS|WS_VISIBLE,hb_parni(4), hb_parni(5), hb_parni(6), hb_parni(7), (HWND)hFrame,0,GetModuleHandle(NULL),&clientCreate);
  hb_retnl((LONG)hwndClient);
}

//-----------------------------------------------------------------------------

// sets a member of the MINMAXINFO  structure

HB_FUNC( SETMINMAXINFO )
{
  MINMAXINFO *mmi = (MINMAXINFO *) hb_parnl(1) ;
  POINT pt  ;

  pt.x = hb_parni( 3, 1 ) ;
  pt.y = hb_parni( 3, 2 ) ;

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


HB_FUNC( ALLOWSETFOREGROUNDWINDOW )
{
   hb_retl( AllowSetForegroundWindow ( (DWORD) hb_parnl( 1 ) ) );
}

#endif
*/


#ifndef ASFW_ANY
  #define ASFW_ANY    ((DWORD)-1)
#endif

HB_FUNC( ALLOWSETFOREGROUNDWINDOW )
{

   HINSTANCE h = LoadLibraryEx( "user32.dll", NULL, 0 );
   BOOL bASFWRet = (BOOL) FALSE ;
   DWORD dwProcessId = ISNIL( 1 ) ? ASFW_ANY : (DWORD) hb_parnl( 1 );

   if( h )
   {
      typedef BOOL (WINAPI *xbAllowSetForegroundWindow)( DWORD dwProcessId );
      xbAllowSetForegroundWindow pfnASFW = (xbAllowSetForegroundWindow)
      GetProcAddress( h, "AllowSetForegroundWindow") ;

      if( pfnASFW )
      {
         bASFWRet = (BOOL) pfnASFW( dwProcessId ) ;
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

HB_FUNC( LOCKSETFOREGROUNDWINDOW )
{
   hb_retl( LockSetForegroundWindow( (UINT) hb_parnl( 1 ) ) );
}

#endif
*/
HB_FUNC( LOCKWINDOWUPDATE )
{
   hb_retl( LockWindowUpdate( (HWND) hb_parnl( 1 ) ) );
}


