/*
 * $Id$
 */

// what32.lib
// Device Context functions

// modified:
// GetDCOrgEx(hDC) -> aPt or NIL on failure

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include <windows.h>
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

HB_FUNC( GETDC )
{
   hb_retnl( (ULONG) GetDC( (HWND) hb_parnl(1) ) ) ;
}

//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI RestoreDC(IN HDC, IN int);

HB_FUNC( RESTOREDC )
{
   hb_retl( RestoreDC( (HDC) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------

// WINGDIAPI int WINAPI SaveDC(IN HDC);

HB_FUNC( SAVEDC )
{
   hb_retni( SaveDC( (HDC) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI HDC WINAPI GetDCEx( IN HWND hWnd, IN HRGN hrgnClip, IN DWORD flags);

HB_FUNC( GETDCEX )
{
   hb_retnl( (LONG) GetDCEx( (HWND) hb_parnl( 1 ) ,
                             (HRGN) hb_parnl( 2 ) ,
                             (DWORD) hb_parnl( 3 )
                           ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( RELEASEDC )
{
   hb_retni( ReleaseDC( (HWND) hb_parnl(1), (HDC) hb_parnl(2) ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DeleteDC( IN HDC);

HB_FUNC( DELETEDC )
{
   hb_retl( DeleteDC( (HDC) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI CancelDC( IN HDC);

HB_FUNC( CANCELDC )
{
   hb_retl( CancelDC( (HDC) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINGDIAPI HDC WINAPI CreateCompatibleDC( IN HDC);

HB_FUNC( CREATECOMPATIBLEDC )
{
   hb_retnl( (LONG) CreateCompatibleDC( (HDC) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( WINDOWFROMDC )
{
   hb_retnl( (LONG) WindowFromDC( (HDC) hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------

// WINUSERAPI HDC WINAPI GetWindowDC( IN HWND hWnd);

HB_FUNC( GETWINDOWDC )
{
   hb_retnl( (LONG) GetWindowDC( (HWND) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// uses xHarbour structures

// HDC CreateDC(LPCTSTR lpszDriver, LPCTSTR lpszDevice, PCTSTR lpszOutput, CONST DEVMODE *lpInitData )

HB_FUNC( CREATEDC )
{
   DEVMODE *lpInitData = NULL;

   if ( ! ISNIL( 4 ) )
        lpInitData = (DEVMODE *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING)->item.asString.value ;

   hb_retnl( (ULONG) CreateDC((LPCTSTR) hb_parcx( 1 )  ,       // pointer to string specifying driver name
                              (LPCTSTR) hb_parcx( 2 )  ,       // pointer to string specifying device name
                              NULL                    ,       // do not use; set to NULL
                              ISNIL( 4 ) ? NULL : lpInitData // pointer to optional printer data
                             )
           ) ;
}



//-----------------------------------------------------------------------------
// WINGDIAPI HDC WINAPI ResetDCA(IN HDC, IN CONST DEVMODEA *);

HB_FUNC( RESETDC )
{

   DEVMODE *lpInitData = NULL;

   if ( ! ISNIL( 2 ) )
        lpInitData = (DEVMODE *) hb_parc( 2 ); //hb_param( 2, HB_IT_STRING)->item.asString.value ;

   hb_retnl( (LONG) ResetDCA( (HDC) hb_parnl( 1 ),
                               ISNIL( 2 )? NULL : lpInitData ) ) ;

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetDCOrgEx( IN HDC, OUT LPPOINT);

// modified
// GetDCOrgEx(hDC) -> aPt


HB_FUNC( GETDCORGEX )
{
   POINT Point ;
   PHB_ITEM aPt;

   if ( GetDCOrgEx( (HDC) hb_parnl( 1 ), &Point ) )
   {
     aPt = Point2Array(&Point);
     _itemReturn( aPt );
     _itemRelease( aPt );
   }

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ScrollDC( IN HDC hDC, IN int dx, IN int dy, IN CONST RECT *lprcScroll, IN CONST RECT *lprcClip, IN HRGN hrgnUpdate, OUT LPRECT lprcUpdate);

/* Call as
Local aSrc  := { 1 , 2 , 5  , 1  }
local asrc2 := { 5 , 4 , 80 , 52 }
Local aDest := array( 4 )
SCROLLDC(nHdc,ndx,ndy,aSrc,aSrc2,hrgnUpdate,@aDest)
*/

HB_FUNC( SCROLLDC )
{
   RECT   lprcScroll ;
   RECT   lprcClip   ;
   RECT lprcUpdate ;
   PHB_ITEM pArray=hb_param(7,HB_IT_ARRAY);

   if( Array2Rect(hb_param( 4, HB_IT_ARRAY ) , &lprcScroll )  && Array2Rect(hb_param( 5, HB_IT_ARRAY ) , &lprcClip ) )
   {
      if( ScrollDC( (HDC) hb_parnl( 1 ) ,
                      hb_parni( 2 )       ,
                      hb_parni( 3 )       ,
                      &lprcScroll         ,
                      &lprcClip           ,
                      (HRGN) hb_parnl( 6 ),
                      &lprcUpdate
                    ) )
        {
         Rect2ArrayEx( &lprcUpdate,pArray);
         hb_retl(TRUE);
        }
   else
      hb_retl(FALSE);
   }
   else
      hb_retl(FALSE);
}








