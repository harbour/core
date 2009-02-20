/*
 * $Id$
 */

// hbwhat
// ViewPort functions

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include "hbapiitm.h"
#include "hbapi.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, SIZE *pt );
extern PHB_ITEM Size2Array( SIZE *siz  );
extern BOOL Array2Size(PHB_ITEM aSize, SIZE *siz );

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetViewportExtEx( IN HDC, OUT LPSIZE);

// Syntax
// GetViewPortExtEx(hDC)->aSize, or NIL

HB_FUNC( VWN_GETVIEWPORTEXTEX )
{
   SIZE siz ;
   PHB_ITEM aSize ;

   if ( GetViewportExtEx( (HDC) HB_PARWH( 1 ), &siz ) )
   {
       aSize = Size2Array( &siz );
       hb_itemReturn( aSize );
       hb_itemRelease( aSize );
   }

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetViewportOrgEx( IN HDC, OUT LPPOINT);

// Syntax
// GetViewportOrgEx(hDC) -> aOrigin, or NIL

HB_FUNC( VWN_GETVIEWPORTORGEX )
{
   POINT pt ;
   PHB_ITEM aPoint;

   if (  GetViewportOrgEx( (HDC) HB_PARWH( 1 ), &pt ) )
   {
      aPoint = Point2Array( &pt );
      hb_itemReturn( aPoint );
      hb_itemRelease( aPoint );
   }

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetWindowExtEx( IN HDC, OUT LPSIZE);

// Syntax
// GetWindowExEx(hDC) -> aSize, or NIL

HB_FUNC( VWN_GETWINDOWEXTEX )
{
   SIZE siz ;
   PHB_ITEM aSize ;

   if ( GetWindowExtEx( (HDC) HB_PARWH( 1 ), &siz ) )
   {
       aSize = Size2Array( &siz );
       hb_itemReturn( aSize );
       hb_itemRelease( aSize );
   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI ScaleViewportExtEx( IN HDC, IN int, IN int, IN int, IN int, OUT LPSIZE);

// Syntax
// ScaleViewport(hDC, xNum, xDeNum, yNum, yDeNum ) -> aOldSize, or NIL on failure

HB_FUNC( VWN_SCALEVIEWPORTEXTEX )
{
   SIZE siz ;
   PHB_ITEM aSize ;

   if (  ScaleViewportExtEx( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ),
                                hb_parni( 4 )    , hb_parni( 5 ), &siz  ) )
   {
       aSize = Size2Array( &siz );
       hb_itemReturn( aSize );
       hb_itemRelease( aSize );

   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetViewportExtEx( IN HDC, IN int, IN int, OUT LPSIZE);

// Syntax
// SetViewportExtEx( hDC, nXExt, nYExt ) -> aOldSize, or NIL

HB_FUNC( VWN_SETVIEWPORTEXTEX )
{
   SIZE siz ;
   PHB_ITEM aSize ;

   if ( SetViewportExtEx( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ), &siz ) )
   {
       aSize = Size2Array( &siz );
       hb_itemReturn( aSize );
       hb_itemRelease( aSize );

   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetViewportOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

// Syntax
// SetViewportOrgEx( hDC, X, Y) -> aOldOrg, or NIL on failure

HB_FUNC( VWN_SETVIEWPORTORGEX )
{
   POINT pt ;
   PHB_ITEM aPoint ;

   if ( SetViewportOrgEx( (HDC) HB_PARWH( 1 ),hb_parni( 2 ), hb_parni( 3 ), &pt ) )
   {
       aPoint = Point2Array( &pt );
       hb_itemReturn( aPoint );
       hb_itemRelease( aPoint );

   }
}
