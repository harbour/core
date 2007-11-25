/*
 * $Id$
 */

// WHAT32
// Brush functions

/*

   GetBrushOrgEx(hDC) -> aPt, or NIL on failure
   SetBrushOrgEx(hDC,x,y,) -> aOldPt, or NIL on failure

*/

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include <windows.h>
#include "item.api"
#include "hbapi.h"


//-----------------------------------------------------------------------------

HB_FUNC( CREATESOLIDBRUSH )
{
   hb_retnl( (LONG) CreateSolidBrush( (COLORREF) hb_parnl( 1 ) ) ) ;    // brush color
}


//-----------------------------------------------------------------------------

HB_FUNC( CREATEPATTERNBRUSH )
{
   hb_retnl( (LONG) CreatePatternBrush((HBITMAP) hb_parnl( 1 ) ) ) ;    // bitmap handle
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrush( IN HGLOBAL, IN UINT);

HB_FUNC( CREATEDIBPATTERNBRUSH )
{
   hb_retnl( (LONG) CreateDIBPatternBrush( (HGLOBAL) hb_parnl( 1 ),
                                           (UINT) hb_parni( 2 )
                                           ) ) ;
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateBrushIndirect( IN CONST LOGBRUSH *);

// uses structure


HB_FUNC( CREATEBRUSHINDIRECT )
{
   //PHB_ITEM br = hb_param( 1,HB_IT_STRING ) ;
   //LOGBRUSH *lb = (LOGBRUSH * ) br->item.asString.value;
   LOGBRUSH *lb = (LOGBRUSH * ) hb_parnl( 1 );

   hb_retnl( (LONG) CreateBrushIndirect( lb ) ) ;
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateHatchBrush( IN int, IN COLORREF);

HB_FUNC( CREATEHATCHBRUSH )
{

   hb_retnl( (LONG) CreateHatchBrush( hb_parni( 1 ), (COLORREF) hb_parnl(2) ) ) ;
}


//-----------------------------------------------------------------------------

// no prototype ?

/*

// WINGDIAPI COLORREF WINAPI GetDCBrushColor( IN HDC);

HB_FUNC( GETDCBRUSHCOLOR )
{
   hb_retnl( (ULONG) GetDCBrushColor( (HDC) hb_parnl( 1 ) ) ) ;
}

*/

//-----------------------------------------------------------------------------

// no prototype ?

/*
// WINGDIAPI COLORREF WINAPI SetDCBrushColor(IN HDC, IN COLORREF);

HB_FUNC( SETDCBRUSHCOLOR )
{

   hb_retnl( (ULONG) SetDCBrushColor( (HDC) hb_parnl( 1 ), (COLORREF) hb_parnl( 2 ) ) ) ;
}
*/
//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrushPt( IN CONST VOID *, IN UINT);

// uses structure

HB_FUNC( CREATEDIBPATTERNBRUSHPT )
{
   BITMAPINFO *bmi = (BITMAPINFO *) hb_parc( 1 );//hb_param( 1,HB_IT_STRING )->item.asString.value;

   hb_retnl( (LONG) CreateDIBPatternBrushPt( bmi, (UINT) hb_parni( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FixBrushOrgEx( IN HDC, IN int, IN int, IN LPPOINT);

// obsolete - not implemented

HB_FUNC( FIXBRUSHORGEX )
{
   POINT *Point = (POINT *) hb_parc( 4 );//hb_param( 4,HB_IT_STRING )->item.asString.value;

   hb_retl( FixBrushOrgEx( (HDC) hb_parnl( 1 ),
                           hb_parni( 2 )      ,
                           hb_parni( 3 )      ,
                           Point
                           ) ) ;
}


//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI GetBrushOrgEx( IN HDC, OUT LPPOINT);

HB_FUNC( GETBRUSHORGEX )
{
   POINT Point ;
   PHB_ITEM aPt;
   PHB_ITEM temp ;

   if ( GetBrushOrgEx( (HDC) hb_parnl( 1 ), &Point ) )
   {
     aPt = _itemArrayNew( 2 );

     temp = _itemPutNL( NULL, Point.x );
     hb_arraySet( aPt, 1, temp );
     _itemRelease( temp );

     temp = _itemPutNL( NULL, Point.y );
     hb_arraySet( aPt, 2, temp );
     _itemRelease( temp );

     _itemReturn( aPt );
     _itemRelease( aPt );
   }
}




//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI SetBrushOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

HB_FUNC( SETBRUSHORGEX )
{

   POINT Point ;
   PHB_ITEM aPt;
   PHB_ITEM temp ;

   if ( SetBrushOrgEx( (HDC) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ), &Point) )
   {
     aPt = _itemArrayNew( 2 );

     temp = _itemPutNL( NULL, Point.x );
     hb_arraySet( aPt, 1, temp );
     _itemRelease( temp );

     temp = _itemPutNL( NULL, Point.y );
     hb_arraySet( aPt, 2, temp );
     _itemRelease( temp );

     _itemReturn( aPt );
     _itemRelease( aPt );

   }

}



