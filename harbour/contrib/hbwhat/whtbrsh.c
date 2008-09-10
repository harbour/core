/*
 * $Id$
 */

// hbwhat
// Brush functions

/*

   GetBrushOrgEx(hDC) -> aPt, or NIL on failure
   SetBrushOrgEx(hDC,x,y,) -> aOldPt, or NIL on failure

*/

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>

#include "hbapiitm.h"
#include "hbapi.h"


//-----------------------------------------------------------------------------

HB_FUNC( CREATESOLIDBRUSH )
{
   HB_RETWH( CreateSolidBrush( (COLORREF) hb_parnl( 1 ) ) );    // brush color
}


//-----------------------------------------------------------------------------

HB_FUNC( CREATEPATTERNBRUSH )
{
   HB_RETWH( CreatePatternBrush((HBITMAP) HB_PARWH( 1 ) ) );    // bitmap handle
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrush( IN HGLOBAL, IN UINT);

HB_FUNC( CREATEDIBPATTERNBRUSH )
{
   HB_RETWH( CreateDIBPatternBrush( (HGLOBAL) HB_PARWH( 1 ),
                                           (UINT) hb_parni( 2 )
                                           ) );
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateBrushIndirect( IN CONST LOGBRUSH *);

// uses structure


HB_FUNC( CREATEBRUSHINDIRECT )
{
   //PHB_ITEM br = hb_param( 1,HB_IT_STRING );
   //LOGBRUSH *lb = (LOGBRUSH * ) br->item.asString.value;
   LOGBRUSH *lb = (LOGBRUSH * ) HB_PARWH( 1 );

   HB_RETWH( CreateBrushIndirect( lb ) );
}

//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateHatchBrush( IN int, IN COLORREF);

HB_FUNC( CREATEHATCHBRUSH )
{

   HB_RETWH( CreateHatchBrush( hb_parni( 1 ), (COLORREF) hb_parnl(2) ) );
}


//-----------------------------------------------------------------------------

// no prototype ?

/*

// WINGDIAPI COLORREF WINAPI GetDCBrushColor( IN HDC);

HB_FUNC( GETDCBRUSHCOLOR )
{
   hb_retnl( (ULONG) GetDCBrushColor( (HDC) HB_PARWH( 1 ) ) );
}

*/

//-----------------------------------------------------------------------------

// no prototype ?

/*
// WINGDIAPI COLORREF WINAPI SetDCBrushColor(IN HDC, IN COLORREF);

HB_FUNC( SETDCBRUSHCOLOR )
{

   hb_retnl( (ULONG) SetDCBrushColor( (HDC) HB_PARWH( 1 ), (COLORREF) hb_parnl( 2 ) ) );
}
*/
//-----------------------------------------------------------------------------

// WINGDIAPI HBRUSH WINAPI CreateDIBPatternBrushPt( IN CONST VOID *, IN UINT);

// uses structure

HB_FUNC( CREATEDIBPATTERNBRUSHPT )
{
   BITMAPINFO *bmi = (BITMAPINFO *) hb_parc( 1 );//hb_param( 1,HB_IT_STRING )->item.asString.value;

   HB_RETWH( CreateDIBPatternBrushPt( bmi, (UINT) hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FixBrushOrgEx( IN HDC, IN int, IN int, IN LPPOINT);

// obsolete - not implemented

HB_FUNC( FIXBRUSHORGEX )
{
   POINT *Point = (POINT *) hb_parc( 4 );//hb_param( 4,HB_IT_STRING )->item.asString.value;

   hb_retl( FixBrushOrgEx( (HDC) HB_PARWH( 1 ),
                           hb_parni( 2 )      ,
                           hb_parni( 3 )      ,
                           Point
                           ) );
}


//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI GetBrushOrgEx( IN HDC, OUT LPPOINT);

HB_FUNC( GETBRUSHORGEX )
{
   POINT Point;

   if( GetBrushOrgEx( ( HDC ) HB_PARWH( 1 ), &Point ) )
   {
      PHB_ITEM aPt = hb_itemArrayNew( 2 );

      hb_arraySetNL( aPt, 1, Point.x );
      hb_arraySetNL( aPt, 2, Point.y );

      hb_itemReturnRelease( aPt );
   }
}




//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI SetBrushOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

HB_FUNC( SETBRUSHORGEX )
{

   POINT Point;

   if( SetBrushOrgEx( ( HDC ) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ), &Point ) )
   {
      PHB_ITEM aPt = hb_itemArrayNew( 2 );

      hb_arraySetNL( aPt, 1, Point.x );
      hb_arraySetNL( aPt, 2, Point.y );

      hb_itemReturnRelease( aPt );
   }
}
