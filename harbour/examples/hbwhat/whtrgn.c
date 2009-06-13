/*
 * $Id$
 */


// hbwhat
// Region functions



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
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreateEllipticRgn( IN int, IN int, IN int, IN int);


HB_FUNC( VWN_CREATEELLIPTICRGN )
{
   HB_RETWH( CreateEllipticRgn( hb_parni( 1 ),
                                       hb_parni( 2 ),
                                       hb_parni( 3 ),
                                       hb_parni( 4 )
                                       ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreateEllipticRgnIndirect( IN CONST RECT *);

// Syntax
// CreateEllipticRgnIndirect( aRect ) -> hRgn, or 0

HB_FUNC( VWN_CREATEELLIPTICRGNINDIRECT )
{
   RECT rc;

   if (ISARRAY( 1 ) && Array2Rect( hb_param( 1, HB_IT_ARRAY ), &rc ) )
      HB_RETWH( CreateEllipticRgnIndirect( &rc ) );
   else
      hb_retnl( 0 );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FillRgn( IN HDC, IN HRGN, IN HBRUSH);


HB_FUNC( VWN_FILLRGN )
{
   hb_retl( FillRgn( (HDC) HB_PARWH( 1 )   ,
                     (HRGN) HB_PARWH( 2 )  ,
                     (HBRUSH) HB_PARWH( 3 )
                     ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreatePolygonRgn( IN CONST POINT *, IN int, IN int);

// Syntax
// CreatePolygonRgn( aPoints, nPolyFillMode ) -> hRgn , or 0

HB_FUNC( VWN_CREATEPOLYGONRGN )
{
   POINT * Point ;
   POINT pt ;
   int iCount ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 1 ) )
   {
       iCount = (int) hb_parinfa( 1, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(1,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
               *(Point+i) = pt ;
          else {
            hb_retnl(0);
            hb_xfree(Point);
            return ;
          }
       }

       HB_RETWH( CreatePolygonRgn( Point, iCount, hb_parni( 2 ) ) );
       hb_xfree(Point);

   }
   else
    hb_retnl( 0 );

}

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreatePolyPolygonRgn( IN CONST POINT *, IN CONST INT *, IN int, IN int);

// Syntax
// CreatePolyPolygonRgn( aPoints, aQtyPoints, nFillMode ) -> hRgn, or 0

HB_FUNC( VWN_CREATEPOLYPOLYGONRGN )
{
   POINT * Point ;
   INT * PolyPoints ;
   int iPolyCount ;
   int iCount ;
   POINT pt ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 1 ) && HB_ISARRAY( 2 ) )
   {
       iPolyCount = hb_parinfa(2,0);
       PolyPoints = ( INT *) hb_xgrab( iPolyCount * sizeof( INT ) );

       for ( i=0 ; i < iPolyCount ; i++ )
       {
          *(PolyPoints+i) = hb_parni( 2,i+1);
       }

       iCount = hb_parinfa( 1, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(1,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
               *(Point+i) = pt ;
          else {
            hb_retnl(0);
            hb_xfree(PolyPoints);
            hb_xfree(Point);
            return ;
          }
       }

       HB_RETWH( CreatePolyPolygonRgn( Point, PolyPoints, iPolyCount, hb_parni( 3 ) ) );
       hb_xfree(PolyPoints);
       hb_xfree(Point);

   }
   else
    hb_retnl( 0 );

}


//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreateRectRgn( IN int, IN int, IN int, IN int);

// Syntax
// CreateRectRgn(x1,y1,x2,y2) ->hRgn


HB_FUNC( VWN_CREATERECTRGN )
{
   HB_RETWH( CreateRectRgn( hb_parni( 1 ),
                                   hb_parni( 2 ),
                                   hb_parni( 3 ),
                                   hb_parni( 4 )
                                   ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreateRectRgnIndirect( IN CONST RECT *);

// Syntax
// CreateRectRgnIndirect( aRect) ->hRgn, or 0

HB_FUNC( VWN_CREATERECTRGNINDIRECT )
{

   RECT rc;

   if (ISARRAY( 1 ) && Array2Rect( hb_param( 1, HB_IT_ARRAY ), &rc ) )
      HB_RETWH( CreateRectRgnIndirect( &rc ) );
   else
      hb_retnl( 0 );
}



//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI CreateRoundRectRgn( IN int, IN int, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_CREATEROUNDRECTRGN )
{
   HB_RETWH( CreateRoundRectRgn( hb_parni( 1 ),
                                        hb_parni( 2 ),
                                        hb_parni( 3 ),
                                        hb_parni( 4 ),
                                        hb_parni( 5 ),
                                        hb_parni( 6 )
                                        ) );
}



//-----------------------------------------------------------------------------
// int CombineRgn( HRGN hrgnDest, HRGN hrgnSrc1,HRGN hrgnSrc2, int fnCombineMode)

// Syntax:
// CombineRgn(hrgnDest,hrgnSrc1, hrgnSrc2, fnCombineMode ) -> nRgnType

HB_FUNC( VWN_COMBINERGN )
{
    hb_retni( CombineRgn( (HRGN) HB_PARWH(1), (HRGN) HB_PARWH(2),
                          (HRGN) HB_PARWH(3), hb_parni(4) ) );

}


//-----------------------------------------------------------------------------

// WINGDIAPI DWORD WINAPI GetRegionData( IN HRGN, IN DWORD, OUT LPRGNDATA);

// Syntax
// GetRegionData(hRgn) -> cData, or NIL

HB_FUNC( VWN_GETREGIONDATA )
{
   RGNDATA *RgnData ;
   DWORD nBytes = GetRegionData( (HRGN) HB_PARWH( 1 ) , 0, NULL );
   DWORD nRet ;

   if ( nBytes )
   {
     RgnData = (RGNDATA *) hb_xgrab( nBytes );
     nRet = GetRegionData( (HRGN) HB_PARWH( 1 ) ,
                             nBytes             ,
                             RgnData
                         );
     if ( nRet == 1 )
         hb_retclen( ( char *) RgnData,nBytes );

     hb_xfree( RgnData);
   }

}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI ExtSelectClipRgn(IN HDC, IN HRGN, IN int);


HB_FUNC( VWN_EXTSELECTCLIPRGN )
{
   hb_retni( ExtSelectClipRgn( (HDC) HB_PARWH( 1 ) ,
                               (HRGN) HB_PARWH( 2 ),
                               hb_parni( 3 )
                               ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RectInRegion(IN HRGN, IN CONST RECT *);

// Syntax
// RectInRegion( hRgn, aRect ) -> lInRgn , or NIL on failure

HB_FUNC( VWN_RECTINREGION )
{
    RECT rc;

   if (ISARRAY( 2 ) && Array2Rect( hb_param( 2, HB_IT_ARRAY ), &rc ) )
      hb_retl( RectInRegion( (HRGN) HB_PARWH( 1 ), &rc ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetRandomRgn (IN HDC, IN HRGN, IN INT);


HB_FUNC( VWN_GETRANDOMRGN )
{
   hb_retni( GetRandomRgn( (HDC) HB_PARWH( 1 ) ,
                           (HRGN) HB_PARWH( 2 ),
                           hb_parni( 3 )
                           ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetClipRgn( IN HDC, IN HRGN);


HB_FUNC( VWN_GETCLIPRGN )
{
   hb_retni( GetClipRgn( (HDC) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FrameRgn( IN HDC, IN HRGN, IN HBRUSH, IN int, IN int);

HB_FUNC( VWN_FRAMERGN )
{
   hb_retl( FrameRgn( (HDC) HB_PARWH( 1 )   ,
                      (HRGN) HB_PARWH( 2 )  ,
                      (HBRUSH) HB_PARWH( 3 ),
                      hb_parni( 4 )         ,
                      hb_parni( 5 )
                      ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI EqualRgn( IN HRGN, IN HRGN);


HB_FUNC( VWN_EQUALRGN )
{
   hb_retl( EqualRgn( (HRGN) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}

/*
//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ValidateRect( IN HWND hWnd, IN CONST RECT *lpRect);

// Syntax
// ValidateRect( hWnd, aRect ) -> lSuccess

HB_FUNC( VWN_VALIDATERECT )
{
   RECT rc ;

   if (ISARRAY( 2 ) && Array2Rect( hb_param( 2, HB_IT_ARRAY ), &rc ) )
      hb_retl( ValidateRect( (HWND) HB_PARWH( 1 ), &rc ) );
   else
      hb_retl( ValidateRect( (HWND) HB_PARWH( 1 ), NULL) );

}
*/


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI InvalidateRgn( IN HWND hWnd, IN HRGN hRgn, IN BOOL bErase);


HB_FUNC( VWN_INVALIDATERGN )
{
   hb_retl( InvalidateRgn( (HWND) HB_PARWH( 1 ),
                           (HRGN) HB_PARWH( 2 ),
                           hb_parl( 3 )
                         ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ValidateRgn( IN HWND hWnd, IN HRGN hRgn);


HB_FUNC( VWN_VALIDATERGN )
{
   hb_retl( ValidateRgn( (HWND) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI InvertRgn( IN HDC, IN HRGN);


HB_FUNC( VWN_INVERTRGN )
{
   hb_retl( InvertRgn( (HDC) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI OffsetClipRgn(IN HDC, IN int, IN int);


HB_FUNC( VWN_OFFSETCLIPRGN )
{
   hb_retni( OffsetClipRgn( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI OffsetRgn(IN HRGN, IN int, IN int);


HB_FUNC( VWN_OFFSETRGN )
{
   hb_retni( OffsetRgn( (HRGN) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PaintRgn(IN HDC, IN HRGN);


HB_FUNC( VWN_PAINTRGN )
{
   hb_retl( PaintRgn( (HDC) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI PathToRegion(IN HDC);


HB_FUNC( VWN_PATHTOREGION )
{
   HB_RETWH( PathToRegion( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PtInRegion(IN HRGN, IN int, IN int);


HB_FUNC( VWN_PTINREGION )
{
   hb_retl( PtInRegion( (HRGN) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SelectClipRgn(IN HDC, IN HRGN);


HB_FUNC( VWN_SELECTCLIPRGN )
{
   hb_retni( SelectClipRgn( (HDC) HB_PARWH( 1 ), (HRGN) HB_PARWH( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetRectRgn(IN HRGN, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_SETRECTRGN )
{
   hb_retl( SetRectRgn( (HRGN) HB_PARWH( 1 ),
                        hb_parni( 2 )       ,
                        hb_parni( 3 )       ,
                        hb_parni( 4 )       ,
                        hb_parni( 5 )
                        ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetUpdateRgn( IN HWND hWnd, IN HRGN hRgn, IN BOOL bErase);


HB_FUNC( VWN_GETUPDATERGN )
{
   hb_retni( GetUpdateRgn( (HWND) HB_PARWH( 1 ),
                           (HRGN) HB_PARWH( 2 ),
                           hb_parl( 3 )
                         ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI ExcludeUpdateRgn( IN HDC hDC, IN HWND hWnd);


HB_FUNC( VWN_EXCLUDEUPDATERGN )
{
   hb_retni( ExcludeUpdateRgn( (HDC) HB_PARWH( 1 ), (HWND) HB_PARWH( 2 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetRgnBox( IN HRGN, OUT LPRECT);


// Syntax
// GetRgnBox(hRgn,@aRect) -> nType, or NIL


/*

HB_FUNC( VWN_GETRGNBOX )
{
   RECT *rc ;


   hb_retni( GetRgnBox( (HRGN) HB_PARWH( 1 ), lpRect ) );

}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI HRGN WINAPI ExtCreateRegion( IN CONST XFORM *, IN DWORD, IN CONST RGNDATA *);

/*

HB_FUNC( VWN_EXTCREATEREGION )
{
   CONST XFORM   ;
   CONST RGNDATA ;

   // Your code goes here

   HB_RETWH( ExtCreateRegion( &XFORM, (DWORD) hb_parnl( 2 ), &&RGNDATA ) );
}

*/
