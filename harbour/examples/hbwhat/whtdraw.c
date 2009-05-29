/*
 * $Id$
 */

// hbwhat
// Graphics  & Drawing

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

// Syntax:
// MoveTo( x, y ) -> lSuccess

HB_FUNC( VWN_MOVETO )
{

   hb_retl( MoveToEx(
                    (HDC) HB_PARWH(1),   // device context handle
                    hb_parni(2)      ,   // x-coordinate of line's ending point
                    hb_parni(3)      ,   // y-coordinate of line's ending point
                    NULL
                 ) );
}

//-----------------------------------------------------------------------------

// SYNTAX:
// MoveTo(hDC, x, y) -> aOldPoint or NIL


HB_FUNC( VWN_MOVETOEX )
{

   POINT Point ;
   PHB_ITEM aPt;


   if ( MoveToEx(
                    (HDC) HB_PARWH(1),   // device context handle
                    hb_parni(2)      ,   // x-coordinate of line's ending point
                    hb_parni(3)      ,   // y-coordinate of line's ending point
                    &Point
                  ) )

     {

     aPt = Point2Array(&Point);
     hb_itemReturn( aPt );
     hb_itemRelease( aPt );

     }

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetCurrentPositionEx( IN HDC OUT LPPOINT);

// SYNTAX:
// GetCurrentPositionEx(hDC) -> aPoint or NIL

HB_FUNC( VWN_GETCURRENTPOSITIONEX )
{
   POINT pt ;
   PHB_ITEM aPt;

   if ( GetCurrentPositionEx( (HDC) HB_PARWH( 1 ), &pt ) )
   {
       aPt = Point2Array( &pt);
       hb_itemReturn( aPt );
       hb_itemRelease( aPt );

   }
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetPixelFormat( IN HDC);

HB_FUNC( VWN_GETPIXELFORMAT )
{
   hb_retni( GetPixelFormat( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetPixelFormat(IN HDC, IN int, IN CONST PIXELFORMATDESCRIPTOR *);

// uses structure

HB_FUNC( VWN_SETPIXELFORMAT )
{
   PIXELFORMATDESCRIPTOR *pfd = (PIXELFORMATDESCRIPTOR * ) hb_parc( 3 ); //hb_param( 3, HB_IT_STRING )->item.asString.value;

   hb_retl( SetPixelFormat( (HDC) HB_PARWH( 1 )   ,
                            hb_parni( 2 )         ,
                            pfd
                            ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI DescribePixelFormat( IN HDC, IN int, IN UINT, OUT LPPIXELFORMATDESCRIPTOR);

// uses structures

// syntax:
// DescribePixelFormat(hDC,nPixelFormat,@cDesc) -> nMaxPixFormat //PIXELFORMATDESCRIPTOR structure

HB_FUNC( VWN_DESCRIBEPIXELFORMAT )
{
   PIXELFORMATDESCRIPTOR pfd ;
   UINT nBytes = sizeof(pfd);

   hb_retni( DescribePixelFormat( (HDC) HB_PARWH( 1 )    ,
                                  hb_parni( 2 )          ,
                                  nBytes                 ,
                                  &pfd
                                  ) );
    if ( ISBYREF(3) )
       hb_storclen( (char*) &pfd, sizeof(PIXELFORMATDESCRIPTOR), 3 );
    //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) pfd , sizeof( PIXELFORMATDESCRIPTOR ) );

}



//-----------------------------------------------------------------------------
// WINGDIAPI COLORREF WINAPI SetPixel(IN HDC, IN int, IN int, IN COLORREF);

HB_FUNC( VWN_SETPIXEL )
{

   hb_retnl( (ULONG) SetPixel( (HDC) HB_PARWH( 1 ),
                               hb_parni( 2 )      ,
                               hb_parni( 3 )      ,
                               (COLORREF) hb_parnl( 4 )
                             ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI COLORREF WINAPI GetPixel( IN HDC, IN int, IN int);


HB_FUNC( VWN_GETPIXEL )
{
   hb_retnl( (ULONG) GetPixel( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetPixelV(IN HDC, IN int, IN int, IN COLORREF);

HB_FUNC( VWN_SETPIXELV )
{

   hb_retl( SetPixelV( (HDC) HB_PARWH( 1 ),
                       hb_parni( 2 )      ,
                       hb_parni( 3 )      ,
                       (COLORREF) hb_parnl( 4 )
                       ) );
}



//////////////////////////
//  Lines and Curves
/////////////////////////


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI LineTo( IN HDC, IN int, IN int);


HB_FUNC( VWN_LINETO )
{
   hb_retl( LineTo( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI LineDDA( IN int, IN int, IN int, IN int, IN LINEDDAPROC, IN LPARAM);

/*

   TBD : Do callback functions first

HB_FUNC( VWN_LINEDDA )
{
   LINEDDAPROC LineddaProc ;
   LPARAM      lParam      ;

   // Your code goes here

   hb_retl( LineDDA( hb_parni( 1 ),
                     hb_parni( 2 ),
                     hb_parni( 3 ),
                     hb_parni( 4 ),
                     LineddaProc  ,
                     lParam
                     ) );
}

*/



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetArcDirection(IN HDC);


HB_FUNC( VWN_GETARCDIRECTION )
{
   hb_retni( GetArcDirection( (HDC) HB_PARWH( 1 ) ) );
}




//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetArcDirection(IN HDC, IN int);


HB_FUNC( VWN_SETARCDIRECTION )
{
   hb_retni( SetArcDirection( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Arc( IN HDC, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_ARC )
{
   hb_retl( Arc( (HDC) HB_PARWH( 1 ),
                 hb_parni( 2 )      ,
                 hb_parni( 3 )      ,
                 hb_parni( 4 )      ,
                 hb_parni( 5 )      ,
                 hb_parni( 6 )      ,
                 hb_parni( 7 )      ,
                 hb_parni( 8 )      ,
                 hb_parni( 9 )
                 ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI ArcTo( IN HDC, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN int);

// NT ??

/*

HB_FUNC( VWN_ARCTO )
{
   hb_retl( ArcTo( (HDC) HB_PARWH( 1 ),
                 hb_parni( 2 )      ,
                 hb_parni( 3 )      ,
                 hb_parni( 4 )      ,
                 hb_parni( 5 )      ,
                 hb_parni( 6 )      ,
                 hb_parni( 7 )      ,
                 hb_parni( 8 )      ,
                 hb_parni( 9 )
                 ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Polyline( IN HDC, IN CONST POINT *, IN int);

// Syntax:
// PolyLine(hDC,aPoints) -> lSuccess

HB_FUNC( VWN_POLYLINE )
{

   POINT * Point ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (DWORD) hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
               hb_retl(0);
               hb_xfree(Point);
               hb_itemRelease( aSub );
               return ;
           }
       }

       hb_retl( Polyline( (HDC) HB_PARWH( 1 ), Point, iCount ) );
       hb_xfree(Point);

   }
   else
     hb_retl( 0 );

}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolylineTo( IN HDC, IN CONST POINT *, IN DWORD);

// SYNTAX:
// PolyLineTo(hDC,aPoints)->lSuccess

HB_FUNC( VWN_POLYLINETO )
{
   POINT * Point ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (DWORD) hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
            hb_retl(0);
            hb_xfree(Point);
            hb_itemRelease( aSub );
            return ;
          }
       }

       hb_retl( PolylineTo( (HDC) HB_PARWH( 1 ), Point, iCount ) );
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolyPolyline( IN HDC, IN CONST POINT *, IN CONST DWORD *, IN DWORD);

// SYNTAX:
// PolyPolyLine(hDC,aPoints,aQtyPoints)->lSuccess

HB_FUNC( VWN_POLYPOLYLINE )
{
   POINT * Point ;
   DWORD * PolyPoints ;
   DWORD iPolyCount ;
   DWORD iCount ;
   POINT pt ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) && ISARRAY( 3 ) )
   {
       iPolyCount = hb_parinfa(3,0);
       PolyPoints = (DWORD *) hb_xgrab( iPolyCount * sizeof( DWORD ) );

       for ( i=0 ; i < iPolyCount ; i++ )
       {
          *(PolyPoints+i) = hb_parnl( 3,i+1);
       }

       iCount = hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );

          }
          else
          {
            hb_retl(0);
            hb_xfree(PolyPoints);
            hb_xfree(Point);
            hb_itemRelease( aSub );
            return ;
          }
       }

       hb_retl( PolyPolyline( (HDC) HB_PARWH( 1 ), Point, PolyPoints, iPolyCount ) );
       hb_xfree(PolyPoints);
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );

}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolyDraw(IN HDC, IN CONST POINT *, IN CONST BYTE *, IN int);

// SYNTAX
// PolyDraw(hDC,aPoints,cBytes) - > lSuccess

// Great Function !!!
// NT ?

/*

HB_FUNC( VWN_POLYDRAW )
{
   CONST POINT ;
   CONST BYTE  ;

   POINT * Point ;
   DWORD * PolyPoints ;
   DWORD iPolyCount ;
   DWORD iCount ;
   POINT pt ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) && ISCHAR( 3 ) )
   {
       iCount = hb_parinfa( 2, 0 );
       if ( iCount == hb_parclen( 3 ) )
       {
            Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
            aParam = hb_param(2,HB_IT_ARRAY);

            for ( i = 0 ; i<iCount ; i++ )
            {
               aSub = hb_itemArrayGet( aParam, i+1 );

               if ( Array2Point(aSub, &pt ))
               {
                    *(Point+i) = pt ;
                    hb_itemRelease( aSub );
               }
               else
               {
                  hb_retl(0);
                  hb_xfree(PolyPoints);
                  hb_xfree(Point);
                  hb_itemRelease( aSub );
                  return ;
               }
            }

            hb_retl( PolyPolyline( (HDC) HB_PARWH( 1 ), Point, PolyPoints, iPolyCount ) );
            hb_retl( PolyDraw( (HDC) HB_PARWH( 1 ), Point, hb_parcx( 3 ) , iCount ) );
            hb_xfree(PolyPoints);
            hb_xfree(Point);
       }
   }
   else
      hb_retl( 0 );

}


*/



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolyBezier( IN HDC, IN CONST POINT *, IN DWORD);

// Syntax
// PolyBezier(hDC,aPoints) -> lSuccess

HB_FUNC( VWN_POLYBEZIER )
{

   POINT * Point ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (DWORD) hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
              hb_itemRelease( aSub );
          }
          else
          {
            hb_retl(0);
            hb_xfree(Point);
            hb_itemRelease( aSub );
            return ;
          }
       }

       hb_retl( PolyBezier( (HDC) HB_PARWH( 1 ), Point, iCount ) );
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolyBezierTo( IN HDC, IN CONST POINT *, IN DWORD);


// Syntax
// PolyBezierTo(hDC,aPoints) -> lSuccess


HB_FUNC( VWN_POLYBEZIERTO )
{

   POINT * Point ;
   POINT pt ;
   DWORD iCount ;
   UINT i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (DWORD) hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
            hb_retl(0);
            hb_xfree(Point);
            hb_itemRelease( aSub );
            return ;
          }
       }

       hb_retl( PolyBezierTo( (HDC) HB_PARWH( 1 ), Point, iCount ) );
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );

}




///////////////////////
//  Filled Shapes
///////////////////////




//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Rectangle(IN HDC, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_RECTANGLE )
{
   hb_retl( Rectangle( (HDC) HB_PARWH( 1 ),
                       hb_parni( 2 )      ,
                       hb_parni( 3 )      ,
                       hb_parni( 4 )      ,
                       hb_parni( 5 )
                       ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RoundRect(IN HDC, IN int, IN int, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_ROUNDRECT )
{
   hb_retl( RoundRect( (HDC) HB_PARWH( 1 ),
                       hb_parni( 2 )      ,
                       hb_parni( 3 )      ,
                       hb_parni( 4 )      ,
                       hb_parni( 5 )      ,
                       hb_parni( 6 )      ,
                       hb_parni( 7 )
                       ) );
}




//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Chord( IN HDC, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_CHORD )
{
   hb_retl( Chord( (HDC) HB_PARWH( 1 ),
                   hb_parni( 2 )      ,
                   hb_parni( 3 )      ,
                   hb_parni( 4 )      ,
                   hb_parni( 5 )      ,
                   hb_parni( 6 )      ,
                   hb_parni( 7 )      ,
                   hb_parni( 8 )      ,
                   hb_parni( 9 )
                   ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Pie(IN HDC, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_PIE )
{
   hb_retl( Pie( (HDC) HB_PARWH( 1 ),
                 hb_parni( 2 )      ,
                 hb_parni( 3 )      ,
                 hb_parni( 4 )      ,
                 hb_parni( 5 )      ,
                 hb_parni( 6 )      ,
                 hb_parni( 7 )      ,
                 hb_parni( 8 )      ,
                 hb_parni( 9 )
                 ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Ellipse( IN HDC, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_ELLIPSE )
{
   hb_retl( Ellipse( (HDC) HB_PARWH( 1 ),
                     hb_parni( 2 )      ,
                     hb_parni( 3 )      ,
                     hb_parni( 4 )      ,
                     hb_parni( 5 )
                     ) );
}





//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI Polygon( IN HDC, IN CONST POINT *, IN int);

// SYNTAX:
// Polygon(hDC,aPoints) -> lSuccess

HB_FUNC( VWN_POLYGON )
{
   POINT * Point ;
   POINT pt ;
   int iCount ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) )
   {
       iCount = (int) hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
            hb_retl(0);
            hb_xfree(Point);
            hb_itemRelease( aSub );
            return ;
          }
       }

       hb_retl( Polygon( (HDC) HB_PARWH( 1 ), Point, iCount ) );
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PolyPolygon(IN HDC, IN CONST POINT *, IN CONST INT *, IN int);

// SYNTAX
// PolyPolygon(hDC,aPoints,aQtyPoints) -> lSuccess

HB_FUNC( VWN_POLYPOLYGON )
{
   POINT * Point ;
   INT * PolyPoints ;
   int iPolyCount ;
   int iCount ;
   POINT pt ;
   int i ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;

   if (ISARRAY( 2 ) && ISARRAY( 3 ) )
   {
       iPolyCount = hb_parinfa(3,0);
       PolyPoints = ( INT *) hb_xgrab( iPolyCount * sizeof( INT ) );

       for ( i=0 ; i < iPolyCount ; i++ )
       {
          *(PolyPoints+i) = hb_parni( 3,i+1);
       }

       iCount = hb_parinfa( 2, 0 );
       Point = (POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );

          if ( Array2Point(aSub, &pt ))
          {
               *(Point+i) = pt ;
               hb_itemRelease( aSub );
          }
          else
          {
               hb_retl(0);
               hb_xfree(PolyPoints);
               hb_xfree(Point);
               hb_itemRelease( aSub );
               return ;
          }
       }

       hb_retl( PolyPolygon( (HDC) HB_PARWH( 1 ), Point, PolyPoints, iPolyCount ) );
       hb_xfree(PolyPoints);
       hb_xfree(Point);

   }
   else
    hb_retl( 0 );

}


//-----------------------------------------------------------------------------
// int FillRect( HDC hDC, CONST RECT *lprc,  HBRUSH hbr )

// SYNTAX:
// FillRect( hDC, aRect,hBrush ) -> nRet


HB_FUNC( VWN_FILLRECT )
{
  RECT rc ;

  if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
     hb_retni( FillRect((HDC) HB_PARWH( 1 ), &rc, (HBRUSH) HB_PARWH( 3 ) ) );
  else
     hb_retni(0);

}



//-----------------------------------------------------------------------------
// int FrameRect( HDC hDC, CONST RECT *lprc,  HBRUSH hbr )

// SYNTAX:
// FrameRect( hDC, aRect,hBrush ) -> nRet


HB_FUNC( VWN_FRAMERECT )
{
   RECT rc ;

   if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
      hb_retni( FrameRect((HDC) HB_PARWH( 1 ), &rc, (HBRUSH) HB_PARWH( 3 ) ) );
   else
      hb_retni(0);
}

//-----------------------------------------------------------------------------
// BOOL InvertRect( HDC hDC, CONST RECT *lprc,  HBRUSH hbr )

// SYNTAX:
// InvertRect( hDC, aRect ) -> lSuccess


HB_FUNC( VWN_INVERTRECT )
{
   RECT rc ;

   if ( ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
      hb_retni( InvertRect((HDC) HB_PARWH( 1 ), &rc ) );
   else
      hb_retni(0);
}

/////////////////////////
//  Filling
/////////////////////////



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetPolyFillMode(IN HDC, IN int);


HB_FUNC( VWN_SETPOLYFILLMODE )
{
   hb_retni( SetPolyFillMode( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI ExtFloodFill( IN HDC, IN int, IN int, IN COLORREF, IN UINT);


HB_FUNC( VWN_EXTFLOODFILL )
{

   hb_retl( ExtFloodFill( (HDC) HB_PARWH( 1 ) ,
                          hb_parni( 2 )       ,
                          hb_parni( 3 )       ,
                          (COLORREF) hb_parnl( 4 ) ,
                          (UINT) hb_parni( 5 )
                          ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FillPath(IN HDC);


HB_FUNC( VWN_FILLPATH )
{
   hb_retl( FillPath( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FlattenPath(IN HDC);


HB_FUNC( VWN_FLATTENPATH )
{
   hb_retl( FlattenPath( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI FloodFill( IN HDC, IN int, IN int, IN COLORREF);

HB_FUNC( VWN_FLOODFILL )
{

   hb_retl( FloodFill( (HDC) HB_PARWH( 1 ),
                       hb_parni( 2 )      ,
                       hb_parni( 3 )      ,
                       (COLORREF) hb_parnl( 4 )
                       ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetPolyFillMode( IN HDC);


HB_FUNC( VWN_GETPOLYFILLMODE )
{
   hb_retni( GetPolyFillMode( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GradientFill( IN HDC, IN PTRIVERTEX, IN ULONG, IN PVOID, IN ULONG, IN ULONG);

/*

 // use arrays !

HB_FUNC( VWN_GRADIENTFILL )
{
   TRIVERTEX * tve = (TRIVERTEX *) hb_parc( 2 );
   PVOID      pVoid      ;

   // Your code goes here

   hb_retl( GradientFill( (HDC) HB_PARWH( 1 )  ,
                          tve                  ,
                          (ULONG) hb_parnl( 3 ),
                          pVoid                ,
                          (ULONG) hb_parnl( 5 ),
                          (ULONG) hb_parnl( 6 )
                          ) );
}


*/
