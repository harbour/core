/*
 * $Id$
 */

// hbwhat
// rectangle functions

// Thanks Luiz

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

//#include <shlobj.h>
#include <windows.h>
#include "hbapiitm.h"
//#include "hbapiitm.h"
#include "hbapi.h"
//#include "hbvm.h"
//#include "hbstack.h"

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
// WINUSERAPI BOOL WINAPI DrawFocusRect( IN HDC hDC, IN CONST RECT * lprc);

/*Call as
Local aSrc := { 1 , 11 , 55 , 62 }
DRAWFOCUSRECT(hDc,aSrc)
*/

HB_FUNC( VWN_DRAWFOCUSRECT )
{
   RECT lprc ;

   if (ISARRAY(2) && Array2Rect( hb_param( 2 ,HB_IT_ARRAY ) , &lprc ) )
      hb_retl( DrawFocusRect( (HDC) HB_PARWH( 1 ), &lprc ) );
   else
      hb_retl(FALSE);
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI IntersectClipRect( IN HDC, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_INTERSECTCLIPRECT )
{
   hb_retni( IntersectClipRect( (HDC) HB_PARWH( 1 ),
                                hb_parni( 2 )      ,
                                hb_parni( 3 )      ,
                                hb_parni( 4 )      ,
                                hb_parni( 5 )
                                ) );
}

//-----------------------------------------------------------------------------
//WINUSERAPI int WINAPI FillRect( IN HDC hDC, IN CONST RECT *lprc, IN HBRUSH hbr);
//SYNTAX FILLRect(nHdc,aRect,hBrush) -> nil

// ok
/*
HB_FUNC( VWN_FILLRECT )
{
   RECT rc;

   if (Array2Rect( hb_param( 2 , HB_IT_ARRAY) , &rc) )
      hb_retni( FillRect(
       (HDC) HB_PARWH( 1 ),   // handle to device context
       &rc, // pointer to structure with rectangle
       (HBRUSH) HB_PARWH( 3 )    // handle to brush
   ) );
}
*/



//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI FillRect( IN HDC hDC, IN CONST RECT *lprc, IN HBRUSH hbr);

/* Call as
   Local aSrc := { 11 , 25 , 32 , 18 }
   FILLRECT(nDC,aSrc,hbr)
*/

// rewritten in _WinDraw.c

/*

HB_FUNC( VWN_FILLRECT )
{
   RECT   lprc ;
   PHB_ITEM pSrc1;

    if (Array2Rect( hb_param( 2 , HB_IT_ARRAY) , &rc) )
      hb_retni( FillRect( (HDC) HB_PARWH( 1 ), &lprc, (HBRUSH) HB_PARWH( 3 ) ) );
   }
   else
    hb_retni(0);
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI FrameRect( IN HDC hDC, IN CONST RECT *lprc, IN HBRUSH hbr);

/* Call as
   Local aSrc := { 11 , 25 , 32 , 18 }
   FRAMERECT(nDC,aSrc,hbr)
*/

// re written in _WinDraw.c

/*
HB_FUNC( VWN_FRAMERECT )
{
   RECT   lprc ;

   PHB_ITEM pSrc1;

 if (Array2Rect( hb_param( 2 , HB_IT_ARRAY) , &lprc) )
      hb_retni( FrameRect( (HDC) HB_PARWH( 1 ), &lprc, (HBRUSH) HB_PARWH( 3 ) ) );
   }
   else
      hb_retni( 0 );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI InvertRect( IN HDC hDC, IN CONST RECT *lprc);

/* Call as
   Local aSrc := { 11 , 25 , 32 , 18 }
   INVERTRECT(nDC,aSrc,hbr)
*/

// rewritten in _WinDraw.c

/*
HB_FUNC( VWN_INVERTRECT )
{
   RECT lprc ;
   PHB_ITEM pSrc1;

 if (Array2Rect( hb_param( 2 , HB_IT_ARRAY) , &lprc) )
    hb_retl( InvertRect( (HDC) HB_PARWH( 1 ), &lprc ) );
   }
   else
      hb_retl(FALSE);
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetRect( OUT LPRECT lprc, IN int xLeft, IN int yTop, IN int xRight, IN int yBottom);

/* Call as
   Local aSrc
aSrc :=  SETRECT(2 ,3,41,60)
*/


HB_FUNC( VWN_SETRECT )
{
   RECT lprc    ;

   if ( SetRect( &lprc         ,
                     hb_parni( 1 ),
                     hb_parni( 2 ),
                     hb_parni( 3 ),
                     hb_parni( 4 )
                   ) ) {
      hb_itemRelease(hb_itemReturn(Rect2Array( &lprc)));
   }
   else
      hb_ret( );

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetRectEmpty( OUT LPRECT lprc);

/* Call as
   Local aSrc
   aSrc :=SETRECTEMPTY(aSrc)
*/


HB_FUNC( VWN_SETRECTEMPTY )
{
   RECT lprc ;
   if( SetRectEmpty( &lprc ) ) {
       hb_itemRelease(hb_itemReturn(Rect2Array( &lprc)));
   }
   else
      hb_ret();

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI CopyRect( OUT LPRECT lprcDst, IN CONST RECT *lprcSrc);

/*Call as folow
Local aRet
Local aSrc:={10,12,14,10}
aret:= COPYRECT(aSrc)
*/


HB_FUNC( VWN_COPYRECT )
{
   RECT   lprcDst ;
   RECT   lprcSrc ;
   if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc ))
      {
      if ( CopyRect( &lprcDst, &lprcSrc ) ){
          hb_itemRelease(hb_itemReturn(Rect2Array( &lprcDst)));

      }
      else
         hb_ret();
      }
   else
     hb_ret();

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI InflateRect( IN OUT LPRECT lprc, IN int dx, IN int dy);
/*
Call as
Local aSrc:={1,2,3,4}
INFLATERECT(@aSrc,12,23)
*/

HB_FUNC( VWN_INFLATERECT )
{
   RECT lprc ;
   PHB_ITEM pArray=hb_param( 1, HB_IT_ARRAY );

   if ( Array2Rect( pArray , &lprc ))
      {
      if ( InflateRect( &lprc, hb_parni( 2 ), hb_parni( 3 ) ) )
{
         Rect2ArrayEx( &lprc,pArray );
         hb_retl( TRUE );
      }
      else
         hb_retl(FALSE);
   }
   else
      hb_retl(FALSE);

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IntersectRect( OUT LPRECT lprcDst, IN CONST RECT *lprcSrc1, IN CONST RECT *lprcSrc2);

/* Call as
Local aDest
local aSrc1 := { 1 , 5 , 4 , 6 }
Local asrc2 := { 10 , 50 , 24 , 66 }
aDest := INTERSECTRECT(,aSrc1,aSrc2)
*/

HB_FUNC( VWN_INTERSECTRECT )
{
   RECT   lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;

   if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (IntersectRect( &lprcDst, &lprcSrc1, &lprcSrc2 ) )
        hb_itemRelease(hb_itemReturn(Rect2Array( &lprcDst)));
     else
        hb_ret();

   }
   else
      hb_ret();

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI UnionRect( OUT LPRECT lprcDst, IN CONST RECT *lprcSrc1, IN CONST RECT *lprcSrc2);

/* Call as
Local  arect
local aSrc1 := { 1 , 5 , 4 , 6 }
Local asrc2 := { 10 , 50 , 24 , 66 }
aDest:=UnionRect(aSrc1,aSrc2)
*/


HB_FUNC( VWN_UNIONRECT )
{
   RECT lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;


  if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (UnionRect( &lprcDst, &lprcSrc1, &lprcSrc2 ) )
         {
         hb_itemRelease(hb_itemReturn(Rect2Array( &lprcDst)));
      }
      else
         hb_ret();
   }
   else
      hb_ret();
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SubtractRect( OUT LPRECT lprcDst, IN CONST RECT *lprcSrc1, IN CONST RECT *lprcSrc2);

/* Call as
Local aDest
local aSrc1 := { 1 , 5 , 4 , 6 }
Local asrc2 := { 10 , 50 , 24 , 66 }
aDest := SUBTRACTRECT(aSrc1,aSrc2)
*/


HB_FUNC( VWN_SUBTRACTRECT )
{
   RECT lprcDst  ;
   RECT   lprcSrc1 ;
   RECT   lprcSrc2 ;

  if ( Array2Rect(hb_param( 1, HB_IT_ARRAY ) , &lprcSrc1 )  && Array2Rect(hb_param( 2, HB_IT_ARRAY ) , &lprcSrc2 ))
   {
      if (SubtractRect( &lprcDst, &lprcSrc1, &lprcSrc2 ))
      {
         hb_itemRelease(hb_itemReturn(Rect2Array(&lprcDst)));

      }
      else
         hb_ret();
   }
   else
      hb_ret();
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI OffsetRect( IN OUT LPRECT lprc, IN int dx, IN int dy);

/* Call as
local aSrc1 := { 1 , 5 , 4 , 6 }
OFFSETRECT(@aSrc1,20,30)
*/


HB_FUNC( VWN_OFFSETRECT )
{
   RECT lprc ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY );

   if (ISARRAY(1) && Array2Rect( pSrc1, &lprc))
   {
       if(OffsetRect( &lprc, hb_parni( 2 ), hb_parni( 3 ) ))
         {
           Rect2ArrayEx(&lprc,pSrc1);
           hb_retl(TRUE);
         }
       else
           hb_retl(FALSE);
      }
   else
      hb_retl(FALSE);
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI IsRectEmpty( IN CONST RECT *lprc);


/* Call as
local aSrc1 := { 1 , 5 , 4 , 6 }
? OFFSETRECT(aSrc1)
*/

HB_FUNC( VWN_ISRECTEMPTY )
{
   RECT lprc ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY );

   if (ISARRAY(1) && Array2Rect( pSrc1, &lprc))
   {
      hb_retl( IsRectEmpty( &lprc ) );
   }
   else
      hb_retl(FALSE);

}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI EqualRect( IN CONST RECT *lprc1, IN CONST RECT *lprc2);

/*Call as
local aSrc1 := { 1 , 5 , 4 , 6 }
Local asrc2 := { 10 , 50 , 24 , 66 }
EQUALRECT(aSrc1,aSrc2)
*/
HB_FUNC( VWN_EQUALRECT )
{
   RECT lprc1 ;
   RECT lprc2 ;
   PHB_ITEM pSrc1=hb_param( 1 ,HB_IT_ARRAY ),pSrc2=hb_param( 2 ,HB_IT_ARRAY );

   if (Array2Rect( pSrc1, &lprc1) && Array2Rect( pSrc2, &lprc2))
   {
      hb_retl( EqualRect( &lprc1, &lprc2 ) );
   }
   else
      hb_retl(FALSE);
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI PtInRect( IN CONST RECT *lprc, IN POINT pt);

/*Call as
local aSrc1 := { 1 , 5 , 4 , 6 }
Local asrc2 := { 10 , 50 }
PTINRECT(aSrc1,aSrc2)
*/


HB_FUNC( VWN_PTINRECT )
{
   RECT  lprc ;
   POINT pt   ;
   PHB_ITEM pSrc1=hb_param( 1, HB_IT_ARRAY ),pSrc2=hb_param( 2, HB_IT_ARRAY );

   if (Array2Rect( pSrc1, &lprc) && Array2Point( pSrc2, &pt))
   {
      hb_retl( (BOOL) PtInRect( &lprc, pt ) );

   }
   else
      hb_retl( FALSE);

}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI ExcludeClipRect( IN HDC, IN int, IN int, IN int, IN int);


HB_FUNC( VWN_EXCLUDECLIPRECT )
{
   hb_retni( ExcludeClipRect( (HDC) HB_PARWH( 1 ),
                              hb_parni( 2 )      ,
                              hb_parni( 3 )      ,
                              hb_parni( 4 )      ,
                              hb_parni( 5 )
                              ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI RectVisible(IN HDC, IN CONST RECT *);


HB_FUNC( VWN_RECTVISIBLE )
{
   RECT rc;

   if ( HB_ISARRAY(2) && Array2Rect( hb_param(2,HB_IT_ARRAY), &rc ))
      hb_retl( RectVisible( (HDC) HB_PARWH( 1 ), &rc ) );
   else
      hb_retl(0);

}


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI GetBoundsRect( IN HDC, OUT LPRECT, IN UINT);

/*

HB_FUNC( VWN_GETBOUNDSRECT )
{
   RECT rc ;

   hb_retni( GetBoundsRect( (HDC) HB_PARWH( 1 ), lpRect, (UINT) hb_parni( 3 ) ) );
}

*/

// WINUSERAPI BOOL WINAPI ValidateRect( IN HWND hWnd, IN CONST RECT  *lpRect);

// Syntax
// ValidateRect( hWnd,[aRect]) -> lSuccess

HB_FUNC( VWN_VALIDATERECT )
{
   RECT rc ;

   if (ISARRAY( 2 ) && Array2Rect( hb_param( 2, HB_IT_ARRAY ), &rc ) )
      hb_retl( ValidateRect( (HWND) HB_PARWH( 1 ), &rc) );
   else
      hb_retl(ValidateRect( (HWND) HB_PARWH( 1 ),NULL));
}
