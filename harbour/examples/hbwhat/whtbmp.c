/*
 * $Id$
 */

// hbwhat
// Bitmap and raster operations related functions


/*

Modified and non-API functions:

  DrawBitmap( hDC, hBmp, nROP, x, y, nWidth, nHeight) -> NIL  // to be revised !!!
  GetBitmapSize(hBmp) -> aSize
  DrawGlyph(hDC, x , y , dx , dy , hBmp , rgbTransparent , lDisabled) -> NIL

*/

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
extern BOOL Array2Size(PHB_ITEM aSize, SIZE *siz );
extern PHB_ITEM Size2Array( SIZE *siz  );
extern void Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint);
extern void Rect2ArrayEx( RECT *pt  , PHB_ITEM aRect);
extern void Size2ArrayEx( SIZE *siz  ,  PHB_ITEM aSize);

//-----------------------------------------------------------------------------
// to be tested

HB_FUNC( VWN_LOADBITMAP )
{
   HB_RETWH( LoadBitmap(
             HB_ISNIL(1) ? GetModuleHandle( NULL ): (HINSTANCE) HB_PARWH(1) ,
             hb_parinfo(2)==HB_IT_STRING ?
                       (LPCTSTR) hb_parcx( 2 ) :
                       MAKEINTRESOURCE( (WORD) hb_parni(2)) ) );
}

//-----------------------------------------------------------------------------
// Alex Kresin
// DrawBitmap( hDC, hBitmap, nROP, x, y, nWidth, hHeight )

HB_FUNC( VWN_DRAWBITMAP )
{
   HDC hDC = (HDC) HB_PARWH( 1 );
   HDC hDCmem = CreateCompatibleDC( hDC );
   DWORD dwraster = (ISNIL(3))? SRCCOPY : hb_parnl(3);
   HBITMAP hBitmap = (HBITMAP) HB_PARWH( 2 );
   BITMAP  bitmap;
   int nWidthDest = ( hb_pcount() >=5 && !HB_ISNIL(6) )? hb_parni(6):0;
   int nHeightDest = ( hb_pcount()>=6 && !HB_ISNIL(7) )? hb_parni(7):0;

   SelectObject( hDCmem, hBitmap );
   GetObject( hBitmap, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   if( nWidthDest && ( nWidthDest != bitmap.bmWidth || nHeightDest != bitmap.bmHeight ))
   {
      StretchBlt( hDC, hb_parni(4), hb_parni(5), nWidthDest, nHeightDest, hDCmem,
                  0, 0, bitmap.bmWidth, bitmap.bmHeight, dwraster );
   }
   else
   {
      BitBlt( hDC, hb_parni(4), hb_parni(5), bitmap.bmWidth, bitmap.bmHeight, hDCmem, 0, 0, dwraster );
   }

   DeleteDC( hDCmem );
}

HB_FUNC( VWN_GETBITMAPSIZE )
{
   PHB_ITEM aArray = hb_itemArrayNew( 2 );
   BITMAP bm;
   HBITMAP hBmp = ( HBITMAP ) HB_PARWH( 1 );

   GetObject( hBmp, sizeof( bm ), &bm );

   hb_arraySetNL( aArray, 1, bm.bmWidth );
   hb_arraySetNL( aArray, 2, bm.bmHeight );

   hb_itemReturnRelease( aArray );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetBitmapDimensionEx( IN HBITMAP, OUT LPSIZE);

// Syntax
// GetBitmapDimensionEx(hBmp) -> aSize or NIL

HB_FUNC( VWN_GETBITMAPDIMENSIONEX )
{
   SIZE Size;

   if( GetBitmapDimensionEx( ( HBITMAP ) HB_PARWH( 1 ), &Size ) )
      hb_itemReturnRelease( Size2Array( &Size ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetBitmapDimensionEx( IN HBITMAP, IN int, IN int, OUT LPSIZE);

// Syntax
// SetBitmapDimensionsEx(hBmp,x,y)-> aOriginalDimesions


HB_FUNC( VWN_SETBITMAPDIMENSIONEX )
{
   SIZE Size;

   if ( SetBitmapDimensionEx( (HBITMAP) HB_PARWH( 1 ),
                                  hb_parni( 2 )          ,
                                  hb_parni( 3 )          ,
                                  &Size
                                  ) )
   {
      PHB_ITEM aSize = hb_itemArrayNew( 2 );

      hb_arraySetNL( aSize, 1, Size.cx );
      hb_arraySetNL( aSize, 2, Size.cy );

      hb_itemReturnRelease( aSize );
   }

}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetStretchBltMode(IN HDC, IN int);


HB_FUNC( VWN_SETSTRETCHBLTMODE )
{
   hb_retni( SetStretchBltMode( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI StretchBlt(IN HDC, IN int, IN int, IN int, IN int, IN HDC, IN int, IN int, IN int, IN int, IN DWORD);


HB_FUNC( VWN_STRETCHBLT )
{
   hb_retl( StretchBlt( (HDC) HB_PARWH( 1 )   ,
                        hb_parni( 2 )         ,
                        hb_parni( 3 )         ,
                        hb_parni( 4 )         ,
                        hb_parni( 5 )         ,
                        (HDC) HB_PARWH( 6 )   ,
                        hb_parni( 7 )         ,
                        hb_parni( 8 )         ,
                        hb_parni( 9 )         ,
                        hb_parni( 10 )        ,
                        (DWORD) hb_parnl( 11 )
                        ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateBitmap( IN int, IN int, IN UINT, IN UINT, IN CONST VOID *);



HB_FUNC( VWN_CREATEBITMAP )
{
   HB_RETWH( CreateBitmap( hb_parni( 1 )       ,
                           hb_parni( 2 )       ,
                           (UINT) hb_parni( 3 ),
                           (UINT) hb_parni( 4 ),
                           hb_parcx(5)
                           ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateBitmapIndirect( IN CONST BITMAP *);

// uses BITMAP structure

HB_FUNC( VWN_CREATEBITMAPINDIRECT )
{
   CONST BITMAP *bmp = (BITMAP * ) hb_parc( 1 );//hb_param( 1,HB_IT_STRING )->item.asString.value;

   HB_RETWH( CreateBitmapIndirect( bmp ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateCompatibleBitmap( IN HDC, IN int, IN int);

HB_FUNC( VWN_CREATECOMPATIBLEBITMAP )
{
   HB_RETWH( CreateCompatibleBitmap( (HDC) HB_PARWH( 1 ),
                                            hb_parni( 2 )      ,
                                            hb_parni( 3 )
                                            ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateDIBitmap( IN HDC, IN CONST BITMAPINFOHEADER *, IN DWORD, IN CONST VOID *, IN CONST BITMAPINFO *, IN UINT);

// uses structures

HB_FUNC( VWN_CREATEDIBITMAP )
{
   BITMAPINFOHEADER *bmih = (BITMAPINFOHEADER *) hb_parc( 2 );//hb_param( 2, HB_IT_STRING )->item.asString.value ;
   BITMAPINFO *bmi  = (BITMAPINFO *) hb_parc( 5 );//hb_param( 5, HB_IT_STRING)->item.asString.value ;

   HB_RETWH( CreateDIBitmap( (HDC) HB_PARWH( 1 )  ,
                                    bmih                ,
                                    (DWORD) hb_parnl( 3 ),
                                    (VOID *) hb_parcx( 3 ),
                                    bmi                 ,
                                    (UINT) hb_parni( 6 )
                                    ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateDIBSection( IN HDC, IN CONST BITMAPINFO *, IN UINT, OUT VOID **, IN HANDLE, IN DWORD);

/// ????????
/// ????????

HB_FUNC( VWN_CREATEDIBSECTION )
{
   BITMAPINFO *bmi  = (BITMAPINFO *) hb_parc( 2 );//hb_param( 2, HB_IT_STRING)->item.asString.value ;
   VOID **ppBits = (VOID **) 0;

   HB_RETWH( CreateDIBSection( (HDC) HB_PARWH( 1 )   ,
                                      bmi                  ,
                                      (UINT) hb_parni( 3 )  ,
                                      ppBits                ,
                                      (HANDLE) HB_PARWH( 5 ),
                                      (DWORD) hb_parnl( 6 )
                                      ) );

   HB_STORWH( *ppBits, 4);
}


//-----------------------------------------------------------------------------
// WINGDIAPI HBITMAP WINAPI CreateDiscardableBitmap( IN HDC, IN int, IN int);


HB_FUNC( VWN_CREATEDISCARDABLEBITMAP )
{
   HB_RETWH( CreateDiscardableBitmap( (HDC) HB_PARWH( 1 ),
                                             hb_parni( 2 )      ,
                                             hb_parni( 3 )
                                             ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI MaskBlt( IN HDC, IN int, IN int, IN int, IN int, IN HDC, IN int, IN int, IN HBITMAP, IN int, IN int, IN DWORD);


HB_FUNC( VWN_MASKBLT )
{
   hb_retl( MaskBlt( (HDC) HB_PARWH( 1 )    ,
                     hb_parni( 2 )          ,
                     hb_parni( 3 )          ,
                     hb_parni( 4 )          ,
                     hb_parni( 5 )          ,
                     (HDC) HB_PARWH( 6 )    ,
                     hb_parni( 7 )          ,
                     hb_parni( 8 )          ,
                     (HBITMAP) HB_PARWH( 9 ),
                     hb_parni( 10 )         ,
                     hb_parni( 11 )         ,
                     (DWORD) hb_parnl( 12 )
                     ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI BitBlt( IN HDC, IN int, IN int, IN int, IN int, IN HDC, IN int, IN int, IN DWORD);


HB_FUNC( VWN_BITBLT )
{
   hb_retl( BitBlt( (HDC) HB_PARWH( 1 )  ,
                    hb_parni( 2 )        ,
                    hb_parni( 3 )        ,
                    hb_parni( 4 )        ,
                    hb_parni( 5 )        ,
                    (HDC) HB_PARWH( 6 )  ,
                    hb_parni( 7 )        ,
                    hb_parni( 8 )        ,
                    (DWORD) hb_parnl( 9 )
                    ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PatBlt(IN HDC, IN int, IN int, IN int, IN int, IN DWORD);


HB_FUNC( VWN_PATBLT )
{
   hb_retl( PatBlt( (HDC) HB_PARWH( 1 )  ,
                    hb_parni( 2 )        ,
                    hb_parni( 3 )        ,
                    hb_parni( 4 )        ,
                    hb_parni( 5 )        ,
                    (DWORD) hb_parnl( 6 )
                    ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetROP2(IN HDC, IN int);


HB_FUNC( VWN_SETROP2 )
{
   hb_retni( SetROP2( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI LONG WINAPI SetBitmapBits(IN HBITMAP, IN DWORD, IN CONST VOID *);

// Syntax:
// SetBitmapBits(hBmp,cBits)

HB_FUNC( VWN_SETBITMAPBITS )
{
   hb_retnl( SetBitmapBits( (HBITMAP) HB_PARWH( 1 ),
                                   (DWORD) hb_parclen( 2 )  ,
                                   (VOID *) hb_parcx( 2 )
                                   ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetDIBits(IN HDC, IN HBITMAP, IN UINT, IN UINT, IN CONST VOID *, IN CONST BITMAPINFO *, IN UINT);

HB_FUNC( VWN_SETDIBITS )
{

   BITMAPINFO *bmi  = (BITMAPINFO *) hb_parc( 6 );//hb_param( 6, HB_IT_STRING)->item.asString.value ;



   // Your code goes here

   hb_retni( SetDIBits( (HDC) HB_PARWH( 1 )    ,
                        (HBITMAP) HB_PARWH( 2 ),
                        (UINT) hb_parni( 3 )   ,
                        (UINT) hb_parni( 4 )   ,
                        (VOID *) hb_parcx(5)    ,
                        bmi                    ,
                        (UINT) hb_parni( 7 )
                        ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetROP2( IN HDC);


HB_FUNC( VWN_GETROP2 )
{
   hb_retni( GetROP2( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetStretchBltMode( IN HDC);


HB_FUNC( VWN_GETSTRETCHBLTMODE )
{
   hb_retni( GetStretchBltMode( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetDIBitsToDevice(IN HDC, IN int, IN int, IN DWORD, IN DWORD, IN int, IN int, IN UINT, IN UINT, IN CONST VOID *, IN CONST BITMAPINFO *, IN UINT);

// uses bitmapinfo structure

HB_FUNC( VWN_SETDIBITSTODEVICE )
{

   BITMAPINFO *bmi  = (BITMAPINFO *) hb_parc( 11 );//hb_param( 11, HB_IT_STRING)->item.asString.value ;

   hb_retni( SetDIBitsToDevice( (HDC) HB_PARWH( 1 )  ,
                                hb_parni( 2 )        ,
                                hb_parni( 3 )        ,
                                (DWORD) hb_parnl( 4 ),
                                (DWORD) hb_parnl( 5 ),
                                hb_parni( 6 )        ,
                                hb_parni( 7 )        ,
                                (UINT) hb_parni( 8 ) ,
                                (UINT) hb_parni( 9 ) ,
                                (VOID *) hb_parcx(10) ,
                                bmi                  ,
                                (UINT) hb_parni( 12 )
                                ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI StretchDIBits(IN HDC, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN int, IN CONST VOID *, IN CONST BITMAPINFO *, IN UINT, IN DWORD);

// uses bitmap info structure

HB_FUNC( VWN_STRETCHDIBITS )
{

   BITMAPINFO *bmi  = (BITMAPINFO *) hb_parc( 11 );//hb_param( 11, HB_IT_STRING)->item.asString.value ;

   hb_retni( StretchDIBits( (HDC) HB_PARWH( 1 )   ,
                            hb_parni( 2 )         ,
                            hb_parni( 3 )         ,
                            hb_parni( 4 )         ,
                            hb_parni( 5 )         ,
                            hb_parni( 6 )         ,
                            hb_parni( 7 )         ,
                            hb_parni( 8 )         ,
                            hb_parni( 9 )         ,
                            (VOID *) hb_parcx(10)  ,
                            bmi                   ,
                            (UINT) hb_parni( 12 ) ,
                            (DWORD) hb_parnl( 13 )
                            ) );
}


//-----------------------------------------------------------------------------

void Pic(HDC hDC, int x , int y , int dx , int dy , HBITMAP hBmp , COLORREF rgbTransparent , BOOL disabled)

//HB_FUNC( VWN_DRAWGLYPH )

{

   //  COLORREF rgbOld ;
     HDC      hDCMem ;
     HDC      hDCMem2 ;
     HBITMAP  hbmDefault ;

     HBITMAP  hbmTransMask ;
     HBRUSH   hBr  ;
     HBRUSH   hOld ;


     hDCMem       = CreateCompatibleDC(hDC);
     hDCMem2      = CreateCompatibleDC(hDC);
     hbmTransMask = CreateBitmap(dx,dy,1,1,NULL);


     SetBkColor(hDC, RGB(255,255,255)); //White)
     SetTextColor(hDC, RGB(0,0,0));     //Black)

     hbmDefault=(HBITMAP)SelectObject(hDCMem, hBmp);
     SelectObject(hDCMem2, hbmTransMask)   ;

    // build mask based on transparent color.

     SetBkColor(hDCMem, rgbTransparent);
     BitBlt(hDCMem2, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY);

    if( disabled)
      {
        hBr=CreateSolidBrush(GetSysColor(COLOR_BTNHIGHLIGHT));
        hOld=(HBRUSH)SelectObject(hDC,hBr)  ;
        BitBlt(hDC, x+1, y+1, dx-2, dy-2, hDCMem2, 0, 0, 12060490);
        SelectObject(hDC,hOld);
        DeleteObject(hBr)      ;

        hBr=CreateSolidBrush(GetSysColor(COLOR_BTNSHADOW));
        hOld=(HBRUSH)SelectObject(hDC,hBr);
        BitBlt(hDC, x, y, dx-2, dy-2, hDCMem2, 0, 0, 12060490);
        SelectObject(hDC,hOld);
        DeleteObject(hBr);
      }
    else
      {
        BitBlt(hDC, x, y, dx, dy, hDCMem, 0, 0, SRCINVERT);
        BitBlt(hDC, x, y, dx, dy, hDCMem2, 0, 0, SRCAND)   ;
        BitBlt(hDC, x, y, dx, dy, hDCMem, 0, 0, SRCINVERT);
      }


    SelectObject(hDCMem, hbmDefault);
    SelectObject(hDCMem2, hbmDefault);
    DeleteObject(hbmTransMask);

    DeleteDC(hDCMem);
    DeleteDC(hDCMem2);

    return ;

}


//-----------------------------------------------------------------------------

/*
DrawGlyph(HDC hDC, int x , int y , int dx , int dy , HBITMAP hBmp , COLORREF rgbTransparent , BOOL disabled)
*/

HB_FUNC( VWN_DRAWGLYPH )
   {
    Pic(    (HDC) HB_PARWH(1),
                  hb_parni(2),
                  hb_parni(3),
                  hb_parni(4),
                  hb_parni(5),
        (HBITMAP) HB_PARWH(6),
       (COLORREF) hb_parnl(7),
                  hb_parl(8));
    return;
   }





//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DrawStateA( IN HDC, IN HBRUSH, IN DRAWSTATEPROC, IN LPARAM, IN WPARAM, IN int, IN int, IN int, IN int, IN UINT);

//The DrawState function displays an image and applies a visual effect to indicate a state, such as a disabled or default state.


/*

HB_FUNC( VWN_DRAWSTATE )
{
   DRAWSTATEPROC drawstateProc ;

   // Your code goes here

   hb_retl( DrawState( (HDC) HB_PARWH( 1 )   ,
                       (HBRUSH) HB_PARWH( 2 ),
                       drawstateProc         ,
                       (LPARAM) hb_parnint( 4 ),
                       (WPARAM) hb_parnint( 5 ),
                       hb_parni( 6 )         ,
                       hb_parni( 7 )         ,
                       hb_parni( 8 )         ,
                       hb_parni( 9 )         ,
                       (UINT) hb_parni( 10 )
                     ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI LONG WINAPI GetBitmapBits( IN HBITMAP, IN LONG, OUT LPVOID);

// obsolete

/*

HB_FUNC( VWN_GETBITMAPBITS )
{
   LPVOID  lpVoid  ;

   // Your code goes here

   HB_RETWH( GetBitmapBits( (HBITMAP) HB_PARWH( 1 ),
                                   hb_parnl( 2 )          ,
                                   lpVoid
                                   ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetDIBits( IN HDC, IN HBITMAP, IN UINT, IN UINT, OUT LPVOID, IN OUT LPBITMAPINFO, IN UINT);


// NOT FINISHED

/*
HB_FUNC( VWN_GETDIBITS )
{
   VOID *lpvBits ;
   BITMAPINFO *bmi  = (BITMAPINFO *) hb_param( 6, HB_IT_STRING)->item.asString.value ;

   hb_retni( GetDIBits( (HDC) HB_PARWH( 1 )       ,
                        (HBITMAP) HB_PARWH( 2 )   ,
                        (UINT) hb_parni( 3 )      ,
                        (UINT) hb_parni( 4 )      ,
                        HB_ISNIL(5) ? NULL : lpvBits ,
                        bmi                       ,
                        (UINT) hb_parni( 7 )
                        ) );

  hb_storc( lpvBits, 5);
}
*/


//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI GetGlyphIndicesA( IN HDC, IN LPCSTR, IN int, OUT LPWORD, IN DWORD);

/*

HB_FUNC( VWN_GETGLYPHINDICESA )
{
   LPWORD lpWord ;

   // Your code goes here

   HB_RETWH( GetGlyphIndicesA( (HDC) HB_PARWH( 1 )  ,
                                      (LPCSTR) hb_parcx( 2 ),
                                      hb_parni( 3 )        ,
                                      lpWord               ,
                                      (DWORD) hb_parnl( 5 )
                                      ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI GetGlyphOutlineA( IN HDC, IN UINT, IN UINT, OUT LPGLYPHMETRICS, IN DWORD, OUT LPVOID, IN CONST MAT2 *);

/*

HB_FUNC( VWN_GETGLYPHOUTLINEA )
{
   LPGLYPHMETRICS lpglyphMetrics ;
   LPVOID         lpVoid         ;
   CONST          MAT2           ;

   // Your code goes here

   HB_RETWH( GetGlyphOutlineA( (HDC) HB_PARWH( 1 )  ,
                                      (UINT) hb_parni( 2 ) ,
                                      (UINT) hb_parni( 3 ) ,
                                      lpglyphMetrics       ,
                                      (DWORD) hb_parnl( 5 ),
                                      lpVoid               ,
                                      &MAT2
                                      ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI AlphaBlend( IN HDC, IN int, IN int, IN int, IN int, IN HDC, IN int, IN int, IN int, IN int, IN BLENDFUNCTION);

// NT only ?

/*
HB_FUNC( VWN_ALPHABLEND )
{
   BLENDFUNCTION *bf = (BLENDFUNCTION *) hb_param( 11, HB_IT_STRING)->item.asString.value ;

   hb_retl( AlphaBlend( (HDC) HB_PARWH( 1 ),
                        hb_parni( 2 )      ,
                        hb_parni( 3 )      ,
                        hb_parni( 4 )      ,
                        hb_parni( 5 )      ,
                        (HDC) HB_PARWH( 6 ),
                        hb_parni( 7 )      ,
                        hb_parni( 8 )      ,
                        hb_parni( 9 )      ,
                        hb_parni( 10 )     ,
                        *bf
                        ) );
}
*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PlgBlt( IN HDC, IN CONST POINT *, IN HDC, IN int, IN int, IN int, IN int, IN HBITMAP, IN int, IN int);

/*

HB_FUNC( VWN_PLGBLT )
{
   CONST   POINT   ;

   // Your code goes here

   hb_retl( PlgBlt( (HDC) HB_PARWH( 1 )    ,
                    &POINT                 ,
                    (HDC) HB_PARWH( 3 )    ,
                    hb_parni( 4 )          ,
                    hb_parni( 5 )          ,
                    hb_parni( 6 )          ,
                    hb_parni( 7 )          ,
                    (HBITMAP) HB_PARWH( 8 ),
                    hb_parni( 9 )          ,
                    hb_parni( 10 )
                    ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI SetDIBColorTable( IN HDC, IN UINT, IN UINT, IN CONST RGBQUAD *);

/*

HB_FUNC( VWN_SETDIBCOLORTABLE )
{
   CONST RGBQUAD ;

   // Your code goes here

   hb_retni( SetDIBColorTable( (HDC) HB_PARWH( 1 ) ,
                               (UINT) hb_parni( 2 ),
                               (UINT) hb_parni( 3 ),
                               &RGBQUAD
                               ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI TransparentBlt(IN HDC,IN int,IN int,IN int,IN int,IN HDC,IN int,IN int,IN int,IN int,IN UINT);

// NT only ?

/*
HB_FUNC( VWN_TRANSPARENTBLT )
{
   hb_retl( TransparentBlt( (HDC) HB_PARWH( 1 )  ,
                            hb_parni( 2 )        ,
                            hb_parni( 3 )        ,
                            hb_parni( 4 )        ,
                            hb_parni( 5 )        ,
                            (HDC) HB_PARWH( 6 )  ,
                            hb_parni( 7 )        ,
                            hb_parni( 8 )        ,
                            hb_parni( 9 )        ,
                            hb_parni( 10 )       ,
                            (UINT) hb_parni( 11 )
                            ) );
}

*/
