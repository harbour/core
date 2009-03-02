/*
 * $Id$
 */

// hbwhat


// GDI functions

/*
 * Some parts Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * with author's permission granted on 27 MAy 2002
   Last change:  WN   30 May 2002    0:20 am
 */



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


// BASIC COLOUR FUNCTIONS

//-----------------------------------------------------------------------------

HB_FUNC( VWN_RGB )
{
   hb_retnl((ULONG)(COLORREF)(((BYTE)(hb_parni(1))|
                              ((WORD)((BYTE)(hb_parni(2)))<<8))|
                              (((DWORD)(BYTE)(hb_parni(3)))<<16)));
}

//-----------------------------------------------------------------------------
// BYTE GetGValue( DWORD rgb );

HB_FUNC( VWN_GETGVALUE )
{
  hb_retni( (INT) GetGValue( (DWORD) hb_parnl( 1 ) ) );
}

//-----------------------------------------------------------------------------
// BYTE GetBValue( WORD rgb );

HB_FUNC( VWN_GETBVALUE )
{
  hb_retni( (INT) GetBValue( (DWORD) hb_parnl( 1 ) ) );
}

//-----------------------------------------------------------------------------
// BYTE GetRValue( DWORD rgb );

HB_FUNC( VWN_GETRVALUE )
{
  hb_retni( (INT) GetRValue( (DWORD) hb_parnl( 1 ) ) );
}

//-----------------------------------------------------------------------------

// WINGDIAPI COLORREF WINAPI SetTextColor(IN HDC, IN COLORREF);

HB_FUNC( VWN_SETTEXTCOLOR )
{

  hb_retnl( (ULONG) SetTextColor( (HDC) HB_PARWH( 1 ), (COLORREF) hb_parnl(2) ) );

}


//-----------------------------------------------------------------------------
// WINGDIAPI COLORREF WINAPI GetTextColor( IN HDC);


HB_FUNC( VWN_GETTEXTCOLOR )
{
   hb_retnl((ULONG) GetTextColor( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------

// WINGDIAPI COLORREF WINAPI GetBkColor( IN HDC);


HB_FUNC( VWN_GETBKCOLOR )
{
   hb_retnl( (ULONG) GetBkColor( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------

// WINGDIAPI COLORREF WINAPI SetBkColor(IN HDC, IN COLORREF);

HB_FUNC( VWN_SETBKCOLOR )
{

   hb_retnl( (ULONG) SetBkColor( (HDC) HB_PARWH( 1 ), (COLORREF) hb_parnl(2) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI UpdateColors(IN HDC);


HB_FUNC( VWN_UPDATECOLORS )
{
   hb_retl( UpdateColors( (HDC) HB_PARWH( 1 ) ) );
}




// OBJECT FUNCTIONS


//-----------------------------------------------------------------------------

// WINGDIAPI HGDIOBJ WINAPI GetStockObject( IN int);


HB_FUNC( VWN_GETSTOCKOBJECT )
{
   HB_RETWH( GetStockObject( hb_parni( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI HGDIOBJ WINAPI SelectObject(IN HDC, IN HGDIOBJ);


HB_FUNC( VWN_SELECTOBJECT )
{
   HB_RETWH( SelectObject( (HDC) HB_PARWH( 1 ), (HGDIOBJ) HB_PARWH( 2 ) ) );
}


//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI DeleteObject( IN HGDIOBJ);


HB_FUNC( VWN_DELETEOBJECT )
{
   hb_retl( DeleteObject( (HGDIOBJ) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI UnrealizeObject( IN HGDIOBJ);


HB_FUNC( VWN_UNREALIZEOBJECT )
{
   hb_retl( UnrealizeObject( (HGDIOBJ) HB_PARWH( 1 ) ) );
}




//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI GetObjectType( IN HGDIOBJ h);


HB_FUNC( VWN_GETOBJECTTYPE )
{
   hb_retnl( ( long ) GetObjectType( (HGDIOBJ) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI HGDIOBJ WINAPI GetCurrentObject( IN HDC, IN UINT);


HB_FUNC( VWN_GETCURRENTOBJECT )
{
   HB_RETWH( GetCurrentObject( (HDC) HB_PARWH( 1 ), (UINT) hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetObject( IN HGDIOBJ, IN int, OUT LPVOID);

//Syntax:
//cBytes:=GetObject(hGDIObj)

HB_FUNC( VWN_GETOBJECT )
{

   int nBytes = GetObject( (HGDIOBJ) HB_PARWH( 1 ), 0, NULL );
   LPVOID  lpObj = (VOID *) hb_xgrab(nBytes);

   nBytes = GetObject( (HGDIOBJ) HB_PARWH( 1 ), nBytes, lpObj )  ;

   hb_retclen( (char *) lpObj,nBytes);
   hb_xfree(lpObj);

}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI EnumObjects( IN HDC, IN int, IN GOBJENUMPROC, IN LPVOID);

/*

HB_FUNC( VWN_ENUMOBJECTS )
{
   GOBJENUMPROC gObjEnumProc ;
   LPVOID       lpVoid       ;

   // Your code goes here

   hb_retni( EnumObjects( (HDC) HB_PARWH( 1 ),
                          hb_parni( 2 )      ,
                          gObjEnumProc       ,
                          lpVoid
                          ) );
}

*/


// MAPPING FUNCTIONS


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetMapMode( IN HDC);


HB_FUNC( VWN_GETMAPMODE )
{
   hb_retni( GetMapMode( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetMapMode(IN HDC, IN int);


HB_FUNC( VWN_SETMAPMODE )
{
   hb_retni( SetMapMode( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI SetMapperFlags(IN HDC, IN DWORD);


HB_FUNC( VWN_SETMAPPERFLAGS )
{
   hb_retnl( (LONG) SetMapperFlags( (HDC) HB_PARWH( 1 ), (DWORD) hb_parnl( 2 ) ) );
}



// WM_PAINT functions


//-----------------------------------------------------------------------------

// HDC BeginPaint( HWND hwnd, LPPAINTSTRUCT lpPaint );

// Syntax:
// Local cPS
// BeginPaint( hWnd, @cPS) -> hDC

HB_FUNC( VWN_BEGINPAINT )
{
   PAINTSTRUCT pps ;
   HB_RETWH( BeginPaint( (HWND) HB_PARWH( 1 ), &pps ) );
   hb_storclen( (char *) &pps, sizeof(PAINTSTRUCT), 2 );
}

//-----------------------------------------------------------------------------
// BOOL EndPaint(  HWND hWnd, CONST PAINTSTRUCT *lpPaint );

// SYNTAX
// EndPaint(hWnd,cPS)->lSuccess

HB_FUNC( VWN_ENDPAINT )
{
   hb_retl( EndPaint( (HWND) HB_PARWH( 1 ), (PAINTSTRUCT*) hb_parcx( 2 ) ) );
}


//------------------------------------------------------------------------------

// WINGDIAPI int WINAPI GetGraphicsMode( IN HDC);

HB_FUNC( VWN_GETGRAPHICSMODE )
{
   hb_retni( GetGraphicsMode( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI GdiComment( IN HDC, IN UINT, IN CONST BYTE *);

HB_FUNC( VWN_GDICOMMENT )
{

   hb_retl( GdiComment( (HDC) HB_PARWH( 1 ), (UINT) hb_parni( 2 ), ( const BYTE * ) hb_parcx( 3 ) ) );

}


//-----------------------------------------------------------------------------

// WINGDIAPI DWORD WINAPI GdiSetBatchLimit( IN DWORD);


HB_FUNC( VWN_GDISETBATCHLIMIT )
{
   hb_retnl( (LONG) GdiSetBatchLimit( (DWORD) hb_parnl( 1 ) ) );
}





// PATH functions



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SelectClipPath(IN HDC, IN int);


HB_FUNC( VWN_SELECTCLIPPATH )
{
   hb_retl( SelectClipPath( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}





//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI WidenPath(IN HDC);


HB_FUNC( VWN_WIDENPATH )
{
   hb_retl( WidenPath( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI StrokeAndFillPath(IN HDC);


HB_FUNC( VWN_STROKEANDFILLPATH )
{
   hb_retl( StrokeAndFillPath( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI StrokePath(IN HDC);


HB_FUNC( VWN_STROKEPATH )
{
   hb_retl( StrokePath( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------

// WINGDIAPI BOOL WINAPI EndPath(IN HDC);


HB_FUNC( VWN_ENDPATH )
{
   hb_retl( EndPath( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI AbortPath(IN HDC);


HB_FUNC( VWN_ABORTPATH )
{
   hb_retl( AbortPath( (HDC) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetPath(IN HDC, OUT LPPOINT, OUT LPBYTE, IN int);

/*

HB_FUNC( VWN_GETPATH )
{
   LPPOINT lpPoInt ;
   LPBYTE  lpByte  ;

   // Your code goes here

   hb_retni( GetPath( (HDC) HB_PARWH( 1 ), lpPoInt, lpByte, hb_parni( 4 ) ) );
}

*/



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI LPtoDP( IN HDC, IN OUT LPPOINT, IN int);

// Syntax
// LPToDP(hDC,aPoints)->lSuccess
// where aPoints = array of point arrays

HB_FUNC( VWN_LPTODP )
{
   POINT * Point ;
   POINT pt;
   INT iCount ;
   INT i ;
   PHB_ITEM aParam;
   PHB_ITEM aSub;


   if (ISARRAY( 2 ) )
   {
       iCount = hb_parinfa( 2, 0 );
       Point = ( POINT *) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );
          if ( Array2Point(aSub, &pt ))
               *(Point+i) = pt ;
          else {
            hb_retl(0);
            hb_xfree(Point);
            return ;
          }
       }

       if ( LPtoDP( (HDC) HB_PARWH( 1 ), Point, iCount ) )
       {
         for ( i = 0 ; i < iCount ; i++ )
         {
            aSub = Point2Array(Point+i);
            //hb_arraySet( aParam, i+1, hb_arrayClone(aSub, NULL ) );
            hb_arraySet( aParam, i+1, hb_arrayClone(aSub) );
            hb_itemRelease(aSub);
         }
         hb_retl(1);
       }
       else
         hb_retl(0);

       hb_xfree(Point);

   }
   else
     hb_retl(0);

}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI DPtoLP( IN HDC, IN OUT LPPOINT, IN int);

// Syntax
// DPtoLP(hDC,aPoints)->lSuccess
// where aPoints = array of point arrays

HB_FUNC( VWN_DPTOLP )
{
   POINT * Point ;
   POINT pt;
   INT iCount ;
   INT i ;
   PHB_ITEM aParam;
   PHB_ITEM aSub;


   if (ISARRAY( 2 ) )
   {
       iCount = hb_parinfa( 2, 0 );
       Point = ( POINT * ) hb_xgrab( iCount * sizeof (POINT) );
       aParam = hb_param(2,HB_IT_ARRAY);

       for ( i = 0 ; i<iCount ; i++ )
       {
          aSub = hb_itemArrayGet( aParam, i+1 );
          if ( Array2Point(aSub, &pt ))
               *(Point+i) = pt ;
          else {
            hb_retl(0);
            hb_xfree(Point);
            return ;
          }
       }

       if ( DPtoLP( (HDC) HB_PARWH( 1 ), Point, iCount ) )
       {
         for ( i = 0 ; i < iCount ; i++ )
         {
            aSub = Point2Array(Point+i);
            hb_arraySet( aParam, i+1, hb_arrayClone(aSub) );
            hb_itemRelease(aSub);
         }
         hb_retl(1);
       }
       else
         hb_retl(0);

       hb_xfree(Point);

   }
   else
     hb_retl(0);

}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetDeviceCaps( IN HDC, IN int);


HB_FUNC( VWN_GETDEVICECAPS )
{
   hb_retni( GetDeviceCaps( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------

// WINUSERAPI BOOL WINAPI PaintDesktop( IN HDC hdc);


HB_FUNC( VWN_PAINTDESKTOP )
{
   hb_retl( PaintDesktop( (HDC) HB_PARWH( 1 ) ) );
}




//-----------------------------------------------------------------------------

// WINUSERAPI DWORD WINAPI GetGuiResources( IN HANDLE hProcess, IN DWORD uiFlags);

#if(WINVER >= 0x0500)

HB_FUNC( VWN_GETGUIRESOURCES )
{
   hb_retnl( (LONG) GetGuiResources( (HANDLE) HB_PARWH( 1 ),
                                     (DWORD) hb_parnl( 2 )
                                   ) );
}

#endif

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI PtVisible(IN HDC, IN int, IN int);


HB_FUNC( VWN_PTVISIBLE )
{
   hb_retl( PtVisible( (HDC) HB_PARWH( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI SetGraphicsMode(IN HDC hdc, IN int iMode);


HB_FUNC( VWN_SETGRAPHICSMODE )
{
   hb_retni( SetGraphicsMode( (HDC) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI DWORD WINAPI SetLayout(IN HDC, IN DWORD);

/*
HB_FUNC( VWN_SETLAYOUT )
{
   hb_retnl( (LONG) SetLayout( (HDC) HB_PARWH( 1 ), (DWORD) hb_parnl( 2 ) ) );
}
*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SwapBuffers(HDC);


HB_FUNC( VWN_SWAPBUFFERS )
{
   hb_retl( SwapBuffers( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI GetClipBox( IN HDC, OUT LPRECT);

// this is not right, the return value is important !


HB_FUNC( VWN_GETCLIPBOX )
{
   RECT Rect ;
   PHB_ITEM aRect ;

   hb_retni( GetClipBox( (HDC) HB_PARWH( 1 ), &Rect ) );

      aRect = Rect2Array( &Rect );
      hb_itemReturn( aRect );
      hb_itemRelease( aRect );


}


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetMiterLimit(IN HDC, OUT PFLOAT);

/*

HB_FUNC( VWN_GETMITERLIMIT )
{
   PFLOAT pFloat ;

   // Your code goes here

   hb_retl( GetMiterLimit( (HDC) HB_PARWH( 1 ), pFloat ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetRasterizerCaps( OUT LPRASTERIZER_STATUS, IN UINT);


HB_FUNC( VWN_GETRASTERIZERCAPS )
{
   LPRASTERIZER_STATUS lprs = (LPRASTERIZER_STATUS) hb_parc(1); //hb_param( 1,HB_IT_STRING )->item.asString.value;

   if( GetRasterizerCaps( lprs, (UINT) hb_parni( 2 ) ) )
       hb_retclen( (char*) lprs, sizeof(RASTERIZER_STATUS) );
   //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) lprs, sizeof(RASTERIZER_STATUS ) );

}




//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetAspectRatioFilterEx( IN HDC, OUT LPSIZE);



HB_FUNC( VWN_GETASPECTRATIOFILTEREX )
{
   SIZE lpSize ;
   PHB_ITEM pArray=hb_param(2,HB_IT_ARRAY);
   // Your code goes here

   if( GetAspectRatioFilterEx( (HDC) HB_PARWH( 1 ), &lpSize ) )
      {
      Size2ArrayEx(&lpSize,pArray);
      hb_retl(TRUE);
      }
   else
      hb_retl(FALSE);


}




//-----------------------------------------------------------------------------
// WINSPOOLAPI int WINAPI DeviceCapabilitiesA( IN LPCSTR, IN LPCSTR, IN WORD, OUT LPSTR, IN CONST DEVMODEA *);

/*

HB_FUNC( VWN_DEVICECAPABILITIESA )
{
   WORD   Word     ;
   CONST  DEVMODEA ;

   // Your code goes here

   hb_retni( DeviceCapabilitiesA( (LPCSTR) hb_parcx( 1 ),
                                  (LPCSTR) hb_parcx( 2 ),
                                  Word                 ,
                                  (LPSTR) hb_parcx( 4 ) ,
                                  &DEVMODEA
                                  ) );
}

*/


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetUpdateRect( IN HWND hWnd, OUT LPRECT lpRect, IN BOOL bErase);

// SYNTAX
// GetUpdateRect(hWnd,lErase) -> aRect or NIL

HB_FUNC( VWN_GETUPDATERECT )
{
   RECT Rect ;
   PHB_ITEM aRect ;

   if ( GetUpdateRect( (HWND) HB_PARWH( 1 ), &Rect, hb_parl( 2 ) ) )
   {
      aRect = Rect2Array( &Rect );
      hb_itemReturn( aRect );
      hb_itemRelease( aRect );
   }

}


//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI DragObject( IN HWND, IN HWND, IN UINT, IN ULONG_PTR, IN HCURSOR);

/*

HB_FUNC( VWN_DRAGOBJECT )
{
   ULONG_PTR uLong_ptr ;

   // Your code goes here

   hb_retnl( (LONG) DragObject( (HWND) HB_PARWH( 1 )   ,
                                (HWND) HB_PARWH( 2 )   ,
                                (UINT) hb_parni( 3 )   ,
                                uLong_ptr              ,
                                (HCURSOR) HB_PARWH( 5 )
                              ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DragDetect( IN HWND, IN POINT);

/*Call as folow
Local aSrc:={10,12}
DRAGDETECT(nHandle,aSrc)
*/


HB_FUNC( VWN_DRAGDETECT )
{
   POINT PoInt ;
   PHB_ITEM pArray;
   if (ISARRAY(2))
   {
      pArray = hb_param(2, HB_IT_ARRAY);
      Array2Point(pArray,&PoInt);
      hb_retl( DragDetect( (HWND) HB_PARWH( 1 ), PoInt ) );
   }
   else
      hb_retl(FALSE);
}



//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETDRAWITEMSTRUCT )
{

  DRAWITEMSTRUCT * dis = ( DRAWITEMSTRUCT * ) HB_PARWH( 1 );

  PHB_ITEM arrDis = hb_itemArrayNew( 12 );

  hb_arraySetNL( arrDis,  1, dis->CtlType );
  hb_arraySetNL( arrDis,  2, dis->CtlID );
  hb_arraySetNL( arrDis,  3, dis->itemID );
  hb_arraySetNL( arrDis,  4, dis->itemAction );
  hb_arraySetNL( arrDis,  5, dis->itemState );
  HB_ARRAYSETWH( arrDis,  6, dis->hwndItem );
  HB_ARRAYSETWH( arrDis,  7, dis->hDC );
  hb_arraySetNL( arrDis,  8, dis->rcItem.left );
  hb_arraySetNL( arrDis,  9, dis->rcItem.top );
  hb_arraySetNL( arrDis, 10, dis->rcItem.right );
  hb_arraySetNL( arrDis, 11, dis->rcItem.bottom );
  hb_arraySetNInt( arrDis, 12, ( HB_PTRDIFF ) dis->itemData );

  hb_itemReturnRelease( arrDis );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DrawFrameControl( IN HDC, IN OUT LPRECT, IN UINT, IN UINT);



HB_FUNC( VWN_DRAWFRAMECONTROL )
{
   RECT lpRect ;
   PHB_ITEM pArray=hb_param(2,HB_IT_ARRAY);

   // Your code goes here
   if (Array2Rect(pArray,&lpRect))
   {
      if( DrawFrameControl( (HDC) HB_PARWH( 1 ) ,
                              &lpRect              ,
                              (UINT) hb_parni( 3 ),
                              (UINT) hb_parni( 4 )
                            ) )
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
// WINUSERAPI BOOL WINAPI DrawAnimatedRects( IN HWND hwnd, IN int idAni, IN CONST RECT * lprcFrom, IN CONST RECT * lprcTo);



HB_FUNC( VWN_DRAWANIMATEDRECTS )
{
   RECT lprcFrom ;
   RECT lprcTo   ;

   // Your code goes here
   if ( Array2Rect(hb_param(3,HB_IT_ARRAY),&lprcFrom ) && Array2Rect(hb_param(4,HB_IT_ARRAY) ,&lprcFrom ))
   {
      if(DrawAnimatedRects( (HWND) HB_PARWH( 1 ),
                               hb_parni( 2 )       ,
                               &lprcFrom           ,
                               &lprcTo
                             ) )
         hb_retl(TRUE);
      else
         hb_retl(FALSE);
   }
   else
      hb_retl(FALSE);

}




//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetWindowOrgEx( IN HDC, OUT LPPOINT);



HB_FUNC( VWN_GETWINDOWORGEX )
{
   POINT lpPoInt ;
   PHB_ITEM pArray=hb_param(2,HB_IT_ARRAY);


   if( GetWindowOrgEx( (HDC) HB_PARWH( 1 ), &lpPoInt ) )
      {
         Point2ArrayEx(&lpPoInt,pArray);
         hb_retl(TRUE);
      }
   else
      hb_retl(FALSE);
}



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI GetWorldTransform( IN HDC, OUT LPXFORM);

/*

HB_FUNC( VWN_GETWORLDTRANSFORM )
{
   LPXFORM lpxForm ;

   // Your code goes here

   hb_retl( GetWorldTransform( (HDC) HB_PARWH( 1 ), lpxForm ) );
}

*/


//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI ModifyWorldTransform( IN HDC, IN CONST XFORM *, IN DWORD);

/*

HB_FUNC( VWN_MODIFYWORLDTRANSFORM )
{
   CONST XFORM ;

   // Your code goes here

   hb_retl( ModifyWorldTransform( (HDC) HB_PARWH( 1 )  ,
                                  &XFORM               ,
                                  (DWORD) hb_parnl( 3 )
                                  ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI OffsetViewportOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

/*

HB_FUNC( VWN_OFFSETVIEWPORTORGEX )
{
   LPPOINT lpPoInt ;

   // Your code goes here

   hb_retl( OffsetViewportOrgEx( (HDC) HB_PARWH( 1 ),
                                 hb_parni( 2 )      ,
                                 hb_parni( 3 )      ,
                                 lpPoInt
                                 ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI OffsetWindowOrgEx( IN HDC, IN int, IN int, OUT LPPOINT);

/*

HB_FUNC( VWN_OFFSETWINDOWORGEX )
{
   LPPOINT lpPoInt ;

   // Your code goes here

   hb_retl( OffsetWindowOrgEx( (HDC) HB_PARWH( 1 ),
                               hb_parni( 2 )      ,
                               hb_parni( 3 )      ,
                               lpPoInt
                               ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI ScaleWindowExtEx( IN HDC, IN int, IN int, IN int, IN int, OUT LPSIZE);



HB_FUNC( VWN_SCALEWINDOWEXTEX )
{
   SIZE lpSize ;
   PHB_ITEM pArray=hb_param(6,HB_IT_ARRAY);


   if( ScaleWindowExtEx( (HDC) HB_PARWH( 1 ),
                              hb_parni( 2 )      ,
                              hb_parni( 3 )      ,
                              hb_parni( 4 )      ,
                              hb_parni( 5 )      ,
                              &lpSize
                              ) )
   {
      Size2ArrayEx(&lpSize,pArray);
      hb_retl(TRUE);
   }
   else
   hb_retl(FALSE);
}



//-----------------------------------------------------------------------------
// WINGDIAPI UINT WINAPI SetBoundsRect(IN HDC, IN CONST RECT *, IN UINT);

/*

HB_FUNC( VWN_SETBOUNDSRECT )
{
   CONST RECT ;

   // Your code goes here

   hb_retni( SetBoundsRect( (HDC) HB_PARWH( 1 ), &RECT, (UINT) hb_parni( 3 ) ) );
}

*/



//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetWorldTransform( IN HDC, IN CONST XFORM *);

/*

HB_FUNC( VWN_SETWORLDTRANSFORM )
{
   CONST XFORM ;

   // Your code goes here

   hb_retl( SetWorldTransform( (HDC) HB_PARWH( 1 ), &XFORM ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI BOOL WINAPI SetMiterLimit(IN HDC, IN FLOAT, OUT PFLOAT);

/*

HB_FUNC( VWN_SETMITERLIMIT )
{
   PFLOAT pFloat ;

   // Your code goes here

   hb_retl( SetMiterLimit( (HDC) HB_PARWH( 1 ), (FLOAT) hb_parnd( 2 ), pFloat ) );
}

*/
