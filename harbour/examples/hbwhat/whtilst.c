/*
 * $Id$
 */


// hbwhat
// ImageList functions

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400
#undef _WIN32_IE
#define _WIN32_IE      0x0500

#include "hbwhat.h"

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"

extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );

//-----------------------------------------------------------------------------
//WINCOMMCTRLAPI HIMAGELIST  WINAPI ImageList_Create(int cx, int cy, UINT flags, int cInitial, int cGrow);

HB_FUNC( VWN_IMAGELIST_CREATE )
{
   HIMAGELIST ilist;
   ilist = ImageList_Create( hb_parni(1), hb_parni(2), hb_parnl(3), hb_parni(4), hb_parni(5));
   HB_RETWH( ilist );
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI int         WINAPI ImageList_ReplaceIcon(HIMAGELIST himl, int i, HICON hicon);

HB_FUNC( VWN_IMAGELIST_REPLACEICON )
{
   hb_retni( ImageList_ReplaceIcon( (HIMAGELIST) HB_PARWH(1),
                          (int) hb_parni(2)                 ,
                          (HICON) HB_PARWH(3) ) )           ;
}

//-----------------------------------------------------------------------------
//#define     ImageList_AddIcon(himl, hicon) ImageList_ReplaceIcon(himl, -1, hicon)

HB_FUNC( VWN_IMAGELIST_ADDICON )
{
   hb_retni( ImageList_AddIcon( (HIMAGELIST) HB_PARWH(1), (HICON) HB_PARWH(2) ) );
}

//-----------------------------------------------------------------------------
//  ImageList_Destroy(HIMAGELIST himl);

HB_FUNC( VWN_IMAGELIST_DESTROY )
{
   hb_retl( ImageList_Destroy( (HIMAGELIST) HB_PARWH(1) ) );
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI int         WINAPI ImageList_GetImageCount(HIMAGELIST himl);

HB_FUNC( VWN_IMAGELIST_GETIMAGECOUNT )
{

   hb_retni( ImageList_GetImageCount((HIMAGELIST) HB_PARWH(1) ) );

}

//-----------------------------------------------------------------------------
//  ImageList_SetImageCount(HIMAGELIST himl, UINT uNewCount);

HB_FUNC( VWN_IMAGELIST_SETIMAGECOUNT )
{
   hb_retl(  ImageList_SetImageCount((HIMAGELIST) HB_PARWH( 1 ),
                                     (UINT) hb_parni( 2 ) ) )  ;
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI int         WINAPI ImageList_Add(HIMAGELIST himl, HBITMAP hbmImage, HBITMAP hbmMask);

HB_FUNC( VWN_IMAGELIST_ADD )
{
   hb_retni( ImageList_Add((HIMAGELIST) HB_PARWH( 1 ),
                           (HBITMAP) HB_PARWH( 2 )   ,
                           (HBITMAP) HB_PARWH( 3 )));
}

//-----------------------------------------------------------------------------
//WINCOMMCTRLAPI COLORREF    WINAPI ImageList_SetBkColor(HIMAGELIST himl, COLORREF clrBk);

HB_FUNC( VWN_IMAGELIST_SETBKCOLOR )
{
   hb_retnl( (LONG) ImageList_SetBkColor((HIMAGELIST) HB_PARWH( 1 ),
                                         (COLORREF) hb_parnl( 2 )));
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI COLORREF    WINAPI ImageList_GetBkColor(HIMAGELIST himl);

HB_FUNC( VWN_IMAGELIST_GETBKCOLOR )
{
   hb_retnl( (LONG) ImageList_GetBkColor((HIMAGELIST) HB_PARWH( 1 )));
}

//-----------------------------------------------------------------------------
// ImageList_SetOverlayImage(HIMAGELIST himl, int iImage, int iOverlay);

HB_FUNC( VWN_IMAGELIST_SETOVERLAYIMAGE )
{
   hb_retl(  ImageList_SetOverlayImage((HIMAGELIST) HB_PARWH( 1 ) ,
                                        hb_parni(2), hb_parni(3)));
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI BOOL WINAPI ImageList_Draw(HIMAGELIST himl, int i, HDC hdcDst,;
//                                           int x, int y, UINT fStyle);

HB_FUNC( VWN_IMAGELIST_DRAW )
{
   hb_retl( ImageList_Draw((HIMAGELIST) HB_PARWH( 1 ), hb_parni(2)    ,
                           (HDC) HB_PARWH(3), hb_parni(4), hb_parni(5),
                           (UINT) hb_parni(6)))                       ;

}

//-----------------------------------------------------------------------------
//  ImageList_Replace(HIMAGELIST himl, int i, HBITMAP hbmImage, HBITMAP hbmMask);

HB_FUNC( VWN_IMAGELIST_REPLACE )
{
   hb_retl(  ImageList_Replace((HIMAGELIST) HB_PARWH( 1 ),
                                hb_parni( 2 )            ,
                                (HBITMAP) HB_PARWH(3)    ,
                                (HBITMAP) HB_PARWH(4)))  ;
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI int         WINAPI ImageList_AddMasked(HIMAGELIST himl, HBITMAP hbmImage, COLORREF crMask);

HB_FUNC( VWN_IMAGELIST_ADDMASKED )
{
   hb_retni( ImageList_AddMasked((HIMAGELIST) HB_PARWH( 1 ) ,
                                  (HBITMAP) HB_PARWH( 2 )   ,
                                  (COLORREF) hb_parnl( 3 )));

}

//-----------------------------------------------------------------------------
//  ImageList_DrawEx(HIMAGELIST himl, int i, HDC hdcDst, int x, int y, int dx, ;
// int dy, COLORREF rgbBk, COLORREF rgbFg, UINT fStyle);

HB_FUNC( VWN_IMAGELIST_DRAWEX )
{
   hb_retl(  ImageList_DrawEx((HIMAGELIST) HB_PARWH( 1 ), hb_parni( 2 )     ,
                              (HDC) HB_PARWH( 3 ), hb_parni(4), hb_parni(5) ,
                              hb_parni(6), hb_parni(7),(COLORREF)hb_parnl(8),
                              (COLORREF) hb_parnl(9), (UINT) hb_parni(10)));
}

//-----------------------------------------------------------------------------
//  ImageList_DrawIndirect(IMAGELISTDRAWPARAMS* pimldp);

// uses structure

HB_FUNC( VWN_IMAGELIST_DRAWINDIRECT )
{
  IMAGELISTDRAWPARAMS *pimldp  = ( IMAGELISTDRAWPARAMS *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;

   hb_retl(  ImageList_DrawIndirect( pimldp ) );
}

//-----------------------------------------------------------------------------
//  ImageList_Remove(HIMAGELIST himl, int i);

HB_FUNC( VWN_IMAGELIST_REMOVE )
{
   hb_retl( ImageList_Remove((HIMAGELIST) HB_PARWH( 1 ), hb_parni( 2 )));
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI HICON       WINAPI ImageList_GetIcon(HIMAGELIST himl, int i, UINT flags);

HB_FUNC( VWN_IMAGELIST_GETICON )
{
   HB_RETWH( ImageList_GetIcon((HIMAGELIST) HB_PARWH( 1 ),
                                       hb_parni( 2 )            ,
                                       (UINT) hb_parni(3)))     ;
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI HIMAGELIST  WINAPI ImageList_LoadImageA(HINSTANCE hi, LPCSTR lpbmp,;
//                 int cx, int cGrow,(COLORREF)crMask, UINT uType, UINT uFlags);

HB_FUNC( VWN_IMAGELIST_LOADIMAGE )
{
   HB_RETWH( ImageList_LoadImageA( (HINSTANCE) HB_PARWH(1),
                                          HB_ISCHAR(2)?(LPCSTR) hb_parcx(2) : MAKEINTRESOURCE(hb_parni(2))   ,
                                           hb_parni(3)           ,
                                           hb_parni(4)           ,
                                           (COLORREF) hb_parnl(5),
                                           (UINT) hb_parni(6)    ,
                                           (UINT) hb_parni(7)))  ;
}

//-----------------------------------------------------------------------------
//  ImageList_Copy(HIMAGELIST himlDst, int iDst, HIMAGELIST himlSrc, int iSrc, UINT uFlags);

HB_FUNC( VWN_IMAGELIST_COPY  )
{
   hb_retl( ImageList_Copy((HIMAGELIST) HB_PARWH( 1 ),
                           hb_parni( 2 )             ,
                           (HIMAGELIST) HB_PARWH( 3 ),
                           hb_parni( 4 )             ,
                           (UINT) hb_parni(5)))      ;
}

//-----------------------------------------------------------------------------
//  ImageList_BeginDrag(HIMAGELIST himlTrack, int iTrack, int dxHotspot, int dyHotspot);

HB_FUNC( VWN_IMAGELIST_BEGINDRAG )
{
   hb_retl( ImageList_BeginDrag((HIMAGELIST) HB_PARWH( 1 ),
                                 hb_parni( 2 )            ,
                                 hb_parni( 3 )            ,
                                 hb_parni( 4 )))          ;

}

//-----------------------------------------------------------------------------
#if defined(__MINGW32__) || defined(__CYGWIN__) || defined(__WATCOMC__)
void WINAPI ImageList_EndDrag(void);
#else
WINCOMMCTRLAPI void WINAPI ImageList_EndDrag(void);
#endif

HB_FUNC( VWN_IMAGELIST_ENDDRAG )
{
   ImageList_EndDrag();
}

//-----------------------------------------------------------------------------
//  ImageList_DragEnter(HWND hwndLock, int x, int y);

HB_FUNC( VWN_IMAGELIST_DRAGENTER  )
{
   hb_retl( ImageList_DragEnter( (HWND) HB_PARWH(1), hb_parni(2), hb_parni(3)));
}

//-----------------------------------------------------------------------------
//  ImageList_DragLeave(HWND hwndLock);

HB_FUNC( VWN_IMAGELIST_DRAGLEAVE )
{
   hb_retl( ImageList_DragLeave( (HWND) HB_PARWH(1)));
}

//-----------------------------------------------------------------------------
//  ImageList_DragMove(int x, int y);

HB_FUNC( VWN_IMAGELIST_MOVE )
{
   hb_retl( ImageList_DragMove( hb_parni(1), hb_parni(2)));

}

//-----------------------------------------------------------------------------
//  ImageList_SetDragCursorImage(HIMAGELIST himlDrag, int iDrag, int dxHotspot, int dyHotspot);

HB_FUNC( VWN_IMAGELIST_SETDRAGCURSORIMAGE  )
{
   hb_retl( ImageList_SetDragCursorImage((HIMAGELIST) HB_PARWH( 1 ),
                                         hb_parni( 2 )             ,
                                         hb_parni( 3 )             ,
                                         hb_parni( 4 )))           ;
}

//-----------------------------------------------------------------------------
//  ImageList_DragShowNolock(BOOL fShow);

HB_FUNC( VWN_IMAGELIST_DRAGSHOWNOLOCK )
{
   hb_retl( ImageList_DragShowNolock( hb_parl( 1 )));
}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI HIMAGELIST  WINAPI ImageList_GetDragImage(POINT FAR* ppt,POINT FAR* pptHotspot);


HB_FUNC( VWN_IMAGELIST_GETDRAGIMAGE )
{
   POINT pt        ;
   POINT ptHotspot ;
   PHB_ITEM aPt1 = hb_param(1,HB_IT_ARRAY);
   PHB_ITEM aPt2 = hb_param(2,HB_IT_ARRAY);

   if ( Array2Point(aPt1,&pt) )
       if( Array2Point(aPt2,&ptHotspot) )
           HB_RETWH( ImageList_GetDragImage( &pt, &ptHotspot));
}


//-----------------------------------------------------------------------------
//  ImageList_GetIconSize(HIMAGELIST himl, int FAR *cx, int FAR *cy);

HB_FUNC( VWN_IMAGELIST_GETICONSIZE )
{
   int cx ;
   int cy ;

   if ( ImageList_GetIconSize((HIMAGELIST) HB_PARWH( 1 ), &cx, &cy) )
   {
      hb_storni( cx, 2 );
      hb_storni( cy, 3 );
      hb_retl( 1 );
   }
   else
     hb_retl(0);
}

//-----------------------------------------------------------------------------
//  ImageList_SetIconSize(HIMAGELIST himl, int cx, int cy);

HB_FUNC( VWN_IMAGELIST_SETICONSIZE )
{
   hb_retl(  ImageList_SetIconSize((HIMAGELIST) HB_PARWH( 1 )      ,
                                   hb_parni( 2 ), hb_parni( 3 ) ) );

}

//-----------------------------------------------------------------------------
//  ImageList_GetImageInfo(HIMAGELIST himl, int i, IMAGEINFO FAR* pImageInfo);

// returns structure buffer

HB_FUNC( VWN_IMAGELIST_GETIMAGEINFO )
{
   IMAGEINFO ii ;

   if (  ImageList_GetImageInfo((HIMAGELIST) HB_PARWH( 1 ), hb_parni( 2 ), &ii ) )
      hb_retclen( (char*) &ii, sizeof(IMAGEINFO));

}


//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI HIMAGELIST  WINAPI ImageList_Merge(HIMAGELIST himl1, int i1, HIMAGELIST himl2, int i2, int dx, int dy);

HB_FUNC( VWN_IMAGELIST_MERGE )
{
   HB_RETWH( ImageList_Merge((HIMAGELIST) HB_PARWH( 1 ),
                                    hb_parni( 2 )             ,
                                    (HIMAGELIST) HB_PARWH( 3 ),
                                    hb_parni( 4 )             ,
                                    hb_parni( 5 )             ,
                                    hb_parni( 6 )))           ;

}

//-----------------------------------------------------------------------------
// WINCOMMCTRLAPI HIMAGELIST  WINAPI ImageList_Duplicate(HIMAGELIST himl);

HB_FUNC( VWN_IMAGELIST_DUPLICATE )
{
   HB_RETWH( ImageList_Duplicate((HIMAGELIST) HB_PARWH( 1 )));
}
