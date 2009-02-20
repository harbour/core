/*
 * $Id$
 */

// hbwhat
// ToolBar functions


#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

//#include <shlobj.h>
#include <windows.h>
#include "hbapiitm.h"
#include <commctrl.h>
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
// HBITMAP CreateMappedBitmap(HINSTANCE hInstance, int idBitmap, UINT wFlags,
//                            LPCOLORMAP lpColorMap, int iNumMaps )

HB_FUNC( VWN_CREATEMAPPEDBITMAP )
{
   COLORMAP *cm = (COLORMAP *) hb_parc( 4 ); //hb_param(4, HB_IT_STRING)->item.asString.value;

   HB_RETWH( CreateMappedBitmap( (HINSTANCE) HB_PARWH(1),
                                       (int) hb_parni(2),
                                       (UINT) hb_parni(3),
                                       ISNIL(4) ? NULL : (COLORMAP *) cm ,
                                       (int) hb_parni(5) ) );
}

//-----------------------------------------------------------------------------
//  HWND CreateToolbarEx( HWND hwnd, DWORD ws, UINT wID,int nBitmaps,HINSTANCE hBMInst,
//                        UINT wBMID,LPCTBBUTTON lpButtons,int iNumButtons,int dxButton,
//                        int dyButton, int dxBitmap, int dyBitmap,UINT uStructSize );

HB_FUNC( VWN_CREATETOOLBAREX )
{

   HB_RETWH( CreateToolbarEx( (HWND) HB_PARWH(1),    // parent
                                     (DWORD)hb_parnl(2),      // style
                                     (UINT) hb_parni(3),      // id,
                                     (int) hb_parni(4),       // number of btn images in bmp
                                     ISNIL(5) ? NULL : (HINSTANCE) HB_PARWH(5), // hInst of bmp
                                     (UINT) hb_parnl(6),      // resource id, or hBmp handle
                                     (LPCTBBUTTON) hb_parcx(7),// array of button structures
                                     (int) hb_parni(8),         // number of buttons to add
                                     (int) hb_parni(9),         // width of button
                                     (int) hb_parni(10),        // height of button
                                     (int) hb_parni(11),        // width of bitmap
                                     (int) hb_parni(12),            // height of bitmap
                                     (UINT) hb_parni(13) ) );   // size of TBBUTTON

}


//-----------------------------------------------------------------------------


HB_FUNC( VWN_GETTOOLBARITEMRECT )
{
   RECT    rc = {0,0,0,0};
   PHB_ITEM aRect ;
   SendMessage((HWND) HB_PARWH(1), TB_GETITEMRECT, hb_parni(2), (LPARAM)&rc);
//   MapWindowPoints((HWND) HB_PARWH(1), HWND_DESKTOP, (POINT*)&rc, 2);
   aRect = Rect2Array( &rc  );
   hb_itemReturn( aRect );
   hb_itemRelease( aRect );
}


//-----------------------------------------------------------------------------
