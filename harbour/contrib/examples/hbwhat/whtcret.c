/*
 * $Id$
 */

/*
   Caret functions
   Last change:  WN   27 May 2002    8:41 pm
*/

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapiitm.h"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );


//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETCARETBLINKTIME )
{
   hb_retni( GetCaretBlinkTime() );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SETCARETBLINKTIME )
{
   hb_retl( SetCaretBlinkTime( (UINT) hb_parni(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETCARETX )
{
   POINT ptPoint ;
   GetCaretPos( (LPPOINT) &ptPoint );
   hb_retnl( ptPoint.x );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETCARETY )
{
   POINT ptPoint ;
   GetCaretPos( (LPPOINT) &ptPoint );
   hb_retnl( ptPoint.y );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETCARETPOS )
{
   POINT Point ;
   PHB_ITEM aPt;

   if ( GetCaretPos( (LPPOINT) &Point ) )
   {
      aPt = Point2Array(&Point);
      hb_itemReturn( aPt );
      hb_itemRelease( aPt );
   }

}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_SETCARETPOS )
{

   hb_retl( SetCaretPos( hb_parni(1), hb_parni(2) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SHOWCARET )
{
   hb_retl( ShowCaret( (HWND) HB_PARWH(1) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_HIDECARET )
{
   hb_retl( HideCaret( (HWND) HB_PARWH(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_CREATECARET )
{
   hb_retl( CreateCaret( (HWND) HB_PARWH(1)  ,
                         (HBITMAP) HB_PARWH(2),
                         (int) hb_parni(3) ,
                         (int) hb_parni(4) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_DESTROYCARET )
{
  hb_retl( DestroyCaret() );
}

//-----------------------------------------------------------------------------
