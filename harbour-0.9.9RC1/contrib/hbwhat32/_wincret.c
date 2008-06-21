/*
 * $Id$
 */

/*
   Caret functions
   Last change:  WN   27 May 2002    8:41 pm
*/

#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "item.api"

extern PHB_ITEM Rect2Array( RECT *rc  );
extern BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
extern PHB_ITEM Point2Array( POINT *pt  );
extern BOOL Array2Point(PHB_ITEM aPoint, POINT *pt );


//-----------------------------------------------------------------------------

HB_FUNC( GETCARETBLINKTIME )
{
   hb_retni( GetCaretBlinkTime() );
}

//-----------------------------------------------------------------------------

HB_FUNC( SETCARETBLINKTIME )
{
   hb_retl( SetCaretBlinkTime( (UINT) hb_parni(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( GETCARETX )
{
   POINT ptPoint ;
   GetCaretPos( (LPPOINT) &ptPoint ) ;
   hb_retnl( ptPoint.x );
}

//-----------------------------------------------------------------------------

HB_FUNC( GETCARETY )
{
   POINT ptPoint ;
   GetCaretPos( (LPPOINT) &ptPoint ) ;
   hb_retnl( ptPoint.y );
}


//-----------------------------------------------------------------------------

HB_FUNC( GETCARETPOS )
{
   POINT Point ;
   PHB_ITEM aPt;
   
   if ( GetCaretPos( (LPPOINT) &Point ) ) 
   {
      aPt = Point2Array(&Point) ;
      _itemReturn( aPt );
      _itemRelease( aPt );
   }
  
}


//-----------------------------------------------------------------------------

HB_FUNC( SETCARETPOS )
{

   hb_retl( SetCaretPos( hb_parni(1), hb_parni(2) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( SHOWCARET )
{
   hb_retl( ShowCaret( (HWND) hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( HIDECARET )
{
   hb_retl( HideCaret( (HWND) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( CREATECARET )
{
   hb_retl( CreateCaret( (HWND) hb_parnl(1)  ,
                         (HBITMAP) hb_parnl(2),
                         (int) hb_parni(3) ,
                         (int) hb_parni(4) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( DESTROYCARET )
{
  hb_retl( DestroyCaret() ) ;
}



//-----------------------------------------------------------------------------





