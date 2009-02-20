/*
 * $Id$
 */

// hbwhat

// Graphic Pen functions


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

//-----------------------------------------------------------------------------
// WINGDIAPI HPEN WINAPI CreatePen( IN int, IN int, IN COLORREF);

HB_FUNC( VWN_CREATEPEN )
{
   HB_RETWH( CreatePen(
               hb_parni( 1 ),   // pen style
               hb_parni( 2 ),   // pen width
               (COLORREF) hb_parnl( 3 ) // pen color
             ) );
}

//-----------------------------------------------------------------------------
// WINGDIAPI HPEN WINAPI CreatePenIndirect( IN CONST LOGPEN *);

/*

HB_FUNC( VWN_CREATEPENINDIRECT )
{
   CONST LOGPEN ;

   // Your code goes here

   HB_RETWH( CreatePenIndirect( &LOGPEN ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI COLORREF WINAPI GetDCPenColor( IN HDC);

// NT ?

/*

HB_FUNC( VWN_GETDCPENCOLOR )
{
   hb_retnl( (ULONG) GetDCPenColor( (HDC) HB_PARWH( 1 ) ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI COLORREF WINAPI SetDCPenColor(IN HDC, IN COLORREF);

// NT ?

/*

HB_FUNC( VWN_SETDCPENCOLOR )
{

   hb_retnl( (ULONG) SetDCPenColor( (HDC) HB_PARWH( 1 ), (COLORREF) hb_parnl( 2 ) ) );
}

*/

//-----------------------------------------------------------------------------
// WINGDIAPI HPEN WINAPI ExtCreatePen(IN DWORD, IN DWORD, IN CONST LOGBRUSH *, IN DWORD, IN CONST DWORD *);

/*

HB_FUNC( VWN_EXTCREATEPEN )
{
   CONST LOGBRUSH ;
   CONST dWord4   ;

   // Your code goes here

   HB_RETWH( ExtCreatePen( (DWORD) hb_parnl( 1 ),
                                  (DWORD) hb_parnl( 2 ),
                                  &LOGBRUSH            ,
                                  (DWORD) hb_parnl( 4 ),
                                  &&dWord4
                                  ) );
}

*/
