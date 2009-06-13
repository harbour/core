/*
 * $Id$
 */

// hbwhat
// Printing

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapiitm.h"
#include "hbapiitm.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"



//-----------------------------------------------------------------------------
// HDC StartDoc( hDC, cDocName, [ cFilename ], [ cDataType ], [DI_Flag] )


HB_FUNC( VWN_STARTDOC )
{
   DOCINFO di;
   di.cbSize       = sizeof(DOCINFO);
   di.lpszDocName  = hb_parcx( 2 );
   di.lpszOutput   = (LPTSTR) ( HB_ISNIL( 3 ) ? NULL : hb_parcx( 3 ) );
   di.lpszDatatype = (LPTSTR) ( HB_ISNIL( 4 ) ? NULL : hb_parcx( 4 ) );
   di.fwType       = (DWORD)  ( HB_ISNIL( 5 ) ? 0 : hb_parnl( 5 ) );

   hb_retni( StartDoc( (HDC) HB_PARWH( 1 ), &di ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ENDDOC )
{
   hb_retni(EndDoc( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI AbortDoc(IN HDC);


HB_FUNC( VWN_ABORTDOC )
{
   hb_retni( AbortDoc( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_STARTPAGE )
{
   hb_retni( StartPage( (HDC) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ENDPAGE )
{
   hb_retni( EndPage( (HDC) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI Escape( IN HDC, IN int, IN int, IN LPCSTR, OUT LPVOID);

/*

HB_FUNC( VWN_ESCAPE )
{
   LPVOID lpVoid ;

   // Your code goes here

   hb_retni( Escape( (HDC) HB_PARWH( 1 )  ,
                     hb_parni( 2 )        ,
                     hb_parni( 3 )        ,
                     (LPCSTR) hb_parcx( 4 ),
                     lpVoid
                     ) );
}

*/
//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI ExtEscape( IN HDC, IN int, IN int, IN LPCSTR, IN int, OUT LPSTR);

/*
HB_FUNC( VWN_EXTESCAPE )
{
   hb_retni( ExtEscape( (HDC) HB_PARWH( 1 )  ,
                        hb_parni( 2 )        ,
                        hb_parni( 3 )        ,
                        (LPCSTR) hb_parcx( 4 ),
                        hb_parni( 5 )        ,
                        (LPSTR) hb_parcx( 6 )
                        ) );
}

*/
