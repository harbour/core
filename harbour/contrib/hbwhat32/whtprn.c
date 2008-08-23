/*
 * $Id$
 */

// what32.lib
// Printing

#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "item.api"
#include "hbapiitm.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"



//-----------------------------------------------------------------------------
// HDC StartDoc( hDC, cDocName, [ cFilename ], [ cDataType ], [DI_Flag] )


HB_FUNC( STARTDOC )
{
   DOCINFO di;
   di.cbSize       = sizeof(DOCINFO);
   di.lpszDocName  = hb_parcx( 2 );
   di.lpszOutput   = (LPTSTR) ( ISNIL( 3 ) ? NULL : hb_parcx( 3 ) ) ;
   di.lpszDatatype = (LPTSTR) ( ISNIL( 4 ) ? NULL : hb_parcx( 4 ) ) ;
   di.fwType       = (DWORD)  ( ISNIL( 5 ) ? 0 : hb_parnl( 5 ) );

   hb_retnl( (LONG) StartDoc( (HDC) hb_parnl( 1 ), &di ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( ENDDOC )
{
   hb_retni(EndDoc( (HDC) hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI AbortDoc(IN HDC);


HB_FUNC( ABORTDOC )
{
   hb_retni( AbortDoc( (HDC) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( STARTPAGE )
{
   hb_retnl( (LONG) StartPage( (HDC) hb_parnl( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( ENDPAGE )
{
   hb_retnl( (LONG) EndPage( (HDC) hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI Escape( IN HDC, IN int, IN int, IN LPCSTR, OUT LPVOID);

/*

HB_FUNC( ESCAPE )
{
   LPVOID lpVoid ;

   // Your code goes here

   hb_retni( Escape( (HDC) hb_parnl( 1 )  ,
                     hb_parni( 2 )        ,
                     hb_parni( 3 )        ,
                     (LPCSTR) hb_parcx( 4 ),
                     lpVoid
                     ) ) ;
}

*/
//-----------------------------------------------------------------------------
// WINGDIAPI int WINAPI ExtEscape( IN HDC, IN int, IN int, IN LPCSTR, IN int, OUT LPSTR);

/*
HB_FUNC( EXTESCAPE )
{
   hb_retni( ExtEscape( (HDC) hb_parnl( 1 )  ,
                        hb_parni( 2 )        ,
                        hb_parni( 3 )        ,
                        (LPCSTR) hb_parcx( 4 ),
                        hb_parni( 5 )        ,
                        (LPSTR) hb_parcx( 6 )
                        ) ) ;
}

*/




