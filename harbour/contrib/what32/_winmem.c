/*
 * $Id$
 */

// Memory management


#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalAlloc( IN UINT uFlags, IN SIZE_T dwBytes );

HB_FUNC( GLOBALALLOC )
{
   hb_retnl( (LONG) GlobalAlloc( (UINT) hb_parni( 1 ),(SIZE_T) hb_parnl(2) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalReAlloc( IN HGLOBAL hMem, IN SIZE_T dwBytes, IN UINT uFlags );

HB_FUNC( GLOBALREALLOC )
{

   hb_retnl( (LONG) GlobalReAlloc( (HGLOBAL) hb_parnl( 1 ),
                                   (SIZE_T) hb_parnl( 2 )   ,
                                   (UINT) hb_parni( 3 )
                                   ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI GlobalSize( IN HGLOBAL hMem );

HB_FUNC( GLOBALSIZE )
{
   hb_retnl( (LONG) GlobalSize( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GlobalFlags( IN HGLOBAL hMem );

HB_FUNC( GLOBALFLAGS )
{
   hb_retni( (UINT) GlobalFlags( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI GlobalLock( IN HGLOBAL hMem );

HB_FUNC( GLOBALLOCK )
{
   hb_retnl( (LONG) GlobalLock( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalHandle( IN LPCVOID pMem );

HB_FUNC( GLOBALHANDLE )
{
   hb_retnl( (LONG) GlobalHandle( (LPCVOID) hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalUnlock( IN HGLOBAL hMem );


HB_FUNC( GLOBALUNLOCK )
{
   hb_retl( GlobalUnlock( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalFree( IN HGLOBAL hMem );


HB_FUNC( GLOBALFREE )
{
   hb_retnl( (LONG) GlobalFree( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI GlobalCompact( IN DWORD dwMinFree );

// Not for 32 bit

HB_FUNC( GLOBALCOMPACT )
{
// (SIZE_T) GlobalCompact( (DWORD) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalFix( IN HGLOBAL hMem );

// Not for 32 bit

HB_FUNC( GLOBALFIX )
{
   GlobalFix( (HGLOBAL) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalUnfix( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( GLOBALUNFIX )
{
   GlobalUnfix( (HGLOBAL) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI GlobalWire( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( GLOBALWIRE )
{
   hb_retnl( (LONG) GlobalWire( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalUnWire( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( GLOBALUNWIRE )
{
   hb_retl( GlobalUnWire( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalMemoryStatus( IN OUT LPMEMORYSTATUS lpBuffer );


// T.B.D.


/*

HB_FUNC( GLOBALMEMORYSTATUS )
{
   LPMEMORYSTATUS lpBuffer ;

   // Your code goes here

   GlobalMemoryStatus( lpBuffer ) ;
}

	Last change:  WN   29 May 2002   11:26 pm
*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalMemoryStatusEx( IN OUT LPMEMORYSTATUSEX lpBuffer );


// T.B.D.

/*

HB_FUNC( GLOBALMEMORYSTATUSEX )
{
   LPMEMORYSTATUSEX lpBuffer ;

   // Your code goes here

   hb_retl( GlobalMemoryStatusEx( lpBuffer ) ) ;
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalAlloc( IN UINT uFlags, IN SIZE_T uBytes );

HB_FUNC( LOCALALLOC )
{
   hb_retnl( (LONG) LocalAlloc( (UINT) hb_parni( 1 ), (SIZE_T) hb_parni( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalReAlloc( IN HLOCAL hMem, IN SIZE_T uBytes, IN UINT uFlags );

HB_FUNC( LOCALREALLOC )
{

   hb_retnl( (LONG) LocalReAlloc( (HLOCAL) hb_parnl( 1 ),
                                  (SIZE_T) hb_parni( 2 )         ,
                                  (UINT) hb_parni( 3 )  
                                  ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI LocalLock( IN HLOCAL hMem );


HB_FUNC( LOCALLOCK )
{
   hb_retnl( (LONG) LocalLock( (HLOCAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalHandle( IN LPCVOID pMem );


HB_FUNC( LOCALHANDLE )
{
   hb_retnl( (LONG) LocalHandle( (LPCVOID) hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI LocalUnlock( IN HLOCAL hMem );


HB_FUNC( LOCALUNLOCK )
{
   hb_retl( LocalUnlock( (HLOCAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalSize( IN HLOCAL hMem );


HB_FUNC( LOCALSIZE )
{
   hb_retni( LocalSize( (HLOCAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI LocalFlags( IN HLOCAL hMem );


HB_FUNC( LOCALFLAGS )
{
   hb_retni( LocalFlags( (HLOCAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalFree( IN HLOCAL hMem );


HB_FUNC( LOCALFREE )
{
   hb_retnl( (LONG) LocalFree( (HLOCAL) hb_parnl( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalShrink( IN HLOCAL hMem, IN UINT cbNewSize );


HB_FUNC( LOCALSHRINK )
{
   hb_retni( LocalShrink( (HLOCAL) hb_parnl( 1 ), (UINT) hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalCompact( IN UINT uMinFree );


HB_FUNC( LOCALCOMPACT )
{
   hb_retni( LocalCompact( (UINT) hb_parni( 1 ) ) ) ;
}



