/*
 * $Id$
 */

// Memory management


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


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalAlloc( IN UINT uFlags, IN SIZE_T dwBytes );

HB_FUNC( VWN_GLOBALALLOC )
{
   HB_RETWH( GlobalAlloc( (UINT) hb_parni( 1 ),(SIZE_T) hb_parnl(2) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalReAlloc( IN HGLOBAL hMem, IN SIZE_T dwBytes, IN UINT uFlags );

HB_FUNC( VWN_GLOBALREALLOC )
{

   HB_RETWH( GlobalReAlloc( (HGLOBAL) HB_PARWH( 1 ),
                                   (SIZE_T) hb_parnl( 2 )   ,
                                   (UINT) hb_parni( 3 )
                                   ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI GlobalSize( IN HGLOBAL hMem );

HB_FUNC( VWN_GLOBALSIZE )
{
   hb_retnl( (LONG) GlobalSize( (HGLOBAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GlobalFlags( IN HGLOBAL hMem );

HB_FUNC( VWN_GLOBALFLAGS )
{
   hb_retni( (UINT) GlobalFlags( (HGLOBAL) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI GlobalLock( IN HGLOBAL hMem );

HB_FUNC( VWN_GLOBALLOCK )
{
   HB_RETWH( GlobalLock( (HGLOBAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalHandle( IN LPCVOID pMem );

HB_FUNC( VWN_GLOBALHANDLE )
{
   HB_RETWH( GlobalHandle( (LPCVOID) HB_PARWH(1) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalUnlock( IN HGLOBAL hMem );


HB_FUNC( VWN_GLOBALUNLOCK )
{
   hb_retl( GlobalUnlock( (HGLOBAL) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI HGLOBAL WINAPI GlobalFree( IN HGLOBAL hMem );


HB_FUNC( VWN_GLOBALFREE )
{
   HB_RETWH( GlobalFree( (HGLOBAL) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI GlobalCompact( IN DWORD dwMinFree );

// Not for 32 bit

HB_FUNC( VWN_GLOBALCOMPACT )
{
// (SIZE_T) GlobalCompact( (DWORD) hb_parnl( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalFix( IN HGLOBAL hMem );

// Not for 32 bit

HB_FUNC( VWN_GLOBALFIX )
{
   GlobalFix( (HGLOBAL) HB_PARWH( 1 ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalUnfix( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( VWN_GLOBALUNFIX )
{
   GlobalUnfix( (HGLOBAL) HB_PARWH( 1 ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI GlobalWire( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( VWN_GLOBALWIRE )
{
   HB_RETWH( GlobalWire( (HGLOBAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalUnWire( IN HGLOBAL hMem );

// not for 32 bit

HB_FUNC( VWN_GLOBALUNWIRE )
{
   hb_retl( GlobalUnWire( (HGLOBAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI GlobalMemoryStatus( IN OUT LPMEMORYSTATUS lpBuffer );


// T.B.D.


/*

HB_FUNC( VWN_GLOBALMEMORYSTATUS )
{
   LPMEMORYSTATUS lpBuffer ;

   // Your code goes here

   GlobalMemoryStatus( lpBuffer );
}

   Last change:  WN   29 May 2002   11:26 pm
*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GlobalMemoryStatusEx( IN OUT LPMEMORYSTATUSEX lpBuffer );


// T.B.D.

/*

HB_FUNC( VWN_GLOBALMEMORYSTATUSEX )
{
   LPMEMORYSTATUSEX lpBuffer ;

   // Your code goes here

   hb_retl( GlobalMemoryStatusEx( lpBuffer ) );
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalAlloc( IN UINT uFlags, IN SIZE_T uBytes );

HB_FUNC( VWN_LOCALALLOC )
{
   HB_RETWH( LocalAlloc( (UINT) hb_parni( 1 ), (SIZE_T) hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalReAlloc( IN HLOCAL hMem, IN SIZE_T uBytes, IN UINT uFlags );

HB_FUNC( VWN_LOCALREALLOC )
{

   HB_RETWH( LocalReAlloc( (HLOCAL) HB_PARWH( 1 ),
                                  (SIZE_T) hb_parni( 2 )         ,
                                  (UINT) hb_parni( 3 )
                                  ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPVOID WINAPI LocalLock( IN HLOCAL hMem );


HB_FUNC( VWN_LOCALLOCK )
{
   HB_RETWH( LocalLock( (HLOCAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalHandle( IN LPCVOID pMem );


HB_FUNC( VWN_LOCALHANDLE )
{
   HB_RETWH( LocalHandle( (LPCVOID) HB_PARWH(1) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI LocalUnlock( IN HLOCAL hMem );


HB_FUNC( VWN_LOCALUNLOCK )
{
   hb_retl( LocalUnlock( (HLOCAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalSize( IN HLOCAL hMem );


HB_FUNC( VWN_LOCALSIZE )
{
   hb_retni( LocalSize( (HLOCAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI LocalFlags( IN HLOCAL hMem );


HB_FUNC( VWN_LOCALFLAGS )
{
   hb_retni( LocalFlags( (HLOCAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI HLOCAL WINAPI LocalFree( IN HLOCAL hMem );


HB_FUNC( VWN_LOCALFREE )
{
   HB_RETWH( LocalFree( (HLOCAL) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalShrink( IN HLOCAL hMem, IN UINT cbNewSize );


HB_FUNC( VWN_LOCALSHRINK )
{
   hb_retni( LocalShrink( (HLOCAL) HB_PARWH( 1 ), (UINT) hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI SIZE_T WINAPI LocalCompact( IN UINT uMinFree );


HB_FUNC( VWN_LOCALCOMPACT )
{
   hb_retni( LocalCompact( (UINT) hb_parni( 1 ) ) );
}
