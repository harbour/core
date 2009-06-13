/*
 * $Id$
 */

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                 Pritpal Bedi <pritpal@vouchcac.com>
//                Serial Communication WinApi Functions
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

//-------------------------------------------------------------------//

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//-------------------------------------------------------------------//
/*
BOOL BuildCommDCB(
  LPCTSTR lpDef,  // device-control string                           IN
  LPDCB   lpDCB   // device-control block                           OUT
);
//
local dcb IS DCB
local cComParam := 'COM1: baud=9600 parity=N data=8 stop=1'
local dcbInfo   := dcb:value

BuildComm( cComParam, @dcbInfo )
dcb:buffer( dcbInfo )
*/
HB_FUNC( VWN_BUILDCOMMDCB )
{
   DCB dcb ;

   hb_retl( BuildCommDCB( ( LPCTSTR ) hb_parcx( 1 ), &dcb ) );

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 );
}

//-------------------------------------------------------------------//
/*
BOOL BuildCommDCBAndTimeouts(
  LPCTSTR         lpDef,          // device-control string           IN
  LPDCB           lpDCB,          // device-control block           OUT
  LPCOMMTIMEOUTS  lpCommTimeouts  // device time-out values          IN
);
local dcb IS DCB
local CommTimeOuts IS COMMTIMEOUTS
local dcbInfo := dcb:value
local cComParam := 'COM1: baud=9600 parity=N data=8 stop=1 to=ON'

BuildComDCBAndTimeouts( cCommParam, @dcbInfo, CommTimeOuts:value )
dcb:buffer( dcbInfo )
*/
//
HB_FUNC( VWN_BUILDCOMMDCBANDTIMEOUTS )
{
   DCB dcb ;
   LPCOMMTIMEOUTS lptimeouts = ( LPCOMMTIMEOUTS ) hb_parcx( 3 );
   hb_retl( BuildCommDCBAndTimeouts( ( LPCTSTR ) hb_parcx( 1 ), &dcb, lptimeouts ) );

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 );
}

//-------------------------------------------------------------------//
/*
BOOL ClearCommBreak(
  HANDLE hFile   // handle to communications device                  IN
);
//
local hFile := CreateFile( ... )
if ClearCommBreak( hFile )
   // Your code goes here
endif
*/
HB_FUNC( VWN_CLEARCOMMBREAK )
{
   hb_retl( ClearCommBreak( ( HANDLE ) HB_PARWH( 1 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL ClearCommError(
  HANDLE     hFile,     // handle to communications device           IN
  LPDWORD    lpErrors,  // error codes                              OUT
  LPCOMSTAT  lpStat     // communications status                    OUT
);
if ClearCommError( hFile, @nError, @cComStat )
   // Proceed with fresh i/o
endif
*/
HB_FUNC( VWN_CLEARCOMMERROR )
{
   DWORD   err = 0 ;
   COMSTAT Stat ;

   hb_retl( ClearCommError( ( HANDLE ) HB_PARWH( 1 ), &err, &Stat ) );

   hb_stornl( err, 2 );
   hb_storclen( ( char * ) &Stat, sizeof( COMSTAT ), 3 );
}

//-------------------------------------------------------------------//
/*
BOOL CommConfigDialog(
  LPCTSTR lpszName,   // device name string                          IN
  HWND hWnd,          // handle to window                            IN
  LPCOMMCONFIG lpCC   // configuration information               IN/OUT
);
local cDeviceName := 'Standard Modem over IR link #4'
local hWnd        := nil
local CommConfig IS COMMCONFIG
local cCommConfig := CommConfig:value

if CommConfigDialog( cDeviceName, hWnd, @cCommConfig )
   ? 'Hurray'
   CommConfig:buffer( cCommConfig )
endif
*/
HB_FUNC( VWN_COMMCONFIGDIALOG )
{
   LPCTSTR      lpszName = ( LPCTSTR ) hb_parcx( 1 );
   HWND         hwnd     = HB_ISNIL( 2 ) ? NULL : ( HWND ) HB_PARWH( 2 );
   LPCOMMCONFIG lpCC     = ( LPCOMMCONFIG ) hb_parcx( 3 );

   hb_retl( CommConfigDialog( lpszName, hwnd, lpCC ) );

   hb_storclen( ( char * ) lpCC, sizeof( COMMCONFIG ), 3 );
}

//-------------------------------------------------------------------//
/*
BOOL EscapeCommFunction(
  HANDLE hFile,   // handle to communications device                 IN
  DWORD  dwFunc   // extended function to perform                    IN
);
local nFunc := CLRDTR  // CLRRTS, SETDTR, SETRTS, SETXOFF, SETXON, SETBREAK, CLRBREAK - one of these values

if EscapeCommFunction( hFile, nFunc )
   // ok
endif
*/
HB_FUNC( VWN_ESCAPECOMMFUNCTION )
{
   hb_retl( EscapeCommFunction( ( HANDLE ) HB_PARWH( 1 ), hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommConfig(
  HANDLE hCommDev,    // handle to communications service            IN
  LPCOMMCONFIG lpCC,  // configuration information                  OUT
  LPDWORD lpdwSize    // size of buffer                          IN/OUT
);
if GetCommConfig( hFile, @cCommConfig )
   CommConfig:buffer( cCommConfig )
endif
*/
HB_FUNC( VWN_GETCOMMCONFIG )
{
   COMMCONFIG lpCC ; // = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = sizeof( COMMCONFIG );

   hb_retl( GetCommConfig( ( HANDLE ) HB_PARWH( 1 ), &lpCC, &size ) );

   hb_storclen( ( char * ) &lpCC, size, 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommMask(
  HANDLE  hFile,      // handle to communications device             IN
  LPDWORD lpEvtMask   // event mask                                 OUT
);
if GetCommMask( hFile, @nMask )
   if nMask == EV_BREAK + EV_CTS + ....
   endif
endif
*/
HB_FUNC( VWN_GETCOMMMASK )
{
   DWORD mask;
   hb_retl( GetCommMask( ( HANDLE ) HB_PARWH( 1 ), &mask ) );
   hb_stornl( ( ULONG ) mask, 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommModemStatus(
  HANDLE  hFile,        // handle to communications device           IN
  LPDWORD lpModemStat   // control-register values                  OUT
);
if GetCommModemStatus( hFile, @nStat )
   if nStat == MS_CTS_ON + MS_DSR_ON ...
   endif
endif
*/
HB_FUNC( VWN_GETCOMMMODEMSTATUS )
{
   DWORD modemStat ;
   hb_retl( GetCommModemStatus( ( HANDLE ) HB_PARWH( 1 ), &modemStat ) );
   hb_stornl( ( ULONG ) modemStat, 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommProperties(
  HANDLE     hFile,       // handle to comm device                   IN
  LPCOMMPROP lpCommProp   // communications properties              OUT
);
local CommProp IS COMMPROP
local cCommProp := CommProp:value
GetCommProperties( hFile, @cCommProp )
CommProp:buffer( cCommProp )
*/
HB_FUNC( VWN_GETCOMMPROPERTIES )
{
   COMMPROP CommProp ;
   CommProp.wPacketLength = sizeof( COMMPROP );

   hb_retl( GetCommProperties( ( HANDLE ) HB_PARWH( 1 ), &CommProp ) );

   hb_storclen( ( char * ) &CommProp, sizeof( COMMPROP ), 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommState(
  HANDLE hFile,  // handle to communications device                  IN
  LPDCB  lpDCB   // device-control block                            OUT
);
GetCommState( hFile, @cDcb )
dcb:buffer( cDcb )
*/
HB_FUNC( VWN_GETCOMMSTATE )
{
   DCB dcb ;
   dcb.DCBlength = sizeof( DCB );

   hb_retl( GetCommState( ( HANDLE ) HB_PARWH( 1 ), &dcb ) );

   hb_storclen( ( char * ) &dcb, sizeof( DCB ), 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetCommTimeouts(
  HANDLE         hFile,          // handle to comm device            IN
  LPCOMMTIMEOUTS lpCommTimeouts  // time-out values                 OUT
);
GetCommTimeouts( cFile, @cCommTimeouts )
CommTimeouts:buffer( cCommTimeouts )
*/
HB_FUNC( VWN_GETCOMMTIMEOUTS )
{
   COMMTIMEOUTS Timeouts ;

   hb_retl( GetCommTimeouts( ( HANDLE ) HB_PARWH( 1 ), &Timeouts ) );

   hb_storclen( ( char * ) &Timeouts, sizeof( COMMTIMEOUTS ), 2 );
}

//-------------------------------------------------------------------//
/*
BOOL GetDefaultCommConfig(
  LPCTSTR      lpszName,    // device name string                    IN
  LPCOMMCONFIG lpCC,        // configuration information            OUT
  LPDWORD      lpdwSize     // size of buffer                        IN
);
GetDefaultCommConfig( 'Standard Modem over IR link #4', @cCommConfig )
CommConfig:buffer( cCommConfig )
*/
HB_FUNC( VWN_GETDEFAULTCOMMCONFIG )
{
   char * Buffer = (char *) hb_xgrab( sizeof( COMMCONFIG ) );
   DWORD size = sizeof( COMMCONFIG );

   if ( GetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), ( COMMCONFIG * ) Buffer, &size ) == 0 )
   {
      hb_xfree( Buffer );
      Buffer = (char *) hb_xgrab( size );
      if ( GetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), ( COMMCONFIG * ) Buffer, &size ) == 0 )
      {
         hb_xfree( Buffer );
         hb_retl( FALSE );
         return ;
      }
   }
   hb_retl( TRUE );
   hb_storclen( ( char * ) Buffer, size, 2 );
   hb_xfree( Buffer );
}
//-------------------------------------------------------------------//
/*
BOOL PurgeComm(
  HANDLE hFile,  // handle to communications resource                IN
  DWORD dwFlags  // action to perform                                IN
);
local nActions := PURGE_TXABORT + PURGE_RXABORT // + ... ANY COMBINATION
if PurgeComm( hFile, nActions )
endif
*/
HB_FUNC( VWN_PURGECOMM )
{
   hb_retl( PurgeComm( ( HANDLE ) HB_PARWH( 1 ), hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommBreak(
  HANDLE hFile   // handle to communications device                  IN
);
*/
HB_FUNC( VWN_SETCOMMBREAK )
{
   hb_retl( SetCommBreak( ( HANDLE ) HB_PARWH( 1 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommConfig(
  HANDLE hCommDev,    // handle to communications device             IN
  LPCOMMCONFIG lpCC,  // configuration services                      IN
  DWORD dwSize        // size of structure                           IN
);
SetCommConfig( hFile, CommConfig:Value, nSize )
*/
HB_FUNC( VWN_SETCOMMCONFIG )
{
   LPCOMMCONFIG lpCC = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = HB_ISNIL( 3 ) ? ( DWORD ) sizeof( COMMCONFIG ) : ( DWORD ) hb_parnl( 3 );

   hb_retl( SetCommConfig( ( HANDLE ) HB_PARWH( 1 ), lpCC, size ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommMask(
  HANDLE hFile,     // handle to communications device               IN
  DWORD  dwEvtMask  // mask that identifies enabled events           IN
);
if SetCommMask( hFile, nEvtMask )
endif
*/
HB_FUNC( VWN_SETCOMMMASK )
{
   hb_retl( SetCommMask( ( HANDLE ) HB_PARWH( 1 ), hb_parnl( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommState(
  HANDLE hFile,  // handle to communications device                  IN
  LPDCB lpDCB    // device-control block                             IN
);
*/
HB_FUNC( VWN_SETCOMMSTATE )
{
   LPDCB lpDCB = ( LPDCB ) hb_parcx( 2 );

   hb_retl( SetCommState( ( HANDLE ) HB_PARWH( 1 ), lpDCB ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetCommTimeouts(
  HANDLE         hFile,          // handle to comm device            IN
  LPCOMMTIMEOUTS lpCommTimeouts  // time-out values                  IN
);
*/
HB_FUNC( VWN_SETCOMMTIMEOUTS )
{
   LPCOMMTIMEOUTS lptimeouts = ( LPCOMMTIMEOUTS ) hb_parcx( 2 );

   hb_retl( SetCommTimeouts( ( HANDLE ) HB_PARWH( 1 ), lptimeouts ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetDefaultCommConfig(
  LPCTSTR      lpszName, // device name string                       IN
  LPCOMMCONFIG lpCC,     // configuration information                IN
  DWORD        dwSize    // size of structure                        IN
);
*/
HB_FUNC( VWN_SETDEFAULTCOMMCONFIG )
{
   LPCOMMCONFIG lpCC = ( LPCOMMCONFIG ) hb_parcx( 2 );
   DWORD        size = sizeof( COMMCONFIG );

   hb_retl( SetDefaultCommConfig( ( LPCTSTR ) hb_parcx( 1 ), lpCC, size ) );
}

//-------------------------------------------------------------------//
/*
BOOL SetupComm(
  HANDLE hFile,      // handle to communications device              IN
  DWORD  dwInQueue,  // size of input buffer                         IN
  DWORD  dwOutQueue  // size of output buffer                        IN
);
*/
HB_FUNC( VWN_SETUPCOMM )
{
   hb_retl( SetupComm( ( HANDLE ) HB_PARWH( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL TransmitCommChar(
  HANDLE hFile,  // handle to communications device                  IN
  char   cChar   // character to transmit                            IN
);
*/
HB_FUNC( VWN_TRANSMITCOMMCHAR )
{
   hb_retl( TransmitCommChar( ( HANDLE ) HB_PARWH( 1 ), ( char ) hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------//
/*
BOOL WaitCommEvent(
  HANDLE hFile,                // handle to comm device              IN
  LPDWORD lpEvtMask,           // event type mask                   OUT
  LPOVERLAPPED lpOverlapped,   // overlapped structure               IN    Not used here
);
if WaitCommEvent( hFile, @nEvent )
   if nEvent == EV_RSCHAR
      // do the needful
   endif
endif
*/
HB_FUNC( VWN_WAITCOMMEVENT )
{
   DWORD evMask ;

   hb_retl( WaitCommEvent( ( HANDLE ) HB_PARWH( 1 ), &evMask, NULL ) );
   hb_stornl( ( ULONG ) evMask, 2 );
}

//-------------------------------------------------------------------//
