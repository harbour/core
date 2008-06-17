/*
 * $Id$
 */


/*
 * $Id$
 */

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                              WHAT32
//                          System Services
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
/*
 * Some parts Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * with author's permission granted on 27 MAy 2002

   The following Copyright 2003 Ian Anderson <i.anderson@procon.online.de>
   GetConsoleWindow()
   SetConsoleTitle()
   GetProcessWorkingSize()
   SetProcessWorkingSize()
   VirtualQuery()      - may not be functional, always seems to return error
   VirtualLock()       - may not be functional, always seems to return error
 */
//-------------------------------------------------------------------//

#define WINVER         0X0400
#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400

//-------------------------------------------------------------------//

#include <shlobj.h>
#include <windows.h>
#if !defined(__MINGW32__) && !defined(__WATCOMC__)
#include <htmlhelp.h>
#endif

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//-------------------------------------------------------------------//

BOOL PASCAL enable_privilege( LPCTSTR privilege_name );

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI GetFreeSpace(UINT);
//
HB_FUNC( GETFREESPACE )
{
   hb_retnl( (LONG) GetFreeSpace( (UINT) hb_parni( 1 ) ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( OUTPUTDEBUGSTRING )
{
   OutputDebugString( (LPCSTR) hb_parcx( 1 ) ) ;
}

//-------------------------------------------------------------------//
//
// DWORD GetTimeZoneInformation(LPTIME_ZONE_INFORMATION lpTimeZoneInformation)
//
// SYNTAX:
// cTZI:=tzi:value
// GetTimeZoneInformation(@cTZI)
// tzi:Buffer(cTZI)
//
HB_FUNC( GETTIMEZONEINFORMATION )
{
 TIME_ZONE_INFORMATION tzi;

 hb_retnl( GetTimeZoneInformation( &tzi ) ) ;

 if ( ISBYREF(1) )
    hb_storclen( (char*) &tzi, sizeof(tzi), 1);
}

//-------------------------------------------------------------------//
//
// BOOL SetTimeZoneInformation(TIME_ZONE_INFORMATION *TimeZoneInformation)
//
// SYNTAX: SetTimeZoneInformation(tzi:value)
//
HB_FUNC( SETTIMEZONEINFORMATION )
{
 TIME_ZONE_INFORMATION *tzi = ( TIME_ZONE_INFORMATION *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;

 hb_retl( SetTimeZoneInformation( tzi ) ) ;

}

//-------------------------------------------------------------------//
//
// Win98 ++
/*
HB_FUNC( ISDEBUGGERPRESENT )
{
   hb_retl( IsDebuggerPresent() ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI VOID WINAPI DebugBreak( VOID );
//
HB_FUNC( DEBUGBREAK )
{
   DebugBreak(  ) ;
}

//-------------------------------------------------------------------//
//
// WINADVAPI BOOL WINAPI EncryptFileA( IN LPCSTR lpFileName );
//
// NT ?
/*
HB_FUNC( ENCRYPTFILE )
{
   hb_retl( EncryptFileA( (LPCSTR) hb_parcx( 1 ) ) ) ;
}
*/

//-------------------------------------------------------------------//
//
// WINADVAPI BOOL WINAPI DecryptFileA( IN LPCSTR lpFileName, IN DWORD dwReserved );
//
// NT ?
/*
HB_FUNC( DECRYPTFILE )
{
   hb_retl( DecryptFileA( (LPCSTR) hb_parcx( 1 ), 0 ) ) ; //(DWORD) hb_parnl( 2 ) ) ) ;
}
*/

//-------------------------------------------------------------------//
//
// WINADVAPI BOOL WINAPI FileEncryptionStatusA( LPCSTR lpFileName, LPDWORD lpStatus );
/*
// need function info !

HB_FUNC( FILEENCRYPTIONSTATUSA )
{
   LPDWORD lpStatus   ;

   // Your code goes here

   hb_retl( FileEncryptionStatusA( (LPCSTR) hb_parcx( 1 ), lpStatus ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI IsProcessorFeaturePresent( IN DWORD ProcessorFeature );
//
HB_FUNC( ISPROCESSORFEATUREPRESENT )
{
   hb_retl( IsProcessorFeaturePresent( (DWORD) hb_parnl( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI int WINAPI MulDiv( IN int nNumber, IN int nNumerator, IN int nDenominator );

HB_FUNC( MULDIV )
{
   hb_retni( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI SystemParametersInfoA( IN UINT uiAction, IN UINT uiParam, IN OUT PVOID pvParam, IN UINT fWinIni);
//
// note: correct parameters must be passed, as per API requirements
//
HB_FUNC( SYSTEMPARAMETERSINFO )
{
   PHB_ITEM pBuffer = hb_param( 3, HB_IT_STRING );

   if( pBuffer )
   {
      char * cText = (char*) hb_xgrab( hb_itemGetCLen( pBuffer )+1 );
      hb_xmemcpy( cText, hb_itemGetC( pBuffer ), hb_itemGetCLen( pBuffer )+1 );

      if( SystemParametersInfo( (UINT) hb_parni( 1 ),
                                (UINT) hb_parni( 2 ),
                                cText,
                                (UINT) hb_parni( 4 ) ) )
      {
         if( ISBYREF( 3 ) )
         {
            if( ! hb_storclen_buffer( cText, hb_itemGetCLen( pBuffer ), 3 ) )
               hb_xfree( cText );
      
            hb_retl( TRUE );
            return;
         }
      }

      hb_xfree( cText );
   }

   hb_retl( FALSE );
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI FreeResource( IN HGLOBAL hResData );
//
HB_FUNC( FREERESOURCE )
{
   hb_retl( FreeResource( (HGLOBAL) hb_parnl( 6 )) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI VOID WINAPI SetDebugErrorLevel( IN DWORD dwLevel );
//
HB_FUNC( SETDEBUGERRORLEVEL )
{
   SetDebugErrorLevel( (DWORD) hb_parnl( 1 ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI VOID WINAPI SetLastErrorEx( IN DWORD dwErrCode, IN DWORD dwType );
//
HB_FUNC( SETLASTERROREX )
{
   SetLastErrorEx( (DWORD) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ;
}

//-------------------------------------------------------------------//
/*
HANDLE GetStdHandle(DWORD nStdHandle )  // input, output, or error device
*/

HB_FUNC( GETSTDHANDLE )
{
   hb_retnl( (LONG) GetStdHandle( (DWORD) hb_parnl(1) ) ) ;
}

//-------------------------------------------------------------------//
/*
BOOL SetStdHandle(
                  DWORD nStdHandle,  // input, output, or error device
                  HANDLE hHandle     // handle to be a standard handle
);
*/

HB_FUNC( SETSTDHANDLE )
{
   hb_retl( SetStdHandle( (DWORD) hb_parnl(1), (HANDLE) hb_parnl(2) ) ) ;
}

//-------------------------------------------------------------------//
/*
BOOL SetConsoleTitle(LPCSTR szTitle )
*/

HB_FUNC( SETCONSOLETITLE )
{
   hb_retnl( ( LONG ) SetConsoleTitle( ( LPCSTR ) hb_parcx( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
/*
WINBASEAPI HANDLE WINAPI GetConsoleWindow(void);
Note: The real API is only supported on Windows 2000 and above, so we do a nasty
      workaround so it works for earlier systems as well
      1) save the console title text
      2) set it to something (hopefully) unique
      3) use FindWindow to find the window handle of the window with our text
      4) restore the original text
*/
HB_FUNC( GETCONSOLEWINDOW )
{
   char realtitle[ MAX_PATH ];

   GetConsoleTitle( realtitle,MAX_PATH );
   SetConsoleTitle( "Finding Handle" );
   hb_retnl( ( LONG ) FindWindow( NULL,"Finding Handle" ) );
   SetConsoleTitle( realtitle );
}

//-------------------------------------------------------------------//
//
// WINUSERAPI int WINAPI GetSystemMetrics( IN int nIndex);
//
HB_FUNC( GETSYSTEMMETRICS )
{
   hb_retni( GetSystemMetrics( hb_parni( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI UINT_PTR WINAPI SetTimer( IN HWND hWnd, IN UINT_PTR nIDEvent, IN UINT uElapse, IN TIMERPROC lpTimerFunc);
//
HB_FUNC( SETTIMER )
{
   hb_retni( SetTimer( (HWND) hb_parnl( 1 ),
                       (UINT) hb_parni( 2 ),
                       (UINT) hb_parni( 3 ),
                       ISNIL(4) ? NULL : (TIMERPROC) hb_parnl(4)
                      ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI KillTimer( IN HWND hWnd, IN UINT_PTR uIDEvent);
//
HB_FUNC( KILLTIMER )
{
   hb_retl( KillTimer( (HWND) hb_parnl( 1 ), (UINT) hb_parni(2) ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( GETSYSCOLOR )
{
  hb_retnl( GetSysColor( hb_parni(1) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI ExitWindowsEx( IN UINT uFlags, IN DWORD dwReserved);
//
HB_FUNC( EXITWINDOWSEX )
{
   hb_retl( ExitWindowsEx( (UINT) hb_parni( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI HBRUSH WINAPI GetSysColorBrush( IN int nIndex);
//
HB_FUNC( GETSYSCOLORBRUSH )
{
   hb_retnl( (LONG) GetSysColorBrush( hb_parni( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI SetSysColors( IN int cElements, IN CONST INT * lpaElements, IN CONST COLORREF * lpaRgbValues);
/*
HB_FUNC( SETSYSCOLORS )
{
   COLORREF lpaRgbValues ;

   // Your code goes here

   hb_retl( SetSysColors( hb_parni( 1 ), hb_parni( 2 ), &lpaRgbValues ) ) ;
}
*/
//-------------------------------------------------------------------//

HB_FUNC( AND )
{
  hb_retnl( hb_parnl(1) & hb_parnl(2) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( OR )
{
  hb_retnl( hb_parnl(1) | hb_parnl(2) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( NOT )
{
   hb_retnl( ~( hb_parnl(1) ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( _GETINSTANCE )
{
   hb_retnl( (LONG) GetModuleHandle( NULL ) );
}

//-------------------------------------------------------------------//

HB_FUNC( LOWORD )
{
   hb_retni( (int) ( hb_parnl( 1 ) & 0xFFFF ) );
}

//-------------------------------------------------------------------//

HB_FUNC( HIWORD )
{
   hb_retni( (int) ( ( hb_parnl( 1 ) >> 16 ) & 0xFFFF ) );
}

//-------------------------------------------------------------------//

HB_FUNC( MAKELONG )
{
   hb_retnl( (LONG) (((WORD) (hb_parni(1))) | (((DWORD) ((WORD) (hb_parni(2)))) << 16)) ) ;
}

//-------------------------------------------------------------------//
/*
HB_FUNC( GETLASTERROR )
{
  hb_retnl( ( LONG ) GetLastError() ) ;
}
*/
//-------------------------------------------------------------------//
//
// T.B.D.
// returns error message text
/*
HB_FUNC( FORMATMESSAGE)
{
   hb_retnl( FormatMessage( (DWORD) hb_parnl( 1 )            ,  // source and processing options
                            ISNIL( 2) ? NULL : hb_parcx( 2 ) ,  // pointer to  message source
                            (DWORD) hb_parnl( 3 )            ,  // requested message identifier
                            (DWORD) hb_parnl( 4 )            ,  // language identifier for requested message
                            LPTSTR lpBuffer                  ,  // pointer to message buffer
                            DWORD nSize                      ,  // maximum size of message buffer
                            va_list *Arguments                  // pointer to array of message inserts
           );
}
*/

//-------------------------------------------------------------------//
//
// WINBASEAPI VOID WINAPI SetLastError( IN DWORD dwErrCode );
//
/*
HB_FUNC( SETLASTERROR )
{
   SetLastError( (DWORD) hb_parnl( 1 ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI UINT WINAPI SetErrorMode( IN UINT uMode );
//
HB_FUNC( SETERRORMODE )
{
   hb_retni( SetErrorMode( (UINT) hb_parni( 1 ) ) ) ;
}

//-------------------------------------------------------------------//

HB_FUNC( OEMTOCHAR )
{
   int iLen = hb_parclen( 1 );
   char * buffer = ( char* ) hb_xgrab( iLen + 1 );

   OemToCharBuff( hb_parcx( 1 ), buffer, iLen );
   hb_retclenAdopt( buffer, iLen );
}

//-------------------------------------------------------------------//

HB_FUNC( CHARTOOEM )
{
   int iLen = hb_parclen( 1 );
   char * buffer = ( char* ) hb_xgrab( iLen + 1 );

   CharToOemBuff( hb_parcx( 1 ), buffer, iLen );
   hb_retclenAdopt( buffer, iLen );
}

//-------------------------------------------------------------------//

HB_FUNC( OEMTOANSI )
{
HB_FUNCNAME( OEMTOCHAR )();
}

//-------------------------------------------------------------------//

HB_FUNC( ANSITOOEM )
{
HB_FUNCNAME( CHARTOOEM )();
}

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI GetVersion( VOID );

HB_FUNC( GETVERSION )
{
   hb_retnl( (LONG) GetVersion(  ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI HRSRC WINAPI FindResourceA( IN HMODULE hModule, IN LPCSTR lpName, IN LPCSTR lpType );
//
HB_FUNC( FINDRESOURCE )
{
   hb_retnl( (LONG) FindResourceA( (HMODULE) hb_parnl( 1 ),
                                   (LPCSTR) hb_parcx( 2 )  ,
                                   (LPCSTR) hb_parcx( 3 )
                                  ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI HRSRC WINAPI FindResourceExA( IN HMODULE hModule, IN LPCSTR lpType, IN LPCSTR lpName, IN WORD wLanguage );
//
HB_FUNC( FINDRESOURCEEX )
{
   hb_retnl( (LONG) FindResourceExA( (HMODULE) hb_parnl( 1 ),
                                     (LPCSTR) hb_parcx( 2 )  ,
                                     (LPCSTR) hb_parcx( 3 )  ,
                                     (WORD) hb_parni( 4 )
                                     ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI HGLOBAL WINAPI LoadResource( IN HMODULE hModule, IN HRSRC hResInfo );
//
HB_FUNC( LOADRESOURCE )
{
   hb_retnl( (LONG) LoadResource( (HMODULE) hb_parnl( 1 ),
                                  (HRSRC) hb_parnl( 2 )
                                 ) ) ;
}

//-------------------------------------------------------------------//
//
// int LoadString(HINSTANCE hInstance,  // handle to module containing string resource
//                UINT uID,             // resource identifier
//                LPTSTR lpBuffer,      // pointer to buffer for resource
//                int nBufferMax        // size of buffer
//                );
//
// modified
//
HB_FUNC( LOADSTRING )
{
   ULONG iLen = ISNIL(3) ? MAX_PATH : (ULONG) hb_parclen( 3 );
   LPTSTR cText = (char*) hb_xgrab( iLen+1 );

   iLen = LoadString( ( ISNIL(1) ? GetModuleHandle(NULL) : (HINSTANCE) hb_parnl(1) ),
                      (UINT) hb_parni(2) ,
                      (LPTSTR) cText ,
                      iLen ) ;

   hb_retclen( cText, iLen );
   hb_xfree( cText );
}

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI SizeofResource( IN HMODULE hModule, IN HRSRC hResInfo );
//
HB_FUNC( SIZEOFRESOURCE )
{
   hb_retnl( (LONG) SizeofResource( (HMODULE) hb_parnl( 1 ),
                                    (HRSRC) hb_parnl( 2 )
                                    ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI LPVOID WINAPI LockResource( IN HGLOBAL hResData );
//
HB_FUNC( LOCKRESOURCE )
{
   hb_retnl( (LONG) LockResource( (HGLOBAL) hb_parnl( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI LoadModule( IN LPCSTR lpModuleName, IN LPVOID lpParameterBlock );
/*
HB_FUNC( LOADMODULE )
{
   LPVOID lpParameterBlock ;

   // Your code goes here

   hb_retnl( (LONG) LoadModule( (LPCSTR) hb_parcx( 1 ), lpParameterBlock ) ) ;
}
*/

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI Beep( IN DWORD dwFreq, IN DWORD dwDuration );
//
HB_FUNC( TONE )
{
   hb_retl( Beep( (DWORD) hb_parnl( 1 ), (DWORD) hb_parnl( 2 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI GetModuleFileNameA( IN HMODULE hModule, OUT LPSTR lpFilename, IN DWORD nSize );

HB_FUNC( GETMODULEFILENAME )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetModuleFileNameA( ISNIL(1) ? GetModuleHandle(NULL) : (HMODULE) hb_parnl( 1 ),
                       szBuffer  ,
                       MAX_PATH
                     ) ;
   hb_retc(szBuffer);
}

//-------------------------------------------------------------------//
//
// WINBASEAPI HMODULE WINAPI GetModuleHandleA( IN LPCSTR lpModuleName );
//
HB_FUNC( GETMODULEHANDLE )
{
   hb_retnl( (LONG) GetModuleHandleA( (ISNIL(1) ? NULL : (LPCSTR) hb_parcx( 1 ) ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI LPSTR WINAPI GetCommandLineA( VOID );
//
HB_FUNC( GETCOMMANDLINE )
{
   hb_retc( (LPSTR) GetCommandLine() );
}

//-------------------------------------------------------------------//
//
// WINBASEAPI VOID WINAPI GetSystemTime( OUT LPSYSTEMTIME lpSystemTime );
/*
HB_FUNC( GETSYSTEMTIME )
{
   LPSYSTEMTIME lpSystemTime ;

   // Your code goes here

   GetSystemTime( lpSystemTime ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI SetSystemTime( IN CONST SYSTEMTIME *lpSystemTime );
//
/*
HB_FUNC( SETSYSTEMTIME )
{
   SYSTEMTIME CONST lpSystemTime ;

   // Your code goes here

   hb_retl( SetSystemTime( &lpSystemTime ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI VOID WINAPI GetLocalTime( OUT LPSYSTEMTIME lpSystemTime );
/*
HB_FUNC( GETLOCALTIME )
{
   LPSYSTEMTIME lpSystemTime ;

   // Your code goes here

   GetLocalTime( lpSystemTime ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI SetLocalTime( IN CONST SYSTEMTIME *lpSystemTime );
/*
HB_FUNC( SETLOCALTIME )
{
   SYSTEMTIME CONST lpSystemTime ;

   // Your code goes here

   hb_retl( SetLocalTime( &lpSystemTime ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI VOID WINAPI GetSystemInfo( OUT LPSYSTEM_INFO lpSystemInfo );
/*
HB_FUNC( GETSYSTEMINFO )
{
   LPSYSTEM_INFO lpSystemInfo ;

   // Your code goes here

   GetSystemInfo( lpSystemInfo ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI GetTickCount( VOID );
//
HB_FUNC( GETTICKCOUNT )
{
   hb_retnl( (LONG) GetTickCount(  ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI DWORD WINAPI GetLogicalDriveStringsA( IN DWORD nBufferLength, OUT LPSTR lpBuffer );

HB_FUNC( GETLOGICALDRIVESTRINGS )
{
   hb_retnl( (LONG) GetLogicalDriveStrings( (DWORD) hb_parnl( 1 ),
                                             (LPSTR) hb_parcx( 2 )
                                             ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI GetComputerNameA ( OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );
//
//
HB_FUNC( GETCOMPUTERNAME )
{
   char cText[MAX_COMPUTERNAME_LENGTH+1]  ;
   DWORD nSize = MAX_COMPUTERNAME_LENGTH+1;

   hb_retl( GetComputerNameA( (LPSTR) &cText, &nSize ) ) ;

   hb_storc( cText, 1 ) ;
   hb_stornl( nSize, 2 ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI SetComputerNameA ( IN LPCSTR lpComputerName );
//
HB_FUNC( SETCOMPUTERNAME )
{
   hb_retl( SetComputerNameA( (LPCSTR) hb_parcx( 1 ) ) ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI GetComputerNameExA ( IN COMPUTER_NAME_FORMAT NameType, OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );
/*
HB_FUNC( GETCOMPUTERNAMEEX )
{
   COMPUTER_NAME_FORMAT NameType ;
   LPDWORD              nSize    ;

   // Your code goes here

   hb_retl( GetComputerNameExA( NameType, (LPSTR) hb_parcx( 2 ), nSize ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI SetComputerNameExA ( IN COMPUTER_NAME_FORMAT NameType, IN LPCSTR lpBuffer );
/*
HB_FUNC( SETCOMPUTERNAMEEX )
{
   COMPUTER_NAME_FORMAT NameType ;

   // Your code goes here

   hb_retl( SetComputerNameExA( NameType, (LPCSTR) hb_parcx( 2 ) ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINADVAPI BOOL WINAPI GetUserNameA ( OUT LPSTR lpBuffer, IN OUT LPDWORD nSize );
//
HB_FUNC( GETUSERNAME )
{
   DWORD nSize    ;
   char *szUser = hb_parcx( 1 );

   hb_retl( GetUserNameA( szUser, &nSize ) ) ;
   hb_storc( szUser , 1 ) ;
   hb_stornl( ( LONG ) nSize , 2 ) ;
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI GetVersionExA( IN OUT LPOSVERSIONINFOA lpVersionInformation );
//
HB_FUNC( GETVERSIONEX )
{
   BOOL bGetVer;
   OSVERSIONINFOEX osvi;
   osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

   bGetVer = GetVersionEx( (OSVERSIONINFO*) &osvi );

   hb_storclen( (char*) &osvi, sizeof(OSVERSIONINFOEX), 1 );
   hb_retl( bGetVer );
}

//-------------------------------------------------------------------//
//
// WINBASEAPI BOOL WINAPI VerifyVersionInfoA( IN LPOSVERSIONINFOEXA lpVersionInformation, IN DWORD dwTypeMask, IN DWORDLONG dwlConditionMask );
/*
HB_FUNC( VERIFYVERSIONINFO )
{
   LPOSVERSIONINFOEXA lpVersionInformation ;

   // Your code goes here

   hb_retl( VerifyVersionInfoA( lpVersionInformation     ,
                                (DWORD) hb_parnl( 2 )    ,
                                (DWORDLONG) hb_parnl( 3 )
                                ) ) ;
}
*/
//-------------------------------------------------------------------//
//
//         The next 3 functions should go to _WinMDI.c
//
//-------------------------------------------------------------------//
//
// WINUSERAPI UINT WINAPI ArrangeIconicWindows( IN HWND hWnd);
//
HB_FUNC( ARRANGEICONICWINDOWS )
{
   hb_retni( ArrangeIconicWindows( (HWND) hb_parnl( 1 ) ) ) ;
}


//-------------------------------------------------------------------//
//
// WINUSERAPI WORD WINAPI TileWindows( IN HWND hwndParent, IN UINT wHow, IN CONST RECT * lpRect, IN UINT cKids, IN const HWND FAR * lpKids);
//
// move to MDI
/*
HB_FUNC( TILEWINDOWS )
{
   RECT lpRect     ;

   // Your code goes here

   hb_retni( TileWindows( (HWND) hb_parnl( 1 ),
                          (UINT) hb_parni( 2 ),
                          &lpRect             ,
                          (UINT) hb_parni( 4 ),
                          (HWND) hb_parnl( 5 )
                        ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINUSERAPI WORD WINAPI CascadeWindows( IN HWND hwndParent, IN UINT wHow, IN CONST RECT * lpRect, IN UINT cKids, IN const HWND FAR * lpKids);
//
// move to MDI
//
/*
HB_FUNC( CASCADEWINDOWS )
{
   RECT lpRect     ;

   // Your code goes here

   hb_retni( CascadeWindows( (HWND) hb_parnl( 1 ),
                             (UINT) hb_parni( 2 ),
                             &lpRect             ,
                             (UINT) hb_parni( 4 ),
                             (HWND) hb_parnl( 5 )
                           ) ) ;
}
*/
//-------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI WinHelpA( IN HWND hWndMain, IN LPCSTR lpszHelp, IN UINT uCommand, IN ULONG_PTR dwData );
//
// need to verify 4th parameter !
//
HB_FUNC( WINHELP )
{
      hb_retl( WinHelp( (HWND) hb_parnl( 1 ) ,
                     (LPCSTR) hb_parcx( 2 ),
                     (UINT) hb_parni( 3 ) ,
                     (ULONG) hb_parnl( 4 )
                   ) ) ;
}

//-------------------------------------------------------------------//
//
// HWND HtmlHelp(HWND hwndCaller, LPCSTR pszFile, UINT uCommand, DWORD dwData);
//
//
//  HtmlHelp( hWndCaller,        ;  // Handle of caller window, can be GetDeskTopWindow()
//            cFullPathAndTopic  )  // C:\Creative.acp\Help\VVouch.htm::default.htm
//                                  // If topic is not given, default topic will appear
//
//  HtmlHelp( GetDeskTopWindow(), 'c:\help\vvouch.chm::de_windo.htm' )
//
//  To create a .chm file, you need to work with Microsoft's
//  free HtmlHelp Workshop doanloadable from MSDN
//
/*
HB_FUNC( HTMLHELP )
{

 hb_retnl( (LONG) HtmlHelp( (HWND)   hb_parnl( 1 )  ,
                            (LPCSTR) hb_parcx( 2 ) ,
                            (UINT)   ISNIL(3) ? HH_DISPLAY_TOPIC : hb_parni( 3 )  ,
                            (DWORD)  ISNIL(4) ? NULL : hb_parnl( 4 )
                          )
         ) ;
}
*/
//-------------------------------------------------------------------//
/*
 HANDLE CreateFile(
  LPCTSTR lpFileName,          // pointer to name of the file
  DWORD dwDesiredAccess,       // access (read-write) mode
  DWORD dwShareMode,           // share mode
  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
                               // pointer to security attributes
  DWORD dwCreationDisposition,  // how to create
  DWORD dwFlagsAndAttributes,  // file attributes
  HANDLE hTemplateFile         // handle to file with attributes to
                               // copy
);
*/
HB_FUNC( CREATEFILE )
{

   SECURITY_ATTRIBUTES *sa = NULL;

   if( ISCHAR( 4 ) )
      sa = ( SECURITY_ATTRIBUTES *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING )->item.asString.value ;

   hb_retnl( (LONG) CreateFile( (LPCTSTR) hb_parcx(1),
                                (DWORD)   hb_parnl(2),
                                (DWORD)   hb_parnl(3),
                                ISCHAR( 4 ) ? (SECURITY_ATTRIBUTES *) sa : NULL ,
                                (DWORD) hb_parnl(5),
                                (DWORD) hb_parnl(6),
                                ISNIL( 7 ) ? NULL : (HANDLE) hb_parnl(7) ) ) ;

}

//-------------------------------------------------------------------//
/*
BOOL CloseHandle(
  HANDLE hObject   // handle to object to close
);
*/
HB_FUNC( CLOSEHANDLE )
{
  hb_retl( CloseHandle( (HANDLE) hb_parnl(1) ) );
}

//-------------------------------------------------------------------//
/*
 BOOL ReadFile(
  HANDLE       hFile,                 // handle of file to read
  LPVOID       lpBuffer,              // pointer to buffer that receives data
  DWORD        nNumberOfBytesToRead,  // number of bytes to read
  LPDWORD      lpNumberOfBytesRead,   // pointer to number of bytes read
  LPOVERLAPPED lpOverlapped           // pointer to structure for data
);
*/
HB_FUNC( READFILE )
{
   char * Buffer = ( char * ) hb_xgrab( hb_parnl( 3 ) ) ;
   DWORD nRead   = 0      ;
   BOOL  bRet             ;
   OVERLAPPED *Overlapped = NULL;

   if( ISCHAR( 5 ) )
      Overlapped = ( OVERLAPPED *) hb_parc( 5 ); //hb_param( 5, HB_IT_STRING )->item.asString.value ;


   bRet = ReadFile( (HANDLE) hb_parnl( 1 ) ,
                    Buffer                 ,
                    (DWORD)  hb_parnl( 3 ) ,
                    &nRead        ,
                    ISCHAR( 5 ) ? Overlapped : NULL ) ;

   if ( bRet )
   {
      hb_storclen( ( char * ) Buffer, nRead, 2 ) ;
   }

   hb_stornl( nRead, 4 ) ;
   hb_retl( bRet ) ;
}

//-------------------------------------------------------------------//
/*
BOOL WriteFile(
  HANDLE hFile,                    // handle to file to write to
  LPCVOID lpBuffer,                // pointer to data to write to file
  DWORD nNumberOfBytesToWrite,     // number of bytes to write
  LPDWORD lpNumberOfBytesWritten,  // pointer to number of bytes written
  LPOVERLAPPED lpOverlapped        // pointer to structure for overlapped I/O
);
*/
HB_FUNC( WRITEFILE )
{

   DWORD nWritten = 0;
   OVERLAPPED *Overlapped = NULL;

   if( ISCHAR( 4 ))
     Overlapped = ( OVERLAPPED *) hb_parc( 4 ); //hb_param( 4, HB_IT_STRING )->item.asString.value ;

   hb_retl ( WriteFile( (HANDLE)  hb_parnl( 1 )   ,
                     hb_parcx( 2 )       ,
                     hb_parclen( 2 )    ,
                     &nWritten          ,
                     ISCHAR( 4 ) ? Overlapped : NULL ) ) ;

   hb_stornl( nWritten, 3 ) ;
}

//-------------------------------------------------------------------//
/*
DWORD GetCurrentProcessId( VOID )
*/
HB_FUNC( GETCURRENTPROCESSID )
{
   hb_retnl( (ULONG) GetCurrentProcessId() );
}

//-------------------------------------------------------------------//
/*
DWORD GetCurrentProcess( VOID )
*/
HB_FUNC( GETCURRENTPROCESS )
{
   hb_retnl( (LONG) GetCurrentProcess() );
}

//-------------------------------------------------------------------//
/*
DWORD GetCurrentThreadId( VOID )
*/
HB_FUNC( GETCURRENTTHREADID )
{
   hb_retnl( (DWORD) GetCurrentThreadId() );
}

//-------------------------------------------------------------------//
/*
BOOL GetProcessWorkingSetSize( HANDLE hProcess, PSIZE_T lpMinimumWorkingSetSize,
                               PSIZE_T lpMaximumWorkingSetSize );
NOTE: This function is not supported and returns .F. under Windows 9x
*/
HB_FUNC( GETPROCESSWORKINGSETSIZE )
{
   DWORD MinimumWorkingSetSize=0;
   DWORD MaximumWorkingSetSize=0;

   hb_retl(GetProcessWorkingSetSize(ISNIL(1) ? GetCurrentProcess() : (HANDLE) hb_parnl( 1 ),
                            &MinimumWorkingSetSize, &MaximumWorkingSetSize ));
   hb_stornl(MinimumWorkingSetSize,2);
   hb_stornl(MaximumWorkingSetSize,3);
}

//-------------------------------------------------------------------//
/*
BOOL SetProcessWorkingSetSize( HANDLE hProcess, PSIZE_T lpMinimumWorkingSetSize,
                               PSIZE_T lpMaximumWorkingSetSize );
NOTE: This function is not supported and returns .F. under Windows 9x
      It may also fail if the process does not have right SE_INC_BASE_PRIORITY_NAME on NT/2000
*/
HB_FUNC( SETPROCESSWORKINGSETSIZE )
{
   hb_retl(SetProcessWorkingSetSize(ISNIL(1) ? GetCurrentProcess() : (HANDLE) hb_parnl( 1 ),
                   hb_parnl( 2 ), hb_parnl( 3 ) ));
}

//-------------------------------------------------------------------//
/*
DWORD VirtualQuery( LPCVOID lpAddress, PMEMORY_BASIC_INFORMATION lpBuffer,  SIZE_T dwLength);
- may not be functional, always seems to return error
*/
HB_FUNC( VIRTUALQUERY )
{
   if (hb_parni(1) >= sizeof(MEMORY_BASIC_INFORMATION))
   {
      hb_retl(VirtualQuery((void *) hb_parnl(1), (struct _MEMORY_BASIC_INFORMATION *) hb_parnl(2), sizeof(MEMORY_BASIC_INFORMATION)));
   }
   else
   {
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
      hb_retl(FALSE);
   }
}

//-------------------------------------------------------------------//
/*
BOOL VirtualLock(LPVOID lpAddress, SIZE_T dwSize );
- may not be functional, always seems to return error
*/
HB_FUNC( VIRTUALLOCK )
{
   hb_retl( VirtualLock( ( void * ) hb_parnl( 1 ), hb_parni( 2 ) ) );
}

//-------------------------------------------------------------------//
//
//   typedef struct { ;
//       DWORD    dwLowDateTime;
//       DWORD    dwHighDateTime;
//   } FILETIME
//
//
//   typedef struct { ;
//       WORD     wYear ;
//       WORD     wMonth ;
//       WORD     wDayOfWeek ;
//       WORD     wDay ;
//       WORD     wHour ;
//       WORD     wMinute ;
//       WORD     wSecond ;
//       WORD     wMilliSeconds ;
//   } SYSTEMTIME
//
//
HB_FUNC( FILETIMETOSYSTEMTIME )
{
   FILETIME   *FileTime  = ( FILETIME *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;
   SYSTEMTIME SystemTime ;

   if ( FileTimeToSystemTime( FileTime, &SystemTime ) )
   {
      hb_retl( TRUE ) ;

      if ( ISBYREF( 2 ) )
      {
         hb_storclen( ( char * ) &SystemTime , sizeof( SYSTEMTIME ), 2 ) ;
      }
   }
   else
   {
      hb_retl( FALSE ) ;
   }
}

//---------------------------------------------------------------------//
// BOOL SetConsoleOutputCP(  UINT wCodePageID )  // code page to set;
HB_FUNC( SETCONSOLEOUTPUTCP )
{
   hb_retl( SetConsoleOutputCP( (UINT) hb_parnl( 1 ) ) ) ;
}


