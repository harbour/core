/*
 * $Id$
 */

//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//
//                         Internet Functions
//
//                 Requires WinINet.dll and WinInet.h
//
//
//                 Pritpal Bedi <vouch32@vouchcac.com>
//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400
#undef _WIN32_IE
#define _WIN32_IE      0x0500

#include "hbwhat.h"

#ifndef __MINGW32__
#ifndef __WATCOMC__

#include   <windows.h>
#include   <commctrl.h>
#include   <shlobj.h>
#include   <shellapi.h>
#include   <wininet.h>

/* Needed for Cygwin */
#ifndef INTERNET_FLAG_NEED_FILE
#define INTERNET_FLAG_NEED_FILE 0x00000010
#endif

#include   "hbapi.h"
#include   "hbvm.h"
#include   "hbstack.h"
#include   "hbapiitm.h"
#include   "winreg.h"

//---------------------------------------------------------------------//
/*
   DWORD InternetDial(
       IN HWND     hwndParent,
       IN LPTSTR   lpszConnectoid,
       IN DWORD    dwFlags,
       OUT LPDWORD lpdwConnection,
       IN DWORD    dwReserved
   );
*/
//
//     InternetDial()
//
HB_FUNC( VWN_INTERNETDIAL )
{
   HWND    hWnd   = HB_ISNIL( 1 ) ? 0    : ( HWND ) HB_PARWH( 1 );
   LPTSTR  lpszId = HB_ISNIL( 2 ) ? NULL : hb_parcx( 2 );
   DWORD   nFlags = INTERNET_AUTODIAL_FORCE_ONLINE ;
   DWORD   nRet   = 0;

   hb_retnl( InternetDial( hWnd, lpszId, nFlags, &nRet, 0 ) );

}

//---------------------------------------------------------------------//
/*
   BOOL InternetGetConnectedState(
       OUT LPDWORD lpdwFlags,
       IN DWORD    dwReserved
   );
*/
//
//     lIsOn := InternetGetConnectedState()
//
HB_FUNC( VWN_INTERNETGETCONNECTEDSTATE )
{
   hb_retl( InternetGetConnectedState( NULL, 0 ) );
}

//---------------------------------------------------------------------//
/*
   HINTERNET InternetOpen(
       IN LPCTSTR lpszAgent,
       IN DWORD   dwAccessType,
       IN LPCTSTR lpszProxyName,
       IN LPCTSTR lpszProxyBypass,
       IN DWORD   dwFlags
   );
*/
//
//   hInternet := InternetOpen()
//   if hInternet <> 0
//       hFtp := InternetConnect( hInternet, 'vouchcac.com', ;
//                   INTERNET_DEFAULT_FTP_PORT, cUserName, cPassword, ;
//                        INTERNET_SERVICE_FTP )
//       if hFtp <> 0
//          if FtpOpenFile( hFtp, 'Temp/Testing.txt', GENERIC_WRITE )
//             cBuffer  := 'This is testing string' + chr( 13 ) + chr( 10 )
//             lSuccess := InternetWrite( hFtp, cBuffer, len( cBuffer ), @nWritten )
//             if lSuccess
//                ? nWritten
//             endif
//          endif
//          InternetCloseHandle( hFtp )
//       endif
//       InternetCloseHandle( hInternet )
//   endif
//
//
//
HB_FUNC( VWN_INTERNETOPEN )
{
   LPCTSTR lpszAgent       = HB_ISNIL( 1 ) ? NULL : hb_parcx( 1 );
   DWORD   dwAccessType    = HB_ISNIL( 2 ) ? INTERNET_OPEN_TYPE_DIRECT : hb_parnl( 2 );
   LPCTSTR lpszProxyName   = HB_ISNIL( 3 ) ? NULL : hb_parcx( 3 );
   LPCTSTR lpszProxyBypass = HB_ISNIL( 4 ) ? NULL : hb_parcx( 4 );
   DWORD   dwFlags         = HB_ISNIL( 5 ) ? 0    : hb_parnl( 5 );

   HB_RETWH( InternetOpen( lpszAgent, dwAccessType, lpszProxyName, lpszProxyBypass, dwFlags ) );
}

//---------------------------------------------------------------------//
/*
   HINTERNET InternetConnect(
     IN HINTERNET hInternet,
     IN LPCTSTR   lpszServerName,
     IN INTERNET_PORT nServerPort,
     IN LPCTSTR   lpszUserName,
     IN LPCTSTR   lpszPassword,
     IN DWORD     dwService,
     IN DWORD     dwFlags,
     IN DWORD_PTR dwContext
   );
*/
//
//       hFtp := InternetConnect( hInternet, 'chcac.com', ;
//                   INTERNET_DEFAULT_FTP_PORT, cUserName, cPassword, ;
//                        INTERNET_SERVICE_FTP )
//
HB_FUNC( VWN_INTERNETCONNECT )
{
   HINTERNET     hInternet      = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR       lpszServerName = hb_parcx( 2 );
   INTERNET_PORT nServerPort    = HB_ISNIL( 3 ) ? INTERNET_DEFAULT_HTTP_PORT : ( INTERNET_PORT ) hb_parni( 3 );
   LPCTSTR       lpszUserName   = HB_ISNIL( 4 ) ? NULL : hb_parcx( 4 );
   LPCTSTR       lpszPassword   = HB_ISNIL( 5 ) ? NULL : hb_parcx( 5 );
   DWORD         dwService      = HB_ISNIL( 6 ) ? INTERNET_SERVICE_HTTP : hb_parnl( 6 );
   DWORD         dwFlags        = HB_ISNIL( 7 ) ? 0    : hb_parnl( 7 );
   DWORD_PTR     dwContext      = HB_ISNIL( 8 ) ? 0    : hb_parnl( 8 );

   HB_RETWH( InternetConnect( hInternet,    lpszServerName,
                    nServerPort, lpszUserName, lpszPassword,
                    dwService, dwFlags,      dwContext ) );
}

//---------------------------------------------------------------------//
/*
   HINTERNET FtpOpenFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszFileName,
       IN DWORD     dwAccess,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//    if FtpOpenFile( hInternet, 'temp/config.sys', GENERIC_WRITE )
//       // take next step
//    endif
//
HB_FUNC( VWN_FTPOPENFILE )
{
   HINTERNET hFtp         = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszFileName = hb_parcx( 2 );
   DWORD     dwAccess     = HB_ISNIL( 3 ) ? GENERIC_READ : ( DWORD ) hb_parni( 3  );
   DWORD     dwFlags      = HB_ISNIL( 4 ) ? FTP_TRANSFER_TYPE_BINARY : hb_parni( 4 );
   DWORD_PTR dwContext    = HB_ISNIL( 5 ) ? 0            : hb_parnl( 5 );

   hb_retl( FtpOpenFile( hFtp, lpszFileName, dwAccess, dwFlags, dwContext ) != NULL );
}

//---------------------------------------------------------------------//
/*
   BOOL InternetWriteFile(
       IN HINTERNET hFile,
       IN LPCVOID   lpBuffer,
       IN DWORD     dwNumberOfBytesToWrite,
       OUT LPDWORD  lpdwNumberOfBytesWritten
   );
*/
//
//    if InternetWriteFile( hFile, @cBuffer, len( cBuffer ), @nWritten )
//       // Take next step
//    endif
//
HB_FUNC( VWN_INTERNETWRITEFILE )
{
   HINTERNET hFile                    = ( HINTERNET ) HB_PARWH( 1 );
   LPCVOID   lpBuffer                 = hb_parcx( 2 );
   DWORD     dwNumberOfBytesToWrite   = ( DWORD ) hb_parnl( 3 );
   DWORD     dwNumberOfBytesWritten   = ( DWORD ) 0 ;

   hb_retl( InternetWriteFile( hFile, lpBuffer, dwNumberOfBytesToWrite,
                                                &dwNumberOfBytesWritten ) );

   if HB_ISBYREF( 4 )
      hb_stornl( ( ULONG ) dwNumberOfBytesWritten, 4 );
}

//---------------------------------------------------------------------//
/*
   BOOL InternetReadFile(
       IN HINTERNET hFile,
       IN LPVOID    lpBuffer,
       IN DWORD     dwNumberOfBytesToRead,
       OUT LPDWORD  lpdwNumberOfBytesRead
   );
*/
//
//     if InternetReadFile( hFile, @cBuffer, len( cBuffer ), @nRead )
//        // Write to local handle
//     endif
//
HB_FUNC( VWN_INTERNETREADFILE )
{
   HINTERNET hFile                    = ( HINTERNET ) HB_PARWH( 1 );
   LPVOID    lpBuffer                 = hb_parcx( 2 );
   DWORD     dwNumberOfBytesToRead    = ( DWORD ) hb_parnl( 3 );
   DWORD     dwNumberOfBytesRead      = ( DWORD ) 0;
   BOOL      bRet;

   bRet = InternetReadFile( hFile, &lpBuffer,
                            dwNumberOfBytesToRead, &dwNumberOfBytesRead );

   hb_retl( bRet );

   if( bRet )
   {
      if HB_ISBYREF( 4 )
      {
         hb_stornl( ( ULONG ) dwNumberOfBytesRead, 4 );
      }
      hb_storclen( ( char * ) lpBuffer, ( ULONG ) dwNumberOfBytesRead, 2 );
   }
}

//---------------------------------------------------------------------//
/*
   BOOL FtpCommand(
       IN HINTERNET  hConnect,
       IN BOOL       fExpectResponse,
       IN DWORD      dwFlags,
       IN LPCTSTR    lpszCommand,
       IN DWORD_PTR  dwContext,
       OUT HINTERNET *phFtpCommand
   );
*/
//
//
//
HB_FUNC( VWN_FTPCOMMAND )
{
   HINTERNET hInternet       = ( HINTERNET ) HB_PARWH( 1 );
   BOOL      fExpectResponse = HB_ISNIL( 2 ) ? 0 : hb_parl( 2 );
   DWORD     dwFlags         = HB_ISNIL( 3 ) ? FTP_TRANSFER_TYPE_BINARY : hb_parnl( 3 );
   LPCTSTR   lpszCommand     = hb_parcx( 4 );
   DWORD_PTR dwContext       = HB_ISNIL( 5 ) ? 0 : hb_parnl( 5 );
   HINTERNET phFtpCommand ;

   BOOL      bRet ;

   bRet = FtpCommand( hInternet, fExpectResponse, dwFlags, lpszCommand,
                      dwContext, &phFtpCommand );

   hb_retl( bRet );

   if ( bRet )
   {
      if ( HB_ISBYREF( 6 ) )
         HB_STORWH( phFtpCommand, 6 );
   }
}
//---------------------------------------------------------------------//
/*
   HINTERNET FtpFindFirstFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszSearchFile,
       OUT LPWIN32_FIND_DATA lpFindFileData,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   #include  "wintypes.ch"
//   #include  "cstruct.ch"
//
//
//   pragma pack(4)
//
//   typedef struct { ;
//       DWORD    dwLowDateTime;
//       DWORD    dwHighDateTime;
//   } FILETIME
//
//   typedef struct { ;
//       DWORD    dwFileAttributes;
//       FILETIME ftCreationTime;
//       FILETIME ftLastAccessTime;
//       FILETIME ftLastWriteTime;
//       DWORD    nFileSizeHigh;
//       DWORD    nFileSizeLow;
//       DWORD    dwReserved0;
//       DWORD    dwReserved1;
//       char     cFileName[ MAX_PATH ];
//       char     cAlternateFileName[ 14 ];
//   } WIN32_FIND_DATA
//
//
//
//   Function FtpDirectory( hInternet, cFileSpec )
//   local hFile
//   local FindData IS WIN32_FIND_DATA
//   local cDirInfo := FindData:value
//
//   DEFAULT cFileSpec TO '*.*'
//
//   hFind := FtpFindFirstFile( hInternet, cFileSpec, @cDirInfo )
//   if hFind <> 0
//      FindData:Buffer( cDirInfo )
//
//      ? FindData:cFileName:value                // Name
//      ? FindData:dwFileAttributes               // Attribute in numeric, 16 for directory, 128 for file
//      ? FindData:nFileSizeLow                   // Size in bytes
//      ? findData:ftLastWriteTime:dwHighDateTime // Date, time in DWORD
//
//      do while .t.
//         if !InternetFindNextFile( hFind, @cDirInfo )
//            exit
//         endif
//         FindData:Buffer( cDirInfo )
//
//         ? FindData:cFileName:value                // Name
//         ? FindData:dwFileAttributes               // Attribute in numeric, 16 for directory, 128 for file
//         ? FindData:nFileSizeLow                   // Size in bytes
//         ? findData:ftLastWriteTime:dwHighDateTime // Date, time in DWORD
//      enddo
//
//   endif
//
//   return nil
//
//
HB_FUNC( VWN_FTPFINDFIRSTFILE )
{
   HINTERNET hInternet              = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszSearchFile         = HB_ISNIL( 2 ) ? TEXT ("*.*") : hb_parcx( 2 );
   WIN32_FIND_DATA FindFileData ;
   DWORD     dwFlags                = HB_ISNIL( 4 ) ? INTERNET_FLAG_NEED_FILE : hb_parnl( 4 );
   DWORD_PTR dwContext              = HB_ISNIL( 5 ) ? 0 : hb_parnl( 5 );
   HINTERNET hResult ;

   hResult = FtpFindFirstFile( hInternet, lpszSearchFile,
                                     &FindFileData, dwFlags, dwContext );

   if ( hResult )
      if ( HB_ISBYREF( 3 ) )
         hb_storclen( (char *) &FindFileData , sizeof( WIN32_FIND_DATA ), 3 );

   HB_RETWH( hResult );
}

//---------------------------------------------------------------------//
/*
   BOOL InternetFindNextFile(
       IN HINTERNET hFind,
       OUT LPVOID   lpvFindData
   );
*/
//
HB_FUNC( VWN_INTERNETFINDNEXTFILE )
{
   HINTERNET       hFind       = ( HINTERNET ) HB_PARWH( 1 );
   WIN32_FIND_DATA FindFileData ;

   if ( InternetFindNextFile( hFind, &FindFileData ) )
      {
         hb_retl( TRUE );
         if ( HB_ISBYREF( 2 ) )
            hb_storclen( ( char * ) &FindFileData, sizeof( WIN32_FIND_DATA ), 2 );
      }
   else
      hb_retl( FALSE );
}

//---------------------------------------------------------------------//
/*
   BOOL FtpGetFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszRemoteFile,
       IN LPCTSTR   lpszNewFile,
       IN BOOL      fFailIfExists,
       IN DWORD     dwFlagsAndAttributes,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   if FtpGetFile( hInternet, cRemoteFile, cLocalFile, lFailIfExist )
//      ? 'Success'
//   endif
//
HB_FUNC( VWN_FTPGETFILE )
{
   HINTERNET hInternet            = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszRemoteFile       = hb_parcx( 2 );
   LPCTSTR   lpszLocalFile        = hb_parcx( 3 );
   BOOL      fFailIfExist         = HB_ISNIL( 4 ) ? FALSE : hb_parl( 4 );
   DWORD     dwFlagsAndAttributes = HB_ISNIL( 5 ) ? FILE_ATTRIBUTE_NORMAL : hb_parnl( 5 );
   DWORD     dwFlags              = HB_ISNIL( 6 ) ? FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_RELOAD : ( DWORD ) hb_parnl( 6 );
   DWORD_PTR dwContext            = HB_ISNIL( 7 ) ? 0 : hb_parnl( 7 );

   hb_retl( FtpGetFile( hInternet, lpszRemoteFile, lpszLocalFile,
                        fFailIfExist, dwFlagsAndAttributes,
                        dwFlags, dwContext ) );
}

//---------------------------------------------------------------------//
/*
   BOOL FtpPutFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszLocalFile,
       IN LPCTSTR   lpszNewRemoteFile,
       IN DWORD     dwFlags,
       IN DWORD_PTR dwContext
   );
*/
//
//   if FtpPutFile( hInternet, cLocalFile, cRemoteFile )
//      ?
//   endif
//
HB_FUNC( VWN_FTPPUTFILE )
{
   HINTERNET hInternet            = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszLocalFile        = hb_parcx( 2 );
   LPCTSTR   lpszRemoteFile       = hb_parcx( 3 );
   DWORD     dwFlags              = HB_ISNIL( 4 ) ? FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_RELOAD : ( DWORD ) hb_parnl( 4 );
   DWORD_PTR dwContext            = HB_ISNIL( 5 ) ? 0 : hb_parnl( 5 );

   hb_retl( FtpPutFile( hInternet, lpszLocalFile, lpszRemoteFile, dwFlags, dwContext ) );
}

//---------------------------------------------------------------------//
/*
   BOOL FtpCreateDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//   if FtpCreateDirectory( hInternet, 'Temp' )
//      ? 'Success'
//   endif
//
HB_FUNC( VWN_FTPCREATEDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszDirectory = hb_parcx( 2 );

   hb_retl( FtpCreateDirectoryA( hInternet, lpszDirectory ) );

}

//---------------------------------------------------------------------//
/*
   BOOL FtpRemoveDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//   if FtpRemoveDirectory( hInternet, cDirectory )
//      ? 'Success'
//   endif
//
HB_FUNC( VWN_FTPREMOVEDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszDirectory = hb_parcx( 2 );

   hb_retl( FtpRemoveDirectoryA( hInternet, lpszDirectory ) );

}

//---------------------------------------------------------------------//
/*
   BOOL FtpDeleteFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszFileName
   );
*/
//
//   if FtpDeleteFile( hInternet, 'temp\config.sys' )
//      ? 'Sucess'
//   endif
//
HB_FUNC( VWN_FTPDELETEFILE )
{
   HINTERNET hInternet    = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszFileName = hb_parcx( 2 );

   hb_retl( FtpDeleteFile( hInternet, lpszFileName ) );

}

//---------------------------------------------------------------------//
/*
   BOOL FtpRenameFile(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszExisting,
       IN LPCTSTR   lpszNew
   );
*/
//
//   if FtpRenameFile( hInternet, cExisting, cNew )
//      ? 'Success'
//   endif
//
HB_FUNC( VWN_FTPRENAMEFILE )
{
   HINTERNET hInternet    = ( HINTERNET ) HB_PARWH( 1 );
   LPCTSTR   lpszExisting = hb_parcx( 2 );
   LPCTSTR   lpszNew      = hb_parcx( 3 );

   hb_retl( FtpRenameFileA( hInternet, lpszExisting, lpszNew ) );

}

//---------------------------------------------------------------------//
/*
   BOOL FtpGetCurrentDirectory(
       IN HINTERNET   hConnect,
       OUT LPTSTR     lpszCurrentDirectory,
       IN OUT LPDWORD lpdwCurrentDirectory
   );
*/
//
//   if FtpGetCurrentDirectory( hInternet, @cDirectory )
//      ? cDirectory
//   endif
//
HB_FUNC( VWN_FTPGETCURRENTDIRECTORY )
{
   HINTERNET hInternet           = ( HINTERNET ) HB_PARWH( 1 );
   LPTSTR   lpszCurrentDirectory = ( LPTSTR ) hb_xgrab( MAX_PATH );
   DWORD    dwCurrentDirectory   = MAX_PATH;
   BOOL     bRet;

   bRet = FtpGetCurrentDirectory( hInternet, lpszCurrentDirectory, &dwCurrentDirectory );
   hb_retl( bRet );

   if ( bRet )
   {
      if ( HB_ISBYREF( 2 ) )
         hb_storclen( ( char * ) lpszCurrentDirectory, ( ULONG ) dwCurrentDirectory, 2 );
   }

   hb_xfree( lpszCurrentDirectory );
}

//---------------------------------------------------------------------//
/*
   BOOL FtpSetCurrentDirectory(
       IN HINTERNET hConnect,
       IN LPCTSTR   lpszDirectory
   );
*/
//
//    if FtpSetCurrentDirectory( hInternet, cDirectory )
//       ? 'Success'
//    endif
//
HB_FUNC( VWN_FTPSETCURRENTDIRECTORY )
{
   HINTERNET hInternet     = ( HINTERNET ) HB_PARWH( 1 );
   LPTSTR    lpszDirectory = hb_parcx( 2 );

   hb_retl( FtpSetCurrentDirectoryA( hInternet, lpszDirectory ) );
}

//---------------------------------------------------------------------//
/*
   BOOL InternetCloseHandle(
       IN HINTERNET hInternet
   );
*/
//
//    if InternetCloseHandle( hInternet )
//       ? 'Success'
//    endif
//
HB_FUNC( VWN_INTERNETCLOSEHANDLE )
{
   HINTERNET hInternet = ( HINTERNET ) HB_PARWH( 1 );

   hb_retl( InternetCloseHandle( hInternet ) );
}

//---------------------------------------------------------------------//
/*
   DWORD InternetAttemptConnect(
       IN DWORD dwReserved
   );
*/
//
//    InternetAttempConnect()
//
HB_FUNC( VWN_INTERNETATTEMPTCONNECT )
{
   DWORD dwReserved = 0 ;

   hb_retnl( ( ULONG ) InternetAttemptConnect( dwReserved ) );
}

//---------------------------------------------------------------------//

#endif
#endif
