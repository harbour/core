/*
 * $Id$
 */


// hbwhat
// disk, directory and file functions


#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0500

#include "hbwhat.h"

#include <windows.h>
#include "hbapi.h"

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetLogicalDrives( VOID );


HB_FUNC( VWN_GETLOGICALDRIVES )
{
   hb_retnl( (LONG) GetLogicalDrives(  ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetDriveTypeA( IN LPCSTR lpRootPathName );


HB_FUNC( VWN_GETDRIVETYPE )
{
   hb_retni( GetDriveType( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetSystemDirectoryA( OUT LPSTR lpBuffer, IN UINT uSize );


// modified
// GetSystemDirectory() -> cDir

HB_FUNC( VWN_GETSYSTEMDIRECTORY )
{

   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetSystemDirectory( szBuffer,MAX_PATH);
   hb_retc(szBuffer);

}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetTempPathA( IN DWORD nBufferLength, OUT LPSTR lpBuffer );

// modified
// GetTempPath() -> cPath


HB_FUNC( VWN_GETTEMPPATH )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetTempPath(MAX_PATH, szBuffer);
   hb_retc(szBuffer);

}




//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetTempFileNameA( IN LPCSTR lpPathName, IN LPCSTR lpPrefixString, IN UINT uUnique, OUT LPSTR lpTempFileName );

// modified
// GetTempFileName(cPath,cPrefix,nUnique) -> cFileName

HB_FUNC( VWN_GETTEMPFILENAME )
{
   char cPath[ MAX_PATH ] = {0};

   GetTempFileName( (LPCSTR) hb_parcx( 1 ),
                            (LPCSTR) hb_parcx( 2 ),
                            (UINT) ( ISNIL(3) ? 0 : hb_parni( 3 ) ) ,
                            (LPSTR) cPath
                           );
   hb_retc( cPath);
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetWindowsDirectoryA( OUT LPSTR lpBuffer, IN UINT uSize );


HB_FUNC( VWN_GETWINDOWSDIRECTORY )
{
   char szBuffer[ MAX_PATH + 1 ] = {0} ;
   GetWindowsDirectory( szBuffer,MAX_PATH);
   hb_retc(szBuffer);
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetSystemWindowsDirectoryA( OUT LPSTR lpBuffer, IN UINT uSize );

/*
// NT only
#if (_WIN32_WINNT >= 0x0500)

HB_FUNC( VWN_GETSYSTEMWINDOWSDIRECTORY )
{
   char cPath[ MAX_PATH +1 ] = {0};

   GetSystemWindowsDirectory( (LPSTR) cPath, MAX_PATH );

   hb_retc( cPath );
}
#endif
*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetCurrentDirectoryA( IN LPCSTR lpPathName );


HB_FUNC( VWN_SETCURRENTDIRECTORY )
{

   hb_retl( SetCurrentDirectory( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetCurrentDirectoryA( IN DWORD nBufferLength, OUT LPSTR lpBuffer );


HB_FUNC( VWN_GETCURRENTDIRECTORY )
{
   char cPath[ MAX_PATH + 1 ] = {0};
   GetCurrentDirectory( MAX_PATH , (LPSTR) cPath );
   hb_retc( cPath );
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetFileAttributesA( IN LPCSTR lpFileName, IN DWORD dwFileAttributes );


HB_FUNC( VWN_SETFILEATTRIBUTES )
{
   hb_retl( SetFileAttributes( (LPCSTR) hb_parcx( 1 ), (DWORD) hb_parnl( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetFileAttributesA( IN LPCSTR lpFileName );


HB_FUNC( VWN_GETFILEATTRIBUTES )
{
   hb_retnl( (LONG) GetFileAttributes( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI DeleteFileA( IN LPCSTR lpFileName );


HB_FUNC( VWN_DELETEFILE )
{
   hb_retl( DeleteFile( (LPCSTR) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetVolumeLabelA( IN LPCSTR lpRootPathName, IN LPCSTR lpVolumeName );


HB_FUNC( VWN_SETVOLUMELABEL )
{
   hb_retl( SetVolumeLabel( (LPCSTR) hb_parcx( 1 ), (LPCSTR) hb_parcx( 2 ) ) );
}



//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI CreateDirectoryA( IN LPCSTR lpPathName, IN LPSECURITY_ATTRIBUTES lpSecurityAttributes );

HB_FUNC( VWN_CREATEDIRECTORY )
{
   SECURITY_ATTRIBUTES *sa = NULL;

   if (ISCHAR(2))
       sa = (SECURITY_ATTRIBUTES *) hb_parc( 2 ); //hb_param(2, HB_IT_STRING)->item.asString.value;

   hb_retl( CreateDirectoryA( (LPCSTR) hb_parcx( 1 ), sa ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI CreateDirectoryExA( IN LPCSTR lpTemplateDirectory, IN LPCSTR lpNewDirectory, IN LPSECURITY_ATTRIBUTES lpSecurityAttributes );

/*

HB_FUNC( VWN_CREATEDIRECTORYEX )
{
   LPSECURITY_ATTRIBUTES lpSecurityAttributes ;

   // Your code goes here

   hb_retl( CreateDirectoryExA( (LPCSTR) hb_parcx( 1 ),
                                (LPCSTR) hb_parcx( 2 ),
                                lpSecurityAttributes
                                ) );
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI RemoveDirectoryA( IN LPCSTR lpPathName );


HB_FUNC( VWN_REMOVEDIRECTORY )
{
   hb_retl( RemoveDirectory( (LPCSTR) hb_parcx( 1 ) ) );
}




//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetFullPathNameA( IN LPCSTR lpFileName, IN DWORD nBufferLength, OUT LPSTR lpBuffer, OUT LPSTR *lpFilePart );

//See MSDN first


HB_FUNC( VWN_GETFULLPATHNAME )
{
   char *szBuffRet = NULL ;
   char buffer[ MAX_PATH + 1 ] = {0};
   char *szIn =hb_parcx( 1 );
   //DWORD dwSize  = hb_parnl( 2 );
   DWORD dwReq;
   dwReq = GetFullPathName( (LPCSTR) szIn,
                            MAX_PATH ,
                            (LPSTR) buffer ,
                            &szBuffRet
                          )  ;
  hb_retnl( dwReq );
  hb_storc( szBuffRet , 4 );
  hb_storc( buffer ,3 );
}





//  No prototype ?

//-----------------------------------------------------------------------------
   // GetVolumeFileName(szPath,@szPathReturn)
// WINBASEAPI BOOL WINAPI GetVolumePathNameA( LPCSTR lpszFileName, LPSTR lpszVolumePathName, DWORD cchBufferLength );
//#if (_WIN32_WINNT >= 0x0500)

HB_FUNC( VWN_GETVOLUMEPATHNAME )
{
   typedef BOOL ( WINAPI * P_GVPN )( LPCTSTR, LPTSTR, DWORD );
   BOOL bResult = FALSE;
   char buffer[MAX_PATH+1] = {0};
   P_GVPN pGVPN ;
   pGVPN = ( P_GVPN ) GetProcAddress( GetModuleHandle( "kernel32.dll" ), "GetVolumePathNameA" );
   if( pGVPN )
   {
      bResult = pGVPN( (LPCSTR) hb_parcx( 1 ), buffer, MAX_PATH );
   }
   hb_retl( bResult );
   if ( ISBYREF( 2 ) )
   {
      hb_storc( buffer ,2 );
   }
}


//#endif


//-----------------------------------------------------------------------------

HB_FUNC( VWN_GETSHORTPATHNAME )
{
   char buffer[ MAX_PATH + 1 ] = {0};
   int iRet;

   iRet = GetShortPathName(hb_parcx(1),buffer,MAX_PATH);
   hb_storc(buffer , 2 );
   hb_stornl(iRet , 3 );

}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetLongPathNameA( IN LPCSTR lpszShortPath, OUT LPSTR lpszLongPath, IN DWORD cchBuffer );

HB_FUNC( VWN_GETLONGPATHNAME )
{
   hb_retnl( (LONG) GetLongPathName( (LPCSTR) hb_parcx( 1 ),
                                      (LPSTR) hb_parcx( 2 ) ,
                                      (DWORD) hb_parnl( 3 )
                                      ) );
}

//-----------------------------------------------------------------------------

/*
BOOL GetVolumeInformation(
  LPCTSTR lpRootPathName,        // address of root directory of the
                                 // file system
  LPTSTR lpVolumeNameBuffer,     // address of name of the volume
  DWORD nVolumeNameSize,         // length of lpVolumeNameBuffer
  LPDWORD lpVolumeSerialNumber,  // address of volume serial number
  LPDWORD lpMaximumComponentLength,
                                 // address of system's maximum
                                 // filename length
  LPDWORD lpFileSystemFlags,     // address of file system flags
  LPTSTR lpFileSystemNameBuffer, // address of name of file system
  DWORD nFileSystemNameSize      // length of lpFileSystemNameBuffer
);
*/

// Syntax:
// GetVolumeInformation([cPath],[@cVolName],[@nSerNum],[@nMaxName],[@nFlags],[@cFATName] )

HB_FUNC( VWN_GETVOLUMEINFORMATION )
{
  char *VolumeNameBuffer     = (char *) hb_xgrab( MAX_PATH );
  DWORD VolumeSerialNumber                              ;
  DWORD MaximumComponentLength                          ;
  DWORD FileSystemFlags                                 ;
  char *FileSystemNameBuffer = (char *) hb_xgrab( MAX_PATH )  ;
  BOOL bRet;

  bRet = GetVolumeInformation( ISNIL(1) ? NULL : (LPCTSTR) hb_parcx(1) ,
                                  (LPTSTR) VolumeNameBuffer              ,
                                  MAX_PATH                               ,
                                  &VolumeSerialNumber                    ,
                                  &MaximumComponentLength                ,
                                  &FileSystemFlags                       ,
                                  (LPTSTR)FileSystemNameBuffer           ,
                                  MAX_PATH );
  if ( bRet  )
  {
     if ( ISBYREF( 2 ) )  hb_storc ((char *) VolumeNameBuffer, 2 );
     if ( ISBYREF( 3 ) )  hb_stornl( (LONG)  VolumeSerialNumber, 3 );
     if ( ISBYREF( 4 ) )  hb_stornl( (LONG)  MaximumComponentLength, 4 );
     if ( ISBYREF( 5 ) )  hb_stornl( (LONG)  FileSystemFlags, 5 );
     if ( ISBYREF( 6 ) )  hb_storc ((char *) FileSystemNameBuffer, 6 );
  }

  hb_retl(bRet);
  hb_xfree( VolumeNameBuffer );
  hb_xfree( FileSystemNameBuffer );
}
