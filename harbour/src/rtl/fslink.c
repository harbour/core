/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_fsLink*(), HB_FLINK*() functions
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   #include <windows.h>
   #if !defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES     ( ( DWORD ) -1 )
   #endif
   #include "hbwinuni.h"
#elif defined( HB_OS_UNIX )
   #include <unistd.h>
#endif

HB_BOOL hb_fsLink( const char * pszExisting, const char * pszNewFile )
{
   HB_BOOL fResult;

   if( pszExisting && pszNewFile )
   {
      hb_vmUnlock();

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
      {
         typedef BOOL ( WINAPI * _HB_CREATEHARDLINK )( LPCTSTR, LPCTSTR, LPSECURITY_ATTRIBUTES );

         static _HB_CREATEHARDLINK s_pCreateHardLink = NULL;

         if( ! s_pCreateHardLink )
            s_pCreateHardLink = ( _HB_CREATEHARDLINK ) GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ),
#if defined( UNICODE )
               "CreateHardLinkW" );
#else
               "CreateHardLinkA" );
#endif

         if( s_pCreateHardLink )
         {
            LPCTSTR lpFileName, lpExistingFileName;
            LPTSTR lpFileNameFree, lpExistingFileNameFree;

            lpFileName = HB_FSNAMECONV( pszNewFile, &lpFileNameFree );
            lpExistingFileName = HB_FSNAMECONV( pszExisting, &lpExistingFileNameFree );

            fResult = s_pCreateHardLink( lpFileName, lpExistingFileName, NULL ) != 0;
            hb_fsSetIOError( fResult, 0 );
            hb_fsSetFError( hb_fsError() );

            if( lpFileNameFree )
               hb_xfree( lpFileNameFree );
            if( lpExistingFileNameFree )
               hb_xfree( lpExistingFileNameFree );
         }
         else
         {
            hb_fsSetFError( 1 );
            fResult = HB_FALSE;
         }
      }
#elif defined( HB_OS_UNIX )
      {
         char * pszExistingFree;
         char * pszNewFileFree;

         pszExisting = hb_fsNameConv( pszExisting, &pszExistingFree );
         pszNewFile = hb_fsNameConv( pszNewFile, &pszNewFileFree );

         fResult = ( link( pszExisting, pszNewFile ) == 0 );
         hb_fsSetIOError( fResult, 0 );
         hb_fsSetFError( hb_fsError() );

         if( pszExistingFree )
            hb_xfree( pszExistingFree );
         if( pszNewFileFree )
            hb_xfree( pszNewFileFree );
      }
#else
      {
         hb_fsSetFError( 1 );
         fResult = HB_FALSE;
      }
#endif

      hb_vmLock();
   }
   else
   {
      hb_fsSetFError( 2 );
      fResult = HB_FALSE;
   }

   return fResult;
}

HB_BOOL hb_fsLinkSym( const char * pszTarget, const char * pszNewFile )
{
   HB_BOOL fResult;

   if( pszTarget && pszNewFile )
   {
      hb_vmUnlock();

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
      {
         typedef BOOL ( WINAPI * _HB_CREATESYMBOLICLINK )( LPCTSTR, LPCTSTR, DWORD );

         static _HB_CREATESYMBOLICLINK s_pCreateSymbolicLink = NULL;

         #ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
         #define SYMBOLIC_LINK_FLAG_DIRECTORY 0x1
         #endif

         if( ! s_pCreateSymbolicLink )
            s_pCreateSymbolicLink = ( _HB_CREATESYMBOLICLINK ) GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ),
#if defined( UNICODE )
               "CreateSymbolicLinkW" );
#else
               "CreateSymbolicLinkA" );
#endif

         if( s_pCreateSymbolicLink )
         {
            LPCTSTR lpSymlinkFileName, lpTargetFileName;
            LPTSTR lpSymlinkFileNameFree, lpTargetFileNameFree;
            DWORD dwAttr;
            HB_BOOL fDir;

            lpSymlinkFileName = HB_FSNAMECONV( pszNewFile, &lpSymlinkFileNameFree );
            lpTargetFileName = HB_FSNAMECONV( pszTarget, &lpTargetFileNameFree );

            dwAttr = GetFileAttributes( lpTargetFileName );
            fDir = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
                   ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

            fResult = s_pCreateSymbolicLink( lpSymlinkFileName, lpTargetFileName, fDir ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0 ) != 0;
            hb_fsSetIOError( fResult, 0 );
            hb_fsSetFError( hb_fsError() );

            if( lpSymlinkFileNameFree )
               hb_xfree( lpSymlinkFileNameFree );
            if( lpTargetFileNameFree )
               hb_xfree( lpTargetFileNameFree );
         }
         else
         {
            hb_fsSetFError( 1 );
            fResult = HB_FALSE;
         }
      }
#elif defined( HB_OS_UNIX )
      {
         char * pszTargetFree;
         char * pszNewFileFree;

         pszTarget = hb_fsNameConv( pszTarget, &pszTargetFree );
         pszNewFile = hb_fsNameConv( pszNewFile, &pszNewFileFree );

         fResult = ( symlink( pszTarget, pszNewFile ) == 0 );
         hb_fsSetIOError( fResult, 0 );
         hb_fsSetFError( hb_fsError() );

         if( pszTargetFree )
            hb_xfree( pszTargetFree );
         if( pszNewFileFree )
            hb_xfree( pszNewFileFree );
      }
#else
      {
         hb_fsSetFError( 1 );
         fResult = HB_FALSE;
      }
#endif

      hb_vmLock();
   }
   else
   {
      hb_fsSetFError( 2 );
      fResult = HB_FALSE;
   }

   return fResult;
}

/* NOTE: Caller must free the pointer, if not NULL */
char * hb_fsLinkRead( const char * pszFile )
{
   char * pszLink = NULL;

   if( pszFile )
   {
      hb_vmUnlock();

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
      {
         typedef DWORD ( WINAPI * _HB_GETFINALPATHNAMEBYHANDLE )( HANDLE, LPTSTR, DWORD, DWORD );

         static _HB_GETFINALPATHNAMEBYHANDLE s_pGetFinalPathNameByHandle = NULL;

         #ifndef VOLUME_NAME_DOS
         #define VOLUME_NAME_DOS 0x0
         #endif
         #ifndef VOLUME_NAME_GUID
         #define VOLUME_NAME_GUID 0x1
         #endif
         #ifndef VOLUME_NAME_NT
         #define VOLUME_NAME_NT 0x2
         #endif
         #ifndef VOLUME_NAME_NONE
         #define VOLUME_NAME_NONE 0x4
         #endif
         #ifndef FILE_NAME_NORMALIZED
         #define FILE_NAME_NORMALIZED 0x0
         #endif
         #ifndef FILE_NAME_OPENED
         #define FILE_NAME_OPENED 0x8
         #endif

         if( ! s_pGetFinalPathNameByHandle )
            s_pGetFinalPathNameByHandle = ( _HB_GETFINALPATHNAMEBYHANDLE ) GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ),
#if defined( UNICODE )
               "GetFinalPathNameByHandleW" );
#else
               "GetFinalPathNameByHandleA" );
#endif

         if( s_pGetFinalPathNameByHandle )
         {
            LPCTSTR lpFileName;
            LPTSTR lpFileNameFree;
            HANDLE hFile;
            DWORD dwAttr;
            HB_BOOL fDir;

            lpFileName = HB_FSNAMECONV( pszFile, &lpFileNameFree );

            dwAttr = GetFileAttributes( lpFileName );
            fDir = ( dwAttr != INVALID_FILE_ATTRIBUTES ) &&
                   ( dwAttr & FILE_ATTRIBUTE_DIRECTORY );

            hFile = CreateFile( lpFileName,
                                GENERIC_READ,
                                FILE_SHARE_READ,
                                NULL,
                                OPEN_EXISTING,
                                fDir ? ( FILE_ATTRIBUTE_DIRECTORY | FILE_FLAG_BACKUP_SEMANTICS ) : FILE_ATTRIBUTE_NORMAL,
                                NULL );

            if( hFile == INVALID_HANDLE_VALUE )
            {
               hb_fsSetIOError( HB_FALSE, 0 );
               hb_fsSetFError( hb_fsError() );
            }
            else
            {
               DWORD size;
               TCHAR lpLink[ HB_PATH_MAX ];
               size = s_pGetFinalPathNameByHandle( hFile, lpLink, HB_PATH_MAX, VOLUME_NAME_DOS );
               if( size < HB_PATH_MAX )
               {
                  if( size > 0 )
                  {
                     lpLink[ size ] = TEXT( '\0' );
                     pszLink = HB_OSSTRDUP( lpLink );
                  }

                  hb_fsSetIOError( HB_TRUE, 0 );
                  hb_fsSetFError( hb_fsError() );
               }
               else
                  hb_fsSetFError( 1 );
            }

            if( lpFileNameFree )
               hb_xfree( lpFileNameFree );
         }
         else
            hb_fsSetFError( 1 );
      }
#elif defined( HB_OS_UNIX )
      {
         char * pszFileFree;
         size_t size;

         pszFile = hb_fsNameConv( pszFile, &pszFileFree );

         pszLink = ( char * ) hb_xgrab( HB_PATH_MAX + 1 );
         size = readlink( pszFile, pszLink, HB_PATH_MAX );
         hb_fsSetIOError( size != ( size_t ) -1, 0 );
         hb_fsSetFError( hb_fsError() );
         if( size == ( size_t ) -1 )
         {
            hb_xfree( pszLink );
            pszLink = NULL;
         }
         else
         {
            pszLink[ size ] = '\0';
            /* Convert from OS codepage */
            pszLink = ( char * ) hb_osDecodeCP( pszLink, NULL, NULL );
         }

         if( pszFileFree )
            hb_xfree( pszFileFree );
      }
#else
      {
         hb_fsSetFError( 1 );
      }
#endif

      hb_vmLock();
   }
   else
      hb_fsSetFError( 2 );

   return pszLink;
}

HB_FUNC( HB_FLINK )
{
   const char * pszExisting = hb_parc( 1 ), * pszNewFile = hb_parc( 2 );

   if( pszExisting && pszNewFile )
      hb_retni( hb_fsLink( pszExisting, pszNewFile ) ? 0 : F_ERROR );
   else
   {
      hb_fsSetFError( 2 );
      hb_retni( F_ERROR );
   }
}

HB_FUNC( HB_FLINKSYM )
{
   const char * pszTarget = hb_parc( 1 ), * pszNewFile = hb_parc( 2 );

   if( pszTarget && pszNewFile )
      hb_retni( hb_fsLinkSym( pszTarget, pszNewFile ) ? 0 : F_ERROR );
   else
   {
      hb_fsSetFError( 2 );
      hb_retni( F_ERROR );
   }
}

HB_FUNC( HB_FLINKREAD )
{
   const char * pszFile = hb_parc( 1 );

   if( pszFile )
      hb_retc_buffer( hb_fsLinkRead( pszFile ) );
   else
   {
      hb_fsSetFError( 2 );
      hb_retc_null();
   }
}
