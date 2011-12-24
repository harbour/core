/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_fsLink*(), HB_FLINK*() functions
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu)
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

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   #include <windows.h>
#elif defined( HB_OS_UNIX )
   #include <unistd.h>
#endif

HB_BOOL hb_fsLink( const char * pszExisting, const char * pszNewFile )
{
   HB_BOOL fResult;

   if( pszExisting && pszNewFile )
   {
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
            LPTSTR lpFileName = HB_TCHAR_CONVTO( pszNewFile );
            LPTSTR lpExistingFileName = HB_TCHAR_CONVTO( pszExisting );

            fResult = s_pCreateHardLink( lpFileName, lpExistingFileName, NULL ) != 0;
            hb_fsSetIOError( fResult, 0 );
            hb_fsSetFError( hb_fsError() );

            HB_TCHAR_FREE( lpFileName );
            HB_TCHAR_FREE( lpExistingFileName );
         }
         else
         {
            hb_fsSetFError( 1 );
            fResult = HB_FALSE;
         }
      }
#elif defined( HB_OS_UNIX )
      {
         fResult = ( link( pszExisting, pszNewFile ) == 0 );
         hb_fsSetIOError( fResult, 0 );
         hb_fsSetFError( hb_fsError() );
      }
#else
      {
         hb_fsSetFError( 1 );
         fResult = HB_FALSE;
      }
#endif
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
            LPTSTR lpSymlinkFileName = HB_TCHAR_CONVTO( pszNewFile );
            LPTSTR lpTargetFileName = HB_TCHAR_CONVTO( pszTarget );

            fResult = s_pCreateSymbolicLink( lpSymlinkFileName, lpTargetFileName, hb_fsIsDirectory( pszTarget ) ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0 ) != 0;
            hb_fsSetIOError( fResult, 0 );
            hb_fsSetFError( hb_fsError() );

            HB_TCHAR_FREE( lpSymlinkFileName );
            HB_TCHAR_FREE( lpTargetFileName );
         }
         else
         {
            hb_fsSetFError( 1 );
            fResult = HB_FALSE;
         }
      }
#elif defined( HB_OS_UNIX )
      {
         fResult = ( symlink( pszTarget, pszNewFile ) == 0 );
         hb_fsSetIOError( fResult, 0 );
         hb_fsSetFError( hb_fsError() );
      }
#else
      {
         hb_fsSetFError( 1 );
         fResult = HB_FALSE;
      }
#endif
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
   char * pszLink;

   if( pszFile )
   {
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
            LPTSTR lpFileName = HB_TCHAR_CONVTO( pszFile );
            HANDLE hFile;

            hFile = CreateFile( lpFileName,
                                GENERIC_READ,
                                FILE_SHARE_READ,
                                NULL,
                                OPEN_EXISTING,
                                FILE_ATTRIBUTE_NORMAL,
                                NULL );

            if( hFile == INVALID_HANDLE_VALUE )
            {
               hb_fsSetIOError( HB_FALSE, 0 );
               hb_fsSetFError( hb_fsError() );
               pszLink = NULL;
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
                     pszLink = ( char * ) hb_xgrab( size + 1 );
#if defined( UNICODE )
                     hb_wcntombcpy( pszLink, lpLink, ( HB_SIZE ) size );
#else
                     hb_strncpy( pszLink, lpLink, ( HB_SIZE ) size );
#endif
                  }
                  else
                     pszLink = NULL;

                  hb_fsSetIOError( HB_TRUE, 0 );
                  hb_fsSetFError( hb_fsError() );
               }
               else
               {
                  hb_fsSetFError( 1 );
                  pszLink = NULL;
               }
            }

            HB_TCHAR_FREE( lpFileName );
         }
         else
         {
            hb_fsSetFError( 1 );
            pszLink = NULL;
         }
      }
#elif defined( HB_OS_UNIX )
      {
         size_t size;
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
            pszLink[ size ] = '\0';
      }
#else
      {
         hb_fsSetFError( 1 );
         pszLink = NULL;
      }
#endif
   }
   else
   {
      hb_fsSetFError( 2 );
      pszLink = NULL;
   }

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
