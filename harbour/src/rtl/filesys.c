/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API (C level)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2010 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_fsSetError()
 *    hb_fsSetDevMode()
 *    hb_fsReadLarge()
 *    hb_fsWriteLarge()
 *    hb_fsCurDirBuff()
 *    hb_fsBaseDirBuff()
 *    fs_win_get_drive()
 *    fs_win_set_drive()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_fsChDrv()
 *    hb_fsCurDrv()
 *    hb_fsIsDrv()
 *    hb_fsIsDevice()
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *            and David G. Holm <dholm@jsd-llc.com>
 *    hb_fsEof()
 *
 * Copyright 2001 Jose Gimenez (JFG) <jfgimenez@wanadoo.es>
 *                                   <tecnico.sireinsa@ctv.es>
 *    Added platform check for any compiler to use the Windows
 *    API calls to allow openning an unlimited number of files
 *    simultaneously.
 *
 * See COPYING for licensing terms.
 *
 */

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET ) will
         set the file pointer to the passed negative value and the subsequent
         hb_fsWrite() call will fail. In CA-Cl*pper, _fsSeek() will fail,
         the pointer will not be moved and thus the _fsWrite() call will
         successfully write the buffer to the current file position. [vszakats]

   This has been corrected by ptucker
 */

/* *nixes */
#if !defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE 1
#endif
#if !defined( _GNU_SOURCE )
#  define _GNU_SOURCE
#endif

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hb_io.h"
#include "hbset.h"

#if defined( HB_OS_UNIX )
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
   #include <sys/types.h>
   #include <sys/wait.h>
   #include <sys/time.h>
#endif
#if !defined( HB_OS_WIN )
#  include <errno.h>
#endif

#if ( defined( __DMC__ ) || defined( __BORLANDC__ ) || \
      defined( __IBMCPP__ ) || defined( _MSC_VER ) || \
      defined( __MINGW32__ ) || defined( __WATCOMC__ ) ) && \
      !defined( HB_OS_UNIX ) && !defined( HB_OS_WIN_CE )
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <process.h>
   #if !defined( __POCC__ ) && !defined( __XCC__ )
      #include <share.h>
   #endif
   #include <direct.h>
   #if defined( __BORLANDC__ )
      #include <dir.h>
      #include <dos.h>
   #elif defined( __WATCOMC__ )
      #include <dos.h>
   #endif

   #if defined( _MSC_VER ) || defined( __MINGW32__ ) || defined( __DMC__ )
      #include <sys/locking.h>
      #define ftruncate _chsize
      #if defined( __MINGW32__ ) && !defined( _LK_UNLCK )
         #define _LK_UNLCK _LK_UNLOCK
      #endif
   #else
      #define ftruncate chsize
   #endif
   #if !defined( HAVE_POSIX_IO )
      #define HAVE_POSIX_IO
   #endif
#elif defined( __GNUC__ ) || defined( HB_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #if defined( __DJGPP__ )
      #include <dir.h>
      #include <utime.h>
      #include <time.h>
   #endif
   #if !defined( HAVE_POSIX_IO )
      #define HAVE_POSIX_IO
   #endif
#endif

#if defined( __MPW__ )
   #include <fcntl.h>
#endif

#if defined( HB_OS_DOS )
   #include <dos.h>
   #include <time.h>
   #include <utime.h>
#elif defined( HB_OS_OS2 )
   #define INCL_BASE
   #define INCL_DOSFILEMGR
   #define INCL_DOSERRORS
   #define INCL_DOSDATETIME
   #include <os2.h>
   #include <time.h>
   #include <share.h>
   #ifndef SH_COMPAT
      #define SH_COMPAT    0x0000
   #endif
#elif defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
   #if !defined( INVALID_SET_FILE_POINTER ) && \
       ( defined( __DMC__ ) || defined( _MSC_VER ) || defined( __LCC__ ) )
      #define INVALID_SET_FILE_POINTER ( ( DWORD ) -1 )
   #endif
   #if !defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES     ( ( DWORD ) -1 )
   #endif
   #if defined( HB_OS_WIN_64 )
      #if !defined( HB_WIN_IOREAD_LIMIT )
         #define HB_WIN_IOREAD_LIMIT      HB_U32_MAX
      #endif
      #if !defined( HB_WIN_IOWRITE_LIMIT )
         #define HB_WIN_IOWRITE_LIMIT     HB_U32_MAX
      #endif
   #endif
#endif
#if defined( HB_USE_SHARELOCKS ) && defined( HB_USE_BSDLOCKS )
   #include <sys/file.h>
#endif

#if !defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and efectively enables lseek64/flock64/ftruncate64 functions
       * on 32bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE ) && ! defined( __WATCOMC__ )
      #define HB_USE_LARGEFILE64
   #endif
#endif

#if defined( HB_OS_HAS_DRIVE_LETTER )
/* 27/08/2004 - <maurilio.longo@libero.it>
                HB_FS_GETDRIVE() should return a number in the range 0..25 ('A'..'Z')
                HB_FS_SETDRIVE() should accept a number inside same range.

                If a particular platform/compiler returns/accepts different ranges of
                values, simply define a branch for that platform.

                NOTE: There is not an implicit "current disk", ALWAYS use

                        my_func( hb_fsCurDrv(), ...)

                      to refer to current disk
*/

#if defined( HB_OS_OS2 )
   /* 1 based version */

   #define HB_FS_GETDRIVE(n)  do { \
                                    ULONG ulDrive, ulLogical; \
                                    DosQueryCurrentDisk( &ulDrive, &ulLogical ); \
                                    ( n ) = ( int ) ulDrive - 1; \
                              } while( 0 )
   #define HB_FS_SETDRIVE(n)  do { DosSetDefaultDisk( ( n ) + 1 ); } while( 0 )

#elif defined( HB_OS_WIN )

   #define HB_FS_GETDRIVE(n)  do { n = fs_win_get_drive(); } while( 0 )
   #define HB_FS_SETDRIVE(n)  fs_win_set_drive( n )

#elif defined( __DJGPP__ ) || defined( __BORLANDC__ )
   /* 0 based version */

   #define HB_FS_GETDRIVE(n)  do { n = getdisk(); } while( 0 )
   #define HB_FS_SETDRIVE(n)  setdisk( n )

#elif defined( __WATCOMC__ )
   /* 1 based version */

   #define HB_FS_GETDRIVE(n)  do { \
                                 unsigned _u = 0; \
                                 _dos_getdrive( &_u ); n = _u - 1; \
                              } while( 0 )
   #define HB_FS_SETDRIVE(n)  do { \
                                 unsigned int _u = 0; \
                                 _dos_setdrive( ( n ) + 1, &_u ); \
                              } while( 0 )

#else /* _MSC_VER */
   /* 1 based version */

   #define HB_FS_GETDRIVE(n)  do { n = _getdrive() - 1; } while( 0 )
   #define HB_FS_SETDRIVE(n)  _chdrive( ( n ) + 1 )

#endif
#endif /* HB_OS_HAS_DRIVE_LETTER */

#ifndef O_BINARY
   #define O_BINARY     0       /* O_BINARY not defined on Linux */
#endif

#ifndef O_LARGEFILE
   #define O_LARGEFILE  0       /* O_LARGEFILE is used for LFS in 32-bit Linux */
#endif

#if !defined( HB_OS_UNIX )
   #if !defined( S_IREAD ) && defined( S_IRUSR )
      #define S_IREAD   S_IRUSR
   #endif
   #if !defined( S_IWRITE ) && defined( S_IWUSR )
      #define S_IWRITE  S_IWUSR
   #endif
   #if !defined( S_IEXEC ) && defined( S_IXUSR )
      #define S_IEXEC   S_IXUSR
   #endif
#endif


#if defined( __DMC__ ) || defined( _MSC_VER ) || defined( __MINGW32__ ) || \
    defined( __IBMCPP__ ) || defined( __WATCOMC__ ) || defined( HB_OS_OS2 )
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define HB_FS_SOPEN
#endif

#if defined( HB_OS_ANDROID )
   /* hack for missing functions in android libc library */
   #define fdatasync          fsync
   #define ftruncate64        ftruncate
   #define pread64            pread
   #define pwrite64(f,b,s,o)  pwrite(f,(void*)b,s,o)
#endif

#if UINT_MAX == USHRT_MAX
   #define HB_FS_IO_16BIT
#endif

#if defined( HB_OS_UNIX ) && defined( EINTR )
#  define HB_FAILURE_RETRY( ret, exp ) \
               do \
               { \
                  ( ret ) = ( exp ); \
                  hb_fsSetIOError( ( ret ) != -1, 0 ); \
               } \
               while( ( ret ) == -1 && hb_fsOsError() == EINTR && \
                      hb_vmRequestQuery() == 0 )
#else
#  define HB_FAILURE_RETRY( ret, exp ) \
               do \
               { \
                  ( ret ) = ( exp ); \
                  hb_fsSetIOError( ( ret ) != -1, 0 ); \
               } \
               while( 0 )
#endif

static HB_BOOL s_fUseWaitLocks = HB_TRUE;

#if defined( HB_OS_WIN ) && defined( HB_OS_HAS_DRIVE_LETTER )

static int fs_win_get_drive( void )
{
   int iDrive;
   char szBuffer[ HB_PATH_MAX ];
   PHB_FNAME pFilepath;

#if defined( UNICODE )
   {
      TCHAR lpBuffer[ HB_PATH_MAX ];
      hb_fsSetIOError( GetCurrentDirectory( HB_SIZEOFARRAY( lpBuffer ), lpBuffer ) != 0, 0 );
      lpBuffer[ HB_SIZEOFARRAY( lpBuffer ) - 1 ] = L'\0';
      hb_wcntombcpy( szBuffer, lpBuffer, HB_SIZEOFARRAY( lpBuffer ) - 1 );
   }
#else
   hb_fsSetIOError( GetCurrentDirectory( HB_SIZEOFARRAY( szBuffer ), szBuffer ) != 0, 0 );
#endif

   pFilepath = hb_fsFNameSplit( szBuffer );

   if( pFilepath->szDrive )
      iDrive = HB_TOUPPER( pFilepath->szDrive[ 0 ] ) - 'A';
   else
      iDrive = 0;

   hb_xfree( pFilepath );

   return iDrive;
}

static void fs_win_set_drive( int iDrive )
{
   if( iDrive >= 0 && iDrive <= 25 )
   {
      TCHAR szBuffer[ 3 ];
      HB_BOOL fResult;
      UINT uiErrMode;

      szBuffer[ 0 ] = ( TCHAR ) ( iDrive + 'A' );
      szBuffer[ 1 ] = TEXT( ':' );
      szBuffer[ 2 ] = TEXT( '\0' );

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( szBuffer ) != FALSE;
      SetErrorMode( uiErrMode );

      hb_fsSetIOError( fResult, 0 );
   }
}

#endif

#if defined( HB_OS_WIN )

static HANDLE DosToWinHandle( HB_FHANDLE fHandle )
{
   if( fHandle == ( HB_FHANDLE ) FS_ERROR )
      return NULL;

   else if( fHandle == ( HB_FHANDLE ) HB_STDIN_HANDLE )
      return GetStdHandle( STD_INPUT_HANDLE );

   else if( fHandle == ( HB_FHANDLE ) HB_STDOUT_HANDLE )
      return GetStdHandle( STD_OUTPUT_HANDLE );

   else if( fHandle == ( HB_FHANDLE ) HB_STDERR_HANDLE )
      return GetStdHandle( STD_ERROR_HANDLE );

   else
      return ( HANDLE ) fHandle;
}

static void convert_open_flags( HB_BOOL fCreate, HB_FATTR ulAttr, HB_USHORT uiFlags,
                                DWORD *dwMode, DWORD *dwShare,
                                DWORD *dwCreat, DWORD *dwAttr )
{
   if( fCreate )
   {
      *dwCreat = ( uiFlags & FO_EXCL ) ? CREATE_NEW : CREATE_ALWAYS;
      *dwMode = GENERIC_READ | GENERIC_WRITE;
   }
   else
   {
      if( uiFlags & FO_CREAT )
      {
         if( uiFlags & FO_EXCL )
            *dwCreat = CREATE_NEW;
         else if( uiFlags & FO_TRUNC )
            *dwCreat = CREATE_ALWAYS;
         else
            *dwCreat = OPEN_ALWAYS;
      }
      else if( uiFlags & FO_TRUNC )
         *dwCreat = TRUNCATE_EXISTING;
      else
         *dwCreat = OPEN_EXISTING;

      *dwMode = 0;
      switch( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
      {
         case FO_READWRITE:
            *dwMode |= GENERIC_READ | GENERIC_WRITE;
            break;
         case FO_WRITE:
            *dwMode |= GENERIC_WRITE;
            break;
         case FO_READ:
            *dwMode |= GENERIC_READ;
            break;
      }
   }

   /* shared flags */
   switch( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE | FO_DENYNONE ) )
   {
      case FO_DENYREAD:
         *dwShare = FILE_SHARE_WRITE;
         break;
      case FO_DENYWRITE:
         *dwShare = FILE_SHARE_READ;
         break;
      case FO_EXCLUSIVE:
         *dwShare = 0;
         break;
      default:
         *dwShare = FILE_SHARE_WRITE | FILE_SHARE_READ;
         break;
   }

   /* file attributes flags */
   if( ulAttr == FC_NORMAL )
   {
      *dwAttr = FILE_ATTRIBUTE_NORMAL;
   }
   else
   {
      *dwAttr = FILE_ATTRIBUTE_ARCHIVE;
      if( ulAttr & FC_READONLY )
         *dwAttr |= FILE_ATTRIBUTE_READONLY;
      if( ulAttr & FC_HIDDEN )
         *dwAttr |= FILE_ATTRIBUTE_HIDDEN;
      if( ulAttr & FC_SYSTEM )
         *dwAttr |= FILE_ATTRIBUTE_SYSTEM;
   }
}

#else

static void convert_open_flags( HB_BOOL fCreate, HB_FATTR ulAttr, HB_USHORT uiFlags,
                                int *flags, unsigned *mode,
                                int *share, int *attr )
{
   HB_TRACE(HB_TR_DEBUG, ("convert_open_flags(%d, %u, %hu, %p, %p, %p, %p)", fCreate, ulAttr, uiFlags, flags, mode, share, attr));

   /* file access mode */
#if defined( HB_OS_UNIX )
   *mode = HB_FA_POSIX_ATTR( ulAttr );
   if( *mode == 0 )
   {
      *mode = ( ulAttr & FC_HIDDEN ) ? S_IRUSR : ( S_IRUSR | S_IRGRP | S_IROTH );
      if( !( ulAttr & FC_READONLY ) )
      {
         if( *mode & S_IRUSR ) *mode |= S_IWUSR;
         if( *mode & S_IRGRP ) *mode |= S_IWGRP;
         if( *mode & S_IROTH ) *mode |= S_IWOTH;
      }
      if( ulAttr & FC_SYSTEM )
      {
         if( *mode & S_IRUSR ) *mode |= S_IXUSR;
         if( *mode & S_IRGRP ) *mode |= S_IXGRP;
         if( *mode & S_IROTH ) *mode |= S_IXOTH;
      }
   }
#else
   *mode = S_IREAD |
           ( ( ulAttr & FC_READONLY ) ? 0 : S_IWRITE ) |
           ( ( ulAttr & FC_SYSTEM ) ? S_IEXEC : 0 );
#endif

   /* dos file attributes */
#if defined( HB_FS_DOSATTR )
   if( ulAttr == FC_NORMAL )
   {
      *attr = _A_NORMAL;
   }
   else
   {
      *attr = _A_ARCH;
      if( ulAttr & FC_READONLY )
         *attr |= _A_READONLY;
      if( ulAttr & FC_HIDDEN )
         *attr |= _A_HIDDEN;
      if( ulAttr & FC_SYSTEM )
         *attr |= _A_SYSTEM;
   }
#else
   *attr = 0;
#endif

   if( fCreate )
   {
      *flags = O_RDWR | O_CREAT | O_TRUNC | O_BINARY | O_LARGEFILE |
               ( ( uiFlags & FO_EXCL ) ? O_EXCL : 0 );
   }
   else
   {
      *attr = 0;
      *flags = O_BINARY | O_LARGEFILE;
      switch( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
      {
         case FO_READ:
            *flags |= O_RDONLY;
            break;
         case FO_WRITE:
            *flags |= O_WRONLY;
            break;
         case FO_READWRITE:
            *flags |= O_RDWR;
            break;
         default:
            /* this should not happen and it's here to force default OS behavior */
            *flags |= ( O_RDONLY | O_WRONLY | O_RDWR );
            break;
      }

      if( uiFlags & FO_CREAT ) *flags |= O_CREAT;
      if( uiFlags & FO_TRUNC ) *flags |= O_TRUNC;
      if( uiFlags & FO_EXCL  ) *flags |= O_EXCL;
   }

   /* shared flags (HB_FS_SOPEN) */
#if defined( _MSC_VER ) || defined( __DMC__ )
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
      *share = _SH_DENYRD;
   else if( uiFlags & FO_EXCLUSIVE )
      *share = _SH_DENYRW;
   else if( uiFlags & FO_DENYWRITE )
      *share = _SH_DENYWR;
   else if( uiFlags & FO_DENYNONE )
      *share = _SH_DENYNO;
   else
      *share = _SH_COMPAT;
#elif !defined( HB_OS_UNIX )
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
      *share = SH_DENYRD;
   else if( uiFlags & FO_EXCLUSIVE )
      *share = SH_DENYRW;
   else if( uiFlags & FO_DENYWRITE )
      *share = SH_DENYWR;
   else if( uiFlags & FO_DENYNONE )
      *share = SH_DENYNO;
   else
      *share = SH_COMPAT;
#else
   *share = 0;
#endif

   HB_TRACE(HB_TR_INFO, ("convert_open_flags: flags=0x%04x, mode=0x%04x, share=0x%04x, attr=0x%04x", *flags, *mode, *share, *attr));

}
#endif

static HB_USHORT convert_seek_flags( HB_USHORT uiFlags )
{
   /* by default FS_SET is set */
   HB_USHORT result_flags = SEEK_SET;

   HB_TRACE(HB_TR_DEBUG, ("convert_seek_flags(%hu)", uiFlags));

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}


/*
 * filesys.api functions:
 */

HB_FHANDLE hb_fsGetOsHandle( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetOsHandle(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

#if defined( HB_OS_WIN )
   return ( HB_FHANDLE ) DosToWinHandle( hFileHandle );
#else
   return hFileHandle;
#endif
}

HB_FHANDLE hb_fsPOpen( const char * pFilename, const char * pMode )
{
   HB_FHANDLE hFileHandle = FS_ERROR;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPOpen(%p, %s)", pFilename, pMode));

#if defined( HB_OS_UNIX ) && !defined( HB_OS_VXWORKS ) && !defined( HB_OS_SYMBIAN )
   {
      HB_FHANDLE hPipeHandle[ 2 ], hNullHandle;
      pid_t pid;
      char * pszTmp;
      HB_BOOL fRead;
      HB_SIZE nLen;
      int iMaxFD, iResult;

      nLen = strlen( pFilename );
      if( pMode && ( *pMode == 'r' || *pMode == 'w' ) )
         fRead = ( *pMode == 'r' );
      else
      {
         if( pFilename[0] == '|' )
            fRead = HB_FALSE;
         else if( pFilename[ nLen - 1 ] == '|' )
            fRead = HB_TRUE;
         else
            fRead = HB_FALSE;
      }

      if( pFilename[0] == '|' )
      {
          ++pFilename;
          --nLen;
      }
      if( pFilename[ nLen - 1 ] == '|' )
      {
          pszTmp = hb_strdup( pFilename );
          pszTmp[ --nLen ] = 0;
          pFilename = pszTmp;
      } else
          pszTmp = NULL;

      hb_vmUnlock();
      if( pipe( hPipeHandle ) == 0 )
      {
         if( ( pid = fork() ) != -1 )
         {
            if( pid != 0 )
            {
               if( fRead )
               {
                  hb_fsClose( hPipeHandle[ 1 ] );
                  hFileHandle = hPipeHandle[ 0 ];
               }
               else
               {
                  hb_fsClose( hPipeHandle[ 0 ] );
                  hFileHandle = hPipeHandle[ 1 ];
               }
            }
            else
            {
               const char * argv[ 4 ];
               argv[0] = "sh";
               argv[1] = "-c";
               argv[2] = pFilename;
               argv[3] = 0;
               HB_FAILURE_RETRY( hNullHandle, open( "/dev/null", O_RDWR ) );
               if( fRead )
               {
                  hb_fsClose( hPipeHandle[ 0 ] );
                  HB_FAILURE_RETRY( iResult, dup2( hPipeHandle[ 1 ], 1 ) );
                  HB_FAILURE_RETRY( iResult, dup2( hNullHandle, 0 ) );
                  HB_FAILURE_RETRY( iResult, dup2( hNullHandle, 2 ) );
               }
               else
               {
                  hb_fsClose( hPipeHandle[ 1 ] );
                  HB_FAILURE_RETRY( iResult, dup2( hPipeHandle[ 0 ], 0 ) );
                  HB_FAILURE_RETRY( iResult, dup2( hNullHandle, 1 ) );
                  HB_FAILURE_RETRY( iResult, dup2( hNullHandle, 2 ) );
                  dup2( hNullHandle, 2 );
               }
               iMaxFD = sysconf( _SC_OPEN_MAX );
               if( iMaxFD < 3 )
                  iMaxFD = 1024;
               for( hNullHandle = 3; hNullHandle < iMaxFD; ++hNullHandle )
                  hb_fsClose( hNullHandle );
               setuid( getuid() );
               setgid( getgid() );
#if defined( __WATCOMC__ )
               HB_FAILURE_RETRY( iResult, execv( "/bin/sh", argv ) );
#else
               HB_FAILURE_RETRY( iResult, execv( "/bin/sh", ( char ** ) argv ) );
#endif
               exit( 1 );
            }
         }
         else
         {
            hb_fsClose( hPipeHandle[ 0 ] );
            hb_fsClose( hPipeHandle[ 1 ] );
         }
      }
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      hb_vmLock();

      if( pszTmp )
         hb_xfree( pszTmp );
   }
#else

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( pMode );

   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#endif

   return hFileHandle;
}

HB_BOOL hb_fsPipeCreate( HB_FHANDLE hPipe[ 2 ] )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeCreate(%p)", hPipe));

#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )
{
   SECURITY_ATTRIBUTES sa;
   HANDLE hPipeRd, hPipeWr;

   memset( &sa, 0, sizeof( sa ) );
   sa.nLength = sizeof( sa );
   sa.bInheritHandle = TRUE;

   fResult = CreatePipe( &hPipeRd, &hPipeWr, &sa, 0 ) != 0;
   if( fResult )
   {
      hPipe[ 0 ] = ( HB_FHANDLE ) hPipeRd;
      hPipe[ 1 ] = ( HB_FHANDLE ) hPipeWr;
   }
   else
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
}
#elif defined( HB_OS_OS2 )
{
#  if defined( __GNUC__ )
      fResult = pipe( hPipe ) == 0;
      if( fResult )
      {
         setmode( hPipe[ 0 ], O_BINARY );
         setmode( hPipe[ 1 ], O_BINARY );
      }
      else
         hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
#  else
      fResult = _pipe( hPipe, 4096, _O_BINARY ) == 0;
      if( !fResult )
         hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
#  endif
}
#elif defined( HB_OS_UNIX ) && !defined( HB_OS_VXWORKS ) && !defined( HB_OS_SYMBIAN )
{
   fResult = pipe( hPipe ) == 0;
   if( !fResult )
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
}
#else
{
#  if !defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif

   hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
   fResult = HB_FALSE;
}
#endif

   return fResult;
}

int hb_fsIsPipeOrSock( HB_FHANDLE hPipeHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsPipeOrSock(%p)", ( void * ) ( HB_PTRDIFF ) hPipeHandle));

#if defined( HB_OS_UNIX )
{
   struct stat statbuf;
   if( fstat( hPipeHandle, &statbuf ) == 0 )
   {
      if( S_ISFIFO( statbuf.st_mode ) || S_ISSOCK( statbuf.st_mode ) )
         return 1;
   }
   return 0;
}
#elif defined( HB_OS_WIN )
{
   return ( GetFileType( ( HANDLE ) hb_fsGetOsHandle( hPipeHandle ) ) ==
            FILE_TYPE_PIPE ) ? 1 : 0;
}
#elif defined( HB_OS_OS2 )
{
   ULONG type = 0, attr = 0;
   if( DosQueryHType( ( HFILE ) hPipeHandle, &type, &attr ) == NO_ERROR )
   {
      if( ( type & 0xFF ) == FHT_PIPE )
         return 1;
   }
   return 0;
}
#else
#  if !defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif
   HB_SYMBOL_UNUSED( hPipeHandle );
   return -1;
#endif
}

HB_SIZE hb_fsPipeIsData( HB_FHANDLE hPipeHandle, HB_SIZE nBufferSize,
                         HB_MAXINT nTimeOut )
{
   HB_SIZE nToRead = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeIsData(%p,%" HB_PFS "u,%" PFHL "d)", ( void * ) ( HB_PTRDIFF ) hPipeHandle, nBufferSize, nTimeOut));

   hb_vmUnlock();

#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )
{
   HB_MAXUINT end_timer = nTimeOut > 0 ? hb_dateMilliSeconds() + nTimeOut : 0;
   HB_BOOL fResult = HB_FALSE;
   DWORD dwAvail;

   do
   {
      if( fResult )
         hb_releaseCPU();

      dwAvail = 0;
      fResult = PeekNamedPipe( ( HANDLE ) hb_fsGetOsHandle( hPipeHandle ),
                               NULL, 0, NULL, &dwAvail, NULL ) != 0;
      hb_fsSetIOError( fResult, 0 );
   }
   while( fResult && dwAvail == 0 &&
          ( nTimeOut < 0 || ( end_timer > 0 &&
                              end_timer > hb_dateMilliSeconds() ) ) &&
          hb_vmRequestQuery() == 0 );

   if( !fResult )
      nToRead = ( HB_SIZE ) -1;
   else if( dwAvail > 0 )
      nToRead = ( ( HB_SIZE ) dwAvail < nBufferSize ) ? dwAvail : nBufferSize;
}
#elif defined( HB_OS_OS2 )
{
   HB_MAXUINT end_timer = nTimeOut > 0 ? hb_dateMilliSeconds() + nTimeOut : 0;
   HB_BOOL fResult = HB_FALSE;
   AVAILDATA avail;

   do
   {
      APIRET ret;

      if( fResult )
         hb_releaseCPU();

      avail.cbpipe = 0;
      avail.cbmessage = 0;
      ret = DosPeekNPipe( ( HPIPE ) hPipeHandle,
                          NULL, 0, NULL, &avail, NULL );
      fResult = ret == NO_ERROR || ret == ERROR_PIPE_BUSY;
      hb_fsSetIOError( fResult, 0 );
   }
   while( fResult && avail.cbpipe == 0 &&
          ( nTimeOut < 0 || ( end_timer > 0 &&
                              end_timer > hb_dateMilliSeconds() ) ) &&
          hb_vmRequestQuery() == 0 );

   if( !fResult )
      nToRead = ( HB_SIZE ) -1;
   else if( avail.cbpipe > 0 )
      nToRead = ( ( HB_SIZE ) avail.cbpipe < nBufferSize ) ? avail.cbpipe :
                                                             nBufferSize;
}
#elif defined( HB_OS_UNIX ) && !defined( HB_OS_SYMBIAN )
{
   struct timeval tv;
   fd_set rfds;
   int iResult;
#if !defined( HB_OS_LINUX )
   HB_MAXUINT timer = nTimeOut <= 0 ? 0 : hb_dateMilliSeconds();
#endif

   for( ;; )
   {
      if( nTimeOut < 0 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
      else
      {
         tv.tv_sec = nTimeOut / 1000;
         tv.tv_usec = ( nTimeOut % 1000 ) * 1000;
      }

      FD_ZERO( &rfds );
      FD_SET( hPipeHandle, &rfds );
      iResult = select( hPipeHandle + 1, &rfds, NULL, NULL, &tv );
      hb_fsSetIOError( iResult >= 0, 0 );
      if( iResult != -1 || nTimeOut == 0 || errno != EINTR ||
          hb_vmRequestQuery() != 0 )
         break;
#if !defined( HB_OS_LINUX )
      else if( nTimeOut > 0 )
      {
         HB_MAXUINT timecurr = hb_dateMilliSeconds();
         if( timecurr > timer )
         {
            if( ( nTimeOut -= timecurr - timer ) <= 0 )
               break;
            timer = timecurr;
         }
      }
#endif
   }
   if( iResult > 0 )
      nToRead = nBufferSize;
}
#else
{
#  if !defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif
   HB_SYMBOL_UNUSED( hPipeHandle );
   HB_SYMBOL_UNUSED( nBufferSize );
   HB_SYMBOL_UNUSED( nTimeOut );
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
}
#endif

   hb_vmLock();

   return nToRead;
}

HB_SIZE hb_fsPipeRead( HB_FHANDLE hPipeHandle, void * buffer, HB_SIZE nSize,
                       HB_MAXINT nTimeOut )
{
   HB_SIZE nRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPipeRead(%p,%p,%" HB_PFS "u,%" PFHL "d)", ( void * ) ( HB_PTRDIFF ) hPipeHandle, buffer, nSize, nTimeOut));

   nRead = hb_fsPipeIsData( hPipeHandle, nSize, nTimeOut );
   if( nRead != ( HB_SIZE ) -1 && nRead > 0 )
      nRead = hb_fsReadLarge( hPipeHandle, buffer, nRead );

   return nRead;
}

HB_FHANDLE hb_fsOpen( const char * pFilename, HB_USHORT uiFlags )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpen(%s, %hu)", pFilename, uiFlags));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pFilename );
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( HB_FALSE, FC_NORMAL, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = CreateFile( lpFilename, dwMode, dwShare, NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      HB_TCHAR_FREE( lpFilename );

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#else
   {
      int flags, share, attr;
      unsigned mode;

      convert_open_flags( HB_FALSE, FC_NORMAL, uiFlags, &flags, &mode, &share, &attr );

      hb_vmUnlock();
#if defined( _MSC_VER ) || defined( __DMC__ )
      if( share )
         hFileHandle = _sopen( pFilename, flags, share, mode );
      else
         hFileHandle = _open( pFilename, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#elif defined( HB_FS_SOPEN )
      if( share )
         hFileHandle = sopen( pFilename, flags, share, mode );
      else
         hFileHandle = open( pFilename, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#else
      HB_FAILURE_RETRY( hFileHandle, open( pFilename, flags | share, mode ) );
#endif
      hb_vmLock();
   }
#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

HB_FHANDLE hb_fsCreate( const char * pFilename, HB_FATTR ulAttr )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%s, %u)", pFilename, ulAttr));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pFilename );
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( HB_TRUE, ulAttr, FO_EXCLUSIVE, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = CreateFile( lpFilename, dwMode, dwShare, NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      HB_TCHAR_FREE( lpFilename );

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#else
   {
      int flags, share, attr;
      unsigned mode;

      convert_open_flags( HB_TRUE, ulAttr, FO_EXCLUSIVE, &flags, &mode, &share, &attr );

      hb_vmUnlock();
#if defined( HB_FS_DOSCREAT )
      hFileHandle = _creat( pFilename, attr );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#elif defined( HB_FS_SOPEN )
      hFileHandle = open( pFilename, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#else
      HB_FAILURE_RETRY( hFileHandle, open( pFilename, flags | share, mode ) );
#endif
      hb_vmLock();
   }
#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

/* Derived from hb_fsCreate()

   NOTE: The default opening mode differs from the one used in hb_fsCreate()
         [vszakats]
 */

HB_FHANDLE hb_fsCreateEx( const char * pFilename, HB_FATTR ulAttr, HB_USHORT uiFlags )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreateEx(%s, %u, %hu)", pFilename, ulAttr, uiFlags));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pFilename );
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( HB_TRUE, ulAttr, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = CreateFile( lpFilename, dwMode, dwShare, NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      HB_TCHAR_FREE( lpFilename );

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#else
   {
      int flags, share, attr;
      unsigned mode;

      convert_open_flags( HB_TRUE, ulAttr, uiFlags, &flags, &mode, &share, &attr );

      hb_vmUnlock();
#if defined( HB_FS_SOPEN )
      hFileHandle = open( pFilename, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#else
      HB_FAILURE_RETRY( hFileHandle, open( pFilename, flags | share, mode ) );
#endif
      hb_vmLock();
   }
#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

void hb_fsClose( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

   hb_vmUnlock();
#if defined( HB_OS_WIN )
   hb_fsSetIOError( CloseHandle( DosToWinHandle( hFileHandle ) ) != 0, 0 );
#else
   {
      int ret;
#  if defined( EINTR )
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
         ret = close( hFileHandle );
      while( ret == -1 && errno == EINTR );
#  else
      ret = close( hFileHandle );
#  endif
      hb_fsSetIOError( ret == 0, 0 );
   }
#endif
   hb_vmLock();
}


#define FD_TEST   0

int hb_fsSetDevMode( HB_FHANDLE hFileHandle, int iDevMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetDevMode(%p, %d)", ( void * ) ( HB_PTRDIFF ) hFileHandle, iDevMode));

#if defined( __BORLANDC__ ) || defined( __IBMCPP__ ) || defined( __DJGPP__ ) || \
    defined( __WATCOMC__ ) || defined( HB_OS_OS2 )
{
   int iRet = -1;

#if defined( HB_OS_WIN )
   if( hFileHandle == ( HB_FHANDLE ) 0 ||
       hFileHandle == ( HB_FHANDLE ) 1 ||
       hFileHandle == ( HB_FHANDLE ) 2 )
#endif
   switch( iDevMode )
   {
      case FD_TEST:
         iRet = setmode( ( int ) hFileHandle, O_BINARY );
         if( iRet != -1 )
            setmode( ( int ) hFileHandle, iRet );
         break;

      case FD_BINARY:
         iRet = setmode( ( int ) hFileHandle, O_BINARY );
         break;

      case FD_TEXT:
         iRet = setmode( ( int ) hFileHandle, O_TEXT );
         break;
   }

   if( iRet != -1 )
      iRet = ( iRet & O_TEXT ) == O_TEXT ? FD_TEXT : FD_BINARY;
   hb_fsSetIOError( iRet != -1, 0 );

   return iRet;
}
#elif ( defined( _MSC_VER ) || defined( __MINGW32__ ) || defined( __DMC__ ) ) && \
      !defined( HB_OS_WIN_CE )
{
   int iRet = -1;

#if defined( HB_OS_WIN )
   if( hFileHandle == ( HB_FHANDLE ) 0 ||
       hFileHandle == ( HB_FHANDLE ) 1 ||
       hFileHandle == ( HB_FHANDLE ) 2 )
#endif
   switch( iDevMode )
   {
      case FD_TEST:
         iRet = _setmode( ( int ) hFileHandle, _O_BINARY );
         if( iRet != -1 )
            _setmode( ( int ) hFileHandle, iRet );
         break;

      case FD_BINARY:
         iRet = _setmode( ( int ) hFileHandle, _O_BINARY );
         break;

      case FD_TEXT:
         iRet = _setmode( ( int ) hFileHandle, _O_TEXT );
         break;
   }

   if( iRet != -1 )
      iRet = ( iRet & _O_TEXT ) == _O_TEXT ? FD_TEXT : FD_BINARY;
   hb_fsSetIOError( iRet != -1, 0 );

   return iRet;
}
#else

   HB_SYMBOL_UNUSED( hFileHandle );

   hb_fsSetError( ( HB_ERRCODE ) ( iDevMode == FD_TEXT ? FS_ERROR : 0 ) );
   return FD_BINARY;

#endif
}

HB_BOOL hb_fsGetFileTime( const char * pszFileName, long * plJulian, long * plMillisec )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetFileTime(%s, %p, %p)", pszFileName, plJulian, plMillisec));

   fResult = HB_FALSE;

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READ | FO_SHARED );

      if( hFile != FS_ERROR )
      {
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         if( GetFileTime( DosToWinHandle( hFile ), NULL, NULL, &ft ) &&
             FileTimeToLocalFileTime( &ft, &local_ft ) &&
             FileTimeToSystemTime( &local_ft, &st ) )
         {
            *plJulian = hb_dateEncode( st.wYear, st.wMonth, st.wDay );
            *plMillisec = hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

            fResult = HB_TRUE;
         }
         hb_fsSetIOError( fResult, 0 );
         hb_fsClose( hFile );
      }
   }
#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 ) || defined( HB_OS_DOS ) || defined( __GNUC__ )
   {
      struct stat sStat;
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      if( stat( pszFileName, &sStat ) == 0 )
      {
         time_t ftime;
         struct tm ft;

         ftime = sStat.st_mtime;
#  if defined( HB_HAS_LOCALTIME_R )
         localtime_r( &ftime, &ft );
#  else
         ft = *localtime( &ftime );
#  endif

         *plJulian = hb_dateEncode( ft.tm_year + 1900, ft.tm_mon + 1, ft.tm_mday );
#if defined( HB_OS_LINUX ) && ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
    defined( __GLIBC__ ) && defined( __GLIBC_MINOR__ ) && \
           ( __GLIBC__ > 2 || ( __GLIBC__ == 2 && __GLIBC_MINOR__ >= 6 ) )
         *plMillisec = hb_timeEncode( ft.tm_hour, ft.tm_min, ft.tm_sec, sStat.st_mtim.tv_nsec / 1000000 );
#else
         *plMillisec = hb_timeEncode( ft.tm_hour, ft.tm_min, ft.tm_sec, 0 );
#endif
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      int iTODO; /* TODO: for given platform */

      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( plJulian );
      HB_SYMBOL_UNUSED( plMillisec );
   }
#endif

   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fsGetAttr( const char * pszFileName, HB_FATTR * pulAttr )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetAttr(%s, %p)", pszFileName, pulAttr));

   *pulAttr = 0;
   fResult = HB_FALSE;
   pszFileName = hb_fsNameConv( pszFileName, &pszFree );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pszFileName );
      DWORD dwAttr;

      dwAttr = GetFileAttributes( lpFilename );

      if( dwAttr != INVALID_FILE_ATTRIBUTES )
      {
         *pulAttr = hb_fsAttrFromRaw( dwAttr );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpFilename );
   }
#elif defined( HB_OS_DOS )
   {
#if defined( __DJGPP__ ) || defined( __BORLANDC__ )
      int attr = _chmod( pszFileName, 0, 0 );
      if( attr != -1 )
#else
      unsigned int attr = 0;
      if( _dos_getfileattr( pszFileName, &attr ) == 0 )
#endif
      {
         *pulAttr = hb_fsAttrFromRaw( attr );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ulrc;

      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         *pulAttr = hb_fsAttrFromRaw( fs3.attrFile );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
#elif defined( HB_OS_UNIX )
   {
      struct stat sStat;

      if( stat( pszFileName, &sStat ) == 0 )
      {
         *pulAttr = hb_fsAttrFromRaw( sStat.st_mode );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
#else
   {
      int iTODO; /* TODO: for given platform */

      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( pulAttr );
   }
#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

HB_BOOL hb_fsSetFileTime( const char * pszFileName, long lJulian, long lMillisec )
{
   HB_BOOL fResult;
   int iYear, iMonth, iDay;
   int iHour, iMinute, iSecond, iMSec;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetFileTime(%s, %ld, %ld)", pszFileName, lJulian, lMillisec));

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READWRITE | FO_SHARED );

      fResult = hFile != FS_ERROR;
      if( fResult )
      {
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         if( lJulian <= 0 || lMillisec < 0 )
            GetLocalTime( &st );
         else
            memset( &st, 0, sizeof( st ) );

         if( lJulian > 0 )
         {
            st.wYear = ( WORD ) iYear;
            st.wMonth = ( WORD ) iMonth;
            st.wDay = ( WORD ) iDay;
         }
         if( lMillisec >= 0 )
         {
            st.wHour = ( WORD ) iHour;
            st.wMinute = ( WORD ) iMinute;
            st.wSecond = ( WORD ) iSecond;
            st.wMilliseconds = ( WORD ) iMSec;
         }
         SystemTimeToFileTime( &st, &local_ft );
         LocalFileTimeToFileTime( &local_ft, &ft );
         fResult = SetFileTime( DosToWinHandle( hFile ), NULL, &ft, &ft ) != 0;
         hb_fsSetIOError( fResult, 0 );
         hb_fsClose( hFile );
      }
   }
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ulrc;
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         FDATE fdate;
         FTIME ftime;

         if( lJulian <= 0 || lMillisec < 0 )
         {
            DATETIME dt;

            DosGetDateTime( &dt );

            if( lJulian <= 0 )
            {
               iYear = dt.year;
               iMonth = dt.month;
               iDay = dt.day;
            }
            if( lMillisec < 0 )
            {
               iHour = dt.hours;
               iMinute = dt.minutes;
               iSecond = dt.seconds;
            }
         }

         fdate.year = iYear - 1980;
         fdate.month = iMonth;
         fdate.day = iDay;
         ftime.hours = iHour;
         ftime.minutes = iMinute;
         ftime.twosecs = iSecond / 2;

         fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
         fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
         ulrc = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }
      fResult = ulrc == NO_ERROR;
      hb_fsSetIOError( fResult, 0 );
      if( pszFree )
         hb_xfree( pszFree );
   }
#elif defined( HB_OS_UNIX ) || defined( HB_OS_DOS )
   {
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      if( lJulian <= 0 && lMillisec )
      {
         fResult = utime( pszFileName, NULL ) == 0;
      }
      else
      {
         struct tm new_value;
         time_t tim;

         if( lJulian <= 0 || lMillisec < 0 )
         {
            time_t current_time;

            current_time = time( NULL );
#  if defined( HB_HAS_LOCALTIME_R )
            localtime_r( &current_time, &new_value );
#  else
            new_value = *localtime( &current_time );
#  endif
         }
         else
            memset( &new_value, 0, sizeof( new_value ) );

         if( lJulian > 0 )
         {
            new_value.tm_year = iYear - 1900;
            new_value.tm_mon = iMonth - 1;
            new_value.tm_mday = iDay;
         }
         if( lMillisec >= 0 )
         {
            new_value.tm_hour = iHour;
            new_value.tm_min = iMinute;
            new_value.tm_sec = iSecond;
         }
         tim = mktime( &new_value );
#  if defined( HB_HAS_LOCALTIME_R )
         gmtime_r( &tim, &new_value );
#  else
         new_value = *gmtime( &tim );
#  endif
#if defined( HB_OS_LINUX ) && !defined( __WATCOMC__ )
         {
            struct timeval times[ 2 ];
            times[ 0 ].tv_sec = times[ 1 ].tv_sec = mktime( &new_value );
            times[ 0 ].tv_usec = times[ 1 ].tv_usec = iMSec * 1000;
            fResult = utimes( pszFileName, times ) == 0;
         }
#else
         {
            struct utimbuf buf;
            buf.actime = buf.modtime = mktime( &new_value );
            fResult = utime( pszFileName, &buf ) == 0;
         }
#endif
      }
      hb_fsSetIOError( fResult, 0 );
      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      int iTODO; /* To force warning */

      fResult = HB_FALSE;
      hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
   }
#endif

   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fsSetAttr( const char * pszFileName, HB_FATTR ulAttr )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetAttr(%s, %u)", pszFileName, ulAttr));

   pszFileName = hb_fsNameConv( pszFileName, &pszFree );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pszFileName );
      DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;

      if( ulAttr & HB_FA_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;
      if( ulAttr & HB_FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;
      if( ulAttr & HB_FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;
      if( ulAttr & HB_FA_NORMAL )
         dwFlags |= FILE_ATTRIBUTE_NORMAL;
      fResult = SetFileAttributes( lpFilename, dwFlags ) != 0;
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpFilename );
   }
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ulrc;
      ULONG ulOsAttr = FILE_NORMAL;

      if( ulAttr & HB_FA_READONLY )
         ulOsAttr |= FILE_READONLY;
      if( ulAttr & HB_FA_HIDDEN )
         ulOsAttr |= FILE_HIDDEN;
      if( ulAttr & HB_FA_SYSTEM )
         ulOsAttr |= FILE_SYSTEM;
      if( ulAttr & HB_FA_ARCHIVE )
         ulOsAttr |= FILE_ARCHIVED;

      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         fs3.attrFile = ulOsAttr;
         ulrc = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }
      fResult = ulrc == NO_ERROR;
      hb_fsSetIOError( fResult, 0 );
   }
#elif defined( HB_OS_DOS )

   ulAttr &= ~( HB_FA_ARCHIVE | HB_FA_HIDDEN | HB_FA_READONLY | HB_FA_SYSTEM );
#  if defined( __DJGPP__ ) || defined( __BORLANDC__ )
   fResult = _chmod( pszFileName, 1, ulAttr ) != -1;
#  else
   fResult = _dos_setfileattr( pszFileName, ulAttr ) != -1;
#  endif
   hb_fsSetIOError( fResult, 0 );

#elif defined( HB_OS_UNIX )
   {
      int iAttr = HB_FA_POSIX_ATTR( ulAttr ), iResult;
      if( iAttr == 0 )
      {
         iAttr = ( ulAttr & HB_FA_HIDDEN ) ? S_IRUSR : ( S_IRUSR | S_IRGRP | S_IROTH );
         if( !( ulAttr & HB_FA_READONLY ) )
         {
            if( iAttr & S_IRUSR ) iAttr |= S_IWUSR;
            if( iAttr & S_IRGRP ) iAttr |= S_IWGRP;
            if( iAttr & S_IROTH ) iAttr |= S_IWOTH;
         }
         if( ulAttr & HB_FA_SYSTEM )
         {
            if( iAttr & S_IRUSR ) iAttr |= S_IXUSR;
            if( iAttr & S_IRGRP ) iAttr |= S_IXGRP;
            if( iAttr & S_IROTH ) iAttr |= S_IXOTH;
         }
      }
      HB_FAILURE_RETRY( iResult, chmod( pszFileName, iAttr ) );
      fResult = iResult != -1;
   }
#else
   {
      int iTODO; /* To force warning */

      fResult = HB_FALSE;
      hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
   }
#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

HB_USHORT hb_fsRead( HB_FHANDLE hFileHandle, void * pBuff, HB_USHORT uiCount )
{
   HB_USHORT uiRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRead(%p, %p, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, uiCount));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      DWORD dwRead;
      BOOL bResult;

      bResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, ( DWORD ) uiCount, &dwRead, NULL );
      hb_fsSetIOError( bResult != 0, 0 );

      uiRead = bResult ? ( HB_USHORT ) dwRead : 0;
   }
#else
   {
      long lRead;
      HB_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, uiCount ) );
      uiRead = lRead == -1 ? 0 : ( HB_USHORT ) lRead;
   }
#endif

   hb_vmLock();

   return uiRead;
}

HB_USHORT hb_fsWrite( HB_FHANDLE hFileHandle, const void * pBuff, HB_USHORT uiCount )
{
   HB_USHORT uiWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWrite(%p, %p, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, uiCount));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      DWORD dwWritten = 0;
      BOOL bResult;

      if( uiCount )
      {
          bResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff, uiCount, &dwWritten, NULL );
      }
      else
      {
          dwWritten = 0;
          bResult = SetEndOfFile( DosToWinHandle( hFileHandle ) );
      }
      hb_fsSetIOError( bResult != 0, 0 );

      uiWritten = bResult ? ( HB_USHORT ) dwWritten : 0;
   }
#else
   if( uiCount )
   {
      long lWritten;
      HB_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, uiCount ) );
      uiWritten = lWritten == -1 ? 0 : ( HB_USHORT ) lWritten;
   }
   else
   {
      int iResult;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) );
#  else
      HB_FAILURE_RETRY( iResult, ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) );
#  endif
      uiWritten = 0;
   }
#endif

   hb_vmLock();

   return uiWritten;
}

HB_SIZE hb_fsReadLarge( HB_FHANDLE hFileHandle, void * pBuff, HB_SIZE nCount )
{
   HB_SIZE nRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLarge(%p, %p, %" HB_PFS "u)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, nCount));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
#  if defined( HB_WIN_IOREAD_LIMIT )
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      BOOL bResult = TRUE;

      nRead = 0;

      while( nCount )
      {
         DWORD dwToRead;
         DWORD dwRead;

         /* Determine how much to read this time */
         if( nCount > ( HB_SIZE ) HB_WIN_IOREAD_LIMIT )
         {
            dwToRead = HB_WIN_IOREAD_LIMIT;
            nCount -= ( HB_SIZE ) dwToRead;
         }
         else
         {
            dwToRead = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = ReadFile( hWFileHandle, ( HB_UCHAR * ) pBuff + nRead,
                             dwToRead, &dwRead, NULL );
         if( ! bResult )
            break;

         nRead += ( HB_SIZE ) dwRead;

         if( dwRead != dwToRead )
            break;
      }
#  else
      DWORD dwRead;
      BOOL bResult;

      bResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, nCount, &dwRead, NULL );
      nRead = bResult ? ( HB_SIZE ) dwRead : 0;
#  endif
      hb_fsSetIOError( bResult != 0, 0 );
   }
#elif defined( HB_FS_IO_16BIT )
   {
      nRead = 0;

      while( nCount )
      {
         unsigned int uiToRead;
         long lRead;

         /* Determine how much to read this time */
         if( nCount > ( HB_SIZE ) INT_MAX )
         {
            uiToRead = INT_MAX;
            nCount -= ( HB_SIZE ) uiToRead;
         }
         else
         {
            uiToRead = ( unsigned int ) nCount;
            nCount = 0;
         }

         HB_FAILURE_RETRY( lRead, read( hFileHandle, ( HB_UCHAR * ) pBuff + nRead, uiToRead ) );

         if( lRead <= 0 )
            break;

         nRead += lRead;

         if( lRead != ( long ) uiToRead )
            break;
      }
   }
#else
   {
      long lRead;
      HB_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, nCount ) );
      nRead = lRead == -1 ? 0 : lRead;
   }
#endif

   hb_vmLock();

   return nRead;
}

HB_SIZE hb_fsWriteLarge( HB_FHANDLE hFileHandle, const void * pBuff, HB_SIZE nCount )
{
   HB_SIZE nWritten = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteLarge(%p, %p, %" HB_PFS "u)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, nCount));

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   if( nCount )
   {
#  if defined( HB_WIN_IOWRITE_LIMIT )
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      BOOL bResult = TRUE;

      while( nCount )
      {
         DWORD dwToWrite;
         DWORD dwWritten;

         /* Determine how much to write this time */
         if( nCount > ( HB_SIZE ) HB_WIN_IOWRITE_LIMIT )
         {
            dwToWrite = HB_WIN_IOWRITE_LIMIT;
            nCount -= ( HB_SIZE ) dwToWrite;
         }
         else
         {
            dwToWrite = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = WriteFile( hWFileHandle, ( const HB_UCHAR * ) pBuff + nWritten,
                              dwToWrite, &dwWritten, NULL );
         if( ! bResult )
            break;

         nWritten += ( HB_SIZE ) dwWritten;

         if( dwWritten != dwToWrite )
            break;
      }
#  else
      DWORD dwWritten;
      BOOL bResult;
      bResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff,
                           nCount, &dwWritten, NULL );
      if( bResult )
         nWritten = ( HB_SIZE ) dwWritten;
#  endif
      hb_fsSetIOError( bResult != 0, 0 );
   }
   else
      hb_fsSetIOError( SetEndOfFile( DosToWinHandle( hFileHandle ) ) != 0, 0 );

#else

   if( nCount )
   {
#  if defined( HB_FS_IO_16BIT )
      while( nCount )
      {
         unsigned int uiToWrite;
         long lWritten;

         /* Determine how much to write this time */
         if( nCount > ( HB_SIZE ) INT_MAX )
         {
            uiToWrite = INT_MAX;
            nCount -= ( HB_SIZE ) uiToWrite;
         }
         else
         {
            uiToWrite = ( unsigned int ) nCount;
            nCount = 0;
         }

         HB_FAILURE_RETRY( lWritten, write( hFileHandle,
                                            ( const HB_UCHAR * ) pBuff + nWritten,
                                            uiToWrite ) );

         if( lWritten <= 0 )
            break;

         nWritten += lWritten;

         if( lWritten != ( long ) uiToWrite )
            break;
      }
#  else
      long lWritten;
      HB_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, nCount ) );
      nWritten = lWritten == -1 ? 0 : lWritten;
#  endif
   }
   else
   {
      int iResult;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) );
#  else
      HB_FAILURE_RETRY( iResult, ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) );
#  endif
   }
#endif

   hb_vmLock();

   return nWritten;
}

HB_SIZE hb_fsReadAt( HB_FHANDLE hFileHandle, void * pBuff, HB_SIZE nCount, HB_FOFFSET nOffset )
{
   HB_SIZE nRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, nCount, nOffset));

   hb_vmUnlock();

#if defined( HB_OS_UNIX ) && !defined( __WATCOMC__ ) && !defined( HB_OS_VXWORKS ) && !defined( HB_OS_SYMBIAN )
   {
      long lRead;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( lRead, pread64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      HB_FAILURE_RETRY( lRead, pread( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nRead = lRead == -1 ? 0 : lRead;
   }
#else
   nRead = 0;
#  if defined( HB_OS_WIN )
#     if defined( HB_WIN_IOREAD_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      while( nCount )
      {
         DWORD dwToRead;
         DWORD dwRead;

         if( nCount > ( HB_SIZE ) HB_WIN_IOREAD_LIMIT )
         {
            dwToRead = HB_WIN_IOREAD_LIMIT;
            nCount -= ( HB_SIZE ) dwToRead;
         }
         else
         {
            dwToRead = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = ReadFile( hWFileHandle, ( HB_UCHAR * ) pBuff + nRead,
                             dwToRead, &dwRead, &Overlapped );

         if( ! bResult )
            break;

         nRead += ( HB_SIZE ) dwRead;

         if( dwRead != dwToRead )
            break;
      }
      hb_fsSetIOError( bResult != 0, 0 );
   }
#     else
   if( hb_iswinnt() )
   {
      DWORD dwRead = 0;
      OVERLAPPED Overlapped;
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );
      hb_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                 pBuff, ( DWORD ) nCount, &dwRead, &Overlapped ) != 0, 0 );
      nRead = dwRead;
   }
   else
   {
      HB_FOFFSET nPos;
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
      if( nPos == ( HB_FOFFSET ) INVALID_SET_FILE_POINTER )
         hb_fsSetIOError( HB_FALSE, 0 );
      else
      {
         DWORD dwRead = 0;
         hb_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                    pBuff, ( DWORD ) nCount, &dwRead, NULL ) != 0, 0 );
         nRead = dwRead;
      }
   }
#     endif

   /* TOFIX: this is not atom operation. It has to be fixed for RDD
    *        file access with shared file handles in aliased work areas
    */

#  elif defined( HB_FS_IO_16BIT )
   if( hb_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nRead = hb_fsReadLarge( hFileHandle, pBuff, nCount );
#  else
   {
      HB_FOFFSET nPos;
#     if defined( HB_USE_LARGEFILE64 )
      nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#     elif defined( HB_OS_OS2 )
      ULONG ulPos;
      if( DosSetFilePtr( hFileHandle, nOffset, SEEK_SET, &ulPos ) == 0 )
         nPos = ( HB_FOFFSET ) ulPos;
      else
         nPos = ( HB_FOFFSET ) -1;
#     else
         nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#     endif
      if( nPos == ( HB_FOFFSET ) -1 )
         hb_fsSetIOError( HB_FALSE, 0 );
      else
      {
         long lRead;
         HB_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, nCount ) );
         nRead = lRead == -1 ? 0 : lRead;
      }
   }
#  endif
#endif

   hb_vmLock();

   return nRead;
}

HB_SIZE hb_fsWriteAt( HB_FHANDLE hFileHandle, const void * pBuff, HB_SIZE nCount, HB_FOFFSET nOffset )
{
   HB_SIZE nWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, nCount, nOffset));

   hb_vmUnlock();

#if defined( HB_OS_UNIX ) && !defined( __WATCOMC__ ) && !defined( HB_OS_VXWORKS ) && !defined( HB_OS_SYMBIAN )
   {
      long lWritten;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( lWritten, pwrite64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      HB_FAILURE_RETRY( lWritten, pwrite( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nWritten = lWritten == -1 ? 0 : lWritten;
   }
#else
   nWritten = 0;
#  if defined( HB_OS_WIN )
#     if defined( HB_WIN_IOWRITE_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      while( nCount )
      {
         DWORD dwToWrite;
         DWORD dwWritten;

         if( nCount > ( HB_SIZE ) HB_WIN_IOWRITE_LIMIT )
         {
            dwToWrite = HB_WIN_IOWRITE_LIMIT;
            nCount -= ( HB_SIZE ) dwToWrite;
         }
         else
         {
            dwToWrite = ( DWORD ) nCount;
            nCount = 0;
         }

         bResult = WriteFile( hWFileHandle, ( HB_UCHAR * ) pBuff + nWritten,
                              dwToWrite, &dwWritten, &Overlapped );

         if( ! bResult )
            break;

         nWritten += ( HB_SIZE ) dwWritten;

         if( dwWritten != dwToWrite )
            break;
      }
      hb_fsSetIOError( bResult != 0, 0 );
   }
#     else
   if( hb_iswinnt() )
   {
      DWORD dwWritten = 0;
      OVERLAPPED Overlapped;
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );
      hb_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                  pBuff, ( DWORD ) nCount, &dwWritten, &Overlapped ) != 0, 0 );
      nWritten = dwWritten;
   }
   else
   {
      HB_FOFFSET nPos;
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
      if( nPos == ( HB_FOFFSET ) INVALID_SET_FILE_POINTER )
         hb_fsSetIOError( HB_FALSE, 0 );
      else
      {
         DWORD dwWritten = 0;
         hb_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                     pBuff, ( DWORD ) nCount, &dwWritten, NULL ) != 0, 0 );
         nWritten = dwWritten;
      }
   }
#     endif

   /* TOFIX: this is not atom operation. It has to be fixed for RDD
    *        file access with shared file handles in aliased work areas
    */

#  elif defined( HB_FS_IO_16BIT )
   if( hb_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nWritten = hb_fsWriteLarge( hFileHandle, pBuff, nCount );
#  else
   {
      HB_FOFFSET nPos;
#     if defined( HB_USE_LARGEFILE64 )
      nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#     elif defined( HB_OS_OS2 )
      ULONG ulPos;
      if( DosSetFilePtr( hFileHandle, nOffset, SEEK_SET, &ulPos ) == 0 )
         nPos = ( HB_FOFFSET ) ulPos;
      else
         nPos = ( HB_FOFFSET ) -1;
#     else
      nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#     endif
      if( nPos == ( HB_FOFFSET ) -1 )
         hb_fsSetIOError( HB_FALSE, 0 );
      else
      {
         long lWritten;
         HB_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, nCount ) );
         nWritten = lWritten == -1 ? 0 : lWritten;
      }
   }
#  endif
#endif

   hb_vmLock();

   return nWritten;
}

HB_BOOL hb_fsTruncAt( HB_FHANDLE hFileHandle, HB_FOFFSET nOffset )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsTruncAt(%p, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, nOffset));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );

      /* This is not atom operation anyhow if someone want to truncate
       * file then he has to made necessary synchronizations in upper level
       * code. We have such situation in our RDD drivers and for us such
       * version is enough. [druzus]
       */
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    ( DWORD ) SEEK_SET );
      if( ( ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow ) == nOffset )
         fResult = SetEndOfFile( DosToWinHandle( hFileHandle ) ) != 0;
      else
         fResult = HB_FALSE;

      hb_fsSetIOError( fResult, 0 );
   }
#else
   {
      int iResult;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( iResult, ftruncate64( hFileHandle, nOffset ) );
#  else
      HB_FAILURE_RETRY( iResult, ftruncate( hFileHandle, nOffset ) );
#  endif
      fResult = iResult != -1;
   }
#endif

   hb_vmLock();

   return fResult;
}

void hb_fsCommit( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   hb_fsSetIOError( FlushFileBuffers( DosToWinHandle( hFileHandle ) ) != 0, 0 );

#elif defined( HB_OS_OS2 )

   hb_fsSetIOError( DosResetBuffer( hFileHandle ) == 0, 0 );

#elif defined( HB_OS_UNIX )
{
   int iResult;
   /* We should check here only for _POSIX_SYNCHRONIZED_IO defined
    * and it should be enough to test if fdatasync() declaration
    * exists in <unistd.h>. Unfortunately on some OS-es like Darwin
    * _POSIX_SYNCHRONIZED_IO is defined but fdatasync() does not exists.
    * As workaround we are using this trick to check non zero version
    * number but on some systems it may disable using fdatasync() [druzus]
    */
#  if defined( _POSIX_SYNCHRONIZED_IO ) && _POSIX_SYNCHRONIZED_IO - 0 > 0
      /* faster - flushes data buffers only, without updating directory info
       */
      HB_FAILURE_RETRY( iResult, fdatasync( hFileHandle ) );
#  else
      /* slower - flushes all file data buffers and i-node info
       */
      HB_FAILURE_RETRY( iResult, fsync( hFileHandle ) );
#  endif
}
#elif defined( __WATCOMC__ )

   hb_fsSetIOError( fsync( hFileHandle ) == 0, 0 );

#else

   /* NOTE: close() functions releases all locks regardles if it is an
    * original or duplicated file handle
    */
   /* This hack is very dangerous. POSIX standard define that if _ANY_
    * file handle is closed all locks set by the process on the file
    * pointed by this descriptor are removed. It doesn't matter they
    * were done using different descriptor. It means that we now clean
    * all locks on hFileHandle with the code below if the OS is POSIX
    * compilant. I vote to disable it. [druzus]
    */
   {
      int dup_handle;
      HB_BOOL fResult = HB_FALSE;

      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
      {
         close( dup_handle );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
   }

#endif

   hb_vmLock();
}

HB_BOOL hb_fsLock( HB_FHANDLE hFileHandle, HB_ULONG ulStart,
                   HB_ULONG ulLength, HB_USHORT uiMode )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, ulStart, ulLength, uiMode));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
      {
         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            DWORD dwFlags;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            dwFlags = ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK;
            if( !s_fUseWaitLocks || !( uiMode & FLX_WAIT ) )
            {
               dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
            }
            fResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0, ulLength, 0, &sOlap ) != 0;
         }
         else
         {
            fResult = LockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength, 0 ) != 0;
         }
         break;
      }
      case FL_UNLOCK:
      {
         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            fResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0, ulLength,0, &sOlap ) != 0;
         }
         else
         {
            fResult = UnlockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength, 0 ) != 0;
         }
         break;

      }
      default:
         fResult = HB_FALSE;
   }
   hb_fsSetIOError( fResult, 0 );
#elif defined( HB_OS_OS2 )
   {
      struct _FILELOCK fl, ful;

      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            fl.lOffset = ulStart;
            fl.lRange = ulLength;
            ful.lOffset = 0;
            ful.lRange = 0;

            /* lock region, 2 seconds timeout, exclusive access - no atomic */
            fResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
            break;

         case FL_UNLOCK:

            fl.lOffset = 0;
            fl.lRange = 0;
            ful.lOffset = ulStart;
            ful.lRange = ulLength;

            /* unlock region, 2 seconds timeout, exclusive access - no atomic */
            fResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
#elif defined( _MSC_VER ) || defined( __DMC__ )
   {
      HB_ULONG ulOldPos;

      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            fResult = ( locking( hFileHandle, _LK_NBLCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            fResult = ( locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined( __MINGW32__ )
   {
      HB_ULONG ulOldPos;

      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            fResult = ( _locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            fResult = ( _locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined( HB_OS_UNIX )
   {
      struct flock lock_info;
      int iResult;

      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            HB_FAILURE_RETRY( iResult, fcntl( hFileHandle,
                               ( uiMode & FLX_WAIT ) ? F_SETLKW: F_SETLK,
                               &lock_info ) );
            fResult = iResult != -1;
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = getpid();

            HB_FAILURE_RETRY( iResult, fcntl( hFileHandle, F_SETLK, &lock_info ) );
            fResult = iResult != -1;
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
#else

   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
         fResult = ( lock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      case FL_UNLOCK:
         fResult = ( unlock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      default:
         fResult = HB_FALSE;
   }
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fsLockLarge( HB_FHANDLE hFileHandle, HB_FOFFSET nStart,
                        HB_FOFFSET nLength, HB_USHORT uiMode )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLockLarge(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, nStart, nLength, uiMode));

#if defined( HB_OS_WIN )
   {
      DWORD dwOffsetLo = ( DWORD ) ( nStart & 0xFFFFFFFF ),
            dwOffsetHi = ( DWORD ) ( nStart >> 32 ),
            dwLengthLo = ( DWORD ) ( nLength & 0xFFFFFFFF ),
            dwLengthHi = ( DWORD ) ( nLength >> 32 );

      hb_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            if( hb_iswinnt() )
            {
               OVERLAPPED sOlap;
               DWORD dwFlags;

               dwFlags = ( ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK );
               if( !s_fUseWaitLocks || !( uiMode & FLX_WAIT ) )
               {
                  dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
               }

               memset( &sOlap, 0, sizeof( sOlap ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               fResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0,
                                     dwLengthLo, dwLengthHi, &sOlap ) != 0;
            }
            else
            {
               fResult = LockFile( DosToWinHandle( hFileHandle ),
                                   dwOffsetLo, dwOffsetHi,
                                   dwLengthLo, dwLengthHi ) != 0;
            }
            break;

         case FL_UNLOCK:
            if( hb_iswinnt() )
            {
               OVERLAPPED sOlap;

               memset( &sOlap, 0, sizeof( sOlap ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               fResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0,
                                       dwLengthLo, dwLengthHi, &sOlap ) != 0;
            }
            else
            {
               fResult = UnlockFile( DosToWinHandle( hFileHandle ),
                                     dwOffsetLo, dwOffsetHi,
                                     dwLengthLo, dwLengthHi ) != 0;
            }
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#elif defined( HB_USE_LARGEFILE64 )
   {
      struct flock64 lock_info;
      int iResult;

      hb_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = nStart;
            lock_info.l_len    = nLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            HB_FAILURE_RETRY( iResult, fcntl( hFileHandle,
                               ( uiMode & FLX_WAIT ) ? F_SETLKW64: F_SETLK64,
                               &lock_info ) );
            fResult = iResult != -1;
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = nStart;
            lock_info.l_len    = nLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = getpid();

            HB_FAILURE_RETRY( iResult, fcntl( hFileHandle, F_SETLK64, &lock_info ) );
            fResult = iResult != -1;
            break;

         default:
            fResult = HB_FALSE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#else
   fResult = hb_fsLock( hFileHandle, ( HB_SIZE ) nStart, ( HB_SIZE ) nLength, uiMode );
#endif

   return fResult;
}

HB_ULONG hb_fsSeek( HB_FHANDLE hFileHandle, HB_LONG lOffset, HB_USHORT uiFlags )
{
   HB_ULONG ulPos;
   HB_USHORT nFlags;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeek(%p, %ld, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, lOffset, uiFlags));

   nFlags = convert_seek_flags( uiFlags );

   hb_vmUnlock();
#if defined( HB_OS_OS2 )
   {
      APIRET ret;

      /* This DOS hack creates 2GB file size limit, Druzus */
      if( lOffset < 0 && nFlags == SEEK_SET )
      {
         ret = 1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ret = DosSetFilePtr( hFileHandle, lOffset, nFlags, &ulPos );
         /* TODO: what we should do with this error code? Is it DOS compatible? */
         hb_fsSetError( ( HB_ERRCODE ) ret );
      }

      if( ret != 0 )
      {
         /* FIXME: it should work if DosSetFilePtr is lseek compatible
            but maybe OS2 has DosGetFilePtr too, if not then remove this
            comment, Druzus */
         if( DosSetFilePtr( hFileHandle, 0, SEEK_CUR, &ulPos ) != 0 )
            ulPos = 0;
      }
   }
#elif defined( HB_OS_WIN )
   /* This DOS hack creates 2GB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( HB_ULONG ) INVALID_SET_FILE_POINTER;
      hb_fsSetError( 25 ); /* 'Seek Error' */
   }
   else
   {
      ulPos = ( ULONG ) SetFilePointer( DosToWinHandle( hFileHandle ), lOffset, NULL, ( DWORD ) nFlags );
      hb_fsSetIOError( ulPos != ( ULONG ) INVALID_SET_FILE_POINTER, 0 );
   }

   if( ulPos == ( ULONG ) INVALID_SET_FILE_POINTER )
   {
      ulPos = ( ULONG ) SetFilePointer( DosToWinHandle( hFileHandle ), 0, NULL, SEEK_CUR );
      if( ulPos == ( ULONG ) INVALID_SET_FILE_POINTER )
         ulPos = 0;
   }

#else
   /* This DOS hack creates 2GB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( HB_ULONG ) -1;
      hb_fsSetError( 25 ); /* 'Seek Error' */
   }
   else
   {
      ulPos = lseek( hFileHandle, lOffset, nFlags );
      hb_fsSetIOError( ulPos != ( HB_ULONG ) -1, 0 );
   }

   if( ulPos == ( HB_ULONG ) -1 )
   {
      ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
      if( ulPos == ( HB_ULONG ) -1 )
         ulPos = 0;
   }
#endif
   hb_vmLock();

   return ulPos;
}

HB_FOFFSET hb_fsSeekLarge( HB_FHANDLE hFileHandle, HB_FOFFSET nOffset, HB_USHORT uiFlags )
{
   HB_FOFFSET nPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeekLarge(%p, %" PFHL "i, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, nOffset, uiFlags));

#if defined( HB_OS_WIN )
   {
      HB_USHORT nFlags = convert_seek_flags( uiFlags );

      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );

      hb_vmUnlock();
      if( nOffset < 0 && nFlags == SEEK_SET )
      {
         nPos = ( HB_FOFFSET ) INVALID_SET_FILE_POINTER;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                       ( DWORD ) nFlags );
         nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
         hb_fsSetIOError( nPos != ( HB_FOFFSET ) INVALID_SET_FILE_POINTER, 0 );
      }

      if( nPos == ( HB_FOFFSET ) INVALID_SET_FILE_POINTER )
      {
         ulOffsetHigh = 0;
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       0, ( PLONG ) &ulOffsetHigh, SEEK_CUR );
         nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
         if( nPos == ( ULONG ) INVALID_SET_FILE_POINTER )
            nPos = 0;
      }
      hb_vmLock();
   }
#elif defined( HB_USE_LARGEFILE64 )
   {
      HB_USHORT nFlags = convert_seek_flags( uiFlags );

      hb_vmUnlock();
      if( nOffset < 0 && nFlags == SEEK_SET )
      {
         nPos = ( HB_FOFFSET ) -1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         nPos = lseek64( hFileHandle, nOffset, nFlags );
         hb_fsSetIOError( nPos != ( HB_FOFFSET ) -1, 0 );
      }

      if( nPos == ( HB_FOFFSET ) -1 )
      {
         nPos = lseek64( hFileHandle, 0L, SEEK_CUR );
         if( nPos == ( HB_FOFFSET ) -1 )
            nPos = 0;
      }
      hb_vmLock();
   }
#else
   nPos = ( HB_FOFFSET ) hb_fsSeek( hFileHandle, ( HB_ISIZ ) nOffset, uiFlags );
#endif

   return nPos;
}

HB_FOFFSET hb_fsTell( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsTell(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

   return hb_fsSeekLarge( hFileHandle, 0, FS_RELATIVE );
}

HB_BOOL hb_fsDelete( const char * pFilename )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", pFilename));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpFilename = HB_TCHAR_CONVTO( pFilename );

      fResult = DeleteFile( lpFilename ) != 0;
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpFilename );
   }
#else

   fResult = ( remove( pFilename ) == 0 );
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

HB_BOOL hb_fsRename( const char * pOldName, const char * pNewName )
{
   HB_BOOL fResult;
   char * pszFreeOld, * pszFreeNew;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", pOldName, pNewName));

   pOldName = hb_fsNameConv( pOldName, &pszFreeOld );
   pNewName = hb_fsNameConv( pNewName, &pszFreeNew );

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   {
      LPTSTR lpOldName = HB_TCHAR_CONVTO( pOldName );
      LPTSTR lpNewName = HB_TCHAR_CONVTO( pNewName );

      fResult = MoveFile( lpOldName, lpNewName ) != 0;
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpOldName );
      HB_TCHAR_FREE( lpNewName );
   }

#else

   fResult = ( rename( pOldName, pNewName ) == 0 );
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   if( pszFreeOld )
      hb_xfree( pszFreeOld );
   if( pszFreeNew )
      hb_xfree( pszFreeNew );

   return fResult;
}

HB_BOOL hb_fsMkDir( const char * pDirname )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", pDirname));

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpDirname = HB_TCHAR_CONVTO( pDirname );

      fResult = CreateDirectory( lpDirname, NULL ) != 0;
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpDirname );
   }
#else

#  if ! defined( HB_OS_UNIX ) && \
      ( defined( __WATCOMC__ ) || defined( __BORLANDC__ ) || \
        defined( __IBMCPP__ ) || defined( __MINGW32__ ) )
   fResult = ( mkdir( pDirname ) == 0 );
#  else
   fResult = ( mkdir( pDirname, S_IRWXU | S_IRWXG | S_IRWXO ) == 0 );
#  endif
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

HB_BOOL hb_fsChDir( const char * pDirname )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpDirname = HB_TCHAR_CONVTO( pDirname );
      UINT uiErrMode;

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( lpDirname ) != FALSE;
      SetErrorMode( uiErrMode );
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpDirname );
   }
#else
   fResult = ( chdir( pDirname ) == 0 );
   hb_fsSetIOError( fResult, 0 );
#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

HB_BOOL hb_fsRmDir( const char * pDirname )
{
   HB_BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPTSTR lpDirname = HB_TCHAR_CONVTO( pDirname );

      fResult = RemoveDirectory( lpDirname ) != 0;
      hb_fsSetIOError( fResult, 0 );

      HB_TCHAR_FREE( lpDirname );
   }
#else
   fResult = ( rmdir( pDirname ) == 0 );
   hb_fsSetIOError( fResult, 0 );
#endif

   hb_vmLock();

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

const char * hb_fsCurDir( int iDrive )
{
   char * pszDirBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDir(%d)", iDrive));

   pszDirBuffer = hb_stackDirBuffer();
   hb_fsCurDirBuff( iDrive, pszDirBuffer, HB_PATH_MAX );

   return pszDirBuffer;
}

/* NOTE: Thread safe version of hb_fsCurDir() */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

HB_ERRCODE hb_fsCurDirBuff( int iDrive, char * pszBuffer, HB_SIZE nSize )
{
   int iCurDrv = iDrive;
   HB_ERRCODE nResult;
   char * pszStart;
   HB_SIZE nLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%d)", iDrive));

   pszBuffer[ 0 ] = '\0';

   /*
    * do not cover this code by HB_OS_HAS_DRIVE_LETTER macro
    * It will allow us to add drive emulation in hb_fsCurDrv()/hb_fsChDrv()
    * and hb_fsNameConv()
    */
#if defined( HB_OS_WIN ) || !( defined( HB_OS_OS2 ) || defined( __MINGW32__ ) )
   if( iDrive > 0 )
   {
      iCurDrv = hb_fsCurDrv() + 1;
      if( iDrive != iCurDrv )
         hb_fsChDrv( iDrive - 1 );
   }
#endif

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      DWORD dwSize = ( DWORD ) nSize;
#if defined( UNICODE )
      LPTSTR lpBuffer = ( LPTSTR ) hb_xgrab( dwSize * sizeof( TCHAR ) );
      hb_fsSetIOError( ( GetCurrentDirectory( dwSize, lpBuffer ) != 0 ), 0 );
      lpBuffer[ dwSize - 1 ] = L'\0';
      hb_wcntombcpy( pszBuffer, lpBuffer, dwSize - 1 );
      hb_xfree( lpBuffer );
#else
      hb_fsSetIOError( ( GetCurrentDirectory( dwSize, pszBuffer ) != 0 ), 0 );
#endif
   }
#elif defined( HB_OS_OS2 )

   if( iDrive >= 0 )
   {
      ULONG nLen = ( ULONG ) nSize;

      hb_fsSetIOError( DosQueryCurrentDir( iDrive, ( PBYTE ) pszBuffer,
                                           &nLen ) == NO_ERROR, 0 );
   }
   else
      hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#elif defined( __MINGW32__ )

   if( iDrive >= 0 )
      hb_fsSetIOError( ( _getdcwd( iDrive, pszBuffer, nSize ) != NULL ), 0 );
   else
      hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#else

   hb_fsSetIOError( ( getcwd( pszBuffer, nSize ) != NULL ), 0 );

#endif

   hb_vmLock();

   nResult = hb_fsError();

   if( iDrive != iCurDrv )
   {
      hb_fsChDrv( iCurDrv - 1 );
      hb_fsSetError( nResult );
   }

   pszBuffer[ nSize - 1 ] = '\0';

   if( nResult == 0 && pszBuffer[ 0 ] )
   {
      /* Strip the leading drive spec, and leading backslash if there's one. */
      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

#if defined( __DJGPP__ ) || defined( HB_OS_OS2 )
      /* convert '/' to '\' */
      while( ( pszStart = strchr( pszBuffer, '/' ) ) != NULL )
         *pszStart = '\\';
#endif

      pszStart = pszBuffer;
      nLen = strlen( pszBuffer );

#if defined( HB_OS_HAS_DRIVE_LETTER )
      if( pszStart[ 1 ] == HB_OS_DRIVE_DELIM_CHR )
      {
         pszStart += 2;
         nLen -= 2;
      }
#endif
      if( strchr( HB_OS_PATH_DELIM_CHR_LIST, ( HB_UCHAR ) pszStart[ 0 ] ) )
      {
         pszStart++;
         nLen--;
      }

      /* Strip the trailing (back)slash if there's one */
      if( nLen && strchr( HB_OS_PATH_DELIM_CHR_LIST, ( HB_UCHAR ) pszStart[ nLen - 1 ] ) )
         nLen--;

      if( nLen && pszBuffer != pszStart )
         memmove( pszBuffer, pszStart, nLen );

      pszBuffer[ nLen ] = '\0';

      /* Convert from OS codepage */
      {
         char * pszFree = NULL;
         const char * pszResult;

         nLen = nSize;
         pszResult = hb_osDecodeCP( pszBuffer, &pszFree, &nLen );

         if( pszResult != pszBuffer )
            hb_strncpy( pszBuffer, pszResult, nSize - 1 );
         if( pszFree )
            hb_xfree( pszFree );
      }
   }

   return nResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

HB_ERRCODE hb_fsChDrv( int iDrive )
{
   HB_ERRCODE nResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", iDrive));

#if defined( HB_OS_HAS_DRIVE_LETTER )
   {
      int iSave, iNewDrive;

      hb_vmUnlock();

      HB_FS_GETDRIVE( iSave );
      HB_FS_SETDRIVE( iDrive );
      HB_FS_GETDRIVE( iNewDrive );

      if( iDrive == iNewDrive )
      {
         nResult = 0;
         hb_fsSetError( 0 );
      }
      else
      {
         HB_FS_SETDRIVE( iSave );

         nResult = ( HB_ERRCODE ) FS_ERROR;
         hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
      }
      hb_vmLock();
   }
#else

   HB_SYMBOL_UNUSED( iDrive );
   nResult = ( HB_ERRCODE ) FS_ERROR;
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#endif

   return nResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

int hb_fsCurDrv( void )
{
   int iDrive;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));

#if defined( HB_OS_HAS_DRIVE_LETTER )

   hb_vmUnlock();
   HB_FS_GETDRIVE( iDrive );
   hb_fsSetError( 0 );
   hb_vmLock();

#else

   iDrive = 0;
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#endif

   return iDrive; /* Return the drive number, base 0. */
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

HB_ERRCODE hb_fsIsDrv( int iDrive )
{
   HB_ERRCODE nResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", iDrive));

   if( iDrive >= 0 )
#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )
   {
      hb_vmUnlock();
      nResult = ( ( GetLogicalDrives() >> iDrive ) & 1 ) ? 0 : ( HB_ERRCODE ) F_ERROR;
      hb_vmLock();
      hb_fsSetError( 0 );
   }
#elif defined( HB_OS_OS2 )
   {
      ULONG ulDrive, ulLogical;

      DosQueryCurrentDisk( &ulDrive, &ulLogical );
      nResult = ( ( ulLogical >> iDrive ) & 1 ) ? 0 : ( HB_ERRCODE ) F_ERROR;
   }
#elif defined( HB_OS_HAS_DRIVE_LETTER )
   {
      int iSave, iNewDrive;

      hb_vmUnlock();

      HB_FS_GETDRIVE( iSave );
      HB_FS_SETDRIVE( iDrive );
      HB_FS_GETDRIVE( iNewDrive );
      nResult = ( iDrive == iNewDrive ) ? 0 : ( HB_ERRCODE ) FS_ERROR;
      HB_FS_SETDRIVE( iSave );
      hb_fsSetError( 0 );

      hb_vmLock();
   }
#else
   {
      HB_SYMBOL_UNUSED( iDrive );
      nResult = ( HB_ERRCODE ) FS_ERROR;
      hb_fsSetError( 0 );
   }
#endif
   else
   {
      nResult = ( HB_ERRCODE ) FS_ERROR;
      hb_fsSetError( 0 );
   }

   return nResult;
}

HB_BOOL hb_fsIsDevice( HB_FHANDLE hFileHandle )
{
   HB_BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDevice(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   fResult = GetFileType( DosToWinHandle( hFileHandle ) ) == FILE_TYPE_CHAR;
   hb_fsSetIOError( fResult, 0 );

#else

#if defined( _MSC_VER ) || defined( __MINGW32__ )
   fResult = _isatty( hFileHandle ) != 0;
#else
   fResult = isatty( hFileHandle ) != 0;
#endif
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   return fResult;
}

/* convert file name for hb_fsExtOpen
 * caller must free the returned buffer
 */
char * hb_fsExtName( const char * pFilename, const char * pDefExt,
                     HB_USHORT uiExFlags, const char * pPaths )
{
   HB_PATHNAMES * pNextPath;
   PHB_FNAME pFilepath;
   HB_BOOL fIsFile = HB_FALSE;
   char * szPath;

   szPath = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pFilename );

   if( pDefExt && ( ( uiExFlags & FXO_FORCEEXT ) || !pFilepath->szExtension ) )
      pFilepath->szExtension = pDefExt;

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( szPath, pFilepath );
   }
   else if( uiExFlags & FXO_DEFAULTS )
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( szPath, pFilepath );
         fIsFile = hb_fsFileExists( szPath );
      }
      if( !fIsFile &&
          ( uiExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
          hb_setGetPath() )
      {
         pNextPath = hb_setGetFirstSetPath();
         while( !fIsFile && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            hb_fsFNameMerge( szPath, pFilepath );
            fIsFile = hb_fsFileExists( szPath );
            pNextPath = pNextPath->pNext;
         }
      }
      if( !fIsFile )
      {
         pFilepath->szPath = szDefault ? szDefault : NULL;
         hb_fsFNameMerge( szPath, pFilepath );
      }
   }
   else if( pPaths && *pPaths )
   {
      HB_PATHNAMES * pSearchPath = NULL;
      hb_fsAddSearchPath( pPaths, &pSearchPath );
      pNextPath = pSearchPath;
      while( !fIsFile && pNextPath )
      {
         pFilepath->szPath = pNextPath->szPath;
         hb_fsFNameMerge( szPath, pFilepath );
         fIsFile = hb_fsFileExists( szPath );
         pNextPath = pNextPath->pNext;
      }
      hb_fsFreeSearchPath( pSearchPath );
      if( !fIsFile )
      {
         pFilepath->szPath = NULL;
         hb_fsFNameMerge( szPath, pFilepath );
      }
   }
   else
      hb_fsFNameMerge( szPath, pFilepath );

   hb_xfree( pFilepath );

   return szPath;
}

HB_FHANDLE hb_fsExtOpen( const char * pFilename, const char * pDefExt,
                         HB_USHORT uiExFlags, const char * pPaths,
                         PHB_ITEM pError )
{
   HB_FHANDLE hFile;
   HB_USHORT uiFlags;
   char * szPath;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsExtOpen(%s, %s, %hu, %p, %p)", pFilename, pDefExt, uiExFlags, pPaths, pError));

#if 0
   #define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
   #define FXO_APPEND    0x0200   /* Create (append if exists) */
   #define FXO_UNIQUE    0x0400   /* Create unique file FO_EXCL ??? */
   #define FXO_FORCEEXT  0x0800   /* Force default extension */
   #define FXO_DEFAULTS  0x1000   /* Use SET command defaults */
   #define FXO_DEVICERAW 0x2000   /* Open devices in raw mode */
   /* Harbour extension */
   #define FXO_SHARELOCK 0x4000   /* emulate DOS SH_DENY* mode in POSIX OS */
   #define FXO_COPYNAME  0x8000   /* copy final szPath into pFilename */

   hb_errGetFileName( pError );
#endif

   szPath = hb_fsExtName( pFilename, pDefExt, uiExFlags, pPaths );

   uiFlags = uiExFlags & 0xff;
   if( uiExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) )
   {
      uiFlags |= FO_CREAT;
      if( uiExFlags & FXO_UNIQUE )
         uiFlags |= FO_EXCL;
#if !defined( HB_USE_SHARELOCKS )
      else if( uiExFlags & FXO_TRUNCATE )
         uiFlags |= FO_TRUNC;
#endif
   }

   hFile = hb_fsOpen( szPath, uiFlags );

#if defined( HB_USE_SHARELOCKS )
   if( hFile != FS_ERROR && uiExFlags & FXO_SHARELOCK )
   {
#if defined( HB_USE_BSDLOCKS )
      int iLock, iResult;
      if( ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ ||
          ( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0 )
         iLock = LOCK_SH | LOCK_NB;
      else
         iLock = LOCK_EX | LOCK_NB;
      hb_vmUnlock();
      HB_FAILURE_RETRY( iResult, flock( hFile, iLock ) );
      hb_vmLock();
      if( iResult != 0 )
#else
      HB_USHORT uiLock;
      if( ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ ||
          ( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0 )
         uiLock = FL_LOCK | FLX_SHARED;
      else
         uiLock = FL_LOCK | FLX_EXCLUSIVE;

      if( !hb_fsLockLarge( hFile, HB_SHARELOCK_POS, HB_SHARELOCK_SIZE, uiLock ) )
#endif
      {
         hb_fsClose( hFile );
         hFile = FS_ERROR;
         /*
          * fix for neterr() support and Clipper compatibility,
          * should be revised with a better multi platform solution.
          */
         hb_fsSetError( ( uiExFlags & FXO_TRUNCATE ) ? 5 : 32 );
      }
      else if( uiExFlags & FXO_TRUNCATE )
      {
         /* truncate the file only if properly locked */
         hb_fsSeek( hFile, 0, FS_SET );
         hb_fsTruncAt( hFile, 0 );
         if( hb_fsError() != 0 )
         {
            hb_fsClose( hFile );
            hFile = FS_ERROR;
            hb_fsSetError( 5 );
         }
      }
   }
#elif 1
   /*
    * Temporary fix for neterr() support and Clipper compatibility,
    * should be revised with a better solution.
    */
   if( ( uiExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
       hb_fsError() == 5 )
   {
      hb_fsSetError( 32 );
   }
#endif

   if( pError )
   {
      hb_errPutFileName( pError, szPath );
      if( hFile == FS_ERROR )
      {
         hb_errPutOsCode( pError, hb_fsError() );
         hb_errPutGenCode( pError, ( HB_ERRCODE ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   if( uiExFlags & FXO_COPYNAME && hFile != FS_ERROR )
      hb_strncpy( ( char * ) pFilename, szPath, HB_PATH_MAX - 1 );

   hb_xfree( szPath );
   return hFile;
}

HB_BOOL hb_fsEof( HB_FHANDLE hFileHandle )
{
   HB_BOOL fResult;

   hb_vmUnlock();

#if defined( __DJGPP__ ) || defined( __CYGWIN__ ) || \
    defined( HB_OS_WIN ) || defined( HB_OS_WIN_CE ) || \
    defined( HB_OS_UNIX )
{
   HB_FOFFSET curPos;
   HB_FOFFSET endPos;
   HB_FOFFSET newPos;

   curPos = hb_fsSeekLarge( hFileHandle, 0L, FS_RELATIVE );
   if( curPos != -1 )
   {
      endPos = hb_fsSeekLarge( hFileHandle, 0L, FS_END );
      newPos = hb_fsSeekLarge( hFileHandle, curPos, FS_SET );
      fResult = ( endPos != -1 && newPos == curPos );
   }
   else
   {
      endPos = -1;
      fResult = HB_FALSE;
   }
   hb_fsSetIOError( fResult, 0 );
   fResult = !fResult || curPos == endPos;
}
#else
   fResult = eof( hFileHandle ) != 0;
   hb_fsSetIOError( fResult, 0 );
#endif

   hb_vmLock();

   return fResult;
}

const char * hb_fsNameConv( const char * szFileName, char ** pszFree )
{
   int iFileCase, iDirCase;
   const char * pszCP;
   char cDirSep;
   HB_BOOL fTrim;

/*
   Convert file and dir case. The allowed SET options are:
      LOWER - Convert all caracters of file to lower
      UPPER - Convert all caracters of file to upper
      MIXED - Leave as is

   The allowed environment options are:
      FILECASE - define the case of file
      DIRCASE - define the case of path
      DIRSEPARATOR - define separator of path (Ex. "/")
      TRIMFILENAME - strip trailing and leading spaces (also from extension)
*/

   if( pszFree )
      *pszFree = NULL;

   if( !hb_stackId() )
      return szFileName;

   fTrim = hb_setGetTrimFileName();
   cDirSep = ( char ) hb_setGetDirSeparator();
   iFileCase = hb_setGetFileCase();
   iDirCase = hb_setGetDirCase();
   pszCP = hb_setGetOSCODEPAGE();
   if( pszCP && *pszCP == 0 )
      pszCP = NULL;

   if( fTrim ||
       cDirSep != HB_OS_PATH_DELIM_CHR ||
       iFileCase != HB_SET_CASE_MIXED ||
       iDirCase != HB_SET_CASE_MIXED ||
       pszCP )
   {
      PHB_FNAME pFileName;
      HB_SIZE nLen;

      if( pszFree )
      {
         szFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                             szFileName, HB_PATH_MAX - 1 );
      }

      if( cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) szFileName;
         while( *p )
         {
            if( *p == cDirSep )
               *p = HB_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( szFileName );

      /* strip trailing and leading spaces */
      if( fTrim )
      {
         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            nLen = hb_strRTrimLen( pFileName->szName, nLen, HB_FALSE );
            pFileName->szName = hb_strLTrim( pFileName->szName, &nLen );
            ( ( char * ) pFileName->szName )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            nLen = hb_strRTrimLen( pFileName->szExtension, nLen, HB_FALSE );
            pFileName->szExtension = hb_strLTrim( pFileName->szExtension, &nLen );
            ( ( char * ) pFileName->szExtension )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( iFileCase == HB_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            hb_strLower( ( char * ) pFileName->szName, strlen( pFileName->szName ) );
         if( pFileName->szExtension )
            hb_strLower( ( char * ) pFileName->szExtension, strlen( pFileName->szExtension ) );
      }
      else if( iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            hb_strUpper( ( char * ) pFileName->szName, strlen( pFileName->szName ) );
         if( pFileName->szExtension )
            hb_strUpper( ( char * ) pFileName->szExtension, strlen( pFileName->szExtension ) );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( iDirCase == HB_SET_CASE_LOWER )
            hb_strLower( ( char * ) pFileName->szPath, strlen( pFileName->szPath ) );
         else if( iDirCase == HB_SET_CASE_UPPER )
            hb_strUpper( ( char * ) pFileName->szPath, strlen( pFileName->szPath ) );
      }

      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_xfree( pFileName );

      if( pszCP )
      {
         const char * pszPrev = szFileName;
         nLen = HB_PATH_MAX;
         szFileName = hb_osEncodeCP( szFileName, pszFree, &nLen );
         if( pszFree == NULL && szFileName != pszPrev )
         {
            hb_strncpy( ( char * ) pszPrev, szFileName, HB_PATH_MAX - 1 );
            hb_xfree( ( void * ) szFileName );
            szFileName = pszPrev;
         }
      }
   }

   return szFileName;
}

/* NOTE: pszBuffer must be HB_PATH_MAX long. */
void hb_fsBaseDirBuff( char * pszBuffer )
{
   const char * szBaseName = hb_cmdargARGVN( 0 );

   if( szBaseName )
   {
      PHB_FNAME pFName = hb_fsFNameSplit( szBaseName );
      const char * pszResult;
      char * pszFree = NULL;
      HB_SIZE nSize = HB_PATH_MAX;

      pFName->szName = NULL;
      pFName->szExtension = NULL;
      hb_fsFNameMerge( pszBuffer, pFName );
      hb_xfree( pFName );

      /* Convert from OS codepage */
      pszResult = hb_osDecodeCP( pszBuffer, &pszFree, &nSize );
      if( pszResult != pszBuffer )
         hb_strncpy( pszBuffer, pszResult, HB_PATH_MAX - 1 );
      if( pszFree )
         hb_xfree( pszFree );
   }
}

static HB_BOOL hb_fsDisableWaitLocks( int iSet )
{
   HB_BOOL fRetVal = s_fUseWaitLocks;

   if( iSet >= 0 )
      s_fUseWaitLocks = ( iSet == 0 );

   return fRetVal;
}

HB_FUNC( HB_DISABLEWAITLOCKS )
{
   hb_retl( hb_fsDisableWaitLocks( hb_parldef( 1, -1 ) ) );
}
