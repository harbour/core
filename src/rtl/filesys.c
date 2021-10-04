/*
 * The FileSys API (C level)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * Copyright 1999-2010 Viktor Szakats (vszakats.net/harbour)
 *    hb_fsSetError(), hb_fsSetDevMode(), hb_fsReadLarge(), hb_fsWriteLarge()
 *    hb_fsCurDirBuff(), hb_fsBaseDirBuff()
 *    fs_win_get_drive(), fs_win_set_drive()
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_fsChDrv(), hb_fsCurDrv(), hb_fsIsDrv(), hb_fsIsDevice()
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>, David G. Holm <dholm@jsd-llc.com>
 *    hb_fsEof()
 * Copyright 2001 Jose Gimenez (JFG) <jfgimenez@wanadoo.es>, <tecnico.sireinsa@ctv.es>
 *    Added platform check for any compiler to use the Windows
 *    API calls to allow opening an unlimited number of files
 *    simultaneously.
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET ) will
         set the file pointer to the passed negative value and the subsequent
         hb_fsWrite() call will fail. In CA-Cl*pper, _fsSeek() will fail,
         the pointer will not be moved and thus the _fsWrite() call will
         successfully write the buffer to the current file position. [vszakats]

   This has been corrected by ptucker
 */

/* *nixes */
#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif
#if ! defined( _GNU_SOURCE )
#  define _GNU_SOURCE
#endif

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapicdp.h"
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
   #if ! defined( HB_HAS_POLL ) && ! defined( HB_NO_POLL ) && \
         defined( _POSIX_C_SOURCE ) && _POSIX_C_SOURCE >= 200112L
      /* use poll() instead of select() to avoid FD_SETSIZE (1024 in Linux)
         file handle limit */
      #define HB_HAS_POLL
   #endif
   #if defined( HB_HAS_POLL )
      #include <poll.h>
   #endif
#endif
#if ! defined( HB_OS_WIN )
#  include <errno.h>
#endif

#if ( defined( __DMC__ ) || defined( __BORLANDC__ ) || \
      defined( __IBMCPP__ ) || defined( _MSC_VER ) || \
      defined( __MINGW32__ ) || defined( __WATCOMC__ ) ) && \
      ! defined( HB_OS_UNIX ) && ! defined( HB_OS_WIN_CE )
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <process.h>
   #if ! defined( __POCC__ ) && ! defined( __XCC__ )
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
      #if defined( __MINGW32__ ) && ! defined( _LK_UNLCK )
         #define _LK_UNLCK _LK_UNLOCK
      #endif
   #else
      #define ftruncate chsize
   #endif
   #if ! defined( HAVE_POSIX_IO )
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
   #if ! defined( HAVE_POSIX_IO )
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
   #define INCL_LONGLONG
   #include <os2.h>
   #include <time.h>
   #include <share.h>
   #ifndef SH_COMPAT
      #define SH_COMPAT  0x0000
   #endif
#elif defined( HB_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
   #if ! defined( INVALID_SET_FILE_POINTER ) && \
       ( defined( __DMC__ ) || defined( _MSC_VER ) || defined( __LCC__ ) )
      #define INVALID_SET_FILE_POINTER ( ( DWORD ) -1 )
   #endif
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES     ( ( DWORD ) -1 )
   #endif
   #if defined( HB_OS_WIN_64 )
      #if ! defined( HB_WIN_IOREAD_LIMIT )
         #define HB_WIN_IOREAD_LIMIT      HB_U32_MAX
      #endif
      #if ! defined( HB_WIN_IOWRITE_LIMIT )
         #define HB_WIN_IOWRITE_LIMIT     HB_U32_MAX
      #endif
   #endif
#endif
#if defined( HB_USE_SHARELOCKS ) && defined( HB_USE_BSDLOCKS )
   #include <sys/file.h>
#endif
#if defined( HB_OS_LINUX )
#  define HB_HAS_SELECT_TIMER
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * defined and effectively enables lseek64()/flock64()/ftruncate64()
       * functions on 32-bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE ) && ! defined( __WATCOMC__ )
      #define HB_USE_LARGEFILE64
   #endif
#endif

#if defined( HB_OS_HAS_DRIVE_LETTER )
/* 2004-08-27 - <maurilio.longo@libero.it>
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

#if ! defined( HB_OS_UNIX )
   #if ! defined( S_IREAD ) && defined( S_IRUSR )
      #define S_IREAD   S_IRUSR
   #endif
   #if ! defined( S_IWRITE ) && defined( S_IWUSR )
      #define S_IWRITE  S_IWUSR
   #endif
   #if ! defined( S_IEXEC ) && defined( S_IXUSR )
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
#elif defined( HB_OS_MINIX )
   /* hack for functions missing from the Minix C library */
   #define fdatasync          fsync
   #define ftruncate64        ftruncate
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
   while( ( ret ) == -1 && hb_fsOsError() == ( HB_ERRCODE ) EINTR && \
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
   TCHAR pBuffer[ HB_PATH_MAX ];
   LPTSTR lpBuffer = pBuffer;
   DWORD dwResult, dwSize;
   int iDrive = 0;

   dwSize = HB_SIZEOFARRAY( pBuffer );
   dwResult = GetCurrentDirectory( dwSize, lpBuffer );
   if( dwResult > dwSize )
   {
      dwSize = dwResult;
      lpBuffer = ( TCHAR * ) hb_xgrab( dwSize * sizeof( TCHAR ) );
      dwResult = GetCurrentDirectory( dwSize, lpBuffer );
   }
   hb_fsSetIOError( dwResult != 0, 0 );
   if( dwResult >= 2 && dwResult < dwSize &&
       lpBuffer[ 1 ] == HB_OS_DRIVE_DELIM_CHR )
   {
      iDrive = HB_TOUPPER( lpBuffer[ 0 ] );
      if( iDrive >= 'A' && iDrive <= 'Z' )
         iDrive -= 'A';
      else
         iDrive = 0;
   }
   if( lpBuffer != pBuffer )
      hb_xfree( lpBuffer );
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
   switch( fHandle )
   {
      case ( HB_FHANDLE ) FS_ERROR:
         return NULL;
      case ( HB_FHANDLE ) HB_STDIN_HANDLE:
         return GetStdHandle( STD_INPUT_HANDLE );
      case ( HB_FHANDLE ) HB_STDOUT_HANDLE:
         return GetStdHandle( STD_OUTPUT_HANDLE );
      case ( HB_FHANDLE ) HB_STDERR_HANDLE:
         return GetStdHandle( STD_ERROR_HANDLE );
   }
   return ( HANDLE ) fHandle;
}

static void convert_open_flags( HB_BOOL fCreate, HB_FATTR nAttr, HB_USHORT uiFlags,
                                DWORD * dwMode, DWORD * dwShare,
                                DWORD * dwCreat, DWORD * dwAttr )
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
   if( nAttr == FC_NORMAL )
      *dwAttr = FILE_ATTRIBUTE_NORMAL;
   else
   {
      *dwAttr = FILE_ATTRIBUTE_ARCHIVE;
      if( nAttr & FC_READONLY )
         *dwAttr |= FILE_ATTRIBUTE_READONLY;
      if( nAttr & FC_HIDDEN )
         *dwAttr |= FILE_ATTRIBUTE_HIDDEN;
      if( nAttr & FC_SYSTEM )
         *dwAttr |= FILE_ATTRIBUTE_SYSTEM;
   }
}

#elif defined( HB_OS_OS2 )
static void convert_open_flags( HB_BOOL fCreate, HB_FATTR nAttr, HB_USHORT uiFlags,
                                PULONG pulAttr, PULONG fsOpenFlags, PULONG fsOpenMode )
{
   /* DosOpen() parameters */

   if( fCreate )
   {
      *fsOpenFlags = OPEN_ACTION_CREATE_IF_NEW |
                    ( uiFlags & FO_EXCL ? OPEN_ACTION_FAIL_IF_EXISTS :
                                          OPEN_ACTION_REPLACE_IF_EXISTS );
      *fsOpenMode = OPEN_ACCESS_READWRITE;
   }
   else
   {
      if( uiFlags & FO_CREAT )
      {
         if( uiFlags & FO_EXCL )
            *fsOpenFlags = OPEN_ACTION_CREATE_IF_NEW | OPEN_ACTION_FAIL_IF_EXISTS;
         else if( uiFlags & FO_TRUNC )
            *fsOpenFlags = OPEN_ACTION_CREATE_IF_NEW | OPEN_ACTION_REPLACE_IF_EXISTS;
         else
            *fsOpenFlags = OPEN_ACTION_CREATE_IF_NEW | OPEN_ACTION_OPEN_IF_EXISTS;
      }
      else if( uiFlags & FO_TRUNC )
         *fsOpenFlags = OPEN_ACTION_FAIL_IF_NEW | OPEN_ACTION_REPLACE_IF_EXISTS;
      else
         *fsOpenFlags = OPEN_ACTION_FAIL_IF_NEW | OPEN_ACTION_OPEN_IF_EXISTS;

      switch( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
      {
         case FO_READWRITE:
            *fsOpenMode = OPEN_ACCESS_READWRITE;
            break;
         case FO_WRITE:
            *fsOpenMode = OPEN_ACCESS_WRITEONLY;
            break;
         case FO_READ:
         default:
            *fsOpenMode = OPEN_ACCESS_READONLY;
            break;
      }
   }

   /* shared flags */
   switch( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE | FO_DENYNONE ) )
   {
      case FO_DENYREAD:
         *fsOpenMode |= OPEN_SHARE_DENYREAD;
         break;
      case FO_DENYWRITE:
         *fsOpenMode |= OPEN_SHARE_DENYWRITE;
         break;
      case FO_EXCLUSIVE:
         *fsOpenMode |= OPEN_SHARE_DENYREADWRITE;
         break;
      default:
         *fsOpenMode |= OPEN_SHARE_DENYNONE;
         break;
   }

   /* inheritance flag, when set file handle is not inherited
      by a process created from a call to DosExecPgm [druzus] */
   if( uiFlags & 0x80 )
      *fsOpenMode |= OPEN_FLAGS_NOINHERIT;

   /* file attributes flags */
   if( nAttr == FC_NORMAL )
      *pulAttr = FILE_NORMAL;
   else
   {
      *pulAttr = FILE_ARCHIVED;
      if( nAttr & FC_READONLY )
         *pulAttr |= FILE_READONLY;
      if( nAttr & FC_HIDDEN )
         *pulAttr |= FILE_HIDDEN;
      if( nAttr & FC_SYSTEM )
         *pulAttr |= FILE_SYSTEM;
   }
}

#else

static void convert_open_flags( HB_BOOL fCreate, HB_FATTR nAttr, HB_USHORT uiFlags,
                                int * flags, unsigned * mode,
                                int * share, int * attr )
{
   HB_TRACE( HB_TR_DEBUG, ( "convert_open_flags(%d, %u, %hu, %p, %p, %p, %p)", fCreate, nAttr, uiFlags, ( void * ) flags, ( void * ) mode, ( void * ) share, ( void * ) attr ) );

   /* file access mode */
#if defined( HB_OS_UNIX )
   *mode = HB_FA_POSIX_ATTR( nAttr );
   if( *mode == 0 )
   {
      *mode = S_IRUSR | S_IRGRP | S_IROTH;
      if( ! ( nAttr & HB_FA_READONLY ) )
         *mode |= S_IWUSR | S_IWGRP | S_IWOTH;
      if( nAttr & HB_FA_SYSTEM )
         *mode |= S_IXUSR | S_IXGRP | S_IXOTH;
      if( nAttr & HB_FA_HIDDEN )
         *mode &= S_IRUSR | S_IWUSR | S_IXUSR;
   }
#else
   *mode = S_IREAD |
           ( ( nAttr & FC_READONLY ) ? 0 : S_IWRITE ) |
           ( ( nAttr & FC_SYSTEM ) ? S_IEXEC : 0 );
#endif

   /* dos file attributes */
#if defined( HB_OS_DOS )
   if( nAttr == FC_NORMAL )
      *attr = _A_NORMAL;
   else
   {
      *attr = _A_ARCH;
      if( nAttr & FC_READONLY )
         *attr |= _A_RDONLY;
      if( nAttr & FC_HIDDEN )
         *attr |= _A_HIDDEN;
      if( nAttr & FC_SYSTEM )
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
#elif ! defined( HB_OS_UNIX )
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

   HB_TRACE( HB_TR_INFO, ( "convert_open_flags: flags=0x%04x, mode=0x%04x, share=0x%04x, attr=0x%04x", *flags, *mode, *share, *attr ) );

}
#endif

static HB_USHORT convert_seek_flags( HB_USHORT uiFlags )
{
   /* by default FS_SET is set */
   HB_USHORT result_flags = SEEK_SET;

   HB_TRACE( HB_TR_DEBUG, ( "convert_seek_flags(%hu)", uiFlags ) );

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
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsGetOsHandle(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

#if defined( HB_OS_WIN )
   return ( HB_FHANDLE ) DosToWinHandle( hFileHandle );
#else
   return hFileHandle;
#endif
}

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
/* for POSIX systems only, hides low-level select()/poll() access,
   intentionally covered by HB_OS_UNIX macro to generate compile time
   error in code which tries to use it on other platforms */

static int hb_fsCanAccess( HB_FHANDLE hFile, HB_MAXINT nTimeOut, HB_BOOL fRead )
{
   int iResult;

   hb_vmUnlock();

#if defined( HB_HAS_POLL )
{
   HB_MAXUINT timer = hb_timerInit( nTimeOut );
   struct pollfd fds;
   short int events = fRead ? POLLIN : POLLOUT;

   fds.fd = hFile;
   fds.events = events;
   fds.revents = 0;

   for( ;; )
   {
      HB_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? ( int ) nTimeOut : 1000;

      iResult = poll( &fds, 1, tout );
      hb_fsSetIOError( iResult >= 0, 0 );
      if( iResult > 0 && ( fds.revents & events ) == 0 )
      {
         if( ( fds.revents & ( POLLHUP | POLLNVAL | POLLERR ) ) != 0 )
         {
            iResult = -1;
            break;
         }
         iResult = 0;
      }
      else if( iResult == -1 && hb_fsOsError() == ( HB_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = HB_FALSE;
      }

      if( iResult == 0 && ! fLast && hb_vmRequestQuery() == 0 &&
          ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 )
         continue;

      break;
   }
}
#elif ! defined( HB_OS_SYMBIAN ) /* ! HB_HAS_POLL */
{
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( nTimeOut );
#  endif

   for( ;; )
   {
      struct timeval tv;
      fd_set fds;

      if( nTimeOut < 0 || nTimeOut >= 1000 )
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }
      else
      {
         tv.tv_sec = ( long ) nTimeOut / 1000;
         tv.tv_usec = ( long ) ( nTimeOut % 1000 ) * 1000;
      }

      FD_ZERO( &fds );
      FD_SET( hFile, &fds );
      iResult = select( hFile + 1, fRead ? &fds : NULL,
                                   fRead ? NULL : &fds, NULL, &tv );
      hb_fsSetIOError( iResult >= 0, 0 );

      if( iResult == -1 && hb_fsOsError() == ( HB_ERRCODE ) EINTR )
      {
         iResult = 0;
#  if defined( HB_HAS_SELECT_TIMER )
         if( nTimeOut > 0 )
            nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
#  endif
      }
#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult == 0 && nTimeOut > 0 )
      {
         if( ( nTimeOut -= 1000 ) < 0 )
            break;
      }

      if( iResult != 0 || nTimeOut == 0 || hb_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) == 0 ||
          hb_vmRequestQuery() != 0 )
         break;
#  endif
   }
}
#else
{
   int iTODO; /* TODO: for given platform */

   HB_SYMBOL_UNUSED( hFile );
   HB_SYMBOL_UNUSED( nTimeOut );
   HB_SYMBOL_UNUSED( fRead );
   iResult = -1;
}
#endif /* ! HB_HAS_POLL */

   hb_vmLock();

   return iResult;
}

int hb_fsCanRead( HB_FHANDLE hFileHandle, HB_MAXINT nTimeOut )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCanRead(%p, %" PFHL "d)", ( void * ) ( HB_PTRUINT ) hFileHandle, nTimeOut ) );

   return hb_fsCanAccess( hFileHandle, nTimeOut, HB_TRUE );
}

int hb_fsCanWrite( HB_FHANDLE hFileHandle, HB_MAXINT nTimeOut )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCanWrite(%p, %" PFHL "d)", ( void * ) ( HB_PTRUINT ) hFileHandle, nTimeOut ) );

   return hb_fsCanAccess( hFileHandle, nTimeOut, HB_FALSE );
}

int hb_fsPoll( PHB_POLLFD pPollSet, int iCount, HB_MAXINT nTimeOut )
{
   int iResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPoll(%p, %d, %" PFHL "d)", ( void * ) pPollSet, iCount, nTimeOut ) );

   hb_vmUnlock();

#if defined( HB_HAS_POLL )
{
   struct pollfd fds[ 16 ], * pfds;
   static const HB_BOOL s_fSamePoll =
                  sizeof( struct pollfd ) == sizeof( HB_POLLFD ) &&
                  sizeof( pPollSet->fd ) == sizeof( fds[ 0 ].fd ) &&
                  sizeof( pPollSet->events ) == sizeof( fds[ 0 ].events ) &&
                  sizeof( pPollSet->revents ) == sizeof( fds[ 0 ].revents ) &&
                  HB_POLLIN == POLLIN && HB_POLLPRI == POLLPRI &&
                  HB_POLLOUT == POLLOUT && HB_POLLERR == POLLERR &&
                  HB_POLLHUP == POLLHUP && HB_POLLNVAL == POLLNVAL;

   HB_MAXUINT timer;
   void * pFree = NULL;
   int i;

   if( s_fSamePoll )
      pfds = ( struct pollfd * ) pPollSet;
   else
   {
      if( iCount <= ( int ) HB_SIZEOFARRAY( fds ) )
         pfds = fds;
      else
         pfds = ( struct pollfd * ) ( pFree = hb_xgrab( sizeof( struct pollfd ) * iCount ) );

      for( i = 0; i < iCount; ++i )
      {
         pfds[ i ].fd = pPollSet[ i ].fd;
         pfds[ i ].events = ( ( pPollSet[ i ].events & HB_POLLIN   ) ? POLLIN   : 0 ) |
                            ( ( pPollSet[ i ].events & HB_POLLPRI  ) ? POLLPRI  : 0 ) |
                            ( ( pPollSet[ i ].events & HB_POLLOUT  ) ? POLLOUT  : 0 ) |
                            ( ( pPollSet[ i ].events & HB_POLLERR  ) ? POLLERR  : 0 ) |
                            ( ( pPollSet[ i ].events & HB_POLLHUP  ) ? POLLHUP  : 0 ) |
                            ( ( pPollSet[ i ].events & HB_POLLNVAL ) ? POLLNVAL : 0 );
         pfds[ i ].revents = 0;
      }
   }

   timer = hb_timerInit( nTimeOut );
   for( ;; )
   {
      HB_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;
      int tout = fLast ? ( int ) nTimeOut : 1000;

      iResult = poll( pfds, iCount, tout );
      hb_fsSetIOError( iResult >= 0, 0 );
      if( iResult == -1 && hb_fsOsError() == ( HB_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = HB_FALSE;
      }
      if( iResult == 0 && ! fLast && hb_vmRequestQuery() == 0 &&
          ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 )
         continue;

      break;
   }

   if( ! s_fSamePoll )
   {
      for( i = 0; i < iCount; ++i )
      {
         pPollSet[ i ].revents = ( ( pfds[ i ].revents & POLLIN   ) ? HB_POLLIN   : 0 ) |
                                 ( ( pfds[ i ].revents & POLLPRI  ) ? HB_POLLPRI  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLOUT  ) ? HB_POLLOUT  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLERR  ) ? HB_POLLERR  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLHUP  ) ? HB_POLLHUP  : 0 ) |
                                 ( ( pfds[ i ].revents & POLLNVAL ) ? HB_POLLNVAL : 0 );
      }
   }

   if( pFree )
      hb_xfree( pFree );
}
#elif ! defined( HB_OS_SYMBIAN ) /* ! HB_HAS_POLL */
{
#  if ! defined( HB_HAS_SELECT_TIMER )
   HB_MAXUINT timer = hb_timerInit( nTimeOut );
#  endif
   fd_set rfds, wfds, efds;
   int i;

   for( ;; )
   {
      struct timeval tv;
      int iMaxFD = 0;
      HB_BOOL fLast = nTimeOut >= 0 && nTimeOut <= 1000;

      if( fLast )
      {
         tv.tv_sec = ( long ) ( nTimeOut / 1000 );
         tv.tv_usec = ( long ) ( nTimeOut % 1000 ) * 1000;
      }
      else
      {
         tv.tv_sec = 1;
         tv.tv_usec = 0;
      }

      FD_ZERO( &rfds );
      FD_ZERO( &wfds );
      FD_ZERO( &efds );

      for( i = 0; i < iCount; ++i )
      {
         PHB_POLLFD pSet = pPollSet + i;
         if( pSet->fd >= 0 &&
             ( pSet->events & ( HB_POLLIN | HB_POLLOUT | HB_POLLPRI ) ) )
         {
            if( pSet->events & HB_POLLIN )
               FD_SET( pSet->fd, &rfds );
            if( pSet->events & HB_POLLOUT )
               FD_SET( pSet->fd, &wfds );
            if( pSet->events & HB_POLLPRI )
               FD_SET( pSet->fd, &efds );
            if( pSet->fd > iMaxFD )
               iMaxFD = pSet->fd;
         }
      }

      iResult = select( iMaxFD + 1, &rfds, &wfds, &efds, &tv );
      hb_fsSetIOError( iResult >= 0, 0 );

      if( iResult == -1 && hb_fsOsError() == ( HB_ERRCODE ) EINTR )
      {
         iResult = 0;
         fLast = HB_FALSE;
#  if defined( HB_HAS_SELECT_TIMER )
         if( nTimeOut > 0 )
            nTimeOut += tv.tv_sec * 1000 + tv.tv_usec / 1000;
#  endif
      }
#  if defined( HB_HAS_SELECT_TIMER )
      if( iResult == 0 && nTimeOut > 0 )
      {
         if( ( nTimeOut -= 1000 ) < 0 )
            break;
      }

      if( iResult != 0 || fLast || nTimeOut == 0 || hb_vmRequestQuery() != 0 )
         break;
#  else
      if( iResult != 0 || fLast || ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) == 0 ||
          hb_vmRequestQuery() != 0 )
         break;
#  endif
   }
   if( iResult > 0 )
   {
      iResult = 0;
      for( i = 0; i < iCount; ++i )
      {
         PHB_POLLFD pSet = pPollSet + i;
         pSet->revents = 0;
         if( pSet->fd >= 0 )
         {
            if( FD_ISSET( pSet->fd, &rfds ) )
               pSet->revents |= HB_POLLIN;
            if( FD_ISSET( pSet->fd, &wfds ) )
               pSet->revents |= HB_POLLOUT;
            if( FD_ISSET( pSet->fd, &efds ) )
               pSet->revents |= HB_POLLPRI;
            if( pSet->revents != 0 )
               ++iResult;
         }
      }
   }
}
#else
{
   int iTODO; /* TODO: for given platform */

   HB_SYMBOL_UNUSED( pPollSet );
   HB_SYMBOL_UNUSED( iCount );
   HB_SYMBOL_UNUSED( nTimeOut );
   iResult = -1;
}
#endif /* ! HB_HAS_POLL */

   hb_vmLock();

   return iResult;
}
#endif /* HB_OS_UNIX */

HB_FHANDLE hb_fsPOpen( const char * pszFileName, const char * pszMode )
{
   HB_FHANDLE hFileHandle = FS_ERROR;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPOpen(%p, %s)", ( const void * ) pszFileName, pszMode ) );

#if defined( HB_OS_UNIX ) && ! defined( HB_OS_VXWORKS ) && ! defined( HB_OS_SYMBIAN )
   {
      HB_FHANDLE hPipeHandle[ 2 ];
      pid_t pid;
      char * pszTmp;
      HB_BOOL fRead;
      HB_SIZE nLen;

      nLen = strlen( pszFileName );
      if( pszMode && ( *pszMode == 'r' || *pszMode == 'w' ) )
         fRead = ( *pszMode == 'r' );
      else
      {
         if( pszFileName[ 0 ] == '|' )
            fRead = HB_FALSE;
         else if( pszFileName[ nLen - 1 ] == '|' )
            fRead = HB_TRUE;
         else
            fRead = HB_FALSE;
      }

      if( pszFileName[ 0 ] == '|' )
      {
         ++pszFileName;
         --nLen;
      }
      if( pszFileName[ nLen - 1 ] == '|' )
      {
         pszTmp = hb_strdup( pszFileName );
         pszTmp[ --nLen ] = 0;
         pszFileName        = pszTmp;
      }
      else
         pszTmp = NULL;

      hb_vmUnlock();
      if( pipe( hPipeHandle ) == 0 )
      {
         if( ( pid = fork() ) != -1 )
         {
            if( pid != 0 )
            {
               int iResult, iStatus = 0;

               HB_FAILURE_RETRY( iResult, waitpid( pid, &iStatus, 0 ) );

               iResult = iResult == pid &&
                         WIFEXITED( iStatus ) &&
                         WEXITSTATUS( iStatus ) == 0 ? 0 : -1;

               if( iResult != 0 )
               {
                  hb_fsClose( hPipeHandle[ 0 ] );
                  hb_fsClose( hPipeHandle[ 1 ] );
               }
               else if( fRead )
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
               HB_FHANDLE hNullHandle;
               int iMaxFD, iResult;

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
               }
               iMaxFD = sysconf( _SC_OPEN_MAX );
               if( iMaxFD < 3 )
                  iMaxFD = 1024;
               for( hNullHandle = 3; hNullHandle < iMaxFD; ++hNullHandle )
                  hb_fsClose( hNullHandle );

               pid = fork();
               if( pid == 0 )
               {
                  const char * argv[ 4 ];

                  argv[ 0 ] = "sh";
                  argv[ 1 ] = "-c";
                  argv[ 2 ] = pszFileName;
                  argv[ 3 ] = 0;

                  if( setuid( getuid() ) == -1 ) {}
                  if( setgid( getgid() ) == -1 ) {}
#if defined( __WATCOMC__ )
                  HB_FAILURE_RETRY( iResult, execv( "/bin/sh", argv ) );
#else
                  HB_FAILURE_RETRY( iResult, execv( "/bin/sh", ( char ** ) HB_UNCONST( argv ) ) );
#endif
               }
               _exit( pid > 0 ? EXIT_SUCCESS : EXIT_FAILURE );
            }
         }
         else
         {
            hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
            hb_fsCloseRaw( hPipeHandle[ 0 ] );
            hb_fsCloseRaw( hPipeHandle[ 1 ] );
         }
      }
      else
         hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      hb_vmLock();

      if( pszTmp )
         hb_xfree( pszTmp );
   }
#else

   HB_SYMBOL_UNUSED( pszFileName );
   HB_SYMBOL_UNUSED( pszMode );

   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

#endif

   return hFileHandle;
}

#if defined( HB_OS_OS2 )
#  if ! defined( HB_OS2_NONAMEDPIPES ) && ! defined( HB_OS2_USENAMEDPIPES )

/* In OS/2 anonymous pipes are not simulated by named pipes and
   unlike in MS-Windows functions for named pipes cannot be used
   with anonymous ones. Read/Write operations from/to anonymous
   pipes are always blocking on OS/2. For unblocking access we
   have to emulate anonymous pipe using named one [druzus] */
#     define HB_OS2_USENAMEDPIPES

#  endif

/* the size of system IO buffers in OS/2 pipes */
#  define HB_OS2_PIPEBUFSIZE        4096

#endif

HB_BOOL hb_fsPipeCreate( HB_FHANDLE hPipe[ 2 ] )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPipeCreate(%p)", ( void * ) hPipe ) );

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
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
   hb_fsSetIOError( fResult, 0 );
}
#elif defined( HB_OS_OS2 )
{
   HPIPE hPipeRd = 0;
   HPIPE hPipeWr = 0;
   APIRET ret;

#  if ! defined( HB_OS2_USENAMEDPIPES )

   /* create anonymous pipe */
   ret = DosCreatePipe( &hPipeRd, &hPipeWr, HB_OS2_PIPEBUFSIZE );

#  else

   /* emulate anonymous pipe using named one */
   ULONG ulOpenMode = NP_INHERIT | NP_ACCESS_DUPLEX;
   ULONG ulPipeMode = NP_NOWAIT | NP_TYPE_BYTE | NP_READMODE_BYTE | 1 /*instance*/;
   ULONG ulPid;
   PPIB ppib = NULL;

   ret = DosGetInfoBlocks( NULL, &ppib );
   ulPid = ret == NO_ERROR ? ppib->pib_ulpid : 0;

   while( ret == NO_ERROR )
   {
      static unsigned long s_ulPipeCnt = 0;
      char szPipeName[ 24 ];

      hb_snprintf( szPipeName, sizeof( szPipeName ), "\\PIPE\\%08lX.%03lX",
                   ++s_ulPipeCnt, ulPid & 0xFFFL );

      /* create the read end of the named pipe. */
      ret = DosCreateNPipe( ( PSZ ) szPipeName, &hPipeRd, ulOpenMode, ulPipeMode,
                            HB_OS2_PIPEBUFSIZE, HB_OS2_PIPEBUFSIZE, NP_DEFAULT_WAIT );
      if( ret == NO_ERROR )
      {
         ret = DosConnectNPipe( hPipeRd );
         if( ret == NO_ERROR || ret == ERROR_PIPE_NOT_CONNECTED )
         {
            /* open the write end of then named pipe */
            ULONG ulAction = 0;
            HB_FHANDLE hFile;

            ret = hb_fsOS2DosOpen( szPipeName, &hFile, &ulAction, 0,
                                   FILE_NORMAL,
                                   OPEN_ACTION_FAIL_IF_NEW | OPEN_ACTION_OPEN_IF_EXISTS,
                                   OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYNONE | OPEN_FLAGS_FAIL_ON_ERROR );
            if( ret == NO_ERROR )
            {
               hPipeWr = ( HFILE ) hFile;
               break;
            }
         }
         DosClose( hPipeRd );
      }
      if( ret == ERROR_PIPE_BUSY || ret == ERROR_ACCESS_DENIED )
         ret = NO_ERROR;
   }

#  endif

   hb_fsSetError( ( HB_ERRCODE ) ret );
   fResult = ret == NO_ERROR;
   if( fResult )
   {
      DosSetNPHState( hPipeRd, NP_WAIT | NP_READMODE_BYTE );
      hPipe[ 0 ] = ( HB_FHANDLE ) hPipeRd;
      hPipe[ 1 ] = ( HB_FHANDLE ) hPipeWr;
   }
   else
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
}
#elif defined( HB_OS_UNIX ) && ! defined( HB_OS_VXWORKS ) && ! defined( HB_OS_SYMBIAN )
{
   fResult = pipe( hPipe ) == 0;
   if( ! fResult )
      hPipe[ 0 ] = hPipe[ 1 ] = FS_ERROR;
   hb_fsSetIOError( fResult, 0 );
}
#else
{
#  if ! defined( HB_OS_DOS )
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
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsIsPipeOrSock(%p)", ( void * ) ( HB_PTRUINT ) hPipeHandle ) );

#if defined( HB_OS_UNIX )
{
#  if defined( HB_USE_LARGEFILE64 )
   struct stat64 statbuf;
   int ret = fstat64( hPipeHandle, &statbuf );
#  else
   struct stat statbuf;
   int ret = fstat( hPipeHandle, &statbuf );
#  endif
   hb_fsSetIOError( ret == 0, 0 );
   return ret == 0 &&
          ( S_ISFIFO( statbuf.st_mode ) || S_ISSOCK( statbuf.st_mode ) ) ? 1 : 0;
}
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
{
   DWORD type = GetFileType( ( HANDLE ) hb_fsGetOsHandle( hPipeHandle ) );
   hb_fsSetIOError( type != FILE_TYPE_UNKNOWN || GetLastError() == NO_ERROR, 0 );
   return type == FILE_TYPE_PIPE ? 1 : 0;
}
#elif defined( HB_OS_OS2 )
{
   ULONG type = 0, attr = 0;
   APIRET ret = DosQueryHType( ( HFILE ) hPipeHandle, &type, &attr );
   hb_fsSetError( ( HB_ERRCODE ) ret );
   return ret == NO_ERROR && ( type & 0xFF ) == FHT_PIPE ? 1 : 0;
}
#else
#  if ! defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif
   HB_SYMBOL_UNUSED( hPipeHandle );
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
   return 0;
#endif
}

HB_BOOL hb_fsPipeUnblock( HB_FHANDLE hPipeHandle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPipeUnblock(%p)", ( void * ) ( HB_PTRUINT ) hPipeHandle ) );

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   {
      DWORD dwMode = PIPE_NOWAIT;
      HB_BOOL fResult;

      fResult = SetNamedPipeHandleState( ( HANDLE ) hb_fsGetOsHandle( hPipeHandle ),
                                         &dwMode, NULL, NULL ) != 0;
      hb_fsSetIOError( fResult, 0 );
      return fResult;
   }
#elif defined( HB_OS_OS2 )
   {
      APIRET ret = DosSetNPHState( ( HPIPE ) hPipeHandle, NP_NOWAIT );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      return ret == NO_ERROR;
   }
#elif defined( HB_OS_UNIX ) && ! defined( HB_OS_MINIX )
   {
      int ret = fcntl( hPipeHandle, F_GETFL, 0 );

      if( ret != -1 && ( ret & O_NONBLOCK ) == 0 )
         ret = fcntl( hPipeHandle, F_SETFL, ret | O_NONBLOCK );
      hb_fsSetIOError( ret != -1, 0 );

      return ret != -1;
   }
#else
   {
#  if ! defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif
      HB_SYMBOL_UNUSED( hPipeHandle );
      hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
      return HB_FALSE;
   }
#endif
}

HB_SIZE hb_fsPipeIsData( HB_FHANDLE hPipeHandle, HB_SIZE nBufferSize,
                         HB_MAXINT nTimeOut )
{
   HB_SIZE nToRead = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPipeIsData(%p,%" HB_PFS "u,%" PFHL "d)", ( void * ) ( HB_PTRUINT ) hPipeHandle, nBufferSize, nTimeOut ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
{
   HB_MAXUINT timer = hb_timerInit( nTimeOut );
   HB_BOOL fResult = HB_FALSE;
   DWORD dwAvail;

   do
   {
      if( fResult )
         hb_releaseCPU();

      dwAvail = 0;
      fResult = PeekNamedPipe( ( HANDLE ) hb_fsGetOsHandle( hPipeHandle ),
                               NULL, 0, NULL, &dwAvail, NULL ) != 0;
      if( ! fResult && GetLastError() == ERROR_BROKEN_PIPE )
      {
         hb_fsSetError( 0 );
         break;
      }
      hb_fsSetIOError( fResult, 0 );
   }
   while( fResult && dwAvail == 0 &&
          ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

   if( ! fResult )
      nToRead = ( HB_SIZE ) FS_ERROR;
   else if( dwAvail > 0 )
      nToRead = ( ( HB_SIZE ) dwAvail < nBufferSize ) ? dwAvail : nBufferSize;
}
#elif defined( HB_OS_OS2 )
{

#  if ! defined( HB_OS2_USENAMEDPIPES )

   HB_SYMBOL_UNUSED( hPipeHandle );
   HB_SYMBOL_UNUSED( nTimeOut );
   hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );

   nToRead += nBufferSize;

#  else

   HB_MAXUINT timer = hb_timerInit( nTimeOut );
   HB_BOOL fResult = HB_FALSE;
   AVAILDATA avail;

   do
   {
      ULONG ulState = 0, cbActual = 0;
      APIRET ret;

      if( fResult )
         hb_releaseCPU();

      avail.cbpipe = 0;
      avail.cbmessage = 0;
      ret = DosPeekNPipe( ( HPIPE ) hPipeHandle,
                          NULL, 0, &cbActual, &avail, &ulState );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      fResult = ret == NO_ERROR &&
                ( avail.cbpipe != 0 || ulState == NP_STATE_CONNECTED );
   }
   while( fResult && avail.cbpipe == 0 &&
          ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 &&
          hb_vmRequestQuery() == 0 );

   if( ! fResult )
      nToRead = ( HB_SIZE ) FS_ERROR;
   else if( avail.cbpipe > 0 )
      nToRead = ( ( HB_SIZE ) avail.cbpipe < nBufferSize ) ? avail.cbpipe :
                                                             nBufferSize;
#  endif
}
#elif defined( HB_OS_UNIX )
{
   int iResult = hb_fsCanRead( hPipeHandle, nTimeOut );

   if( iResult > 0 )
      nToRead = nBufferSize;
   else if( iResult < 0 )
      nToRead = ( HB_SIZE ) FS_ERROR;
}
#else
{
#  if ! defined( HB_OS_DOS )
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPipeRead(%p,%p,%" HB_PFS "u,%" PFHL "d)", ( void * ) ( HB_PTRUINT ) hPipeHandle, buffer, nSize, nTimeOut ) );

   nRead = hb_fsPipeIsData( hPipeHandle, nSize, nTimeOut );
   if( nRead != ( HB_SIZE ) FS_ERROR && nRead > 0 )
   {
      nRead = hb_fsReadLarge( hPipeHandle, buffer, nRead );
      if( nRead == 0 )
         nRead = ( HB_SIZE ) FS_ERROR;
   }

   return nRead;
}

HB_SIZE hb_fsPipeWrite( HB_FHANDLE hPipeHandle, const void * buffer, HB_SIZE nSize,
                        HB_MAXINT nTimeOut )
{
   HB_SIZE nWritten;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsPipeWrite(%p,%p,%" HB_PFS "u,%" PFHL "d)", ( void * ) ( HB_PTRUINT ) hPipeHandle, buffer, nSize, nTimeOut ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
{
   HANDLE hPipe = ( HANDLE ) hb_fsGetOsHandle( hPipeHandle );
   DWORD dwMode = 0;

   if( GetNamedPipeHandleState( hPipe, &dwMode, NULL, NULL, NULL, NULL, 0 ) )
   {
      HB_MAXUINT timer = hb_timerInit( nTimeOut );
      HB_BOOL fResult = HB_FALSE;

      if( ( dwMode & PIPE_NOWAIT ) == 0 )
      {
         DWORD dwNewMode = dwMode | PIPE_NOWAIT;
         SetNamedPipeHandleState( hPipe, &dwNewMode, NULL, NULL );
      }

      nWritten = 0;
      do
      {
         DWORD dwWritten, dwToWrite;

         if( fResult )
            hb_releaseCPU();

         dwToWrite = ( DWORD ) ( nSize - nWritten );
         /* real life tests show that MSDN is wrong and MS-Windows
            refuse to accept even single byte if data is longer then
            size of PIPE buffer in unblocking mode [druzus] */
         if( dwToWrite > 4096 )
            dwToWrite = 4096;
         fResult = WriteFile( hPipe, ( const HB_BYTE * ) buffer + nWritten, dwToWrite, &dwWritten, NULL ) != 0;
         if( fResult )
            nWritten += ( HB_SIZE ) dwWritten;
         else if( nWritten == 0 )
            nWritten = ( HB_SIZE ) FS_ERROR;
         hb_fsSetIOError( fResult, 0 );
      }
      while( fResult && nWritten < nSize &&
             ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 &&
             hb_vmRequestQuery() == 0 );

      if( ( dwMode & PIPE_NOWAIT ) == 0 )
         SetNamedPipeHandleState( hPipe, &dwMode, NULL, NULL );
   }
   else
   {
      hb_fsSetIOError( HB_FALSE, 0 );
      nWritten = ( HB_SIZE ) FS_ERROR;
   }
}
#elif defined( HB_OS_OS2 )
{
#  if ! defined( HB_OS2_USENAMEDPIPES )

   HB_SYMBOL_UNUSED( nTimeOut );
   nWritten = hb_fsWriteLarge( hPipeHandle, buffer, nSize );

#  else

   ULONG state = 0;
   APIRET ret;

   ret = DosQueryNPHState( ( HPIPE ) hPipeHandle, &state );
   if( ret == NO_ERROR )
   {
      HB_MAXUINT timer = hb_timerInit( nTimeOut );
      HB_BOOL fResult = HB_FALSE;

      if( ( state & NP_NOWAIT ) == 0 )
         DosSetNPHState( ( HPIPE ) hPipeHandle, NP_NOWAIT );

      nWritten = 0;
      do
      {
         ULONG cbActual = ( ULONG ) ( nSize - nWritten );

         if( fResult )
            hb_releaseCPU();

         ret = DosWrite( ( HPIPE ) hPipeHandle, ( PSZ ) buffer + nWritten,
                         cbActual, &cbActual );
         hb_fsSetError( ( HB_ERRCODE ) ret );
         fResult = ret == NO_ERROR;
         nWritten = fResult ? ( HB_SIZE ) cbActual : ( HB_SIZE ) FS_ERROR;
      }
      while( fResult && nWritten == 0 &&
             ( nTimeOut = hb_timerTest( nTimeOut, &timer ) ) != 0 &&
             hb_vmRequestQuery() == 0 );

      if( ( state & NP_NOWAIT ) == 0 )
         DosSetNPHState( ( HPIPE ) hPipeHandle, NP_WAIT );
   }
   else
   {
      hb_fsSetError( ( HB_ERRCODE ) ret );
      nWritten = ( HB_SIZE ) FS_ERROR;
   }
#  endif
}
#elif defined( HB_OS_UNIX )
{
   int iResult = hb_fsCanWrite( hPipeHandle, nTimeOut );

   if( iResult > 0 )
   {
      int iFlags = -1;

      iResult = fcntl( hPipeHandle, F_GETFL, 0 );
      if( iResult != -1 && ( iResult & O_NONBLOCK ) == 0 )
      {
         iFlags = iResult;
         iResult = fcntl( hPipeHandle, F_SETFL, iResult | O_NONBLOCK );
      }
      if( iResult == -1 )
      {
         hb_fsSetIOError( HB_FALSE, 0 );
         nWritten = ( HB_SIZE ) FS_ERROR;
      }
      else
         nWritten = hb_fsWriteLarge( hPipeHandle, buffer, nSize );
      if( iFlags != -1 )
         fcntl( hPipeHandle, F_SETFL, iFlags );
   }
   else
      nWritten = ( HB_SIZE ) iResult;
}
#else
{
#  if ! defined( HB_OS_DOS )
      int iTODO; /* TODO: for given platform */
#  endif
   HB_SYMBOL_UNUSED( nTimeOut );
   nWritten = hb_fsWriteLarge( hPipeHandle, buffer, nSize );
}
#endif

   hb_vmLock();

   return nWritten;
}

HB_FHANDLE hb_fsCreate( const char * pszFileName, HB_FATTR nAttr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCreate(%s, %u)", pszFileName, nAttr ) );

   return hb_fsOpenEx( pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | FO_EXCLUSIVE, nAttr );
}

HB_FHANDLE hb_fsCreateEx( const char * pszFileName, HB_FATTR nAttr, HB_USHORT uiFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCreateEx(%s, %u, %hu)", pszFileName, nAttr, uiFlags ) );

   uiFlags &= ~( FO_READ | FO_WRITE | FO_READWRITE );

   return hb_fsOpenEx( pszFileName, FO_READWRITE | FO_CREAT | FO_TRUNC | uiFlags, nAttr );
}

HB_FHANDLE hb_fsOpen( const char * pszFileName, HB_USHORT uiFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsOpen(%s, %hu)", pszFileName, uiFlags ) );

   return hb_fsOpenEx( pszFileName, uiFlags, FC_NORMAL );
}

HB_FHANDLE hb_fsOpenEx( const char * pszFileName, HB_USHORT uiFlags, HB_FATTR nAttr )
{
   HB_FHANDLE hFileHandle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsOpenEx(%s, %hu, %u)", pszFileName, uiFlags, nAttr ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

      convert_open_flags( HB_FALSE, nAttr, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = CreateFile( lpFileName, dwMode, dwShare, NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#elif defined( HB_OS_OS2 )
   {
      ULONG ulAction = 0, ulAttribute, fsOpenFlags, fsOpenMode;

      convert_open_flags( HB_FALSE, nAttr, uiFlags,
                          &ulAttribute, &fsOpenFlags, &fsOpenMode );
      hb_vmUnlock();
      hb_fsOS2DosOpenL( pszFileName, &hFileHandle, &ulAction, 0,
                        ulAttribute, fsOpenFlags, fsOpenMode );
      hb_vmLock();
   }
#else
   {
      char * pszFree;
      int flags, share, attr;
      unsigned mode;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      convert_open_flags( HB_FALSE, nAttr, uiFlags, &flags, &mode, &share, &attr );

      hb_vmUnlock();

#if defined( HB_OS_DOS )
      if( ( nAttr & ( FC_HIDDEN | FC_SYSTEM ) ) == 0 ||
          access( pszFileName, F_OK ) == 0 )
         attr = 0;
#endif

#if defined( _MSC_VER )
      if( share )
         hFileHandle = _sopen( pszFileName, flags, share, mode );
      else
         hFileHandle = _open( pszFileName, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#elif defined( HB_FS_SOPEN )
      if( share )
         hFileHandle = sopen( pszFileName, flags, share, mode );
      else
         hFileHandle = open( pszFileName, flags, mode );
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
#else
      HB_FAILURE_RETRY( hFileHandle, open( pszFileName, flags | share, mode ) );
#endif

#if defined( HB_OS_DOS )
      if( attr != 0 && hFileHandle != ( HB_FHANDLE ) FS_ERROR )
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         _chmod( pszFileName, 1, attr );
#     else
         _dos_setfileattr( pszFileName, attr );
#     endif
#endif

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return hFileHandle;
}

void hb_fsCloseRaw( HB_FHANDLE hFileHandle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCloseRaw(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

   hb_vmUnlock();
#if defined( HB_OS_WIN )
   CloseHandle( DosToWinHandle( hFileHandle ) );
#elif defined( HB_OS_OS2 )
   DosClose( hFileHandle );
#else
   {
#  if defined( EINTR )
      int ret;
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( hFileHandle );
      }
      while( ret == -1 && errno == EINTR );
#  else
      close( hFileHandle );
#  endif
   }
#endif
   hb_vmLock();
}

void hb_fsClose( HB_FHANDLE hFileHandle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsClose(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

   hb_vmUnlock();
#if defined( HB_OS_WIN )
   hb_fsSetIOError( CloseHandle( DosToWinHandle( hFileHandle ) ) != 0, 0 );
#elif defined( HB_OS_OS2 )
   {
      APIRET ret = DosClose( hFileHandle );

      hb_fsSetError( ( HB_ERRCODE ) ret );
   }
#else
   {
      int ret;
#  if defined( EINTR )
      /* ignoring EINTR in close() it's quite common bug when sockets or
       * pipes are used. Without such protection it's not safe to use
       * signals in user code.
       */
      do
      {
         ret = close( hFileHandle );
      }
      while( ret == -1 && errno == EINTR );
#  else
      ret = close( hFileHandle );
#  endif
      hb_fsSetIOError( ret == 0, 0 );
   }
#endif
   hb_vmLock();
}


#define FD_TEST  0

int hb_fsSetDevMode( HB_FHANDLE hFileHandle, int iDevMode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSetDevMode(%p, %d)", ( void * ) ( HB_PTRUINT ) hFileHandle, iDevMode ) );

#if defined( HB_OS_DOS )
{
   int iRet = O_BINARY;

   switch( iDevMode )
   {
      case FD_TEST:
         iRet = setmode( ( int ) hFileHandle, O_BINARY );
         if( iRet != -1 && iRet != O_BINARY )
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
#else

   HB_SYMBOL_UNUSED( hFileHandle );

   hb_fsSetError( ( HB_ERRCODE ) ( iDevMode == FD_TEXT ? FS_ERROR : 0 ) );
   return FD_BINARY;

#endif
}

HB_BOOL hb_fsGetFileTime( const char * pszFileName, long * plJulian, long * plMillisec )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsGetFileTime(%s, %p, %p)", pszFileName, ( void * ) plJulian, ( void * ) plMillisec ) );

   fResult = HB_FALSE;
   *plJulian = *plMillisec = 0;

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      typedef BOOL ( WINAPI * _HB_GETFILEATTRIBUTESEX )( LPCTSTR, GET_FILEEX_INFO_LEVELS, LPVOID );
      static _HB_GETFILEATTRIBUTESEX s_pGetFileAttributesEx = ( _HB_GETFILEATTRIBUTESEX ) -1;

      if( s_pGetFileAttributesEx == ( _HB_GETFILEATTRIBUTESEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "kernel32.dll" ) );
         if( hModule )
            s_pGetFileAttributesEx = ( _HB_GETFILEATTRIBUTESEX )
               HB_WINAPI_GETPROCADDRESST( hModule, "GetFileAttributesEx" );
         else
            s_pGetFileAttributesEx = NULL;
      }

      if( s_pGetFileAttributesEx )
      {
         LPCTSTR lpFileName;
         LPTSTR lpFree;
         WIN32_FILE_ATTRIBUTE_DATA attrex;

         lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

         memset( &attrex, 0, sizeof( attrex ) );

         if( GetFileAttributesEx( lpFileName, GetFileExInfoStandard, &attrex ) )
         {
            FILETIME local_ft;
            SYSTEMTIME st;

            if( FileTimeToLocalFileTime( &attrex.ftLastWriteTime, &local_ft ) &&
                FileTimeToSystemTime( &local_ft, &st ) )
            {
               *plJulian = hb_dateEncode( st.wYear, st.wMonth, st.wDay );
               *plMillisec = hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

               fResult = HB_TRUE;
            }
         }
         hb_fsSetIOError( fResult, 0 );

         if( lpFree )
            hb_xfree( lpFree );
      }
      else
      {
         HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READ | FO_SHARED );
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         if( hFile != FS_ERROR )
         {
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
         else
         {
            WIN32_FIND_DATA findFileData;
            HANDLE hFindFile;
            LPCTSTR lpFileName;
            LPTSTR lpFree;

            lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );
            hFindFile = FindFirstFile( lpFileName, &findFileData );
            if( lpFree )
               hb_xfree( lpFree );

            if( hFindFile != INVALID_HANDLE_VALUE )
            {
               if( FileTimeToLocalFileTime( &findFileData.ftLastWriteTime, &local_ft ) &&
                   FileTimeToSystemTime( &local_ft, &st ) )
               {
                  *plJulian = hb_dateEncode( st.wYear, st.wMonth, st.wDay );
                  *plMillisec = hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

                  fResult = HB_TRUE;
               }
               hb_fsSetIOError( fResult, 0 );
               FindClose( hFindFile );
            }
         }
      }
   }
#elif defined( HB_OS_OS2 )

   fResult = hb_fsOS2QueryPathInfo( pszFileName, NULL, NULL, plJulian, plMillisec );

#elif defined( HB_OS_UNIX ) || defined( HB_OS_DOS ) || defined( __GNUC__ )
   {
      char * pszFree;
#  if defined( HB_USE_LARGEFILE64 )
      struct stat64 statbuf;
      if( stat64( hb_fsNameConv( pszFileName, &pszFree ), &statbuf ) == 0 )
#  else
      struct stat statbuf;
      if( stat( hb_fsNameConv( pszFileName, &pszFree ), &statbuf ) == 0 )
#  endif
      {
         time_t ftime;
         struct tm ft;

         ftime = statbuf.st_mtime;
#  if defined( HB_HAS_LOCALTIME_R )
         localtime_r( &ftime, &ft );
#  else
         ft = *localtime( &ftime );
#  endif

         *plJulian = hb_dateEncode( ft.tm_year + 1900, ft.tm_mon + 1, ft.tm_mday );
#if defined( HB_OS_LINUX ) && ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) ) && \
    defined( __GLIBC__ ) && defined( __GLIBC_MINOR__ ) && \
           ( __GLIBC__ > 2 || ( __GLIBC__ == 2 && __GLIBC_MINOR__ >= 6 ) )
         *plMillisec = hb_timeEncode( ft.tm_hour, ft.tm_min, ft.tm_sec, statbuf.st_mtim.tv_nsec / 1000000 );
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
   }
#endif

   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fsGetAttr( const char * pszFileName, HB_FATTR * pnAttr )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsGetAttr(%s, %p)", pszFileName, ( void * ) pnAttr ) );

   hb_vmUnlock();

   *pnAttr = 0;
   fResult = HB_FALSE;
#if defined( HB_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwAttr;

      lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

      dwAttr = GetFileAttributes( lpFileName );

      if( dwAttr != INVALID_FILE_ATTRIBUTES )
      {
         *pnAttr = hb_fsAttrFromRaw( dwAttr );
         fResult = HB_TRUE;
      }
      hb_fsSetIOError( fResult, 0 );

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )

   fResult = hb_fsOS2QueryPathInfo( pszFileName, NULL, pnAttr, NULL, NULL );

#else
   {
      char * pszFree;
      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

#  if defined( HB_OS_DOS )
      {
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
         int attr = _chmod( pszFileName, 0, 0 );
         if( attr != -1 )
#     else
         unsigned int attr = 0;
         if( _dos_getfileattr( pszFileName, &attr ) == 0 )
#     endif
         {
            *pnAttr = hb_fsAttrFromRaw( attr );
            fResult = HB_TRUE;
         }
         hb_fsSetIOError( fResult, 0 );
      }
#  elif defined( HB_OS_UNIX )
      {
#     if defined( HB_USE_LARGEFILE64 )
         struct stat64 statbuf;
         if( stat64( pszFileName, &statbuf ) == 0 )
#     else
         struct stat statbuf;
         if( stat( pszFileName, &statbuf ) == 0 )
#     endif
         {
            *pnAttr = hb_fsAttrFromRaw( statbuf.st_mode );
            fResult = HB_TRUE;
         }
         hb_fsSetIOError( fResult, 0 );
      }
#  else
      {
         int iTODO; /* TODO: for given platform */

         HB_SYMBOL_UNUSED( pszFileName );
      }
#  endif
      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   hb_vmLock();

   return fResult;
}

HB_BOOL hb_fsSetFileTime( const char * pszFileName, long lJulian, long lMillisec )
{
   HB_BOOL fResult;
   int iYear, iMonth, iDay;
   int iHour, iMinute, iSecond, iMSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSetFileTime(%s, %ld, %ld)", pszFileName, lJulian, lMillisec ) );

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READWRITE | FO_SHARED );

      fResult = hFile != FS_ERROR;
      if( fResult )
      {
         FILETIME local_ft;
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

         if( SystemTimeToFileTime( &st, &local_ft ) )
         {
            FILETIME ft;
            LocalFileTimeToFileTime( &local_ft, &ft );
            fResult = SetFileTime( DosToWinHandle( hFile ), NULL, &ft, &ft ) != 0;
         }
         else
            fResult = HB_FALSE;

         hb_fsSetIOError( fResult, 0 );
         hb_fsClose( hFile );
      }
   }
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ret;
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      ret = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ret == NO_ERROR )
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
         ret = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                               &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }
      hb_fsSetError( ( HB_ERRCODE ) ret );
      fResult = ret == NO_ERROR;
      if( pszFree )
         hb_xfree( pszFree );
   }
#elif defined( HB_OS_UNIX ) || defined( HB_OS_DOS )
   {
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      if( lJulian <= 0 && lMillisec < 0 )
      {
#  if defined( HB_OS_LINUX ) && ! defined( __WATCOMC__ )
         fResult = utimes( pszFileName, NULL ) == 0;
#  else
         fResult = utime( pszFileName, NULL ) == 0;
#  endif
      }
      else
      {
         struct tm new_value;

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
         new_value.tm_isdst = -1;

#  if defined( HB_OS_LINUX ) && ! defined( __WATCOMC__ )
         {
            struct timeval times[ 2 ];
            times[ 0 ].tv_sec = times[ 1 ].tv_sec = mktime( &new_value );
            times[ 0 ].tv_usec = times[ 1 ].tv_usec = iMSec * 1000;
            fResult = utimes( pszFileName, times ) == 0;
         }
#  else
         {
            struct utimbuf buf;
            buf.actime = buf.modtime = mktime( &new_value );
            fResult = utime( pszFileName, &buf ) == 0;
         }
#  endif
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

HB_BOOL hb_fsSetAttr( const char * pszFileName, HB_FATTR nAttr )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSetAttr(%s, %u)", pszFileName, nAttr ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;
      DWORD dwFlags = 0;

      lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

      if( nAttr & HB_FA_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;
      if( nAttr & HB_FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;
      if( nAttr & HB_FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;
      if( nAttr & HB_FA_ARCHIVE )
         dwFlags |= FILE_ATTRIBUTE_ARCHIVE;
      if( dwFlags == 0 )
         dwFlags = FILE_ATTRIBUTE_NORMAL;
      fResult = SetFileAttributes( lpFileName, dwFlags ) != 0;
      hb_fsSetIOError( fResult, 0 );

      if( lpFree )
         hb_xfree( lpFree );
   }
#else
   {
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

#  if defined( HB_OS_OS2 )
      {
         FILESTATUS3 fs3;
         APIRET ret;
         ULONG ulOsAttr = FILE_NORMAL;

         if( nAttr & HB_FA_READONLY )
            ulOsAttr |= FILE_READONLY;
         if( nAttr & HB_FA_HIDDEN )
            ulOsAttr |= FILE_HIDDEN;
         if( nAttr & HB_FA_SYSTEM )
            ulOsAttr |= FILE_SYSTEM;
         if( nAttr & HB_FA_ARCHIVE )
            ulOsAttr |= FILE_ARCHIVED;

         ret = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
         if( ret == NO_ERROR )
         {
            fs3.attrFile = ulOsAttr;
            ret = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                  &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
         }
         hb_fsSetError( ( HB_ERRCODE ) ret );
         fResult = ret == NO_ERROR;
      }
#  elif defined( HB_OS_DOS )

      nAttr &= ~( HB_FA_ARCHIVE | HB_FA_HIDDEN | HB_FA_READONLY | HB_FA_SYSTEM );
#     if defined( __DJGPP__ ) || defined( __BORLANDC__ )
      fResult = _chmod( pszFileName, 1, nAttr ) != -1;
#     else
      fResult = _dos_setfileattr( pszFileName, nAttr ) != -1;
#     endif
      hb_fsSetIOError( fResult, 0 );

#  elif defined( HB_OS_UNIX )
      {
         int iAttr = HB_FA_POSIX_ATTR( nAttr ), iResult;
         if( iAttr == 0 )
         {
            iAttr = S_IRUSR | S_IRGRP | S_IROTH;
            if( ! ( nAttr & HB_FA_READONLY ) )
               iAttr |= S_IWUSR | S_IWGRP | S_IWOTH;
            if( nAttr & HB_FA_SYSTEM )
               iAttr |= S_IXUSR | S_IXGRP | S_IXOTH;
            if( nAttr & HB_FA_HIDDEN )
               iAttr &= S_IRUSR | S_IWUSR | S_IXUSR;
         }
         HB_FAILURE_RETRY( iResult, chmod( pszFileName, iAttr ) );
         fResult = iResult != -1;
      }
#  else
      {
         int iTODO; /* To force warning */

         fResult = HB_FALSE;
         hb_fsSetError( ( HB_ERRCODE ) FS_ERROR );
      }
#  endif
      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   hb_vmLock();

   return fResult;
}

HB_USHORT hb_fsRead( HB_FHANDLE hFileHandle, void * pBuff, HB_USHORT uiCount )
{
   HB_USHORT uiRead;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsRead(%p, %p, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, uiCount ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      DWORD dwRead;
      BOOL bResult;

      bResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, ( DWORD ) uiCount, &dwRead, NULL );
      hb_fsSetIOError( bResult != 0, 0 );

      uiRead = bResult ? ( HB_USHORT ) dwRead : 0;
   }
#elif defined( HB_OS_OS2 )
   {
      ULONG ulRead = 0;
      APIRET ret;

      ret = DosRead( hFileHandle, pBuff, uiCount, &ulRead );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      uiRead = ret == NO_ERROR ? ( HB_USHORT ) ulRead : 0;
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
   HB_USHORT uiWritten = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsWrite(%p, %p, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, uiCount ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      BOOL bResult;

      if( uiCount )
      {
         DWORD dwWritten = 0;
         bResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff, uiCount, &dwWritten, NULL );
         uiWritten = bResult ? ( HB_USHORT ) dwWritten : 0;
      }
      else
          bResult = SetEndOfFile( DosToWinHandle( hFileHandle ) );
      hb_fsSetIOError( bResult != 0, 0 );

   }
#elif defined( HB_OS_OS2 )
   {
      APIRET ret;

      if( uiCount )
      {
         ULONG ulWritten = 0;
         ret = DosWrite( hFileHandle, ( void * ) pBuff, uiCount, &ulWritten );
         hb_fsSetError( ( HB_ERRCODE ) ret );
         uiWritten = ret == NO_ERROR ? ( HB_USHORT ) ulWritten : 0;
      }
      else
      {
         HB_FOFFSET nPos;
         ret = hb_fsOS2DosSetFilePtrL( hFileHandle, 0, SEEK_CUR, &nPos );
         if( ret == NO_ERROR )
            ret = hb_fsOS2DosSetFileSizeL( hFileHandle, nPos );
      }
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
   }
#endif

   hb_vmLock();

   return uiWritten;
}

HB_SIZE hb_fsReadLarge( HB_FHANDLE hFileHandle, void * pBuff, HB_SIZE nCount )
{
   HB_SIZE nRead;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsReadLarge(%p, %p, %" HB_PFS "u)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, nCount ) );

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
#elif defined( HB_OS_OS2 )
   {
      ULONG ulRead = 0;
      APIRET ret;

      ret = DosRead( hFileHandle, pBuff, nCount, &ulRead );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      nRead = ret == NO_ERROR ? ( HB_SIZE ) ulRead : 0;
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsWriteLarge(%p, %p, %" HB_PFS "u)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, nCount ) );

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

#elif defined( HB_OS_OS2 )
   {
      APIRET ret;

      if( nCount )
      {
         ULONG ulWritten = 0;
         ret = DosWrite( hFileHandle, ( void * ) pBuff, nCount, &ulWritten );
         hb_fsSetError( ( HB_ERRCODE ) ret );
         nWritten = ret == NO_ERROR ? ( HB_SIZE ) ulWritten : 0;
      }
      else
      {
         HB_FOFFSET nPos;
         ret = hb_fsOS2DosSetFilePtrL( hFileHandle, 0, SEEK_CUR, &nPos );
         if( ret == NO_ERROR )
            ret = hb_fsOS2DosSetFileSizeL( hFileHandle, nPos );
      }
   }
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsReadAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, nCount, nOffset ) );

   hb_vmUnlock();

#if defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ ) && ! defined( HB_OS_VXWORKS ) && ! defined( HB_OS_SYMBIAN )
   {
      long lRead;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( lRead, pread64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      HB_FAILURE_RETRY( lRead, pread( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nRead = lRead == -1 ? 0 : lRead;
   }
#elif defined( HB_OS_WIN )
#  if defined( HB_WIN_IOREAD_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      nRead = 0;
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
#  else
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
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
      {
         hb_fsSetIOError( HB_FALSE, 0 );
         nRead = 0;
      }
      else
      {
         DWORD dwRead = 0;
         hb_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                    pBuff, ( DWORD ) nCount, &dwRead, NULL ) != 0, 0 );
         nRead = dwRead;
      }
   }
#  endif /* HB_WIN_IOREAD_LIMIT */

/* FIXME: below are not atom operations. It has to be fixed for RDD
 *        file access with shared file handles in aliased work areas
 */
#elif defined( HB_OS_OS2 )
   {
      ULONG ulRead = 0;
      HB_FOFFSET nPos;
      APIRET ret;

      ret = hb_fsOS2DosSetFilePtrL( hFileHandle, nOffset, SEEK_SET, &nPos );
      if( ret == NO_ERROR )
         ret = DosRead( hFileHandle, pBuff, nCount, &ulRead );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      nRead = ret == NO_ERROR ? ulRead : 0;
   }
#elif defined( HB_FS_IO_16BIT )
   if( hb_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nRead = hb_fsReadLarge( hFileHandle, pBuff, nCount );
   else
      nRead = 0;
#else
   {
#  if defined( HB_USE_LARGEFILE64 )
      HB_FOFFSET nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#  else
      HB_FOFFSET nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#  endif
      if( nPos == ( HB_FOFFSET ) -1 )
      {
         hb_fsSetIOError( HB_FALSE, 0 );
         nRead = 0;
      }
      else
      {
         long lRead;
         HB_FAILURE_RETRY( lRead, read( hFileHandle, pBuff, nCount ) );
         nRead = lRead == -1 ? 0 : lRead;
      }
   }
#endif

   hb_vmLock();

   return nRead;
}

HB_SIZE hb_fsWriteAt( HB_FHANDLE hFileHandle, const void * pBuff, HB_SIZE nCount, HB_FOFFSET nOffset )
{
   HB_SIZE nWritten;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsWriteAt(%p, %p, %" HB_PFS "u, %" PFHL "i)", ( void * ) ( HB_PTRUINT ) hFileHandle, pBuff, nCount, nOffset ) );

   hb_vmUnlock();

#if defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ ) && ! defined( HB_OS_VXWORKS ) && ! defined( HB_OS_SYMBIAN )
   {
      long lWritten;
#  if defined( HB_USE_LARGEFILE64 )
      HB_FAILURE_RETRY( lWritten, pwrite64( hFileHandle, pBuff, nCount, nOffset ) );
#  else
      HB_FAILURE_RETRY( lWritten, pwrite( hFileHandle, pBuff, nCount, nOffset ) );
#  endif
      nWritten = lWritten == -1 ? 0 : lWritten;
   }
#elif defined( HB_OS_WIN )
#  if defined( HB_WIN_IOWRITE_LIMIT )
   {
      HANDLE hWFileHandle = DosToWinHandle( hFileHandle );
      OVERLAPPED Overlapped;
      BOOL bResult = TRUE;

      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( nOffset & 0xFFFFFFFF );
      Overlapped.OffsetHigh = ( DWORD ) ( nOffset >> 32 );

      nWritten = 0;
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
#  else
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
      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    SEEK_SET );
      if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
      {
         hb_fsSetIOError( HB_FALSE, 0 );
         nWritten = 0;
      }
      else
      {
         DWORD dwWritten = 0;
         hb_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                     pBuff, ( DWORD ) nCount, &dwWritten, NULL ) != 0, 0 );
         nWritten = dwWritten;
      }
   }
#  endif /* HB_WIN_IOWRITE_LIMIT */

/* FIXME: below are not atom operations. It has to be fixed for RDD
 *        file access with shared file handles in aliased work areas
 */
#elif defined( HB_OS_OS2 )
   {
      ULONG ulWritten = 0;
      HB_FOFFSET nPos;
      APIRET ret;

      ret = hb_fsOS2DosSetFilePtrL( hFileHandle, nOffset, SEEK_SET, &nPos );
      if( ret == NO_ERROR )
         ret = DosWrite( hFileHandle, ( void * ) pBuff, nCount, &ulWritten );
      hb_fsSetError( ( HB_ERRCODE ) ret );
      nWritten = ret == NO_ERROR ? ulWritten : 0;
   }
#elif defined( HB_FS_IO_16BIT )
   if( hb_fsSeekLarge( hFileHandle, nOffset, FS_SET ) == nOffset )
      nWritten = hb_fsWriteLarge( hFileHandle, pBuff, nCount );
   else
      nWritten = 0;
#else
   {
#  if defined( HB_USE_LARGEFILE64 )
      HB_FOFFSET nPos = lseek64( hFileHandle, nOffset, SEEK_SET );
#  else
      HB_FOFFSET nPos = lseek( hFileHandle, nOffset, SEEK_SET );
#  endif
      if( nPos == ( HB_FOFFSET ) -1 )
      {
         hb_fsSetIOError( HB_FALSE, 0 );
         nWritten = 0;
      }
      else
      {
         long lWritten;
         HB_FAILURE_RETRY( lWritten, write( hFileHandle, pBuff, nCount ) );
         nWritten = lWritten == -1 ? 0 : lWritten;
      }
   }
#endif

   hb_vmLock();

   return nWritten;
}

HB_BOOL hb_fsTruncAt( HB_FHANDLE hFileHandle, HB_FOFFSET nOffset )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsTruncAt(%p, %" PFHL "i)", ( void * ) ( HB_PTRUINT ) hFileHandle, nOffset ) );

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
#elif defined( HB_OS_OS2 )
   {
      APIRET ret;

      ret = hb_fsOS2DosSetFileSizeL( hFileHandle, nOffset );
      fResult = ret == NO_ERROR;
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
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCommit(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   hb_fsSetIOError( FlushFileBuffers( DosToWinHandle( hFileHandle ) ) != 0, 0 );

#elif defined( HB_OS_OS2 )

   hb_fsSetError( ( HB_ERRCODE ) DosResetBuffer( hFileHandle ) );

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

   /* NOTE: close() functions releases all locks regardless if it is an
    * original or duplicated file handle
    */
   /* This hack is very dangerous. POSIX standard define that if _ANY_
    * file handle is closed all locks set by the process on the file
    * pointed by this descriptor are removed. It doesn't matter they
    * were done using different descriptor. It means that we now clean
    * all locks on hFileHandle with the code below if the OS is POSIX
    * compliant. I vote to disable it. [druzus]
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsLock(%p, %lu, %lu, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, ulStart, ulLength, uiMode ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:

         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            DWORD dwFlags;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            dwFlags = ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK;
            if( ! s_fUseWaitLocks || ! ( uiMode & FLX_WAIT ) )
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

      case FL_UNLOCK:

         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            memset( &sOlap, 0, sizeof( sOlap ) );
            sOlap.Offset = ( DWORD ) ulStart;
            fResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0, ulLength, 0, &sOlap ) != 0;
         }
         else
         {
            fResult = UnlockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength, 0 ) != 0;
         }
         break;

      default:
         fResult = HB_FALSE;
   }
   hb_fsSetIOError( fResult, 0 );
#elif defined( HB_OS_OS2 )
   {
      struct _FILELOCKL fl, ful;
      APIRET ret;

      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            fl.lOffset = ulStart;
            fl.lRange = ulLength;
            ful.lOffset = 0;
            ful.lRange = 0;

            /* lock region, 2 seconds timeout, exclusive access - no atomic */
            ret = hb_fsOS2DosSetFileLocksL( hFileHandle, &ful, &fl, 2000L, 0L );
            break;

         case FL_UNLOCK:

            fl.lOffset = 0;
            fl.lRange = 0;
            ful.lOffset = ulStart;
            ful.lRange = ulLength;

            /* unlock region, 2 seconds timeout, exclusive access - no atomic */
            ret = hb_fsOS2DosSetFileLocksL( hFileHandle, &ful, &fl, 2000L, 0L );
            break;

         default:
            ret = ERROR_INVALID_DATA;
         hb_fsSetError( ( HB_ERRCODE ) ret );
      }
      fResult = ret == NO_ERROR;
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
            lock_info.l_pid    = 0;

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
            lock_info.l_pid    = 0;

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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsLockLarge(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, nStart, nLength, uiMode ) );

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
               if( ! s_fUseWaitLocks || ! ( uiMode & FLX_WAIT ) )
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
#elif defined( HB_OS_OS2 )
   {
      struct _FILELOCKL fl, ful;
      APIRET ret;

      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            fl.lOffset = nStart;
            fl.lRange = nLength;
            ful.lOffset = 0;
            ful.lRange = 0;

            /* lock region, 2 seconds timeout, exclusive access - no atomic */
            ret = hb_fsOS2DosSetFileLocksL( hFileHandle, &ful, &fl, 2000L, 0L );
            break;

         case FL_UNLOCK:

            fl.lOffset = 0;
            fl.lRange = 0;
            ful.lOffset = nStart;
            ful.lRange = nLength;

            /* unlock region, 2 seconds timeout, exclusive access - no atomic */
            ret = hb_fsOS2DosSetFileLocksL( hFileHandle, &ful, &fl, 2000L, 0L );
            break;

         default:
            ret = ERROR_INVALID_DATA;
            hb_fsSetError( ( HB_ERRCODE ) ret );
      }
      fResult = ret == NO_ERROR;
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
            lock_info.l_pid    = 0;

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
            lock_info.l_pid    = 0;

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

int hb_fsLockTest( HB_FHANDLE hFileHandle, HB_FOFFSET nStart,
                   HB_FOFFSET nLength, HB_USHORT uiMode )
{
   int iResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsLockTest(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, nStart, nLength, uiMode ) );

#if defined( HB_OS_UNIX )
{
#  if defined( HB_USE_LARGEFILE64 )
      struct flock64 lock_info;

      lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
      lock_info.l_start  = nStart;
      lock_info.l_len    = nLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid    = 0;
      iResult = fcntl( hFileHandle, F_GETLK64, &lock_info ) != -1 ?
                ( int ) lock_info.l_pid : -1;
#  else
      struct flock lock_info;

      lock_info.l_type   = ( uiMode & FLX_SHARED ) ? F_RDLCK : F_WRLCK;
      lock_info.l_start  = nStart;
      lock_info.l_len    = nLength;
      lock_info.l_whence = SEEK_SET;
      lock_info.l_pid    = 0;
      iResult = fcntl( hFileHandle, F_GETLK, &lock_info ) != -1 ?
                ( int ) lock_info.l_pid : -1;
#  endif
}
#else
   if( hb_fsLockLarge( hFileHandle, nStart, nLength, ( uiMode & FLX_SHARED ) | FL_LOCK ) )
   {
      if( ! hb_fsLockLarge( hFileHandle, nStart, nLength, FL_UNLOCK ) )
         iResult = -1;
      else
         iResult = 0;
   }
   else
      iResult = 1;
#endif

   return iResult;
}

HB_ULONG hb_fsSeek( HB_FHANDLE hFileHandle, HB_LONG lOffset, HB_USHORT uiFlags )
{
   HB_ULONG ulPos;
   HB_USHORT nFlags;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSeek(%p, %ld, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, lOffset, uiFlags ) );

   nFlags = convert_seek_flags( uiFlags );

   hb_vmUnlock();
#if defined( HB_OS_WIN )
   /* This DOS hack creates 2 GiB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( ULONG ) INVALID_SET_FILE_POINTER;
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
#elif defined( HB_OS_OS2 )
   {
      APIRET ret;

      /* This DOS hack creates 2 GiB file size limit, Druzus */
      if( lOffset < 0 && nFlags == SEEK_SET )
         ret = 25; /* 'Seek Error' */
      else
         ret = DosSetFilePtr( hFileHandle, lOffset, nFlags, &ulPos );
      hb_fsSetError( ( HB_ERRCODE ) ret );

      if( ret != NO_ERROR )
      {
         if( DosSetFilePtr( hFileHandle, 0, SEEK_CUR, &ulPos ) != NO_ERROR )
            ulPos = 0;
      }
   }
#else
   /* This DOS hack creates 2 GiB file size limit, Druzus */
   if( lOffset < 0 && nFlags == SEEK_SET )
   {
      ulPos = ( HB_ULONG ) -1;
      hb_fsSetError( 25 ); /* 'Seek Error' */
   }
   else
   {
      ulPos = lseek( hFileHandle, lOffset, nFlags );
      hb_fsSetIOError( ulPos != ( HB_ULONG ) -1, 0 );
#  if defined( HB_OS_UNIX )
      /* small trick to resolve problem with position reported for directories */
      if( ulPos == LONG_MAX && lOffset == 0 && nFlags == SEEK_END )
      {
         /* we do not need to use fstat64() here on 32-bit platforms, [druzus] */
         struct stat st;

         if( fstat( hFileHandle, &st ) == 0 )
            ulPos = st.st_size;
      }
#  endif
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSeekLarge(%p, %" PFHL "i, %hu)", ( void * ) ( HB_PTRUINT ) hFileHandle, nOffset, uiFlags ) );

#if defined( HB_OS_WIN )
   {
      HB_USHORT nFlags = convert_seek_flags( uiFlags );

      ULONG ulOffsetLow  = ( ULONG ) ( nOffset & 0xFFFFFFFF ),
            ulOffsetHigh = ( ULONG ) ( nOffset >> 32 );

      hb_vmUnlock();
      if( nOffset < 0 && nFlags == SEEK_SET )
      {
         nPos = ( HB_FOFFSET ) -1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                       ( DWORD ) nFlags );
         if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
            nPos = ( HB_FOFFSET ) -1;
         else
            nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
         hb_fsSetIOError( nPos != ( HB_FOFFSET ) -1, 0 );
      }

      if( nPos == ( HB_FOFFSET ) -1 )
      {
         ulOffsetHigh = 0;
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       0, ( PLONG ) &ulOffsetHigh, SEEK_CUR );
         if( ulOffsetLow == ( ULONG ) INVALID_SET_FILE_POINTER && GetLastError() != NO_ERROR )
            nPos = 0;
         else
            nPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
      }
      hb_vmLock();
   }
#elif defined( HB_OS_OS2 )
   {
      HB_USHORT nFlags = convert_seek_flags( uiFlags );
      APIRET ret;

      hb_vmUnlock();
      nPos = 0;
      if( nOffset < 0 && nFlags == SEEK_SET )
         ret = 25; /* 'Seek Error' */
      else
         ret = hb_fsOS2DosSetFilePtrL( hFileHandle, nOffset, nFlags, &nPos );

      if( ret != NO_ERROR )
      {
         if( hb_fsOS2DosSetFilePtrL( hFileHandle, 0, SEEK_CUR, &nPos ) != NO_ERROR )
            nPos = 0;
      }
      hb_fsSetError( ( HB_ERRCODE ) ret );
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
#  if defined( HB_OS_UNIX )
         /* small trick to resolve problem with position reported for directories */
         if( nPos == LONG_MAX && nOffset == 0 && nFlags == SEEK_END )
         {
            struct stat64 st;

            if( fstat64( hFileHandle, &st ) == 0 )
               nPos = st.st_size;
         }
#  endif
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
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsTell(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

   return hb_fsSeekLarge( hFileHandle, 0, FS_RELATIVE );
}

HB_FOFFSET hb_fsGetSize( HB_FHANDLE hFileHandle )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_fsGetSize(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

#if defined( HB_OS_WIN )
   {
      DWORD dwFileSizeLow, dwFileSizeHigh = 0;
      HB_BOOL fOK;

      dwFileSizeLow = GetFileSize( DosToWinHandle( hFileHandle ), &dwFileSizeHigh );
      fOK = dwFileSizeLow != INVALID_FILE_SIZE || GetLastError() == NO_ERROR;
      hb_fsSetIOError( fOK, 0 );

      return fOK ? ( ( HB_FOFFSET ) dwFileSizeHigh << 32 ) | dwFileSizeLow : 0;
   }
#else
   return hb_fsSeekLarge( hFileHandle, 0, FS_END );
#endif
}

HB_BOOL hb_fsDelete( const char * pszFileName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsDelete(%s)", pszFileName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpFileName;
      LPTSTR lpFree;

      lpFileName = HB_FSNAMECONV( pszFileName, &lpFree );

      hb_vmUnlock();

      fResult = DeleteFile( lpFileName ) != 0;
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFree;
      APIRET ret;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      hb_vmUnlock();

      ret = DosDelete( ( PCSZ ) pszFileName );
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      hb_vmUnlock();

      fResult = ( remove( pszFileName ) == 0 );
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return fResult;
}

HB_BOOL hb_fsRename( const char * pOldName, const char * pNewName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsRename(%s, %s)", pOldName, pNewName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpOldName, lpNewName;
      LPTSTR lpOldFree, lpNewFree;

      lpOldName = HB_FSNAMECONV( pOldName, &lpOldFree );
      lpNewName = HB_FSNAMECONV( pNewName, &lpNewFree );

      hb_vmUnlock();

      fResult = MoveFile( lpOldName, lpNewName ) != 0;
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( lpOldFree )
         hb_xfree( lpOldFree );
      if( lpNewFree )
         hb_xfree( lpNewFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFreeOld, * pszFreeNew;
      APIRET ret;


      pOldName = hb_fsNameConv( pOldName, &pszFreeOld );
      pNewName = hb_fsNameConv( pNewName, &pszFreeNew );

      hb_vmUnlock();

      ret = DosMove( ( PCSZ ) pOldName, ( PCSZ ) pNewName );
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFreeOld )
         hb_xfree( pszFreeOld );
      if( pszFreeNew )
         hb_xfree( pszFreeNew );
   }
#else
   {
      char * pszFreeOld, * pszFreeNew;

      pOldName = hb_fsNameConv( pOldName, &pszFreeOld );
      pNewName = hb_fsNameConv( pNewName, &pszFreeNew );

      hb_vmUnlock();

      fResult = ( rename( pOldName, pNewName ) == 0 );
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( pszFreeOld )
         hb_xfree( pszFreeOld );
      if( pszFreeNew )
         hb_xfree( pszFreeNew );
   }
#endif

   return fResult;
}

HB_BOOL hb_fsMkDir( const char * pszDirName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsMkDir(%s)", pszDirName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;

      lpDirName = HB_FSNAMECONV( pszDirName, &lpFree );

      hb_vmUnlock();

      fResult = CreateDirectory( lpDirName, NULL ) != 0;
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFree;
      APIRET ret;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      ret = DosCreateDir( ( PCSZ ) pszDirName, NULL );
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      char * pszFree;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

#  if ! defined( HB_OS_UNIX ) && \
      ( defined( __WATCOMC__ ) || defined( __BORLANDC__ ) || \
        defined( __IBMCPP__ ) || defined( __MINGW32__ ) )
      fResult = ( mkdir( pszDirName ) == 0 );
#  else
      fResult = ( mkdir( pszDirName, S_IRWXU | S_IRWXG | S_IRWXO ) == 0 );
#  endif
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return fResult;
}

HB_BOOL hb_fsChDir( const char * pszDirName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsChDir(%s)", pszDirName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;
      UINT uiErrMode;

      lpDirName = HB_FSNAMECONV( pszDirName, &lpFree );

      hb_vmUnlock();

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( lpDirName ) != FALSE;
      SetErrorMode( uiErrMode );
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFree;
      APIRET ret;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      ret = DosSetCurrentDir( ( PCSZ ) pszDirName );
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      char * pszFree;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      fResult = ( chdir( pszDirName ) == 0 );
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return fResult;
}

HB_BOOL hb_fsRmDir( const char * pszDirName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsRmDir(%s)", pszDirName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;

      lpDirName = HB_FSNAMECONV( pszDirName, &lpFree );

      hb_vmUnlock();

      fResult = RemoveDirectory( lpDirName ) != 0;
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFree;
      APIRET ret;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      ret = DosDeleteDir( ( PCSZ ) pszDirName );
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      char * pszFree;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      fResult = ( rmdir( pszDirName ) == 0 );
      hb_fsSetIOError( fResult, 0 );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return fResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

const char * hb_fsCurDir( int iDrive )
{
   char * pszDirBuffer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCurDir(%d)", iDrive ) );

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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCurDirBuff(%d)", iDrive ) );

   pszBuffer[ 0 ] = '\0';

   /*
    * do not cover this code by HB_OS_HAS_DRIVE_LETTER macro
    * It will allow us to add drive emulation in hb_fsCurDrv()/hb_fsChDrv()
    * and hb_fsNameConv()
    */
#if defined( HB_OS_WIN ) || ! ( defined( HB_OS_OS2 ) || defined( __MINGW32__ ) )
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
      LPTSTR lpBuffer = ( LPTSTR ) hb_xgrab( dwSize * sizeof( TCHAR ) );
      lpBuffer[ 0 ] = TEXT( '\0' );
      hb_fsSetIOError( ( GetCurrentDirectory( dwSize, lpBuffer ) != 0 ), 0 );
      lpBuffer[ dwSize - 1 ] = TEXT( '\0' );
      HB_OSSTRDUP2( lpBuffer, pszBuffer, nSize - 1 );
      hb_xfree( lpBuffer );
   }
#elif defined( HB_OS_OS2 )

   if( iDrive >= 0 )
   {
      ULONG ulLen = ( ULONG ) nSize;
      hb_fsSetError( ( HB_ERRCODE ) DosQueryCurrentDir( iDrive, ( PBYTE ) pszBuffer, &ulLen ) );
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
      char * pszStart;
      HB_SIZE nLen;

      /* Strip the leading drive spec, and leading backslash if there's one. */
      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

#if defined( __DJGPP__ ) || defined( HB_OS_OS2 )
      /* convert '/' to '\' */
      nLen = 0;
      while( pszBuffer[ nLen ] != 0 )
      {
         if( pszBuffer[ nLen ] == '/' )
            pszBuffer[ nLen ] = '\\';
         ++nLen;
      }
#else
      nLen = strlen( pszBuffer );
#endif
      pszStart = pszBuffer;

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

#if ! defined( HB_OS_WIN )
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
#endif
   }

   return nResult;
}

HB_BOOL hb_fsGetCWD( char * pszBuffer, HB_SIZE nSize )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsGetCWD(%p,%" HB_PFS "u)", ( void * ) pszBuffer, nSize ) );

   pszBuffer[ 0 ] = '\0';

   hb_vmUnlock();

#if defined( HB_OS_WIN )
   {
      DWORD dwSize = ( DWORD ) nSize;
      LPTSTR lpBuffer = ( LPTSTR ) hb_xgrab( dwSize * sizeof( TCHAR ) );
      lpBuffer[ 0 ] = TEXT( '\0' );
      fResult = GetCurrentDirectory( dwSize, lpBuffer ) != 0;
      hb_fsSetIOError( fResult, 0 );
      lpBuffer[ dwSize - 1 ] = TEXT( '\0' );
      HB_OSSTRDUP2( lpBuffer, pszBuffer, nSize - 1 );
      hb_xfree( lpBuffer );
   }
#elif defined( HB_OS_OS2 )
   {
      ULONG ulDrive = 0, ulLogical;
      APIRET ret;

      ret = DosQueryCurrentDisk( &ulDrive, &ulLogical );
      if( ret == NO_ERROR )
      {
         ULONG ulLen = ( ULONG ) nSize - 3;
         pszBuffer[ 0 ] = ( char ) ( ulDrive + ( 'A' - 1 ) );
         pszBuffer[ 1 ] = HB_OS_DRIVE_DELIM_CHR;
         pszBuffer[ 2 ] = HB_OS_PATH_DELIM_CHR;
         ret = DosQueryCurrentDir( 0, ( PBYTE ) pszBuffer + 3, &ulLen );
      }
      hb_fsSetError( ( HB_ERRCODE ) ret );
      fResult = ret == NO_ERROR;
      if( ! fResult )
         pszBuffer[ 0 ] = '\0';
   }
#else

   fResult = getcwd( pszBuffer, nSize ) != NULL;
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   pszBuffer[ nSize - 1 ] = '\0';

   if( fResult && pszBuffer[ 0 ] )
   {
      HB_SIZE nLen;
#if defined( __DJGPP__ )
      /* convert '/' to '\' */
      nLen = 0;
      while( pszBuffer[ nLen ] != 0 )
      {
         if( pszBuffer[ nLen ] == '/' )
            pszBuffer[ nLen ] = '\\';
         ++nLen;
      }
#else
      nLen = strlen( pszBuffer );
#endif

      /* add the trailing (back)slash if there's no one */
      if( nLen + 1 < nSize &&
          strchr( HB_OS_PATH_DELIM_CHR_LIST, ( HB_UCHAR ) pszBuffer[ nLen - 1 ] ) == 0 )
      {
         pszBuffer[ nLen++ ] = HB_OS_PATH_DELIM_CHR;
         pszBuffer[ nLen ] = '\0';
      }

#if ! defined( HB_OS_WIN )
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
#endif
   }

   return fResult;
}

HB_BOOL hb_fsSetCWD( const char * pszDirName )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsSetCWD(%s)", pszDirName ) );

#if defined( HB_OS_WIN )
   {
      LPCTSTR lpDirName;
      LPTSTR lpFree;
      UINT uiErrMode;

      lpDirName = HB_FSNAMECONV( pszDirName, &lpFree );

      hb_vmUnlock();

      uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
      fResult = SetCurrentDirectory( lpDirName ) != FALSE;
      hb_fsSetIOError( fResult, 0 );
      SetErrorMode( uiErrMode );

      hb_vmLock();

      if( lpFree )
         hb_xfree( lpFree );
   }
#elif defined( HB_OS_OS2 )
   {
      char * pszFree;
      APIRET ret;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      ret = DosSetCurrentDir( ( PCSZ ) pszDirName );
      if( ret == NO_ERROR && pszDirName[ 0 ] != 0 &&
          pszDirName[ 1 ] == HB_OS_DRIVE_DELIM_CHR )
      {
         int iDrive = pszDirName[ 0 ];

         if( iDrive >= 'A' && iDrive <= 'Z' )
            iDrive -= 'A';
         else if( iDrive >= 'a' && iDrive <= 'z' )
            iDrive -= 'a';
         else
            iDrive = 0;

         if( iDrive )
            ret = DosSetDefaultDisk( iDrive );
      }
      fResult = ret == NO_ERROR;
      hb_fsSetError( ( HB_ERRCODE ) ret );

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      char * pszFree;

      pszDirName = hb_fsNameConv( pszDirName, &pszFree );

      hb_vmUnlock();

      fResult = ( chdir( pszDirName ) == 0 );
      hb_fsSetIOError( fResult, 0 );

#if defined( HB_OS_HAS_DRIVE_LETTER ) && ! defined( __DJGPP__ )
      if( fResult && pszDirName[ 0 ] != 0 &&
          pszDirName[ 1 ] == HB_OS_DRIVE_DELIM_CHR )
      {
         int iDrive = pszDirName[ 0 ];

         if( iDrive >= 'A' && iDrive <= 'Z' )
            iDrive -= 'A';
         else if( iDrive >= 'a' && iDrive <= 'z' )
            iDrive -= 'a';
         else
            iDrive = 0;

         if( iDrive )
            HB_FS_SETDRIVE( iDrive );
      }
#endif

      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#endif

   return fResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

HB_ERRCODE hb_fsChDrv( int iDrive )
{
   HB_ERRCODE nResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsChDrv(%d)", iDrive ) );

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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsCurDrv()" ) );

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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsIsDrv(%d)", iDrive ) );

   if( iDrive >= 0 )
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   {
      hb_vmUnlock();
      nResult = ( ( GetLogicalDrives() >> iDrive ) & 1 ) ? 0 : ( HB_ERRCODE ) F_ERROR;
      hb_vmLock();
      hb_fsSetError( 0 );
   }
#elif defined( HB_OS_OS2 )
   {
      ULONG ulDrive, ulLogical;
      APIRET ret;

      hb_vmUnlock();
      ret = DosQueryCurrentDisk( &ulDrive, &ulLogical );
      hb_vmLock();
      nResult = ret == NO_ERROR && ( ( ulLogical >> iDrive ) & 1 ) ? 0 : ( HB_ERRCODE ) F_ERROR;
      hb_fsSetError( 0 );
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

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsIsDevice(%p)", ( void * ) ( HB_PTRUINT ) hFileHandle ) );

   hb_vmUnlock();

#if defined( HB_OS_WIN )

   fResult = GetFileType( DosToWinHandle( hFileHandle ) ) == FILE_TYPE_CHAR;
   hb_fsSetIOError( fResult, 0 );

#elif defined( HB_OS_OS2 )
{
   ULONG type = 0, attr = 0;
   APIRET ret = DosQueryHType( ( HFILE ) hFileHandle, &type, &attr );
   hb_fsSetError( ( HB_ERRCODE ) ret );
   fResult = ret == NO_ERROR && ( type & 0xFF ) == FHT_CHRDEV;
}
#else

#  if defined( _MSC_VER ) || defined( __MINGW32__ )
      fResult = _isatty( hFileHandle ) != 0;
#  else
      fResult = isatty( hFileHandle ) != 0;
#  endif
   hb_fsSetIOError( fResult, 0 );

#endif

   hb_vmLock();

   return fResult;
}

/* convert file name for hb_fsExtOpen()
 * caller must free the returned buffer
 */
char * hb_fsExtName( const char * pszFileName, const char * pDefExt,
                     HB_FATTR nExFlags, const char * pPaths )
{
   HB_PATHNAMES * pNextPath;
   PHB_FNAME pFilepath;
   HB_BOOL fIsFile = HB_FALSE;
   char * szPath;

   szPath = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pszFileName );

   if( pDefExt && ( ( nExFlags & FXO_FORCEEXT ) || ! pFilepath->szExtension ) )
      pFilepath->szExtension = pDefExt;

   if( pFilepath->szPath )
   {
      hb_fsFNameMerge( szPath, pFilepath );
   }
   else if( nExFlags & FXO_DEFAULTS )
   {
      const char * szDefault = hb_setGetDefault();
      if( szDefault )
      {
         pFilepath->szPath = szDefault;
         hb_fsFNameMerge( szPath, pFilepath );
         fIsFile = hb_fsFileExists( szPath );
      }
      if( ! fIsFile &&
          ( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
          hb_setGetPath() )
      {
         pNextPath = hb_setGetFirstSetPath();
         while( ! fIsFile && pNextPath )
         {
            pFilepath->szPath = pNextPath->szPath;
            hb_fsFNameMerge( szPath, pFilepath );
            fIsFile = hb_fsFileExists( szPath );
            pNextPath = pNextPath->pNext;
         }
      }
      if( ! fIsFile )
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
      while( ! fIsFile && pNextPath )
      {
         pFilepath->szPath = pNextPath->szPath;
         hb_fsFNameMerge( szPath, pFilepath );
         fIsFile = hb_fsFileExists( szPath );
         pNextPath = pNextPath->pNext;
      }
      hb_fsFreeSearchPath( pSearchPath );
      if( ! fIsFile )
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

HB_FHANDLE hb_fsExtOpen( const char * pszFileName, const char * pDefExt,
                         HB_FATTR nExFlags, const char * pPaths,
                         PHB_ITEM pError )
{
   HB_FHANDLE hFile;
   HB_USHORT uiFlags;
   const char * szPath;
   char * szFree = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsExtOpen(%s, %s, %u, %p, %p)", pszFileName, pDefExt, nExFlags, ( const void * ) pPaths, ( void * ) pError ) );

#if 0
   #define FXO_TRUNCATE   0x0100  /* Create (truncate if exists) */
   #define FXO_APPEND     0x0200  /* Create (append if exists) */
   #define FXO_UNIQUE     0x0400  /* Create unique file FO_EXCL ??? */
   #define FXO_FORCEEXT   0x0800  /* Force default extension */
   #define FXO_DEFAULTS   0x1000  /* Use SET command defaults */
   #define FXO_DEVICERAW  0x2000  /* Open devices in raw mode */
   /* Harbour extension */
   #define FXO_NOSEEKPOS FXO_DEVICERAW /* seek pos not needed in regular file */
   #define FXO_SHARELOCK  0x4000  /* emulate DOS SH_DENY* mode in POSIX OS */
   #define FXO_COPYNAME   0x8000  /* copy final szPath into pszFileName */

   hb_errGetFileName( pError );
#endif

   if( pDefExt || pPaths || pError ||
       ( nExFlags & ( FXO_DEFAULTS | FXO_COPYNAME ) ) != 0 )
      szPath = szFree = hb_fsExtName( pszFileName, pDefExt, nExFlags, pPaths );
   else
      szPath = pszFileName;

   uiFlags = ( HB_USHORT ) ( nExFlags & 0xff );
   if( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) )
   {
      uiFlags |= FO_CREAT;
      if( nExFlags & FXO_UNIQUE )
         uiFlags |= FO_EXCL;
#if defined( HB_USE_SHARELOCKS )
      else if( ( nExFlags & ( FXO_TRUNCATE | FXO_SHARELOCK ) ) == FXO_TRUNCATE )
#else
      else if( nExFlags & FXO_TRUNCATE )
#endif
         uiFlags |= FO_TRUNC;
   }

   hFile = hb_fsOpenEx( szPath, uiFlags, FC_NORMAL );

#if defined( HB_USE_SHARELOCKS )
   if( hFile != FS_ERROR && ( nExFlags & FXO_SHARELOCK ) != 0 )
   {
#if defined( HB_USE_BSDLOCKS )
      int iLock, iResult;
      if( /* ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ || */
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

      if( ! hb_fsLockLarge( hFile, HB_SHARELOCK_POS, HB_SHARELOCK_SIZE, uiLock ) )
#endif
      {
         hb_fsClose( hFile );
         hFile = FS_ERROR;
         /*
          * fix for NetErr() support and Clipper compatibility,
          * should be revised with a better multi platform solution.
          */
         hb_fsSetError( ( nExFlags & FXO_TRUNCATE ) ? 5 : 32 );
      }
      else if( nExFlags & FXO_TRUNCATE )
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
    * Temporary fix for NetErr() support and Clipper compatibility,
    * should be revised with a better solution.
    */
   if( ( nExFlags & ( FXO_TRUNCATE | FXO_APPEND | FXO_UNIQUE ) ) == 0 &&
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
         hb_errPutGenCode( pError, ( HB_ERRCODE ) ( ( nExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   if( nExFlags & FXO_COPYNAME && hFile != FS_ERROR )
      hb_strncpy( ( char * ) HB_UNCONST( pszFileName ), szPath, HB_PATH_MAX - 1 );

   if( szFree )
      hb_xfree( szFree );

   return hFile;
}

HB_BOOL hb_fsEof( HB_FHANDLE hFileHandle )
{
   HB_BOOL fResult;

   hb_vmUnlock();

#if defined( HB_OS_DOS ) && ! defined( __DJGPP__ )
   fResult = eof( hFileHandle ) != 0;
   hb_fsSetIOError( fResult, 0 );
#else
{
   HB_FOFFSET curPos;
   HB_FOFFSET endPos;

   curPos = hb_fsSeekLarge( hFileHandle, 0L, FS_RELATIVE );
   if( curPos != -1 )
   {
      HB_FOFFSET newPos;
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
   fResult = ! fResult || curPos >= endPos;
}
#endif

   hb_vmLock();

   return fResult;
}

const char * hb_fsNameConv( const char * pszFileName, char ** pszFree )
{
   int iFileCase, iDirCase;
   char cDirSep;
   HB_BOOL fTrim, fEncodeCP;

/*
   Convert file and dir case. The allowed SET options are:
      LOWER - Convert all characters of file to lower
      UPPER - Convert all characters of file to upper
      MIXED - Leave as is

   The allowed environment options are:
      FILECASE - define the case of file
      DIRCASE - define the case of path
      DIRSEPARATOR - define separator of path (Ex. "/")
      TRIMFILENAME - strip trailing and leading spaces (also from extension)
 */

   if( pszFree )
      *pszFree = NULL;

   if( ! hb_vmIsReady() )
      return pszFileName;

   fTrim = hb_setGetTrimFileName();
   fEncodeCP = hb_osUseCP();
   cDirSep = ( char ) hb_setGetDirSeparator();
   iFileCase = hb_setGetFileCase();
   iDirCase = hb_setGetDirCase();
   if( fTrim )
   {
      if( strchr( pszFileName, ' ' ) == NULL )
         fTrim = HB_FALSE;
   }
   if( cDirSep != HB_OS_PATH_DELIM_CHR )
   {
      if( strchr( pszFileName, ( HB_UCHAR ) cDirSep ) == NULL )
         cDirSep = HB_OS_PATH_DELIM_CHR;
   }

   if( fTrim || fEncodeCP ||
       cDirSep != HB_OS_PATH_DELIM_CHR ||
       iFileCase != HB_SET_CASE_MIXED ||
       iDirCase != HB_SET_CASE_MIXED )
   {
      PHB_FNAME pFileName;
      HB_SIZE nLen;
      char * pszPath = NULL, * pszName = NULL, * pszExt = NULL;

      if( pszFree )
      {
         pszFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                              pszFileName, HB_PATH_MAX - 1 );
      }

      if( cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) HB_UNCONST( pszFileName );
         while( *p )
         {
            if( *p == cDirSep )
               *p = HB_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( pszFileName );

      /* strip trailing and leading spaces */
      if( fTrim )
      {
         if( pFileName->szName )
         {
            nLen = strlen( pFileName->szName );
            nLen = hb_strRTrimLen( pFileName->szName, nLen, HB_FALSE );
            pFileName->szName = hb_strLTrim( pFileName->szName, &nLen );
            ( ( char * ) HB_UNCONST( pFileName->szName ) )[ nLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            nLen = strlen( pFileName->szExtension );
            nLen = hb_strRTrimLen( pFileName->szExtension, nLen, HB_FALSE );
            pFileName->szExtension = hb_strLTrim( pFileName->szExtension, &nLen );
            ( ( char * ) HB_UNCONST( pFileName->szExtension ) )[ nLen ] = '\0';
         }
      }

      /* FILECASE */
      if( iFileCase == HB_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = hb_cdpnDupLower( hb_vmCDP(), pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = hb_cdpnDupLower( hb_vmCDP(), pFileName->szExtension, NULL );
      }
      else if( iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = hb_cdpnDupUpper( hb_vmCDP(), pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = hb_cdpnDupUpper( hb_vmCDP(), pFileName->szExtension, NULL );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( iDirCase == HB_SET_CASE_LOWER )
            pFileName->szPath = pszPath = hb_cdpnDupLower( hb_vmCDP(), pFileName->szPath, NULL );
         else if( iDirCase == HB_SET_CASE_UPPER )
            pFileName->szPath = pszPath = hb_cdpnDupUpper( hb_vmCDP(), pFileName->szPath, NULL );
      }

      hb_fsFNameMerge( ( char * ) HB_UNCONST( pszFileName ), pFileName );
      hb_xfree( pFileName );
      if( pszPath )
         hb_xfree( pszPath );
      if( pszName )
         hb_xfree( pszName );
      if( pszExt )
         hb_xfree( pszExt );

      if( fEncodeCP )
      {
         const char * pszPrev = pszFileName;
         nLen = HB_PATH_MAX;
         pszFileName = hb_osEncodeCP( pszFileName, pszFree, &nLen );
         if( pszFree == NULL && pszFileName != pszPrev )
         {
            hb_strncpy( ( char * ) HB_UNCONST( pszPrev ), pszFileName, HB_PATH_MAX - 1 );
            hb_xfree( HB_UNCONST( pszFileName ) );
            pszFileName = pszPrev;
         }
      }
   }

   return pszFileName;
}

#if defined( HB_OS_WIN )
HB_WCHAR * hb_fsNameConvU16( const char * pszFileName )
{
   char * pszBuffer = NULL;
   HB_WCHAR * lpwFileName;
   HB_SIZE nLen;
   PHB_CODEPAGE cdp;
   int iFileCase, iDirCase;
   char cDirSep;
   HB_BOOL fTrim;

/*
   Convert file and dir case. The allowed SET options are:
      LOWER - Convert all characters of file to lower
      UPPER - Convert all characters of file to upper
      MIXED - Leave as is

   The allowed environment options are:
      FILECASE - define the case of file
      DIRCASE - define the case of path
      DIRSEPARATOR - define separator of path (Ex. "/")
      TRIMFILENAME - strip trailing and leading spaces (also from extension)
 */

   if( ! hb_vmIsReady() )
      return hb_mbtowc( pszFileName );  /* No HVM stack */

   cdp = hb_vmCDP();
   fTrim = hb_setGetTrimFileName();
   cDirSep = ( char ) hb_setGetDirSeparator();
   iFileCase = hb_setGetFileCase();
   iDirCase = hb_setGetDirCase();
   if( fTrim )
   {
      if( strchr( pszFileName, ' ' ) == NULL )
         fTrim = HB_FALSE;
   }
   if( cDirSep != HB_OS_PATH_DELIM_CHR )
   {
      if( strchr( pszFileName, ( HB_UCHAR ) cDirSep ) == NULL )
         cDirSep = HB_OS_PATH_DELIM_CHR;
   }

   if( fTrim ||
       cDirSep != HB_OS_PATH_DELIM_CHR ||
       iFileCase != HB_SET_CASE_MIXED ||
       iDirCase != HB_SET_CASE_MIXED )
   {
      char * pszPath = NULL, * pszName = NULL, * pszExt = NULL;
      PHB_FNAME pFileName;

      pszFileName = pszBuffer = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                            pszFileName, HB_PATH_MAX - 1 );

      if( cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = pszBuffer;
         while( *p )
         {
            if( *p == cDirSep )
               *p = HB_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( pszBuffer );

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
            pFileName->szName = pszName = hb_cdpnDupLower( cdp, pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = hb_cdpnDupLower( cdp, pFileName->szExtension, NULL );
      }
      else if( iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            pFileName->szName = pszName = hb_cdpnDupUpper( cdp, pFileName->szName, NULL );
         if( pFileName->szExtension )
            pFileName->szExtension = pszExt = hb_cdpnDupUpper( cdp, pFileName->szExtension, NULL );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( iDirCase == HB_SET_CASE_LOWER )
            pFileName->szPath = pszPath = hb_cdpnDupLower( cdp, pFileName->szPath, NULL );
         else if( iDirCase == HB_SET_CASE_UPPER )
            pFileName->szPath = pszPath = hb_cdpnDupUpper( cdp, pFileName->szPath, NULL );
      }

      hb_fsFNameMerge( pszBuffer, pFileName );
      hb_xfree( pFileName );
      if( pszPath )
         hb_xfree( pszPath );
      if( pszName )
         hb_xfree( pszName );
      if( pszExt )
         hb_xfree( pszExt );
   }

   lpwFileName = hb_cdpStrDupU16( cdp, HB_CDP_ENDIAN_NATIVE, pszFileName );
   if( pszBuffer )
      hb_xfree( pszBuffer );

   return lpwFileName;
}
#endif /* HB_OS_WIN */

/* NOTE: pszBuffer must be HB_PATH_MAX long. */
void hb_fsBaseDirBuff( char * pszBuffer )
{
   char * pszBaseName = hb_cmdargProgName();

   if( pszBaseName )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( pszBaseName );
      pFileName->szName = NULL;
      pFileName->szExtension = NULL;
      hb_fsFNameMerge( pszBuffer, pFileName );
      hb_xfree( pFileName );
      hb_xfree( pszBaseName );
   }
   else
      pszBuffer[ 0 ] = '\0';
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
