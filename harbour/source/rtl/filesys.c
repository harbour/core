/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API (C level)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
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
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_fsSetError()
 *    hb_fsSetDevMode()
 *    hb_fsReadLarge()
 *    hb_fsWriteLarge()
 *    hb_fsCurDirBuff()
 *    hb_fsBaseDirBuff()
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
#  define _LARGEFILE64_SOURCE
#endif
#if !defined( _GNU_SOURCE )
#  define _GNU_SOURCE
#endif

/* OS2 */
#define INCL_DOSFILEMGR   /* File Manager values */
#define INCL_DOSERRORS    /* DOS error values    */
#define INCL_DOSDATETIME  /* DATETIME functions  */

/* Windows */
#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hb_io.h"
#include "hbset.h"

#if defined(HB_OS_UNIX_COMPATIBLE)
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
   #include <sys/types.h>
   #include <sys/wait.h>
   #if defined( HB_OS_LINUX ) && !defined( __WATCOMC__ )
      #include <sys/time.h>
   #endif
#endif

#if ( defined(__DMC__) || defined(__BORLANDC__) || \
      defined(__IBMCPP__) || defined(_MSC_VER) || \
      defined(__MINGW32__) || defined(__WATCOMC__) ) && \
      !defined( HB_OS_UNIX ) && !defined( HB_OS_WIN_CE )
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <process.h>
   #if !defined( __POCC__ ) && !defined( __XCC__ )
      #include <share.h>
   #endif
   #include <direct.h>
   #if defined(__BORLANDC__)
      #include <dir.h>
      #include <dos.h>
   #elif defined(__WATCOMC__)
      #include <dos.h>
   #endif

   #if defined(_MSC_VER) || defined(__MINGW32__) || defined(__DMC__)
      #include <sys/locking.h>
      #define ftruncate _chsize
      #if defined(__MINGW32__) && !defined(_LK_UNLCK)
         #define _LK_UNLCK _LK_UNLOCK
      #endif
   #else
      #define ftruncate chsize
   #endif
   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif
#elif defined(__GNUC__) || defined(HB_OS_UNIX)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #if defined(__CYGWIN__)
      #include <io.h>
   #elif defined(__DJGPP__)
      #include <dir.h>
      #include <utime.h>
      #include <time.h>
   #endif
   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif
#endif

#if defined(__MPW__)
   #include <fcntl.h>
#endif

#if defined(HB_OS_DOS)
   #include <dos.h>
   #include <time.h>
   #include <utime.h>
#elif defined(HB_OS_OS2)
   #include <time.h>
   #include <share.h>
   #ifndef SH_COMPAT
      #define SH_COMPAT    0x0000
   #endif
#elif defined( HB_IO_WIN )
   #include <windows.h>

   #if !defined( INVALID_SET_FILE_POINTER ) && \
       ( defined(__DMC__) || defined( _MSC_VER ) || defined( __LCC__ ) )
      #define INVALID_SET_FILE_POINTER ( ( DWORD ) -1 )
   #endif
   #if !defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES     ( ( DWORD ) -1 )
   #endif
#endif
#if defined( HB_USE_SHARELOCKS ) && defined( HB_USE_BSDLOCKS )
   #include <sys/file.h>
#endif

#if !defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX_COMPATIBLE )
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

#if defined(HB_OS_HAS_DRIVE_LETTER)
/* 27/08/2004 - <maurilio.longo@libero.it>
                HB_FS_GETDRIVE() should return a number in the range 0..25 ('A'..'Z')
                HB_FS_SETDRIVE() should accept a number inside same range.

                If a particular platform/compiler returns/accepts different ranges of
                values, simply define a branch for that platform.

                NOTE: There is not an implicit "current disk", ALWAYS use

                        my_func( hb_fsCurDrv(), ...)

                      to refer to current disk
*/

#if defined( __DJGPP__ )
   #define HB_FS_GETDRIVE(n)  do { n = getdisk(); } while( 0 )
   #define HB_FS_SETDRIVE(n)  setdisk( n )

#elif defined( __WATCOMC__ )
   #define HB_FS_GETDRIVE(n)  do { _dos_getdrive( &( n ) ); --( n ); } while( 0 )
   #define HB_FS_SETDRIVE(n)  do { \
                                 UINT uiDummy; \
                                 _dos_setdrive( ( n ) + 1, &uiDummy ); \
                              } while( 0 )

#elif defined(HB_OS_OS2)
   #define HB_FS_GETDRIVE(n)  do { n = _getdrive() - 'A'; } while( 0 )
   #define HB_FS_SETDRIVE(n)  _chdrive( ( n ) + 'A' )

#else
   #define HB_FS_GETDRIVE(n)  do { \
                                 n = _getdrive(); \
                                 n -= ( ( n ) < 'A' ) ? 1 : 'A'; \
                              } while( 0 )
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


#if defined(HAVE_POSIX_IO) || defined( HB_IO_WIN ) || defined(_MSC_VER) || defined(__MINGW32__) || defined(__LCC__) || defined(__DMC__)
/* Only compilers with Posix or Posix-like I/O support are supported */
   #define HB_FS_FILE_IO
#endif

#if defined(__DMC__) || defined(_MSC_VER) || defined(__MINGW32__) || defined(__IBMCPP__) || defined(__WATCOMC__) || defined(HB_OS_OS2)
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define HB_FS_SOPEN
#endif

#if UINT_MAX == USHRT_MAX
   #define LARGE_MAX ( UINT_MAX - 1L )
#else
   #define HB_FS_LARGE_OPTIMIZED
#endif

static BOOL s_fUseWaitLocks = TRUE;

#if defined(HB_FS_FILE_IO)

#if defined(HB_IO_WIN)

static HANDLE DosToWinHandle( HB_FHANDLE fHandle )
{
   if( fHandle == ( HB_FHANDLE ) HB_STDIN_HANDLE )
      return GetStdHandle( STD_INPUT_HANDLE );

   else if( fHandle == ( HB_FHANDLE ) HB_STDOUT_HANDLE )
      return GetStdHandle( STD_OUTPUT_HANDLE );

   else if( fHandle == ( HB_FHANDLE ) HB_STDERR_HANDLE )
      return GetStdHandle( STD_ERROR_HANDLE );

   else
      return ( HANDLE ) fHandle;
}

static void convert_open_flags( BOOL fCreate, ULONG ulAttr, USHORT uiFlags,
                                DWORD *dwMode, DWORD *dwShare,
                                DWORD *dwCreat, DWORD *dwAttr )
{
   if( fCreate )
   {
      *dwCreat = CREATE_ALWAYS;
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
      {
         *dwCreat = TRUNCATE_EXISTING;
      }
      else
      {
         *dwCreat = OPEN_EXISTING;
      }

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

static void convert_open_flags( BOOL fCreate, ULONG ulAttr, USHORT uiFlags,
                                int *flags, unsigned *mode,
                                int *share, int *attr )
{
   HB_TRACE(HB_TR_DEBUG, ("convert_open_flags(%d, %lu, %hu, %p, %p, %p, %p)", fCreate, ulAttr, uiFlags, flags, mode, share, attr));

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

static USHORT convert_seek_flags( USHORT uiFlags )
{
   /* by default FS_SET is set */
   USHORT result_flags = SEEK_SET;

   HB_TRACE(HB_TR_DEBUG, ("convert_seek_flags(%hu)", uiFlags));

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}

#endif


/*
 * filesys.api functions:
 */

HB_FHANDLE hb_fsGetOsHandle( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetOsHandle(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

#if defined(HB_IO_WIN)
   return ( HB_FHANDLE ) DosToWinHandle( hFileHandle );
#else
   return hFileHandle;
#endif
}

HB_FHANDLE hb_fsPOpen( const char * pFilename, const char * pMode )
{
   HB_FHANDLE hFileHandle = FS_ERROR;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsPOpen(%p, %s)", pFilename, pMode));

#if defined(HB_OS_UNIX_COMPATIBLE) && !defined(__CYGWIN__)
   {
      HB_FHANDLE hPipeHandle[2], hNullHandle;
      pid_t pid;
      char * pszTmp;
      BOOL bRead;
      ULONG ulLen;
      int iMaxFD;

      ulLen = strlen( pFilename );
      if( pMode && ( *pMode == 'r' || *pMode == 'w' ) )
         bRead = ( *pMode == 'r' );
      else
      {
         if( pFilename[0] == '|' )
            bRead = FALSE;
         else if( pFilename[ ulLen - 1 ] == '|' )
            bRead = TRUE;
         else
            bRead = FALSE;
      }

      if( pFilename[0] == '|' )
      {
          ++pFilename;
          --ulLen;
      }
      if( pFilename[ ulLen - 1 ] == '|' )
      {
          pszTmp = hb_strdup( pFilename );
          pszTmp[--ulLen] = 0;
          pFilename = pszTmp;
      } else
          pszTmp = NULL;

      hb_vmUnlock();
      if( pipe( hPipeHandle ) == 0 ) {
         if( ( pid = fork() ) != -1 ) {
            if( pid != 0 ) {
               if( bRead ) {
                  close( hPipeHandle[ 1 ] );
                  hFileHandle = hPipeHandle[ 0 ];
               } else {
                  close( hPipeHandle[ 0 ] );
                  hFileHandle = hPipeHandle[ 1 ];
               }
            } else {
               const char * argv[4];
               argv[0] = "sh";
               argv[1] = "-c";
               argv[2] = pFilename;
               argv[3] = 0;
               hNullHandle = open( "/dev/null", O_RDWR );
               if( bRead ) {
                  close( hPipeHandle[ 0 ] );
                  dup2( hPipeHandle[ 1 ], 1 );
                  dup2( hNullHandle, 0 );
                  dup2( hNullHandle, 2 );
               } else {
                  close( hPipeHandle[ 1 ] );
                  dup2( hPipeHandle[ 0 ], 0 );
                  dup2( hNullHandle, 1 );
                  dup2( hNullHandle, 2 );
               }
               iMaxFD = sysconf( _SC_OPEN_MAX );
               if( iMaxFD < 3 )
                  iMaxFD = 1024;
               for( hNullHandle = 3; hNullHandle < iMaxFD; ++hNullHandle )
                  close( hNullHandle );
               setuid( getuid() );
               setgid( getgid() );
               execv( "/bin/sh", ( char ** ) argv );
               exit(1);
            }
         }
         else
         {
            close( hPipeHandle[0] );
            close( hPipeHandle[1] );
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

   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return hFileHandle;
}

HB_FHANDLE hb_fsOpen( const char * pFilename, USHORT uiFlags )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpen(%s, %hu)", pFilename, uiFlags));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined(HB_IO_WIN)
   {
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( FALSE, FC_NORMAL, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = ( HANDLE ) CreateFileA( pFilename, dwMode, dwShare,
                                      NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#elif defined(HB_FS_FILE_IO)
   {
      int flags, share, attr;
      unsigned mode;

      convert_open_flags( FALSE, FC_NORMAL, uiFlags, &flags, &mode, &share, &attr );
      hb_vmUnlock();
#if defined(_MSC_VER) || defined(__DMC__)
      if( share )
         hFileHandle = _sopen( pFilename, flags, share, mode );
      else
         hFileHandle = _open( pFilename, flags, mode );
#elif defined(HB_FS_SOPEN)
      if( share )
         hFileHandle = sopen( pFilename, flags, share, mode );
      else
         hFileHandle = open( pFilename, flags, mode );
#else
      hFileHandle = open( pFilename, flags | share, mode );
#endif
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      hb_vmLock();
   }
#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

HB_FHANDLE hb_fsCreate( const char * pFilename, ULONG ulAttr )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%s, %lu)", pFilename, ulAttr));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined(HB_IO_WIN)
   {
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( TRUE, ulAttr, FO_EXCLUSIVE, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = ( HANDLE ) CreateFileA( pFilename, dwMode, dwShare,
                                      NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#elif defined(HB_FS_FILE_IO)
   {
      int flags, share, attr;
      unsigned mode;
      convert_open_flags( TRUE, ulAttr, FO_EXCLUSIVE, &flags, &mode, &share, &attr );

      hb_vmUnlock();
#if defined(HB_FS_DOSCREAT)
      hFileHandle = _creat( pFilename, attr );
#elif defined(HB_FS_SOPEN)
      hFileHandle = open( pFilename, flags, mode );
#else
      hFileHandle = open( pFilename, flags | share, mode );
#endif
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      hb_vmLock();
   }
#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

/* Derived from hb_fsCreate()

   NOTE: The default opening mode differs from the one used in hb_fsCreate()
         [vszakats]
 */

HB_FHANDLE hb_fsCreateEx( const char * pFilename, ULONG ulAttr, USHORT uiFlags )
{
   HB_FHANDLE hFileHandle;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreateEx(%s, %lu, %hu)", pFilename, ulAttr, uiFlags));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined( HB_IO_WIN )
   {
      DWORD dwMode, dwShare, dwCreat, dwAttr;
      HANDLE hFile;

      convert_open_flags( TRUE, ulAttr, uiFlags, &dwMode, &dwShare, &dwCreat, &dwAttr );

      hb_vmUnlock();
      hFile = ( HANDLE ) CreateFileA( pFilename, dwMode, dwShare,
                                      NULL, dwCreat, dwAttr, NULL );
      hb_fsSetIOError( hFile != ( HANDLE ) INVALID_HANDLE_VALUE, 0 );
      hb_vmLock();

      hFileHandle = ( HB_FHANDLE ) hFile;
   }
#elif defined(HB_FS_FILE_IO)
   {
      int flags, share, attr;
      unsigned mode;
      convert_open_flags( TRUE, ulAttr, uiFlags, &flags, &mode, &share, &attr );

      hb_vmUnlock();
#if defined(HB_FS_SOPEN)
      hFileHandle = open( pFilename, flags, mode );
#else
      hFileHandle = open( pFilename, flags | share, mode );
#endif
      hb_fsSetIOError( hFileHandle != FS_ERROR, 0 );
      hb_vmLock();
   }
#else

   hFileHandle = FS_ERROR;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return hFileHandle;
}

void hb_fsClose( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

#if defined(HB_FS_FILE_IO)

   hb_vmUnlock();
   #if defined(HB_IO_WIN)
      hb_fsSetIOError( CloseHandle( DosToWinHandle( hFileHandle ) ), 0 );
   #else
      hb_fsSetIOError( close( hFileHandle ) == 0, 0 );
   #endif
   hb_vmLock();

#else

   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif
}

BOOL hb_fsSetDevMode( HB_FHANDLE hFileHandle, USHORT uiDevMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetDevMode(%p, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, uiDevMode));

   /* TODO: HB_IO_WIN support */

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__DJGPP__) || \
    defined(__CYGWIN__) || defined(__WATCOMC__) || defined(HB_OS_OS2)
{
   int iRet = 0;

#if defined(HB_IO_WIN)
   if( hFileHandle != ( HB_FHANDLE ) 0 &&
       hFileHandle != ( HB_FHANDLE ) 1 &&
       hFileHandle != ( HB_FHANDLE ) 2 )
      iRet = -1;
   else
#endif
   switch( uiDevMode )
   {
      case FD_BINARY:
         iRet = setmode( ( HB_NHANDLE ) hFileHandle, O_BINARY );
         break;

      case FD_TEXT:
         iRet = setmode( ( HB_NHANDLE ) hFileHandle, O_TEXT );
         break;
   }

   hb_fsSetIOError( iRet != -1, 0 );

   return iRet != -1;
}
#elif ( defined(_MSC_VER) || defined(__MINGW32__) || defined(__DMC__) ) && \
      !defined(HB_OS_WIN_CE)
{
   int iRet = 0;

#if defined(HB_IO_WIN)
   if( hFileHandle != ( HB_FHANDLE ) 0 &&
       hFileHandle != ( HB_FHANDLE ) 1 &&
       hFileHandle != ( HB_FHANDLE ) 2 )
      iRet = -1;
   else
#endif
   switch( uiDevMode )
   {
      case FD_BINARY:
         iRet = _setmode( ( HB_NHANDLE ) hFileHandle, _O_BINARY );
         break;

      case FD_TEXT:
         iRet = _setmode( ( HB_NHANDLE ) hFileHandle, _O_TEXT );
         break;
   }

   hb_fsSetIOError( iRet != -1, 0 );

   return iRet != -1;
}
#elif defined( HB_OS_UNIX ) || defined( HB_OS_WIN_CE )

   HB_SYMBOL_UNUSED( hFileHandle );

   if( uiDevMode == FD_TEXT )
   {
      hb_fsSetError( ( USHORT ) FS_ERROR );
      return FALSE;
   }

   hb_fsSetError( 0 );
   return TRUE;

#else

   hb_fsSetError( ( USHORT ) FS_ERROR );
   return FALSE;

#endif
}

BOOL hb_fsGetFileTime( const char * pszFileName, long * plJulian, long * plMillisec )
{
   BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetFileTime(%s, %p, %p)", pszFileName, plJulian, plMillisec));

   fResult = FALSE;

#if defined( HB_IO_WIN )
   {
      HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READ | FO_SHARED );

      if( hFile != FS_ERROR )
      {
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         hb_vmUnlock();
         if( GetFileTime( DosToWinHandle( hFile ), NULL, NULL, &ft ) &&
             FileTimeToLocalFileTime( &ft, &local_ft ) &&
             FileTimeToSystemTime( &local_ft, &st ) )
         {
            *plJulian = hb_dateEncode( st.wYear, st.wMonth, st.wDay );
            *plMillisec = hb_timeEncode( st.wHour, st.wMinute, st.wSecond, st.wMilliseconds );

            fResult = TRUE;
         }
         hb_fsSetIOError( fResult, 0 );
         hb_vmLock();
         hb_fsClose( hFile );
      }
   }
#elif defined( HB_OS_UNIX ) || defined( HB_OS_OS2 ) || defined( HB_OS_DOS ) || defined( __GNUC__ )
   {
      struct stat sStat;
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      hb_vmUnlock();
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
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      int TODO; /* TODO: for given platform */

      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( plJulian );
      HB_SYMBOL_UNUSED( plMillisec );
   }
#endif

   return fResult;
}

BOOL hb_fsGetAttr( const char * pszFileName, ULONG * pulAttr )
{
   BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsGetAttr(%s, %p)", pszFileName, pulAttr));

   *pulAttr = 0;
   fResult = FALSE;
   pszFileName = hb_fsNameConv( pszFileName, &pszFree );

#if defined( HB_OS_WIN )
   {
      DWORD dwAttr;

      hb_vmUnlock();
      dwAttr = GetFileAttributesA( pszFileName );

      if( dwAttr != INVALID_FILE_ATTRIBUTES )
      {
         *pulAttr = hb_fsAttrFromRaw( dwAttr );
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#elif defined( HB_OS_DOS )
   hb_vmUnlock();
   {
#if defined( __DJGPP__ ) || defined(__BORLANDC__)
      int attr = _chmod( pszFileName, 0, 0 );
      if( attr != -1 )
#else
      unsigned int attr = 0;
      if( _dos_getfileattr( pszFileName, &attr ) == 0 )
#endif
      {
         *pulAttr = hb_fsAttrFromRaw( attr );
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
   }
   hb_vmLock();
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ulrc;

      hb_vmUnlock();
      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         *pulAttr = hb_fsAttrFromRaw( fs3.attrFile );
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#elif defined( HB_OS_UNIX )
   {
      struct stat sStat;

      hb_vmUnlock();
      if( stat( pszFileName, &sStat ) == 0 )
      {
         *pulAttr = hb_fsAttrFromRaw( sStat.st_mode );
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#else
   {
      int TODO; /* TODO: for given platform */

      HB_SYMBOL_UNUSED( pszFileName );
      HB_SYMBOL_UNUSED( pulAttr );
   }
#endif

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

BOOL hb_fsSetFileTime( const char * pszFileName, long lJulian, long lMillisec )
{
   BOOL fResult;
   int iYear, iMonth, iDay;
   int iHour, iMinute, iSecond, iMSec;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetFileTime(%s, %ld, %ld)", pszFileName, lJulian, lMillisec));

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   hb_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

#if defined( HB_OS_WIN ) && !defined( __CYGWIN__ )
   {
      HB_FHANDLE hFile = hb_fsOpen( pszFileName, FO_READWRITE | FO_SHARED );

      fResult = hFile != FS_ERROR;
      if( fResult )
      {
         FILETIME ft, local_ft;
         SYSTEMTIME st;

         hb_vmUnlock();
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
         hb_vmLock();
         hb_fsClose( hFile );
      }
   }
#elif defined( HB_OS_OS2 )
   {
      FILESTATUS3 fs3;
      APIRET ulrc;
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      hb_vmUnlock();
      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         FDATE fdate;
         FTIME ftime;

         if( lJulian <= 0 || lMillisec < 0 )
         {
            DATETIME dt;

            DosGetDateTime( &dt );

            fdate.year = dt.year - 1980;
            fdate.month = dt.month;
            fdate.day = dt.day;
            ftime.hours = dt.hours;
            ftime.minutes = dt.minutes;
            ftime.twosecs = dt.seconds / 2;
         }
         if( lJulian > 0 )
         {
            fdate.year = iYear - 1980;
            fdate.month = iMonth;
            fdate.day = iDay;
         }
         if( lMillisec >= 0 )
         {
            ftime.hours = iHour;
            ftime.minutes = iMinute;
            ftime.twosecs = iSecond / 2;
         }

         fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
         fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
         ulrc = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }
      fResult = ulrc == NO_ERROR;
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
      if( pszFree )
         hb_xfree( pszFree );
   }
#elif defined( HB_OS_UNIX_COMPATIBLE ) || defined( HB_OS_DOS )
   {
      char * pszFree;

      pszFileName = hb_fsNameConv( pszFileName, &pszFree );

      hb_vmUnlock();
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
            struct timeval times[2];
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
      hb_vmLock();
      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   {
      int TODO; /* To force warning */

      fResult = FALSE;
      hb_fsSetError( ( USHORT ) FS_ERROR );
   }
#endif

   return fResult;
}

BOOL hb_fsSetAttr( const char * pszFileName, ULONG ulAttr )
{
   BOOL fResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetAttr(%s, %lu)", pszFileName, ulAttr));

   pszFileName = hb_fsNameConv( pszFileName, &pszFree );

#if defined( HB_OS_WIN )
   {
      DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;

      if( ulAttr & HB_FA_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;
      if( ulAttr & HB_FA_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;
      if( ulAttr & HB_FA_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;
      if( ulAttr & HB_FA_NORMAL )
         dwFlags |= FILE_ATTRIBUTE_NORMAL;
      hb_vmUnlock();
      fResult = SetFileAttributesA( pszFileName, dwFlags );
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
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

      hb_vmUnlock();
      ulrc = DosQueryPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD, &fs3, sizeof( fs3 ) );
      if( ulrc == NO_ERROR )
      {
         fs3.attrFile = ulOsAttr;
         ulrc = DosSetPathInfo( ( PCSZ ) pszFileName, FIL_STANDARD,
                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }
      fResult = ulrc == NO_ERROR;
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#elif defined( HB_OS_DOS )

   ulAttr &= ~( HB_FA_ARCHIVE | HB_FA_HIDDEN | HB_FA_READONLY | HB_FA_SYSTEM );
   hb_vmUnlock();
#  if defined( __DJGPP__ ) || defined( __BORLANDC__ )
   fResult = _chmod( pszFileName, 1, ulAttr ) != -1;
#  else
   fResult = _dos_setfileattr( pszFileName, ulAttr ) != -1;
#  endif
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#elif defined( HB_OS_UNIX_COMPATIBLE )
   {
      int iAttr = HB_FA_POSIX_ATTR( ulAttr );
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
      hb_vmUnlock();
      fResult = chmod( pszFileName, iAttr ) != -1;
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }
#else
   {
      int TODO; /* To force warning */

      fResult = FALSE;
      hb_fsSetError( ( USHORT ) FS_ERROR );
   }
#endif

   if( pszFree )
      hb_xfree( pszFree );

   return fResult;
}

USHORT hb_fsRead( HB_FHANDLE hFileHandle, void * pBuff, USHORT uiCount )
{
   USHORT uiRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRead(%p, %p, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_IO_WIN)
      {
         DWORD dwRead;
         BOOL fResult;

         hb_vmUnlock();
         fResult = ReadFile( DosToWinHandle( hFileHandle ), pBuff, ( DWORD ) uiCount, &dwRead, NULL );
         hb_fsSetIOError( fResult, 0 );
         hb_vmLock();

         uiRead = fResult ? ( USHORT ) dwRead : 0;
      }
   #else
      hb_vmUnlock();
      uiRead = read( hFileHandle, pBuff, uiCount );
      hb_fsSetIOError( uiRead != ( USHORT ) -1, 0 );
      hb_vmLock();
   #endif

   if( uiRead == ( USHORT ) -1 )
      uiRead = 0;

#else

   uiRead = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return uiRead;
}

USHORT hb_fsWrite( HB_FHANDLE hFileHandle, const void * pBuff, USHORT uiCount )
{
   USHORT uiWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWrite(%p, %p, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_IO_WIN)
      {
         DWORD dwWritten = 0;
         BOOL fResult;

         hb_vmUnlock();
         if( uiCount )
         {
             fResult = WriteFile( DosToWinHandle( hFileHandle ), pBuff, uiCount, &dwWritten, NULL );
         }
         else
         {
             dwWritten = 0;
             fResult = SetEndOfFile( DosToWinHandle( hFileHandle ) );
         }
         hb_fsSetIOError( fResult, 0 );
         hb_vmLock();

         uiWritten = fResult ? ( USHORT ) dwWritten : 0;
      }
   #else
      if( uiCount )
      {
         hb_vmUnlock();
         uiWritten = write( hFileHandle, pBuff, uiCount );
         hb_fsSetIOError( uiWritten != ( USHORT ) -1, 0 );
         if( uiWritten == ( USHORT ) -1 )
            uiWritten = 0;
         hb_vmLock();
      }
      else
      {
         hb_vmUnlock();
#if defined(HB_USE_LARGEFILE64)
         hb_fsSetIOError( ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) != -1, 0 );
#else
         hb_fsSetIOError( ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) != -1, 0 );
#endif
         uiWritten = 0;
         hb_vmLock();
      }
   #endif
#else

   uiWritten = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return uiWritten;
}

ULONG hb_fsReadLarge( HB_FHANDLE hFileHandle, void * pBuff, ULONG ulCount )
{
   ULONG ulRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLarge(%p, %p, %lu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_IO_WIN)
   {
      hb_vmUnlock();
      hb_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                 pBuff, ulCount, &ulRead, NULL ), 0 );
      hb_vmLock();
   }
   #elif defined(HB_FS_LARGE_OPTIMIZED)
   {
      hb_vmUnlock();
      ulRead = read( hFileHandle, pBuff, ulCount );
      hb_fsSetIOError( ulRead != ( ULONG ) -1, 0 );
      if( ulRead == ( ULONG ) -1 )
         ulRead = 0;
      hb_vmLock();
   }
   #else
   {
      ULONG ulLeftToRead = ulCount;
      USHORT uiToRead;
      USHORT uiRead;
      BYTE * pPtr = ( BYTE * ) pBuff;

      ulRead = 0;

      hb_vmUnlock();
      while( ulLeftToRead )
      {
         /* Determine how much to read this time */
         if( ulLeftToRead > ( ULONG ) INT_MAX )
         {
            uiToRead = INT_MAX;
            ulLeftToRead -= ( ULONG ) uiToRead;
         }
         else
         {
            uiToRead = ( USHORT ) ulLeftToRead;
            ulLeftToRead = 0;
         }

         uiRead = read( hFileHandle, pPtr, uiToRead );
         /* -1 on bad hFileHandle
             0 on disk full
          */

         if( uiRead == 0 )
            break;

         if( uiRead == ( USHORT ) -1 )
         {
            uiRead = 0;
            break;
         }

         ulRead += ( ULONG ) uiRead;
         pPtr += uiRead;
      }
      hb_fsSetIOError( ulLeftToRead == 0, 0 );
      hb_vmLock();
   }
   #endif

#else

   ulRead = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return ulRead;
}

ULONG hb_fsWriteLarge( HB_FHANDLE hFileHandle, const void * pBuff, ULONG ulCount )
{
   ULONG ulWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteLarge(%p, %p, %lu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_IO_WIN)
   {
      ulWritten = 0;
      hb_vmUnlock();
      if( ulCount )
      {
         hb_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle), pBuff, ulCount, &ulWritten, NULL ), 0 );
      }
      else
      {
         hb_fsSetIOError( SetEndOfFile( DosToWinHandle( hFileHandle ) ), 0 );
      }
      hb_vmLock();
   }
   #else
      if( ulCount )
      #if defined(HB_FS_LARGE_OPTIMIZED)
         {
            hb_vmUnlock();
            ulWritten = write( hFileHandle, pBuff, ulCount );
            hb_fsSetIOError( ulWritten != ( ULONG ) -1, 0 );
            if( ulWritten == ( ULONG ) -1 )
               ulWritten = 0;
            hb_vmLock();
         }
      #else
         {
            ULONG ulLeftToWrite = ulCount;
            USHORT uiToWrite;
            USHORT uiWritten;
            const BYTE * pPtr = ( const BYTE * ) pBuff;

            ulWritten = 0;

            hb_vmUnlock();
            while( ulLeftToWrite )
            {
               /* Determine how much to write this time */
               if( ulLeftToWrite > ( ULONG ) INT_MAX )
               {
                  uiToWrite = INT_MAX;
                  ulLeftToWrite -= ( ULONG ) uiToWrite;
               }
               else
               {
                  uiToWrite = ( USHORT ) ulLeftToWrite;
                  ulLeftToWrite = 0;
               }

               uiWritten = write( hFileHandle, pPtr, uiToWrite );

               /* -1 on bad hFileHandle
                   0 on disk full
                */

               if( uiWritten == 0 )
                  break;

               if( uiWritten == ( USHORT ) -1 )
               {
                  uiWritten = 0;
                  break;
               }

               ulWritten += ( ULONG ) uiWritten;
               pPtr += uiWritten;
            }
            hb_fsSetIOError( ulLeftToWrite == 0, 0 );
            hb_vmLock();
         }
      #endif
      else
      {
         hb_vmUnlock();
#if defined(HB_USE_LARGEFILE64)
         hb_fsSetIOError( ftruncate64( hFileHandle, lseek64( hFileHandle, 0L, SEEK_CUR ) ) != -1, 0 );
#else
         hb_fsSetIOError( ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) ) != -1, 0 );
#endif
         ulWritten = 0;
         hb_vmLock();
      }

   #endif

#else

   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return ulWritten;
}

ULONG hb_fsReadAt( HB_FHANDLE hFileHandle, void * pBuff, ULONG ulCount, HB_FOFFSET llOffset )
{
   ULONG ulRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadAt(%p, %p, %lu, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, ulCount, llOffset));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_OS_UNIX) && !defined(__WATCOMC__)
   {
      hb_vmUnlock();
      #if defined(HB_USE_LARGEFILE64)
         ulRead = pread64( hFileHandle, pBuff, ulCount, llOffset );
      #else
         ulRead = pread( hFileHandle, pBuff, ulCount, llOffset );
      #endif
      hb_fsSetIOError( ulRead != ( ULONG ) -1, 0 );
      if( ulRead == ( ULONG ) -1 )
         ulRead = 0;
      hb_vmLock();
   }
   #else
   #if defined(HB_IO_WIN)
   if( hb_iswinnt() )
   {
      OVERLAPPED Overlapped;
      hb_vmUnlock();
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( llOffset & 0xFFFFFFFF ),
      Overlapped.OffsetHigh = ( DWORD ) ( llOffset >> 32 ),
      hb_fsSetIOError( ReadFile( DosToWinHandle( hFileHandle ),
                                 pBuff, ulCount, &ulRead, &Overlapped ), 0 );
      hb_vmLock();
   }
   else
   #endif
   {
      hb_vmUnlock();
      /* TOFIX: this is not atom operation. It has to be fixed for RDD
       *        file access with shared file handles in aliased work areas
       */
      if( hb_fsSeekLarge( hFileHandle, llOffset, FS_SET ) == llOffset )
         ulRead = hb_fsReadLarge( hFileHandle, pBuff, ulCount );
      else
         ulRead = 0;
      hb_vmLock();
   }
   #endif

#else

   ulRead = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return ulRead;
}

ULONG hb_fsWriteAt( HB_FHANDLE hFileHandle, const void * pBuff, ULONG ulCount, HB_FOFFSET llOffset )
{
   ULONG ulWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteAt(%p, %p, %lu, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, pBuff, ulCount, llOffset));

#if defined(HB_FS_FILE_IO)

   #if defined(HB_OS_UNIX) && !defined(__WATCOMC__)
   {
      hb_vmUnlock();
      #if defined(HB_USE_LARGEFILE64)
         ulWritten = pwrite64( hFileHandle, pBuff, ulCount, llOffset );
      #else
         ulWritten = pwrite( hFileHandle, pBuff, ulCount, llOffset );
      #endif
      hb_fsSetIOError( ulWritten != ( ULONG ) -1, 0 );
      if( ulWritten == ( ULONG ) -1 )
         ulWritten = 0;
      hb_vmLock();
   }
   #else
   #if defined(HB_IO_WIN)
   if( hb_iswinnt() )
   {
      OVERLAPPED Overlapped;
      hb_vmUnlock();
      memset( &Overlapped, 0, sizeof( Overlapped ) );
      Overlapped.Offset     = ( DWORD ) ( llOffset & 0xFFFFFFFF ),
      Overlapped.OffsetHigh = ( DWORD ) ( llOffset >> 32 ),
      hb_fsSetIOError( WriteFile( DosToWinHandle( hFileHandle ),
                                  pBuff, ulCount, &ulWritten, &Overlapped ), 0 );
      hb_vmLock();
   }
   else
   #endif
   {
      hb_vmUnlock();
      /* TOFIX: this is not atom operation. It has to be fixed for RDD
       *        file access with shared file handles in aliased work areas
       */
      if( hb_fsSeekLarge( hFileHandle, llOffset, FS_SET ) == llOffset )
         ulWritten = hb_fsWriteLarge( hFileHandle, pBuff, ulCount );
      else
         ulWritten = 0;
      hb_vmLock();
   }
   #endif

#else

   ulWritten = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return ulWritten;
}

BOOL hb_fsTruncAt( HB_FHANDLE hFileHandle, HB_FOFFSET llOffset )
{
   BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadAt(%p, %" PFHL "i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, llOffset));

#if defined(HB_FS_FILE_IO)

   hb_vmUnlock();
   #if defined(HB_IO_WIN)
   {
      ULONG ulOffsetLow  = ( ULONG ) ( llOffset & ULONG_MAX ),
            ulOffsetHigh = ( ULONG ) ( llOffset >> 32 );

      /* This is not atom operation anyhow if someone want to truncate
       * file then he has to made necessary synchronizations in upper level
       * code. We have such situation in our RDD drivers and for us such
       * version is enough. [druzus]
       */
      ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                    ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                    ( DWORD ) SEEK_SET );
      if( ( ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow ) == llOffset )
         fResult = SetEndOfFile( DosToWinHandle( hFileHandle ) );
      else
         fResult = FALSE;
   }
   #elif defined(HB_USE_LARGEFILE64)
      fResult = ftruncate64( hFileHandle, llOffset ) != -1;
   #else
      fResult = ftruncate( hFileHandle, llOffset ) != -1;
   #endif
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#else

   fResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return fResult;
}

void hb_fsCommit( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

#if defined(HB_OS_WIN)
   {
      hb_vmUnlock();
      #if defined(HB_IO_WIN)
         hb_fsSetIOError( FlushFileBuffers( ( HANDLE ) DosToWinHandle( hFileHandle ) ), 0 );
      #else
         #if defined(__WATCOMC__)
            hb_fsSetIOError( fsync( hFileHandle ) == 0, 0 );
         #else
            hb_fsSetIOError( _commit( hFileHandle ) == 0, 0 );
         #endif
      #endif
      hb_vmLock();
   }

#elif defined(HB_OS_OS2)

   {
      hb_vmUnlock();
      hb_fsSetIOError( DosResetBuffer( hFileHandle ) == 0, 0 );
      hb_vmLock();
   }

#elif defined(HB_OS_UNIX)

   /* NOTE: close() functions releases all locks regardles if it is an
    * original or duplicated file handle
   */
   hb_vmUnlock();
   /* We should check here only for _POSIX_SYNCHRONIZED_IO defined
    * and it should be enough to test if fdatasync() declaration
    * exists in <unistd.h>. Unfortunately on some OS-es like Darwin
    * _POSIX_SYNCHRONIZED_IO is defined but fdatasync() does not exists.
    * As workaround we are using this trick to check non zero version
    * number but on some systems it may disable using fdatasync() [druzus]
    */
   #if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO + 0 > 0
      /* faster - flushes data buffers only, without updating directory info
      */
      hb_fsSetIOError( fdatasync( hFileHandle ) == 0, 0 );
   #else
      /* slower - flushes all file data buffers and i-node info
      */
      hb_fsSetIOError( fsync( hFileHandle ) == 0, 0 );
   #endif
   hb_vmLock();

#elif defined(__WATCOMC__)

   hb_vmUnlock();
   hb_fsSetIOError( fsync( hFileHandle ) == 0, 0 );
   hb_vmLock();

#elif defined(HB_FS_FILE_IO) && !defined(HB_OS_OS2) && !defined(HB_OS_UNIX)

   /* This hack is very dangerous. POSIX standard define that if _ANY_
      file handle is closed all locks set by the process on the file
      pointed by this descriptor are removed. It doesn't matter they
      were done using different descriptor. It means that we now clean
      all locks on hFileHandle with the code below if the OS is POSIX
      compilant. I vote to disable it.
    */
   {
      int dup_handle;
      BOOL fResult = FALSE;

      hb_vmUnlock();
      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
      {
         close( dup_handle );
         fResult = TRUE;
      }
      hb_fsSetIOError( fResult, 0 );
      hb_vmLock();
   }

#else

   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif
}

BOOL hb_fsLock( HB_FHANDLE hFileHandle, ULONG ulStart,
                          ULONG ulLength, USHORT uiMode )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, ulStart, ulLength, uiMode));

#if defined(HB_IO_WIN)
   hb_vmUnlock();
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
      {
         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            DWORD dwFlags;
            memset( &sOlap, 0, sizeof( OVERLAPPED ) );
            sOlap.Offset = ( ULONG ) ulStart;
            dwFlags = ( uiMode & FLX_SHARED ) ? 0 : LOCKFILE_EXCLUSIVE_LOCK;
            if( !s_fUseWaitLocks || !( uiMode & FLX_WAIT ) )
            {
               dwFlags |= LOCKFILE_FAIL_IMMEDIATELY;
            }
            bResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0, ulLength, 0, &sOlap );
         }
         else
         {
             bResult = LockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength,0 );
         }
         break;
      }
      case FL_UNLOCK:
      {
         if( hb_iswinnt() )
         {
            OVERLAPPED sOlap;
            memset( &sOlap, 0, sizeof( OVERLAPPED ) );
            sOlap.Offset = ( ULONG ) ulStart;
            bResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0, ulLength,0, &sOlap );
         }
         else
         {
            bResult = UnlockFile( DosToWinHandle( hFileHandle ), ulStart, 0, ulLength,0 );
         }
         break;

      }
      default:
         bResult = FALSE;
   }
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();
#elif defined(HB_OS_OS2)
   {
      struct _FILELOCK fl, ful;

      hb_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            fl.lOffset = ulStart;
            fl.lRange = ulLength;
            ful.lOffset = 0;
            ful.lRange = 0;

            /* lock region, 2 seconds timeout, exclusive access - no atomic */
            bResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
            break;

         case FL_UNLOCK:

            fl.lOffset = 0;
            fl.lRange = 0;
            ful.lOffset = ulStart;
            ful.lRange = ulLength;

            /* unlock region, 2 seconds timeout, exclusive access - no atomic */
            bResult = ( DosSetFileLocks( hFileHandle, &ful, &fl, 2000L, 0L ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
   }
#elif defined(_MSC_VER) || defined(__DMC__)
   {
      ULONG ulOldPos;

      hb_vmUnlock();
      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            bResult = ( locking( hFileHandle, _LK_NBLCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined(__MINGW32__)
   {
      ULONG ulOldPos;

      hb_vmUnlock();
      ulOldPos = lseek( hFileHandle, 0L, SEEK_CUR );
      lseek( hFileHandle, ulStart, SEEK_SET );
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:
            bResult = ( _locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( _locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
      lseek( hFileHandle, ulOldPos, SEEK_SET );
   }
#elif defined(HB_OS_UNIX)
   {
      /* TODO: check for append locks (SEEK_END)
       */
      struct flock lock_info;

      hb_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle,
                               (uiMode & FLX_WAIT) ? F_SETLKW: F_SETLK,
                               &lock_info ) >= 0 );
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle, F_SETLK, &lock_info ) >= 0 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
   }
#elif defined(HAVE_POSIX_IO) && !defined(__IBMCPP__) && ( !defined(__GNUC__) || defined(__DJGPP__) )

   hb_vmUnlock();
   switch( uiMode & FL_MASK )
   {
      case FL_LOCK:
         bResult = ( lock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      case FL_UNLOCK:
         bResult = ( unlock( hFileHandle, ulStart, ulLength ) == 0 );
         break;

      default:
         bResult = FALSE;
   }
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return bResult;
}

BOOL hb_fsLockLarge( HB_FHANDLE hFileHandle, HB_FOFFSET ulStart,
                     HB_FOFFSET ulLength, USHORT uiMode )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLockLarge(%p, %" PFHL "u, %" PFHL "i, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, ulStart, ulLength, uiMode));

#if defined(HB_IO_WIN)
   {
      DWORD dwOffsetLo = ( DWORD ) ( ulStart & 0xFFFFFFFF ),
            dwOffsetHi = ( DWORD ) ( ulStart >> 32 ),
            dwLengthLo = ( DWORD ) ( ulLength & 0xFFFFFFFF ),
            dwLengthHi = ( DWORD ) ( ulLength >> 32 );

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

               memset( &sOlap, 0, sizeof( OVERLAPPED ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               bResult = LockFileEx( DosToWinHandle( hFileHandle ), dwFlags, 0,
                                     dwLengthLo, dwLengthHi, &sOlap );
            }
            else
            {
               bResult = LockFile( DosToWinHandle( hFileHandle ),
                                   dwOffsetLo, dwOffsetHi,
                                   dwLengthLo, dwLengthHi );
            }
            break;

         case FL_UNLOCK:
            if( hb_iswinnt() )
            {
               OVERLAPPED sOlap;

               memset( &sOlap, 0, sizeof( OVERLAPPED ) );
               sOlap.Offset = dwOffsetLo;
               sOlap.OffsetHigh = dwOffsetHi;

               bResult = UnlockFileEx( DosToWinHandle( hFileHandle ), 0,
                                       dwLengthLo, dwLengthHi, &sOlap );
            }
            else
            {
               bResult = UnlockFile( DosToWinHandle( hFileHandle ),
                                     dwOffsetLo, dwOffsetHi,
                                     dwLengthLo, dwLengthHi );
            }
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
   }
#elif defined(HB_USE_LARGEFILE64)
   {
      struct flock64 lock_info;

      hb_vmUnlock();
      switch( uiMode & FL_MASK )
      {
         case FL_LOCK:

            lock_info.l_type   = (uiMode & FLX_SHARED) ? F_RDLCK : F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle,
                               (uiMode & FLX_WAIT) ? F_SETLKW64: F_SETLK64,
                               &lock_info ) != -1 );
            break;

         case FL_UNLOCK:

            lock_info.l_type   = F_UNLCK;   /* unlock */
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle, F_SETLK64, &lock_info ) != -1 );
            break;

         default:
            bResult = FALSE;
      }
      hb_fsSetIOError( bResult, 0 );
      hb_vmLock();
   }
#else
   bResult = hb_fsLock( hFileHandle, ( ULONG ) ulStart, ( ULONG ) ulLength, uiMode );
#endif

   return bResult;
}

ULONG hb_fsSeek( HB_FHANDLE hFileHandle, LONG lOffset, USHORT uiFlags )
{
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeek(%p, %ld, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, lOffset, uiFlags));

#if defined(HB_FS_FILE_IO)
{
   USHORT Flags = convert_seek_flags( uiFlags );

   hb_vmUnlock();
   #if defined(HB_OS_OS2)
   {
      APIRET ret;

      /* This DOS hack creates 2GB file size limit, Druzus */
      if( lOffset < 0 && Flags == SEEK_SET )
      {
         ret = 1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ret = DosSetFilePtr( hFileHandle, lOffset, Flags, &ulPos );
         /* TODO: what we should do with this error code? Is it DOS compatible? */
         hb_fsSetError( ( USHORT ) ret );
      }
      if( ret != 0 )
      {
         /* FIXME: it should work if DosSetFilePtr is lseek compatible
            but maybe OS2 has DosGetFilePtr too, if not then remove this
            comment, Druzus */
         if( DosSetFilePtr( hFileHandle, 0, SEEK_CUR, &ulPos ) != 0 )
         {
            ulPos = 0;
         }
      }
   }
   #elif defined(HB_IO_WIN)
      /* This DOS hack creates 2GB file size limit, Druzus */
      if( lOffset < 0 && Flags == SEEK_SET )
      {
         ulPos = ( ULONG ) INVALID_SET_FILE_POINTER;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulPos = ( DWORD ) SetFilePointer( DosToWinHandle( hFileHandle ), lOffset, NULL, ( DWORD ) Flags );
         hb_fsSetIOError( ( DWORD ) ulPos != INVALID_SET_FILE_POINTER, 0 );
      }

      if( ( DWORD ) ulPos == INVALID_SET_FILE_POINTER )
      {
         ulPos = ( DWORD ) SetFilePointer( DosToWinHandle( hFileHandle ), 0, NULL, SEEK_CUR );
      }
   #else
      /* This DOS hack creates 2GB file size limit, Druzus */
      if( lOffset < 0 && Flags == SEEK_SET )
      {
         ulPos = ( ULONG ) -1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulPos = lseek( hFileHandle, lOffset, Flags );
         hb_fsSetIOError( ulPos != ( ULONG ) -1, 0 );
      }
      if( ulPos == ( ULONG ) -1 )
      {
         ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
         if( ulPos == ( ULONG ) -1 )
         {
            ulPos = 0;
         }
      }
   #endif
   hb_vmLock();
}
#else
   hb_fsSetError( 25 );
   ulPos = 0;
#endif

   return ulPos;
}

HB_FOFFSET hb_fsSeekLarge( HB_FHANDLE hFileHandle, HB_FOFFSET llOffset, USHORT uiFlags )
{
   HB_FOFFSET llPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeekLarge(%p, %" PFHL "i, %hu)", ( void * ) ( HB_PTRDIFF ) hFileHandle, llOffset, uiFlags));

#if defined(HB_IO_WIN)
   {
      USHORT Flags = convert_seek_flags( uiFlags );

      ULONG ulOffsetLow  = ( ULONG ) ( llOffset & ULONG_MAX ),
            ulOffsetHigh = ( ULONG ) ( llOffset >> 32 );

      hb_vmUnlock();
      if( llOffset < 0 && Flags == SEEK_SET )
      {
         llPos = ( HB_FOFFSET ) INVALID_SET_FILE_POINTER;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       ulOffsetLow, ( PLONG ) &ulOffsetHigh,
                                       ( DWORD ) Flags );
         llPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
         hb_fsSetIOError( llPos != ( HB_FOFFSET ) INVALID_SET_FILE_POINTER, 0 );
      }

      if( llPos == ( HB_FOFFSET ) INVALID_SET_FILE_POINTER )
      {
         ulOffsetHigh = 0;
         ulOffsetLow = SetFilePointer( DosToWinHandle( hFileHandle ),
                                       0, ( PLONG ) &ulOffsetHigh, SEEK_CUR );
         llPos = ( ( HB_FOFFSET ) ulOffsetHigh << 32 ) | ulOffsetLow;
      }
      hb_vmLock();
   }
#elif defined(HB_USE_LARGEFILE64)
   {
      USHORT Flags = convert_seek_flags( uiFlags );

      hb_vmUnlock();
      if( llOffset < 0 && Flags == SEEK_SET )
      {
         llPos = (HB_FOFFSET) -1;
         hb_fsSetError( 25 ); /* 'Seek Error' */
      }
      else
      {
         llPos = lseek64( hFileHandle, llOffset, Flags );
         hb_fsSetIOError( llPos != ( HB_FOFFSET ) -1, 0 );
      }

      if( llPos == (HB_FOFFSET) -1 )
      {
         llPos = lseek64( hFileHandle, 0L, SEEK_CUR );
      }
      hb_vmLock();
   }
#else
   llPos = (HB_FOFFSET) hb_fsSeek( hFileHandle, ( LONG ) llOffset, uiFlags );
#endif

   return llPos;
}

ULONG hb_fsTell( HB_FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsTell(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

   return hb_fsSeek( hFileHandle, 0, FS_RELATIVE );
}

BOOL hb_fsDelete( const char * pFilename )
{
   BOOL bResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", pFilename));

   pFilename = hb_fsNameConv( pFilename, &pszFree );

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = DeleteFileA( pFilename );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HB_FS_FILE_IO)

   hb_vmUnlock();
   bResult = ( remove( pFilename ) == 0 );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return bResult;
}

BOOL hb_fsRename( const char * pOldName, const char * pNewName )
{
   BOOL bResult;
   char * pszFreeOld, * pszFreeNew;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", pOldName, pNewName));

   pOldName = hb_fsNameConv( pOldName, &pszFreeOld );
   pNewName = hb_fsNameConv( pNewName, &pszFreeNew );

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = MoveFileA( pOldName, pNewName );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HB_FS_FILE_IO)

   hb_vmUnlock();
   bResult = ( rename( pOldName, pNewName ) == 0 );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFreeOld )
      hb_xfree( pszFreeOld );
   if( pszFreeNew )
      hb_xfree( pszFreeNew );

   return bResult;
}

BOOL hb_fsMkDir( const char * pDirname )
{
   BOOL bResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", pDirname));

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = CreateDirectoryA( pDirname, NULL );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   hb_vmUnlock();
#  if ! defined(HB_OS_UNIX) && \
      ( defined(__WATCOMC__) || defined(__BORLANDC__) || \
        defined(__IBMCPP__) || defined(__MINGW32__) )
      bResult = ( mkdir( pDirname ) == 0 );
#  else
      bResult = ( mkdir( pDirname, S_IRWXU | S_IRWXG | S_IRWXO ) == 0 );
#  endif
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return bResult;
}

BOOL hb_fsChDir( const char * pDirname )
{
   BOOL bResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = SetCurrentDirectoryA( pDirname );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   hb_vmUnlock();
   bResult = ( chdir( pDirname ) == 0 );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return bResult;
}

BOOL hb_fsRmDir( const char * pDirname )
{
   BOOL bResult;
   char * pszFree;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", pDirname));

   pDirname = hb_fsNameConv( pDirname, &pszFree );

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = RemoveDirectoryA( pDirname );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   hb_vmUnlock();
   bResult = ( rmdir( pDirname ) == 0 );
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   if( pszFree )
      hb_xfree( pszFree );

   return bResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

const char * hb_fsCurDir( USHORT uiDrive )
{
   char * pszDirBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDir(%hu)", uiDrive));

   pszDirBuffer = hb_stackDirBuffer();
   hb_fsCurDirBuff( uiDrive, pszDirBuffer, HB_PATH_MAX );

   return pszDirBuffer;
}

/* NOTE: Thread safe version of hb_fsCurDir() */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

USHORT hb_fsCurDirBuff( USHORT uiDrive, char * pszBuffer, ULONG ulSize )
{
   USHORT uiCurDrv = uiDrive, usError;
   char * pszStart;
   ULONG ulLen;
   BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%hu)", uiDrive));

   pszBuffer[ 0 ] = '\0';

   /*
    * do not cover this code by HB_OS_HAS_DRIVE_LETTER macro
    * It will allow us to add drive emulation in hb_fsCurDrv()/hb_fsChDrv()
    * and hb_fsNameConv()
    */
#if defined(HB_OS_WIN) || \
    ( !( defined(HB_OS_OS2) && defined(__GNUC__) ) && \
      !defined(__MINGW32__) && defined(HAVE_POSIX_IO) )
   if( uiDrive )
   {
      uiCurDrv = hb_fsCurDrv() + 1;
      if( uiDrive != uiCurDrv )
         hb_fsChDrv( ( BYTE ) ( uiDrive - 1 ) );
   }
#endif

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   fResult = GetCurrentDirectoryA( ulSize, pszBuffer );
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#elif defined(HB_OS_OS2) && defined(__GNUC__)

   hb_vmUnlock();
   fResult = ( _getcwd1( pszBuffer, uiDrive + 'A' - 1 ) == 0 );
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#elif defined(__MINGW32__)

   hb_vmUnlock();
   fResult = ( _getdcwd( uiDrive, pszBuffer, ulSize ) != NULL );
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#elif defined(HAVE_POSIX_IO)

   hb_vmUnlock();
   fResult = ( getcwd( pszBuffer, ulSize ) != NULL );
   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

#else

   fResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   usError = hb_fsError();

   if( uiDrive != uiCurDrv )
   {
      hb_fsChDrv( ( BYTE ) ( uiCurDrv - 1 ) );
      hb_fsSetError( usError );
   }

   pszBuffer[ ulSize - 1 ] = '\0';

   if( usError == 0 && pszBuffer[ 0 ] )
   {
      /* Strip the leading drive spec, and leading backslash if there's one. */
      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

#if defined(__DJGPP__)
      /* convert '/' to '\' */
      while( ( pszStart = strchr( pszBuffer, '/' ) ) != NULL )
         *pszStart = '\\';
#endif

      pszStart = pszBuffer;
      ulLen = strlen( pszBuffer );

#if defined(HB_OS_HAS_DRIVE_LETTER)
      if( pszStart[ 1 ] == HB_OS_DRIVE_DELIM_CHR )
      {
         pszStart += 2;
         ulLen -= 2;
      }
#endif
      if( strchr( HB_OS_PATH_DELIM_CHR_LIST, ( UCHAR ) pszStart[ 0 ] ) )
      {
         pszStart++;
         ulLen--;
      }

      /* Strip the trailing (back)slash if there's one */
      if( ulLen && strchr( HB_OS_PATH_DELIM_CHR_LIST, ( UCHAR ) pszStart[ ulLen - 1 ] ) )
         ulLen--;

      if( ulLen && pszBuffer != pszStart )
         memmove( pszBuffer, pszStart, ulLen );

      pszBuffer[ ulLen ] = '\0';

      /* Convert from OS codepage */
      {
         char * pszFree;
         const char * pszResult = hb_osDecode( pszBuffer, &pszFree );

         if( pszResult != pszBuffer )
            hb_strncpy( pszBuffer, pszResult, ulSize - 1 );
         if( pszFree )
            hb_xfree( pszFree );
      }
   }

   return usError;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

USHORT hb_fsChDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", (int) nDrive));

#if defined(HB_OS_HAS_DRIVE_LETTER)
   {
      /* 'unsigned int' _have to_ be used in Watcom */
      UINT uiSave, uiNewDrive;

      hb_vmUnlock();

      HB_FS_GETDRIVE( uiSave );
      HB_FS_SETDRIVE( nDrive );
      HB_FS_GETDRIVE( uiNewDrive );

      if( ( UINT ) nDrive == uiNewDrive )
      {
         uiResult = 0;
         hb_fsSetError( 0 );
      }
      else
      {
         HB_FS_SETDRIVE( uiSave );

         uiResult = ( USHORT ) FS_ERROR;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }
      hb_vmLock();
   }
#else

   HB_SYMBOL_UNUSED( nDrive );
   uiResult = ( USHORT ) FS_ERROR;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return uiResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

BYTE hb_fsCurDrv( void )
{
   UINT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));

#if defined(HB_OS_HAS_DRIVE_LETTER)

   hb_vmUnlock();
   HB_FS_GETDRIVE( uiResult );
   hb_fsSetError( 0 );
   hb_vmLock();

#else

   uiResult = 0;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return ( BYTE ) uiResult; /* Return the drive number, base 0. */
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

/* TOFIX: This isn't fully compliant because CA-Cl*pper doesn't access
          the drive before checking. hb_fsIsDrv only returns TRUE
          if there is a disk in the drive. */

USHORT hb_fsIsDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", (int) nDrive));

#if defined(HB_OS_WIN) && !defined(HB_OS_WIN_CE)
   {
      char buffer[ 4 ];
      UINT type;

      buffer[ 0 ] = nDrive + 'A';
      buffer[ 1 ] = ':';
      buffer[ 2 ] = '\\';
      buffer[ 3 ] = '\0';

      hb_vmUnlock();
      type = GetDriveTypeA( buffer );
      hb_vmLock();
      uiResult = ( type == DRIVE_UNKNOWN || type == DRIVE_NO_ROOT_DIR ) ? F_ERROR : 0;
      hb_fsSetError( 0 );
   }
#elif defined(HB_OS_HAS_DRIVE_LETTER)
   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      UINT uiSave, uiNewDrive;

      hb_vmUnlock();

      HB_FS_GETDRIVE( uiSave );
      HB_FS_SETDRIVE( nDrive );
      HB_FS_GETDRIVE( uiNewDrive );
      if( ( UINT ) nDrive != uiNewDrive )
      {
         uiResult = ( USHORT ) FS_ERROR;
         hb_fsSetError( ( USHORT ) FS_ERROR );
      }
      else
      {
         uiResult = 0;
         hb_fsSetError( 0 );
      }
      HB_FS_SETDRIVE( uiSave );

      hb_vmLock();
   }
#else

   HB_SYMBOL_UNUSED( nDrive );
   uiResult = ( USHORT ) FS_ERROR;
   hb_fsSetError( ( USHORT ) FS_ERROR );

#endif

   return uiResult;
}

BOOL hb_fsIsDevice( HB_FHANDLE hFileHandle )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDevice(%p)", ( void * ) ( HB_PTRDIFF ) hFileHandle));

#if defined(HB_OS_WIN)

   hb_vmUnlock();
   bResult = GetFileType( DosToWinHandle( hFileHandle ) ) == FILE_TYPE_CHAR;
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#elif defined(HB_FS_FILE_IO)

   hb_vmUnlock();
#if defined( _MSC_VER ) || defined( __MINGW32__ )
   bResult = _isatty( hFileHandle ) != 0;
#else
   bResult = isatty( hFileHandle ) != 0;
#endif
   hb_fsSetIOError( bResult, 0 );
   hb_vmLock();

#else

   bResult = FALSE;
   hb_fsSetError( ( USHORT ) FS_ERROR );
   HB_SYMBOL_UNUSED( hFileHandle );

#endif

   return bResult;
}

/* convert file name for hb_fsExtOpen
 * caller must free the returned buffer
 */
char * hb_fsExtName( const char * pFilename, const char * pDefExt,
                     USHORT uiExFlags, const char * pPaths )
{
   HB_PATHNAMES * pNextPath;
   PHB_FNAME pFilepath;
   BOOL fIsFile = FALSE;
   char * szPath;

   szPath = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFilepath = hb_fsFNameSplit( pFilename );

   if( pDefExt && ( ( uiExFlags & FXO_FORCEEXT ) || !pFilepath->szExtension ) )
   {
      pFilepath->szExtension = pDefExt;
   }

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
   {
      hb_fsFNameMerge( szPath, pFilepath );
   }
   hb_xfree( pFilepath );

   return szPath;
}

HB_FHANDLE hb_fsExtOpen( const char * pFilename, const char * pDefExt,
                         USHORT uiExFlags, const char * pPaths,
                         PHB_ITEM pError )
{
   HB_FHANDLE hFile;
   USHORT uiFlags;
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
      int iLock;
      if( ( uiFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) ) == FO_READ ||
          ( uiFlags & ( FO_DENYREAD | FO_DENYWRITE | FO_EXCLUSIVE ) ) == 0 )
         iLock = LOCK_SH | LOCK_NB;
      else
         iLock = LOCK_EX | LOCK_NB;
      hb_vmUnlock();
      iLock = flock( hFile, iLock );
      hb_vmLock();
      if( iLock != 0 )
#else
      USHORT uiLock;
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
         hb_fsWrite( hFile, NULL, 0 );
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
         hb_errPutGenCode( pError, ( USHORT ) ( ( uiExFlags & FXO_TRUNCATE ) ? EG_CREATE : EG_OPEN ) );
      }
   }

   if( uiExFlags & FXO_COPYNAME && hFile != FS_ERROR )
      hb_strncpy( ( char * ) pFilename, szPath, HB_PATH_MAX - 1 );

   hb_xfree( szPath );
   return hFile;
}

BOOL hb_fsEof( HB_FHANDLE hFileHandle )
{
   BOOL fResult;

   hb_vmUnlock();

#if defined(__DJGPP__) || defined(__CYGWIN__) || \
    defined(HB_IO_WIN) || defined(HB_OS_WIN_CE) || \
    defined(HB_OS_UNIX_COMPATIBLE)
{
   HB_FOFFSET curPos;
   HB_FOFFSET endPos;
   HB_FOFFSET newPos;

   curPos = hb_fsSeekLarge( hFileHandle, 0L, SEEK_CUR );
   if( curPos != -1 )
   {
      endPos = hb_fsSeekLarge( hFileHandle, 0L, SEEK_END );
      newPos = hb_fsSeekLarge( hFileHandle, curPos, SEEK_SET );
      fResult = ( endPos != -1 && newPos == curPos );
   }
   else
   {
      endPos = -1;
      fResult = FALSE;
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
   BOOL fTrim;

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
      ULONG ulLen;

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
            ulLen = strlen( pFileName->szName );
            ulLen = hb_strRTrimLen( pFileName->szName, ulLen, FALSE );
            pFileName->szName = hb_strLTrim( pFileName->szName, &ulLen );
            ( ( char * ) pFileName->szName )[ ulLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            ulLen = strlen( pFileName->szExtension );
            ulLen = hb_strRTrimLen( pFileName->szExtension, ulLen, FALSE );
            pFileName->szExtension = hb_strLTrim( pFileName->szExtension, &ulLen );
            ( ( char * ) pFileName->szExtension )[ ulLen ] = '\0';
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

      if( pszCP )
      {
         szFileName = hb_osEncode( szFileName, NULL );
         if( pszFree )
            *pszFree = ( char * ) szFileName;
      }

      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_xfree( pFileName );
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
      char * pszFree;

      pFName->szName = NULL;
      pFName->szExtension = NULL;
      hb_fsFNameMerge( pszBuffer, pFName );
      hb_xfree( pFName );

      /* Convert from OS codepage */
      pszResult = hb_osDecode( pszBuffer, &pszFree );
      if( pszResult != pszBuffer )
         hb_strncpy( pszBuffer, pszResult, HB_PATH_MAX - 1 );
      if( pszFree )
         hb_xfree( pszFree );
   }
}

static BOOL hb_fsDisableWaitLocks( int iSet )
{
   BOOL fRetVal = s_fUseWaitLocks;

   if( iSet >= 0 )
      s_fUseWaitLocks = ( iSet == 0 );

   return fRetVal;
}

HB_FUNC( HB_DISABLEWAITLOCKS )
{
   hb_retl( hb_fsDisableWaitLocks( HB_ISLOG( 1 ) ? ( hb_parl( 1 ) ? 1 : 0 ) : -1 ) );
}
