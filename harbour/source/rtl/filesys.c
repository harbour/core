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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_fsSetError()
 *    hb_fsSetDevMode()
 *    hb_fsReadLarge()
 *    hb_fsWriteLarge()
 *    hb_fsCurDirBuff()
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
 *    Added __WIN32__ check for any compiler to use the Win32
 *    API calls to allow openning an unlimited number of files
 *    simultaneously.
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET) will
         set the file pointer to the passed negative value, and the subsequent
         hb_fsWrite() call will fail. In CA-Clipper hb_fsSeek() will fail,
         the pointer will not be moved, and thus the hb_fsWrite() call will
         successfully write the buffer to the current file position. [vszakats]

   This has been corrected by ptucker
 */

#define HB_OS_WIN_32_USED

#include <ctype.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hb_io.h"

#if defined(__GNUC__) && !defined(__MINGW32__)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <errno.h>

   #if defined(__CYGWIN__)
      #include <io.h>
   #endif

   #if defined(__DJGPP__)
      #include <dir.h>
      #define _getdrive getdisk
      #define _chdrive  setdisk
   #endif

   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif

   extern int rename( const char *, const char * );

#endif

#if defined(__WATCOMC__)
   #include <sys/stat.h>
   #include <share.h>
   #include <fcntl.h>
   #include <direct.h>
   #include <errno.h>
   #include <dos.h>

   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif
   #define ftruncate chsize
#endif

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(_MSC_VER) || defined(__MINGW32__)
   #include <sys\stat.h>
   #include <share.h>
   #include <fcntl.h>
   #include <direct.h>
   #if defined(__BORLANDC__)
      #include <dir.h>
      #include <dos.h>
      #include <windows.h>
   #endif

   #if defined(_MSC_VER) || defined(__MINGW32__)
      #include <sys\locking.h>
      #define ftruncate _chsize
   #else
      #define ftruncate chsize
      #if !defined(HAVE_POSIX_IO)
         #define HAVE_POSIX_IO
      #endif
   #endif
   #include <errno.h>
#endif

#if defined(__MPW__)
   #include <fcntl.h>
#endif

#if defined(HB_OS_DOS)
   #include <dos.h>
#endif

#ifndef O_BINARY
   #define O_BINARY     0       /* O_BINARY not defined on Linux */
#endif

#ifndef S_IEXEC
   #define S_IEXEC      0x0040  /* owner may execute <directory search> */
#endif

#ifndef S_IRWXU
   #define S_IRWXU      0x01C0  /* RWE permissions mask for owner */
#endif

#ifndef S_IRUSR
   #define S_IRUSR      0x0100  /* owner may read */
#endif

#ifndef S_IWUSR
   #define S_IWUSR      0x0080  /* owner may write */
#endif

#ifndef S_IXUSR
   #define S_IXUSR      0x0040  /* owner may execute <directory search> */
#endif

#ifndef SH_COMPAT
   #define SH_COMPAT    0x00    /* Compatibility */
#endif

#ifndef SH_DENYRW
   #define SH_DENYRW    0x10    /* Deny read/write */
#endif

#ifndef SH_DENYWR
   #define SH_DENYWR    0x20    /* Deny write */
#endif

#ifndef SH_DENYRD
   #define SH_DENYRD    0x30    /* Deny read */
#endif

#ifndef SH_DENYNO
   #define SH_DENYNO    0x40    /* Deny nothing */
#endif

static USHORT s_uiErrorLast = 0;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER) || defined(__MINGW32__)
/* Only compilers with Posix or Posix-like I/O support are supported */
   #define HB_FS_FILE_IO
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(__IBMCPP__) || defined(__WATCOMC__)
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define HB_FS_SOPEN
#endif

#if ( defined(HAVE_POSIX_IO) && ( defined(HB_OS_OS2) || defined(HB_OS_DOS) || defined(_Windows) || defined(_WIN32) ) && ! defined(__CYGWIN__) ) || defined(__MINGW32__)
/* These platforms and/or compilers have common drive letter support */
   #define HB_FS_DRIVE_LETTER
#endif

#if UINT_MAX == ULONG_MAX
   #define HB_FS_LARGE_OPTIMIZED
#else
   #define LARGE_MAX ( UINT_MAX - 1L )
#endif

/* Convert HARBOUR flags to IO subsystem flags */

#if defined(HB_FS_FILE_IO)

static int convert_open_flags( USHORT uiFlags )
{
   /* by default FO_READ + FO_COMPAT is set */
   int result_flags = 0;

   HB_TRACE(HB_TR_DEBUG, ("convert_open_flags(%hu)", uiFlags));

   result_flags |= O_BINARY;
   HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_BINARY\n"));

#if defined(HB_FS_SOPEN)
   if( ( uiFlags & ( FO_WRITE | FO_READWRITE ) ) == FO_READ )
   {
      result_flags |= O_RDONLY;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDONLY\n"));
   }
#else
   if( ( uiFlags & ( FO_WRITE | FO_READWRITE ) ) == FO_READ )
   {
      result_flags |= ( O_RDONLY | SH_COMPAT );
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDONLY SH_COMPAT\n"));
   }
#endif

   /* read & write flags */
   if( uiFlags & FO_WRITE )
   {
      result_flags |= O_WRONLY;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_WRONLY\n"));
   }

   if( uiFlags & FO_READWRITE )
   {
      result_flags |= O_RDWR;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added O_RDWR\n"));
   }

#if ! defined(HB_FS_SOPEN)
   /* shared flags */
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
   {
      result_flags |= SH_DENYRD;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYRD\n"));
   }

   else if( uiFlags & FO_EXCLUSIVE )
   {
      result_flags |= SH_DENYRW;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYRW\n"));
   }

   else if( uiFlags & FO_DENYWRITE )
   {
      result_flags |= SH_DENYWR;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYWR\n"));
   }

   if( uiFlags & FO_DENYNONE )
   {
      result_flags |= SH_DENYNO;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYNO\n"));
   }

   if( uiFlags & FO_SHARED )
   {
      result_flags |= SH_DENYNO;
      HB_TRACE(HB_TR_INFO, ("convert_open_flags: added SH_DENYNO\n"));
   }
#endif

   HB_TRACE(HB_TR_INFO, ("convert_open_flags: result is 0x%04x\n", result_flags));

   return result_flags;
}

static int convert_seek_flags( USHORT uiFlags )
{
   /* by default FS_SET is set */
   int result_flags = SEEK_SET;

   HB_TRACE(HB_TR_DEBUG, ("convert_seek_flags(%hu)", uiFlags));

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}

static void convert_create_flags( USHORT uiFlags, int * result_flags, unsigned * result_pmode )
{
   HB_TRACE(HB_TR_DEBUG, ("convert_create_flags(%hu, %p, %p)", uiFlags, result_flags, result_pmode));

   /* by default FC_NORMAL is set */

   *result_flags = O_BINARY | O_CREAT | O_TRUNC | O_RDWR;
   *result_pmode = S_IRUSR | S_IWUSR;

   if( uiFlags & FC_READONLY )
   {
      *result_pmode = S_IRUSR;
      HB_TRACE(HB_TR_INFO, ("convert_create_flags: S_IRUSR"));
   }

   if( uiFlags & FC_HIDDEN )
      *result_flags |= 0;

   if( uiFlags & FC_SYSTEM )
      *result_flags |= 0;

   HB_TRACE(HB_TR_INFO, ("convert_create_flags: 0x%04x, 0x%04x\n", *result_flags, *result_pmode));
}

#endif


/*
 * FILESYS.API FUNCTIONS --
 */

FHANDLE hb_fsOpen( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsOpen(%p, %hu)", pFilename, uiFlags));

#if defined(X__WIN32__)

   {
      DWORD dwFlags = 0;
      DWORD dwShare = FILE_SHARE_READ | FILE_SHARE_WRITE;
      HANDLE hFile;

      /* read & write flags */
      if( uiFlags & FO_WRITE )
         dwFlags |= GENERIC_WRITE;

      if( uiFlags & FO_READWRITE )
         dwFlags |= GENERIC_READ;

      /* shared flags */
      if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
         dwShare = FILE_SHARE_WRITE;

      else if( uiFlags & FO_DENYWRITE )
         dwShare = FILE_SHARE_READ;

      else if( uiFlags & FO_EXCLUSIVE )
         dwShare = 0;

      errno = 0;

      hFile = ( HANDLE ) CreateFile( ( char * ) pFilename, dwFlags,
                dwShare, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );

      if( hFile == ( HANDLE ) INVALID_HANDLE_VALUE )
         errno = GetLastError();
      hFileHandle=(int)hFile;
      s_uiErrorLast = errno;
   }

#elif defined(HAVE_POSIX_IO) && ! defined(__IBMCPP__)

   errno = 0;
   hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
   s_uiErrorLast = errno;

#elif defined(_MSC_VER)

   {
      int iShare = _SH_DENYNO;

      if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
         iShare = _SH_DENYRD;

      else if( uiFlags & FO_EXCLUSIVE )
         iShare = _SH_DENYRW;

      else if( uiFlags & FO_DENYWRITE )
         iShare = _SH_DENYWR;

      errno = 0;
      if( iShare )
         hFileHandle = _sopen( ( char * ) pFilename, convert_open_flags( uiFlags ), iShare );
      else
         hFileHandle = _open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
      s_uiErrorLast = errno;
   }

#elif defined(__MINGW32__) || defined(__IBMCPP__)

   {
      int iShare = SH_DENYNO;

      if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
         iShare = SH_DENYRD;

      else if( uiFlags & FO_EXCLUSIVE )
         iShare = SH_DENYRW;

      else if( uiFlags & FO_DENYWRITE )
         iShare = SH_DENYWR;

      errno = 0;
      if( iShare )
         hFileHandle = sopen( ( char * ) pFilename, convert_open_flags( uiFlags ), iShare );
      else
         hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
      s_uiErrorLast = errno;
   }

#else

   hFileHandle = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return hFileHandle;
}

FHANDLE hb_fsCreate( BYTE * pFilename, USHORT uiAttr )
{
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%p, %hu)", pFilename, uiAttr));

   s_uiErrorLast = 0;

#if defined(X__WIN32__)

   {
      DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;
      HANDLE hFile;
      if( uiAttr & FC_READONLY )
         dwFlags |= FILE_ATTRIBUTE_READONLY;

      if( uiAttr & FC_HIDDEN )
         dwFlags |= FILE_ATTRIBUTE_HIDDEN;

      if( uiAttr & FC_SYSTEM )
         dwFlags |= FILE_ATTRIBUTE_SYSTEM;

      errno = 0;

      hFile = ( HANDLE ) CreateFile( ( char * ) pFilename,
                    GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
                    dwFlags, NULL );

      if( hFile == ( FHANDLE ) INVALID_HANDLE_VALUE )
         errno = GetLastError();
         hFileHandle=(int)hFile;
      s_uiErrorLast = errno;
   }

#elif defined(HB_FS_FILE_IO)

   errno = 0;
   convert_create_flags( uiAttr, &oflag, &pmode );
   hFileHandle = open( ( char * ) pFilename, oflag, pmode );
   if( hFileHandle == -1 )
   {
      /* This if block is required, because errno will be set
         if the file did not exist and had to be created, even
         when the create is successful! */
      s_uiErrorLast = errno;
   }

#else

   hFileHandle = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return hFileHandle;
}

/* Derived from hb_fsCreate() 

   NOTE: The default opening mode differs from the one used in hb_fsCreate() 
         [vszakats]
 */

FHANDLE hb_fsCreateEx( BYTE * pFilename, USHORT uiAttr, USHORT uiFlags )
{
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreateEx(%p, %hu, %hu)", pFilename, uiAttr, uiFlags));

   s_uiErrorLast = 0;

#if defined(HB_FS_FILE_IO)

   errno = 0;
   convert_create_flags( uiAttr, &oflag, &pmode );
   hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ), pmode );
   if( hFileHandle == -1 )
   {
      /* This if block is required, because errno will be set
         if the file did not exist and had to be created, even
         when the create is successful! */
      s_uiErrorLast = errno;
   }

#else

   hFileHandle = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return hFileHandle;
}

void    hb_fsClose( FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", hFileHandle));

#if defined(HB_FS_FILE_IO)

   errno = 0;

   #if defined(X__WIN32__)
      CloseHandle( ( HANDLE ) hFileHandle );
   #else
      close( hFileHandle );
   #endif

   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

   /* Convert 'Invalid Memory Block' to 'Invalid Handle' */
   if( s_uiErrorLast == 9 )
      s_uiErrorLast = 6;
}

void    hb_fsSetDevMode( FHANDLE hFileHandle, USHORT uiDevMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetDevMode(%p, %hu)", hFileHandle, uiDevMode));

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__DJGPP__) || defined(__CYGWIN__) || defined(__WATCOMC__)

   errno = 0;
   switch( uiDevMode )
   {
      case FD_BINARY:
         setmode( hFileHandle, O_BINARY );
         break;

      case FD_TEXT:
         setmode( hFileHandle, O_TEXT );
         break;
   }
   s_uiErrorLast = errno;

#elif defined(_MSC_VER) || defined(__MINGW32__)

   errno = 0;
   switch( uiDevMode )
   {
      case FD_BINARY:
         _setmode( hFileHandle, _O_BINARY );
         break;

      case FD_TEXT:
         _setmode( hFileHandle, _O_TEXT );
         break;
   }
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

}

void    hb_fsSetDevRaw( FHANDLE hFileHandle )
{
   hb_fsSetDevMode( hFileHandle, FD_BINARY );
}

void    hb_fsSetDevText( FHANDLE hFileHandle )
{
   hb_fsSetDevMode( hFileHandle, FD_TEXT );
}

USHORT  hb_fsRead( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   USHORT uiRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRead(%p, %p, %hu)", hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   errno = 0;

   #if defined(X__WIN32__)
      {
         DWORD dwRead = 0;
         BOOL bError;
         bError=ReadFile( ( HANDLE ) hFileHandle, pBuff, uiCount, &dwRead, NULL );
         if (!bError)
            errno = GetLastError();
         uiRead = ( USHORT ) dwRead;
         
      }
   #else
      uiRead = read( hFileHandle, pBuff, uiCount );
   #endif

   s_uiErrorLast = errno;
   if( uiRead == ( USHORT ) -1 )
      uiRead = 0;

#else

   uiRead = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiRead;
}

USHORT  hb_fsWrite( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   USHORT uiWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWrite(%p, %p, %hu)", hFileHandle, pBuff, uiCount));

#if defined(HB_FS_FILE_IO)

   errno = 0;

   #if defined(X__WIN32__)
      {
         DWORD dwWritten = 0;
         BOOL bError;
         bError=WriteFile( ( HANDLE ) hFileHandle, pBuff, uiCount, &dwWritten, NULL );
         if (!bError)
            errno = GetLastError();
         uiWritten = ( USHORT ) dwWritten;

      }
   #else

      if( uiCount )
      {
         uiWritten = write( hFileHandle, pBuff, uiCount );
         if( uiWritten == ( USHORT ) -1 )
            uiWritten = 0;
      }
      else
      {
         uiWritten = 0;
         ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) );
      }
      s_uiErrorLast = errno;

   #endif

#else

   uiWritten = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiWritten;
}

ULONG   hb_fsReadLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   ULONG ulRead;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLarge(%p, %p, %lu)", hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   errno = 0;

   #if defined(X__WIN32__)
      {
      BOOL bError;
      bError=ReadFile( ( HANDLE ) hFileHandle, pBuff, ulCount, &ulRead, NULL );
      if (!bError)
         errno = GetLastError();
      }
   #elif defined(HB_FS_LARGE_OPTIMIZED)
      ulRead = read( hFileHandle, pBuff, ulCount );
   #else
      {
         ULONG ulLeftToRead = ulCount;
         USHORT uiToRead;
         USHORT uiRead;
         BYTE * pPtr = pBuff;

         ulRead = 0;

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
      }
   #endif
   s_uiErrorLast = errno;

#else

   ulRead = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return ulRead;
}

ULONG   hb_fsWriteLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   ULONG ulWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsWriteLarge(%p, %p, %lu)", hFileHandle, pBuff, ulCount));

#if defined(HB_FS_FILE_IO)

   errno = 0;

   #if defined(X__WIN32__)
   {
      BOOL bError;
      bError=WriteFile( ( HANDLE ) hFileHandle, pBuff, ulCount, &ulWritten, NULL );
      if (!bError)
         errno = GetLastError();
      }
   #else
      if( ulCount )
      #if defined(HB_FS_LARGE_OPTIMIZED)
         {
            ulWritten = write( hFileHandle, pBuff, ulCount );
            if( ulWritten == ( ULONG ) -1 )
               ulWritten = 0;
         }
      #else
         {
            ULONG ulLeftToWrite = ulCount;
            USHORT uiToWrite;
            USHORT uiWritten;
            BYTE * pPtr = pBuff;

            ulWritten = 0;

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
         }
      #endif
      else
      {
         ulWritten = 0;
         ftruncate( hFileHandle, lseek( hFileHandle, 0L, SEEK_CUR ) );
      }
   #endif

   s_uiErrorLast = errno;

#else

   ulWritten = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return ulWritten;
}

ULONG   hb_fsSeek( FHANDLE hFileHandle, LONG lOffset, USHORT uiFlags )
{
   ULONG ulPos;
   USHORT Flags;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsSeek(%p, %ld, %hu)", hFileHandle, lOffset, uiFlags));

   Flags = convert_seek_flags( uiFlags );

   if( lOffset < 0 && Flags == SEEK_SET )
   {

   #if defined(HB_OS_OS2)

      {
         APIRET ret = DosSetFilePtr( hFileHandle, 0, SEEK_CUR, &ulPos );

         if( ret != 0 )
         {
            ulPos = 0;
            s_uiErrorLast = ( USHORT ) ret;
         }
      }

   #elif defined(HB_FS_FILE_IO)

      /* get current offset */
      errno = 0;
      #if defined(X__WIN32__)

         ulPos = SetFilePointer( ( HANDLE ) hFileHandle, 0, NULL, FILE_CURRENT );
            if ((DWORD)ulPos = (DWORD)-1)
               errno=GetLastError();
      #else
         ulPos = lseek( hFileHandle, 0, SEEK_CUR );
      #endif
      if( errno != 0 )
      {
         ulPos = 0;
         s_uiErrorLast = errno;
      }
      else
         s_uiErrorLast = 25; /* 'Seek Error' */

   #else

      ulPos = 0;
      s_uiErrorLast = 25; /* 'Seek Error' */

   #endif

   }
   else
   {

   #if defined(HB_OS_OS2)

      {
         APIRET ret = DosSetFilePtr( hFileHandle, lOffset, Flags, &ulPos );

         if( ret != 0 )
         {
            ulPos = 0;
            s_uiErrorLast = ( USHORT ) ret;
         }
      }

   #elif defined(HB_FS_FILE_IO)

      errno = 0;
      #if defined(X__WIN32__)
         ulPos = SetFilePointer( ( HANDLE ) hFileHandle, lOffset, NULL, (DWORD)Flags );
            if ((DWORD)ulPos = (DWORD)-1)
               errno=GetLastError();

      #else
         ulPos = lseek( hFileHandle, lOffset, Flags );
      #endif
      if( errno != 0 )
         ulPos = 0;
      s_uiErrorLast = errno;

   #else

      ulPos = 0;
      s_uiErrorLast = FS_ERROR;

   #endif

      /* Convert 'Unknown Command' to 'Seek Error' */
      if( s_uiErrorLast == 22 )
         s_uiErrorLast = 25;
   }

   return ulPos;
}

ULONG   hb_fsTell( FHANDLE hFileHandle )
{
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsTell(%p)", hFileHandle));

#if defined(HB_FS_FILE_IO)

   errno = 0;
   #if defined(X__WIN32__)
      ulPos = SetFilePointer( ( HANDLE ) hFileHandle, 0, NULL, FILE_CURRENT );
      if ((DWORD)ulPos = (DWORD)-1)
         errno=GetLastError();

   #else
      ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
   #endif
   s_uiErrorLast = errno;

#else

   ulPos = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return ulPos;
}

USHORT  hb_fsError( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsError()"));

   return s_uiErrorLast;
}

void    hb_fsSetError( USHORT uiError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetError(%hu)", uiError));

   s_uiErrorLast = uiError;
}

BOOL hb_fsDelete( BYTE * pFilename )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", (char*) pFilename));

#if defined(HB_OS_WIN_32)

   bResult = ( DeleteFile( ( char * ) pFilename ) == 0 );
   s_uiErrorLast = GetLastError();

#elif defined(HAVE_POSIX_IO)

   errno = 0;
   bResult = ( unlink( ( char * ) pFilename ) == 0 );
   s_uiErrorLast = errno;

#elif defined(_MSC_VER) || defined(__MINGW32__)

   errno = 0;
   bResult = ( remove( ( char * ) pFilename ) == 0 );
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

BOOL hb_fsRename( BYTE * pOldName, BYTE * pNewName )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", (char*) pOldName, (char*) pNewName));

#if defined(HB_OS_WIN_32)

   bResult = ( MoveFile( ( char * ) pOldName, ( char * ) pNewName ) == 0 );
   s_uiErrorLast = GetLastError();

#elif defined(HB_FS_FILE_IO)

   errno = 0;
   bResult = ( rename( ( char * ) pOldName, ( char * ) pNewName ) == 0 );
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

BOOL    hb_fsLock   ( FHANDLE hFileHandle, ULONG ulStart,
                      ULONG ulLength, USHORT uiMode )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", hFileHandle, ulStart, ulLength, uiMode));

#if defined(HB_OS_OS2)

   {
      struct _FILELOCK fl, ful;

      errno = 0;

      switch( uiMode )
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

      s_uiErrorLast = errno;
   }

#elif defined(_MSC_VER)

   {
      ULONG ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

      errno = 0;
      switch( uiMode )
      {
         case FL_LOCK:
            bResult = ( locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( locking( hFileHandle, _LK_UNLCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      s_uiErrorLast = errno;

      hb_fsSeek( hFileHandle, ulOldPos, FS_SET );
   }

#elif defined(__MINGW32__)

   {
      ULONG ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

      errno = 0;
      switch( uiMode )
      {
         case FL_LOCK:
            bResult = ( _locking( hFileHandle, _LK_LOCK, ulLength ) == 0 );
            break;

         case FL_UNLOCK:
            bResult = ( _locking( hFileHandle, _LK_UNLOCK, ulLength ) == 0 );
            break;

         default:
            bResult = FALSE;
      }
      s_uiErrorLast = errno;

      hb_fsSeek( hFileHandle, ulOldPos, FS_SET );
   }

#elif defined(__GNUC__) && defined(HB_OS_UNIX)

   {
      /* TODO: check for append locks (SEEK_END)
       */
      struct flock lock_info;

      errno = 0;

      switch( uiMode )
      {
         case FL_LOCK:

            lock_info.l_type   = F_WRLCK;
            lock_info.l_start  = ulStart;
            lock_info.l_len    = ulLength;
            lock_info.l_whence = SEEK_SET;   /* start from the beginning of the file */
            lock_info.l_pid    = getpid();

            bResult = ( fcntl( hFileHandle, F_SETLK, &lock_info ) >= 0 );
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

      s_uiErrorLast = errno;
   }

#elif defined(HAVE_POSIX_IO) && !defined(__IBMCPP__) && ( !defined(__GNUC__) || defined(__DJGPP__) )

   errno = 0;
   switch( uiMode )
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
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

void    hb_fsCommit( FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", hFileHandle));

#if defined(__WATCOMC__)

   _dos_commit( hFileHandle );

#elif defined(HB_FS_FILE_IO) && !defined(HB_OS_OS2) && !defined(HB_OS_UNIX)

   {
      int dup_handle;

      errno = 0;

      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
         close( dup_handle );

      s_uiErrorLast = errno;
   }

#elif defined(HB_OS_OS2)

   {
      errno = 0;

      /* TODO: what about error code from DosResetBuffer() call? */
      DosResetBuffer( hFileHandle );

      s_uiErrorLast = errno;
   }

#elif defined(HB_OS_UNIX)

   /* NOTE: close() functions releases all lock regardles if it is an
    * original or duplicated file handle
   */
   #if defined(_POSIX_SYNCHRONIZED_IO)
      /* faster - flushes data buffers only, without updating directory info
      */
      s_uiErrorLast = ( fdatasync( hFileHandle ) < -1 ) ? FS_ERROR : 0;
   #else
      /* slower - flushes all file data buffers and i-node info
      */
      s_uiErrorLast = ( fsync( hFileHandle ) < -1 ) ? FS_ERROR : 0;
   #endif

#else

   s_uiErrorLast = FS_ERROR;

#endif
}

BOOL    hb_fsMkDir( BYTE * pDirname )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", (char*) pDirname));

#if defined(HB_OS_WIN_32)

   bResult = ( CreateDirectory( ( char * ) pDirname, NULL ) == 0 );
   s_uiErrorLast = GetLastError();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;

   #if !defined(__WATCOMC__) && !defined(__BORLANDC__) && !defined(__IBMCPP__) && !defined(__MINGW32__)
      bResult = ( mkdir( ( char * ) pDirname, S_IWUSR | S_IRUSR | S_IXUSR ) == 0 );
   #else
      bResult = ( mkdir( ( char * ) pDirname ) == 0 );
   #endif

   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

BOOL    hb_fsChDir( BYTE * pDirname )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", (char*) pDirname));

#if defined(HB_OS_WIN_32)

   bResult = ( SetCurrentDirectory( ( char * ) pDirname ) == 0 );
   s_uiErrorLast = GetLastError();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   bResult = ( chdir( ( char * ) pDirname ) == 0 );
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

BOOL    hb_fsRmDir( BYTE * pDirname )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", (char*) pDirname));

#if defined(HB_OS_WIN_32)

   bResult = ( RemoveDirectory( ( char * ) pDirname ) == 0 );
   s_uiErrorLast = GetLastError();

#elif defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   bResult = ( rmdir( ( char * ) pDirname ) == 0 );
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;

#endif

   return bResult;
}

/* NOTE: This is not thread safe function, it's there for compatibility. */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

BYTE *  hb_fsCurDir( USHORT uiDrive )
{
   static BYTE s_byDirBuffer[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDir(%hu)", uiDrive));

   hb_fsCurDirBuff( uiDrive, s_byDirBuffer, _POSIX_PATH_MAX + 1 );

   return ( BYTE * ) s_byDirBuffer;
}

/* NOTE: Thread safe version of hb_fsCurDir() */
/* NOTE: 0 = current drive, 1 = A, 2 = B, 3 = C, etc. */

USHORT  hb_fsCurDirBuff( USHORT uiDrive, BYTE * pbyBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDirBuff(%hu)", uiDrive));

   HB_SYMBOL_UNUSED( uiDrive );

   pbyBuffer[ 0 ] = '\0';

#if defined(HB_OS_WIN_32)

   GetCurrentDirectory( ulLen, ( char * ) pbyBuffer );
   s_uiErrorLast = GetLastError();

#elif defined(HAVE_POSIX_IO)

   errno = 0;
   getcwd( ( char * ) pbyBuffer, ulLen );
   s_uiErrorLast = errno;
    
#elif defined(__MINGW32__)

   errno = 0;
   _getdcwd( uiDrive, pbyBuffer, ulLen );
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

   /* Strip the leading drive spec, and leading backslash if there's one. */

   {
      BYTE * pbyStart = pbyBuffer;

      /* NOTE: A trailing underscore is not returned on this platform,
               so we don't need to strip it. [vszakats] */

      if( pbyStart[ 1 ] == ':' )
         pbyStart += 2;
      if( strchr( OS_PATH_DELIMITER_LIST, pbyStart[ 0 ] ) )
         pbyStart++;

      if( pbyBuffer != pbyStart )
         memmove( pbyBuffer, pbyStart, ulLen );
   }

   /* Strip the trailing (back)slash if there's one */

   {
      ULONG ulLen = strlen( ( char * ) pbyBuffer );

      if( strchr( OS_PATH_DELIMITER_LIST, pbyBuffer[ ulLen - 1 ] ) )
         pbyBuffer[ ulLen - 1 ] = '\0';
   }

   return s_uiErrorLast;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

USHORT  hb_fsChDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)

 #if defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      unsigned int uiSave;
      unsigned int uiTotal;

      /* 1 = A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiSave );

      _dos_setdrive( nDrive + 1, &uiTotal );
      _dos_getdrive( &uiTotal );
      if( ( nDrive + 1 ) == uiTotal )
      {
         uiResult = 0;
         s_uiErrorLast = 0;
      }
      else
      {
         _dos_setdrive( uiSave, &uiTotal );
         uiResult = FS_ERROR;
         s_uiErrorLast = FS_ERROR;
      }
   }
 #else
   {
      USHORT uiSave = _getdrive();

      errno = 0;
      _chdrive( nDrive + 1 );
      if( ( nDrive + 1 ) == _getdrive() )
      {
         uiResult = 0;
         s_uiErrorLast = errno;
      }
      else
      {
         _chdrive( uiSave );
         uiResult = FS_ERROR;
         s_uiErrorLast = FS_ERROR;
      }
   }
 #endif
#else

   uiResult = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

/* TOFIX: This isn't fully compliant because CA-Cl*pper doesn't access
          the drive before checking. hb_fsIsDrv only returns TRUE
          if there is a disk in the drive. */

USHORT  hb_fsIsDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)

 #if defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      unsigned int uiSave;
      unsigned int uiTotal;

      /* 1=  A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiSave );

      s_uiErrorLast = 0;
      uiResult = 0;
      _dos_setdrive( nDrive + 1, &uiTotal );
      _dos_getdrive( &uiTotal );
      if( ( nDrive + 1 ) != uiTotal )
      {
         s_uiErrorLast = FS_ERROR;
         uiResult = FS_ERROR;
      }
      _dos_setdrive( uiSave, &uiTotal );
   }
 #else
   {
      USHORT uiSave = _getdrive();

      errno = 0;
      _chdrive( nDrive + 1 );
      if( ( nDrive + 1 ) == _getdrive() )
      {
         uiResult = 0;
         s_uiErrorLast = errno;
      }
      else
      {
         uiResult = FS_ERROR;
         s_uiErrorLast = FS_ERROR;
      }

      _chdrive( uiSave );
   }
 #endif

#else

   uiResult = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

BOOL    hb_fsIsDevice( FHANDLE hFileHandle )
{
   BOOL bResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDevice(%p)", hFileHandle));

#if defined(HB_FS_DRIVE_LETTER)

   errno = 0;
   bResult = ( isatty( hFileHandle ) == 0 );
   s_uiErrorLast = errno;

#else

   bResult = FALSE;
   s_uiErrorLast = FS_ERROR;
   HB_SYMBOL_UNUSED( hFileHandle );

#endif

   return bResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */

BYTE    hb_fsCurDrv( void )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));

#if defined(HB_FS_DRIVE_LETTER)

   {
      errno = 0;
      uiResult = _getdrive();
      #if defined(__DJGPP__)
         /* _getdrive() returned a drive number, base 0. */
      #else
      if( uiResult < 65 )
      {
         /* _getdrive() returned a drive number, base 1. */
         uiResult--;
      }
      else
      {
         /* _getdrive() returned a drive letter. */
         uiResult -= 65;
      }
      #endif
      s_uiErrorLast = errno;
   }

#elif defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      unsigned uiDrive;

      /* 1 = A:, 2 = B:, 3 = C:, etc
       * _dos_*() functions don't set 'errno'
       */
      _dos_getdrive( &uiDrive );
      s_uiErrorLast = 0;
      uiResult = ( USHORT ) uiDrive - 1;
   }

#else

   uiResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( BYTE ) uiResult; /* Return the drive number, base 0. */
}

/* TODO: Implement hb_fsExtOpen */

FHANDLE hb_fsExtOpen( BYTE * pFilename, BYTE * pDefExt,
                      USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsExtOpen(%s, %s, %hu, %p, %p)", (char*) pFilename, (char*) pDefExt, uiFlags, pPaths, pError));

   s_uiErrorLast = FS_ERROR;

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( pDefExt );
   HB_SYMBOL_UNUSED( uiFlags );
   HB_SYMBOL_UNUSED( pPaths );
   HB_SYMBOL_UNUSED( pError );

   return s_uiErrorLast;
}

BOOL hb_fsEof( FHANDLE hFileHandle )
{
#if defined(__DJGPP__) || defined(__CYGWIN__) || defined(OS_UNIX_COMPATIBLE)
   long curPos = lseek( hFileHandle, 0L, SEEK_CUR );
   long endPos = lseek( hFileHandle, 0L, SEEK_END );
   long newPos = lseek( hFileHandle, curPos, SEEK_SET );
   if( newPos == -1L )
   {
      hb_fsSetError( errno );
   }
   else if( newPos != curPos )
   {
      hb_fsSetError( FS_ERROR );
   }
   return curPos >= endPos;
#else
   return eof( hFileHandle ) != 0;
#endif
}
