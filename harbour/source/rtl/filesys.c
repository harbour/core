
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_fsSetError()
 *    hb_fsSetDevMode()
 *    hb_fsReadLarge()
 *    hb_fsWriteLarge()
 *    HB_CURDIR()
 *    HB_CURDRIVE()
 *    HB_DIRCHANGE()
 *    HB_MAKEDIR()
 *    HB_DIRREMOVE()
 *    HB_ISDISK()
 *    HB_DISKCHANGE()
 *    HB_DISKNAME()
 *    HB_DISKSPACE() (parts by Luiz Rafael Culik <culik@sl.conex.net>)
 *    HB_HB_FNAMESPLIT()
 *    HB_HB_FNAMEMERGE()
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_fsChDrv()
 *    hb_fsCurDrv()
 *    hb_fsIsDrv()
 *    hb_fsIsDevice()
 *    HB_FSETDEVMOD()
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    FOPEN()    Documentation
 *    FCLOSE()   Documentation
 *    FWRITE()   Documentation
 *    FSEEK()    Documentation
 *    FREAD()    Documentation
 *    FILE()     Documentation
 *    FREADSTR() Documentation
 *    FRENAME()  Documentation
 *    FERROR()   Documentation
 *    RENAME     Documentation
 *    ERASE      Documentation
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

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "filesys.h"
#include "errorapi.h"

#if defined(__CYGWIN__)
   #include <mingw32/share.h>
   #include <fcntl.h>
   #include <io.h>
#endif

#if defined(__GNUC__) && !defined(__MINGW32__)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
   #include <fcntl.h>
   #include <errno.h>

   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
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
#endif

#if defined(__WATCOMC__)
   #include <sys/stat.h>
   #include <share.h>
   #include <fcntl.h>
   #include <io.h>
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
   #include <io.h>
   #include <fcntl.h>
   #include <share.h>
   #include <direct.h>
   #include <io.h>
   #if defined(__BORLANDC__)
      #include <dir.h>
      #include <dos.h>
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

#ifdef __MPW__
   #include <fcntl.h>
#endif

#ifdef DOS
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

#if defined( _MSC_VER ) || defined(__MINGW32__) || defined(__IBMCPP__)
/* These compilers use sopen() rather than open(), because their
   versions of open() do not support combined O_ and SH_ flags */
   #define HB_FS_SOPEN
#endif

#if ( defined(HAVE_POSIX_IO) && ( defined(OS2) || defined(DOS) || defined(_Windows) ) && ! defined(__CYGWIN__) ) || defined(__MINGW32__)
/* These platforms and/or compilers have common drive letter support */
   #define HB_FS_DRIVE_LETTER
#endif

#if UINT_MAX == ULONG_MAX
   #define HB_FS_LARGE_OPTIMIZED
#else
   #define LARGE_MAX ( UINT_MAX - 1L )
#endif

extern int rename( const char *, const char * );

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

#if defined(HAVE_POSIX_IO) && ! defined(__IBMCPP__)

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

FHANDLE hb_fsCreate( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCreate(%p, %hu)", pFilename, uiFlags));

   s_uiErrorLast = 0;

#if defined(HB_FS_FILE_IO)

   errno = 0;
   convert_create_flags( uiFlags, &oflag, &pmode );
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

void    hb_fsClose( FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsClose(%p)", hFileHandle));

#if defined(HB_FS_FILE_IO)

   errno = 0;
   close( hFileHandle );
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
   uiRead = read( hFileHandle, pBuff, uiCount );
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
   #if defined(HB_FS_LARGE_OPTIMIZED)
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
               ulLeftToRead = 0L;
            }
            uiRead = read( hFileHandle, pPtr, uiToRead );
            /* -1 on bad hFileHandle
                0 on disk full
             */
            if( uiRead == ( USHORT ) -1 || uiRead == 0 )
               break;

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
   if( ulCount )
   #if defined(HB_FS_LARGE_OPTIMIZED)
      ulWritten = write( hFileHandle, pBuff, ulCount );
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
               ulLeftToWrite = 0L;
            }
            uiWritten = write( hFileHandle, pPtr, uiToWrite );
            /* -1 on bad hFileHandle
                0 on disk full
             */
            if( uiWritten == ( USHORT ) -1 || uiWritten == 0 )
               break;

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

   #if defined(HB_FS_FILE_IO)

      /* get current offset */
      errno = 0;
      ulPos = lseek( hFileHandle, 0, SEEK_CUR );
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

   #if defined(HB_FS_FILE_IO)

      errno = 0;
      ulPos = lseek( hFileHandle, lOffset, Flags );
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
   ulPos = lseek( hFileHandle, 0L, SEEK_CUR );
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

int     hb_fsDelete( BYTE * pFilename )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsDelete(%s)", (char*) pFilename));

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = unlink( ( char * ) pFilename );
   s_uiErrorLast = errno;

#elif defined(_MSC_VER) || defined(__MINGW32__)

   errno = 0;
   iResult = remove( ( char * ) pFilename );
   s_uiErrorLast = errno;

#else

   iResult = -1;
   s_uiErrorLast = FS_ERROR;

#endif

   return iResult;
}

int hb_fsRename( BYTE * pOldName, BYTE * pNewName )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRename(%s, %s)", (char*) pOldName, (char*) pNewName));

#if defined(HB_FS_FILE_IO)

   errno = 0;
   iResult = rename( ( char * ) pOldName, ( char * ) pNewName );
   s_uiErrorLast = errno;

#else

   iResult = -1;
   s_uiErrorLast = FS_ERROR;

#endif

   return iResult;
}

BOOL    hb_fsLock   ( FHANDLE hFileHandle, ULONG ulStart,
                      ULONG ulLength, USHORT uiMode )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsLock(%p, %lu, %lu, %hu)", hFileHandle, ulStart, ulLength, uiMode));

#if defined(HAVE_POSIX_IO) && !defined(__GNUC__) && !defined(__IBMCPP__)

   errno = 0;
   switch( uiMode )
   {
      case FL_LOCK:
         iResult = lock( hFileHandle, ulStart, ulLength );
         break;

      case FL_UNLOCK:
         iResult = unlock( hFileHandle, ulStart, ulLength );
         break;

      default:
         iResult = 0;
   }
   s_uiErrorLast = errno;

#elif defined(_MSC_VER)

   {
      ULONG ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

      errno = 0;
      switch( uiMode )
      {
         case FL_LOCK:
            iResult = locking( hFileHandle, _LK_LOCK, ulLength );
            break;

         case FL_UNLOCK:
            iResult = locking( hFileHandle, _LK_UNLCK, ulLength );
            break;

         default:
            iResult = 0;
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
            iResult = _locking( hFileHandle, _LK_LOCK, ulLength );
            break;

         case FL_UNLOCK:
            iResult = _locking( hFileHandle, _LK_UNLOCK, ulLength );
            break;

         default:
            iResult = 0;
      }
      s_uiErrorLast = errno;

      hb_fsSeek( hFileHandle, ulOldPos, FS_SET );
   }

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( iResult ? FALSE : TRUE );
}

void    hb_fsCommit( FHANDLE hFileHandle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsCommit(%p)", hFileHandle));

#if defined(HB_FS_FILE_IO)

   {
      int dup_handle;

      errno = 0;

      dup_handle = dup( hFileHandle );
      if( dup_handle != -1 )
         close( dup_handle );

      s_uiErrorLast = errno;
   }

#else

   s_uiErrorLast = FS_ERROR;

#endif
}

BOOL    hb_fsMkDir( BYTE * pDirname )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsMkDir(%s)", (char*) pDirname));

#if defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;

   #if !defined(__WATCOMC__) && !defined(__BORLANDC__) && !defined(__IBMCPP__) && !defined(__MINGW32__)
      iResult = mkdir( ( char * ) pDirname, S_IWUSR | S_IRUSR );
   #else
      iResult = mkdir( ( char * ) pDirname );
   #endif

   s_uiErrorLast = errno;

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( iResult ? FALSE : TRUE );
}

BOOL    hb_fsChDir( BYTE * pDirname )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDir(%s)", (char*) pDirname));

#if defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   iResult = chdir( ( char * ) pDirname );
   s_uiErrorLast = errno;

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( iResult ? FALSE : TRUE );
}

BOOL    hb_fsRmDir( BYTE * pDirname )
{
   int iResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsRmDir(%s)", (char*) pDirname));

#if defined(HAVE_POSIX_IO) || defined(__MINGW32__)

   errno = 0;
   iResult = rmdir( ( char * ) pDirname );
   s_uiErrorLast = errno;

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( iResult ? FALSE : TRUE );
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

#if defined(HAVE_POSIX_IO)

   errno = 0;
   getcwd( ( char * ) pbyBuffer, ulLen );
   s_uiErrorLast = errno;

#elif defined(__MINGW32__)

   {
      BYTE * pbyStart = pbyBuffer;

      errno = 0;
      _getdcwd( uiDrive, pbyBuffer, ulLen );
      s_uiErrorLast = errno;

      /* Strip the leading drive spec, and leading underscore. */
      /* NOTE: The trailing underscore is not returned on this platform */

      if( pbyStart[ 1 ] == ':' )
         pbyStart += 2;
      if( pbyStart[ 0 ] == '\\' )
         pbyStart++;

      if( pbyBuffer != pbyStart )
         memmove( pbyBuffer, pbyStart, ulLen );
   }

#else

   s_uiErrorLast = FS_ERROR;

#endif

   return s_uiErrorLast;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */
/* TODO: add documentation */

USHORT  hb_fsChDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsChDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)

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

#elif defined( __WATCOMC__ )

   {
      /* 'unsigned int' _have to_ be used in Watcom
       */
      unsigned int uiSave = _getdrive();
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

   uiResult = FS_ERROR;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

/* NOTE: 0=A:, 1=B:, 2=C:, 3=D:, ... */
/* TODO: add documentation */

/* TOFIX: This isn't fully compliant because CA-Cl*pper doesn't access
          the drive before checking. hb_fsIsDrv only returns TRUE
          if there is a disk in the drive. */

USHORT  hb_fsIsDrv( BYTE nDrive )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsIsDrv(%d)", (int) nDrive));

#if defined(HB_FS_DRIVE_LETTER)

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

#elif defined( __WATCOMC__ )

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
/* TODO: add documentation */

BYTE    hb_fsCurDrv( void )
{
   USHORT uiResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCurDrv()"));

#if defined(HB_FS_DRIVE_LETTER)

   {
      errno = 0;
      uiResult = _getdrive() - 1;
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
      uiResult = ( USHORT ) uiDrive -1;
   }

#else

   uiResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( BYTE ) uiResult;
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

/*
 * -- HARBOUR FUNCTIONS --
 */

/*  $DOC$
 *  $FUNCNAME$
 *      FOPEN()
 *  $CATEGORY$
 *      LOW LEVEL
 *  $ONELINER$
 *      Open a file.
 *  $SYNTAX$
 *     FOPEN(<cFile>, [<nMode>]) --> nHandle
 *  $ARGUMENTS$
 *     <cFile> Name of file to open
 *     <nMode> Dos file open mode
 *   file is to be accessed.  The open mode is composed of elements from the
 *   two types of modes described in the tables below.  If just the Access
 *   Mode is used, the file is opened non-sharable.  The default open mode is
 *   zero, which indicates non-sharable and read-only.
 *
 *
 *   FOPEN() Sharing Modes
 *   컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 *   Mode    Fileio.ch      Operation
 *   컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 *   0       FO_COMPAT      Compatibility mode (default)
 *   16      FO_EXCLUSIVE   Exclusive use
 *   32      FO_DENYWRITE   Prevent others from writing
 *   48      FO_DENYREAD    Prevent others from reading
 *   64      FO_DENYNONE    Allow others to read or write
 *   64      FO_SHARED      Same as FO_DENYNONE
 *   컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 *
 *     The Access Modes in combination (+) with the Sharing modes determine the
 *   accessibility of the file in a network environment.
 *
 *  $RETURNS$
 *      FOPEN() returns a DOS file handle
 *  $DESCRIPTION$
 *      This function opens a file expressed as <cFile> and returns a DOS
 *      file handle to be used with other low-level file functions.The
 *      value of <nMode> represents the status of the file to be opened;
 *      the default value is 0.The DOS file open modes are as the follows:
 *
 *      0          Read only
 *      1          Write only
 *      2          Read/write
 *      16         Exclusive read only
 *      17         Exclusive write only
 *      18         Exclusive read/write only
 *      32         Prevent others from writing
 *      33         Prevent others from reading
 *      34         Prevent read/write only
 *      48         Deny read only
 *      49         Deny write only
 *      50         Deny read/Write
 *      64         Share read only
 *      65         Share write only
 *      66         Share read/write
 *
 *      If there is an error in opening a file, a -1 will be returned by
 *      the function.Files handles may be in the range of 0 to 65535. The
 *      status of the SET DEFAULT TO and SET PATH TO commands has no effect
 *      on this function.Directory names and paths must be specified along
 *      with the file that is to be opened.
 *      If an error has occured, see the returns values from FERROR() for
 *      possible reasons for the error.
 *  $EXAMPLES$
 *      IF (nH:=FOPEN('X.TXT',66)<0
 *         ? 'File cann't be open'
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is CA-Clipper compliant
 *  $SEEALSO$
 *      FCREATE(),FERROR(),FCLOSE()
 *  $INCLUDE$
 *  
 *  $END$
 */

HARBOUR HB_FOPEN( void )
{
   if( ISCHAR( 1 ) )
      hb_retni( hb_fsOpen( ( BYTE * ) hb_parc( 1 ),
                           ISNUM( 2 ) ? hb_parni( 2 ) : FO_READ | FO_COMPAT ) );
   else
      hb_errRT_BASE( EG_ARG, 2021, NULL, "FOPEN" ); /* NOTE: Undocumented but existing Clipper Run-time error */
}

/*  $DOC$
 *  $FUNCNAME$
 *      FCREATE()
 *  $CATEGORY$
 *      LOW LEVEL
 *  $ONELINER$
 *      Creates a file
 *  $SYNTAX$
 *      FCREATE(<cFile>, [<nAttribute>]) --> nHandle
 *  $ARGUMENTS$
 *      <cFile> is the name of the file to create.
 *
 *      <nAttribute> Numeric code for the DOS file attribute
 *
 *  $RETURNS$
 *      <nHandle>  Numeric expression
 *  $DESCRIPTION$
 *      This function creates a new file with a filename of <cFile>. The
 *      default value of <nAttribute> is 0 and is used to set the DOS
 *      attribute byte for the file being created by this function.
 *      The return value will be DOS file handle that is associated
 *      with the new file. This number will be between zero to 65,535,
 *      inclusive. If an error occurs, the return value of this function
 *      will be -1
 *      If the file <cFile> already exists, the existing file will be
 *      truncated to a file lenght of 0 bytes.
 *      If specified, the folowing table shows the value for <nAttribute>
 *      and their related meaning to the file <cFile> being created by
 *      this Function.
 *
 *      Value of <nAttribute>       File Attribute
 *          0                       Normal/Default,Read/Write
 *          1                       Read-only,Attemptinf to open for
 *                                  output returns an error
 *          2                       Hidden,Excluded from normal DIR
 *                                  search
 *          4                       Create,Excluded from normal DIR
 *                                  search
 *  $EXAMPLES$
 *      IF (nh:=FCREATE("TEST.TXT") <0
 *          ? "Can not create file"
 *      ENDIF 
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compliant
 *  $SEEALSO$
 *     FCLOSE(),FOPEN(),FWRITE(),FREAD(),FERROR()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FCREATE( void )
{
   if( ISCHAR( 1 ) )
      hb_retni( hb_fsCreate( ( BYTE * ) hb_parc( 1 ),
                             ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL ) );
   else
      hb_retni( FS_ERROR );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FREAD()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Reads a specified number of bytes from a file.
 *  $SYNTAX$
 *      FREAD(<nHandle>, @<cBuffer>, <nBytes>) --> nBytes
 *  $ARGUMENTS$
 *      <nHandle>     Dos file handle
 *      <cBufferVar>  Character expression passed by reference
 *      <nBytes>      Number of bytes to read.
 *  $RETURNS$
 *      FREAD() returns the number of bytes successfyly read from the file
 *      <nHandle>
 *  $DESCRIPTION$
 *      This function reads the characters from a DOS file whose file handle
 *      is <nHandle> into it character memory variable expressed as <cBuffer>.
 *      The function returns the number of bytes successfully read into
 *      <cBuffer>.
 *      The value of <nHandle> is obtained from either it call to the FOPEN()
 *      or the FCREATE() function.
 *      The <cBuffer> expression is passed by reference and must be defined
 *      before this function is called. It also must be at least the same
 *      length as <nBytes>.
 *      <nBytes> is the number of bytes to read, starting at the current DOS
 *      file pointer position. If this function is successful in reading
 *      the characters from the file, the length of <cBuffer> or the number
 *      of bytes specified in <nBytes> will be the value returned. The current
 *      DOS file pointer advances the number of bytes read with each succe-
 *      ssive read.The return value is the number of byte successfully read
 *      from the file <nHandle>. If a 0 is returned, or if the number of
 *      bytes read matches neither the length of <cBuffer> nor the specified
 *      value in <nBytes> an end-of-file condition has been reached.
 *  $EXAMPLES$
 *      cBuffer:=SPACE(500)
 *      IF (nH:=FOPEN('X.TXT))>0
 *         FREAD(Hh,@cBuffer,500)
 *         ? cbuffer
 *      ENDIF
 *      FCLOSE(nH)
 *  $TESTS$
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is not CA-Clipper compliant since may can read
 *     strings greather the 65K depending of platform.
 *  $SEEALSO$
 *     BIN2I(),BIN2L(),BIN2W(),FERROR(),FWRITE()
 *  $INCLUDE$
 *  $END$
 */

HARBOUR HB_FREAD( void )
{
   ULONG ulRead;

   if( ISNUM( 1 ) && ISCHAR( 2 ) && ISBYREF( 2 ) && ISNUM( 3 ) )
   {
      ulRead = hb_parnl( 3 );

      /* NOTE: CA-Clipper determines the maximum size by calling _parcsiz()
               instead of _parclen(), this means that the maximum read length
               will be one more than the length of the passed buffer, because
               the terminating zero could be used if needed. [vszakats] */

      if( ulRead <= hb_parcsiz( 2 ) )
         /* NOTE: Warning, the read buffer will be directly modified,
                  this is normal here ! [vszakats] */
         ulRead = hb_fsReadLarge( hb_parni( 1 ),
                                  ( BYTE * ) hb_parc( 2 ),
                                  ulRead );
      else
         ulRead = 0;
   }
   else
      ulRead = 0;

   hb_retnl( ulRead );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FWRITE()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Writes characters to a file
 *  $SYNTAX$
 *     FWRITE(<nHandle>, <cBuffer>, [<nBytes>])
 *      --> nBytesWritten
 *  $ARGUMENTS$
 *     <nHandle>  DOS file handle number.
 *     <cBuffer>  Character expression to be written.
 *     <nBytes>   The number of bytes to write.
 *  $RETURNS$
 *      FWRITE() returns the number of bytes successfully written.
 *  $DESCRIPTION$
 *      This function writes the contents of <cBuffer> to the file designa-
 *      ted by its file handle <nHandle>. If used, <nBytes> is the number of
 *      bytes in <cBuffer> to write.
 *      The returned value is the number of bytes successfully written to the
 *      DOS file. If the returned value is 0, an error has occurred (unless
 *      this is intended). A successful write occurs when the number returned
 *      by FWRITE() is equal to either LEN( <cBuffer>) or <nBytes>.
 *      The value of <cBuffer> is the string or variable to be written to the
 *      open DOS file <nHandle>.
 *      The value of <nBytes> is the number of bytes to write out to the file.
 *      The disk write begins with the current file position in <nHandle>. If
 *      this variable is not used, the entire contents of <cBuffer> is written
 *      to the file.
 *      To truncate a file. a call of FWRITE( nHandle, "", 0 ) is needed.
 *  $EXAMPLES$
 *      nHandle:=FCREATE('x.txt')
 *      FOR X:=1 to 10
 *        FWRITE(nHandle,STR(x))
 *      NEXT
 *      FCLOSE(nHandle)
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is not CA-Clipper compatile since
 *      it can writes strings greather the 64K
 *  $SEEALSO$
 *      FCLOSE(),FCREATE(),FERROR(),FOPEN(),I2BIN(),L2BIN()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FWRITE( void )
{
   if( ISNUM( 1 ) && ISCHAR( 2 ) )
      hb_retnl( hb_fsWriteLarge( hb_parni( 1 ),
                                 ( BYTE * ) hb_parc( 2 ),
                                 ISNUM( 3 ) ? hb_parnl( 3 ) : hb_parclen( 2 ) ) );
   else
      hb_retnl( 0 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FERROR()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Reports the error status of low-level file functions
 *  $SYNTAX$
 *      FERROR() --> <nErrorCode>
 *  $ARGUMENTS$
 *
 *  $RETURNS$
 *      <nErrorCode> Value of the DOS error last encountered by a
 *      low-level file function.
 *
 *      FERROR() Return Values
 *   
 *      Error   Meaning
 *   
 *      0       Successful
 *      2       File not found
 *      3       Path not found
 *      4       Too many files open
 *      5       Access denied
 *      6       Invalid handle
 *      8       Insufficient memory
 *      15      Invalid drive specified
 *      19      Attempted to write to a write-protected disk
 *      21      Drive not ready
 *      23      Data CRC error
 *      29      Write fault
 *      30      Read fault
 *      32      Sharing violation
 *      33      Lock Violation
 *   
 *  $DESCRIPTION$
 *      After every low-level file function,this function will return
 *      a value that provides additional informationon the status of
 *      the last low-level file functions's performance.If the FERROR()
 *      function returns a 0, no error was detected.Below is a table
 *      of possibles values returned by the FERROR() function.
 *
 *  $EXAMPLES$
 *   ^CFE  This example tests FERROR() after the creation of a binary
 *      file and displays an error message if the create fails:
 *
 *      #include "Fileio.ch"
 *      //
 *      nHandle := FCREATE("Temp.txt", FC_NORMAL)
 *      IF FERROR() != 0
 *         ? "Cannot create file, DOS error ", FERROR()
 *      ENDIF
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compatible
 *  $SEEALSO$
 *     FCLOSE(),FERASE(),FOPEN(),FWRITE()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FERROR( void )
{
   hb_retni( hb_fsError() );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FCLOSE()
 *  $CATEGORY$
 *     Low Level
 *  $ONELINER$
 *     Closes an open file
 *  $SYNTAX$
 *     FCLOSE(<nHandle>) --> <lSuccess>
 *  $ARGUMENTS$
 *     <nHandle> DOS file handle
 *  $RETURNS$
 *     <lSuccess>  Logical TRUE (.T.) or FALSE (.F.)
 *  $DESCRIPTION$
 *     This function closes an open file with a dos file handle
 *     of <nHandle> and writes the associated DOS buffer to the
 *     disk. The <nHandle> value is derived from the FCREATE()
 *     or FOPEN() function.
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compliant
 *  $SEEALSO$
 *     FOPEN(),FCREATE(),FREAD(),FWRITE(),FERROR()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FCLOSE( void )
{
   s_uiErrorLast = 0;

   if( ISNUM( 1 ) )
   {
      hb_fsClose( hb_parni( 1 ) );
      hb_retl( s_uiErrorLast == 0 );
   }
   else
      hb_retl( FALSE );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FERASE()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Erase a file from disk
 *  $SYNTAX$
 *      FERASE(<cFile>) --> nSuccess
 *  $ARGUMENTS$
 *      <cFile> Name of file to erase.
 *  $RETURNS$
 *      <nSuccess> 0 if successful, -1 if not
 *  $DESCRIPTION$
 *      This function deletes the file specified in <cFile> from the disk.
 *      No extensions are assumed. The drive and path my be included in
 *      <cFile>; neither the SET DEFAULT not the SET PATH command controls
 *      the performance of this function.If the drive or path is not used,
 *      the function will look for the file only on the currently selected
 *      direcytory on the logged drive.
 *
 *      If the function is able to successfully delete the file from the
 *      disk, the value of the function will be 0; otherwise a -1 will
 *      be returned.If not successfu, aditional information may be
 *      obtained by calling the FERROR() function.
 *      Note: Any file to be removed by FERASE() must still be closed.
 *
 *  $EXAMPLES$
 *      IF (FERASE("TEST.TXT")==0)
 *          ? "File successfully erased"
 *      ELSE
 *          ? "File can not be deleted"
 *      ENDIF 
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper Compatible
 *  $SEEALSO$
 *     FERROR(),FRENAME()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FERASE( void )
{
   s_uiErrorLast = 3;

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsDelete( ( BYTE * ) hb_parc( 1 ) ) );
   else
      hb_retni( -1 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FRENAME()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Renames a file
 *  $SYNTAX$
 *      FRENAME(<cOldFile>, <cNewFile>) --> nSuccess
 *  $ARGUMENTS$
 *     <cOldFile> Old filenarne to he changed
 *     <cNewFile> New filename
 *  $RETURNS$
 *     FRENAME() returns If sucessful, a 0 will he returned otherwise,
 *     a -1 will be returned.
 *  $DESCRIPTION$
 *      This function renames the specified file <cOldFile> to <cNewFile>.
 *      A filename and/or directory name may be specified for either para-
 *      meter. However, if a path is supplied as part of <cNewFile> and
 *      this path is different from either the path specified in <cOldFile>
 *      or (if none is used) the current drive and directory, the function
 *      will not execute successfully.
 *      Neither parameter is subject to the control of the SET PATH TO or
 *      SET DEFAULT TO commands. In attempting to locate the file to be
 *      renamed, this function will search the default drive and directory
 *      or the drive and path specified in <cOldFile>. It will not search
 *      directories named by the SET PATH TO and SET DEFAULT TO commands
 *      or by the DOS PATH statement.
 *      If the file specified in <cNewFile> exists or the file is open,
 *      the function will be unable to rename the file.If the function
 *      is unable to complete its operation,it will return a value of -1.
 *      If it is able to rename the file, the return value for the function
 *      will be 0.A call to FERROR() function will give additional infor-
 *      mation about any error found.

 *  $EXAMPLES$
 *      nResult:=FRENAME("x.txt","x1.txt")
 *      IF nResult <0
 *         ? "File could not be renamed."
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compliant
 *  $SEEALSO$
 *     ERASE,FERASE(),FERROR(),FILE(),RENAME
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FRENAME( void )
{
   s_uiErrorLast = 2;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
      hb_retni( hb_fsRename( ( BYTE * ) hb_parc( 1 ), ( BYTE * ) hb_parc( 2 ) ) );
   else
      hb_retni( -1 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FSEEK()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Positions the file pointer in a file.
 *  $SYNTAX$
 *      FSEEK(<nHandle>, <nOffset>, [<nOrigin>]) --> nPosition
 *  $ARGUMENTS$
 *     <nHandle> DOS file handle.
 *     <nOffset> The number of bytes to move.
 *     <nOrigin> The relative position in the file.
 *  $RETURNS$
 *      FSEEK() return the current position relative to begin-of-file
 *  $DESCRIPTION$
 *      This function sets the file pointer in the file whose DOS file
 *      handle is <nHandle> and moves the file pointer by <expN2> bytes
 *      from the file position designated by <nOrigin>. The returned value
 *      is the relative position of the file pointer to the beginning-of-file
 *      marker once the operation has been completed.
 *      <nHandle> is the file handle number. It is obtained from the FOPEN()
 *      or FCREATE() function.
 *      The value of <nOffSet> is the number of bytes to move the file pointer
 *      from the position determined by <nOrigin>.The value of <nOffset> may
 *      be a negative number, suggesting backward movement.
 *      The value of <nOrigin> designates the starting point from which the
 *      file pointer should he moved, as shown in the following table:
 *
 *      <nOrigin> File position
 *
 *        0     Beginning of file
 *        1     Current file pointer position
 *        2     End of file
 *
 *      If a value is not provided for <nOrigin>, it defaults to 0 and
 *      moves the file pointer from the beginning of the file.
 *  $EXAMPLES$
 *      STATIC FUNCTION FREADln(nH,cB,nMaxLine)
 *   
 *      LOCAL cLine,nSavePos,nEol,nNumRead
 *
 *      cLine:=space(nMaxLine)
 *
 *      cB:=''
 *
 *      nSavePos:=FTELL(nH)
 *
 *      nNumRead:=FREAD(nH,@cLine,nMaxLine)
 *
 *      IF (nEol:= AT(CRLF,RETURN(cLine,1,nNumRead)))==0
 *        cB:=cLine
 *      ELSE
 *        cB:=SUBSTR(cLine,1,nEol-1)
 *        FSEEK(nH,nSavePos+nEol+1,FS_SET)
 *      ENDIF
 *
 *      RETURN nNumRead != 0
 *  $TESTS$
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compliant.
 *  $SEEALSO$
 *     FCREATE(),FERROR(),FOPEN(),FREAD(),FREADSTR(),FWRITE()
 *  $INCLUDE$
 *     Fileio.ch
 *  $END$
 */

HARBOUR HB_FSEEK( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_retnl( hb_fsSeek( hb_parni( 1 ),
                           hb_parnl( 2 ),
                           ISNUM( 3 ) ? hb_parni( 3 ) : FS_SET ) );
   else
      hb_retnl( 0 );
}

BOOL hb_fsFile( BYTE * pFilename )
{
   BOOL bIsFile;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsFile(%s)", (char*) pFilename));

/* TODO: Check if F_OK is defined in all compilers */
#ifdef OS_UNIX_COMPATIBLE

   bIsFile = ( access( ( const char * ) pFilename, F_OK ) == 0 );

#elif defined(__MPW__)
   {
      int hFileHandle;

      if( ( hFileHandle = open( pFilename, O_RDONLY ) ) >= 0 )
      {
         close( hFileHandle );
         bIsFile = TRUE;
      }
      else
         bIsFile = FALSE;
   }
#else

   bIsFile = ( access( ( const char * ) pFilename, 0 ) == 0 );

#endif

   return bIsFile;
}

/*  $DOC$
 *  $FUNCNAME$
 *      FILE()
 *  $CATEGORY$
 *      Low Level
 *  $ONELINER$
 *      Tests for the existence of file(s)
 *  $SYNTAX$
 *      FILE(<cFileSpec>) --> lExists
 *  $ARGUMENTS$
 *      <cFileSpec> Dos Skeleton or file name to find.
 *  $RETURNS$
 *      FILE() return a logical true (.T.) or logical false (.F.).    
 *  $DESCRIPTION$
 *      This function return a logical true (.T.) if the given filename
 *      <cFileSpec> exist.
 *      Dos skeletons symbols may be used in the filename in <cFileSpec>,
 *      as may the drive and/or path name. If a drive are not explicity
 *      specified,FILE() will first search the current drive and directory,
 *      and will look for the file in the directories specified by SET PATH
 *      TO and ST DEFAULT TO commands.However, this command does not look
 *      at the values in the DOS PATH command.
 *  $EXAMPLES$
 *      ? file('c:\harbour\doc\compiler.txt")
 *      ? file('c:/harbour/doc/subcodes.txt")
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is CA-Clipper compatible.
 *  $SEEALSO$
 *
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FILE( void )
{
   hb_retl( ISCHAR( 1 ) ? hb_fsFile( ( BYTE * ) hb_parc( 1 ) ) : FALSE );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FREADSTR()
 *  $CATEGORY$
 *     Low Level
 *  $ONELINER$
 *      Reads a string from a file.
 *  $SYNTAX$
 *      FREADSTR(<nHandle>, <nBytes>) --> cString
 *  $ARGUMENTS$
 *     <nHandle> DOS file handle number
 *     <nBytes>  Number of bytes to read
 *  $RETURNS$
 *     FREADSTR() return an characted expression
 *  $DESCRIPTION$
 *      This function returns a character string of <nBytes> bytes from a
 *      file whose DOS file handle is <nHandle>.
 *      The value of the file handle <nHandle> is obtained from either the
 *      FOPEN() or FCREATE() functions.
 *      The value of <nBytes> is the number of bytes to read from the file.
 *      The returned string will be the number of characters specified in
 *      <nBytes> or the number of bytes read before an end-of-file charac-
 *      ter (ASCII 26) is found.
 *      NOTE  This function is similar to the FREAD() function, except that
 *      it will not	read binary characters that may he required as part of
 *      a header of a file construct. Characters Such as CHR(0) and CHR(26)
 *      may keep this	function from performing its intended operation. In this
 *      event, the FREAD() function should he used in place of the FREADSTR()
 *      function.
 *  $EXAMPLES$
 *      IF ( nH := FOPEN("x.txt") ) > 0
 *         cStr := Freadstr(nH,100)
 *         ? cStr
 *      ENDIF
 *      FCLOSE(nH)
 *  $TESTS$
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This function is not CA-Clipper compliant since may read
 *     strings greather the 65K depending of platform.
 *  $SEEALSO$
 *     BIN2I(),BIN2L(),BIN2W(),FERROR(),FREAD(),FSEEK()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FREADSTR( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      ULONG ulToRead = hb_parnl( 2 );

      if( ulToRead > 0 )
      {
         FHANDLE fhnd = ( FHANDLE ) hb_parni( 1 );
         BYTE * buffer = ( BYTE * ) hb_xgrab( ulToRead + 1 );
         ULONG ulRead;

         ulRead = hb_fsReadLarge( fhnd, buffer, ulToRead );
         buffer[ ulRead ] = '\0';

         /* NOTE: Clipper will not return zero chars from this functions. */

         hb_retc( ( char * ) buffer );

         hb_xfree( buffer );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

/* NOTE: This function should not return the leading and trailing */
/*       (back)slashes. */

HARBOUR HB_CURDIR( void )
{
   USHORT uiErrorOld = s_uiErrorLast;
   BYTE * pbyBuffer = ( BYTE * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   hb_fsCurDirBuff( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
      ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0, pbyBuffer, _POSIX_PATH_MAX + 1 );

   hb_retc( ( char * ) pbyBuffer );
   hb_xfree( pbyBuffer );

   s_uiErrorLast = uiErrorOld;
}

#ifdef HB_COMPAT_C53

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DIRCHANGE( void )
{
   USHORT uiErrorOld = s_uiErrorLast;

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsChDir( ( BYTE * ) hb_parc( 1 ) ) ? 0 : s_uiErrorLast );
   else
      hb_retni( -1 );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */
/* NOTE: Clipper 5.3 NG incorrectly states that the name if this function is
         DIRMAKE(), in reality it's not. */

HARBOUR HB_MAKEDIR( void )
{
   USHORT uiErrorOld = s_uiErrorLast;

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsMkDir( ( BYTE * ) hb_parc( 1 ) ) ? 0 : s_uiErrorLast );
   else
      hb_retni( -1 );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DIRREMOVE( void )
{
   USHORT uiErrorOld = s_uiErrorLast;

   if( ISCHAR( 1 ) )
      hb_retni( hb_fsRmDir( ( BYTE * ) hb_parc( 1 ) ) ? 0 : s_uiErrorLast );
   else
      hb_retni( -1 );

   s_uiErrorLast = uiErrorOld;
}

#endif

HARBOUR HB_DISKSPACE( void )
{
   ULONG ulSpaceFree = 0;
   USHORT uiDrive = ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
                     ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0;

#if defined( DOS ) || defined( __WATCOMC__ )

   struct diskfree_t disk;
   unsigned uiResult;

   while( ( uiResult = _dos_getdiskfree( uiDrive, &disk ) ) != 0 )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( uiResult != 0 )
      ulSpaceFree = ( ULONG ) disk.avail_clusters *
                    ( ULONG ) disk.sectors_per_cluster *
                    ( ULONG ) disk.bytes_per_sector;

#elif defined(_Windows) || defined(WINNT)

   {
      char szPath[ 4 ];

      DWORD dwSectorsPerCluster;
      DWORD dwBytesPerSector;
      DWORD dwNumberOfFreeClusters;
      DWORD dwTotalNumberOfClusters;

      szPath[ 0 ] = uiDrive + 'A' - 1;
      szPath[ 1 ] = ':';
      szPath[ 2 ] = '\\';
      szPath[ 3 ] = '\0';

      if( GetDiskFreeSpace( szPath,
                            &dwSectorsPerCluster,
                            &dwBytesPerSector,
                            &dwNumberOfFreeClusters,
                            &dwTotalNumberOfClusters ) )
      {
         ulSpaceFree = dwNumberOfFreeClusters *
                       dwSectorsPerCluster *
                       dwBytesPerSector;
      }
   }

#else

   HB_SYMBOL_UNUSED( uiDrive );

#endif

   hb_retnl( ( LONG ) ulSpaceFree );
}

#ifdef HB_COMPAT_C53

/* NOTE: Clipper 5.3 undocumented */

HARBOUR HB_ISDISK()
{
   USHORT uiErrorOld = s_uiErrorLast;

   hb_retl( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
            hb_fsIsDrv( ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' ) ) == 0 :
            FALSE );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DISKCHANGE( void )
{
   USHORT uiErrorOld = s_uiErrorLast;

   hb_retl( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
            hb_fsChDrv( ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' ) ) == 0 :
            FALSE );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DISKNAME( void )
{
   USHORT uiErrorOld = s_uiErrorLast;
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';

   hb_retclen( szDrive, 1 );

   s_uiErrorLast = uiErrorOld;
}

#endif

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible */

HARBOUR HB_CURDRIVE( void )
{
   USHORT uiErrorOld = s_uiErrorLast;
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szDrive, 1 );

   if( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
   {
      if( hb_fsChDrv( ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' ) ) != 0 )
      {
         /* TODO: Throw some XBase++ like runtime error. [vszakats] */
      }
   }

   s_uiErrorLast = uiErrorOld;
}

#endif

HARBOUR HB_HB_FNAMESPLIT( void )
{
   if( ISCHAR( 1 ) )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      hb_storc( pFileName->szPath, 2 );
      hb_storc( pFileName->szName, 3 );
      hb_storc( pFileName->szExtension, 4 );
      hb_storc( pFileName->szDrive, 5 );

      hb_xfree( pFileName );
   }
}

HARBOUR HB_HB_FNAMEMERGE( void )
{
   HB_FNAME pFileName;
   char szFileName[ _POSIX_PATH_MAX ];

   pFileName.szPath = ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   pFileName.szName = ISCHAR( 2 ) ? hb_parc( 2 ) : NULL;
   pFileName.szExtension = ISCHAR( 3 ) ? hb_parc( 3 ) : NULL;
   pFileName.szDrive = ISCHAR( 4 ) ? hb_parc( 4 ) : NULL;

   hb_retc( hb_fsFNameMerge( szFileName, &pFileName ) );
}

#ifdef HB_COMPAT_C53

/* NOTE: Clipper 5.3 undocumented */

HARBOUR HB_FSETDEVMOD( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_fsSetDevMode( hb_parni( 1 ), hb_parni( 2 ) );
}

#endif

/* HARBOUR COMMANDS */

/*  $DOC$
 *  $FUNCNAME$
 *      RENAME
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Changes the name of a specified file
 *  $SYNTAX$
 *     RENAME <cOldFile> TO <cNewFile>
 *  $ARGUMENTS$
 *      <cOldFile> Old filename
 *      <cNewFile> New Filename
 *  $RETURNS$
 *  $DESCRIPTION$
 *      This command changes the name of <cOldFile> to <cNewFile>.Both
 *      <cOldFile> and <cNewFile> must include a file extension.This command
 *      if not affected by the SET PATH TO or SET DEFAULT TO commands;drive
 *      and directoy designaters must be specified if either file is in a
 *      directory other then the default drive and directory.
 *      If <cNewFile> id currently open or if it previously exists, this
 *      command will not perform the desired operation. 
 *  $EXAMPLES$
 *      RENAME c:\autoexec.bat to c:\autoexec.old
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This command is CA-Clipper compatible
 *  $SEEALSO$
 *      CURDIR(),ERASE,FILE(),FERASE(),FRENAME()
 *  $INCLUDE$
 *
 *  $END$
 */

/*  $DOC$
 *  $FUNCNAME$
 *      ERASE
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Remove a file from disk
 *  $SYNTAX$
 *      ERASE <xcFile>
 *  $ARGUMENTS$
 *      <xcFile> Name of file to remove
 *  $RETURNS$
 *  $DESCRIPTION$
 *      This command removes a file from the disk.The use of a drive,directo-
 *      ry,and wild-card skeleton operator is allowed for the root of the
 *      filename.The file extension is required.The SET DEFAULT and SET PATH
 *      commands do not affect this command.
 *      The file must be considered closed by the operating system before it
 *      may be deleted.
 *  $EXAMPLES$
 *      Erase c:\autoexec.bat
 *      Erase c:/temp/read.txt
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This command is CA-Clipper compatible
 *  $SEEALSO$
 *      CURDIR(),FILE(),FERASE(),DELETE FILE
 *  $INCLUDE$
 *
 *  $END$
 */

/*  $DOC$
 *  $FUNCNAME$
 *      DELETE FILE
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Remove a file from disk
 *  $SYNTAX$
 *      DELETE FILE <xcFile>
 *  $ARGUMENTS$
 *      <xcFile> Name of file to remove
 *  $RETURNS$
 *  $DESCRIPTION$
 *      This command removes a file from the disk.The use of a drive,directo-
 *      ry,and wild-card skeleton operator is allowed for the root of the
 *      filename.The file extension is required.The SET DEFAULT and SET PATH
 *      commands do not affect this command.
 *      The file must be considered closed by the operating system before it
 *      may be deleted.
 *  $EXAMPLES$
 *      Erase c:\autoexec.bat
 *      Erase c:/temp/read.txt
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This command is CA-Clipper compatible
 *  $SEEALSO$
 *      CURDIR(),FILE(),FERASE(),ERASE
 *  $INCLUDE$
 *
 *  $END$
 */
