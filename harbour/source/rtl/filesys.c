/*
 * $Id$
 */

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb_fsSetError()
      hb_fsSetDevMode()
      hb_fsReadLarge()
      hb_fsWriteLarge()
      HB_CURDIR()
      HB_DIRCHANGE()
      HB_MAKEDIR()
      HB_DIRREMOVE()
      HB_DISKCHANGE()
      HB_DISKNAME()
      HB_DISKSPACE() (parts by Luiz Rafael Culik <Culik@sl.conex.net>)
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following functions are Copyright 1999 Jose Lalin <dezac@corevia.com>:
      hb_fsChDrv()
      hb_fsCurDrv()
      hb_fsIsDrv()
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET) will
         set the file pointer to the passed negative value, and the subsequent
         hb_fsWrite() call will fail. In CA-Clipper hb_fsSeek() will fail,
         the pointer will not be moved, and thus the hb_fsWrite() call will
         successfully write the buffer to the current file position. [vszel]

   This has been corrected by ptucker
 */

#include <ctype.h>
#include "extend.h"
#include "filesys.h"
#include "errorapi.h"

#if defined(__CYGWIN__)
   #include <mingw32/share.h>
   #include <fcntl.h>
   #include <io.h>
#endif

#if defined(__GNUC__)
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

   #if !defined(HAVE_POSIX_IO)
      #define HAVE_POSIX_IO
   #endif
#endif

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(_MSC_VER)
   #include <sys\stat.h>
   #include <io.h>
   #include <fcntl.h>
   #include <share.h>
   #include <direct.h>
   #if defined(__BORLANDC__)
      #include <dir.h>
   #endif

   #if defined(_MSC_VER)
      #include <sys\locking.h>
   #else
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

#if !defined(PATH_MAX)
/* if PATH_MAX isn't defined, 256 bytes is a good number :) */
   #define PATH_MAX 256
#endif

#define MKLONG( _1, _2, _3, _4 ) ( ( ( long ) _4 ) << 24 ) | \
                                 ( ( ( long ) _3 ) << 16 ) | \
                                 ( ( ( long ) _2 ) <<  8 ) | _1
#define MKINT( _1, _2 )          ( ( ( long ) _2 ) <<  8 ) | _1

extern int rename( const char *, const char * );

/* Convert HARBOUR flags to IO subsystem flags */

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

#ifdef __CYGWIN__
/* TODO: Get Cygwin fixed so that this bug fix won't be needed */
static inline int FixCygwinIOflags( int flags )
{
   /* Starting with O_CREAT, the Cygwin I/O flags are 1 bit too high */
   return ( ( flags & 0x1FF00 ) >> 1 ) | ( flags & 0xFF );
}
#endif

static int convert_open_flags( USHORT uiFlags )
{
   /* by default FO_READ + FO_COMPAT is set */
   int result_flags = 0;

   result_flags |= O_BINARY;
/* DEBUG: printf("\nconvert_open_flags: O_BINARY"); */

#if defined( _MSC_VER )
   if( uiFlags == 0 )
   {
      result_flags |= O_RDONLY;
/* DEBUG: printf(" O_RDONLY"); */
   }
#else

   if( uiFlags == 0 )
   {
      result_flags |= ( O_RDONLY | SH_COMPAT );
/* DEBUG: printf(" O_RDONLY SH_COMPAT"); */
   }
#endif

   /* read & write flags */
   if( uiFlags & FO_WRITE )
   {
      result_flags |= O_WRONLY;
/* DEBUG: printf(" O_WRONLY"); */
   }

   if( uiFlags & FO_READWRITE )
   {
      result_flags |= O_RDWR;
/* DEBUG: printf(" O_RDWR"); */
   }

#if ! defined(_MSC_VER)
   /* shared flags */
   if( ( uiFlags & FO_DENYREAD ) == FO_DENYREAD )
   {
      result_flags |= SH_DENYRD;
/* DEBUG: printf(" SH_DENYRD"); */
   }

   else if( uiFlags & FO_EXCLUSIVE )
   {
      result_flags |= SH_DENYRW;
/* DEBUG: printf(" SH_DENYRW"); */
   }

   else if( uiFlags & FO_DENYWRITE )
   {
      result_flags |= SH_DENYWR;
/* DEBUG: printf(" SH_DENYWR"); */
   }

   if( uiFlags & FO_DENYNONE )
   {
      result_flags |= SH_DENYNO;
/* DEBUG: printf(" SH_DENYNO"); */
   }

   if( uiFlags & FO_SHARED )
   {
      result_flags |= SH_DENYNO;
/* DEBUG: printf(" SH_DENYNO"); */
   }
/* DEBUG: printf(" 0x%04x\n", result_flags); */
#ifdef __CYGWIN__
/* TODO: Get Cygwin fixed so that this bug fix won't be needed */
   result_flags = FixCygwinIOflags( result_flags );
/* DEBUG: printf(" Cygwin fix: 0x%04x\n", result_flags); */
#endif
#endif

   return result_flags;
}

static int convert_seek_flags( USHORT uiFlags )
{
   /* by default FS_SET is set */
   int result_flags = 0;

   result_flags = SEEK_SET;

   if( uiFlags & FS_RELATIVE )
      result_flags = SEEK_CUR;

   if( uiFlags & FS_END )
      result_flags = SEEK_END;

   return result_flags;
}

static void convert_create_flags( USHORT uiFlags, int * result_flags, unsigned * result_pmode )
{
   /* by default FC_NORMAL is set */

   *result_flags = O_BINARY | O_CREAT | O_TRUNC | O_RDWR;
   *result_pmode = S_IRUSR | S_IWUSR;

   if( uiFlags & FC_READONLY )
   {
      *result_pmode = S_IRUSR;
/* DEBUG: printf(" S_IRUSR"); */
   }

   if( uiFlags & FC_HIDDEN )
      *result_flags |= 0;

   if( uiFlags & FC_SYSTEM )
      *result_flags |= 0;
/* DEBUG: printf(" 0x%04x, 0x%04x\n", *result_flags, *result_pmode); */
#ifdef __CYGWIN__
/* TODO: Get Cygwin fixed so that this bug fix won't be needed */
   *result_flags = FixCygwinIOflags( *result_flags );
/* DEBUG: printf(" Cygwin fix: 0x%04x\n", *result_flags); */
#endif
}

#endif


/*
 * FILESYS.API FUNCTIONS --
 */

FHANDLE hb_fsOpen( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

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

   #else

      hFileHandle = FS_ERROR;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return hFileHandle;
}

FHANDLE hb_fsCreate( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   s_uiErrorLast = 0;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

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
#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

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

#if defined(__BORLANDC__) || defined(__IBMCPP__) || defined(__DJGPP__) || defined(__CYGWIN__)

   errno = 0;
   switch( uiDevMode )
   {
      case FM_BINARY:
         setmode( hFileHandle, O_BINARY );
         break;

      case FM_TEXT:
         setmode( hFileHandle, O_TEXT );
         break;
   }
   s_uiErrorLast = errno;

#elif defined(_MSC_VER)

   errno = 0;
   switch( uiDevMode )
   {
      case FM_BINARY:
         _setmode( hFileHandle, _O_BINARY );

      case FM_TEXT:
         _setmode( hFileHandle, _O_TEXT );
         break;
   }
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

}

USHORT  hb_fsRead( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   USHORT uiRead;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   errno = 0;
   uiRead = read( hFileHandle, pBuff, uiCount );
   s_uiErrorLast = errno;
   if( uiRead == ( USHORT )-1 )
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

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   errno = 0;
   uiWritten = write( hFileHandle, pBuff, uiCount );
   s_uiErrorLast = errno;
   if( uiWritten == ( USHORT )-1 )
      uiWritten = 0;

#else

   uiWritten = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiWritten;
}

ULONG   hb_fsReadLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   ULONG ulReadTotal = 0;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   errno = 0;
   while( ulReadTotal < ulCount )
   {
      USHORT uiRead = read( hFileHandle, pBuff, ( USHORT ) ( ulCount - ulReadTotal ) );

      /* -1 for bad hFileHandle or file is WriteOnly
          0 for EOF
       */
      if( uiRead == ( USHORT )-1  || uiRead == 0 )
         break;

      ulReadTotal += ( ULONG ) uiRead;

   }
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

   return ulReadTotal;
}

ULONG   hb_fsWriteLarge( FHANDLE hFileHandle, BYTE * pBuff, ULONG ulCount )
{
   ULONG ulWrittenTotal = 0;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   errno = 0;
   while( ulWrittenTotal < ulCount )
   {
      USHORT uiWritten = write( hFileHandle, pBuff, ( USHORT ) ( ulCount - ulWrittenTotal ) );

      /* -1 on bad hFileHandle
          0 on disk full
       */
      if( uiWritten == ( USHORT )-1 || uiWritten == 0 )
         break;

      ulWrittenTotal += ( ULONG ) uiWritten;

   }
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif

   return ulWrittenTotal;
}

ULONG   hb_fsSeek( FHANDLE hFileHandle, LONG lOffset, USHORT uiFlags )
{
   ULONG ulPos = -1;
   USHORT Flags = convert_seek_flags( uiFlags );

   if( lOffset < 0 && Flags == SEEK_SET )
   {
      /* 'Seek Error' */
      s_uiErrorLast = 25;

   #if defined(HAVE_POSIX_IO) || defined(_MSC_VER)
      /* get current offset */
      errno = 0;
      ulPos = lseek( hFileHandle, 0, SEEK_CUR );
      if( errno != 0 )
         s_uiErrorLast = errno;

   #endif

   }
   else
   {

   #if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

      errno = 0;
      ulPos = lseek( hFileHandle, lOffset, Flags );
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

USHORT  hb_fsError( void )
{
   return s_uiErrorLast;
}

void    hb_fsSetError( USHORT uiError )
{
   s_uiErrorLast = uiError;
}

int     hb_fsDelete( BYTE * pFilename )
{
   int iResult;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = unlink( ( char * ) pFilename );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

      errno = 0;
      iResult = remove( ( char * ) pFilename );
      s_uiErrorLast = errno;

   #else

      iResult = -1;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return iResult;
}

int hb_fsRename( BYTE * pOldName, BYTE * pNewName )
{
   int iResult;

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

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
   int iResult = 0;

#if defined(_MSC_VER)
   ULONG ulOldPos;
#endif

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
   }
   s_uiErrorLast = errno;

#else

#if defined(_MSC_VER)

   ulOldPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

   switch( uiMode )
   {
      case FL_LOCK:
         iResult = locking( hFileHandle, _LK_LOCK, ulLength );
         break;

      case FL_UNLOCK:
         iResult = locking( hFileHandle, _LK_UNLCK, ulLength );
         break;
   }

   hb_fsSeek( hFileHandle, ulOldPos, FS_SET );

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

#endif

   return ( iResult ? FALSE : TRUE );
}

void    hb_fsCommit( FHANDLE hFileHandle )
{
#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   int dup_handle;

   errno = 0;
   dup_handle = dup( hFileHandle );
   s_uiErrorLast = errno;

   if( dup_handle != -1 )
   {
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

#if defined(HAVE_POSIX_IO)

   errno = 0;

   #if !defined(__WATCOMC__) && !defined(__BORLANDC__) && !defined(__IBMCPP__)
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

#if defined(HAVE_POSIX_IO)

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

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = rmdir( ( char * ) pDirname );
   s_uiErrorLast = errno;

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

   return ( iResult ? FALSE : TRUE );
}

/* TODO: Make it thread safe */

BYTE *  hb_fsCurDir( USHORT uiDrive )
{
   static char cwd_buff[ PATH_MAX + 1 ];

   HB_SYMBOL_UNUSED( uiDrive );

#if defined(HAVE_POSIX_IO)

   errno = 0;
   getcwd( cwd_buff, PATH_MAX );
   s_uiErrorLast = errno;

#else

   cwd_buff[ 0 ] = '\0';
   s_uiErrorLast = FS_ERROR;

#endif

   return ( BYTE * ) cwd_buff;
}

USHORT  hb_fsChDrv( BYTE nDrive )
{
   USHORT uiResult;

#if defined(HAVE_POSIX_IO) && ( defined(OS2) || defined(DOS) || defined(_Windows) ) && ! defined(__CYGWIN__)

   USHORT uiSave = _getdrive();

   errno = 0;
   uiResult = _chdrive( nDrive );
   if( nDrive == _getdrive() )
   {
      s_uiErrorLast = errno;
   }
   else
   {
      _chdrive( uiSave );
      s_uiErrorLast = FS_ERROR;
   }

#else

   uiResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

BYTE    hb_fsCurDrv( void )
{
   USHORT uiResult;

#if defined(HAVE_POSIX_IO) && ( defined(OS2) || defined(DOS) || defined(_Windows) ) && ! defined(__CYGWIN__)

   errno = 0;
   uiResult = _getdrive();
   s_uiErrorLast = errno;

#else

   uiResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

USHORT  hb_fsIsDrv( BYTE nDrive )
{
   USHORT uiResult;

#if defined(HAVE_POSIX_IO) && ( defined(OS2) || defined(DOS) || defined(_Windows) ) && ! defined(__CYGWIN__)

   USHORT uiSave = _getdrive();

   errno = 0;
   _chdrive( nDrive );
   if( nDrive == _getdrive() )
   {
      uiResult = 1;
      s_uiErrorLast = errno;
   }
   else
   {
      uiResult = 0;
      _chdrive( uiSave );
      s_uiErrorLast = FS_ERROR;
   }

#else

   uiResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return uiResult;
}

/* TODO: Implement hb_fsExtOpen */

FHANDLE hb_fsExtOpen( BYTE * pFilename, BYTE * pDefExt,
                      USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError )
{

   s_uiErrorLast = FS_ERROR;

   HB_SYMBOL_UNUSED( pFilename );
   HB_SYMBOL_UNUSED( pDefExt );
   HB_SYMBOL_UNUSED( uiFlags );
   HB_SYMBOL_UNUSED( pPaths );
   HB_SYMBOL_UNUSED( pError );

   return FS_ERROR;
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HARBOUR HB_FOPEN( void )
{
   if( ISCHAR( 1 ) )
      hb_retni( hb_fsOpen( ( BYTE * ) hb_parc( 1 ),
                           ISNUM( 2 ) ? hb_parni( 2 ) : FO_READ | FO_COMPAT ) );
   else
      hb_errRT_BASE( EG_ARG, 2021, NULL, "FOPEN" ); /* NOTE: Undocumented but existing Clipper Run-time error */
}

HARBOUR HB_FCREATE( void )
{
   FHANDLE hFileHandle;

   if( ISCHAR( 1 ) )
      hFileHandle = hb_fsCreate( ( BYTE * ) hb_parc( 1 ),
                                 ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL );
   else
      hFileHandle = FS_ERROR;

   hb_retni( hFileHandle );
}

HARBOUR HB_FREAD( void )
{
   ULONG ulRead = 0;

   if( ISNUM( 1 ) && ISCHAR( 2 ) && ISBYREF( 2 ) && ISNUM( 3 ) )
   {
      ULONG ulToRead = hb_parnl( 3 );

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      /* CA-Clipper determines the maximum size by calling _parcsiz() instead */
      /* of hb_parclen(), this means that the maximum read length will be one */
      /* more then the length of the passed buffer, because the terminating */
      /* zero could be used if needed */

      if( ulToRead <= hb_parcsiz( 2 ) )
#else
      if( ulToRead <= hb_parclen( 2 ) )
#endif
      {
         /* NOTE: Warning, the read buffer will be directly modified ! */

         ulRead = hb_fsRead( hb_parni( 1 ),
                             ( BYTE * ) hb_parc( 2 ),
                             ulToRead );
      }
   }

   hb_retnl( ulRead );
}

HARBOUR HB_FWRITE( void )
{
   ULONG ulWritten;

   if( ISNUM( 1 ) && ISCHAR( 2 ) )
      ulWritten = hb_fsWrite( hb_parni( 1 ),
                              ( BYTE * ) hb_parc( 2 ),
                              ISNUM( 3 ) ? hb_parnl( 3 ) : hb_parclen( 2 ) );
   else
      ulWritten = 0;

   hb_retnl( ulWritten );
}

HARBOUR HB_FERROR( void )
{
   hb_retni( hb_fsError() );
}

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

HARBOUR HB_FERASE( void )
{
   int iResult;

   s_uiErrorLast = 3;

   if( ISCHAR( 1 ) )
      iResult = hb_fsDelete( ( BYTE * ) hb_parc( 1 ) );
   else
      iResult = -1;

   hb_retni( iResult );
}

HARBOUR HB_FRENAME( void )
{
   int iResult;

   s_uiErrorLast = 2;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
      iResult = hb_fsRename( ( BYTE * ) hb_parc( 1 ),
                            ( BYTE * ) hb_parc( 2 ) );
   else
      iResult = -1;

   hb_retni( iResult );
}

HARBOUR HB_FSEEK( void )
{
   ULONG ulPos;

   if( ISNUM( 1 ) && ISNUM( 2 ) )
      ulPos = hb_fsSeek( hb_parni( 1 ),
                         hb_parnl( 2 ),
                         ISNUM( 3 ) ? hb_parni( 3 ) : FS_SET );
   else
      ulPos = 0;

   hb_retnl( ulPos );
}

BOOL hb_fsFile ( BYTE * pFilename )
{
   BOOL is_file = FALSE;
/* TODO: Check if F_OK is defined in all compilers */
#ifdef OS_UNIX_COMPATIBLE

         is_file = ( access( (const char *)pFilename, F_OK ) == 0 );

#else

   #ifdef __MPW__

         int hFileHandle;

         if( ( hFileHandle = open( pFilename, O_RDONLY ) ) >= 0 )
         {
            close( hFileHandle );
            is_file = TRUE;
         }

   #else

         is_file = ( access( (const char *)pFilename, 0 ) == 0 );

   #endif

#endif

   return is_file;
}

HARBOUR HB_FILE( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISCHAR( 1 ) )
      {
         hb_retl( hb_fsFile( (BYTE *)hb_parc( 1 ) ) );
      }
      else
         hb_retl( FALSE );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "FILE" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_FREADSTR( void )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      ULONG ulToRead = hb_parnl( 2 );

      if( ulToRead > 0 )
      {
         BYTE * buffer = ( BYTE * ) hb_xgrab( ulToRead + 1 );
         ULONG ulRead;

         ulRead = hb_fsReadLarge( ( FHANDLE ) hb_parni( 1 ), buffer, ulToRead );

         buffer[ ulRead ] = '\0';

         /* NOTE: This is valid, Clipper will not return Chr(0) from FREADSTR() */
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

/* NOTE: Clipper 5.3 only */

HARBOUR HB_CURDIR( void )
{
   int uiErrorOld = s_uiErrorLast;

   hb_retc( ( char * ) hb_fsCurDir( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
      ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0 ) );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DIRCHANGE( void )
{
   int uiErrorOld = s_uiErrorLast;
   int iResult;

   if( ISCHAR( 1 ) )
   {
      if( hb_fsChDir( ( BYTE * ) hb_parc( 1 ) ) )
         iResult = 0;
      else
         iResult = s_uiErrorLast;
   }
   else
      iResult = -1;

   hb_retni( iResult );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */
/* NOTE: Clipper 5.3 NG incorrectly states that the name if this function is
         DIRMAKE(), in reality it's not. */

HARBOUR HB_MAKEDIR( void )
{
   int uiErrorOld = s_uiErrorLast;
   int iResult;

   if( ISCHAR( 1 ) )
   {
      if( hb_fsMkDir( ( BYTE * ) hb_parc( 1 ) ) )
         iResult = 0;
      else
         iResult = s_uiErrorLast;
   }
   else
      iResult = -1;

   hb_retni( iResult );

   s_uiErrorLast = uiErrorOld;
}

/* NOTE: Clipper 5.3 only */

HARBOUR HB_DIRREMOVE( void )
{
   int uiErrorOld = s_uiErrorLast;
   int iResult;

   if( ISCHAR( 1 ) )
   {
      if( hb_fsRmDir( ( BYTE * ) hb_parc( 1 ) ) )
         iResult = 0;
      else
         iResult = s_uiErrorLast;
   }
   else
      iResult = -1;

   hb_retni( iResult );

   s_uiErrorLast = uiErrorOld;
}

HARBOUR HB_DISKSPACE( void )
{
   ULONG ulSpaceFree = 0;
   USHORT uiDrive = ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
                     ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0;

#ifdef DOS
   struct diskfree_t disk;
   unsigned uiResult;

   while( ( uiResult = _dos_getdiskfree( uiDrive, &disk ) ) != 0 )
   {
      WORD wResult = hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT );

      if( wResult == E_DEFAULT || wResult == E_BREAK )
         break;
   }

   if( uiResult != 0 )
      ulSpaceFree = ( ( ULONG ) disk.avail_clusters *
                      ( ULONG ) disk.sectors_per_cluster *
                      ( ULONG ) disk.bytes_per_sector );
#else

   HB_SYMBOL_UNUSED( uiDrive );

#endif

   hb_retnl( ( LONG ) ulSpaceFree );
}

HARBOUR HB_DISKCHANGE( void )
{
   int uiErrorOld = s_uiErrorLast;

   hb_retl( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
            hb_fsChDrv( ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) ) == 0 :
            FALSE );

   s_uiErrorLast = uiErrorOld;
}

HARBOUR HB_DISKNAME( void )
{
   int uiErrorOld = s_uiErrorLast;
   char szDrive[ 1 ];

   szDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';

   hb_retclen( szDrive, 1 );

   s_uiErrorLast = uiErrorOld;
}

HARBOUR HB_BIN2I( void )
{
   int iResult = 0;

   if( ISCHAR( 1 ) )
   {
      char * szString = hb_parc( 1 );

      if( hb_parclen( 1 ) >= 2 )
         iResult = MKINT( szString[ 0 ], szString[ 1 ] );
   }

   hb_retni( iResult );
}

HARBOUR HB_BIN2L( void )
{
   long lResult = 0;

   if( ISCHAR( 1 ) )
   {
      char * szString = hb_parc( 1 );

      if( hb_parclen( 1 ) >= 4 )
         lResult = MKLONG( szString[ 0 ], szString[ 1 ], szString[ 2 ], szString[ 3 ] );
   }

   hb_retnl( lResult );
}

HARBOUR HB_BIN2W( void )
{
   HB_BIN2I();
}

HARBOUR HB_I2BIN( void )
{
   char szString[ 2 ];

   if( ISNUM( 1 ) )
   {
      int iValue = hb_parni( 1 );

      szString[ 0 ] =   iValue & 0x00FF;
      szString[ 1 ] = ( iValue & 0xFF00 ) >> 8;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] = '\0';
   }

   hb_retclen( szString, 2 );
}

HARBOUR HB_L2BIN( void )
{
   char szString[ 4 ];

   if( ISNUM( 1 ) )
   {
      long lValue = hb_parnl( 1 );

      szString[ 0 ] =   lValue & 0x000000FF;
      szString[ 1 ] = ( lValue & 0x0000FF00 ) >> 8;
      szString[ 2 ] = ( lValue & 0x00FF0000 ) >> 16;
      szString[ 3 ] = ( lValue & 0xFF000000 ) >> 24;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] =
      szString[ 2 ] =
      szString[ 3 ] = '\0';
   }

   hb_retclen( szString, 4 );
}

HARBOUR HB_W2BIN( void )
{
   HB_I2BIN();
}

#define IS_PATH_SEP( c ) ( strchr( OS_PATH_DELIMITER_LIST, ( c ) ) != NULL )

/* Split given filename into path, name and extension */
PHB_FNAME hb_fsFNameSplit( char * szFileName )
{
   PHB_FNAME pFileName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   int iLen = strlen( szFileName );
   int iSlashPos;
   int iDotPos;
   int iPos;

   pFileName->szPath =
   pFileName->szName =
   pFileName->szExtension = NULL;

   iSlashPos = iLen - 1;
   iPos = 0;

   while( iSlashPos >= 0 && !IS_PATH_SEP( szFileName[ iSlashPos ] ) )
      --iSlashPos;

   if( iSlashPos == 0 )
   {
      /* root path -> \filename */
      pFileName->szBuffer[ 0 ] = OS_PATH_DELIMITER;
      pFileName->szBuffer[ 1 ] = '\0';
      pFileName->szPath = pFileName->szBuffer;
      iPos = 2; /* first free position after the slash */
   }
   else if( iSlashPos > 0 )
   {
      /* If we are after a drive letter let's keep the following backslash */
      if( IS_PATH_SEP( ':' ) &&
         ( szFileName[ iSlashPos ] == ':' || szFileName[ iSlashPos - 1 ] == ':' ) )
      {
         /* path with separator -> d:\path\filename or d:path\filename */
         memcpy( pFileName->szBuffer, szFileName, iSlashPos + 1 );
         pFileName->szBuffer[ iSlashPos + 1 ] = '\0';
         iPos = iSlashPos + 2; /* first free position after the slash */
      }
      else
      {
         /* path with separator -> path\filename */
         memcpy( pFileName->szBuffer, szFileName, iSlashPos );
         pFileName->szBuffer[ iSlashPos ] = '\0';
         iPos = iSlashPos + 1; /* first free position after the slash */
      }

      pFileName->szPath = pFileName->szBuffer;
   }

   iDotPos = iLen - 1;
   while( iDotPos > iSlashPos && szFileName[ iDotPos ] != '.' )
      --iDotPos;

   if( ( iDotPos - iSlashPos ) > 1 )
   {
      /* the dot was found
       * and there is at least one character between a slash and a dot
       */
      if( iDotPos == iLen - 1 )
      {
         /* the dot is the last character - use it as extension name */
         pFileName->szExtension = pFileName->szBuffer + iPos;
         pFileName->szBuffer[ iPos++ ] = '.';
         pFileName->szBuffer[ iPos++ ] = '\0';
      }
      else
      {
         pFileName->szExtension = pFileName->szBuffer + iPos;
         /* copy rest of the string with terminating ZERO character */
         memcpy( pFileName->szExtension, szFileName + iDotPos + 1, iLen - iDotPos );
         iPos += iLen - iDotPos;
      }
   }
   else
      /* there is no dot in the filename or it is  '.filename' */
      iDotPos = iLen;

   if( ( iDotPos - iSlashPos - 1 ) > 0 )
   {
      pFileName->szName = pFileName->szBuffer + iPos;
      memcpy( pFileName->szName, szFileName + iSlashPos + 1, iDotPos - iSlashPos - 1 );
      pFileName->szName[ iDotPos - iSlashPos - 1 ] = '\0';
   }

/* DEBUG
   printf( "\nFilename: %s\n", szFileName );
   printf( "\n  szPath: %s\n", pFileName->szPath );
   printf( "\n  szName: %s\n", pFileName->szName );
   printf( "\n   szExt: %s\n", pFileName->szExtension );
*/

   return pFileName;
}

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char * szFileName, PHB_FNAME pFileName )
{
   if( pFileName->szPath && pFileName->szPath[ 0 ] )
   {
      /* we have not empty path specified */
      int iLen = strlen( pFileName->szPath );

      strcpy( szFileName, pFileName->szPath );

      /* if the path is a root directory then we don't need to add path separator */
      if( !( IS_PATH_SEP( pFileName->szPath[ 0 ] ) && pFileName->szPath[ 0 ] == '\0' ) )
      {
         /* add the path separator only in cases:
          *  when a name doesn't start with it
          *  when the path doesn't end with it
          */
         if( !( IS_PATH_SEP( pFileName->szName[ 0 ] ) || IS_PATH_SEP( pFileName->szPath[ iLen-1 ] ) ) )
         {
            szFileName[ iLen++ ] = OS_PATH_DELIMITER;
            szFileName[ iLen ] = '\0';
         }
      }
      if( pFileName->szName )
         strcpy( szFileName + iLen, pFileName->szName );
   }
   else
   {
      if( pFileName->szName )
         strcpy( szFileName, pFileName->szName );
   }

   if( pFileName->szExtension )
   {
      int iLen = strlen( szFileName );

      if( !( pFileName->szExtension[ 0 ] == '.' || szFileName[ iLen - 1 ] == '.') )
      {
         /* add extension separator only when extansion doesn't contain it */
         szFileName[ iLen++ ] = '.';
         szFileName[ iLen ] = '\0';
      }
      strcpy( szFileName + iLen, pFileName->szExtension );
   }

/* DEBUG
   printf( "\nMERGE:\n" );
   printf( "\n  szPath: %s\n", pFileName->szPath );
   printf( "\n  szName: %s\n", pFileName->szName );
   printf( "\n   szExt: %s\n", pFileName->szExtension );
   printf( "\nFilename result: %s\n", szFileName );
*/

   return szFileName;
}
