/*
 * $Id$
 */

/* NOTE: In DOS/DJGPP under WinNT4 hb_fsSeek( fhnd, offset < 0, FS_SET) will
         set the file pointer to the passed negative value, and the subsequent
         hb_fsWrite() call will fail. In CA-Clipper hb_fsSeek() will fail,
         the pointer will not be moved, and thus the hb_fsWrite() call will
         successfully write the buffer the current file position. [vszel]
 */

#include <ctype.h>
#include "extend.h"
#include "filesys.h"
#include "errorapi.h"

#if defined(__CYGWIN__)
   #include <mingw32/share.h>
#endif

#if defined(__GNUC__)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
   #include <fcntl.h>
   #include <errno.h>

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
   #if defined(__IBMCPP__) || defined(_MSC_VER)
      #include <direct.h>
   #else
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

#define MKLONG(_1,_2,_3,_4) (((long)_4)<<24)|(((long)_3)<<16)|(((long)_2)<<8)|_1
#define MKINT(_1,_2)        (((long)_2)<<8)|_1

extern int rename( const char *, const char * );

/* Convert HARBOUR flags to IO subsystem flags */

#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

static int convert_open_flags( USHORT uiFlags )
{
   /* by default FO_READ + FO_COMPAT is set */
   int result_flags = 0;

   result_flags |= O_BINARY;

   if( uiFlags == 0 )
      result_flags |= O_RDONLY | SH_COMPAT;

   /* read & write flags */
   if( uiFlags & FO_WRITE )
      result_flags |= O_WRONLY;

   if( uiFlags & FO_READWRITE )
      result_flags |= O_RDWR;

   /* shared flags */
   if( uiFlags & FO_EXCLUSIVE )
      result_flags |= SH_DENYRW;

   if( uiFlags & FO_DENYWRITE )
      result_flags |= SH_DENYWR;

   if( uiFlags & FO_DENYREAD )
      result_flags |= SH_DENYRD;

   if( uiFlags & FO_DENYNONE )
      result_flags |= SH_DENYNO;

   if( uiFlags & FO_SHARED )
      result_flags |= SH_DENYNO;

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
      *result_pmode = S_IRUSR;

   if( uiFlags & FC_HIDDEN )
      *result_flags |= 0;

   if( uiFlags & FC_SYSTEM )
      *result_flags |= 0;
}

#endif


/*
 * FILESYS.API FUNCTIONS --
 */

FHANDLE hb_fsOpen   ( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   hFileHandle = open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

      errno = 0;
      hFileHandle = _open( ( char * ) pFilename, convert_open_flags( uiFlags ) );
      s_uiErrorLast = errno;

   #else

      hFileHandle = FS_ERROR;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return hFileHandle;
}

FHANDLE hb_fsCreate ( BYTE * pFilename, USHORT uiFlags )
{
   FHANDLE hFileHandle;
   int oflag;
   unsigned pmode;

   s_uiErrorLast = 0;

#if defined(HAVE_POSIX_IO)

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

   #if defined(_MSC_VER)

      errno = 0;
      convert_create_flags( uiFlags, &oflag, &pmode );
      hFileHandle = _open( ( char * ) pFilename, oflag, pmode );
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

#endif

   return hFileHandle;
}

void    hb_fsClose  ( FHANDLE hFileHandle )
{
#if defined(HAVE_POSIX_IO)

   errno = 0;
   close( hFileHandle );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

      errno = 0;
      _close( hFileHandle );
      s_uiErrorLast = errno;

   #else

      s_uiErrorLast = FS_ERROR;

   #endif

#endif

}

USHORT  hb_fsRead   ( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   USHORT uiRead;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   uiRead = read( hFileHandle, pBuff, uiCount );
   s_uiErrorLast = errno;
   if( uiRead == ( USHORT )-1 )
      uiRead = 0;

#else

   #if defined(_MSC_VER)

      errno = 0;
      uiRead = _read( hFileHandle, pBuff, uiCount );
      s_uiErrorLast = errno;
      if( uiRead == ( USHORT )-1 )
         uiRead = 0;

   #else

      uiRead = 0;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return uiRead;
}

USHORT  hb_fsWrite  ( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiCount )
{
   USHORT uiWritten;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   uiWritten = write( hFileHandle, pBuff, uiCount );
   s_uiErrorLast = errno;
   if( uiWritten == ( USHORT )-1 )
      uiWritten = 0;

#else

   #if defined(_MSC_VER)

      errno = 0;
      uiWritten = _write( hFileHandle, pBuff, uiCount );
      s_uiErrorLast = errno;
      if( uiWritten == ( USHORT )-1 )
         uiWritten = 0;

   #else

      uiWritten = 0;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return uiWritten;
}

ULONG   hb_fsSeek   ( FHANDLE hFileHandle, LONG lOffset, USHORT uiFlags )
{
   ULONG ulPos;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   ulPos = lseek( hFileHandle, lOffset, convert_seek_flags( uiFlags ) );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

      errno = 0;
      ulPos = _lseek( hFileHandle, lOffset, convert_seek_flags( uiFlags ) );
      s_uiErrorLast = errno;

   #else

      ulPos = 0;
      s_uiErrorLast = FS_ERROR;

   #endif

#endif

   return ulPos;
}

USHORT  hb_fsError  ( void )
{
   return s_uiErrorLast;
}

void    hb_fsDelete ( BYTE * pFilename )
{
#if defined(HAVE_POSIX_IO)

   errno = 0;
   unlink( ( char * ) pFilename );
   s_uiErrorLast = errno;

#else

   #if defined(_MSC_VER)

      errno = 0;
      remove( ( char *) pFilename );
      s_uiErrorLast = errno;

   #else

      s_uiErrorLast = FS_ERROR;

   #endif

#endif
}

void    hb_fsRename ( BYTE * pOldName, BYTE * pNewName )
{
#if defined(HAVE_POSIX_IO) || defined(_MSC_VER)

   errno = 0;
   rename( ( char * ) pOldName, ( char * ) pNewName );
   s_uiErrorLast = errno;

#else

   s_uiErrorLast = FS_ERROR;

#endif
}

BOOL    hb_fsLock   ( FHANDLE hFileHandle, ULONG ulStart,
                      ULONG ulLength, USHORT uiMode )
{
   int iResult = 0;

#if defined(_MSC_VER)
   ULONG ulPos;
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
   }
   s_uiErrorLast = errno;

#else

#if defined(_MSC_VER)

   ulPos = hb_fsSeek( hFileHandle, ulStart, FS_SET );

   switch( uiMode )
   {
      case FL_LOCK:
         iResult = locking( hFileHandle, _LK_LOCK, ulLength );
         break;

      case FL_UNLOCK:
         iResult = locking( hFileHandle, _LK_UNLCK, ulLength );
   }

   hb_fsSeek( hFileHandle, ulPos, FS_SET );

#else

   iResult = 1;
   s_uiErrorLast = FS_ERROR;

#endif

#endif

   return ( iResult ? FALSE : TRUE );
}

void    hb_fsCommit ( FHANDLE hFileHandle )
{
#if defined(HAVE_POSIX_IO)

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

BOOL    hb_fsMkDir  ( BYTE * pDirname )
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

BOOL    hb_fsChDir  ( BYTE * pDirname )
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

BOOL    hb_fsRmDir  ( BYTE * pDirname )
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

BYTE *  hb_fsCurDir ( USHORT uiDrive )
{
   static char cwd_buff[ PATH_MAX + 1 ];

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

/* TODO: Implement nDrive */

USHORT  hb_fsChDrv  ( BYTE * nDrive )
{
   USHORT iResult;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = 0;
   s_uiErrorLast = errno;
   s_uiErrorLast = FS_ERROR; /* TODO: Remove when function implemented */

#else

   iResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return iResult;
}

BYTE    hb_fsCurDrv ( void )
{
   USHORT iResult;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = 0;
   s_uiErrorLast = errno;
   s_uiErrorLast = FS_ERROR; /* TODO: Remove when function implemented */

#else

   iResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return iResult;
}

USHORT  hb_fsIsDrv  ( BYTE nDrive )
{
   USHORT iResult;

#if defined(HAVE_POSIX_IO)

   errno = 0;
   iResult = 0;
   s_uiErrorLast = errno;
   s_uiErrorLast = FS_ERROR; /* TODO: Remove when function implemented */

#else

   iResult = 0;
   s_uiErrorLast = FS_ERROR;

#endif

   return iResult;
}

/* TODO: Implement hb_fsExtOpen */
FHANDLE hb_fsExtOpen( BYTE * pFilename, BYTE * pDefExt,
                      USHORT uiFlags, BYTE * pPaths, PHB_ITEM pError )
{

   s_uiErrorLast = FS_ERROR;

   return FS_ERROR;
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HARBOUR HB_FOPEN( void )
{
   if( ISCHAR( 1 ) )
   {
      hb_retni( hb_fsOpen( ( BYTE * ) hb_parc( 1 ),
                           ISNUM( 2 ) ? hb_parni( 2 ) : FO_READ ) );
   }
   else
   {
      /* NOTE: Undocumented but existing Clipper Run-time error */
      hb_errRT_BASE( EG_ARG, 2021, NULL, "FOPEN" );
   }
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

   if( ISNUM( 1 ) && ( hb_parinfo( 2 ) & IT_STRING ) && ( hb_parinfo( 2 ) & IT_BYREF ) && ISNUM( 3 ) )
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
   if( ISCHAR( 1 ) )
   {
      hb_fsDelete( ( BYTE * ) hb_parc( 1 ) );
   }

   hb_retni( s_uiErrorLast );
}

HARBOUR HB_FRENAME( void )
{
   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      hb_fsRename( ( BYTE * ) hb_parc( 1 ),
                   ( BYTE * ) hb_parc( 2 ) );
   }

   hb_retni( s_uiErrorLast );
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

HARBOUR HB_FILE( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISCHAR( 1 ) )
      {

/* TODO: Check if F_OK is defined in all compilers */
#ifdef OS_UNIX_COMPATIBLE

         hb_retl( access( hb_parc( 1 ), F_OK ) == 0 );

#else

   #ifdef __MPW__

         int hFileHandle;

         if( ( hFileHandle = open( hb_parc( 1 ), O_RDONLY ) ) >= 0 )
         {
            close( hFileHandle );
            hb_retl( TRUE );
         }
         else
            hb_retl( FALSE );

   #else

         hb_retl( access( hb_parc( 1 ), 0 ) == 0 );

   #endif

#endif

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

         ulRead = hb_fsRead( ( FHANDLE ) hb_parni( 1 ), buffer, ulToRead );

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

HARBOUR HB_CURDIR( void )
{
   hb_retc( ( char * ) hb_fsCurDir( ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
      ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0 ) );
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

#define IS_PATH_SEP( c ) ( c == OS_PATH_DELIMITER )

/* Split given filename into path, name and extension */
PHB_FNAME hb_fsFNameSplit( char *szFilename )
{
   PHB_FNAME pName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   int iLen = strlen( szFilename );
   int iSlashPos;
   int iDotPos;
   int iPos;

   pName->szPath =
   pName->szName =
   pName->szExtension = NULL;

   iSlashPos = iLen - 1;
   iPos = 0;

   while( iSlashPos >= 0 && !IS_PATH_SEP( szFilename[ iSlashPos ] ) )
      --iSlashPos;

   if( iSlashPos == 0 )
   {
      /* root path -> \filename */
      pName->szBuffer[ 0 ] = OS_PATH_DELIMITER;
      pName->szBuffer[ 1 ] = '\0';
      pName->szPath = pName->szBuffer;
      iPos = 2;  /* first free position after the slash */
   }
   else if( iSlashPos > 0 )
   {
      /* path with separator -> path\filename */
      memcpy( pName->szBuffer, szFilename, iSlashPos );
      pName->szBuffer[ iSlashPos ] = '\0';
      pName->szPath = pName->szBuffer;
      iPos = iSlashPos + 1;   /* first free position after the slash */
   }

   iDotPos = iLen - 1;
   while( iDotPos > iSlashPos && szFilename[ iDotPos ] != '.' )
      --iDotPos;

   if( ( iDotPos - iSlashPos ) > 1 )
   {
      /* the dot was found
       * and there is at least one character between a slash and a dot
       */
      if( iDotPos == iLen - 1 )
      {
         /* the dot is the last character -use it as extension name */
         pName->szExtension = pName->szBuffer + iPos;
         pName->szBuffer[ iPos++ ] = '.';
         pName->szBuffer[ iPos++ ] = '\0';
      }
      else
      {
         pName->szExtension = pName->szBuffer + iPos;
         /* copy rest of the string with terminating ZERO character */
         memcpy( pName->szExtension, szFilename + iDotPos + 1, iLen - iDotPos );
         iPos += iLen - iDotPos;
      }
   }
   else
      /* there is no dot in the filename or it is  '.filename' */
      iDotPos = iLen;

   pName->szName = pName->szBuffer + iPos;
   memcpy( pName->szName, szFilename + iSlashPos + 1, iDotPos - iSlashPos - 1 );
   pName->szName[ iDotPos - iSlashPos - 1 ] = '\0';

   return pName;
}

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char *szFileName, PHB_FNAME pFileName )
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
      strcpy( szFileName + iLen, pFileName->szName );
   }
   else
      strcpy( szFileName, pFileName->szName );

   if( pFileName->szExtension )
   {
      int iLen = strlen( szFileName );

      if( !( pFileName->szExtension[ 0 ] == '.' || szFileName[ iLen-1 ] == '.') )
      {
         /* add extension separator only when extansion doesn't contain it */
         szFileName[ iLen++ ] = '.';
         szFileName[ iLen ] = '\0';
      }
      strcpy( szFileName + iLen, pFileName->szExtension );
   }

   return szFileName;
}

/* TOFIX:
If you call pFileName = hb_fsFNameSplit( "C:FILE.EXT" ) the result is:
   pFileName->szPath      => (null)       must be 'C:'
   pFileName->szName      => 'C:FILE'     must be 'FILE'
   pFileName->szExtension => '.EXT'       Ok!

If you call pFileName = hb_fsFNameSplit( "C:\FILE.EXT" ) the result is:
   pFileName->szPath      => 'C:'         must be 'C:\'
   pFileName->szName      => 'FILE'       Ok!
   pFileName->szExtension => '.EXT'       Ok!

If you call pFileName = hb_fsFNameSplit( "\FILE.EXT" ) the result is:
   pFileName->szPath      => '\'          Ok!
   pFileName->szName      => 'FILE'       Ok!
   pFileName->szExtension => '.EXT'       Ok!

If you call pFileName = hb_fsFNameSplit( "C:\DIR\FILE.EXT" ) the result
is:
   pFileName->szPath      => 'C:\DIR'     Ok!
   pFileName->szName      => 'FILE'       Ok!
   pFileName->szExtension => '.EXT'       Ok!

*/
