/*
 * $Id$
 */

#include <extend.h>
#include <init.h>
#include <filesys.h>
#include <string.h>
#include <errorapi.h>

#if defined(__CYGNUS__)
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

  #if !defined(_MSC_VER)
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
   #define O_BINARY 0   /* O_BINARY not defined on Linux */
#endif

#ifndef S_IEXEC
#define S_IEXEC  0x0040 /* owner may execute <directory search> */
#endif

#ifndef S_IRWXU
#define S_IRWXU  0x01c0 /* RWE permissions mask for owner */
#endif

#ifndef S_IRUSR
#define S_IRUSR  0x0100 /* owner may read */
#endif

#ifndef S_IWUSR
#define S_IWUSR  0x0080 /* owner may write */
#endif

#ifndef S_IXUSR
#define S_IXUSR  0x0040 /* owner may execute <directory search> */
#endif

#ifndef SH_COMPAT
#define SH_COMPAT       0x00    /* Compatibility */
#endif

#ifndef SH_DENYRW
#define SH_DENYRW       0x10    /* Deny read/write */
#endif

#ifndef SH_DENYWR
#define SH_DENYWR       0x20    /* Deny write */
#endif

#ifndef SH_DENYRD
#define SH_DENYRD       0x30    /* Deny read */
#endif

#ifndef SH_DENYNO
#define SH_DENYNO       0x40    /* Deny nothing */
#endif


#define IT_NUMBER       (IT_INTEGER|IT_LONG|IT_DOUBLE)

static USHORT last_error = 0;

#if !defined(PATH_MAX)
/* if PATH_MAX isn't defined, 256 bytes is a good number :) */
#define PATH_MAX 256
#endif

#define MKLONG(_1,_2,_3,_4) (((long)_4)<<24)|(((long)_3)<<16)|(((long)_2)<<8)|_1
#define MKINT(_1,_2)        (((long)_2)<<8)|_1

extern int rename( const char *, const char * );

HARBOUR HB_BIN2I( void );
HARBOUR HB_BIN2L( void );
HARBOUR HB_BIN2W( void );
HARBOUR HB_FCLOSE( void );
HARBOUR HB_FCREATE( void );
HARBOUR HB_FERASE( void );
HARBOUR HB_FERROR( void );
HARBOUR HB_FILE( void );
HARBOUR HB_FOPEN( void );
HARBOUR HB_FREAD( void );
HARBOUR HB_FREADSTR( void );
HARBOUR HB_FRENAME( void );
HARBOUR HB_FSEEK( void );
HARBOUR HB_FWRITE( void );
HARBOUR HB_I2BIN( void );
HARBOUR HB_L2BIN( void );
HARBOUR HB_W2BIN( void );

HB_INIT_SYMBOLS_BEGIN( Files__InitSymbols )
{ "BIN2I"   , FS_PUBLIC, HB_BIN2I   , 0 },
{ "BIN2L"   , FS_PUBLIC, HB_BIN2L   , 0 },
{ "BIN2W"   , FS_PUBLIC, HB_BIN2W   , 0 },
{ "FCLOSE"  , FS_PUBLIC, HB_FCLOSE  , 0 },
{ "FCREATE" , FS_PUBLIC, HB_FCREATE , 0 },
{ "FERASE"  , FS_PUBLIC, HB_FERASE  , 0 },
{ "FERROR"  , FS_PUBLIC, HB_FERROR  , 0 },
{ "FILE"    , FS_PUBLIC, HB_FILE    , 0 },
{ "FOPEN"   , FS_PUBLIC, HB_FOPEN   , 0 },
{ "FREAD"   , FS_PUBLIC, HB_FREAD   , 0 },
{ "FREADSTR", FS_PUBLIC, HB_FREADSTR, 0 },
{ "FSEEK"   , FS_PUBLIC, HB_FSEEK   , 0 },
{ "FWRITE"  , FS_PUBLIC, HB_FWRITE  , 0 },
{ "I2BIN"   , FS_PUBLIC, HB_I2BIN   , 0 },
{ "L2BIN"   , FS_PUBLIC, HB_L2BIN   , 0 },
{ "W2BIN"   , FS_PUBLIC, HB_W2BIN   , 0 }
HB_INIT_SYMBOLS_END( Files__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Files__InitSymbols
#endif

/* Convert HARBOUR flags to IO subsystem flags */

#if defined(HAVE_POSIX_IO)

static int convert_open_flags( int flags )
{
        /* by default FO_READ+FO_COMPAT is set */
        int result_flags = 0;

        result_flags |= O_BINARY;

        if( flags == 0 )
                result_flags |= O_RDONLY|SH_COMPAT;

        /* read & write flags */
        if( flags & FO_WRITE )
                result_flags |= O_WRONLY;

        if( flags & FO_READWRITE )
                result_flags |= O_RDWR;

        /* shared flags */
        if( flags & FO_EXCLUSIVE )
                result_flags |= SH_DENYRW;

        if( flags & FO_DENYWRITE )
                result_flags |= SH_DENYWR;

        if( flags & FO_DENYREAD )
                result_flags |= SH_DENYRD;

        if( flags & FO_DENYNONE )
                result_flags |= SH_DENYNO;

        if( flags & FO_SHARED )
                result_flags |= SH_DENYNO;

        return result_flags;
}

static int convert_seek_flags( int flags )
{
        /* by default FS_SET is set */
        int result_flags=0;

        result_flags = SEEK_SET;

        if( flags & FS_RELATIVE )
                result_flags = SEEK_CUR;

        if( flags & FS_END )
                result_flags = SEEK_END;

        return result_flags;
}

static void convert_create_flags( int flags, int *result_flags, unsigned *result_pmode )
{
        /* by default FC_NORMAL is set */

        *result_flags = O_BINARY | O_CREAT | O_TRUNC | O_RDWR;
        *result_pmode = S_IRUSR | S_IWUSR;

        if( flags & FC_READONLY )
                *result_pmode = S_IRUSR;

        if( flags & FC_HIDDEN )
                *result_flags |= 0;

        if( flags & FC_SYSTEM )
                *result_flags |= 0;
}

#endif


/*
 * FILESYS.API FUNCTIONS --
 */

FHANDLE hb_fsOpen   ( BYTEP name, USHORT flags )
{
        FHANDLE handle;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        handle = open((char *)name,convert_open_flags(flags));
        last_error = errno;
#else
        handle = FS_ERROR;
        last_error = FS_ERROR;
#endif
        return handle;
}

FHANDLE hb_fsCreate ( BYTEP name, USHORT flags )
{
        FHANDLE handle;
        int oflag;
        unsigned pmode;

#if defined(HAVE_POSIX_IO)
        errno = 0;
        convert_create_flags( flags, &oflag, &pmode );
        handle = open((char *)name,oflag,pmode);
        if( handle == FS_ERROR)
        {
           /* This if block is required, because errno will be set
              if the file did not exist and had to be created, even
              when the create is successful! */
           last_error = errno;
        }
#else
        handle = FS_ERROR;
        last_error = FS_ERROR;
#endif
        return handle;
}

void    hb_fsClose  ( FHANDLE handle )
{
#if defined(HAVE_POSIX_IO)
    close(handle);
    return;
#endif
}

USHORT  hb_fsRead   ( FHANDLE handle, BYTEP buff, USHORT count )
{
        USHORT bytes;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        bytes = read(handle,buff,count);
        last_error = errno;
        if( bytes == 65535U ) bytes = 0;
#else
        bytes = 0;
        last_error = FS_ERROR;
#endif
        return bytes;
}

USHORT  hb_fsWrite  ( FHANDLE handle, BYTEP buff, USHORT count )
{
        USHORT bytes;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        bytes = write(handle,buff,count);
        last_error = errno;
#else
        bytes = 0;
        last_error = FS_ERROR;
#endif
        return bytes;
}

ULONG   hb_fsSeek   ( FHANDLE handle, LONG offset, USHORT flags )
{
        ULONG position;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        position = lseek(handle,offset,convert_seek_flags(flags));
        last_error = errno;
#else
        position = 0;
        last_error = FS_ERROR;
#endif
        return position;
}

USHORT  hb_fsError  ( void )
{
        return last_error;
}

void    hb_fsDelete ( BYTEP name )
{
#if defined(HAVE_POSIX_IO)
        errno = 0;
        unlink(( char *)name );
        last_error = errno;
        return;
#endif
}

void    hb_fsRename ( BYTEP older, BYTEP newer )
{
#if defined(HAVE_POSIX_IO)
        errno = 0;
        rename( (char *)older, (char *)newer );
        last_error = errno;
        return;
#endif
}

BOOL    hb_fsLock   ( FHANDLE handle, ULONG start,
                      ULONG length, USHORT mode )
{
        int result=0;

#if defined(HAVE_POSIX_IO) && !defined(__GNUC__) && !defined(__IBMCPP__)
        errno = 0;
        switch( mode )
        {
           case FL_LOCK:
              result = lock(handle, start, length);
              break;

           case FL_UNLOCK:
              result = unlock(handle, start, length);
        }
        last_error = errno;
#else
        result = 1;
        last_error = FS_ERROR;
#endif

        return (result ? FALSE : TRUE );
}

void    hb_fsCommit ( FHANDLE handle )
{
#if defined(HAVE_POSIX_IO)

        int dup_handle;
        errno = 0;
        dup_handle = dup(handle);
        last_error = errno;
        if (dup_handle != -1)
        {
           close(dup_handle);
           last_error = errno;
        }

#endif
        return;
}

BOOL    hb_fsMkDir  ( BYTEP name )
{
        int result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
  #if !defined(__WATCOMC__) && !defined(__BORLANDC__) && !defined(__IBMCPP__)
        result = mkdir( (char *)name, S_IWUSR|S_IRUSR);
  #else
        result = mkdir( (char *)name );
  #endif
        last_error = errno;
#else
        result = 1;
        last_error = FS_ERROR;
#endif
        return (result ? FALSE : TRUE );
}

BOOL    hb_fsChDir  ( BYTEP name )
{
        int result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        result = chdir( (char *)name );
        last_error = errno;
#else
        result = 1;
        last_error = FS_ERROR;
#endif
        return (result ? FALSE : TRUE );
}

BOOL    hb_fsRmDir  ( BYTEP name )
{
        int result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        result = rmdir( (char *)name );
        last_error = errno;
#else
        result = 1;
        last_error = FS_ERROR;
#endif
        return (result ? FALSE : TRUE );
}

BYTEP   hb_fsCurDir ( USHORT uiDrive )
{
        static char cwd_buff[PATH_MAX+1];
#if defined(HAVE_POSIX_IO)
        errno = 0;
        getcwd(cwd_buff,PATH_MAX);
        last_error = errno;
#else
        cwd_buff[0] = 0;
        last_error = FS_ERROR;
#endif
#if defined(_MSC_VER)
   BYTEP dmm = (BYTEP)cwd_buff;
#endif
        return (BYTEP)cwd_buff;
}

USHORT  hb_fsChDrv  ( BYTEP nDrive )
{
        USHORT result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        result = 0;
        last_error = errno;
        last_error = FS_ERROR; /* TODO: Remove when function implemented */
#else
        result = 0;
        last_error = FS_ERROR;
#endif
        return result;
}

BYTE    hb_fsCurDrv ( void )
{
        USHORT result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        result = 0;
        last_error = errno;
        last_error = FS_ERROR; /* TODO: Remove when function implemented */
#else
        result = 0;
        last_error = FS_ERROR;
#endif
        return result;
}

USHORT  hb_fsIsDrv  ( BYTE nDrive )
{
        USHORT result;
#if defined(HAVE_POSIX_IO)
        errno = 0;
        result = 0;
        last_error = errno;
        last_error = FS_ERROR; /* TODO: Remove when function implemented */
#else
        result = 0;
        last_error = FS_ERROR;
#endif
        return result;
}

/* TODO: Implement hb_fsExtOpen */
FHANDLE hb_fsExtOpen( BYTEP fpFilename, BYTEP fpDefExt,
                      USHORT uiFlags, BYTEP fpPaths, ERRORP pError )
{
   return FS_ERROR;
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HARBOUR HB_FOPEN( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_STRING);
        PHB_ITEM arg2_it = hb_param(2,IT_NUMBER);

        int open_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                open_flags = hb_parni(2);
            else
                open_flags = 0;

            file_handle = hb_fsOpen( (BYTEP)hb_parc(1), open_flags );
        }
        else
        {
            hb_errorRT_BASE(EG_ARG, 2021, "Argument error", "FOPEN");
        }

        hb_retni(file_handle);
        return;
}

HARBOUR HB_FCREATE( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_STRING);
        PHB_ITEM arg2_it = hb_param(2,IT_NUMBER);

        int create_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                create_flags = hb_parni(2);
            else
                create_flags = 0;

            file_handle = hb_fsCreate( (BYTEP)hb_parc(1), create_flags );
        }

        hb_retni(file_handle);
        return;
}

HARBOUR HB_FREAD( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_NUMBER);
        PHB_ITEM arg2_it = hb_param(2,IT_STRING+IT_BYREF);
        PHB_ITEM arg3_it = hb_param(3,IT_NUMBER);

        long   bytes=0;

        if( arg1_it && arg2_it && arg3_it )
        {
            bytes = hb_fsRead(hb_parni(1), (BYTEP)hb_parc(2), hb_parnl(3) );
        }

        hb_retnl(bytes);
        return;
}

HARBOUR HB_FWRITE( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_NUMBER);
        PHB_ITEM arg2_it = hb_param(2,IT_STRING);
        PHB_ITEM arg3_it = hb_param(3,IT_NUMBER);

        long   bytes=0;

        if( arg1_it && arg2_it )
        {
            bytes = (arg3_it ? hb_parnl(3) : hb_parclen( 2 ) );
            bytes = hb_fsWrite( hb_parni(1), (BYTEP)hb_parc(2), bytes);
        }

        hb_retnl(bytes);
        return;
}

HARBOUR HB_FERROR( void )
{
        hb_retni(hb_fsError());
        return;
}

HARBOUR HB_FCLOSE( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_NUMBER);

        last_error = 0;
        if( arg1_it )
        {
            hb_fsClose(hb_parni(1));
        }
        hb_retl( last_error == 0 );
        return;
}

HARBOUR HB_FERASE( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_STRING);

        if( arg1_it )
        {
           hb_fsDelete( (BYTEP)hb_parc(1) );
        }

        hb_retni(last_error=0);
        return;
}

HARBOUR HB_FRENAME( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_STRING);
        PHB_ITEM arg2_it = hb_param(2,IT_STRING);

        if( arg1_it && arg2_it )
        {
            hb_fsRename( (BYTEP)hb_parc(1), (BYTEP)hb_parc(2) );
        }

        hb_retni(last_error);
        return;
}

HARBOUR HB_FSEEK( void )
{
        PHB_ITEM arg1_it = hb_param(1,IT_NUMBER);
        PHB_ITEM arg2_it = hb_param(2,IT_NUMBER);
        PHB_ITEM arg3_it = hb_param(3,IT_NUMBER);

        long bytes=0;
        int  pos;

        if( arg1_it && arg2_it )
        {
            pos = (arg3_it ? hb_parni(3) : FS_SET);
            bytes = hb_fsSeek(hb_parni(1),hb_parnl(2),pos);
        }

        hb_retnl(bytes);
        return;
}

HARBOUR HB_FILE( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_STRING );

        if( arg1_it )
        {
/*TODO: Check if F_OK is defined in all compilers */
#ifdef OS_UNIX_COMPATIBLE
           hb_retl( access(hb_parc(1), F_OK) == 0 );
#else
  #ifdef __MPW__
           int hFileHandle;

           if( (hFileHandle = open( hb_parc( 1 ), O_RDONLY )) >= 0 )
           {
               close( hFileHandle );
               hb_retl( 1 );
           }
           else hb_retl(0);
  #else
           hb_retl( access(hb_parc(1), 0) == 0 );
  #endif
#endif
        }
        else hb_retl(0);
        return;
}

HARBOUR HB_FREADSTR( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_NUMBER );
        PHB_ITEM arg2_it = hb_param( 2, IT_NUMBER );

        int    handle;
        long   bytes;
        long   nRead;
        long   readed;
        char * buffer;
        char   ch[1];

        if( arg1_it )
        {
           handle = hb_parni(1);
           bytes  = (arg2_it ? hb_parnl(2) : 0);
           buffer = ( char * ) hb_xgrab(bytes + 1);

           readed=0; ch[0]=1;
           while( readed < bytes )
           {
                 nRead = read(handle,ch,1);
                 if( nRead < 1 )
                        break;
                 buffer[readed]=ch[0];
                 readed++;
           }

           buffer[readed]=0;
           hb_retc(buffer);
           hb_xfree(buffer);
        }
        else
           hb_retc("");

        return;
}

HARBOUR HB_BIN2I( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_STRING );
        char * s;
        int    result=0;

        if( arg1_it )
        {
           s = hb_parc(1);
           if( hb_parclen(1) >= 2 )
                result = MKINT(s[0],s[1]);
           else
              result = 0;
        }

        hb_retni(result);
        return;
}

HARBOUR HB_BIN2L( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_STRING );
        char * s;
        long   result=0;

        if( arg1_it )
        {
           s = hb_parc(1);
           if( hb_parclen(1) >= 4 )
              result = MKLONG(s[0],s[1],s[2],s[3]);
           else
              result = 0;
        }

        hb_retni(result);
        return;
}

HARBOUR HB_BIN2W( void )
{
        HB_BIN2I();
}

HARBOUR HB_I2BIN( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_INTEGER );
        int n;
        char s[3];

        if( arg1_it )
        {
           n = hb_parni(1);
           s[0] = n & 0xFF;
           s[1] = (n & 0xFF00)>>8;
           s[2] = 0;
           hb_retclen(s,3);
        }
        else
           hb_retclen("\0\0",2);

        return;
}

HARBOUR HB_L2BIN( void )
{
        PHB_ITEM arg1_it = hb_param( 1, IT_LONG );
        long  n;
        char  s[5];

        if( arg1_it )
        {
           n = hb_parnl(1);
           s[0] =  n & 0x000000FF;
           s[1] = (n & 0x0000FF00)>>8;
           s[2] = (n & 0x00FF0000)>>16;
           s[3] = (n & 0xFF000000)>>24;
           s[4] = 0;
           hb_retclen(s,5);
        }
        else
           hb_retclen("\0\0\0\0",4);

        return;
}

HARBOUR HB_W2BIN( void )
{
        HB_I2BIN();
}

