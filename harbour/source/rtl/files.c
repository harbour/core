/*
 * $Id$
 */

#include <extend.h>

#if defined(__GNUC__) || defined(__DJGPP__)
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <unistd.h>
  #include <fcntl.h>
  #include <errno.h>
#endif

#if defined(__DJGPP__)
  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif
#endif

#if defined(__WATCOMC__)
  #include <sys/stat.h>
  #include <share.h>
  #include <fcntl.h>
  #include <io.h>
  #include <errno.h>
  #include <direct.h>

  #if !defined(HAVE_POSIX_IO)
  #define HAVE_POSIX_IO
  #endif
#endif

#if defined(__BORLANDC__)
  #include <sys\stat.h>
  #include <io.h>
#endif

#define IT_NUMBER       (IT_INTEGER|IT_LONG|IT_DOUBLE)

static int last_error = 0;

#if !defined(PATH_MAX)
/* if PATH_MAX isn't defined, 256 bytes is a good number :) */
#define PATH_MAX 256
#endif

#define MKLONG(_1,_2,_3,_4) (((long)_4)<<24)|(((long)_3)<<16)|(((long)_2)<<8)|_1
#define MKINT(_1,_2)        (((long)_2)<<8)|_1

/* FLAGS TO FOPEN */
#define FO_READ         0
#define FO_WRITE        1
#define FO_READWRITE    2
#define FO_COMPAT       0
#define FO_EXCLUSIVE    16
#define FO_DENYWRITE    32
#define FO_DENYREAD     48
#define FO_DENYONE      64
#define FO_SHARE        FO_DENYONE

/* FLAGS TO FCREATE */
#define FC_NORMAL       0
#define FC_READONLY     1
#define FC_HIDDEN       2
#define FC_SYSTEM       4

/* FLAGS TO SEEK */
#define FS_SET          0
#define FS_RELATIVE     1
#define FS_END          2

/*
 * NOTE: for avoid include stdio.h,
 * this include define as an 'struct FILE'
 * and we have a HARBOUR function named FILE() this
 * made a name conflict
 */
extern int rename( const char *, const char * );

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

        if( flags & FO_DENYONE )
                result_flags |= SH_DENYNO;

        if( flags & FO_SHARE )
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

static int convert_create_flags( int flags )
{
        /* by default FC_NORMAL is set */
        int result_flags=S_IWUSR;

        if( flags & FC_READONLY )
                result_flags = result_flags & ~(S_IWUSR);

        if( flags & FC_HIDDEN )
                result_flags |= 0;

        if( flags & FC_SYSTEM )
                result_flags |= 0;

        return result_flags;
}

#endif


/*
 * FILESYS.API FUNCTIONS --
 */

int _fsOpen( char * name, int flags )
{
#if defined(HAVE_POSIX_IO)
        return open(name,convert_open_flags(flags));
#else
        return open(name, flags);
#endif
}

int _fsCreate( char * name, int flags )
{
#if defined(HAVE_POSIX_IO)
        return creat(name,convert_create_flags(flags));
#else
   /* TO DO create a file with attributes in flags
      for now create it normal */
       return creat(name,S_IWRITE);
#endif
}

int _fsClose( int handle )
{
#if defined(HAVE_POSIX_IO)
    return close(handle);
#else
    return close(handle);
#endif
}

long _fsRead( int handle, char * buff, long count )
{
#if defined(HAVE_POSIX_IO)
        return read(handle,buff,count);
#else
        return read(handle,buff,count);
#endif
}

long _fsWrite( int handle, char * buff, long count )
{
#if defined(HAVE_POSIX_IO)
        return write(handle,buff,count);
#else
        return write(handle,buff,count);
#endif
}

long _fsSeek( int handle, long offset, int flags )
{
#if defined(HAVE_POSIX_IO)
        return lseek(handle,offset,convert_seek_flags(flags));
#else
        return lseek(handle,offset,flags);
#endif
}

int _fsError()
{
        return last_error;
}

int _fsDelete( char * name )
{
#if defined(HAVE_POSIX_IO)
        unlink(name);
#else
   return unlink(name);
#endif
}

int _fsRename( char * older, char * newer )
{
#if defined(HAVE_POSIX_IO)
        return rename(older,newer);
#else
        return rename(older,newer);
#endif
}

int _fsLock( int handle, long start, long length, long mode )
{
        int result=0;

#if defined(HAVE_POSIX_IO)
/* TODO: I'm thinking about this :) */
   return;
#else
   /* TODO not in io,h is in stdio.h as fflush(handle)*/
   return;
#endif
}

void _fsCommit( int handle )
{
#if defined(HAVE_POSIX_IO)
/* TODO: I'm thinking about this :) */
#endif
        return;
}

int _fsMkDir( char * name )
{
#if defined(HAVE_POSIX_IO)
  #ifndef __WATCOMC__
        return mkdir(name,S_IWUSR|S_IRUSR);
  #else
	return mkdir( name );
  #endif
#else
	return 0;
#endif
}

int _fsChDir( char * name )
{
#if defined(HAVE_POSIX_IO)
        return chdir(name);
#else
	return 0;
#endif
}

int _fsRmDir( char * name )
{
#if defined(HAVE_POSIX_IO)
        return rmdir(name);
#else
	return 0;
#endif
}

char * _fsCurDir( int driver )
{
#if defined(HAVE_POSIX_IO)
        static char cwd_buff[PATH_MAX+1];
        getcwd(cwd_buff,PATH_MAX);
        return cwd_buff;
#else
	return 0;
#endif
}

int _fsCurDrv( void  )
{
#if defined(HAVE_POSIX_IO)
        return 0;
#else
	return 0;
#endif
}

long _fsChDrv( int  driver )
{
#if defined(HAVE_POSIX_IO)
        return 0;
#else
	return 0;
#endif
}

long _fsIsDrv( int driver )
{
#if defined(HAVE_POSIX_IO)
        return 0;
#else
	return 0;
#endif
}

#ifdef NOT_IMPLEMENTED_YET

/* Unknow that it make :( if anyone can say me !! */
int    _fsExtOpen(PBYTE   filename, PBYTE defExt, ULONG flags,
                   PBYTE   paths, ERRORP error );


#endif

/*
 * -- HARBOUR FUNCTIONS --
 */

#ifdef FOPEN
#define HB_FOPEN FOPEN
#undef FOPEN
#endif

HARBOUR FOPEN()

#ifdef HB_FOPEN
#define FOPEN HB_FOPEN
#undef HB_FOPEN
#endif

{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(2,IT_NUMBER);

        int open_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                open_flags = _parni(2);
            else
                open_flags = 0;

            file_handle = _fsOpen(_parc(1),open_flags);
            last_error = errno;
        }

        _retni(file_handle);
        return;
}

HARBOUR FCREATE()
{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(2,IT_NUMBER);

        int create_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                create_flags = _parni(2);
            else
                create_flags = 0;

            file_handle = _fsCreate(_parc(1),create_flags);
            last_error = errno;
        }

        _retni(file_handle);
        return;
}

#ifdef FREAD
#define HB_FREAD FREAD
#undef FREAD
#endif

HARBOUR FREAD()

#ifdef HB_FREAD
#define FREAD HB_FREAD
#undef HB_FREAD
#endif

{
        PITEM arg1_it = _param(1,IT_NUMBER);
        PITEM arg2_it = _param(2,IT_STRING+IT_BYREF);
        PITEM arg3_it = _param(3,IT_NUMBER);

        long   bytes=0;

        if( arg1_it && arg2_it && arg3_it )
        {
            bytes = _fsRead(_parni(1),_parc(2),_parnl(3));
            last_error = errno;
        }

        _retnl(bytes);
        return;
}

#ifdef FWRITE
#define HB_FWRITE FWRITE
#undef FWRITE
#endif

HARBOUR FWRITE()

#ifdef HB_FWRITE
#define FWRITE HB_FWRITE
#undef HB_FWRITE
#endif

{
        PITEM arg1_it = _param(1,IT_NUMBER);
        PITEM arg2_it = _param(2,IT_STRING);
        PITEM arg3_it = _param(3,IT_NUMBER);

        long   bytes=0;

        if( arg1_it && arg2_it )
        {
            bytes = (arg3_it ? _parnl(3) : arg2_it->wLength );         
            bytes = _fsWrite(_parni(1),_parc(2),bytes);
            last_error = errno;
        }

        _retnl(bytes);
        return;
}

HARBOUR FERROR()
{
        _retni(_fsError());
        return;
}

HARBOUR FCLOSE()
{
        PITEM arg1_it = _param(1,IT_NUMBER);
        int result=-1;

        if( arg1_it )
        {
            result= _fsClose(_parni(1));
            /* last_error = errno; */
        }

        _retl(result>=0?1:0);
        return;
}

HARBOUR FERASE()
{
        PITEM arg1_it = _param(1,IT_STRING);

        int    result = -1;

        if( arg1_it )
        {
                result= _fsDelete(_parc(1));
                last_error = errno;
        }

        _retni(result);
        return;
}

HARBOUR FRENAME()
{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(2,IT_STRING);

        int result=-1;

        if( arg1_it && arg2_it )
        {
            result = _fsRename(_parc(1),_parc(1));
            last_error = errno;
        }

        _retni(result);
        return;
}

HARBOUR FSEEK()
{
        PITEM arg1_it = _param(1,IT_NUMBER);
        PITEM arg2_it = _param(2,IT_NUMBER);
        PITEM arg3_it = _param(3,IT_NUMBER);

        long bytes=0;
        int  pos;
        
        if( arg1_it && arg2_it )
        {
            pos = (arg3_it ? _parni(3) : FS_SET);      
            bytes = _fsSeek(_parni(1),_parnl(2),pos);
            last_error = errno;
        }

        _retnl(bytes);
        return;
}

HARBOUR _FILE()
{
        PITEM arg1_it = _param( 1, IT_STRING );

        if( arg1_it )
        {
                /* TODO: I'm thinking about this :( */
        }
        _retl(0);
        return;
}

HARBOUR FREADSTR()
{
        PITEM arg1_it = _param( 1, IT_NUMBER );
        PITEM arg2_it = _param( 2, IT_NUMBER );

        int    handle;
        long   bytes;
        long   nRead;
        long   readed;
        char * buffer;
        char   ch[1];

        if( arg1_it )
        {
           handle = _parni(1);
           bytes  = (arg2_it ? _parnl(2) : 0);
           buffer = ( char * ) _xgrab(bytes);

           readed=0; ch[0]=1;
           while( readed < bytes )
           {
                 nRead = read(handle,ch,1);
                 if( nRead < 1 || ch[0] == 0 )
                        break;
                 buffer[readed]=ch[0];
                 readed++;
           }

           buffer[readed]=0;
           _retc(buffer);
           _xfree(buffer);
        }
        else
           _retc("");

        return;
}

HARBOUR BIN2I()
{
        PITEM arg1_it = _param( 1, IT_STRING );
        char * s;
        int    result=0;

        if( arg1_it )
        {
           s = _parc(1);
           if( _parclen(1) >= 2 )
                result = MKINT(s[0],s[1]);
           else
              result = 0;
        }

        _retni(result);
        return;
}

HARBOUR BIN2L()
{
        PITEM arg1_it = _param( 1, IT_STRING );
        char * s;
        long   result=0;

        if( arg1_it )
        {
           s = _parc(1);
           if( _parclen(1) >= 4 )
              result = MKLONG(s[0],s[1],s[2],s[3]);
           else
              result = 0;
        }

        _retni(result);
        return;
}

HARBOUR BIN2W()
{
        BIN2I();
}

HARBOUR I2BIN()
{
        PITEM arg1_it = _param( 1, IT_INTEGER );
        int n;
        char s[3];

        if( arg1_it )
        {
           n = _parni(1);
           s[0] = n & 0xFF;
           s[1] = (n & 0xFF00)>>8;
           s[2] = 0;
           _retclen(s,3);
        }
        else
           _retclen("\0\0",2);

        return;
}

HARBOUR L2BIN()
{
        PITEM arg1_it = _param( 1, IT_LONG );
        long  n;
        char  s[5];

        if( arg1_it )
        {
           n = _parnl(1);
           s[0] =  n & 0x000000FF;
           s[1] = (n & 0x0000FF00)>>8;
           s[2] = (n & 0x00FF0000)>>16;
           s[3] = (n & 0xFF000000)>>24;
           s[4] = 0;
           _retclen(s,5);
        }
        else
           _retclen("\0\0\0\0",4);

        return;
}

HARBOUR W2BIN()
{
        I2BIN();
}
