#include <extend.h>

#if defined(_SO_LINUX)
#include <unistd.h>
#endif

#if defined(__GNUC__)
#include <unistd.h>
/* This is ugly, but we are using FOPEN, etc., and those names collide
   with names in the standard C library. */
extern int open _PARAMS ((const char *, int, ...));
extern int creat _PARAMS ((const char *, mode_t));
#endif

#if defined(__WATCOMC__)
   #include <unistd.h>
#endif

#if defined(__BORLANDC__)
#include <fcntl.h>
#include <io.h>
#endif

#if defined(_SO_DOS)

#if defined(_CC_DJGPP)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#endif

#ifndef __WATCOMC__
   extern int errno;
#endif

HARBOUR BIN2I( void );
HARBOUR I2BIN( void );

static int last_error = 0;

#define MKLONG(_1,_2,_3,_4) (((long)_4)<<24)|(((long)_3)<<16)|(((long)_2)<<8)|_1
#define MKINT(_1,_2)  (((long)_2)<<8)|_1

HARBOUR FOPEN()
{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(1,IT_NUMERIC);

        int open_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                open_flags = _parni(2);
            else
                open_flags = 0;

            /* TODO: Study equivalence between Clipper Flags & SO Flags
               they are very so dependent */

            file_handle = open(_parc(1),open_flags);
            last_error = errno;
        }

        _retni(file_handle);
        return;
}

HARBOUR FCREATE()
{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(1,IT_NUMERIC);

        int create_flags;
        int file_handle = -1;

        if( arg1_it )
        {
            if( arg2_it )
                create_flags = _parni(2);
            else
                create_flags = 0;

            /* TODO: Study equivalence between Clipper Flags & SO Flags
               they are very so dependent */

            file_handle = creat(_parc(1),create_flags);
            last_error = errno;
        }

        _retni(file_handle);
        return;
}

HARBOUR FREAD()
{
        PITEM arg1_it = _param(1,IT_NUMERIC);
        PITEM arg2_it = _param(1,IT_STRING+IT_BYREF);
        PITEM arg3_it = _param(1,IT_NUMERIC);

        long   bytes=0;

        if( arg1_it && arg2_it && arg3_it )
        {
            bytes = read(_parni(1),_parc(2),_parnl(3));
            last_error = errno;
        }

        _retnl(bytes);
        return;
}

HARBOUR FWRITE()
{
   PITEM arg1_it = _param( 1, IT_NUMERIC );
   PITEM arg2_it = _param( 2, IT_STRING );
   long  bytes   = 0;

   if( arg1_it && arg2_it )
   {
      bytes = write( _parni( 1 ), _parc( 2 ),
                     _parnl( 3 ) ? _parnl( 3 ): _parclen( 2 ) );
      last_error = errno;
   }
   _retnl( bytes );
}

HARBOUR FERROR()
{
        _retni(last_error);
        return;
}

HARBOUR FCLOSE()
{
        PITEM arg1_it = _param(1,IT_NUMERIC);
        int result=-1;

        if( arg1_it )
        {
            result=close(_parni(1));
            //last_error = errno;
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
                result=unlink(_parc(1));
                last_error = errno;
        }

        _retni(result);
        return;
}

HARBOUR FRENAME()
{
        PITEM arg1_it = _param(1,IT_STRING);
        PITEM arg2_it = _param(1,IT_STRING);

        int result=-1;

        if( arg1_it && arg2_it )
        {
            result = rename(_parc(1),_parc(1));
            last_error = errno;
        }

        _retni(result);
        return;
}

HARBOUR FSEEK()
{
        PITEM arg1_it = _param(1,IT_NUMERIC);
        PITEM arg2_it = _param(1,IT_NUMERIC);
        PITEM arg3_it = _param(1,IT_NUMERIC);

        long   bytes=0;

        if( arg1_it && arg2_it && arg3_it )
        {
            bytes = lseek(_parni(1),_parnl(2),_parni(3));
            last_error = errno;
        }

        _retnl(bytes);
        return;
}

HARBOUR File()
{
        PITEM arg1_it = _param( 1, IT_STRING );

        if( arg1_it )
        {
        // TODO: In this moment I'm thinking about two alternatives
        }
}

HARBOUR FREADSTR()
{
        PITEM arg1_it = _param( 1, IT_NUMERIC );

        int    handle;
        long   bytes;
        long   readed;
        char * buffer;
        char   ch[1];

        if( arg1_it )
        {
           handle = _parni(1);
           bytes  = _parnl(2);
           buffer = ( char * ) _xgrab(bytes);

           readed=0; ch[0]=1;
           while( readed < bytes )
           {
                 bytes = read(handle,ch,1);
                 if( bytes < 1 || ch[0] == 0 )
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
           _retclen("\0\0\0",3);

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
           _retclen("\0\0\0\0\0",5);

        return;
}

HARBOUR W2BIN()
{
        I2BIN();
}
