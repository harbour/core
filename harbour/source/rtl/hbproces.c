/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * low level functions to create, wait and terminate processes
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 * based on xHarbour code by
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://www.xharbour.org
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

#define HB_OS_WIN_USED

/* #define HB_IO_WIN_OFF */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  include <signal.h>
#elif defined( HB_OS_OS2 ) || ( defined( HB_OS_WIN ) && !defined( HB_IO_WIN ) )
#  include <io.h>
#  include <process.h>
#  include <fcntl.h>
#  if defined( HB_OS_OS2 )
#    include <sys/wait.h>
#  endif
#endif

#if defined( HB_OS_OS2 ) || defined( HB_OS_UNIX ) || \
    ( defined( HB_OS_WIN ) && !defined( HB_IO_WIN ) )

/* convert command to argument list using standard bourne shell encoding:
 * "" and '' can be used to group parameters with blank characters,
 * the escape character is '\', quoting by '' disables escape character.
 */
static char ** hb_buildArgs( const char *pszFilename )
{
   const char * src;
   char ** argv, * dst, cQuote = 0;
   int argc = 0;

   while( HB_ISSPACE( *pszFilename ) )
      ++pszFilename;
   src = pszFilename;
   while( *src )
   {
#if defined( HB_OS_UNIX )
      if( *src == '\\' && cQuote != '\'' )
      {
         if( src[ 1 ] )
            ++src;
      }
      else
#endif
      if( *src == cQuote )
         cQuote = 0;
      else if( cQuote == 0 )
      {
#if defined( HB_OS_UNIX )
         if( *src == '"' || *src == '\'' )
#else
         if( *src == '"' )
#endif
            cQuote = *src;
         else if( HB_ISSPACE( *src ) )
         {
            while( HB_ISSPACE( src[ 1 ] ) )
               ++src;
            if( src[ 1 ] )
               ++argc;
         }
      }
      ++src;
   }
   dst = ( char * ) hb_xgrab( strlen( pszFilename ) + 1 );
   argv = ( char ** ) hb_xgrab( ( argc + 2 ) * sizeof( char * ) );
   argv[ 0 ] = dst;
   argv[ argc + 1 ] = NULL;
   argc = 0;

   cQuote = 0;
   src = pszFilename;
   while( *src )
   {
#if defined( HB_OS_UNIX )
      if( *src == '\\' && cQuote != '\'' )
      {
         if( src[ 1 ] )
         {
            *dst++ = src[ 1 ];
            ++src;
         }
      }
      else
#endif
      if( *src == cQuote )
         cQuote = 0;
      else if( cQuote != 0 )
         *dst++ = *src;
      else
      {
#if defined( HB_OS_UNIX )
         if( *src == '"' || *src == '\'' )
#else
         if( *src == '"' )
#endif
            cQuote = *src;
         else if( HB_ISSPACE( *src ) )
         {
            *dst++ = '\0';
            while( HB_ISSPACE( src[ 1 ] ) )
               ++src;
            if( src[ 1 ] )
               argv[ ++argc ] = dst;
         }
         else
            *dst++ = *src;
      }
      ++src;
   }
   *dst = 0;

   return argv;
}

static void hb_freeArgs( char ** argv )
{
   hb_xfree( argv[ 0 ] );
   hb_xfree( argv );
}

#endif

HB_FHANDLE hb_fsProcessOpen( const char *pszFilename,
                             HB_FHANDLE *phStdin, HB_FHANDLE *phStdout,
                             HB_FHANDLE *phStderr,
                             BOOL fDetach, ULONG *pulPID )
{
   HB_FHANDLE hResult = FS_ERROR;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsProcessOpen(%s, %p, %p, %p, %d, %p)", pszFilename, phStdin, phStdout, phStderr, fDetach, pulPID));

#if defined( HB_IO_WIN )
{

#if defined( HB_OS_WIN_CE )
#  define CreatePipe( hIn, hOut, sa, flags )    ( FALSE )
#endif

   BOOL fError = FALSE;
   HANDLE hPipes[ 6 ];
   SECURITY_ATTRIBUTES sa;
   int i;

   for( i = 0; i < 6; ++i )
      hPipes[ i ] = INVALID_HANDLE_VALUE;

   memset( &sa, 0, sizeof( sa ) );
   sa.nLength = sizeof( sa );
   sa.bInheritHandle = TRUE;

   if( phStdin != NULL )
      fError = !CreatePipe( &hPipes[0], &hPipes[1], &sa, 0 );
   if( !fError && phStdout != NULL )
      fError = !CreatePipe( &hPipes[2], &hPipes[3], &sa, 0 );
   if( !fError && phStderr != NULL )
   {
      if( phStdout == phStderr )
      {
         hPipes[4] = hPipes[2];
         hPipes[5] = hPipes[3];
      }
      else
         fError = !CreatePipe( &hPipes[4], &hPipes[5], &sa, 0 );
   }

   if( fError )
      hb_fsSetIOError( FALSE, 0 );
   else
   {
      PROCESS_INFORMATION pi;
      STARTUPINFO si;
      DWORD dwFlags = 0;
#if defined(UNICODE)
      LPWSTR lpCommand = hb_mbtowc( pszFilename );
#else
      char * lpCommand = hb_strdup( pszFilename );
#endif
      memset( &pi, 0, sizeof( pi ) );
      memset( &si, 0, sizeof( si ) );
      si.cb = sizeof( si );
#ifdef STARTF_USESTDHANDLES
      si.dwFlags = STARTF_USESTDHANDLES;
#endif
      if( fDetach )
      {
#ifdef STARTF_USESHOWWINDOW
         si.dwFlags |= STARTF_USESHOWWINDOW;
#endif
         si.wShowWindow = SW_HIDE;
         si.hStdInput  = hPipes[ 0 ];
         si.hStdOutput = hPipes[ 3 ];
         si.hStdError  = hPipes[ 5 ];
#ifdef DETACHED_PROCESS
         dwFlags |= DETACHED_PROCESS;
#endif
      }
      else
      {
         si.hStdInput  = phStdin  ? hPipes[ 0 ] : GetStdHandle( STD_INPUT_HANDLE );
         si.hStdOutput = phStdout ? hPipes[ 3 ] : GetStdHandle( STD_OUTPUT_HANDLE );
         si.hStdError  = phStderr ? hPipes[ 5 ] : GetStdHandle( STD_ERROR_HANDLE );
      }
      fError = ! CreateProcess( NULL,           /* lpAppName */
                                lpCommand,
                                NULL,           /* lpProcessAttr */
                                NULL,           /* lpThreadAttr */
                                TRUE,           /* bInheritHandles */
                                dwFlags,        /* dwCreationFlags */
                                NULL,           /* lpEnvironment */
                                NULL,           /* lpCurrentDirectory */
                                &si,
                                &pi );
      hb_fsSetIOError( !fError, 0 );
      hb_xfree( lpCommand );
      if( !fError )
      {
         if( phStdin != NULL )
         {
            *phStdin = ( HB_FHANDLE ) hPipes[ 1 ];
            hPipes[ 1 ] = INVALID_HANDLE_VALUE;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( HB_FHANDLE ) hPipes[ 2 ];
            hPipes[ 2 ] = INVALID_HANDLE_VALUE;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( HB_FHANDLE ) hPipes[ 4 ];
            hPipes[ 4 ] = INVALID_HANDLE_VALUE;
         }
         if( pulPID )
            *pulPID = pi.dwProcessId;
         CloseHandle( pi.hThread );
         hResult = ( HB_FHANDLE ) pi.hProcess;
      }
   }

   for( i = phStdout == phStderr ? 3 : 5; i >= 0; --i )
   {
      if( hPipes[ i ] != INVALID_HANDLE_VALUE )
         CloseHandle( hPipes[ i ] );
   }
}
#elif defined( HB_OS_UNIX )
{
   BOOL fError = FALSE;
   HB_FHANDLE hPipeIn [ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeOut[ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeErr[ 2 ] = { FS_ERROR, FS_ERROR };

   if( phStdin != NULL )
      fError = pipe( hPipeIn ) != 0;
   if( !fError && phStdout != NULL )
      fError = pipe( hPipeOut ) != 0;
   if( !fError && phStderr != NULL )
   {
      if( phStdout == phStderr )
      {
         hPipeErr[ 0 ] = hPipeOut[ 0 ];
         hPipeErr[ 1 ] = hPipeOut[ 1 ];
      }
      else
         fError = pipe( hPipeErr ) != 0;
   }

   if( !fError )
   {
      pid_t pid = fork();

      if( pid == -1 )
         fError = TRUE;
      else if( pid != 0 )    /* parent process */
      {
         if( phStdin != NULL )
         {
            *phStdin = ( HB_FHANDLE ) hPipeIn[ 1 ];
            hPipeIn[ 1 ] = FS_ERROR;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( HB_FHANDLE ) hPipeOut[ 0 ];
            hPipeOut[ 0 ] = FS_ERROR;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( HB_FHANDLE ) hPipeErr[ 0 ];
            hPipeErr[ 0 ] = FS_ERROR;
         }
         if( pulPID )
            *pulPID = pid;
         hResult = ( HB_FHANDLE ) pid;
      }
      else                    /* child process */
      {
         if( fDetach && ( !phStdin || !phStdout || !phStderr ) )
         {
            HB_FHANDLE hNull = open( "/dev/null", O_RDWR );

            if( !phStdin )
               dup2( hNull, 0 );
            if( !phStdout )
               dup2( hNull, 1 );
            if( !phStderr )
               dup2( hNull, 2 );

            if( hNull != FS_ERROR )
               close( hNull );
         }

         if( phStdin != NULL )
            dup2( hPipeIn[ 0 ], 0 );
         if( phStdout != NULL )
            dup2( hPipeOut[ 1 ], 1 );
         if( phStderr != NULL )
            dup2( hPipeErr[ 1 ], 2 );

         /* close all non std* handles */
         {
            int iMaxFD, i;
            iMaxFD = sysconf( _SC_OPEN_MAX );
            if( iMaxFD < 3 )
               iMaxFD = 1024;
            for( i = 3; i < iMaxFD; ++i )
               close( i );
         }

         /* reset extended process attributes */
         setuid( getuid() );
         setgid( getgid() );

         /* execute command */
         {
#if 0
            char * argv[4];

            argv[0] = ( char * ) "sh";
            argv[1] = ( char * ) "-c";
            argv[2] = ( char * ) pszFilename;
            argv[3] = ( char * ) 0;
            execv( "/bin/sh", argv );
#else
            char ** argv;

            argv = hb_buildArgs( pszFilename );
            execvp( argv[ 0 ], argv );
            hb_freeArgs( argv );
#endif
            exit(1);
         }
      }
   }

   hb_fsSetIOError( !fError, 0 );

   if( hPipeIn[ 0 ] != FS_ERROR )
      close( hPipeIn[ 0 ] );
   if( hPipeIn[ 1 ] != FS_ERROR )
      close( hPipeIn[ 1 ] );
   if( hPipeOut[ 0 ] != FS_ERROR )
      close( hPipeOut[ 0 ] );
   if( hPipeOut[ 1 ] != FS_ERROR )
      close( hPipeOut[ 1 ] );
   if( phStdout != phStderr )
   {
      if( hPipeErr[ 0 ] != FS_ERROR )
         close( hPipeErr[ 0 ] );
      if( hPipeErr[ 1 ] != FS_ERROR )
         close( hPipeErr[ 1 ] );
   }
}
#elif defined( HB_OS_OS2 ) || defined( HB_OS_WIN )
{

#if defined( HB_OS_WIN )

#  define pid_t               int
#  define _hb_pipe( e, p )    do { \
                                 (e) = _pipe( (p), 2048, _O_BINARY ) != 0; \
                              } while( 0 )

#elif defined( HB_OS_OS2 )

#  define _hb_pipe( e, p )    do { \
                                 (e) = pipe( (p) ) != 0; \
                                 if( !(e) ) \
                                 { \
                                    setmode( (p)[ 0 ], O_BINARY ); \
                                    setmode( (p)[ 1 ], O_BINARY ); \
                                 } \
                              } while( 0 )
#endif

   BOOL fError = FALSE;
   HB_FHANDLE hPipeIn [ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeOut[ 2 ] = { FS_ERROR, FS_ERROR },
              hPipeErr[ 2 ] = { FS_ERROR, FS_ERROR };

   if( phStdin != NULL )
   {
      _hb_pipe( fError, hPipeIn );
   }
   if( !fError && phStdout != NULL )
   {
      _hb_pipe( fError, hPipeOut );
   }
   if( !fError && phStderr != NULL )
   {
      if( phStdout == phStderr )
      {
         hPipeErr[ 0 ] = hPipeOut[ 0 ];
         hPipeErr[ 1 ] = hPipeOut[ 1 ];
      }
      else
      {
         _hb_pipe( fError, hPipeErr );
      }
   }

   if( !fError )
   {
      int hStdIn, hStdOut, hStdErr;
      char ** argv;
      pid_t pid;

      hStdIn  = dup( 0 );
      hStdOut = dup( 1 );
      hStdErr = dup( 2 );

      if( fDetach && ( !phStdin || !phStdout || !phStderr ) )
      {
         HB_FHANDLE hNull = open( "NUL:", O_RDWR );

         if( !phStdin )
            dup2( hNull, 0 );
         if( !phStdout )
            dup2( hNull, 1 );
         if( !phStderr )
            dup2( hNull, 2 );

         if( hNull != FS_ERROR )
            close( hNull );
      }

      if( phStdin != NULL )
         dup2( hPipeIn[ 0 ], 0 );
      if( phStdout != NULL )
         dup2( hPipeOut[ 1 ], 1 );
      if( phStderr != NULL )
         dup2( hPipeErr[ 1 ], 2 );

      argv = hb_buildArgs( pszFilename );

#if defined( _MSC_VER ) || defined( __LCC__ ) || \
    defined( __XCC__ ) || defined( __POCC__ )
      pid = _spawnvp( _P_NOWAIT, argv[ 0 ], argv );
#elif defined( __MINGW32__ )
      pid = spawnvp( P_NOWAIT, argv[ 0 ], ( const char * const * ) argv );
#else
      pid = spawnvp( P_NOWAIT, argv[ 0 ], ( char * const * ) argv );
#endif
      hb_freeArgs( argv );

      dup2( hStdIn,  0 );
      dup2( hStdOut, 1 );
      dup2( hStdErr, 2 );

      if( pid < 0 )
         fError = TRUE;
      else if( pid != 0 )    /* parent process */
      {
         if( phStdin != NULL )
         {
            *phStdin = ( HB_FHANDLE ) hPipeIn[ 1 ];
            hPipeIn[ 1 ] = FS_ERROR;
         }
         if( phStdout != NULL )
         {
            *phStdout = ( HB_FHANDLE ) hPipeOut[ 0 ];
            hPipeOut[ 0 ] = FS_ERROR;
         }
         if( phStderr != NULL )
         {
            *phStderr = ( HB_FHANDLE ) hPipeErr[ 0 ];
            hPipeErr[ 0 ] = FS_ERROR;
         }
         if( pulPID )
            *pulPID = pid;
         hResult = ( HB_FHANDLE ) pid;
      }
   }

   hb_fsSetIOError( !fError, 0 );

   if( hPipeIn[ 0 ] != FS_ERROR )
      close( hPipeIn[ 0 ] );
   if( hPipeIn[ 1 ] != FS_ERROR )
      close( hPipeIn[ 1 ] );
   if( hPipeOut[ 0 ] != FS_ERROR )
      close( hPipeOut[ 0 ] );
   if( hPipeOut[ 1 ] != FS_ERROR )
      close( hPipeOut[ 1 ] );
   if( phStdout != phStderr )
   {
      if( hPipeErr[ 0 ] != FS_ERROR )
         close( hPipeErr[ 0 ] );
      if( hPipeErr[ 1 ] != FS_ERROR )
         close( hPipeErr[ 1 ] );
   }
}
#else
{
   int TODO; /* TODO: for given platform */

   HB_SYMBOL_UNUSED( pszFilename );
   HB_SYMBOL_UNUSED( phStdin );
   HB_SYMBOL_UNUSED( phStdout );
   HB_SYMBOL_UNUSED( phStderr );
   HB_SYMBOL_UNUSED( fDetach );
   HB_SYMBOL_UNUSED( pulPID );

   hb_fsSetError( ( USHORT ) FS_ERROR );
}
#endif

   return hResult;
}

int hb_fsProcessValue( HB_FHANDLE hProcess, BOOL fWait )
{
   int iRetStatus = -1;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsProcessValue(%p, %d)", ( void * ) ( HB_PTRDIFF ) hProcess, fWait));

#if defined( HB_IO_WIN )
{
   BOOL fError = TRUE;
   DWORD dwResult;
   HANDLE hProc = ( HANDLE ) hb_fsGetOsHandle( hProcess );

   if( hProc )
   {
      hb_vmUnlock();
      dwResult = WaitForSingleObject( hProc, fWait ? INFINITE : 0 );
      if( dwResult == WAIT_OBJECT_0 )
      {
         fError = !GetExitCodeProcess( hProc, &dwResult );
         iRetStatus = !fError ? ( int ) dwResult : -2;
      }
      hb_fsSetIOError( !fError, 0 );
      if( !fError )
         CloseHandle( hProc );
      hb_vmLock();
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#elif defined( HB_OS_UNIX ) || ( defined( HB_OS_OS2 ) && defined( __GNUC__ ) )
{
   int iStatus;
   pid_t pid = ( pid_t ) hProcess;

   if( pid > 0 )
   {
      hb_vmUnlock();
      iRetStatus = waitpid( pid, &iStatus, fWait ? 0 : WNOHANG );
      hb_fsSetIOError( iRetStatus >= 0, 0 );
#ifdef ERESTARTSYS
      if( iRetStatus < 0 && errno != ERESTARTSYS)
#else
      if( iRetStatus < 0 )
#endif
         iRetStatus = -2;
      else if( iRetStatus == 0 )
         iRetStatus = -1;
      else
         iRetStatus = WIFEXITED( iStatus ) ? WEXITSTATUS( iStatus ) : 0;
      hb_vmLock();
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#elif defined( HB_OS_OS2 ) || defined( HB_OS_WIN )
{
   int iPid = ( int ) hProcess;

   HB_SYMBOL_UNUSED( fWait );

   if( iPid > 0 )
   {
      hb_vmUnlock();
#if defined( __BORLANDC__ )
      iPid = cwait( &iRetStatus, iPid, 0 );
#else
      iPid = _cwait( &iRetStatus, iPid, 0 );
#endif
      hb_fsSetIOError( iPid > 0, 0 );
      if( iPid != ( int ) hProcess )
         iRetStatus = -1;
      hb_vmLock();
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#else
{
   int TODO; /* TODO: for given platform */

   HB_SYMBOL_UNUSED( hProcess );
   HB_SYMBOL_UNUSED( fWait );
   hb_fsSetError( ( USHORT ) FS_ERROR );
}
#endif
   return iRetStatus;
}

/* Closes/kills process. The handle is still valid until you
 * catch it with hb_fsProcessValue.
 */
BOOL hb_fsProcessClose( HB_FHANDLE hProcess, BOOL fGentle )
{
   BOOL fResult = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsProcessClose(%p, %d)", ( void * ) ( HB_PTRDIFF ) hProcess, fGentle));

#if defined( HB_IO_WIN )
{
   HANDLE hProc = ( HANDLE ) hb_fsGetOsHandle( hProcess );

   if( hProc )
   {
      if( TerminateProcess( hProc, fGentle ? 0 : 1 ) )
         fResult = TRUE;
      hb_fsSetIOError( fResult, 0 );
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#elif defined( HB_OS_UNIX ) || ( defined( HB_OS_OS2 ) && defined( __GNUC__ ) )
{
   pid_t pid = ( pid_t ) hProcess;
   if( pid > 0 )
   {
      if( kill( pid, fGentle ? SIGTERM : SIGKILL ) == 0 )
         fResult = TRUE;
      hb_fsSetIOError( fResult, 0 );
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#elif defined( HB_OS_WIN )
{
   HANDLE hProc = OpenProcess( PROCESS_TERMINATE, FALSE, hProcess );

   if( hProc )
   {
      if( TerminateProcess( hProc, fGentle ? 0 : 1 ) )
         fResult = TRUE;
      hb_fsSetIOError( fResult, 0 );
      CloseHandle( hProc );
   }
   else
      hb_fsSetError( ( USHORT ) FS_ERROR );
}
#else
{
   int TODO; /* TODO: for given platform */

   HB_SYMBOL_UNUSED( hProcess );
   HB_SYMBOL_UNUSED( fGentle );
   hb_fsSetError( ( USHORT ) FS_ERROR );
}
#endif
   return fResult;
}
