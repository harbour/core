/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Exception handlers
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt)
 *    hb_winExceptionHandler() Windows exception info dump code.
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 *    hb_winExceptionHandler() Module listing code.
 *    hb_winExceptionHandler() x64 support.
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbvm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "hbset.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  include <signal.h>
#  if defined( SIGSTKSZ ) && \
      ( ( defined( _BSD_SOURCE ) && _BSD_SOURCE ) || \
        ( defined( _XOPEN_SOURCE ) && _XOPEN_SOURCE >= 500 ) )
#     define HB_SIGNAL_EXCEPTION_HANDLER
#  endif
#endif

#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )
#  include <windows.h>
#  include <tlhelp32.h>
   /* BCC and MinGW doesn't seem to #define this */
#  ifndef TH32CS_SNAPMODULE32
#     define TH32CS_SNAPMODULE32  0
#  endif
#endif

#if defined( HB_SIGNAL_EXCEPTION_HANDLER )
   static HB_BYTE * s_signal_stack[ SIGSTKSZ ];
#endif

#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )

static LONG WINAPI hb_winExceptionHandler( struct _EXCEPTION_POINTERS * pExceptionInfo )
{
   char errmsg[ 8192 ];
   int errmsglen = sizeof( errmsg ) - 1;

   errmsg[ 0 ] = '\0';

#if defined( HB_OS_WIN_64 ) && defined( HB_CPU_X86_64 )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
                "\n\n"
                "    Exception Code:%08X\n"
                "    Exception Address:0x%016" PFLL "X\n"
                "    RAX:0x%016" PFLL "X  RBX:0x%016" PFLL "X  RCX:0x%016" PFLL "X  RDX:0x%016" PFLL "X\n"
                "    RSI:0x%016" PFLL "X  RDI:0x%016" PFLL "X  RBP:0x%016" PFLL "X\n"
                "    R8 :0x%016" PFLL "X  R9 :0x%016" PFLL "X  R10:0x%016" PFLL "X  R11:0x%016" PFLL "X\n"
                "    R12:0x%016" PFLL "X  R13:0x%016" PFLL "X  R14:0x%016" PFLL "X  R15:0x%016" PFLL "X\n"
                "    CS:RIP:%04X:0x%016" PFLL "X  SS:RSP:%04X:0x%016" PFLL "X\n"
                "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
                "    Flags:%08X\n",
                ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
                ( HB_PTRDIFF ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
                pCtx->Rax, pCtx->Rbx, pCtx->Rcx, pCtx->Rdx,
                pCtx->Rsi, pCtx->Rdi, pCtx->Rbp,
                pCtx->R8 , pCtx->R9 , pCtx->R10, pCtx->R11,
                pCtx->R12, pCtx->R13, pCtx->R14, pCtx->R15,
                ( HB_U32 ) pCtx->SegCs, pCtx->Rip, ( HB_U32 ) pCtx->SegSs, pCtx->Rsp,
                ( HB_U32 ) pCtx->SegDs, ( HB_U32 ) pCtx->SegEs, ( HB_U32 ) pCtx->SegFs, ( HB_U32 ) pCtx->SegGs,
                ( HB_U32 ) pCtx->EFlags );

      /* TODO: 64-bit stack trace.
               See: - StackWalk64()
                    - http://www.codeproject.com/KB/threads/StackWalker.aspx?fid=202364 */
   }
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_IA_64 )
   {
      /* TODO: Itanium
               See: winnt.h for PCONTEXT structure. */
   }
#else
   {
      char              buf[ 64 + MAX_PATH ];
      PCONTEXT          pCtx = pExceptionInfo->ContextRecord;
      unsigned char *   pc;
      unsigned int *    sc;
      unsigned int *    ebp;
      unsigned int      eip;
      unsigned int      j;
      int               i;

      hb_snprintf( errmsg, errmsglen,
                "\n\n"
                "    Exception Code:%08X\n"
                "    Exception Address:%08X\n"
                "    EAX:%08X  EBX:%08X  ECX:%08X  EDX:%08X\n"
                "    ESI:%08X  EDI:%08X  EBP:%08X\n"
                "    CS:EIP:%04X:%08X  SS:ESP:%04X:%08X\n"
                "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
                "    Flags:%08X\n",
                ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
                ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
                ( HB_U32 ) pCtx->Eax, ( HB_U32 ) pCtx->Ebx, ( HB_U32 ) pCtx->Ecx, ( HB_U32 ) pCtx->Edx,
                ( HB_U32 ) pCtx->Esi, ( HB_U32 ) pCtx->Edi, ( HB_U32 ) pCtx->Ebp,
                ( HB_U32 ) pCtx->SegCs, ( HB_U32 ) pCtx->Eip, ( HB_U32 ) pCtx->SegSs, ( HB_U32 ) pCtx->Esp,
                ( HB_U32 ) pCtx->SegDs, ( HB_U32 ) pCtx->SegEs, ( HB_U32 ) pCtx->SegFs, ( HB_U32 ) pCtx->SegGs,
                ( HB_U32 ) pCtx->EFlags );

      hb_strncat( errmsg, "    CS:EIP:", errmsglen );
      pc = ( unsigned char * ) pCtx->Eip;
      for( i = 0; i < 16; i++ )
      {
         /* TOFIX: Unsafe funcion. */
         if( IsBadReadPtr( pc, 1 ) )
            break;
         hb_snprintf( buf, sizeof( buf ) - 1, " %02X", ( int ) pc[ i ] );
         hb_strncat( errmsg, buf, errmsglen );
      }
      hb_strncat( errmsg, "\n    SS:ESP:", errmsglen );
      sc = ( unsigned int * ) pCtx->Esp;
      for( i = 0; i < 16; i++ )
      {
         /* TOFIX: Unsafe funcion. */
         if( IsBadReadPtr( sc, 4 ) )
            break;
         hb_snprintf( buf, sizeof( buf ), " %08X", sc[ i ] );
         hb_strncat( errmsg, buf, errmsglen );
      }
      hb_strncat( errmsg, "\n\n", errmsglen );
      hb_strncat( errmsg, "    C stack:\n", errmsglen );
      hb_strncat( errmsg, "    EIP:     EBP:       Frame: OldEBP, RetAddr, Params...\n", errmsglen );
      eip = pCtx->Eip;
      ebp = ( unsigned int * ) pCtx->Ebp;
      /* TOFIX: Unsafe funcion. */
      if( ! IsBadWritePtr( ebp, 8 ) )
      {
         for( i = 0; i < 20; i++ )
         {
            /* TOFIX: Unsafe funcion. */
            if( ( unsigned int ) ebp % 4 != 0 || IsBadWritePtr( ebp, 40 ) || ( unsigned int ) ebp >= ebp[ 0 ] )
               break;
            hb_snprintf( buf, sizeof( buf ), "    %08X %08X  ", ( int ) eip, ( int ) ebp );
            hb_strncat( errmsg, buf, errmsglen );
            for( j = 0; j < 10 && ( unsigned int ) ( ebp + j ) < ebp[ 0 ]; j++ )
            {
               hb_snprintf( buf, sizeof( buf ), " %08X", ebp[ j ] );
               hb_strncat( errmsg, buf, errmsglen );
            }
            hb_strncat( errmsg, "\n", errmsglen );
            eip = ebp[ 1 ];
            ebp = ( unsigned int * ) ebp[ 0 ];
         }
         hb_strncat( errmsg, "\n", errmsglen );
      }
   }
#endif

   {
      /* NOTE: Several non-MS sources say that Win9x has these functions
               in tlhelp32.dll. Testing shows though, that in Win95, Win95b
               and Win98 they are in kernel32.dll, and tlhelp32.dll doesn't
               exist. [vszakats] */
      HMODULE hKernel32 = GetModuleHandle( TEXT( "kernel32.dll" ) );

      if( hKernel32 )
      {
         /* NOTE: Hack to force the ASCII versions of these types. [vszakats] */
         #if defined( UNICODE )
            #undef MODULEENTRY32
            #undef LPMODULEENTRY32
         #endif

         typedef HANDLE ( WINAPI * P_CTH32SSH )( DWORD, DWORD ); /* CreateToolhelp32Snapshot() */
         typedef BOOL ( WINAPI * P_M32F )( HANDLE, LPMODULEENTRY32 ); /* Module32First() */
         typedef BOOL ( WINAPI * P_M32N )( HANDLE, LPMODULEENTRY32 ); /* Module32Next() */

         P_CTH32SSH pCreateToolhelp32Snapshot = ( P_CTH32SSH ) GetProcAddress( hKernel32, "CreateToolhelp32Snapshot" );
         P_M32F     pModule32First            = ( P_M32F     ) GetProcAddress( hKernel32, "Module32First" );
         P_M32N     pModule32Next             = ( P_M32N     ) GetProcAddress( hKernel32, "Module32Next" );

         if( pCreateToolhelp32Snapshot &&
             pModule32First &&
             pModule32Next )
         {
            /* Take a snapshot of all modules in the specified process. */
            HANDLE hModuleSnap = pCreateToolhelp32Snapshot( TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, GetCurrentProcessId() );

            if( hModuleSnap != INVALID_HANDLE_VALUE )
            {
               MODULEENTRY32 me32;

               /* Set the size of the structure before using it. */
               me32.dwSize = sizeof( MODULEENTRY32 );

               /* Retrieve information about the first module, and exit if unsuccessful */
               if( pModule32First( hModuleSnap, &me32 ) )
               {
                  hb_strncat( errmsg, "\nModules:\n", errmsglen );

                  /* Now walk the module list of the process, and display information about each module */
                  do
                  {
                     char buf[ 256 ];
#if defined( HB_OS_WIN_64 )
                     /* TOFIX: me32.szExePath seemed trashed in some (standalone) tests. */
                     hb_snprintf( buf, sizeof( buf ), "0x%016" PFLL "X 0x%016" PFLL "X %s\n", ( HB_PTRDIFF ) me32.modBaseAddr, ( HB_PTRDIFF ) me32.modBaseSize, me32.szExePath );
#else
                     hb_snprintf( buf, sizeof( buf ), "0x%08lX 0x%08lX %s\n", ( HB_PTRDIFF ) me32.modBaseAddr, ( HB_PTRDIFF ) me32.modBaseSize, me32.szExePath );
#endif
                     hb_strncat( errmsg, buf, errmsglen );
                  } while( pModule32Next( hModuleSnap, &me32 ) );
               }

               /* Do not forget to clean up the snapshot object. */
               CloseHandle( hModuleSnap );
            }
         }
      }
   }

   hb_errInternalRaw( 6005, "Exception error: %s", errmsg, NULL );

   return hb_cmdargCheck( "BATCH" ) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH;
}

#elif defined( HB_OS_OS2 )

static EXCEPTIONREGISTRATIONRECORD s_regRec; /* Exception Registration Record */

static ULONG _System hb_os2ExceptionHandler( PEXCEPTIONREPORTRECORD       p1,
                                             PEXCEPTIONREGISTRATIONRECORD p2,
                                             PCONTEXTRECORD               p3,
                                             PVOID                        pv )
{
   HB_SYMBOL_UNUSED( p1 );
   HB_SYMBOL_UNUSED( p2 );
   HB_SYMBOL_UNUSED( p3 );
   HB_SYMBOL_UNUSED( pv );

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if( p1->ExceptionNum != XCPT_UNWIND && p1->ExceptionNum < XCPT_BREAKPOINT )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
      char file[ HB_PATH_MAX ];
      HB_USHORT uiLine;
      int iLevel = 0;

      fprintf( stderr, HB_I_("\nException %lx at address %p \n"), p1->ExceptionNum, p1->ExceptionAddress );

      while( hb_procinfo( iLevel++, buffer, &uiLine, file ) )
         fprintf( stderr, HB_I_("Called from %s(%hu)%s%s\n"), buffer, uiLine, *file ? HB_I_(" in ") : "", file );
   }

   return hb_cmdargCheck( "BATCH" ) ? XCPT_CONTINUE_STOP : XCPT_CONTINUE_SEARCH /* Exception not resolved... */;
}

#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )

static void hb_signalExceptionHandler( int sig, siginfo_t * si, void * ucp )
{
   char buffer[ 32 ];
   const char * signame;
   const char * sigaddr;

   HB_SYMBOL_UNUSED( ucp );

   switch( sig )
   {
   case SIGSEGV:
      signame = "SIGSEGV";
      hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
      sigaddr = buffer;
      break;
   case SIGILL:
      signame = "SIGILL";
      hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
      sigaddr = buffer;
      break;
   case SIGFPE:
      signame = "SIGFPE";
      hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
      sigaddr = buffer;
      break;
   case SIGBUS:
      signame = "SIGBUS";
      hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
      sigaddr = buffer;
      break;
   default:
      hb_snprintf( buffer, sizeof( buffer ), "sig:%d", sig );
      signame = buffer;
      sigaddr = "UNKNOWN";
      break;
   }

   hb_errInternal( 6005, "Exception %s at address %s", signame, sigaddr );
}

#endif

void hb_vmSetExceptionHandler( void )
{
#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )
   {
      LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter( hb_winExceptionHandler );
      HB_SYMBOL_UNUSED( ef );
   }
#elif defined( HB_OS_OS2 ) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      memset( &s_regRec, 0, sizeof( s_regRec ) );
      s_regRec.ExceptionHandler = ( ERR ) hb_os2ExceptionHandler;
      rc = DosSetExceptionHandler( &s_regRec );
      if( rc != NO_ERROR )
         hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
   }
#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )
   {
      stack_t ss;
      ss.ss_sp = ( void * ) s_signal_stack;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = 0;
      /* set alternative stack for SIGSEGV executed on stack overflow */
      if( sigaltstack( &ss, NULL ) == 0 )
      {
         struct sigaction act;
         int i, sigs[] = { SIGSEGV, SIGILL, SIGFPE, SIGBUS, 0 };

         /* Ignore SIGPIPEs so they don't kill us. */
         signal( SIGPIPE, SIG_IGN );
         for( i = 0; sigs[ i ]; ++i )
         {
            sigaction( sigs[ i ], 0, &act );
            act.sa_sigaction = hb_signalExceptionHandler;
            act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESETHAND;
            sigaction( sigs[ i ], &act, 0 );
         }
      }
   }
#endif
}

void hb_vmUnsetExceptionHandler( void )
{
#if defined( HB_OS_OS2 ) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      /* I don't do any check on return code since harbour is exiting in any case */
      rc = DosUnsetExceptionHandler( &s_regRec );
      HB_SYMBOL_UNUSED( rc );
   }
#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )
   {
      /* we are using static buffer for alternative stack so we do not
       * have to deallocate it to free the memory on application exit
       */
#if 0
      stack_t ss, oss;
      ss.ss_sp = NULL;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = SS_DISABLE;
      /* set alternative stack for SIGSEGV executed on stack overflow */
      if( sigaltstack( &ss, &oss ) == 0 )
      {
         if( oss.ss_sp && SS_DISABLE )
            free( oss.ss_sp );
      }
#endif
   }
#endif
}
