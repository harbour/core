/*
 * xHarbour Project source code:
 * The Service/Daemon support
 * (Includes also signal/low level error management)
 *
 * Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.txt.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbserv.h"
#include "hbthread.h"

#include <stdio.h>

#if defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#endif

/* These targets can't compile this module */
#if ! defined( HB_OS_DOS ) && \
   ! defined( HB_OS_DARWIN_5 ) && \
   ! ( defined( HB_OS_WIN_CE ) && ( defined( __POCC__ ) || ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) ) ) && \
   ! defined( HB_OS_WIN_64 ) && \
   ( ! defined( HB_OS_OS2 ) || defined( HB_OS_OS2_GCC ) ) && \
   ! defined( __HAIKU__ )
/* TODO: Haiku will supposedly do this later on, read /boot/develop/headers/posix/signal.h */

#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2_GCC )
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#endif

/* TODO:
   we'll use hb_fsPopen for popening once we put it multiplatform. For now: */
#ifdef HB_OS_WIN
   #define popen                       _popen
   #define pclose                      _pclose
#endif

#ifdef __LCC__
#define EXCEPTION_ILLEGAL_INSTRUCTION  STATUS_ILLEGAL_INSTRUCTION
#endif

/**************************************************
 * Global definition, valid for all systems
 ***************************************************/

static void s_serviceSetHBSig( void );
static void s_serviceSetDflSig( void );
static void s_signalHandlersInit( void );

static PHB_ITEM sp_hooks       = NULL;
static HB_BOOL  bSignalEnabled = HB_TRUE;
static int      sb_isService   = 0;

/* There is a service mutex in multithreading */
static HB_CRITICAL_NEW( s_ServiceMutex );

/* This structure holds a translation to transform a certain OS level signal
   into abstract HB_SIGNAL; os specific implementation must provide the
   s_sigTable containing all the available translations */

typedef struct
{
   HB_UINT sig;
   HB_UINT subsig;
   HB_UINT translated;
} S_TUPLE;

static int s_translateSignal( HB_UINT sig, HB_UINT subsig );

/*****************************************************************************
* Unix specific signal handling implementation
*
* This section has unix specific code to manage the
* signals, both from kernel or from users.
*****************************************************************************/

#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2_GCC )

/* TODO: Register the old signal action to allow graceful fallback
         static struct sigaction sa_oldAction[ SIGUSR2 + 1 ]; */

/* Implementation of the signal translation table */
static S_TUPLE s_sigTable[] =
{
   { SIGHUP,  0, HB_SIGNAL_REFRESH   },
   { SIGINT,  0, HB_SIGNAL_INTERRUPT },
   { SIGQUIT, 0, HB_SIGNAL_QUIT      },
   { SIGILL,  0, HB_SIGNAL_FAULT     },
   { SIGABRT, 0, HB_SIGNAL_QUIT      },
   { SIGFPE,  0, HB_SIGNAL_MATHERR   },
   { SIGSEGV, 0, HB_SIGNAL_FAULT     },
   { SIGTERM, 0, HB_SIGNAL_QUIT      },
   { SIGUSR1, 0, HB_SIGNAL_USER1     },
   { SIGUSR2, 0, HB_SIGNAL_USER2     },
   { 0,       0, 0                   }
};

#if defined( HB_OS_OS2_GCC ) || defined( __WATCOMC__ )
static void s_signalHandler( int sig )
#else
static void s_signalHandler( int sig, siginfo_t * info, void * v )
#endif
{
   HB_UINT  uiMask;
   HB_UINT  uiSig;
   PHB_ITEM pFunction, pExecArray, pRet;
   HB_SIZE  nPos;
   int      iRet;

   #if ! ( defined( HB_OS_OS2_GCC ) || defined( __WATCOMC__ ) )
   HB_SYMBOL_UNUSED( v );
   #endif

   /* let's find the right signal handler. */
   hb_threadEnterCriticalSection( &s_ServiceMutex );

   /* avoid working if PRG signal handling has been disabled */
   if( ! bSignalEnabled )
   {
      hb_threadLeaveCriticalSection( &s_ServiceMutex );
      return;
   }

   bSignalEnabled = HB_FALSE;
   nPos = hb_arrayLen( sp_hooks );
   /* subsig not necessary */
   uiSig = ( HB_UINT ) s_translateSignal( ( HB_UINT ) sig, 0 );

   while( nPos > 0 )
   {
      pFunction = hb_arrayGetItemPtr( sp_hooks, nPos );
      uiMask    = ( HB_UINT ) hb_arrayGetNI( pFunction, 1 );
      if( uiMask & uiSig )
      {
         /* we don't unlock the mutex now, even if it is
            a little dangerous. But we are in a signal hander...
            for now just 2 parameters */
         pExecArray = hb_itemArrayNew( 3 );
         hb_arraySet( pExecArray, 1, hb_arrayGetItemPtr( pFunction, 2 ) );
         hb_arraySetNI( pExecArray, 2, uiSig );

         /* the third parameter is an array: */

         pRet = hb_arrayGetItemPtr( pExecArray, 3 );
         #if defined( HB_OS_OS2_GCC ) || defined( __WATCOMC__ )
         hb_arrayNew( pRet, 1 );
         #elif defined( HB_OS_BSD )
         hb_arrayNew( pRet, info ? 6 : 1 );
         #else
         hb_arrayNew( pRet, 6 );
         #endif
         hb_arraySetNI( pRet, HB_SERVICE_OSSIGNAL, sig );
         #if ! ( defined( HB_OS_OS2_GCC ) || defined( __WATCOMC__ ) )
         #if defined( HB_OS_BSD )
         if( info )
         #endif
         {
            hb_arraySetNI( pRet, HB_SERVICE_OSSUBSIG, info->si_code );
            #if ! defined( HB_OS_VXWORKS )
            hb_arraySetNI( pRet, HB_SERVICE_OSERROR, info->si_errno );
            hb_arraySetPtr( pRet, HB_SERVICE_ADDRESS, ( void * ) info->si_addr );
            hb_arraySetNI( pRet, HB_SERVICE_PROCESS, info->si_pid );
            hb_arraySetNI( pRet, HB_SERVICE_UID, info->si_uid );
            #endif
         }
         #endif

         pRet = hb_itemDo( pExecArray, 0 );
         iRet = hb_itemGetNI( pRet );
         hb_itemRelease( pRet );
         hb_itemRelease( pExecArray );

         switch( iRet )
         {
            case HB_SERVICE_HANDLED:
               bSignalEnabled = HB_TRUE;
               hb_threadLeaveCriticalSection( &s_ServiceMutex );
               return;

            case HB_SERVICE_QUIT:
               bSignalEnabled = HB_FALSE;
               hb_threadLeaveCriticalSection( &s_ServiceMutex );
               /* TODO: A service cleanup routine */
               hb_vmRequestQuit();
               /* Allow signals to go through pthreads */
               s_serviceSetDflSig();
               /* NOTICE: should be pthread_exit(0), but a bug in linuxthread prevents it:
                  calling pthread exit from a signal handler will cause infinite wait for
                  restart signal.
                  This solution is rude, while the other would allow clean VM termination...
                  but it works.
                */
               exit( 0 );
         }
      }
      nPos--;
   }

   bSignalEnabled = HB_TRUE;
   /*s_serviceSetHBSig();*/

   #if 0
   if( uiSig != HB_SIGNAL_UNKNOWN )
   {
      if( sa_oldAction[ sig ].sa_flags & SA_SIGINFO )
         sa_oldAction[ sig ].sa_sigaction( sig, info, v );
      else
         sa_oldAction[ sig ].sa_handler( sig );
   }
   #endif
}

/* 2003 - <maurilio.longo@libero.it>
   to fix as soon as thread support is ready on OS/2
 */
#if defined( HB_THREAD_SUPPORT ) && ! defined( HB_OS_OS2 )
static void * s_signalListener( void * my_stack )
{
   static HB_BOOL bFirst = HB_TRUE;
   sigset_t       passall;
   HB_STACK *     pStack = ( HB_STACK * ) my_stack;

#if defined( HB_OS_BSD )
   int sig;
#else
   siginfo_t sinfo;
#endif

#ifdef HB_THREAD_TLS_KEYWORD
   hb_thread_stack = my_stack;
#else
   pthread_setspecific( hb_pkCurrentStack, my_stack );
#endif

   pStack->th_id = HB_CURRENT_THREAD();
   hb_threadLinkStack( pStack );
   HB_STACK_LOCK;

   /* and now accepts all signals */
   sigemptyset( &passall );

   /* and wait for all signals */
   sigaddset( &passall, SIGHUP );
   sigaddset( &passall, SIGQUIT );
   sigaddset( &passall, SIGILL );
   sigaddset( &passall, SIGABRT );
   sigaddset( &passall, SIGFPE );
   sigaddset( &passall, SIGSEGV );
   sigaddset( &passall, SIGTERM );
   sigaddset( &passall, SIGUSR1 );
   sigaddset( &passall, SIGUSR2 );
   sigaddset( &passall, SIGHUP );

   pthread_cleanup_push( hb_threadTerminator, my_stack );
   pthread_setcanceltype( PTHREAD_CANCEL_DEFERRED, NULL );
   pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );

   for(;; )
   {
      /* allow safe cancelation */
      HB_STACK_UNLOCK;

      /* reset signal handling; this is done here (so I don't
         mangle with pthread_ calls, and I don't hold mutexes),
         and just once (doing it twice would be useless). */
      if( bFirst )
      {
         pthread_sigmask( SIG_SETMASK, &passall, NULL );
         bFirst = HB_FALSE;
      }

      /* This is also a cancelation point. When the main thread
         is done, it will kill all the threads having a stack
         including this one.
         ATM we don't care very much about signal handling during
         termination: no handler is set for them, so the DFL
         action is taken (and that should be fine). */
#if defined( HB_OS_BSD )
      sigwait( &passall, &sig );
#else
      sigwaitinfo( &passall, &sinfo );
#endif

      /* lock stack before passing the ball to VM. */
      HB_STACK_LOCK;
#if defined( HB_OS_BSD )
      s_signalHandler( sig, NULL, NULL );
#else
      s_signalHandler( sinfo.si_signo, &sinfo, NULL );
#endif
   }

   pthread_cleanup_pop( 1 );
   return 0;
}
#endif
#endif

/*****************************************************************************
* Windows specific exception filter system.
*
* Windows will only catch exceptions; It is necessary to rely on the
* HB_SERVICELOOP to receive user generated messages.
*****************************************************************************/

#ifdef HB_OS_WIN
static void s_serviceSetHBSig( void );

/* message filter hook for user generated signals */
static HHOOK s_hMsgHook = NULL;

/* old error mode */
static UINT s_uiErrorMode = 0;

/* -------------------------------
   implementation of the signal translation table
   Under windows, 0 is a system exception, while 1 is a user message
 */
static S_TUPLE s_sigTable[] = {

   /* memory/processor fault exception */
   { 0, EXCEPTION_ACCESS_VIOLATION,      HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_ILLEGAL_INSTRUCTION,   HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_IN_PAGE_ERROR,         HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_STACK_OVERFLOW,        HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_PRIV_INSTRUCTION,      HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_ARRAY_BOUNDS_EXCEEDED, HB_SIGNAL_FAULT     },
   { 0, EXCEPTION_DATATYPE_MISALIGNMENT, HB_SIGNAL_FAULT     },

   /* Math exceptions */
   { 0, EXCEPTION_FLT_DENORMAL_OPERAND,  HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_INVALID_OPERATION, HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_INEXACT_RESULT,    HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_DIVIDE_BY_ZERO,    HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_OVERFLOW,          HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_STACK_CHECK,       HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_FLT_UNDERFLOW,         HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_INT_DIVIDE_BY_ZERO,    HB_SIGNAL_MATHERR   },
   { 0, EXCEPTION_INT_OVERFLOW,          HB_SIGNAL_MATHERR   },

   /* User requests */
   { 1, WM_USER,                         HB_SIGNAL_USER1     },
   { 1, WM_USER + 1,                     HB_SIGNAL_USER2     },
   { 1, WM_USER + 2,                     HB_SIGNAL_REFRESH   },
   { 1, WM_USER + 3,                     HB_SIGNAL_INTERRUPT },
   { 1, WM_QUIT,                         HB_SIGNAL_QUIT      },

   /* Console handler */
   { 2, CTRL_C_EVENT,                    HB_SIGNAL_INTERRUPT },
   { 2, CTRL_BREAK_EVENT,                HB_SIGNAL_INTERRUPT },
   { 2, CTRL_CLOSE_EVENT,                HB_SIGNAL_QUIT      },
   { 2, CTRL_LOGOFF_EVENT,               HB_SIGNAL_QUIT      },
   { 2, CTRL_SHUTDOWN_EVENT,             HB_SIGNAL_QUIT      },

   { 0, 0,                               0                   }
};

/* -------------------------------
   Manager of signals for windows
 */
static LONG s_signalHandler( int type, int sig, PEXCEPTION_RECORD exc )
{
   PHB_ITEM pFunction, pExecArray, pRet;
   HB_SIZE  nPos;
   HB_UINT  uiSig, uiMask;
   int      iRet;

   /* let's find the right signal handler. */
   hb_threadEnterCriticalSection( &s_ServiceMutex );

   /* avoid working if PRG signal handling has been disabled */
   if( ! bSignalEnabled )
   {
      hb_threadLeaveCriticalSection( &s_ServiceMutex );
      return EXCEPTION_EXECUTE_HANDLER;
   }

   bSignalEnabled = HB_FALSE;
   nPos = hb_arrayLen( sp_hooks );
   /* subsig not necessary */
   uiSig = ( HB_UINT ) s_translateSignal( ( HB_UINT ) type, ( HB_UINT ) sig );

   while( nPos > 0 )
   {
      pFunction = hb_arrayGetItemPtr( sp_hooks, nPos );
      uiMask    = ( HB_UINT ) hb_arrayGetNI( pFunction, 1 );
      if( ( uiMask & uiSig ) == uiSig )
      {
         /* we don't unlock the mutex now, even if it is
            a little dangerous. But we are in a signal hander...
            for now just 2 parameters */
         pExecArray = hb_itemArrayNew( 3 );
         hb_arraySetForward( pExecArray, 1, hb_arrayGetItemPtr( pFunction, 2 ) );
         hb_arraySetNI( pExecArray, 2, uiSig );

         /* the third parameter is an array:
          * 1: low-level signal
          * 2: low-level subsignal
          * 3: low-level system error
          * 4: address that rised the signal
          * 5: process id of the signal riser
          * 6: UID of the riser
          */

         pRet = hb_arrayGetItemPtr( pExecArray, 3 );
         hb_arrayNew( pRet, 6 );

         hb_arraySetNI( pRet, HB_SERVICE_OSSIGNAL, type );
         hb_arraySetNI( pRet, HB_SERVICE_OSSUBSIG, sig );
         /* could be meaningless, but does not matter here */
         hb_arraySetNI( pRet, HB_SERVICE_OSERROR, GetLastError() );

         if( type == 0 ) /* exception */
         {
            hb_arraySetPtr( pRet, HB_SERVICE_ADDRESS, ( void * ) exc->ExceptionAddress );
         }
         else
         {
            hb_arraySetPtr( pRet, HB_SERVICE_ADDRESS, NULL );
         }
         /* TODO: */
         hb_arraySetNI( pRet, HB_SERVICE_PROCESS, GetCurrentThreadId() );
         /* TODO: */
         hb_arraySetNI( pRet, HB_SERVICE_UID, 0 );

         pRet = hb_itemDo( pExecArray, 0 );
         iRet = hb_itemGetNI( pRet );
         hb_itemRelease( pRet );
         hb_itemRelease( pExecArray );

         switch( iRet )
         {
            case HB_SERVICE_HANDLED:
               bSignalEnabled = HB_TRUE;
               hb_threadLeaveCriticalSection( &s_ServiceMutex );
               return EXCEPTION_CONTINUE_EXECUTION;

            case HB_SERVICE_QUIT:
               bSignalEnabled = HB_FALSE;
               hb_threadLeaveCriticalSection( &s_ServiceMutex );
               hb_vmRequestQuit();
#ifndef HB_THREAD_SUPPORT
               hb_vmQuit();
               exit( 0 );
#else
               hb_threadCancelInternal();
#endif
         }
      }
      nPos--;
   }

   bSignalEnabled = HB_TRUE;
   return EXCEPTION_EXECUTE_HANDLER;
}


static LRESULT CALLBACK s_exceptionFilter( PEXCEPTION_POINTERS exInfo )
{
   return s_signalHandler( 0, exInfo->ExceptionRecord->ExceptionCode, exInfo->ExceptionRecord );
}

static LRESULT CALLBACK s_MsgFilterFunc( int nCode, WPARAM wParam, LPARAM lParam )
{
   PMSG msg;

   if( nCode < 0 )
      return CallNextHookEx( s_hMsgHook, nCode, wParam, lParam );

   msg = ( PMSG ) lParam;

   switch( msg->message )
   {
      case WM_USER:
      case WM_USER + 1:
      case WM_USER + 2:
      case WM_USER + 3:
      case WM_QUIT:
         /* we'll ignore the request here.
            the application must still receive the message */
         s_signalHandler( 1, msg->message, NULL );
   }

   /* return next hook anyway */
   return CallNextHookEx( s_hMsgHook, nCode, wParam, lParam );
}

#ifdef HB_THREAD_SUPPORT
extern DWORD hb_dwCurrentStack;
#endif

BOOL WINAPI s_ConsoleHandlerRoutine( DWORD dwCtrlType )
{
#ifdef HB_THREAD_SUPPORT
   HB_STACK * pStack = NULL;

   /* we need a new stack: this is NOT an hb thread. */

   if( TlsGetValue( hb_dwCurrentStack ) == 0 )
   {
      pStack       = hb_threadCreateStack( GetCurrentThreadId() );
      pStack->th_h = GetCurrentThread();
      TlsSetValue( hb_dwCurrentStack, ( void * ) pStack );
   }
#endif

   s_signalHandler( 2, dwCtrlType, NULL );

#ifdef HB_THREAD_SUPPORT
   if( pStack )
      hb_threadDestroyStack( pStack );
#endif
   /* We have handled it */
   return TRUE;
}

#endif

/*****************************************************************************
* Filter/handlers setup/shutdown
* This utility functions are meant to abstract the process of declare and
* remove the signal handlers, and do it in a mutltiplatform fashon. Use this
* to implement new platform signal/exception handlers.
*****************************************************************************/

/*-----------------------------------------------------
   Set the signal handlers to our program interceptors.
 */

static void s_serviceSetHBSig( void )
{

#if defined( HB_OS_UNIX ) || defined( HB_OS_OS2_GCC )
   struct sigaction act;

#if defined( HB_THREAD_SUPPORT ) && ! defined( HB_OS_OS2 )
   sigset_t blockall;
   /* set signal mask */
   sigemptyset( &blockall );
   sigaddset( &blockall, SIGHUP );
   sigaddset( &blockall, SIGQUIT );
   sigaddset( &blockall, SIGILL );
   sigaddset( &blockall, SIGABRT );
   sigaddset( &blockall, SIGFPE );
   sigaddset( &blockall, SIGSEGV );
   sigaddset( &blockall, SIGTERM );
   sigaddset( &blockall, SIGUSR1 );
   sigaddset( &blockall, SIGUSR2 );
   sigaddset( &blockall, SIGHUP );

   pthread_sigmask( SIG_SETMASK, &blockall, NULL );
#endif

   /* to avoid problems with differ sigaction structures and uninitialized
      fields */
   memset( &act, 0, sizeof( struct sigaction ) );

   #if defined( HB_OS_OS2_GCC ) || defined( __WATCOMC__ )
   act.sa_handler = s_signalHandler;
   #else
   /* using more descriptive sa_action instead of sa_handler */
   act.sa_handler   = NULL;            /* if act.sa.. is a union, we just clean this */
   act.sa_sigaction = s_signalHandler; /* this is what matters */
   /* block al signals, we don't want to be interrupted. */
   /*sigfillset( &act.sa_mask );*/
   #endif


   #ifdef HB_OS_OS2_GCC
   act.sa_flags = SA_NOCLDSTOP;
   #else
   act.sa_flags = SA_NOCLDSTOP | SA_SIGINFO;
   #endif

   sigaction( SIGHUP, &act, NULL );
   sigaction( SIGQUIT, &act, NULL );
   sigaction( SIGILL, &act, NULL );
   sigaction( SIGABRT, &act, NULL );
   sigaction( SIGFPE, &act, NULL );
   sigaction( SIGSEGV, &act, NULL );
   sigaction( SIGTERM, &act, NULL );
   sigaction( SIGUSR1, &act, NULL );
   sigaction( SIGUSR2, &act, NULL );

   /* IGNORE pipe */
   signal( SIGPIPE, SIG_IGN );
#endif

#ifdef HB_OS_WIN
   /* disable all os-level error boxes */
   s_uiErrorMode = SetErrorMode(
      SEM_FAILCRITICALERRORS | SEM_NOALIGNMENTFAULTEXCEPT | SEM_NOGPFAULTERRORBOX |
      SEM_NOOPENFILEERRORBOX );

   SetUnhandledExceptionFilter( s_exceptionFilter );
   s_hMsgHook = SetWindowsHookEx( WH_GETMESSAGE, ( HOOKPROC ) s_MsgFilterFunc, NULL, GetCurrentThreadId() );
   SetConsoleCtrlHandler( s_ConsoleHandlerRoutine, TRUE );

#endif
}
/* ---------------------------------------------------
   Reset the signal handlers to the default OS value
 */


static void s_serviceSetDflSig( void )
{
#ifdef HB_OS_UNIX
   signal( SIGHUP, SIG_DFL );
   signal( SIGQUIT, SIG_DFL );
   signal( SIGILL, SIG_DFL );
   signal( SIGABRT, SIG_DFL );
   signal( SIGFPE, SIG_DFL );
   signal( SIGSEGV, SIG_DFL );
   signal( SIGTERM, SIG_DFL );
   signal( SIGUSR1, SIG_DFL );
   signal( SIGUSR2, SIG_DFL );
   signal( SIGPIPE, SIG_DFL );
#endif

#ifdef HB_OS_WIN
   SetUnhandledExceptionFilter( NULL );
   if( s_hMsgHook != NULL )
   {
      UnhookWindowsHookEx( s_hMsgHook );
      s_hMsgHook = NULL;
   }
   SetErrorMode( s_uiErrorMode );
   SetConsoleCtrlHandler( s_ConsoleHandlerRoutine, FALSE );
#endif
}


/* ---------------------------------------------------
   This translates a signal into abstract HB_SIGNAL
   from os specific representation
 */

static int s_translateSignal( HB_UINT sig, HB_UINT subsig )
{
   int i = 0;

   while( s_sigTable[ i ].sig != 0 || s_sigTable[ i ].subsig != 0 || s_sigTable[ i ].translated != 0 )
   {
      if( s_sigTable[ i ].sig == sig &&
          ( s_sigTable[ i ].subsig == subsig || s_sigTable[ i ].subsig == 0 ) )
      {
         return s_sigTable[ i ].translated;
      }
      i++;
   }
   return HB_SIGNAL_UNKNOWN;
}

static void hb_service_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hb_serviceExit();
}

/**
 * Initializes signal handler system
 */

static void s_signalHandlersInit()
{
#if defined( HB_THREAD_SUPPORT ) && ( defined( HB_OS_UNIX ) || defined( HB_OS_UNIX ) )
   pthread_t  res;
   HB_STACK * pStack;

   s_serviceSetHBSig();

   pStack = hb_threadCreateStack( 0 );
   pthread_create( &res, NULL, s_signalListener, pStack );
#else
   s_serviceSetHBSig();
#endif

   sp_hooks = hb_itemNew( NULL );
   hb_arrayNew( sp_hooks, 0 );
   hb_vmAtQuit( hb_service_exit, NULL );
}

/*****************************************************************************
* HB_*Service routines
* This is the core of the service system.
*****************************************************************************/

/**
 * Starts the service system.
 * Initializes the needed variables.
 * On unix: if the parameter is .T., puts the server in daemonic mode, detaching
 * the main thread from the console and terminating it.
 */

HB_FUNC( HB_STARTSERVICE )
{
   #ifdef HB_THREAD_SUPPORT
   int iCount = hb_threadCountStacks();
   if( iCount > 2 || ( sp_hooks == NULL && iCount > 1 ) )
   {
      /* TODO: Right error code here */
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Service must be started before starting threads", NULL, 0 );
      return;
   }
   #endif

   #if defined( HB_OS_UNIX ) && ! defined( HB_OS_VXWORKS )
   {
      int pid;

      /* Iconic? */
      if( hb_parl( 1 ) )
      {
         pid = fork();

         if( pid != 0 )
         {
            hb_vmRequestQuit();
            return;
         }
#ifdef HB_THREAD_SUPPORT
   #ifdef HB_THREAD_TLS_KEYWORD
         hb_thread_stack = &hb_stackMT;
   #else
         pthread_setspecific( hb_pkCurrentStack, ( void * ) &hb_stackMT );
   #endif
#endif
      }
   }
   #endif

   /* let's begin */
   sb_isService = HB_TRUE;

   /* in windows, we just detach from console */
   #ifdef HB_OS_WIN
   if( hb_parl( 1 ) )
   {
      FreeConsole();
   }
   #endif

   /* Initialize only if the service has not yet been initialized */
   if( sp_hooks == NULL )
   {
      s_signalHandlersInit();
   }
}

/**
 * Returns true if the current program is a service, that is if hb_StartService() has
 * Been called. C version useful for internal api
 */

HB_BOOL hb_isService( void )
{
   return sb_isService;
}

/**
 * Clean up when system exits
 * Called from hb_vmQuit()
 */

void hb_serviceExit( void )
{
   if( sp_hooks != NULL )
   {
      /* reset default signal handling */
      s_serviceSetDflSig();
      hb_itemRelease( sp_hooks );
      sp_hooks = NULL;
   }
}


/**
 * Returns true if the current program is a service, that is if hb_StartService() has
 * Been called.
 */
HB_FUNC( HB_ISSERVICE )
{
   hb_retl( sb_isService );
}

/**
 * This is -at least- an helper functions that implements the main loop for
 * the service/daemon system.
 * The minimal thing to do is a hb_gcCollectAll(), because, generally, servers
 * are not interactive, so they tend to have garbage to collect.
 * Under windows, it peeks the pending messages and send the relevant ones
 * (quit, user+1 and user+2) to our handling functions.
 */

HB_FUNC( HB_SERVICELOOP )
{
#ifdef HB_OS_WIN
   MSG msg;
   /* This is just here to trigger our internal hook routine, if the
      final application does not any message handling.
    */
   if( ! PeekMessage( &msg, NULL, WM_QUIT, WM_QUIT, PM_REMOVE ) )
   {
      PeekMessage( &msg, NULL, WM_USER, WM_USER + 3, PM_REMOVE );
   }
#endif

   hb_gcCollectAll( HB_FALSE );
}

HB_FUNC( HB_PUSHSIGNALHANDLER )
{
   int      iMask = hb_parni( 1 );
   PHB_ITEM pFunc = hb_param( 2, HB_IT_ANY ), pHandEntry;

   if( pFunc == NULL || iMask == 0 ||
       ( ! HB_IS_POINTER( pFunc ) && ! HB_IS_STRING( pFunc ) && ! HB_IS_BLOCK( pFunc ) )
       )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Wrong parameter count/type", NULL,
                            2, hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ) );
      return;
   }

   pHandEntry = hb_itemArrayNew( 2 );
   hb_arraySetNI( pHandEntry, 1, iMask );
   hb_arraySet( pHandEntry, 2, pFunc );

   /* if the hook is not initialized, initialize it */
   if( sp_hooks == NULL )
   {
      s_signalHandlersInit();
   }

   hb_threadEnterCriticalSection( &s_ServiceMutex );

   hb_arrayAddForward( sp_hooks, pHandEntry );

   hb_threadLeaveCriticalSection( &s_ServiceMutex );

   hb_itemRelease( pHandEntry );
}


HB_FUNC( HB_POPSIGNALHANDLER )
{
   int nLen;

   if( sp_hooks != NULL )
   {
      hb_threadEnterCriticalSection( &s_ServiceMutex );

      nLen = hb_arrayLen( sp_hooks );
      if( nLen > 0 )
      {
         hb_arrayDel( sp_hooks, nLen );
         hb_arrayDel( sp_hooks, nLen - 1 );
         hb_arraySize( sp_hooks, nLen - 2 );
         hb_retl( HB_TRUE );
         if( hb_arrayLen( sp_hooks ) == 0 )
         {
            hb_itemRelease( sp_hooks );
            sp_hooks = NULL;              /* So it can be reinitilized */
         }
      }
      else
      {
         hb_retl( HB_FALSE );
      }
      hb_threadLeaveCriticalSection( &s_ServiceMutex );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

/**
 * Return a character description of the low level signal that has been
 * issued to signal handling routines. This is system dependant.
 * TODO: Make it international through the xHarbour standard message system.
 */
HB_FUNC( HB_SIGNALDESC )
{
   int iSig    = hb_parni( 1 );
   int iSubSig = hb_parni( 2 );

   /* UNIX MESSGES */
   #if defined( HB_OS_UNIX ) || defined( HB_OS_OS2_GCC )

   switch( iSig )
   {
      case SIGSEGV: switch( iSubSig )
         {
         #if ! defined( HB_OS_BSD ) && ! defined( HB_OS_OS2_GCC ) && ! defined( __WATCOMC__ )
            case SEGV_MAPERR: hb_retc_const( "Segmentation fault: address not mapped to object" ); return;
            case SEGV_ACCERR: hb_retc_const( "Segmentation fault: invalid permissions for mapped object" ); return;
         #endif
            default: hb_retc_const( "Segmentation fault" ); return;
         }

      case SIGILL: switch( iSubSig )
         {
         #if ! defined( HB_OS_BSD ) && ! defined( HB_OS_OS2_GCC ) && ! defined( __WATCOMC__ )
            case ILL_ILLOPC: hb_retc_const( "Illegal operation: illegal opcode" ); return;
            case ILL_ILLOPN: hb_retc_const( "Illegal operation: illegal operand" ); return;
            case ILL_ILLADR: hb_retc_const( "Illegal operation: illegal addressing mode" ); return;
            case ILL_ILLTRP: hb_retc_const( "Illegal operation: illegal trap" ); return;
            case ILL_PRVOPC: hb_retc_const( "Illegal operation: privileged opcode" ); return;
            case ILL_PRVREG: hb_retc_const( "Illegal operation: privileged register" ); return;
            case ILL_COPROC: hb_retc_const( "Illegal operation: coprocessor error" ); return;
            case ILL_BADSTK: hb_retc_const( "Illegal operation: internal stack error" ); return;
         #endif
            default: hb_retc_const( "Illegal operation" ); return;
         }

      case SIGFPE: switch( iSubSig )
         {
         #if ! defined( HB_OS_OS2_GCC ) && ! defined( __WATCOMC__ )
         #if ! defined( HB_OS_DARWIN )
            case FPE_INTDIV: hb_retc_const( "Floating point: integer divide by zero" ); return;
            case FPE_INTOVF: hb_retc_const( "Floating point: integer overflow" ); return;
         #endif
            case FPE_FLTDIV: hb_retc_const( "Floating point: floating point divide by zero" ); return;
            case FPE_FLTOVF: hb_retc_const( "Floating point: floating point overflow" ); return;
            case FPE_FLTUND: hb_retc_const( "Floating point: floating point underflow" ); return;
            case FPE_FLTRES: hb_retc_const( "Floating point: floating point inexact result" ); return;
         #if ! defined( HB_OS_VXWORKS )
            case FPE_FLTINV: hb_retc_const( "Floating point: floating point invalid operation" ); return;
         #endif
         #if ! defined( HB_OS_DARWIN )
            case FPE_FLTSUB: hb_retc_const( "Floating point: subscript out of range" ); return;
         #endif
         #endif
            default: hb_retc_const( "Floating point" ); return;
         }

      case SIGQUIT:
         hb_retc_const( "Quit" );
         return;

      case SIGHUP:
         hb_retc_const( "Update" );
         return;

      case SIGINT:
         hb_retc_const( "Interrupt" );
         return;

      case SIGPIPE:
         hb_retc_const( "Broken pipe" );
         return;

      case SIGTERM:
         hb_retc_const( "Terminate process" );
         return;

      case SIGABRT:
         hb_retc_const( "Abort" );

      case SIGUSR1:
         hb_retc_const( "User defined" );
         return;

      case SIGUSR2:
         hb_retc_const( "User defined (secondary)" );
         return;
   }
   #endif

   #ifdef HB_OS_WIN
   if( iSig == 0 ) /* exception */
   {
      switch( iSubSig )
      {
         case EXCEPTION_ACCESS_VIOLATION:
            hb_retc_const( "Memory read/write access violation" ); return;

         case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
            hb_retc_const( "Array out of bounds" ); return;

         case EXCEPTION_DATATYPE_MISALIGNMENT:
            hb_retc_const( "Data misaligned" ); return;

         case EXCEPTION_FLT_DENORMAL_OPERAND:
            hb_retc_const( "Denormal operand in Floating-point operation" ); return;

         case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            hb_retc_const( "Floating-point division by zero" ); return;

         case EXCEPTION_FLT_INEXACT_RESULT:
            hb_retc_const( "Inexact floating-point operation result" ); return;

         case EXCEPTION_FLT_INVALID_OPERATION:
            hb_retc_const( "Invalid floating-point operation" ); return;

         case EXCEPTION_FLT_OVERFLOW:
            hb_retc_const( "Floating-point numeric overflow" ); return;

         case EXCEPTION_FLT_STACK_CHECK:
            hb_retc_const( "Floating-point out of stack" ); return;

         case EXCEPTION_FLT_UNDERFLOW:
            hb_retc_const( "Floating-point numeric underflow" ); return;

         case EXCEPTION_ILLEGAL_INSTRUCTION:
            hb_retc_const( "Illegal instruction" ); return;

         case EXCEPTION_IN_PAGE_ERROR:
            hb_retc_const( "Paging error" ); return;

         case EXCEPTION_INT_DIVIDE_BY_ZERO:
            hb_retc_const( "Integer division by zero" ); return;

         case EXCEPTION_INT_OVERFLOW:
            hb_retc_const( "Integer numeric overflow" ); return;

         case EXCEPTION_PRIV_INSTRUCTION:
            hb_retc_const( "Illegal instruction for current machine mode" ); return;

         case EXCEPTION_STACK_OVERFLOW:
            hb_retc_const( "Stack overflow" ); return;
      }
   }

   #endif

   hb_retc_const( "Unrecognized signal" );
}


/*****************************************************************************
 * Debug help: generates a fault or a math error to see if signal catching
 * is working
 **************************************/

HB_FUNC( HB_SERVICEGENERATEFAULT )
{
   int * pGPF = NULL;

   *pGPF = 0;
   /* if it doesn't cause GPF (on some platforms it's possible) try this */
   *( --pGPF ) = 0;
}

HB_FUNC( HB_SERVICEGENERATEFPE )
{
   static double a = 100.0, b = 0.0;

   a = a / b;
}

#else

HB_FUNC( HB_STARTSERVICE ) {}
HB_FUNC( HB_ISSERVICE ) {}
HB_FUNC( HB_SERVICELOOP ) {}
HB_FUNC( HB_PUSHSIGNALHANDLER ) {}
HB_FUNC( HB_POPSIGNALHANDLER ) {}
HB_FUNC( HB_SIGNALDESC ) {}
HB_FUNC( HB_SERVICEGENERATEFAULT ) {}
HB_FUNC( HB_SERVICEGENERATEFPE ) {}

#endif
