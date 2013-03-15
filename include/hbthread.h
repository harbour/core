/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    header file with MT mode functions
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#ifndef HB_THREAD_H_
#define HB_THREAD_H_

#include "hbapi.h"
#include "hbset.h"

#if defined( HB_TASK_THREAD )
   /* Harbour tasks explicitly requested */
#elif ( defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) ) || \
      defined( HB_OS_DOS ) || defined( HB_OS_MINIX )
#  define HB_TASK_THREAD
#elif defined( HB_OS_LINUX ) || defined( HB_OS_DARWIN ) || \
      defined( HB_OS_SUNOS ) || defined( HB_OS_HPUX ) || \
      defined( HB_OS_BSD ) || defined( HB_OS_BEOS ) || \
      defined( HB_OS_QNX ) || defined( HB_OS_VXWORKS ) || \
      defined( HB_OS_SYMBIAN ) || defined( HB_OS_CYGWIN ) || \
      defined( HB_OS_AIX )
#  include <pthread.h>
#  define HB_PTHREAD_API
#  if defined( HB_OS_VXWORKS )
#     /* Avoids compiler warnings in mutex initialization. MT still doesn't work though. */
#     define HB_CRITICAL_NEED_INIT
#  endif
#elif defined( HB_OS_WIN )
#  include <windows.h>
#  if ! defined( HB_OS_WIN_CE )
#    include <process.h>
#  endif
#elif defined( HB_OS_OS2 )
#  include <os2.h>
#  if defined( __WATCOMC__ )
#     include <process.h>
#  endif
#endif

HB_EXTERN_BEGIN

#if defined( HB_TASK_THREAD )

#  include "hbtask.h"

   typedef HB_MAXINT          HB_THREAD_NO;
   typedef void *             HB_THREAD_ID;
   typedef void *             HB_CRITICAL_T;
   typedef void *             HB_COND_T;
   typedef void *             HB_THREAD_HANDLE;
   typedef HB_CRITICAL_T      HB_RAWCRITICAL_T;
   typedef HB_COND_T          HB_RAWCOND_T;

#  define HB_COND_OS_SUPPORT
#  undef  HB_COND_HARBOUR_SUPPORT
#  undef  HB_COND_NEED_INIT
#  undef  HB_CRITICAL_NEED_INIT


#  define HB_THREAD_STARTFUNC( func )     void * func( void * Cargo )
#  define HB_THREAD_END                   return NULL;
#  define HB_THREAD_RAWEND                return NULL;

#  define HB_THREAD_INFINITE_WAIT   HB_TASK_INFINITE_WAIT

#  define HB_THREAD_SELF()          hb_taskSelf()
#  define HB_THREAD_SHEDULER()      hb_taskSheduler()

#  define HB_CRITICAL_NEW( name )   HB_CRITICAL_T name = NULL
#  define HB_CRITICAL_INIT(v)       do { (v) = NULL; } while( 0 )
#  define HB_CRITICAL_DESTROY(v)    hb_taskDestroyMutex( &(v) )
#  define HB_CRITICAL_LOCK(v)       hb_taskLock( &(v), HB_TASK_INFINITE_WAIT )
#  define HB_CRITICAL_UNLOCK(v)     hb_taskUnlock( &(v) )

#  define HB_COND_NEW( name )       HB_COND_T name = NULL
#  define HB_COND_INIT(v)           do { (v) = NULL; } while( 0 )
#  define HB_COND_DESTROY(v)        hb_taskDestroyCond( &(v) )
#  define HB_COND_SIGNAL(v)         hb_taskSignal( &(v) )
#  define HB_COND_SIGNALN(v,n)      hb_taskBroadcast( &(v) )
#  define HB_COND_WAIT(c,m)         hb_taskWait( (c), (m), HB_TASK_INFINITE_WAIT )
#  define HB_COND_TIMEDWAIT(c,m,n)  hb_taskWait( (c), (m), (n) )

#elif defined( HB_PTHREAD_API )

   typedef HB_MAXINT       HB_THREAD_NO;
   typedef pthread_t       HB_THREAD_ID;
   typedef pthread_t       HB_THREAD_HANDLE;
   typedef pthread_mutex_t HB_RAWCRITICAL_T;
   typedef pthread_cond_t  HB_RAWCOND_T;

#  define HB_THREAD_STARTFUNC( func )     void * func( void * Cargo )
#  define HB_THREAD_END                   return NULL;
#  define HB_THREAD_RAWEND                return NULL;

#  define HB_THREAD_SELF()          pthread_self()
#  define HB_THREAD_EQUAL( x, y )   pthread_equal( x, y )

#  define HB_CRITICAL_INIT(v)       pthread_mutex_init( &(v), NULL )
#  define HB_CRITICAL_DESTROY(v)    pthread_mutex_destroy( &(v) )
#  define HB_CRITICAL_LOCK(v)       pthread_mutex_lock( &(v) )
#  define HB_CRITICAL_UNLOCK(v)     pthread_mutex_unlock( &(v) )
#  define HB_COND_INIT(v)           pthread_cond_init( &(v), NULL )
#  define HB_COND_DESTROY(v)        pthread_cond_destroy( &(v) )
#  define HB_COND_SIGNAL(v)         pthread_cond_signal( &(v) )
#  define HB_COND_SIGNALN(v,n)      pthread_cond_broadcast( &(v) )

#  define HB_COND_OS_SUPPORT        /* OS support for conditional variables */
#  undef  HB_COND_HARBOUR_SUPPORT

#  if defined( PTHREAD_MUTEX_INITIALIZER ) && ! defined( HB_CRITICAL_NEED_INIT )
      typedef pthread_mutex_t          HB_CRITICAL_T;
#     define HB_CRITICAL_NEW( name )   HB_CRITICAL_T name = PTHREAD_MUTEX_INITIALIZER
#     define HB_CRITICAL_GET(v)        ( v )
#  else
      /* platform does not support static mutex initialization */
#     if ! defined( HB_CRITICAL_NEED_INIT )
#        define HB_CRITICAL_NEED_INIT
#     endif
#     define HB_CRITICAL_GET(v)        ( &( (v)->critical.value ) )
#  endif

#  if defined( PTHREAD_COND_INITIALIZER ) && ! defined( HB_COND_NEED_INIT )
      typedef pthread_cond_t           HB_COND_T;
#     define HB_COND_NEW( name )       HB_COND_T name = PTHREAD_COND_INITIALIZER
#     define HB_COND_GET(v)            ( v )
#  else
      /* platform does not support static condition var initialization */
#     if ! defined( HB_COND_NEED_INIT )
#        define HB_COND_NEED_INIT
#     endif
#     define HB_COND_GET(v)            ( &( (v)->cond.value ) )
#  endif

#elif defined( HB_OS_WIN )

   typedef HB_MAXINT          HB_THREAD_NO;
   typedef HANDLE             HB_THREAD_HANDLE;
   typedef CRITICAL_SECTION   HB_RAWCRITICAL_T;
   typedef HANDLE             HB_OSCOND_T;

#  if defined( HB_OS_WIN_CE ) && \
      ( ( defined( __MINGW32CE__ ) && ! defined( __MSVCRT__ ) ) || \
          defined( __POCC__ ) ) || \
        ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) )
#     define HB_THREAD_RAWWINAPI
#  endif

#  if defined( HB_THREAD_RAWWINAPI )
      typedef DWORD                       HB_THREAD_ID;
#     define HB_THREAD_STARTFUNC( func )  DWORD WINAPI func( void * Cargo )
#     define HB_THREAD_END                ExitThread( 0 ); return 0;
#  else
      typedef unsigned                    HB_THREAD_ID;
#     define HB_THREAD_STARTFUNC( func )  unsigned __stdcall func( void * Cargo )
#     define HB_THREAD_END                _endthreadex( 0 ); return 0;
#  endif
#  define HB_THREAD_RAWEND             return 0;

#  define HB_THREAD_SELF()          GetCurrentThreadId()

#  define HB_CRITICAL_INIT(v)       InitializeCriticalSection( &(v) )
#  define HB_CRITICAL_DESTROY(v)    DeleteCriticalSection( &(v) )
#  define HB_CRITICAL_LOCK(v)       EnterCriticalSection( &(v) )
#  define HB_CRITICAL_UNLOCK(v)     LeaveCriticalSection( &(v) )

#  undef  HB_COND_OS_SUPPORT
#  undef  HB_COND_NEED_INIT
#  define HB_COND_HARBOUR_SUPPORT
#  define HB_CRITICAL_NEED_INIT

#  define HB_THREAD_INFINITE_WAIT   INFINITE

#elif defined( HB_OS_OS2 )

   /* In OS2 thread ID is continuous integer number so we can use it directly
    * anyhow I'd prefer to not make such strict binding to OS values because
    * it may cause troubles when code will be ported to other platforms.
    */
   /* typedef TID                HB_THREAD_NO; */
   typedef HB_MAXINT          HB_THREAD_NO;
   typedef TID                HB_THREAD_ID;
   typedef TID                HB_THREAD_HANDLE;
   typedef HMTX               HB_RAWCRITICAL_T;
   typedef HEV                HB_OSCOND_T;

   extern ULONG _hb_gettid( void );

#  define HB_THREAD_STARTFUNC( func )     void func( void * Cargo )
#  define HB_THREAD_END                   _endthread(); return;
#  define HB_THREAD_RAWEND                return;

#  if defined( __GNUC__ ) && 0
#     define HB_THREAD_SELF()    ( ( TID ) _gettid() )
#  else
#     define HB_THREAD_SELF()    ( ( TID ) _hb_gettid() )
#  endif

#  define HB_CRITICAL_INIT(v)       DosCreateMutexSem( NULL, &(v), 0L, FALSE )
#  define HB_CRITICAL_DESTROY(v)    DosCloseMutexSem( v )
#  define HB_CRITICAL_LOCK(v)       DosRequestMutexSem( (v), SEM_INDEFINITE_WAIT )
#  define HB_CRITICAL_UNLOCK(v)     DosReleaseMutexSem( v )

#  undef  HB_COND_OS_SUPPORT
#  undef  HB_COND_NEED_INIT
#  define HB_COND_HARBOUR_SUPPORT
#  define HB_CRITICAL_NEED_INIT

#  define HB_THREAD_INFINITE_WAIT   SEM_INDEFINITE_WAIT

#  ifndef SEM_INDEFINITE_WAIT
#     define SEM_INDEFINITE_WAIT    ( ( HB_ULONG ) -1 )
#  endif

#else

   typedef int HB_THREAD_NO;
   typedef int HB_THREAD_ID;
   typedef int HB_THREAD_HANDLE;
   typedef int HB_CRITICAL_T;
   typedef int HB_RAWCRITICAL_T;
   typedef int HB_COND_T;
   typedef int HB_RAWCOND_T;

#  define HB_THREAD_STARTFUNC( func )     void func( void * Cargo )
#  define HB_THREAD_END                   return;
#  define HB_THREAD_RAWEND                return;

#  define HB_CRITICAL_NEW( name )      HB_CRITICAL_T name = 0
#  define HB_COND_NEW( name )          HB_COND_T name = 0
#  define HB_THREAD_SELF()             (-1)

#  define HB_CRITICAL_LOCK(v)
#  define HB_CRITICAL_UNLOCK(v)
#  define HB_COND_SIGNAL(v)
#  define HB_COND_SIGNALN(v,n)
#  define HB_COND_WAIT(v)           ( HB_FALSE )
#  define HB_COND_TIMEDWAIT(v,n)    ( HB_FALSE )

#endif

#if defined( HB_COND_HARBOUR_SUPPORT )

   typedef struct _HB_WAIT_LIST
   {
      struct _HB_WAIT_LIST *  prev;
      struct _HB_WAIT_LIST *  next;
      HB_OSCOND_T             cond;
      HB_BOOL                 signaled;
   } HB_WAIT_LIST, * PHB_WAIT_LIST;

   typedef struct
   {
      PHB_WAIT_LIST     waiters;
   } HB_COND_T, HB_RAWCOND_T;

#  define HB_COND_NEW( name )       HB_COND_T name = { NULL }

#  define HB_COND_SIGNAL(v)         _hb_thread_cond_signal( &(v) )
#  define HB_COND_SIGNALN(v,n)      _hb_thread_cond_broadcast( &(v) )
#  define HB_COND_WAIT(v)           _hb_thread_cond_wait( (v), HB_THREAD_INFINITE_WAIT )
#  define HB_COND_TIMEDWAIT(v,n)    _hb_thread_cond_wait( (v), (n) )

#endif

#ifdef HB_CRITICAL_NEED_INIT
   typedef struct
   {
      HB_BOOL fInit;
      union
      {
         int               dummy;
         HB_RAWCRITICAL_T  value;
      } critical;
   } HB_CRITICAL_T;
#  define HB_CRITICAL_NEW( name )   HB_CRITICAL_T name = { HB_FALSE, { 0 } }
#endif /* HB_CRITICAL_NEED_INIT */

#ifdef HB_COND_NEED_INIT
#  if defined( HB_COND_OS_SUPPORT )
      typedef struct
      {
         HB_BOOL        fInit;
         union
         {
            int            dummy;
            HB_RAWCOND_T   value;
         } cond;
      } HB_COND_T;
#     define HB_COND_NEW( name )       HB_COND_T name = { HB_FALSE, { 0 } }
#  else
      typedef struct
      {
         HB_BOOL           fInit;
         int               waiters;
         union
         {
            int               dummy;
            HB_RAWCOND_T      value;
         } cond;
         union
         {
            int               dummy;
            HB_RAWCRITICAL_T  value;
         } critical;
      } HB_COND_T;
#     define HB_COND_NEW( name )       HB_COND_T name = { HB_FALSE, 0, { 0 }, { 0 } }
#  endif
#endif /* HB_COND_NEED_INIT */

#ifndef HB_THREAD_EQUAL
#  define HB_THREAD_EQUAL( x, y )   ( (x) == (y) )
#endif

#ifndef HB_THREAD_SHEDULER
#  define HB_THREAD_SHEDULER()
#endif

#ifndef HB_THREAD_INFINITE_WAIT
#  define HB_THREAD_INFINITE_WAIT   ( ( HB_ULONG ) -1 )
#endif

typedef HB_THREAD_STARTFUNC( PHB_THREAD_STARTFUNC );

extern HB_EXPORT void hb_threadReleaseCPU( void );

/* atomic oprtations */
extern HB_EXPORT void        hb_atomic_set( volatile HB_COUNTER * pCounter, HB_COUNTER value );
extern HB_EXPORT HB_COUNTER  hb_atomic_get( volatile HB_COUNTER * pCounter );
extern HB_EXPORT void        hb_atomic_inc( volatile HB_COUNTER * pCounter );
extern HB_EXPORT HB_BOOL     hb_atomic_dec( volatile HB_COUNTER * pCounter ); /* returns HB_TRUE when counter reaches 0 after decrementation */

/* Critical sections or fast non recursive MUTEXes */
extern HB_EXPORT void     hb_threadEnterCriticalSection( HB_CRITICAL_T * critical );
extern HB_EXPORT void     hb_threadLeaveCriticalSection( HB_CRITICAL_T * critical );

/* conditional variables */
extern HB_EXPORT HB_BOOL  hb_threadCondSignal( HB_COND_T * cond );
extern HB_EXPORT HB_BOOL  hb_threadCondBroadcast( HB_COND_T * cond );
extern HB_EXPORT HB_BOOL  hb_threadCondWait( HB_COND_T * cond, HB_CRITICAL_T * mutex );
extern HB_EXPORT HB_BOOL  hb_threadCondTimedWait( HB_COND_T * cond, HB_CRITICAL_T * mutex, HB_ULONG ulMilliSec );

extern HB_EXPORT HB_THREAD_HANDLE hb_threadCreate( HB_THREAD_ID * th_id, PHB_THREAD_STARTFUNC start_func, void * Cargo );
extern HB_EXPORT HB_BOOL  hb_threadJoin( HB_THREAD_HANDLE th_h );
extern HB_EXPORT HB_BOOL  hb_threadDetach( HB_THREAD_HANDLE th_h );
extern HB_EXPORT HB_THREAD_NO hb_threadNO( void );

/* used by .prg code */
extern HB_EXPORT PHB_ITEM hb_threadMutexCreate( void );
extern HB_EXPORT HB_BOOL  hb_threadMutexLock( PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL  hb_threadMutexTimedLock( PHB_ITEM pItem, HB_ULONG ulMilliSec );
extern HB_EXPORT HB_BOOL  hb_threadMutexUnlock( PHB_ITEM pItem );
extern HB_EXPORT void     hb_threadMutexNotify( PHB_ITEM pItem, PHB_ITEM pNotifier, HB_BOOL fWaiting );
extern HB_EXPORT PHB_ITEM hb_threadMutexSubscribe( PHB_ITEM pItem, HB_BOOL fClear );
extern HB_EXPORT PHB_ITEM hb_threadMutexTimedSubscribe( PHB_ITEM pItem, HB_ULONG ulMilliSec, HB_BOOL fClear );

#if defined( _HB_API_INTERNAL_ )

typedef struct _HB_THREADSTATE
{
   const char *   pszCDP;
   const char *   pszLang;
   const char *   pszDefRDD;
   PHB_SET_STRUCT pSet;
   void *         pI18N;
   void *         hGT;
   void *         pStackId;
   void *         cargo;
   PHB_CARGO_FUNC pFunc;
   HB_BOOL        fActive;
   HB_BOOL        fFinished;
   PHB_ITEM       pParams;
   PHB_ITEM       pMemvars;
   PHB_ITEM       pResult;
   PHB_ITEM       pThItm;
   HB_THREAD_NO      th_no;
   HB_THREAD_ID      th_id;
   HB_THREAD_HANDLE  th_h;
   struct _HB_THREADSTATE * pPrev;
   struct _HB_THREADSTATE * pNext;
#if defined( HB_COND_HARBOUR_SUPPORT )
   HB_WAIT_LIST   pWaitList;
#endif
} HB_THREADSTATE, * PHB_THREADSTATE;

#if defined( HB_MT_VM )

extern void hb_threadInit( void );
extern void hb_threadExit( void );

extern PHB_THREADSTATE hb_threadStateNew( void );
extern PHB_THREADSTATE hb_threadStateClone( HB_ULONG ulAttr, PHB_ITEM pParams );
extern PHB_ITEM        hb_threadStart( HB_ULONG ulAttr, PHB_CARGO_FUNC pFunc, void * cargo );

extern void    hb_threadMutexUnlockAll( void );
extern void    hb_threadMutexUnsubscribeAll( void );
extern void    hb_threadMutexSyncSignal( PHB_ITEM pItemMtx );
extern HB_BOOL hb_threadMutexSyncWait( PHB_ITEM pItemMtx, HB_ULONG ulMilliSec, PHB_ITEM pItemSync );

#if defined( HB_NO_TLS ) || defined( HB_TASK_THREAD )
#  undef HB_USE_TLS
#elif ! defined( HB_USE_TLS )
   /* enable native compiler TLS support by default for this compilers
    * which are known that it will work correctly
    */
#  if ( defined( _MSC_VER ) && ( _MSC_VER > 1500 ) ) && ! defined( __POCC__ ) && ! defined( __XCC__ )
#     define HB_USE_TLS
#  elif defined( __GNUC__ ) && __GNUC__ >= 3 && \
        defined( __GLIBC__ ) && defined( __GLIBC_MINOR__ ) && \
        ( __GLIBC__ > 2 || ( __GLIBC__ == 2 && __GLIBC_MINOR__ >= 6 ) ) && \
        defined( HB_OS_LINUX ) && \
        ( defined( __i386__ ) || defined( __x86_64__ ) ) && \
        ! defined( __OPENCC__ )
#     define HB_USE_TLS
#  endif
#endif

#ifdef HB_USE_TLS
#  if ( defined( __GNUC__ ) && __GNUC__ >= 3 ) || defined( __BORLANDC__ )
#     define HB_TLS_ATTR      __thread
#  elif defined( _MSC_VER ) || defined( __WATCOMC__ ) || defined( __DMC__ )
#     define HB_TLS_ATTR      __declspec( thread )
#  else
#     undef HB_USE_TLS
#     error "TLS support undefined for this compiler" /* */
#  endif
#endif /* HB_USE_TLS */

#ifndef HB_USE_TLS
#  if defined( HB_TASK_THREAD )
#     define HB_TLS_KEY       void *
#     define hb_tls_init(k)   HB_SYMBOL_UNUSED( k )
#     define hb_tls_set(k,v)  hb_taskSetData( ( void * ) (v) )
#     define hb_tls_get(k)    hb_taskGetData()
#  elif defined( HB_PTHREAD_API )
#     define HB_TLS_KEY       pthread_key_t
#     define hb_tls_init(k)   pthread_key_create( &k, NULL )
#     define hb_tls_set(k,v)  pthread_setspecific( k, ( void * ) (v) )
#     define hb_tls_get(k)    pthread_getspecific( k )
#  elif defined( HB_OS_WIN )
#     define HB_TLS_KEY       DWORD
#     define hb_tls_init(k)   do { k = TlsAlloc(); } while( 0 )
#     define hb_tls_set(k,v)  TlsSetValue( k, ( void * ) (v) )
#     define hb_tls_get(k)    TlsGetValue( k )
#  elif defined( HB_OS_OS2 )
#     define HB_TLS_KEY       PULONG
#     define hb_tls_init(k)   DosAllocThreadLocalMemory( 1, &k )
#     define hb_tls_set(k,v)  do { *k = ( ULONG ) (v); } while( 0 )
#     define hb_tls_get(k)    ( *k )
#  endif
#endif /* ! HB_USE_TLS */

#endif /* HB_MT_VM */

#endif /* _HB_API_INTERNAL_ */

HB_EXTERN_END

#endif /* HB_THREAD_H_ */
