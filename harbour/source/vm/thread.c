/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    MT mode functions
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define HB_OS_WIN_USED

#define INCL_DOSSEMAPHORES
#define INCL_DOSPROCESS

#define _HB_THREAD_INTERNAL_

#include "hbvmopt.h"
#include "hbthread.h"
#include "hbatomic.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapicdp.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"
#include "hbmemvar.ch"
#include "hbthread.ch"

#if defined( HB_PTHREAD_API )
#  include <time.h>
#  include <sys/time.h>
#endif


static volatile BOOL s_fThreadInit = FALSE;
static PHB_ITEM s_pOnceMutex = NULL;

#if !defined( HB_MT_VM )
   /* nothing */
#else

   static int s_waiting_for_threads = 0;

#  if defined( HB_PTHREAD_API )
   static void hb_threadTimeInit( struct timespec * ts, ULONG ulMilliSec )
   {
#     if _POSIX_C_SOURCE >= 199309L
      clock_gettime( CLOCK_REALTIME, ts );
#     else
      struct timeval tv;
      gettimeofday( &tv, NULL );
      ts->tv_sec  = tv.tv_sec;
      ts->tv_nsec = tv.tv_usec * 1000l;
#     endif
      ts->tv_nsec += ( ulMilliSec % 1000 ) * 1000000l;
      ts->tv_sec  += ulMilliSec / 1000 + ts->tv_nsec / 1000000000l;
      ts->tv_nsec %= 1000000000l;
   }
#  endif

#  if defined( HB_CRITICAL_NEED_INIT )
   static HB_RAWCRITICAL_T s_init_mtx;
   static HB_RAWCRITICAL_T s_once_mtx;
   static HB_RAWCRITICAL_T s_thread_mtx;
   static HB_RAWCRITICAL_T s_mutexlst_mtx;
   static void hb_threadCriticalInit( HB_CRITICAL_T * critical )
   {
      if( !s_fThreadInit )
         hb_threadInit();

      HB_CRITICAL_LOCK( s_init_mtx );
      if( !critical->fInit )
      {
         HB_CRITICAL_INIT( critical->critical );
         critical->fInit = TRUE;
      }
      HB_CRITICAL_UNLOCK( s_init_mtx );
   }
#  else
#     if defined( HB_COND_NEED_INIT )
         static HB_CRITICAL_NEW( s_init_mtx );
#     endif
      static HB_CRITICAL_NEW( s_once_mtx );
      static HB_CRITICAL_NEW( s_thread_mtx );
      static HB_CRITICAL_NEW( s_mutexlst_mtx );
#  endif

#  if defined( HB_COND_NEED_INIT )
      static HB_RAWCOND_T s_thread_cond;
      static void hb_threadCondInit( HB_COND_T * cond )
      {
         if( !s_fThreadInit )
            hb_threadInit();

         HB_CRITICAL_LOCK( s_init_mtx );
         if( !cond->fInit )
         {
            HB_COND_INIT( cond->cond );
#     if !defined( HB_COND_OS_SUPPORT )
            HB_CRITICAL_INIT( cond->critical );
            cond->waiters = 0;
#     endif
            cond->fInit = TRUE;
         }
         HB_CRITICAL_UNLOCK( s_init_mtx );
      }
#  else
      static HB_COND_NEW( s_thread_cond );
#  endif

#endif /* HB_MT_VM */

void hb_threadInit( void )
{
   if( !s_fThreadInit )
   {
#if !defined( HB_MT_VM )
      /* nothing to do */
#else
#  if defined( HB_CRITICAL_NEED_INIT )
      HB_CRITICAL_INIT( s_init_mtx );
      HB_CRITICAL_INIT( s_once_mtx );
      HB_CRITICAL_INIT( s_thread_mtx );
      HB_CRITICAL_INIT( s_mutexlst_mtx );
#  endif
#  if defined( HB_COND_NEED_INIT )
      HB_COND_INIT( s_thread_cond );
#  endif
#endif
      s_fThreadInit = TRUE;
   }
}

void hb_threadExit( void )
{
   if( s_pOnceMutex )
   {
      hb_itemRelease( s_pOnceMutex );
      s_pOnceMutex = NULL;
   }
}

#if defined( HB_MT_VM ) && defined( HB_COND_HARBOUR_SUPPORT )
static PHB_WAIT_LIST _hb_thread_wait_list( void )
{
   PHB_THREADSTATE pThread = ( PHB_THREADSTATE ) hb_vmThreadState();

   if( pThread )
      return &pThread->pWaitList;
   else
      return NULL;
}

static void _hb_thread_wait_add( HB_COND_T * cond, PHB_WAIT_LIST pWaiting )
{
#if defined( HB_OS_OS2 )
   ULONG ulPostCount = 0;
   DosResetEventSem( pWaiting->cond, &ulPostCount );
#elif defined( HB_OS_WIN )
   /* It's not necessary becayse we have workaround for possible race
    * condition inside _hb_thread_cond_wait() function
    */
   /* WaitForSingleObject( pWaiting->cond, 0 ); */
#endif
   pWaiting->signaled = FALSE;

   if( cond->waiters == NULL )
   {
      cond->waiters = pWaiting->next = pWaiting->prev = pWaiting;
   }
   else
   {
      pWaiting->next = cond->waiters;
      pWaiting->prev = cond->waiters->prev;
      cond->waiters->prev = pWaiting->prev->next = pWaiting;
   }
}

static void _hb_thread_wait_del( HB_COND_T * cond, PHB_WAIT_LIST pWaiting )
{
   pWaiting->next->prev = pWaiting->prev;
   pWaiting->prev->next = pWaiting->next;
   if( pWaiting == cond->waiters )
   {
      cond->waiters = pWaiting->next;
      if( pWaiting == cond->waiters )
         cond->waiters = NULL;
   }
}

static BOOL _hb_thread_cond_signal( HB_COND_T * cond )
{
   if( cond->waiters )
   {
      PHB_WAIT_LIST pWaiting = cond->waiters;
      do
      {
         if( !pWaiting->signaled )
         {
#if defined( HB_OS_OS2 )
            DosPostEventSem( pWaiting->cond );
#elif defined( HB_OS_WIN )
            ReleaseSemaphore( pWaiting->cond, 1, NULL );
#endif
            pWaiting->signaled = TRUE;
            /* signal only single thread */
            break;
         }
         pWaiting = pWaiting->next;
      }
      while( pWaiting != cond->waiters );
   }

   return TRUE;
}

static BOOL _hb_thread_cond_broadcast( HB_COND_T * cond )
{
   if( cond->waiters )
   {
      PHB_WAIT_LIST pWaiting = cond->waiters;
      do
      {
         if( !pWaiting->signaled )
         {
#if defined( HB_OS_OS2 )
            DosPostEventSem( pWaiting->cond );
#elif defined( HB_OS_WIN )
            ReleaseSemaphore( pWaiting->cond, 1, NULL );
#endif
            pWaiting->signaled = TRUE;
         }
         pWaiting = pWaiting->next;
      }
      while( pWaiting != cond->waiters );
   }

   return TRUE;
}

static BOOL _hb_thread_cond_wait( HB_COND_T * cond, HB_RAWCRITICAL_T * critical, ULONG ulMillisec )
{
   PHB_WAIT_LIST pWaiting = _hb_thread_wait_list();
   BOOL fResult = FALSE;

   if( pWaiting )
   {
      _hb_thread_wait_add( cond, pWaiting );

#if defined( HB_OS_OS2 )
      DosReleaseMutexSem( *critical );
      fResult = DosWaitEventSem( pWaiting->cond, ulMillisec ) == NO_ERROR;
      DosRequestMutexSem( *critical, SEM_INDEFINITE_WAIT );
#elif defined( HB_OS_WIN )
      LeaveCriticalSection( critical );
      fResult = WaitForSingleObject( pWaiting->cond, ulMillisec ) == WAIT_OBJECT_0;
      EnterCriticalSection( critical );
      /* workaround for race condition */
      if( !fResult && pWaiting->signaled )
         fResult = WaitForSingleObject( pWaiting->cond, 0 ) == WAIT_OBJECT_0;
#endif

      _hb_thread_wait_del( cond, pWaiting );
   }

   return fResult;
}
#endif

#if defined( HB_OS_OS2 ) && !defined( __GNUC__ )
ULONG _hb_gettid( void )
{
   ULONG tid = 0;
   PTIB  ptib = NULL;

   if( DosGetInfoBlocks( &ptib, NULL ) == NO_ERROR )
      tid = ptib->tib_ptib2->tib2_ultid;

   return tid;
}
#endif

/*
 * atomic increment/decrement operations
 */
#if !defined( HB_MT_VM )
void hb_atomic_set( volatile HB_COUNTER * pCounter, HB_COUNTER value )
{
   *pCounter = value;
}

HB_COUNTER hb_atomic_get( volatile HB_COUNTER * pCounter )
{
   return *pCounter;
}

void hb_atomic_inc( volatile HB_COUNTER * pCounter )
{
   ++( *pCounter );
}

BOOL hb_atomic_dec( volatile HB_COUNTER * pCounter )
{
   return --( *pCounter ) == 0;
}
#elif defined( HB_ATOM_INC ) && defined( HB_ATOM_DEC ) && \
      defined( HB_ATOM_GET ) && defined( HB_ATOM_SET )
void hb_atomic_set( volatile HB_COUNTER * pCounter, HB_COUNTER value )
{
   HB_ATOM_SET( pCounter, value );
}

HB_COUNTER hb_atomic_get( volatile HB_COUNTER * pCounter )
{
   return HB_ATOM_GET( pCounter );
}

void hb_atomic_inc( volatile HB_COUNTER * pCounter )
{
   HB_ATOM_INC( pCounter );
}

BOOL hb_atomic_dec( volatile HB_COUNTER * pCounter )
{
   return HB_ATOM_DEC( pCounter ) == 0;
}
#else
static HB_CRITICAL_NEW( s_atomicMtx );
void hb_atomic_set( volatile HB_COUNTER * pCounter, HB_COUNTER value )
{
   /* NOTE: on some platforms it may be necessary to protect this
    * by cirtical section, f.e. when HB_COUNTER cannot be accessed
    * using single memory access by CPU.
    */
   *pCounter = value;
}

HB_COUNTER hb_atomic_get( volatile HB_COUNTER * pCounter )
{
   /* NOTE: on some platforms it may be necessary to protect this
    * by cirtical section, f.e. when HB_COUNTER cannot be accessed
    * using single memory access by CPU.
    */
   return *pCounter;
}

void hb_atomic_inc( volatile HB_COUNTER * pCounter )
{
   hb_threadEnterCriticalSection( &s_atomicMtx );
   ++( *pCounter );
   hb_threadLeaveCriticalSection( &s_atomicMtx );
}

BOOL hb_atomic_dec( volatile HB_COUNTER * pCounter )
{
   BOOL fResult;
   hb_threadEnterCriticalSection( &s_atomicMtx );
   fResult = --( *pCounter ) == 0;
   hb_threadLeaveCriticalSection( &s_atomicMtx );
   return fResult;
}
#endif

void hb_threadEnterCriticalSection( HB_CRITICAL_T * critical )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( critical );
#elif defined( HB_CRITICAL_NEED_INIT )
   if( !critical->fInit )
      hb_threadCriticalInit( critical );
   HB_CRITICAL_LOCK( critical->critical );
#else
   HB_CRITICAL_LOCK( *critical );
#endif
}

void hb_threadLeaveCriticalSection( HB_CRITICAL_T * critical )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( critical );
#elif defined( HB_CRITICAL_NEED_INIT )
   HB_CRITICAL_UNLOCK( critical->critical );
#else
   HB_CRITICAL_UNLOCK( *critical );
#endif
}

BOOL hb_threadCondSignal( HB_COND_T * cond )
{
#if !defined( HB_MT_VM )

   HB_SYMBOL_UNUSED( cond );
   return FALSE;

#elif defined( HB_PTHREAD_API )

#  if defined( HB_COND_NEED_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_signal( HB_COND_GET( cond ) ) == 0;

#elif defined( HB_COND_HARBOUR_SUPPORT )

   return _hb_thread_cond_signal( cond );

#else

   if( !cond->fInit )
      hb_threadCondInit( cond );

   HB_CRITICAL_LOCK( cond->critical );
   if( cond->waiters )
   {
      HB_COND_SIGNAL( cond->cond );
      cond->waiters--;
   }
   HB_CRITICAL_UNLOCK( cond->critical );

   return TRUE;

#endif
}

BOOL hb_threadCondBroadcast( HB_COND_T * cond )
{
#if !defined( HB_MT_VM )

   HB_SYMBOL_UNUSED( cond );
   return FALSE;

#elif defined( HB_PTHREAD_API )

#  if defined( HB_COND_NEED_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_broadcast( HB_COND_GET( cond ) ) == 0;

#elif defined( HB_COND_HARBOUR_SUPPORT )

   return _hb_thread_cond_broadcast( cond );

#else

   if( !cond->fInit )
      hb_threadCondInit( cond );

   HB_CRITICAL_LOCK( cond->critical );
   if( cond->waiters )
   {
      HB_COND_SIGNALN( cond->cond, cond->waiters );
      cond->waiters = 0;
   }
   HB_CRITICAL_UNLOCK( cond->critical );

   return TRUE;

#endif
}

BOOL hb_threadCondWait( HB_COND_T * cond, HB_CRITICAL_T * mutex )
{
#if !defined( HB_MT_VM )

   HB_SYMBOL_UNUSED( cond );
   HB_SYMBOL_UNUSED( mutex );
   return FALSE;

#elif defined( HB_PTHREAD_API )

#  if defined( HB_COND_NEED_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_wait( HB_COND_GET( cond ), HB_CRITICAL_GET( mutex ) ) == 0;

#elif defined( HB_COND_HARBOUR_SUPPORT )

   return _hb_thread_cond_wait( cond, &mutex->critical, HB_THREAD_INFINITE_WAIT );

#else

   BOOL fResult;

   if( !cond->fInit )
      hb_threadCondInit( cond );

   /* mutex should be already locked so it's not necessary
    * to make initialization test here
    */

   HB_CRITICAL_LOCK( cond->critical );
   cond->waiters++;
   HB_CRITICAL_UNLOCK( cond->critical );

   HB_CRITICAL_UNLOCK( mutex->critical );
   fResult = HB_COND_WAIT( cond->cond );
   HB_CRITICAL_LOCK( mutex->critical );
   /* There is race condition here and user code should always check if
    * the wait condition is valid after leaving hb_threadCondWait()
    * even if it returns TRUE
    */
   if( !fResult )
   {
      HB_CRITICAL_LOCK( cond->critical );
      cond->waiters--;
      HB_CRITICAL_UNLOCK( cond->critical );
   }

   return fResult;

#endif
}

BOOL hb_threadCondTimedWait( HB_COND_T * cond, HB_CRITICAL_T * mutex, ULONG ulMilliSec )
{
#if !defined( HB_MT_VM )

   HB_SYMBOL_UNUSED( cond );
   HB_SYMBOL_UNUSED( mutex );
   HB_SYMBOL_UNUSED( ulMilliSec );
   return FALSE;

#elif defined( HB_PTHREAD_API )
   struct timespec ts;

#  if defined( HB_COND_NEED_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   hb_threadTimeInit( &ts, ulMilliSec );
   return pthread_cond_timedwait( HB_COND_GET( cond ), HB_CRITICAL_GET( mutex ), &ts ) == 0;

#elif defined( HB_COND_HARBOUR_SUPPORT )

   return _hb_thread_cond_wait( cond, &mutex->critical, ulMilliSec );

#else

   BOOL fResult;

   if( !cond->fInit )
      hb_threadCondInit( cond );

   /* mutex should be already locked so it's not necessary
    * to make initialization test here
    */

   HB_CRITICAL_LOCK( cond->critical );
   cond->waiters++;
   HB_CRITICAL_UNLOCK( cond->critical );

   HB_CRITICAL_UNLOCK( mutex->critical );
   fResult = HB_COND_TIMEDWAIT( cond->cond, ulMilliSec );
   HB_CRITICAL_LOCK( mutex->critical );
   /* There is race condition here and user code should always check if
    * the wait condition is valid after leaving hb_threadCondTimedWait()
    * even if it returns TRUE
    */
   if( !fResult )
   {
      HB_CRITICAL_LOCK( cond->critical );
      cond->waiters--;
      HB_CRITICAL_UNLOCK( cond->critical );
   }

   return fResult;

#endif
}

HB_THREAD_HANDLE hb_threadCreate( HB_THREAD_ID * th_id, PHB_THREAD_STARTFUNC start_func, void * Cargo )
{
   HB_THREAD_HANDLE th_h;

#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( start_func );
   HB_SYMBOL_UNUSED( Cargo );
   *th_id = ( HB_THREAD_ID ) 0;
   th_h = ( HB_THREAD_HANDLE ) 0;
#elif defined( HB_PTHREAD_API )
   if( pthread_create( th_id, NULL, start_func, Cargo ) != 0 )
      *th_id = ( HB_THREAD_ID ) 0;
   th_h = *th_id;
#elif defined( HB_OS_WIN )
   th_h = ( HANDLE ) _beginthreadex( NULL, 0, start_func, Cargo, 0, th_id );
   if( !th_h )
      *th_id = ( HB_THREAD_ID ) 0;
#elif defined( HB_OS_OS2 )
   *th_id = _beginthread( start_func, NULL, 128 * 1024, Cargo );
   th_h = *th_id;
#else
   { int TODO_MT; }
   *th_id = ( HB_THREAD_ID ) 0;
   th_h = ( HB_THREAD_HANDLE ) 0;
#endif

   return th_h;
}

BOOL hb_threadJoin( HB_THREAD_HANDLE th_h )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( th_h );
   return FALSE;
#elif defined( HB_PTHREAD_API )
   return pthread_join( th_h, NULL ) == 0;
#elif defined( HB_OS_WIN )
   if( WaitForSingleObject( th_h, INFINITE ) != WAIT_FAILED )
   {
      CloseHandle( th_h );
      return TRUE;
   }
   return FALSE;
#elif defined( HB_OS_OS2 )
   APIRET rc = DosWaitThread( &th_h, DCWW_WAIT );
   /* TOFIX: ERROR_INVALID_THREADID is a hack for failing DosWaitThread()
    *        when thread terminates before DosWaitThread() call.
    *        OS2 users please check and fix this code if possible.
    */
   return rc == NO_ERROR || rc == ERROR_INVALID_THREADID;
#else
   { int TODO_MT; }
   return FALSE;
#endif
}

BOOL hb_threadDetach( HB_THREAD_HANDLE th_h )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( th_h );
   return FALSE;
#elif defined( HB_PTHREAD_API )
   return pthread_detach( th_h ) == 0;
#elif defined( HB_OS_WIN )
   return CloseHandle( th_h ) != 0;
#elif defined( HB_OS_OS2 )
   APIRET rc = DosWaitThread( &th_h, DCWW_NOWAIT );
   return rc == NO_ERROR || rc == ERROR_INVALID_THREADID;
#else
   { int TODO_MT; }
   return FALSE;
#endif
}

/*
 * .PRG level functions
 */

/* I. THREADS */

static HB_GARBAGE_FUNC( hb_threadDestructor )
{
   PHB_THREADSTATE pThread = ( PHB_THREADSTATE ) Cargo;

   if( pThread->pParams )
   {
      hb_itemRelease( pThread->pParams );
      pThread->pParams = NULL;
   }
   if( pThread->pMemvars )
   {
      hb_itemRelease( pThread->pMemvars );
      pThread->pMemvars = NULL;
   }
   if( pThread->pResult )
   {
      hb_itemRelease( pThread->pResult );
      pThread->pResult = NULL;
   }
   if( pThread->pI18N )
   {
      hb_i18n_release( pThread->pI18N );
      pThread->pI18N = NULL;
   }
   if( pThread->pSet )
   {
      hb_setRelease( pThread->pSet );
      hb_xfree( pThread->pSet );
      pThread->pSet = NULL;
   }
   if( pThread->th_h != 0 )
   {
      hb_threadDetach( pThread->th_h );
      pThread->th_h = 0;
   }
   if( pThread->hGT )
   {
      hb_gtRelease( pThread->hGT );
      pThread->hGT = NULL;
   }
#if defined( HB_COND_HARBOUR_SUPPORT )
   if( pThread->pWaitList.cond )
   {
#  if defined( HB_OS_OS2 )
      DosCloseEventSem( pThread->pWaitList.cond );
      pThread->pWaitList.cond = ( HEV ) 0;
#  elif defined( HB_OS_WIM )
      CloseHandle( pThread->pWaitList.cond );
      pThread->pWaitList.cond = ( HANDLE ) 0;
#  endif
   }
#endif
}

static HB_THREAD_STARTFUNC( hb_threadStartVM )
{
#if defined( HB_MT_VM )
   PHB_ITEM pThItm = ( PHB_ITEM ) Cargo;
   ULONG ulPCount, ulParam;
   PHB_THREADSTATE pThread;

   pThread = ( PHB_THREADSTATE ) hb_itemGetPtrGC( pThItm, hb_threadDestructor );

   hb_vmThreadInit( ( void * ) pThread );

   ulPCount = hb_arrayLen( pThread->pParams );
   if( ulPCount > 0 )
   {
      PHB_ITEM pStart = hb_arrayGetItemPtr( pThread->pParams, 1 );

      if( HB_IS_BLOCK( pStart ) )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pStart );
      }
      else if( HB_IS_SYMBOL( pStart ) )
      {
         hb_vmPush( pStart );
         hb_vmPushNil();
      }
      else if( HB_IS_STRING( pStart ) )
      {
         hb_vmPushDynSym( hb_dynsymGet( hb_itemGetCPtr( pStart ) ) );
         hb_vmPushNil();
      }
      else
         ulPCount = 0;
   }

   if( ulPCount > 0 )
   {
      for( ulParam = 2; ulParam <= ulPCount; ++ulParam )
         hb_vmPush( hb_arrayGetItemPtr( pThread->pParams, ulParam ) );

      hb_itemRelease( pThread->pParams );
      pThread->pParams = NULL;

      hb_vmDo( ( USHORT ) ( ulPCount - 1 ) );
   }
   else
   {
      hb_itemRelease( pThread->pParams );
      pThread->pParams = NULL;
      if( pThread->pMemvars )
      {
         hb_itemRelease( pThread->pMemvars );
         pThread->pMemvars = NULL;
      }

      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 0 );
   }

   /* hb_vmThreadQuit() unlocks and release HVM stack and may release
    * also pThItm item so we should not access any HVM items or
    * pThread structure after this function.
    */
   hb_vmThreadQuit();

   HB_CRITICAL_LOCK( s_thread_mtx );
   if( s_waiting_for_threads )
   {
      HB_COND_SIGNALN( s_thread_cond, s_waiting_for_threads );
      s_waiting_for_threads = 0;
   }
   HB_CRITICAL_UNLOCK( s_thread_mtx );

   HB_THREAD_END
#else
   hb_itemRelease( ( PHB_ITEM ) Cargo );
   HB_THREAD_RAWEND
#endif
}

PHB_THREADSTATE hb_threadStateNew( void )
{
   PHB_ITEM pThItm;
   PHB_THREADSTATE pThread;

   pThItm = hb_itemNew( NULL );
   pThread = ( PHB_THREADSTATE )
                  hb_gcAlloc( sizeof( HB_THREADSTATE ), hb_threadDestructor );
   memset( pThread, 0, sizeof( HB_THREADSTATE ) );
   hb_itemPutPtrGC( pThItm, pThread );

   pThread->pszCDP  = HB_MACRO2STRING( HB_CODEPAGE_DEFAULT );
   pThread->pszLang = HB_MACRO2STRING( HB_LANG_DEFAULT );
   pThread->pThItm  = pThItm;
   pThread->hGT     = hb_gtAlloc( NULL );

#if defined( HB_COND_HARBOUR_SUPPORT )
#  if defined( HB_OS_OS2 )
      DosCreateEventSem( NULL, &pThread->pWaitList.cond, 0L, FALSE );
#  elif defined( HB_OS_WIM )
      pThread->pWaitList.cond = CreateSemaphore( NULL, 0, 1, NULL );
#  endif
#endif

   return pThread;
}

static PHB_THREADSTATE hb_thParam( int iParam, int iPos )
{
   PHB_THREADSTATE pThread = ( PHB_THREADSTATE )
                             hb_parptrGC( hb_threadDestructor, iParam, iPos );
   if( pThread )
      return pThread;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

HB_FUNC( HB_THREADSTART )
{
   ULONG ulAttr = 0, ulStart = 1;
   const char * szFuncName = NULL;
   PHB_SYMB pSymbol = NULL;
   PHB_ITEM pStart;

   pStart = hb_param( ulStart, HB_IT_ANY );
   while( pStart && HB_IS_NUMERIC( pStart ) )
   {
      ulAttr |= ( ULONG ) hb_itemGetNL( pStart );
      pStart = hb_param( ++ulStart, HB_IT_ANY );
   }

   if( pStart )
   {
      if( HB_IS_STRING( pStart ) )
      {
         PHB_DYNS pDynSym;
         szFuncName = hb_itemGetCPtr( pStart );
         pDynSym = hb_dynsymFindName( szFuncName );
         if( pDynSym )
            pSymbol = pDynSym->pSymbol;
         if( !pSymbol || !pSymbol->value.pFunPtr )
            pStart = NULL;
      }
      else if( HB_IS_SYMBOL( pStart ) )
      {
         pSymbol = hb_itemGetSymbol( pStart );
         if( !pSymbol->value.pFunPtr )
         {
            szFuncName = pSymbol->szName;
            pStart = NULL;
         }
      }
      else if( !HB_IS_BLOCK( pStart ) )
         pStart = NULL;
   }

   if( pStart )
   {
      PHB_ITEM pReturn;
      PHB_THREADSTATE pThread;
      ULONG ulPCount, ulParam;

      pThread = hb_threadStateNew();
      pReturn = pThread->pThItm;

      pThread->pszCDP    = hb_cdpID();
      pThread->pszLang   = hb_langID();
      pThread->pI18N     = hb_i18n_alloc( hb_vmI18N() );
      pThread->pszDefRDD = hb_stackRDD()->szDefaultRDD;
      pThread->pSet      = hb_setClone( hb_stackSetStruct() );
      pThread->pParams   = hb_arrayBaseParams();

      ulPCount = hb_arrayLen( pThread->pParams );
      /* remove thread attributes */
      if( ulStart > 1 )
      {
         for( ulParam = 1; ulParam < ulStart; ++ulParam )
            hb_arrayDel( pThread->pParams, 1 );
         ulPCount -= ulStart - 1;
         hb_arraySize( pThread->pParams, ulPCount );
      }
      if( HB_IS_STRING( pStart ) && pSymbol )
         hb_itemPutSymbol( hb_arrayGetItemPtr( pThread->pParams, 1 ), pSymbol );
      /* detach LOCAL variables passed by reference */
      for( ulParam = 1; ulParam <= ulPCount; ++ulParam )
      {
         PHB_ITEM pParam = hb_arrayGetItemPtr( pThread->pParams, ulParam );
         if( HB_IS_BYREF( pParam ) )
         {
            if( ulParam == 1 )
               hb_itemCopy( pParam, hb_itemUnRef( pParam ) );
            else
               hb_memvarDetachLocal( pParam );
         }
      }

      if( ( ulAttr & HB_THREAD_INHERIT_MEMVARS ) != 0 )
      {
         int iScope = 0;
         if( ( ulAttr & HB_THREAD_INHERIT_PUBLIC ) != 0 )
            iScope |= HB_MV_PUBLIC;
         if( ( ulAttr & HB_THREAD_INHERIT_PRIVATE ) != 0 )
            iScope |= HB_MV_PRIVATE;
         pThread->pMemvars = hb_memvarSaveInArray( iScope,
                                    ( ulAttr & HB_THREAD_MEMVARS_COPY ) != 0 );
      }

      /* make copy of thread pointer item before we pass it to new thread
       * to avoid race condition
       */
      hb_itemReturn( pReturn );

#if defined( HB_MT_VM )
      if( hb_vmThreadRegister( ( void * ) pThread ) )
#endif
         pThread->th_h = hb_threadCreate( &pThread->th_id, hb_threadStartVM, ( void * ) pReturn );

      if( !pThread->th_h )
      {
#if defined( HB_MT_VM )
         hb_vmThreadRelease( pThread );
#else
         hb_itemRelease( pReturn );
#endif
         hb_ret();
      }
   }
   else
   {
      if( szFuncName )
         hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, szFuncName, 0 );
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( HB_THREADSELF )
{
#if defined( HB_MT_VM )
   PHB_THREADSTATE pThread = ( PHB_THREADSTATE ) hb_vmThreadState();
   /* It's possible that pThread will be NULL and this function will
    * return NIL. It may happen only in one case when this function is
    * executed by one of destructors of items stored in thread pointer
    * item (in practice it can be only thread return value) and parent
    * thread destroyed this thread pointer item. [druzus]
    */
   if( pThread )
      hb_itemReturn( pThread->pThItm );
#endif
}

HB_FUNC( HB_THREADID )
{
#if defined( HB_MT_VM )
   PHB_THREADSTATE pThread;

   if( hb_pcount() > 0 )
   {
      pThread = hb_thParam( 1, 0 );
      if( pThread )
         hb_retnint( pThread->th_no );
   }
   else
   {
      pThread = ( PHB_THREADSTATE ) hb_vmThreadState();
      if( pThread )
         hb_retnint( pThread->th_no );
      else
         hb_retnint( 0 );
   }
#else
   hb_retnint( 0 );
#endif
}

#if defined( HB_MT_VM )
static int hb_threadWait( PHB_THREADSTATE * pThreads, int iThreads,
                          BOOL fAll, ULONG ulMilliSec )
{
   int i, iFinished, iResult = 0;
   BOOL fExit = ulMilliSec == 0;

#if defined( HB_PTHREAD_API )
   struct timespec ts;

   if( ulMilliSec != HB_THREAD_INFINITE_WAIT )
      hb_threadTimeInit( &ts, ulMilliSec );
   else
      ts.tv_sec = ts.tv_nsec = 0;
#else
   HB_ULONG timer;

   if( ulMilliSec != HB_THREAD_INFINITE_WAIT )
      timer = hb_dateMilliSeconds() + ulMilliSec;
   else
      timer = 0;
#endif

   HB_CRITICAL_LOCK( s_thread_mtx );
   for( ;; )
   {
      for( i = iFinished = 0; i < iThreads; ++i )
      {
         if( pThreads[ i ]->fFinished )
         {
            iFinished++;
            if( !fAll )
            {
               iResult = i + 1;
               break;
            }
         }
      }
      if( iFinished >= ( fAll ? iThreads : 1 ) )
         break;

      if( fExit )
         break;

      s_waiting_for_threads++;
#if defined( HB_PTHREAD_API )
      hb_vmUnlock();
      if( ulMilliSec != HB_THREAD_INFINITE_WAIT )
         fExit = pthread_cond_timedwait( &s_thread_cond, &s_thread_mtx, &ts ) != 0;
      else
         fExit = pthread_cond_wait( &s_thread_cond, &s_thread_mtx ) != 0;
      hb_vmLock();
#else
#  if defined( HB_COND_HARBOUR_SUPPORT )
      hb_vmUnlock();
      fExit = !_hb_thread_cond_wait( &s_thread_cond, &s_thread_mtx, ulMilliSec );
      hb_vmLock();
#  else

      HB_CRITICAL_UNLOCK( s_thread_mtx );
      hb_vmUnlock();
      fExit = !HB_COND_TIMEDWAIT( s_thread_cond, ulMilliSec );
      hb_vmLock();
      HB_CRITICAL_LOCK( s_thread_mtx );
      if( fExit )
         s_waiting_for_threads--;
#  endif
      if( !fExit && timer )
      {
         HB_ULONG curr = hb_dateMilliSeconds();
         if( timer <= curr )
            fExit = TRUE;
         else
            ulMilliSec = ( ULONG ) ( timer - curr );
      }
#endif

      if( !fExit && hb_vmRequestQuery() != 0 )
         break;
   }
   HB_CRITICAL_UNLOCK( s_thread_mtx );

   return fAll ? iFinished : iResult;
}
#endif

HB_FUNC( HB_THREADJOIN )
{
   PHB_THREADSTATE pThread = hb_thParam( 1, 0 );

   if( pThread )
   {
      BOOL fResult = FALSE;

      if( pThread->th_h )
      {
         hb_vmUnlock();
         fResult = hb_threadJoin( pThread->th_h );
         if( fResult )
            pThread->th_h = 0;
         hb_vmLock();
      }
      if( fResult )
      {
         if( pThread->pResult )
         {
            hb_itemParamStoreForward( 2, pThread->pResult );
            hb_itemRelease( pThread->pResult );
            pThread->pResult = NULL;
         }
      }
      hb_retl( fResult );
   }
}

HB_FUNC( HB_THREADDETACH )
{
   PHB_THREADSTATE pThread = hb_thParam( 1, 0 );

   if( pThread )
   {
      BOOL fResult = FALSE;

      if( pThread->th_h && hb_threadDetach( pThread->th_h ) )
      {
         pThread->th_h = 0;
         fResult = TRUE;
      }
      hb_retl( fResult );
   }
}

HB_FUNC( HB_THREADQUITREQUEST )
{
   PHB_THREADSTATE pThread = hb_thParam( 1, 0 );

   if( pThread )
   {
      BOOL fResult = FALSE;

#if defined( HB_MT_VM )
      if( !pThread->fActive )
      {
         hb_vmThreadQuitRequest( ( void * ) pThread );
         fResult = TRUE;
      }
#endif
      hb_retl( fResult );
   }
}

HB_FUNC( HB_THREADWAIT )
{
#if defined( HB_MT_VM )
#  define HB_THREAD_WAIT_ALLOC  16
   BOOL fAll = FALSE;
   ULONG ulMilliSec = HB_THREAD_INFINITE_WAIT;
   PHB_THREADSTATE * pThreads, pAlloc[ HB_THREAD_WAIT_ALLOC ];
   int iThreads = -1;

   pThreads = pAlloc;
   if( ISARRAY( 1 ) )
   {
      PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
      int iLen = ( int ) hb_arrayLen( pArray ), i;

      for( i = iThreads = 0; i < iLen; ++i )
      {
         PHB_THREADSTATE pThread = hb_thParam( 1, i + 1 );
         if( !pThread )
         {
            iThreads = -1;
            break;
         }
         if( pThreads == pAlloc && iThreads >= HB_THREAD_WAIT_ALLOC )
         {
            pThreads = ( PHB_THREADSTATE * )
                       hb_xgrab( sizeof( PHB_THREADSTATE ) * iLen );
            memcpy( pThreads, pAlloc, sizeof( pAlloc ) );
         }
         pThreads[ iThreads++ ] = pThread;
      }
   }
   else
   {
      pThreads[ 0 ] = hb_thParam( 1, 0 );
      if( pThreads[ 0 ] )
         iThreads = 1;
   }

   if( iThreads > 0 )
   {
      if( ISNUM( 2 ) )
      {
         double dTimeOut = hb_parnd( 2 );
         ulMilliSec = dTimeOut > 0 ? ( ULONG ) ( dTimeOut * 1000 ) : 0;
      }

      if( ISLOG( 3 ) )
         fAll = hb_parl( 3 );

      hb_retni( hb_threadWait( pThreads, iThreads, fAll, ulMilliSec ) );
   }
   else if( iThreads == 0 )
      hb_retni( 0 );

   if( pThreads != pAlloc )
      hb_xfree( pThreads );
#endif
}


HB_FUNC( HB_THREADWAITFORALL )
{
#if defined( HB_MT_VM )
   hb_vmWaitForThreads();
#endif
}

HB_FUNC( HB_THREADTERMINATEALL )
{
#if defined( HB_MT_VM )
   hb_vmTerminateThreads();
#endif
}

/* hb_threadOnce( @<onceControl> [, <bAction> ] ) -> <lFirstCall>
 * Execute <bAction> only once. <onceControl> is variable which holds
 * the execution status and have to be initialized to NIL. In most of
 * cases it will be simple staticvariable in user code.
 * When <bAction> is executed by a thread all other threads which call
 * hb_threadOnce() are stopped even if they use different <onceControl>.
 * Because hb_threadOnce() uses single recursive mutex then deadlock caused
 * by cross call to hb_threadOnce() from different threads is not possible.
 * If thread calls hb_threadOnce() with the same <onceControl> variable
 * recursively from <bAction> then hb_threadOnce() returns immediately
 * returning FALSE without executing <bAction>.
 * This function returns logical value indicating if it was 1-st call to
 * hb_threadOnce() for given <onceControl> variable
 */
HB_FUNC( HB_THREADONCE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
   if( pItem && ISBYREF( 1 ) && ( HB_IS_NIL( pItem ) || HB_IS_LOGICAL( pItem ) ) )
   {
      BOOL fFirstCall = FALSE;
      if( HB_IS_NIL( pItem ) || !hb_itemGetL( pItem ) )
      {
         PHB_ITEM pAction = hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL );

#if defined( HB_MT_VM )
         if( !s_pOnceMutex )
         {
            if( !s_fThreadInit )
               hb_threadInit();
            HB_CRITICAL_LOCK( s_once_mtx );
            if( !s_pOnceMutex )
               s_pOnceMutex = hb_threadMutexCreate( FALSE );
            HB_CRITICAL_UNLOCK( s_once_mtx );
         }
         if( hb_threadMutexLock( s_pOnceMutex ) )
         {
            if( HB_IS_NIL( pItem ) )
            {
               if( pAction )
               {
                  hb_storl( FALSE, 1 );
                  hb_vmEvalBlock( pAction );
               }
               hb_storl( TRUE, 1 );
               fFirstCall = TRUE;
            }
            hb_threadMutexUnlock( s_pOnceMutex );
         }
#else
         hb_storl( TRUE, 1 );
         fFirstCall = TRUE;
         if( pAction )
            hb_vmEvalBlock( pAction );
#endif
      }
      hb_retl( fFirstCall );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* II. MUTEXES */

typedef struct _HB_MUTEX
{
   BOOL                 fSync;
   int                  lock_count;
   int                  lockers;
   int                  waiters;
   PHB_ITEM             events;
   HB_THREAD_ID         owner;
   HB_RAWCRITICAL_T     mutex;
   HB_RAWCOND_T         cond_l;
   HB_RAWCOND_T         cond_w;
   struct _HB_MUTEX *   pNext;
   struct _HB_MUTEX *   pPrev;
}
HB_MUTEX, * PHB_MUTEX;

typedef struct _HB_MTXLST
{
   int                  lock_count;
   PHB_MUTEX            pMutex;
   struct _HB_MTXLST *  pNext;
}
HB_MTXLST, * PHB_MTXLST;

static PHB_MUTEX s_pSyncList = NULL;
static PHB_MUTEX s_pMutexList = NULL;

static void hb_mutexLink( PHB_MUTEX *pList, PHB_MUTEX pItem )
{
   if( *pList )
   {
      pItem->pNext = *pList;
      pItem->pPrev = (*pList)->pPrev;
      pItem->pPrev->pNext = pItem;
      (*pList)->pPrev = pItem;
   }
   else
   {
      *pList = pItem->pNext = pItem->pPrev = pItem;
   }
}

static void hb_mutexUnlink( PHB_MUTEX *pList, PHB_MUTEX pItem )
{
   pItem->pPrev->pNext = pItem->pNext;
   pItem->pNext->pPrev = pItem->pPrev;
   if( *pList == pItem )
   {
      *pList = pItem->pNext;
      if( *pList == pItem )
         *pList = NULL;    /* this was the last block */
   }
}

#if defined( HB_MT_VM )
static void hb_mutexListUnlock( PHB_MUTEX * pList, PHB_MTXLST * pStore )
{
   HB_CRITICAL_LOCK( s_mutexlst_mtx );
   if( *pList )
   {
      PHB_MUTEX pMutex = *pList;
      do
      {
         if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
         {
            HB_CRITICAL_LOCK( pMutex->mutex );
            if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
            {
               if( pStore )
               {
                  *pStore = ( PHB_MTXLST ) hb_xgrab( sizeof( HB_MTXLST ) );
                  (*pStore)->lock_count = pMutex->lock_count;
                  (*pStore)->pMutex = pMutex;
                  pStore = &(*pStore)->pNext;
                  *pStore = NULL;
               }
               pMutex->lock_count = 0;
               pMutex->owner = ( HB_THREAD_ID ) 0;
               if( pMutex->lockers )
                  HB_COND_SIGNAL( pMutex->cond_l );
            }
            HB_CRITICAL_UNLOCK( pMutex->mutex );
         }
         pMutex = pMutex->pNext;
      }
      while( pMutex != *pList );
   }
   HB_CRITICAL_UNLOCK( s_mutexlst_mtx );
}

static void hb_mutexListLock( PHB_MTXLST pList )
{
   while( pList )
   {
      PHB_MUTEX pMutex = pList->pMutex;

      HB_CRITICAL_LOCK( pMutex->mutex );
      while( pMutex->lock_count != 0 )
      {
         pMutex->lockers++;
#if defined( HB_PTHREAD_API )
         pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#elif defined( HB_COND_HARBOUR_SUPPORT )
         _hb_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, HB_THREAD_INFINITE_WAIT );
#else
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) HB_COND_WAIT( pMutex->cond_l );
         HB_CRITICAL_LOCK( pMutex->mutex );
#endif
         pMutex->lockers--;
      }
      pMutex->lock_count = pList->lock_count;
      pMutex->owner = HB_THREAD_SELF();
      HB_CRITICAL_UNLOCK( pMutex->mutex );
      {
         PHB_MTXLST pFree = pList;
         pList = pList->pNext;
         hb_xfree( pFree );
      }
   }
}

void hb_threadMutexUnlockAll( void )
{
   hb_mutexListUnlock( &s_pMutexList, NULL );
   hb_mutexListUnlock( &s_pSyncList, NULL );
}

#endif

static HB_GARBAGE_FUNC( hb_mutexDestructor )
{
   PHB_MUTEX pMutex = ( PHB_MUTEX ) Cargo;

#if defined( HB_MT_VM )
   HB_CRITICAL_LOCK( s_mutexlst_mtx );
   hb_mutexUnlink( pMutex->fSync ? &s_pSyncList : &s_pMutexList, pMutex );
   HB_CRITICAL_UNLOCK( s_mutexlst_mtx );
#else
   hb_mutexUnlink( pMutex->fSync ? &s_pSyncList : &s_pMutexList, pMutex );
#endif

   if( pMutex->events )
      hb_itemRelease( pMutex->events );

#if !defined( HB_MT_VM )
   /* nothing */
#else
   HB_CRITICAL_DESTROY( pMutex->mutex );
#  if !defined( HB_COND_HARBOUR_SUPPORT )
   HB_COND_DESTROY( pMutex->cond_l );
   HB_COND_DESTROY( pMutex->cond_w );
#  endif
#endif
}

static PHB_MUTEX hb_mutexPtr( PHB_ITEM pItem )
{
   return ( PHB_MUTEX ) hb_itemGetPtrGC( pItem, hb_mutexDestructor );
}

static PHB_ITEM hb_mutexParam( int iParam )
{
   PHB_ITEM pItem = hb_param( iParam, HB_IT_POINTER );

   if( hb_itemGetPtrGC( pItem, hb_mutexDestructor ) )
      return pItem;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PHB_ITEM hb_threadMutexCreate( BOOL fSync )
{
   PHB_MUTEX pMutex;
   PHB_ITEM pItem;

   pItem = hb_itemNew( NULL );
   pMutex = ( PHB_MUTEX ) hb_gcAlloc( sizeof( HB_MUTEX ), hb_mutexDestructor );
   memset( pMutex, 0, sizeof( HB_MUTEX ) );
   pItem = hb_itemPutPtrGC( pItem, pMutex );

#if !defined( HB_MT_VM )
   /* nothing */
#else
   HB_CRITICAL_INIT( pMutex->mutex );
#  if !defined( HB_COND_HARBOUR_SUPPORT )
   HB_COND_INIT( pMutex->cond_l );
   HB_COND_INIT( pMutex->cond_w );
#  endif
#endif

   pMutex->fSync = fSync;
#if defined( HB_MT_VM )
   HB_CRITICAL_LOCK( s_mutexlst_mtx );
   hb_mutexLink( fSync ? &s_pSyncList : &s_pMutexList, pMutex );
   HB_CRITICAL_UNLOCK( s_mutexlst_mtx );
#else
   hb_mutexLink( fSync ? &s_pSyncList : &s_pMutexList, pMutex );
#endif

   return pItem;
}

BOOL hb_threadMutexUnlock( PHB_ITEM pItem )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      if( pMutex->lock_count )
      {
         if( --pMutex->lock_count == 0 )
            pMutex->owner = ( HB_THREAD_ID ) 0;
         fResult = TRUE;
      }
#else
      HB_CRITICAL_LOCK( pMutex->mutex );
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
      {
         if( --pMutex->lock_count == 0 )
         {
            pMutex->owner = ( HB_THREAD_ID ) 0;
            if( pMutex->lockers )
               HB_COND_SIGNAL( pMutex->cond_l );
         }
         fResult = TRUE;
      }
      HB_CRITICAL_UNLOCK( pMutex->mutex );
#endif
   }
   return fResult;
}

BOOL hb_threadMutexLock( PHB_ITEM pItem )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      pMutex->lock_count++;
      pMutex->owner = ( HB_THREAD_ID ) 1;
      fResult = TRUE;
#else
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
      {
         pMutex->lock_count++;
         fResult = TRUE;
      }
      else
      {
         hb_vmUnlock();

         HB_CRITICAL_LOCK( pMutex->mutex );
         while( pMutex->lock_count != 0 )
         {
            pMutex->lockers++;
#  if defined( HB_PTHREAD_API )
            pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( HB_COND_HARBOUR_SUPPORT )
            _hb_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, HB_THREAD_INFINITE_WAIT );
#  else
            HB_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) HB_COND_WAIT( pMutex->cond_l );
            HB_CRITICAL_LOCK( pMutex->mutex );
#  endif
            pMutex->lockers--;
         }
         pMutex->lock_count = 1;
         pMutex->owner = HB_THREAD_SELF();
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         fResult = TRUE;

         hb_vmLock();
      }
#endif
   }
   return fResult;
}

BOOL hb_threadMutexTimedLock( PHB_ITEM pItem, ULONG ulMilliSec )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      HB_SYMBOL_UNUSED( ulMilliSec );
      pMutex->lock_count++;
      pMutex->owner = ( HB_THREAD_ID ) 1;
      fResult = TRUE;
#else
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
      {
         pMutex->lock_count++;
         fResult = TRUE;
      }
      else
      {
         hb_vmUnlock();

         HB_CRITICAL_LOCK( pMutex->mutex );
         if( ulMilliSec && pMutex->lock_count != 0 )
         {
#  if defined( HB_PTHREAD_API )
            struct timespec ts;

            hb_threadTimeInit( &ts, ulMilliSec );

            /* pthread_cond_signal() wakes up at least one thread
             * but it's not guaranteed it's exactly one thread so
             * we should use while loop here.
             */
            pMutex->lockers++;
            do
            {
               if( pthread_cond_timedwait( &pMutex->cond_l, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
            while( pMutex->lock_count != 0 );
            pMutex->lockers--;
#  else
            /* TODO: on some platforms HB_COND_SIGNAL() may wake up more then
             *       one thread so we should use while loop to check if wait
             *       condition is true.
             */
#     if defined( HB_COND_HARBOUR_SUPPORT )
            pMutex->lockers++;
            _hb_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, ulMilliSec );
            pMutex->lockers--;
#     else
            pMutex->lockers++;
            HB_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) HB_COND_TIMEDWAIT( pMutex->cond_l, ulMilliSec );
            HB_CRITICAL_LOCK( pMutex->mutex );
            pMutex->lockers--;
#     endif
#  endif
         }
         if( pMutex->lock_count == 0 )
         {
            pMutex->lock_count = 1;
            pMutex->owner = HB_THREAD_SELF();
            fResult = TRUE;
         }
         HB_CRITICAL_UNLOCK( pMutex->mutex );

         hb_vmLock();
      }
#endif
   }
   return fResult;
}

void hb_threadMutexNotify( PHB_ITEM pItem, PHB_ITEM pNotifier, BOOL fWaiting )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      if( !fWaiting )
      {
         if( !pMutex->events )
         {
            pMutex->events = hb_itemArrayNew( 1 );
            if( pNotifier && !HB_IS_NIL( pNotifier ) )
               hb_arraySet( pMutex->events, 1, pNotifier );
         }
         else if( pNotifier )
            hb_arrayAdd( pMutex->events, pNotifier );
         else
            hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) + 1 );
      }
      else if( pMutex->waiters )
      {
         int iCount = pMutex->waiters;
         ULONG ulLen;

         if( pMutex->events )
         {
            ulLen = hb_arrayLen( pMutex->events );
            iCount -= ulLen;
            if( iCount > 0 )
               hb_arraySize( pMutex->events, ulLen + iCount );
         }
         else
         {
            ulLen = 0;
            pMutex->events = hb_itemArrayNew( iCount );
         }
         if( iCount > 0 )
         {
            if( pNotifier && !HB_IS_NIL( pNotifier ) )
            {
               int iSet = iCount;
               do
                  hb_arraySet( pMutex->events, ++ulLen, pNotifier );
               while( --iSet );
            }
         }
      }
#else
      HB_CRITICAL_LOCK( pMutex->mutex );
      if( !fWaiting )
      {
         if( !pMutex->events )
         {
            pMutex->events = hb_itemArrayNew( 1 );
            if( pNotifier && !HB_IS_NIL( pNotifier ) )
               hb_arraySet( pMutex->events, 1, pNotifier );
         }
         else if( pNotifier )
            hb_arrayAdd( pMutex->events, pNotifier );
         else
            hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) + 1 );
         if( pMutex->waiters )
            HB_COND_SIGNAL( pMutex->cond_w );
      }
      else if( pMutex->waiters )
      {
         int iCount = pMutex->waiters;
         ULONG ulLen;

         if( pMutex->events )
         {
            ulLen = hb_arrayLen( pMutex->events );
            iCount -= ulLen;
            if( iCount > 0 )
               hb_arraySize( pMutex->events, ulLen + iCount );
         }
         else
         {
            ulLen = 0;
            pMutex->events = hb_itemArrayNew( iCount );
         }
         if( iCount > 0 )
         {
            if( pNotifier && !HB_IS_NIL( pNotifier ) )
            {
               int iSet = iCount;
               do
                  hb_arraySet( pMutex->events, ++ulLen, pNotifier );
               while( --iSet );
            }
            if( iCount == 1 )
               HB_COND_SIGNAL( pMutex->cond_w );
            else
               HB_COND_SIGNALN( pMutex->cond_w, iCount );
         }
      }
      HB_CRITICAL_UNLOCK( pMutex->mutex );
#endif
   }
}

PHB_ITEM hb_threadMutexSubscribe( PHB_ITEM pItem, BOOL fClear )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   PHB_ITEM pResult = NULL;

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      if( pMutex->events && hb_arrayLen( pMutex->events ) > 0 )
      {
         if( fClear && pMutex->events )
            hb_arraySize( pMutex->events, 0 );
         else
         {
            pResult = hb_itemNew( NULL );
            hb_arrayGet( pMutex->events, 1, pResult );
            hb_arrayDel( pMutex->events, 1 );
            hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) - 1 );
         }
      }
#else
      PHB_MTXLST pSyncList = NULL;
      BOOL fSync = TRUE;
      int lock_count = 0;

      hb_vmUnlock();

      HB_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
         hb_arraySize( pMutex->events, 0 );

      /* release own locak from this mutex */
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
      {
         lock_count = pMutex->lock_count;
         pMutex->lock_count = 0;
         pMutex->owner = ( HB_THREAD_ID ) 0;
         if( pMutex->lockers )
            HB_COND_SIGNAL( pMutex->cond_l );
      }

      while( !pMutex->events || hb_arrayLen( pMutex->events ) == 0 )
      {
         if( fSync )
         {
            /* SYNC method mutexes cannot be used for subscribe so it's safe
             * to unlock them when THIS mutex is internally locked
             */
            hb_mutexListUnlock( &s_pSyncList, &pSyncList );
            fSync = FALSE;
         }

         pMutex->waiters++;
#  if defined( HB_PTHREAD_API )
         pthread_cond_wait( &pMutex->cond_w, &pMutex->mutex );
#  elif defined( HB_COND_HARBOUR_SUPPORT )
         _hb_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, HB_THREAD_INFINITE_WAIT );
#  else
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) HB_COND_WAIT( pMutex->cond_w );
         HB_CRITICAL_LOCK( pMutex->mutex );
#  endif
         pMutex->waiters--;
      }

      if( pMutex->events && hb_arrayLen( pMutex->events ) > 0 )
      {
         pResult = hb_itemNew( NULL );
         hb_arrayGet( pMutex->events, 1, pResult );
         hb_arrayDel( pMutex->events, 1 );
         hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) - 1 );
      }

      /* restore the own lock on this mutex if necessary */
      if( lock_count )
      {
         if( pMutex->owner )
         {
            pMutex->lockers++;
            while( pMutex->lock_count != 0 )
            {
#  if defined( HB_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( HB_COND_HARBOUR_SUPPORT )
               _hb_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, HB_THREAD_INFINITE_WAIT );
#  else
               HB_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) HB_COND_WAIT( pMutex->cond_l );
               HB_CRITICAL_LOCK( pMutex->mutex );
#  endif
            }
            pMutex->lockers--;
         }
         pMutex->lock_count = lock_count;
         pMutex->owner = HB_THREAD_SELF();
      }

      HB_CRITICAL_UNLOCK( pMutex->mutex );

      hb_mutexListLock( pSyncList );

      hb_vmLock();
#endif
   }
   return pResult;
}

PHB_ITEM hb_threadMutexTimedSubscribe( PHB_ITEM pItem, ULONG ulMilliSec, BOOL fClear )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   PHB_ITEM pResult = NULL;

   if( pMutex )
   {
#if !defined( HB_MT_VM )
      HB_SYMBOL_UNUSED( ulMilliSec );

      if( pMutex->events && hb_arrayLen( pMutex->events ) > 0 )
      {
         if( fClear && pMutex->events )
            hb_arraySize( pMutex->events, 0 );
         else
         {
            pResult = hb_itemNew( NULL );
            hb_arrayGet( pMutex->events, 1, pResult );
            hb_arrayDel( pMutex->events, 1 );
            hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) - 1 );
         }
      }
#else
      PHB_MTXLST pSyncList = NULL;
      int lock_count = 0;

      hb_vmUnlock();

      HB_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
         hb_arraySize( pMutex->events, 0 );

      if( ulMilliSec && !( pMutex->events && hb_arrayLen( pMutex->events ) > 0 ) )
      {
         /* release own locak from this mutex */
         if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
         {
            lock_count = pMutex->lock_count;
            pMutex->lock_count = 0;
            pMutex->owner = ( HB_THREAD_ID ) 0;
            if( pMutex->lockers )
               HB_COND_SIGNAL( pMutex->cond_l );
         }

         /* SYNC method mutexes cannot be used for subscribe so it's safe
          * to unlock them when THIS mutex is internally locked
          */
         hb_mutexListUnlock( &s_pSyncList, &pSyncList );

         pMutex->waiters++;
#  if defined( HB_PTHREAD_API )
         {
            struct timespec ts;

            hb_threadTimeInit( &ts, ulMilliSec );
            while( !pMutex->events || hb_arrayLen( pMutex->events ) == 0 )
            {
               if( pthread_cond_timedwait( &pMutex->cond_w, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
         }
#  else
         {
            /* TODO: on some platforms HB_COND_SIGNAL() may wake up more then
             *       one thread so we should use while loop to check if wait
             *       condition is true.
             */
#     if defined( HB_COND_HARBOUR_SUPPORT )
            _hb_thread_cond_wait( &pMutex->cond_w, &pMutex->mutex, ulMilliSec );
#     else
            HB_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) HB_COND_TIMEDWAIT( pMutex->cond_w, ulMilliSec );
            HB_CRITICAL_LOCK( pMutex->mutex );
#     endif
         }
#  endif
         pMutex->waiters--;
      }

      if( pMutex->events && hb_arrayLen( pMutex->events ) > 0 )
      {
         pResult = hb_itemNew( NULL );
         hb_arrayGet( pMutex->events, 1, pResult );
         hb_arrayDel( pMutex->events, 1 );
         hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) - 1 );
      }

      /* restore the own lock on this mutex if necessary */
      if( lock_count )
      {
         if( pMutex->owner )
         {
            pMutex->lockers++;
            while( pMutex->lock_count != 0 )
            {
#  if defined( HB_PTHREAD_API )
               pthread_cond_wait( &pMutex->cond_l, &pMutex->mutex );
#  elif defined( HB_COND_HARBOUR_SUPPORT )
               _hb_thread_cond_wait( &pMutex->cond_l, &pMutex->mutex, HB_THREAD_INFINITE_WAIT );
#  else
               HB_CRITICAL_UNLOCK( pMutex->mutex );
               ( void ) HB_COND_WAIT( pMutex->cond_l );
               HB_CRITICAL_LOCK( pMutex->mutex );
#  endif
            }
            pMutex->lockers--;
         }
         pMutex->lock_count = lock_count;
         pMutex->owner = HB_THREAD_SELF();
      }

      HB_CRITICAL_UNLOCK( pMutex->mutex );

      hb_mutexListLock( pSyncList );

      hb_vmLock();
#endif
   }
   return pResult;
}

HB_FUNC( HB_MUTEXCREATE )
{
   hb_itemReturnRelease( hb_threadMutexCreate( FALSE ) );
}

HB_FUNC( HB_MUTEXLOCK )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
   {
      if( ISNUM( 2 ) )
      {
         ULONG ulMilliSec = 0;
         double dTimeOut = hb_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ULONG ) ( dTimeOut * 1000 );
         hb_retl( hb_threadMutexTimedLock( pItem, ulMilliSec ) );
      }
      else
         hb_retl( hb_threadMutexLock( pItem ) );
   }
}

HB_FUNC( HB_MUTEXUNLOCK )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
      hb_retl( hb_threadMutexUnlock( pItem ) );
}

HB_FUNC( HB_MUTEXNOTIFY )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
      hb_threadMutexNotify( pItem, hb_param( 2, HB_IT_ANY ), FALSE );
}

HB_FUNC( HB_MUTEXNOTIFYALL )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
      hb_threadMutexNotify( pItem, hb_param( 2, HB_IT_ANY ), TRUE );
}

HB_FUNC( HB_MUTEXSUBSCRIBE )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
   {
      PHB_ITEM pResult;

      if( ISNUM( 2 ) )
      {
         ULONG ulMilliSec = 0;
         double dTimeOut = hb_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ULONG ) ( dTimeOut * 1000 );
         pResult = hb_threadMutexTimedSubscribe( pItem, ulMilliSec, FALSE );
      }
      else
         pResult = hb_threadMutexSubscribe( pItem, FALSE );

      if( pResult )
      {
         hb_itemParamStoreForward( 3, pResult );
         hb_itemRelease( pResult );
         hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
}

HB_FUNC( HB_MUTEXSUBSCRIBENOW )
{
   PHB_ITEM pItem = hb_mutexParam( 1 );

   if( pItem )
   {
      PHB_ITEM pResult;

      if( ISNUM( 2 ) )
      {
         ULONG ulMilliSec = 0;
         double dTimeOut = hb_parnd( 2 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ULONG ) ( dTimeOut * 1000 );
         pResult = hb_threadMutexTimedSubscribe( pItem, ulMilliSec, TRUE );
      }
      else
         pResult = hb_threadMutexSubscribe( pItem, TRUE );

      if( pResult )
      {
         hb_itemParamStoreForward( 3, pResult );
         hb_itemRelease( pResult );
         hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
}

HB_FUNC( HB_MTVM )
{
#if defined( HB_MT_VM )
   hb_retl( TRUE );
#else
   hb_retl( FALSE );
#endif
}
