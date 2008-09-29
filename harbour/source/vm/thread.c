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

#define HB_OS_WIN_32_USED

#define INCL_DOSSEMAPHORES
#define INCL_DOSPROCESS

#define _HB_THREAD_INTERNAL_

#include "hbvmopt.h"
#include "hbthread.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapicdp.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbmemvar.ch"
#include "hbthread.ch"

#if defined( HB_PTHREAD_API )
#  include <time.h>
#  include <sys/time.h>
#endif

static volatile BOOL s_fThreadInit = FALSE;

#if !defined( HB_MT_VM )
   /* nothing */
#else

#  if defined( HB_PTHREAD_API )
   struct timespec ts;

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

#  if defined( HB_CRITICAL_INIT )
   static HB_RAWCRITICAL_T s_critical_init;
   static void hb_threadCriticalInit( HB_CRITICAL_T * critical )
   {
      if( !s_fThreadInit )
         hb_threadInit();

      HB_CRITICAL_LOCK( s_critical_init );
      if( !critical->fInit )
      {
         HB_CRITICAL_INIT( critical->critical );
         critical->fInit = TRUE;
      }
      HB_CRITICAL_UNLOCK( s_critical_init );
   }
#  else
   static HB_CRITICAL_NEW( s_critical_init );
#  endif

#  if defined( HB_COND_INIT )
      static void hb_threadCondInit( HB_COND_T * cond )
      {
         if( !s_fThreadInit )
            hb_threadInit();
   
         HB_CRITICAL_LOCK( s_critical_init );
         if( !cond->fInit )
         {
            HB_COND_INIT( cond->cond );
#     if defined( HB_CRITICAL_INIT )
            HB_CRITICAL_INIT( cond->critical );
#     endif
            cond->waiters = 0;
            cond->fInit = TRUE;
         }
         HB_CRITICAL_UNLOCK( s_critical_init );
      }
#  endif

#endif /* HB_MT_VM */

void hb_threadInit( void )
{
   if( !s_fThreadInit )
   {
#if !defined( HB_MT_VM )
      /* nothing to do */
#elif defined( HB_CRITICAL_INIT )
      HB_CRITICAL_INIT( s_critical_init );
#endif
      s_fThreadInit = TRUE;
   }
}

void hb_threadExit( void )
{
   if( s_fThreadInit )
   {
      s_fThreadInit = FALSE;
#if !defined( HB_MT_VM )
      /* nothing to do */
#elif defined( HB_CRITICAL_DESTROY )
      HB_CRITICAL_DESTROY( s_critical_init );
#endif
   }
}

void hb_threadEnterCriticalSection( HB_CRITICAL_T * critical )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( critical );
#elif defined( HB_CRITICAL_INIT )
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
#elif defined( HB_CRITICAL_INIT )
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

#  if defined( HB_COND_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_signal( HB_COND_GET( cond ) ) == 0;

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

#  if defined( HB_COND_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_broadcast( HB_COND_GET( cond ) ) == 0;

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

#  if defined( HB_COND_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   return pthread_cond_wait( HB_COND_GET( cond ), HB_CRITICAL_GET( mutex ) ) == 0;

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

#  if defined( HB_COND_INIT )
      if( !cond->fInit )
         hb_threadCondInit( cond );
#  endif
   hb_threadTimeInit( &ts, ulMilliSec );
   return pthread_cond_timedwait( HB_COND_GET( cond ), HB_CRITICAL_GET( mutex ), &ts ) == 0;

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

   return fResult;

#endif
}

HB_THREAD_T hb_threadCreate( PHB_THREAD_STARTFUNC start_func, void * Cargo )
{
   HB_THREAD_T th_id;

#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( start_func );
   HB_SYMBOL_UNUSED( Cargo );
   th_id = 0;
#elif defined( HB_PTHREAD_API )
   if( pthread_create( &th_id, NULL, start_func, Cargo ) != 0 )
      th_id = 0;
#elif defined( HB_OS_WIN_32 )
   th_id = ( HANDLE ) _beginthreadex( NULL, 0, start_func, Cargo, 0, NULL );
#elif defined( HB_OS_OS2 )
   th_id = _beginthread( ( void * ) start_func, NULL, 128 * 1024, Cargo );
#else
   { int TODO_MT; }
   th_id = 0;
#endif

   return th_id;
}

BOOL hb_threadJoin( HB_THREAD_T th_id )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( th_id );
   return FALSE;
#elif defined( HB_PTHREAD_API )
   return pthread_join( th_id, NULL ) == 0;
#elif defined( HB_OS_WIN_32 )
   if( WaitForSingleObject( th_id, INFINITE ) != WAIT_FAILED )
   {
      CloseHandle( th_id );
      return TRUE;
   }
   return FALSE;
#elif defined( HB_OS_OS2 )
   APIRET rc = DosWaitThread( &th_id, DCWW_WAIT );
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

BOOL hb_threadDetach( HB_THREAD_T th_id )
{
#if !defined( HB_MT_VM )
   HB_SYMBOL_UNUSED( th_id );
   return FALSE;
#elif defined( HB_PTHREAD_API )
   return pthread_detach( th_id ) == 0;
#elif defined( HB_OS_WIN_32 )
   return CloseHandle( th_id ) != 0;
#elif defined( HB_OS_OS2 )
   APIRET rc = DosWaitThread( &th_id, DCWW_NOWAIT );
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
   if( pThread->pSet )
   {
      hb_setRelease( pThread->pSet );
      hb_xfree( pThread->pSet );
      pThread->pSet = NULL;
   }
   if( pThread->th_id != 0 )
   {
      hb_threadDetach( pThread->th_id );
      pThread->th_id = 0;
   }
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

   return pThread;
}

static PHB_THREADSTATE hb_thParam( int iParam )
{
   PHB_THREADSTATE pThread = ( PHB_THREADSTATE ) hb_parptrGC( hb_threadDestructor, iParam );

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
         pThread->th_id = hb_threadCreate( hb_threadStartVM, ( void * ) pReturn );

      if( pThread->th_id == 0 )
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
   if( pThread )
      hb_itemReturn( pThread->pThItm );
#endif
}

HB_FUNC( HB_THREADJOIN )
{
   PHB_THREADSTATE pThread = hb_thParam( 1 );

   if( pThread )
   {
      BOOL fResult = FALSE;

      if( pThread->th_id )
      {
         hb_vmUnlock();
         fResult = hb_threadJoin( pThread->th_id );
         if( fResult )
            pThread->th_id = 0;
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
   PHB_THREADSTATE pThread = hb_thParam( 1 );

   if( pThread )
   {
      BOOL fResult = FALSE;

      if( pThread->th_id && hb_threadDetach( pThread->th_id ) )
      {
         pThread->th_id = 0;
         fResult = TRUE;
      }
      hb_retl( fResult );
   }
}

HB_FUNC( HB_THREADQUITREQUEST )
{
   PHB_THREADSTATE pThread = hb_thParam( 1 );

   if( pThread )
   {
      BOOL fResult = FALSE;

#if defined( HB_MT_VM )
      if( pThread->fActive )
      {
         hb_vmThreadQuitRequest( ( void * ) pThread );
         fResult = TRUE;
      }
#endif
      hb_retl( fResult );
   }
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

/* II. MUTEXES */

typedef struct _HB_MUTEX
{
   int                  lock_count;
   int                  lockers;
   int                  waiters;
   PHB_ITEM             events;
   HB_THREAD_ID         owner;
   HB_RAWCRITICAL_T     mutex;
   HB_RAWCOND_T         cond;
   BOOL                 fSync;
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
static void hb_mutexUnlockList( PHB_MUTEX * pList, PHB_MTXLST * pStore )
{
   HB_CRITICAL_LOCK( s_critical_init );
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
               HB_COND_SIGNALN( pMutex->cond, pMutex->lockers );
            }
            HB_CRITICAL_UNLOCK( pMutex->mutex );
         }
         pMutex = pMutex->pNext;
      }
      while( pMutex != *pList );
   }
   HB_CRITICAL_UNLOCK( s_critical_init );
}

static void hb_mutexLockList( PHB_MTXLST pList )
{
   while( pList )
   {
      PHB_MUTEX pMutex = pList->pMutex;

      HB_CRITICAL_LOCK( pMutex->mutex );
      pMutex->lockers++;
      while( pMutex->lock_count != 0 )
      {
#if defined( HB_PTHREAD_API )
         pthread_cond_wait( &pMutex->cond, &pMutex->mutex );
#else
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) HB_COND_WAIT( pMutex->cond );
         HB_CRITICAL_LOCK( pMutex->mutex );
#endif
      }
      pMutex->lockers--;
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
   hb_mutexUnlockList( &s_pMutexList, NULL );
   hb_mutexUnlockList( &s_pSyncList, NULL );
}

#endif

static HB_GARBAGE_FUNC( hb_mutexDestructor )
{
   PHB_MUTEX pMutex = ( PHB_MUTEX ) Cargo;

#if defined( HB_MT_VM )
   HB_CRITICAL_LOCK( s_critical_init );
   hb_mutexUnlink( pMutex->fSync ? &s_pSyncList : &s_pMutexList, pMutex );
   HB_CRITICAL_UNLOCK( s_critical_init );
#else
   hb_mutexUnlink( pMutex->fSync ? &s_pSyncList : &s_pMutexList, pMutex );
#endif

   if( pMutex->events )
      hb_itemRelease( pMutex->events );

#if !defined( HB_MT_VM )
   /* nothing */
#elif defined( HB_PTHREAD_API )
   pthread_mutex_destroy( &pMutex->mutex );
   pthread_cond_destroy( &pMutex->cond );
#else
   HB_CRITICAL_DESTROY( pMutex->mutex );
   HB_COND_DESTROY( pMutex->cond );
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
#elif defined( HB_PTHREAD_API )
   pthread_mutex_init( &pMutex->mutex, NULL );
   pthread_cond_init( &pMutex->cond, NULL );
#else
   HB_CRITICAL_INIT( pMutex->mutex );
   HB_COND_INIT( pMutex->cond );
#endif

   pMutex->fSync = fSync;
#if defined( HB_MT_VM )
   HB_CRITICAL_LOCK( s_critical_init );
   hb_mutexLink( fSync ? &s_pSyncList : &s_pMutexList, pMutex );
   HB_CRITICAL_UNLOCK( s_critical_init );
#else
   hb_mutexLink( fSync ? &s_pSyncList : &s_pMutexList, pMutex );
#endif

   return pItem;
}

BOOL hb_threadMutexLock( PHB_ITEM pItem )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
         pMutex->lock_count++;
      else
      {
         hb_vmUnlock();

#if !defined( HB_MT_VM )
         pMutex->lock_count = 1;
         pMutex->owner = HB_THREAD_SELF();
         fResult = TRUE;
#else
         HB_CRITICAL_LOCK( pMutex->mutex );
         pMutex->lockers++;
         while( pMutex->lock_count != 0 )
         {
#if defined( HB_PTHREAD_API )
            pthread_cond_wait( &pMutex->cond, &pMutex->mutex );
#else
            HB_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) HB_COND_WAIT( pMutex->cond );
            HB_CRITICAL_LOCK( pMutex->mutex );
#endif
         }
         pMutex->lockers--;
         pMutex->lock_count = 1;
         pMutex->owner = HB_THREAD_SELF();
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         fResult = TRUE;
#endif

         hb_vmLock();
      }
   }
   return fResult;
}

BOOL hb_threadMutexTimedLock( PHB_ITEM pItem, ULONG ulMilliSec )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
         pMutex->lock_count++;
      else
      {
         hb_vmUnlock();

#if !defined( HB_MT_VM )
         HB_SYMBOL_UNUSED( ulMilliSec );
         pMutex->lock_count = 1;
         pMutex->owner = HB_THREAD_SELF();
         fResult = TRUE;
#elif defined( HB_PTHREAD_API )
         pthread_mutex_lock( &pMutex->mutex );
         if( ulMilliSec && pMutex->lock_count != 0 )
         {
            struct timespec ts;

            hb_threadTimeInit( &ts, ulMilliSec );

            /* pthread_cond_signal() wakes up at least one thread
             * but it's not guaranteed it's exactly one thread so
             * we should use while look here.
             */
            pMutex->lockers++;
            while( pMutex->lock_count == 0 )
            {
               if( pthread_cond_timedwait( &pMutex->cond, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
            pMutex->lockers--;
         }
         if( pMutex->lock_count == 0 )
         {
            pMutex->lock_count = 1;
            pMutex->owner = HB_THREAD_SELF();
            fResult = TRUE;
         }
         pthread_mutex_unlock( &pMutex->mutex );
#else
         HB_CRITICAL_LOCK( pMutex->mutex );
         if( ulMilliSec && pMutex->lock_count != 0 )
         {
            pMutex->lockers++;
            HB_CRITICAL_UNLOCK( pMutex->mutex );
            ( void ) HB_COND_TIMEDWAIT( pMutex->cond, ulMilliSec );
            HB_CRITICAL_LOCK( pMutex->mutex );
            pMutex->lockers--;
         }
         if( pMutex->lock_count == 0 )
         {
            pMutex->lock_count = 1;
            pMutex->owner = HB_THREAD_SELF();
            fResult = TRUE;
         }
         HB_CRITICAL_UNLOCK( pMutex->mutex );
#endif

         hb_vmLock();
      }
   }
   return fResult;
}

BOOL hb_threadMutexUnlock( PHB_ITEM pItem )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );
   BOOL fResult = FALSE;

   if( pMutex )
   {
      HB_CRITICAL_LOCK( pMutex->mutex );
      if( HB_THREAD_EQUAL( pMutex->owner, HB_THREAD_SELF() ) )
      {
         if( --pMutex->lock_count == 0 )
         {
            pMutex->owner = ( HB_THREAD_ID ) 0;
            HB_COND_SIGNALN( pMutex->cond, pMutex->lockers );
         }
         fResult = TRUE;
      }
      HB_CRITICAL_UNLOCK( pMutex->mutex );
   }
   return fResult;
}

void hb_threadMutexNotify( PHB_ITEM pItem, PHB_ITEM pNotifier, BOOL fWaiting )
{
   PHB_MUTEX pMutex = hb_mutexPtr( pItem );

   if( pMutex )
   {
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
         HB_COND_SIGNAL( pMutex->cond );
      }
      else if( pMutex->waiters )
      {
         int iCount = pMutex->waiters;
         ULONG ulLen;

         if( pMutex->events )
         {
            ulLen = hb_arrayLen( pMutex->events );
            hb_arraySize( pMutex->events, ulLen + pMutex->waiters );
         }
         else
         {
            ulLen = pMutex->waiters;
            pMutex->events = hb_itemArrayNew( ulLen );
         }
         if( pNotifier && !HB_IS_NIL( pNotifier ) )
         {
            do
               hb_arraySet( pMutex->events, ++ulLen, pNotifier );
            while( --iCount );
         }
         HB_COND_SIGNALN( pMutex->cond, pMutex->waiters );
      }
      HB_CRITICAL_UNLOCK( pMutex->mutex );
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

      hb_mutexUnlockList( &s_pSyncList, &pSyncList );
      hb_vmUnlock();

      HB_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
         hb_arraySize( pMutex->events, 0 );

      pMutex->waiters++;
      while( !pMutex->events || hb_arrayLen( pMutex->events ) == 0 )
      {
#  if defined( HB_PTHREAD_API )
         pthread_cond_wait( &pMutex->cond, &pMutex->mutex );
#  else
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) HB_COND_WAIT( pMutex->cond );
         HB_CRITICAL_LOCK( pMutex->mutex );
#  endif
      }
      pMutex->waiters--;

      if( pMutex->events && hb_arrayLen( pMutex->events ) > 0 )
      {
         pResult = hb_itemNew( NULL );
         hb_arrayGet( pMutex->events, 1, pResult );
         hb_arrayDel( pMutex->events, 1 );
         hb_arraySize( pMutex->events, hb_arrayLen( pMutex->events ) - 1 );
      }

      HB_CRITICAL_UNLOCK( pMutex->mutex );

      hb_vmLock();
      hb_mutexLockList( pSyncList );
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

      hb_mutexUnlockList( &s_pSyncList, &pSyncList );
      hb_vmUnlock();

      HB_CRITICAL_LOCK( pMutex->mutex );

      if( fClear && pMutex->events )
         hb_arraySize( pMutex->events, 0 );

      if( ulMilliSec && !( pMutex->events && hb_arrayLen( pMutex->events ) > 0 ) )
      {
         pMutex->waiters++;
#  if defined( HB_PTHREAD_API )
         {
            struct timespec ts;

            hb_threadTimeInit( &ts, ulMilliSec );
            while( !pMutex->events || hb_arrayLen( pMutex->events ) == 0 )
            {
               if( pthread_cond_timedwait( &pMutex->cond, &pMutex->mutex, &ts ) != 0 )
                  break;
            }
         }
#  else
         HB_CRITICAL_UNLOCK( pMutex->mutex );
         ( void ) HB_COND_TIMEDWAIT( pMutex->cond, ulMilliSec );
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

      HB_CRITICAL_UNLOCK( pMutex->mutex );

      hb_vmLock();
      hb_mutexLockList( pSyncList );
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
         double dTimeOut = hb_parnd( 1 );
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
         double dTimeOut = hb_parnd( 1 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ULONG ) ( dTimeOut * 1000 );
         pResult = hb_threadMutexTimedSubscribe( pItem, ulMilliSec, FALSE );
      }
      else
         pResult = hb_threadMutexSubscribe( pItem, FALSE );

      hb_itemParamStoreForward( 3, pResult);
      hb_itemRelease( pResult );
      hb_retl( pResult != NULL );
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
         double dTimeOut = hb_parnd( 1 );
         if( dTimeOut > 0 )
            ulMilliSec = ( ULONG ) ( dTimeOut * 1000 );
         pResult = hb_threadMutexTimedSubscribe( pItem, ulMilliSec, TRUE );
      }
      else
         pResult = hb_threadMutexSubscribe( pItem, TRUE );

      hb_itemParamStoreForward( 3, pResult);
      hb_itemRelease( pResult );
      hb_retl( pResult != NULL );
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
