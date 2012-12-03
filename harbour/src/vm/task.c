/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    platform independent task system. It's used when when OS does not
 *    support threads
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


#define HB_TASK_DEBUG

#include "hbapi.h"
#include "hbtask.h"
#include "hbapierr.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#endif

#include <time.h>
#if defined( HB_OS_UNIX )
#  include <sys/time.h>
#  include <sys/types.h>
#  include <unistd.h>
#elif defined( __DJGPP__ )
#  include <unistd.h>
#elif defined( __MINGW32__ )
#  include <sys/time.h>
#endif

#if ! defined( HB_HAS_UCONTEXT )
#  if ( defined( HB_OS_LINUX ) && ! defined( __WATCOMC__ ) ) || defined( HB_OS_MINIX )
#     define HB_HAS_UCONTEXT
#  endif
#endif

#if defined( HB_HAS_UCONTEXT )
#  include <ucontext.h>
#else
#  include <setjmp.h>
#endif


#define HB_TASK_STACK_MIN       16384
#define HB_TASK_STACK_ALIGN     16
#define HB_TASK_NO_DELAY        0
#define HB_TASK_INFINITE_DELAY  HB_VMLONG_MAX


#undef HB_TASK_STACK_INIT

#if ! defined( HB_HAS_UCONTEXT )

#  if defined( __GNUC__ )
#     if defined( __DJGPP__ )
#        define HB_TASK_STACK_INIT( jmp, sp )      \
                  do { (jmp)[0].__esp = (unsigned) (sp); } while( 0 )
#     elif defined( HB_OS_LINUX )
#        if defined( JB_SP )
#           define HB_TASK_STACK_INIT( jmp, sp )      \
                  do { (jmp)[0].__jmpbuf[JB_SP] = (int) (sp); } while( 0 )
#        else
#           define HB_TASK_STACK_INIT( jmp, sp )      \
                  do { (jmp)[0].__jmpbuf[4] = (int) (sp); } while( 0 )
#        endif
#     elif defined( HB_OS_WIN )
#        define HB_TASK_STACK_INIT( jmp, sp )      \
                  do { (jmp)[7] = (unsigned) (sp); } while( 0 )
#     endif
#  elif defined( __WATCOMC__ )
#     if defined( __386__ )
#        define HB_TASK_STACK_INIT( jmp, sp )      \
                  do { (jmp)[7] = (unsigned int) (sp); } while( 0 )
#     endif
#  endif

#  ifndef HB_TASK_STACK_INIT
#     error Unsupported system/architecture
#  endif

#endif

typedef enum
{
   TASK_INIT = 0,
   TASK_RUNNING,
   TASK_SLEEPING,
   TASK_SUSPEND,
   TASK_DONE,
   TASK_ZOMBIE
}
HB_TASKSTATE;

struct _HB_TASKMTX;
struct _HB_TASKCOND;

typedef struct _HB_TASKINFO
{
   int            id;
   HB_TASKSTATE   state;
   HB_BOOL        detached;
   HB_BOOL        stop;

   HB_MAXINT      wakeup;

   char *         stack;
   long           stack_size;

#if defined( HB_HAS_UCONTEXT )
   ucontext_t     context;
#else
   jmp_buf        context;
#endif

   void *         ( * start ) ( void * );
   void *         cargo;
   void *         result;

   void *         data;

   int            joiners;
   int            locked;

   struct _HB_TASKINFO * joining;
   struct _HB_TASKMTX  * locking;
   struct _HB_TASKCOND * waiting;

   struct _HB_TASKINFO * pSleepNext;
   struct _HB_TASKINFO * pBlockNext;
   struct _HB_TASKINFO * pWaitNext;

   struct _HB_TASKINFO * pNext;
   struct _HB_TASKINFO * pPrev;
}
HB_TASKINFO, * PHB_TASKINFO;

typedef struct _HB_TASKMTX
{
   int            count;
   PHB_TASKINFO   task;
   PHB_TASKINFO   lockers;
   struct _HB_TASKMTX * next;
}
HB_TASKMTX, * PHB_TASKMTX;

typedef struct _HB_TASKCOND
{
   PHB_TASKINFO   waiters;
   PHB_TASKMTX    mutex;
   struct _HB_TASKCOND * next;
}
HB_TASKCOND, * PHB_TASKCOND;


static PHB_TASKINFO s_taskSleep = NULL;
static PHB_TASKINFO s_taskList = NULL;
static PHB_TASKINFO s_currTask = NULL;
static PHB_TASKINFO s_mainTask = NULL;

static PHB_TASKCOND s_condList = NULL;
static PHB_TASKMTX s_mutexList = NULL;

static int s_iTaskID = 0;

static HB_MAXINT hb_taskTimeStop( unsigned long ulMilliSec )
{
   if( ulMilliSec == HB_TASK_INFINITE_WAIT )
      return HB_TASK_INFINITE_DELAY;
   else
   {
#if defined( __DJGPP__ )
      /* uclock_t uclock() * 1000 / UCLOCKS_PER_SEC */
      return ( HB_MAXINT ) clock() * 1000 / CLOCKS_PER_SEC + ulMilliSec;
#elif _POSIX_C_SOURCE >= 199309L
      struct timespec ts;
      clock_gettime( CLOCK_REALTIME, &ts );
      return ( HB_MAXINT ) ts.tv_sec * 1000 + ts.tv_nsec / 1000000 + ulMilliSec;
#elif defined( HB_OS_UNIX )
      struct timeval tv;
      gettimeofday( &tv, NULL );
      return ( HB_MAXINT ) tv.tv_sec * 1000 + tv.tv_usec / 1000 + ulMilliSec;
#else
      return ( HB_MAXINT ) clock() * 1000 / CLOCKS_PER_SEC + ulMilliSec;
#endif
   }
}

static void hb_taskFreeze( HB_MAXINT wakeup )
{
#if defined( HB_OS_DOS )
   while( wakeup > hb_taskTimeStop( 0 ) )
   {
      union REGS regs;

      regs.h.ah = 2;
      regs.HB_XREGS.ax = 0x1680;

      HB_DOS_INT86( 0x2F, &regs, &regs );
   }
#else
   wakeup -= hb_taskTimeStop( 0 );
   if( wakeup > 0 )
   {
#  if defined( __DJGPP__ )
      usleep( wakeup * 1000 );
#  elif defined( HB_OS_WIN )
      Sleep( wakeup );
#  elif defined( HB_OS_OS2 )
      DosSleep( wakeup );     /* Duration is in milliseconds */
#  elif defined( HB_OS_UNIX )
      struct timeval tv;
      tv.tv_sec = wakeup / 1000;
      tv.tv_usec = ( wakeup % 1000 ) * 1000;
      select( 0, NULL, NULL, NULL, &tv );
#  endif
   }
#endif
}

static void hb_taskLink( PHB_TASKINFO * pList, PHB_TASKINFO pTask )
{
   if( *pList )
   {
      pTask->pNext = *pList;
      ( pTask->pPrev = ( *pList )->pPrev )->pNext = pTask;
      ( *pList )->pPrev = pTask;
   }
   else
      *pList = pTask->pNext = pTask->pPrev = pTask;
}

static void hb_taskUnlink( PHB_TASKINFO * pList, PHB_TASKINFO pTask )
{
   pTask->pPrev->pNext = pTask->pNext;
   pTask->pNext->pPrev = pTask->pPrev;
   if( *pList == pTask )
   {
      *pList = pTask->pNext;
      if( *pList == pTask )
         *pList = NULL;
   }
}

static PHB_TASKMTX hb_taskMutexNew( void )
{
   PHB_TASKMTX pMutex;

   pMutex = ( PHB_TASKMTX ) hb_xgrab( sizeof( HB_TASKMTX ) );
   pMutex->count = 0;
   pMutex->task = NULL;
   pMutex->lockers = NULL;
   pMutex->next = s_mutexList;
   s_mutexList = pMutex;

   return pMutex;
}

static PHB_TASKCOND hb_taskCondNew( void )
{
   PHB_TASKCOND pCond;

   pCond = ( PHB_TASKCOND ) hb_xgrab( sizeof( HB_TASKCOND ) );
   pCond->waiters = NULL;
   pCond->mutex = NULL;
   pCond->next = s_condList;
   s_condList = pCond;

   return pCond;
}

static void hb_taskSetSleep( PHB_TASKINFO pTask, unsigned long ulMilliSec )
{
   PHB_TASKINFO * pSleep;

   pTask->wakeup = hb_taskTimeStop( ulMilliSec );
   pTask->state = TASK_SLEEPING;

   /* insert current task to sleepers queue */
   pSleep = &s_taskSleep;
   while( *pSleep && ( *pSleep )->wakeup <= pTask->wakeup )
      pSleep = &( *pSleep )->pSleepNext;
   pTask->pSleepNext = *pSleep;
   *pSleep = pTask;
}

static void hb_taskWakeUp( PHB_TASKINFO pTask )
{
   PHB_TASKINFO * pSleep = &s_taskSleep;

   while( *pSleep )
   {
      if( *pSleep == pTask )
      {
         *pSleep = ( *pSleep )->pSleepNext;
         break;
      }
      pSleep = &( *pSleep )->pSleepNext;
   }
}

static PHB_TASKINFO hb_taskSwitchLock( PHB_TASKMTX pMutex )
{
   PHB_TASKINFO pTask = pMutex->lockers;

   if( pTask )
   {
      pMutex->lockers = pTask->pBlockNext;
#ifdef HB_TASK_DEBUG
      if( pTask->locking != pMutex )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskSwitchLock: broken lock", NULL, NULL );
#endif
      pTask->locking = NULL;
      pTask->locked++;
      pMutex->task = pTask;
      pMutex->count = 1;
      if( pTask->state == TASK_SLEEPING )
      {
         hb_taskWakeUp( pTask );
         pTask->state = TASK_RUNNING;
      }
      else if( pTask->state != TASK_RUNNING )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskSwitchLock: task resumed", NULL, NULL );
   }
   return pTask;
}

static void hb_taskFree( PHB_TASKINFO pTask )
{
   hb_taskUnlink( &s_taskList, pTask );
   if( pTask->stack )
      hb_xfree( pTask->stack );
   hb_xfree( pTask );
}

static void hb_taskFinalize( PHB_TASKINFO pTask )
{
   pTask->data = NULL;

   if( pTask->joiners )
   {
      PHB_TASKINFO * pSleep = &s_taskSleep, pJoiner;
      while( *pSleep )
      {
         if( ( *pSleep )->joining == pTask )
         {
            pJoiner = *pSleep;
            pJoiner->joining = NULL;
            *pSleep = ( *pSleep )->pSleepNext;
            pJoiner->wakeup = 0;
            pJoiner->pSleepNext = s_taskSleep;
            s_taskSleep = pJoiner;
            if( --pTask->joiners == 0 )
               break;
         }
         else
            pSleep = &( *pSleep )->pSleepNext;
      }
#ifdef HB_TASK_DEBUG
      if( pTask->joiners )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskFinalize: dummy joiners", NULL, NULL );
#endif
   }

   /* it cannot happen for runing threads */

   /* remove from mutex lockers queue */
   if( pTask->locking )
   {
      PHB_TASKINFO * pLock = &pTask->locking->lockers;

      while( *pLock )
      {
         if( *pLock == pTask )
         {
            *pLock = pTask->pBlockNext;
            pTask->locking = NULL;
            break;
         }
         else
            pLock = &( *pLock )->pBlockNext;
      }
#ifdef HB_TASK_DEBUG
      if( pTask->locking )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskFinalize: dummy lock", NULL, NULL );
#endif
   }

   /* remove from condition queue */
   if( pTask->waiting )
   {
      PHB_TASKINFO * pWait = &pTask->waiting->waiters;

      while( *pWait )
      {
         if( *pWait == pTask )
         {
            *pWait = pTask->pWaitNext;
            pTask->waiting = NULL;
            break;
         }
         else
            pWait = &( *pWait )->pWaitNext;
      }
#ifdef HB_TASK_DEBUG
      if( pTask->waiting )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskFinalize: dummy cond", NULL, NULL );
#endif
   }

   if( pTask->state == TASK_SLEEPING )
      hb_taskWakeUp( pTask );

   pTask->state = TASK_DONE;

   if( pTask->locked )
   {
      PHB_TASKMTX pMutex = s_mutexList;

      while( pMutex )
      {
         if( pMutex->task == pTask && pMutex->count )
         {
            pTask->locked--;
            pMutex->task = NULL;
            pMutex->count = 0;
            hb_taskSwitchLock( pMutex );
         }
         pMutex = pMutex->next;
      }

#ifdef HB_TASK_DEBUG
      if( pTask->locked )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskFinalize: dummy lock", NULL, NULL );
#endif
   }

   if( pTask->detached )
   {
      pTask->state = TASK_ZOMBIE;
      if( pTask == s_currTask )
      {
         /* switch to next active task */
         hb_taskYield();
         /* unreachable code */
      }
      else
         hb_taskFree( pTask );
   }
}

static void hb_taskRun( void )
{
   /* execute task startup function */
   s_currTask->result = s_currTask->start( s_currTask->cargo );
   /* mark task as finished */
   hb_taskFinalize( s_currTask );
   /* switch to next active task */
   hb_taskYield();
   /* unreachable code */
}

static PHB_TASKINFO hb_taskNew( long stack_size )
{
   PHB_TASKINFO pTask;

   if( stack_size < HB_TASK_STACK_MIN )
      stack_size = HB_TASK_STACK_MIN;

   pTask = ( PHB_TASKINFO ) memset( hb_xgrab( sizeof( HB_TASKINFO ) ), 0,
                                    sizeof( HB_TASKINFO ) );
   pTask->stack = ( char * ) hb_xgrab( stack_size );

   stack_size += ( HB_PTRDIFF ) pTask->stack;
   stack_size &= ~( HB_TASK_STACK_ALIGN - 1 );
   stack_size -= ( HB_PTRDIFF ) pTask->stack;

   pTask->stack_size = stack_size;
   pTask->id = ++s_iTaskID;

   pTask->state = TASK_INIT;

#if defined( HB_HAS_UCONTEXT )
   /* create new execution context and initialize its private stack */
   if( getcontext( &pTask->context ) == -1 )
      hb_errInternal( HB_EI_ERRUNRECOV, "getcontext", NULL, NULL );
   pTask->context.uc_link          = NULL;
   pTask->context.uc_stack.ss_sp   = pTask->stack;
   pTask->context.uc_stack.ss_size = pTask->stack_size;
   makecontext( &pTask->context, hb_taskRun, 0 );
#endif

   return pTask;
}

#if ! defined( HB_HAS_UCONTEXT )
static void hb_taskStart( void )
{
   static jmp_buf context;

   s_currTask->state = TASK_RUNNING;

   /* create new execution context and initialize its private stack */
   if( setjmp( context ) == 0 )
   {
      HB_TASK_STACK_INIT( context, s_currTask->stack + s_currTask->stack_size );

      /* switch to new stack */
      longjmp( context, 1 );
   }

   hb_taskRun();
   /* unreachable code */
}
#endif



/* initialize task switching, create and register main task structure */
void hb_taskInit( void )
{
   if( s_iTaskID == 0 )
   {
      s_mainTask = s_currTask = ( PHB_TASKINFO )
         memset( hb_xgrab( sizeof( HB_TASKINFO ) ), 0, sizeof( HB_TASKINFO ) );
      /* main task uses default application stack */
      s_currTask->id = ++s_iTaskID;
      s_currTask->state = TASK_RUNNING;
      hb_taskLink( &s_taskList, s_currTask );
   }
}

/* uninitialize task switching, release main task structure */
void hb_taskExit( void )
{
   if( s_mainTask )
   {
      s_mainTask->state = TASK_ZOMBIE;
      hb_taskFree( s_mainTask );
      s_mainTask = s_currTask = NULL;
      s_iTaskID = 0;

      /* release all mutexes */
      while( s_mutexList )
      {
         PHB_TASKMTX pMutex = s_mutexList;
         s_mutexList = pMutex->next;
         hb_xfree( pMutex );
      }
      /* release all conditional variables */
      while( s_condList )
      {
         PHB_TASKCOND pCond = s_condList;
         s_condList = pCond->next;
         hb_xfree( pCond );
      }
   }
}

/* return main task pointer */
void * hb_taskMain( void )
{
   return s_mainTask;
}

/* return current task pointer */
void * hb_taskSelf( void )
{
   return s_currTask;
}

/* return given task number */
int hb_taskID( void * pTask )
{
   return ( ( PHB_TASKINFO ) pTask )->id;
}

/* get current task user data */
void * hb_taskGetData( void )
{
   return s_currTask->data;
}

/* set current task user data */
void hb_taskSetData( void * pData )
{
   s_currTask->data = pData;
}

/* get given task user data */
void * hb_taskRestoreData( void * pTask )
{
   return ( ( PHB_TASKINFO ) pTask )->data;
}

/* set given task user data */
void hb_taskSaveData( void * pTask, void * pData )
{
   ( ( PHB_TASKINFO ) pTask )->data = pData;
}

/* get result of task execution */
void * hb_taskResult( void * pTask )
{
   return ( ( PHB_TASKINFO ) pTask )->result;
}

void hb_taskSleep( unsigned long ulMilliSec )
{
   if( ulMilliSec > 0 )
      hb_taskSetSleep( s_currTask, ulMilliSec );

   hb_taskYield();
}

/* switch execution context to next task */
void hb_taskYield( void )
{
   PHB_TASKINFO pTask = s_currTask->pNext;

   while( pTask != s_currTask )
   {
      if( pTask->state == TASK_RUNNING || pTask->state == TASK_INIT )
         break;
      else if( pTask->state == TASK_ZOMBIE )
      {
         PHB_TASKINFO pFree = pTask;
         pTask = pTask->pNext;
         hb_taskFree( pFree );
      }
      else
      {
         if( pTask->state == TASK_SLEEPING && pTask == s_taskSleep )
         {
            if( s_taskSleep->wakeup == HB_TASK_NO_DELAY ||
                ( s_taskSleep->wakeup != HB_TASK_INFINITE_DELAY &&
                  s_taskSleep->wakeup <= hb_taskTimeStop( 0 ) ) )
            {
               s_taskSleep = pTask->pSleepNext;
               pTask->state = TASK_RUNNING;
               break;
            }
         }
         pTask = pTask->pNext;
      }
   }

   if( pTask->state != TASK_RUNNING && pTask->state != TASK_INIT )
   {
      if( s_taskSleep && s_taskSleep->wakeup != HB_TASK_INFINITE_DELAY )
      {
         hb_taskFreeze( s_taskSleep->wakeup );
         pTask = s_taskSleep;
         s_taskSleep = pTask->pSleepNext;
         pTask->state = TASK_RUNNING;
      }
      else
         hb_errInternal( HB_EI_ERRUNRECOV, "DEADLOCK", NULL, NULL );
   }

   hb_taskResume( pTask );
}

void hb_taskSheduler( void )
{
   /* The cost of accessing time in many systems is huge and causes
    * noticeable slowness so we not use it and switch task very often
    * on each call to hb_taskSheduler(). It also reduces the total
    * performance though the overhead is smaller.
    * The problem can be resolved in many system by chip access to some
    * counter updated by interrupt.
    * In DJGPP clock() seems to be such chip counter.
    */
#if defined( __DJGPP__ )
   static clock_t s_timer;
   clock_t timer = clock();
   if( s_timer != timer )
   {
      s_timer = timer;
      hb_taskYield();
   }
#else
   hb_taskYield();
#endif
}

/* suspend current task execution */
void hb_taskSuspend( void )
{
   s_currTask->state = TASK_SUSPEND;
   /* switch to next active task */
   hb_taskYield();
}

/* resume given task execution */
/* TODO: do not start task immediately */
void hb_taskResume( void * pTaskPtr )
{
   PHB_TASKINFO pTask = ( PHB_TASKINFO ) pTaskPtr, pCurrTask;

   if( s_currTask != pTask )
   {
      pCurrTask = s_currTask;
      switch( pTask->state )
      {
#if ! defined( HB_HAS_UCONTEXT )
         case TASK_INIT:
            /* save current execution context  */
            if( setjmp( s_currTask->context ) == 0 )
            {
               s_currTask = pTask;
               hb_taskStart();
               /* unreachable code */
            }
            break;
#endif
         case TASK_SLEEPING:
            hb_taskWakeUp( pTask );
            /* no break */
#if defined( HB_HAS_UCONTEXT )
         case TASK_INIT:
#endif
         case TASK_SUSPEND:
            pTask->state = TASK_RUNNING;
            /* no break */
         case TASK_RUNNING:
#if defined( HB_HAS_UCONTEXT )
            s_currTask = pTask;
            /* save current execution context and switch to the new one */
            swapcontext( &pCurrTask->context, &pTask->context );
#else
            /* save current execution context  */
            if( setjmp( s_currTask->context ) == 0 )
            {
               s_currTask = pTask;
               /* switch execution context */
               longjmp( pTask->context, 1 );
               /* unreachable code */
            }
#endif
            break;

         case TASK_DONE:
            break;

         case TASK_ZOMBIE:
            /* It should not happen - it's bug in user code */
            hb_errInternal( HB_EI_ERRUNRECOV, "TaskResume: zombie", NULL, NULL );
/*
         default:
            hb_errInternal( HB_EI_ERRUNRECOV, "TaskResume: corrupt", NULL, NULL );
 */
      }
   }
}

/* create new task */
void * hb_taskCreate( void * ( *start )( void * ), void * cargo, long stack_size )
{
   PHB_TASKINFO pTask;

   pTask = hb_taskNew( stack_size );

   pTask->start = start;
   pTask->cargo = cargo;

   hb_taskLink( &s_taskList, pTask );

   return pTask;
}

/* destroy given task */
void hb_taskDestroy( void * pTaskPtr )
{
   PHB_TASKINFO pTask = ( PHB_TASKINFO ) pTaskPtr;

   if( pTask != s_mainTask )
   {
      pTask->detached = HB_TRUE;
      if( pTask->state == TASK_ZOMBIE || pTask->state == TASK_DONE )
         hb_taskFree( pTask );
      else
         hb_taskFinalize( pTask );
   }
}

/* wait for given task termination */
int hb_taskJoin( void * pTaskPtr, unsigned long ulMilliSec, void ** pResult )
{
   PHB_TASKINFO pTask = ( PHB_TASKINFO ) pTaskPtr;
   int result = 0;

   if( pTask != s_mainTask && pTask != s_currTask )
   {
      if( ( pTask->state == TASK_INIT || pTask->state == TASK_RUNNING ) &&
          ulMilliSec > 0 )
      {
         s_currTask->joining = pTask;
         pTask->joiners++;

         hb_taskSleep( ulMilliSec );

         if( s_currTask->joining )
         {
            s_currTask->joining = NULL;
            pTask->joiners--;
         }
      }

      if( pTask->state == TASK_DONE )
      {
         if( pResult )
            *pResult = pTask->result;
         pTask->state = TASK_ZOMBIE;
         hb_taskFree( pTask );
         result = 1;
      }
   }
   return result;
}

/* detach given task - it will be removed automatically */
void hb_taskDetach( void * pTask )
{
   ( ( PHB_TASKINFO ) pTask )->detached = HB_TRUE;
}

/* current task quit */
void hb_taskQuit( void * result )
{
   if( s_currTask != s_mainTask )
   {
      s_currTask->result = result;
      hb_taskFinalize( s_currTask );
      /* switch to next active task */
      hb_taskYield();
      /* unreachable code */
   }
}

/* (try to) lock given mutex */
int hb_taskLock( void ** pMutexPtr, unsigned long ulMilliSec )
{
   PHB_TASKMTX pMutex;

   if( s_iTaskID == 0 )
      return 0;

   if( *pMutexPtr == NULL )
      *pMutexPtr = ( void * ) hb_taskMutexNew();

   pMutex = ( PHB_TASKMTX ) *pMutexPtr;
   if( pMutex->count == 0 )
   {
      s_currTask->locked++;
      pMutex->task = s_currTask;
      return ++pMutex->count;
   }
   else if( pMutex->task == s_currTask )
   {
      return ++pMutex->count;
   }

   if( ulMilliSec )
   {
      PHB_TASKINFO * pLockers = &pMutex->lockers;

      while( *pLockers )
         pLockers = &( *pLockers )->pBlockNext;
      *pLockers = s_currTask;
      s_currTask->pBlockNext = NULL;
      s_currTask->locking = pMutex;

      hb_taskSleep( ulMilliSec );

      if( s_currTask->locking )
      {
         pLockers = &pMutex->lockers;
         while( *pLockers )
         {
            if( *pLockers == s_currTask )
            {
               *pLockers = s_currTask->pBlockNext;
               s_currTask->locking = NULL;
               break;
            }
            else
               pLockers = &( *pLockers )->pBlockNext;
         }
#ifdef HB_TASK_DEBUG
         if( s_currTask->locking )
            hb_errInternal( HB_EI_ERRUNRECOV, "TaskLock: dummy lock", NULL, NULL );
#endif
      }
   }

   return pMutex->task == s_currTask ? pMutex->count : 0;
}

/* unlock given mutex */
void hb_taskUnlock( void ** pMutexPtr )
{
   PHB_TASKMTX pMutex = ( PHB_TASKMTX ) *pMutexPtr;

   if( pMutex && pMutex->task == s_currTask )
   {
      if( --pMutex->count == 0 )
      {
         pMutex->task = NULL;
         s_currTask->locked--;
         if( hb_taskSwitchLock( pMutex ) )
            hb_taskYield();
      }
   }
}

void hb_taskSignal( void ** pCondPtr )
{
   PHB_TASKCOND pCond;

   if( *pCondPtr == NULL )
      *pCondPtr = ( void * ) hb_taskCondNew();

   pCond = ( PHB_TASKCOND ) *pCondPtr;

   if( pCond->waiters )
   {
      PHB_TASKINFO * pLockers = &pCond->mutex->lockers, pTask;

      while( *pLockers )
         pLockers = &( *pLockers )->pBlockNext;

      pTask = pCond->waiters;

#ifdef HB_TASK_DEBUG
      if( pTask->waiting != pCond )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskSignal: broken cond", NULL, NULL );
#endif
      pTask->waiting = NULL;
      pCond->waiters = pTask->pWaitNext;

      pTask->locking = pCond->mutex;
      pTask->pBlockNext = NULL;
      *pLockers = pTask;

      if( pCond->mutex->count == 0 )
         hb_taskSwitchLock( pCond->mutex );
   }
}

void hb_taskBroadcast( void ** pCondPtr )
{
   PHB_TASKCOND pCond;

   if( *pCondPtr == NULL )
      *pCondPtr = ( void * ) hb_taskCondNew();

   pCond = ( PHB_TASKCOND ) *pCondPtr;

   if( pCond->waiters )
   {
      PHB_TASKINFO * pLockers = &pCond->mutex->lockers;

      while( *pLockers )
         pLockers = &( *pLockers )->pBlockNext;

      do
      {
         PHB_TASKINFO pTask = pCond->waiters;

#ifdef HB_TASK_DEBUG
         if( pTask->waiting != pCond )
            hb_errInternal( HB_EI_ERRUNRECOV, "TaskBroadcast: broken cond", NULL, NULL );
#endif
         pTask->waiting = NULL;
         pCond->waiters = pTask->pWaitNext;

         pTask->locking = pCond->mutex;
         pTask->pBlockNext = NULL;
         *pLockers = pTask;
         pLockers = &pTask->pBlockNext;
      }
      while( pCond->waiters );

      if( pCond->mutex->count == 0 )
         hb_taskSwitchLock( pCond->mutex );
   }
}

int hb_taskWait( void ** pCondPtr, void ** pMutexPtr, unsigned long ulMilliSec )
{
   PHB_TASKINFO * pWaiters;
   PHB_TASKMTX pMutex = ( PHB_TASKMTX ) *pMutexPtr;
   PHB_TASKCOND pCond;
   int iCount;

   if( pMutex == NULL )
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskWait: no mutex", NULL, NULL );

   if( pMutex->count == 0 || pMutex->task != s_currTask )
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskWait: no mutex lock", NULL, NULL );

   if( *pCondPtr == NULL )
      *pCondPtr = ( void * ) hb_taskCondNew();

   pCond = ( PHB_TASKCOND ) *pCondPtr;

   /* POSIX threads have such condition */
   if( pCond->waiters && pCond->mutex != pMutex )
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskWait: wrong mutex", NULL, NULL );

   /* add task to conditional variable waiting queue */
   pWaiters = &pCond->waiters;
   while( *pWaiters )
      pWaiters = &( *pWaiters )->pWaitNext;
   *pWaiters = s_currTask;
   s_currTask->pWaitNext = NULL;
   s_currTask->waiting = pCond;

   /* unlock mutex with recursive locks if any */
   pCond->mutex = pMutex;
   iCount = pMutex->count;
   pMutex->task = NULL;
   pMutex->count = 0;
   s_currTask->locked--;
   hb_taskSwitchLock( pMutex );

   hb_taskSleep( ulMilliSec );

   if( ! hb_taskLock( pMutexPtr, HB_TASK_INFINITE_WAIT ) )
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskWait: lock fail", NULL, NULL );

   pMutex->count = iCount;

   if( s_currTask->waiting )
   {
      pWaiters = &pCond->waiters;
      while( *pWaiters )
      {
         if( *pWaiters == s_currTask )
         {
            *pWaiters = s_currTask->pWaitNext;
            s_currTask->waiting = NULL;
            break;
         }
         else
            pWaiters = &( *pWaiters )->pWaitNext;
      }
#ifdef HB_TASK_DEBUG
      if( s_currTask->waiting )
         hb_errInternal( HB_EI_ERRUNRECOV, "TaskWait: dummy cond", NULL, NULL );
#endif
      return 0;
   }
   else
      return 1;
}

void hb_taskDestroyMutex( void ** pMutexPtr )
{
   PHB_TASKMTX pMutex = ( PHB_TASKMTX ) *pMutexPtr;

   if( pMutex )
   {
      PHB_TASKMTX * pMutexLst = &s_mutexList;

      while( *pMutexLst )
      {
         if( *pMutexLst == pMutex )
         {
            *pMutexLst = pMutex->next;
            if( pMutex->count )
               hb_errInternal( HB_EI_ERRUNRECOV, "TaskDestroyMutex: locked", NULL, NULL );
            else if( pMutex->lockers )
               hb_errInternal( HB_EI_ERRUNRECOV, "TaskDestroyMutex: lockers", NULL, NULL );
            hb_xfree( pMutex );
            return;
         }
         pMutexLst = &( *pMutexLst )->next;
      }
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskDestroyMutex: not a mutex", NULL, NULL );
   }
}

void hb_taskDestroyCond( void ** pCondPtr )
{
   PHB_TASKCOND pCond = ( PHB_TASKCOND ) *pCondPtr;

   if( pCond )
   {
      PHB_TASKCOND * pCondLst = &s_condList;

      while( *pCondLst )
      {
         if( *pCondLst == pCond )
         {
            *pCondLst = pCond->next;
            if( pCond->waiters )
               hb_errInternal( HB_EI_ERRUNRECOV, "TaskDestroyCond: waiters", NULL, NULL );
            hb_xfree( pCond );
            return;
         }
         pCondLst = &( *pCondLst )->next;
      }
      hb_errInternal( HB_EI_ERRUNRECOV, "TaskDestroyCond: not a cond", NULL, NULL );
   }
}
