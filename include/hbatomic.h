/*
 * Harbour Project source code:
 *    header file with functions for atomic operations
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#ifndef HB_ATOMIC_H_
#define HB_ATOMIC_H_

#include "hbdefs.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#elif defined( HB_OS_DARWIN )
#  include <libkern/OSAtomic.h>
#elif defined( HB_OS_SUNOS )
#  include <atomic.h>
#endif
#if defined( __SVR4 )
#  include <thread.h>
#endif
#if defined( HB_OS_UNIX ) && !( defined( __WATCOMC__ ) || defined( HB_OS_MINIX ) )
#  include <sched.h>
#endif


HB_EXTERN_BEGIN

/* yield the processor */
#if defined( HB_TASK_THREAD )
#  define HB_SCHED_YIELD()    hb_taskYield()
#elif defined( HB_OS_WIN )
#  define HB_SCHED_YIELD()    Sleep( 0 )
#elif defined( HB_OS_OS2 )
#  define HB_SCHED_YIELD()    DosSleep( 0 )
#elif defined( HB_OS_SUNOS ) || defined( __SVR4 )
#  define HB_SCHED_YIELD()    thr_yield()
#elif defined( HB_OS_UNIX )
#  define HB_SCHED_YIELD()    sched_yield()
#else
#  define HB_SCHED_YIELD()    sleep( 0 );
#endif


/* Inline assembler version of atomic operations on memory reference counters */
#if defined( __GNUC__ )

#  if defined( HB_USE_GCCATOMIC_OFF )
#     undef HB_USE_GCCATOMIC
#  elif ( ( __GNUC__ > 4 ) || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 1) ) && \
        ! defined( __MINGW32CE__ ) && ! defined( HB_USE_GCCATOMIC )
#     define HB_USE_GCCATOMIC
#  endif

#  if defined( HB_USE_GCCATOMIC )

#     define HB_ATOM_INC( p )       __sync_add_and_fetch( (p), 1 )
#     define HB_ATOM_DEC( p )       __sync_sub_and_fetch( (p), 1 )
#     define HB_ATOM_GET( p )       ( *(p) )
#     define HB_ATOM_SET( p, n )    do { *(p) = (n); } while(0)

      static __inline__ void hb_spinlock_acquire( int * l )
      {
         for( ;; )
         {
            if( ! __sync_lock_test_and_set( l, 1 ) )
               return;

            #ifdef HB_SPINLOCK_REPEAT
               if( ! __sync_lock_test_and_set( l, 1 ) )
                  return;
            #endif
            HB_SCHED_YIELD();
         }
      }

#     define HB_SPINLOCK_T          int
#     define HB_SPINLOCK_INIT       0
#     define HB_SPINLOCK_TRY(l)     (__sync_lock_test_and_set(l, 1)==0)
#     define HB_SPINLOCK_RELEASE(l) __sync_lock_release(l)
#     define HB_SPINLOCK_ACQUIRE(l) hb_spinlock_acquire(l)

#  elif defined( HB_CPU_X86 ) || defined( HB_CPU_X86_64 )

#     if HB_COUNTER_SIZE == 4

         static __inline__ void hb_atomic_inc32( volatile int * p )
         {
            __asm__ __volatile__(
               "lock; incl %0\n"
               :"=m" (*p) :"m" (*p)
            );
         }

         static __inline__ int hb_atomic_dec32( volatile int * p )
         {
            unsigned char c;
            __asm__ __volatile__(
               "lock; decl %0\n"
               "sete %1\n"
               :"=m" (*p), "=qm" (c) :"m" (*p) : "memory"
            );
            return c == 0;
         }

#        define HB_ATOM_INC( p )    ( hb_atomic_inc32( ( volatile int * ) (p) ) )
#        define HB_ATOM_DEC( p )    ( hb_atomic_dec32( ( volatile int * ) (p) ) )
#        define HB_ATOM_GET( p )    (*(int volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif HB_COUNTER_SIZE == 8

         static __inline__ void hb_atomic_inc64( volatile long long int * p )
         {
            __asm__ __volatile__(
               "lock; incq %0\n"
               :"=m" (*p) :"m" (*p)
            );
         }

         static __inline__ int hb_atomic_dec64( volatile long long int * p )
         {
            unsigned char c;
            __asm__ __volatile__(
               "lock; decq %0\n"
               "sete %1\n"
               :"=m" (*p), "=qm" (c) :"m" (*p) : "memory"
            );
            return c == 0;
         }

#        define HB_ATOM_INC( p )    ( hb_atomic_inc64( ( volatile long long int * ) (p) ) )
#        define HB_ATOM_DEC( p )    ( hb_atomic_dec64( ( volatile long long int * ) (p) ) )
#        define HB_ATOM_GET( p )    (*(long long int volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((long long int volatile *)(p)) = (n); } while(0)

#     endif

      static __inline__ int hb_spinlock_trylock( volatile int * p )
      {
         int i = 1;
         __asm__ __volatile__(
            "xchgl %0, %1\n\t"
            : "=r" (i)
            : "m" (*p), "0" (i)
            : "memory"
         );
         return i;
      }

      static __inline__ void hb_spinlock_acquire( volatile int * l )
      {
         for( ;; )
         {
            if( ! hb_spinlock_trylock( l ) )
               return;
            #ifdef HB_SPINLOCK_REPEAT
               if( ! hb_spinlock_trylock( l ) )
                  return;
            #endif
            HB_SCHED_YIELD();
         }
      }

      static __inline__ void hb_spinlock_release( volatile int * l )
      {
         *l = 0;
      }

#     define HB_SPINLOCK_T          volatile int
#     define HB_SPINLOCK_INIT       0
#     define HB_SPINLOCK_TRY(l)     (hb_spinlock_trylock(l)==0)
#     define HB_SPINLOCK_RELEASE(l) hb_spinlock_release(l)
#     define HB_SPINLOCK_ACQUIRE(l) hb_spinlock_acquire(l)

#  elif defined( HB_CPU_PPC )

#     if HB_COUNTER_SIZE == 4

         static __inline__ void hb_atomic_inc32( volatile int * p )
         {
            int i;

            __asm__ __volatile__(
               "1:   lwarx    %0,0,%2\n\t"
               "     addic    %0,%0,1\n\t"
               "     stwcx.   %0,0,%2\n\t"
               "     bne-     1b\n\t"
               : "=&r" (i), "=m" (*p) : "r" (p), "m" (*p) : "cc"
            );
         }

         static __inline__ int hb_atomic_dec32( volatile int * p )
         {
            int i;

            __asm__ __volatile__(
               "1:   lwarx    %0,0,%1\n\t"
               "     addic    %0,%0,-1\n\t"
               "     stwcx.   %0,0,%1\n\t"
               "     bne-     1b\n\t"
               "     isync\n\t"
               : "=&r" (i) : "r" (p) : "cc", "memory"
            );
            return i;
         }

#        define HB_ATOM_INC( p )    ( hb_atomic_inc32( ( volatile int * ) (p) ) )
#        define HB_ATOM_DEC( p )    ( hb_atomic_dec32( ( volatile int * ) (p) ) )
#        define HB_ATOM_GET( p )    (*(int volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif HB_COUNTER_SIZE == 8

         /* TODO: */

#     endif

#  endif  /* ???CPU?? */

#elif defined( _MSC_VER )

#  if defined( HB_CPU_X86 )

#     if HB_COUNTER_SIZE == 4

         static __inline void hb_atomic_inc32( volatile int * p )
         {
            __asm mov eax, p
            __asm lock inc dword ptr [eax]
         }

         static __inline int hb_atomic_dec32( volatile int * p )
         {
            unsigned char c;

            __asm mov eax, p
            __asm lock dec dword ptr [eax]
            __asm setne c

            return c;
         }

#        define HB_ATOM_INC( p )    ( hb_atomic_inc32( ( volatile int * ) (p) ) )
#        define HB_ATOM_DEC( p )    ( hb_atomic_dec32( ( volatile int * ) (p) ) )
#        define HB_ATOM_GET( p )    (*(int volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif HB_COUNTER_SIZE == 8

         /* TODO: */

#     endif

#  endif    /* x86 */

#elif defined( __WATCOMC__ )

#  if defined( HB_CPU_X86 ) || defined( HB_CPU_X86_64 )

#     if HB_COUNTER_SIZE == 4

         void hb_atomic_inc32( volatile int * p );
         #pragma aux hb_atomic_inc32 = \
               "lock inc dword ptr [eax]" \
               parm [ eax ] modify exact [] ;

         unsigned char hb_atomic_dec32( volatile int * p );
         #pragma aux hb_atomic_dec32 = \
               "lock dec dword ptr [eax]", \
               "setne al" \
               parm [ eax ] value [ al ] modify exact [ al ] ;

#        define HB_ATOM_INC( p )    ( hb_atomic_inc32( ( volatile int * ) (p) ) )
#        define HB_ATOM_DEC( p )    ( hb_atomic_dec32( ( volatile int * ) (p) ) )
#        define HB_ATOM_GET( p )    (*(int volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((int volatile *)(p)) = (n); } while(0)

#     elif HB_COUNTER_SIZE == 8

         /* TODO: */

#     endif

      int hb_spinlock_trylock( volatile int * p );
      #pragma aux hb_spinlock_trylock = \
            "mov eax, 1", \
            "xchg eax, dword ptr [edx]" \
            parm [ edx ] value [ eax ] modify exact [ eax ] ;

      static __inline void hb_spinlock_acquire( volatile int * l )
      {
         for( ;; )
         {
            if( ! hb_spinlock_trylock( l ) )
               return;

            #ifdef HB_SPINLOCK_REPEAT
               if( ! hb_spinlock_trylock( l ) )
                  return;
            #endif
            HB_SCHED_YIELD();
         }
      }

      static __inline void hb_spinlock_release( volatile int * l )
      {
         *l = 0;
      }

#     define HB_SPINLOCK_T          volatile int
#     define HB_SPINLOCK_INIT       0
#     define HB_SPINLOCK_TRY(l)     (hb_spinlock_trylock(l)==0)
#     define HB_SPINLOCK_RELEASE(l) hb_spinlock_release(l)
#     define HB_SPINLOCK_ACQUIRE(l) hb_spinlock_acquire(l)

#  endif    /* x86 */

#endif  /* ??? C compiler ??? */


#if defined( HB_OS_WIN )

   /* Atomic operations on memory reference counters */
#  if ! defined( HB_ATOM_INC ) || ! defined( HB_ATOM_DEC )
#     undef HB_ATOM_DEC
#     undef HB_ATOM_INC
#     undef HB_ATOM_GET
#     undef HB_ATOM_SET
#     if HB_COUNTER_SIZE == 8
#        define HB_ATOM_INC( p )    (InterlockedIncrement64((LONGLONG *)(p)))
#        define HB_ATOM_DEC( p )    (InterlockedDecrement64((LONGLONG *)(p)))
#        define HB_ATOM_GET( p )    (*(LONGLONG volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { (*(LONGLONG volatile *)(p)) = (n); } while(0)
#     else
#        define HB_ATOM_INC( p )    (InterlockedIncrement((LONG *)(p)))
#        define HB_ATOM_DEC( p )    (InterlockedDecrement((LONG *)(p)))
#        define HB_ATOM_GET( p )    (*(LONG volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { (*(LONG volatile *)(p)) = (n); } while(0)
#     endif
#  endif

   /* Spin locks */
#  if ! defined( HB_SPINLOCK_T )
#     define HB_SPINLOCK_T          volatile LONG
#     define HB_SPINLOCK_INIT       0
#     define HB_SPINLOCK_TRY(l)     (! InterlockedExchange( (LONG*)(l), 1 ))
#     define HB_SPINLOCK_RELEASE(l) ( *(l) = 0 )
#  endif

#elif defined( HB_OS_DARWIN )

   /* Atomic operations on memory reference counters */
#  if ! defined( HB_ATOM_INC ) || ! defined( HB_ATOM_DEC )
#     undef HB_ATOM_DEC
#     undef HB_ATOM_INC
#     undef HB_ATOM_GET
#     undef HB_ATOM_SET
#     if HB_COUNTER_SIZE == 8
#        define HB_ATOM_INC( p )    (OSAtomicIncrement64((int64_t *)(p)))
#        define HB_ATOM_DEC( p )    (OSAtomicDecrement64((int64_t *)(p)))
#        define HB_ATOM_GET( p )    (*(int64_t volatile *)(p))
#        define HB_ATOM_SET( p, n ) do { *((int64_t volatile *)(p)) = (n); } while(0)
#     else
#        define HB_ATOM_INC( p )    (OSAtomicIncrement32((int32_t *)(p)))
#        define HB_ATOM_DEC( p )    (OSAtomicDecrement32((int32_t *)(p)))
#        define HB_ATOM_GET( p )    (*(volatile int32_t *)(p))
#        define HB_ATOM_SET( p, n ) do { *((volatile int32_t *)(p)) = (n); } while(0)
#     endif
#  endif

   /* Spin locks */
#  if ! defined( HB_SPINLOCK_T ) || 1 /* <= force using OSSpinLock */
#     undef HB_SPINLOCK_T
#     undef HB_SPINLOCK_INIT
#     undef HB_SPINLOCK_TRY
#     undef HB_SPINLOCK_RELEASE
#     undef HB_SPINLOCK_ACQUIRE
#     define HB_SPINLOCK_T          OSSpinLock
#     define HB_SPINLOCK_INIT       OS_SPINLOCK_INIT
#     define HB_SPINLOCK_TRY(l)     OSSpinLockTry(l)
#     define HB_SPINLOCK_RELEASE(l) OSSpinLockUnlock(l)
#     define HB_SPINLOCK_ACQUIRE(l) OSSpinLockLock(l)
#  endif

#elif defined( HB_OS_SUNOS )

   /* Atomic operations on memory reference counters */
#  if ! defined( HB_ATOM_INC ) || ! defined( HB_ATOM_DEC )
#     undef HB_ATOM_DEC
#     undef HB_ATOM_INC
#     undef HB_ATOM_GET
#     undef HB_ATOM_SET
#     define HB_ATOM_INC( p )    atomic_inc_ulong((ulong_t *)(p))
#     define HB_ATOM_DEC( p )    atomic_dec_ulong_nv((ulong_t *)(p))
#     define HB_ATOM_GET( p )    (*(ulong_t volatile *)(p))
#     define HB_ATOM_SET( p, n ) do { *((ulong_t volatile *)(p)) = (n); } while(0)
#  endif

   /* Spin locks */
#  if ! defined( HB_SPINLOCK_T )
#     define HB_SPINLOCK_T          volatile uint_t
#     define HB_SPINLOCK_INIT       0
#     define HB_SPINLOCK_TRY(l)     ( ! atomic_swap_uint( (l), 1 ) )
#     define HB_SPINLOCK_RELEASE(l) ( *(l) = 0 )
#  endif

#endif  /* HB_OS_??? */

#if defined( HB_SPINLOCK_T )
#  if ! defined( HB_SPINLOCK_ACQUIRE )
#     ifdef HB_SPINLOCK_REPEAT
#        define HB_SPINLOCK_ACQUIRE(l) do { \
                                          if( HB_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          if( HB_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          HB_SCHED_YIELD(); \
                                       } while(1)
#     else
#        define HB_SPINLOCK_ACQUIRE(l) do { \
                                          if( HB_SPINLOCK_TRY( l ) ) \
                                             break; \
                                          HB_SCHED_YIELD(); \
                                       } while(1)
#     endif
#  endif
#  if ! defined( HB_SPINLOCK_R )
      struct hb_spinlock_r
      {
         HB_SPINLOCK_T  lock;
         unsigned int   count;
         HB_THREAD_ID   thid;
      };

      static HB_FORCEINLINE void hb_spinlock_release_r( struct hb_spinlock_r * sl )
      {
         if( --sl->count == 0 )
         {
            sl->thid = 0;
            HB_SPINLOCK_RELEASE( &sl->lock );
         }
      }

      static HB_FORCEINLINE int hb_spinlock_try_r( struct hb_spinlock_r * sl )
      {
         HB_SPINLOCK_T * l = &sl->lock;
         int r = 0;
         if( *l != HB_SPINLOCK_INIT )
         {
            if( sl->thid == HB_THREAD_SELF() )
            {
               sl->count++;
               r = 1;
            }
         }
         else if( HB_SPINLOCK_TRY( l ) )
         {
            sl->thid = HB_THREAD_SELF();
            sl->count = 1;
            r = 1;
         }
         return r;
      }

#     ifndef HB_SPINLOCK_REPEAT
#        define HB_SPINLOCK_REPEAT     63
#     endif

/* workaround for borland C/C++ compiler limitation */
#if defined( __BORLANDC__ )
#     define hb_spinlock_acquire_r( sl ) \
      do { \
         HB_SPINLOCK_T * l = &(sl)->lock; \
         int count = HB_SPINLOCK_REPEAT; \
         for( ;; ) \
         { \
            if( *l != HB_SPINLOCK_INIT ) \
            { \
               if( (sl)->thid == HB_THREAD_SELF() ) \
               { \
                  (sl)->count++; \
                  break; \
               } \
            } \
            else if( HB_SPINLOCK_TRY( l ) ) \
            { \
               (sl)->thid = HB_THREAD_SELF(); \
               (sl)->count = 1; \
               break; \
            } \
            if( --count == 0 ) \
            { \
               HB_SCHED_YIELD(); \
               count = HB_SPINLOCK_REPEAT; \
            } \
         } \
      } while( 0 )
#else
      static HB_FORCEINLINE void hb_spinlock_acquire_r( struct hb_spinlock_r * sl )
      {
         HB_SPINLOCK_T * l = &sl->lock;
         int count = HB_SPINLOCK_REPEAT;
         for( ;; )
         {
            if( *l != HB_SPINLOCK_INIT )
            {
               if( sl->thid == HB_THREAD_SELF() )
               {
                  sl->count++;
                  break;
               }
            }
            else if( HB_SPINLOCK_TRY( l ) )
            {
               sl->thid = HB_THREAD_SELF();
               sl->count = 1;
               break;
            }
            if( --count == 0 )
            {
               HB_SCHED_YIELD();
               count = HB_SPINLOCK_REPEAT;
            }
         }
      }
#endif

#     define HB_SPINLOCK_R             struct hb_spinlock_r
#     define HB_SPINLOCK_INITVAL_R     { 0, 0, 0 }
#     define HB_SPINLOCK_INIT_R(l)     do { (l)->lock = 0; (l)->count = 0; (l)->thid = 0; } while( 0 )
#     define HB_SPINLOCK_TRY_R(l)      hb_spinlock_try_r(l)
#     define HB_SPINLOCK_RELEASE_R(l)  hb_spinlock_release_r(l)
#     define HB_SPINLOCK_ACQUIRE_R(l)  hb_spinlock_acquire_r(l)
#  endif /* ! HB_SPINLOCK_R */
#endif /* HB_SPINLOCK_T */

HB_EXTERN_END

#endif /* HB_ATOMIC_H_ */
