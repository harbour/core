/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Fixed Memory API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_xquery()
 *
 * See COPYING for licensing terms.
 *
 */

/* NOTE: This definitions must be ahead of any and all #include statements */

#if !defined( HB_FM_STATISTICS ) && \
    !defined( HB_FM_STATISTICS_OFF ) && \
    !defined( HB_FM_STATISTICS_DYN_OFF )
#  define HB_FM_STATISTICS_OFF
#endif

/* For Linux and mremap() function */
#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

/* NOTE: Need to have these before Harbour headers,
         because in MT mode, they will automatically #include <os2.h>. */
#define INCL_BASE

/* malloc.h has been obsoleted by stdlib.h, which is included via
   hbvmpub.h, which is include via hbapi.h
   #include <malloc.h>
*/

#define HB_STACK_PRELOAD

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbmemory.ch"
#include "hbdate.h"
#include "hbset.h"
#include "hbvm.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#elif defined( HB_OS_DOS )
#  include <dos.h>
#endif

#if defined( HB_MT_VM )
#  include "hbthread.h"
#  include "hbatomic.h"
#endif

#if defined( HB_FM_STD_ALLOC )
#  undef HB_FM_DL_ALLOC
#  undef HB_FM_DLMT_ALLOC
#  undef HB_FM_WIN_ALLOC
#elif defined( HB_FM_WIN_ALLOC )
#  undef HB_FM_DL_ALLOC
#elif !defined( HB_FM_DL_ALLOC ) && !defined( HB_FM_WIN_ALLOC )
#  if defined( HB_OS_WIN_CE )
      /* In WinCE builds DLMALLOC creates problems when allocated
       * memory is used in file IO operations.
       */
#     define HB_FM_STD_ALLOC
#  elif defined( _MSC_VER ) || defined( __BORLANDC__ ) || defined( __MINGW32__ ) || \
        ( defined( __WATCOMC__ ) && defined( HB_OS_WIN ) ) || \
        defined( HB_OS_OS2 ) || \
        ( defined( HB_FM_DLMT_ALLOC ) && defined( HB_MT_VM ) )
#     define HB_FM_DL_ALLOC
#  else
      /* #define HB_FM_DL_ALLOC */
#  endif
#endif

#if defined( HB_FM_STATISTICS_OFF )
#  undef HB_FM_STATISTICS
#endif


/* #define HB_FM_WIN_ALLOC */
/* #define HB_FM_STATISTICS */
/* #define HB_PARANOID_MEM_CHECK */

#if defined( HB_FM_DL_ALLOC )
#  if !defined( HB_FM_DLMT_ALLOC ) && !defined( HB_FM_DLMT_ALLOC_OFF ) && \
      defined( HB_MT_VM )
#     if !( defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) && ( _MSC_VER <= 1500 ) ) )
#        define HB_FM_DLMT_ALLOC
#     endif
#  endif
/* #  define NO_MALLINFO 1 */
/* #  define INSECURE */
/* #  define USE_DL_PREFIX */
#  undef FORCEINLINE
#  if !defined( FORCEINLINE )
#     define FORCEINLINE HB_FORCEINLINE
#  endif
#  define REALLOC_ZERO_BYTES_FREES
#  if defined( HB_MT_VM )
#     if defined( HB_SPINLOCK_R )
#        define USE_LOCKS  2
#     else
#        define USE_LOCKS  1
#     endif
#     if defined( HB_FM_DLMT_ALLOC )
#        define ONLY_MSPACES  1
#        define FOOTERS       1
#     endif
#  else
#     undef HB_FM_DLMT_ALLOC
#  endif
#  if defined( __BORLANDC__ )
#     pragma warn -aus
#     pragma warn -ccc
#     pragma warn -eff
#     pragma warn -ngu
#     pragma warn -prc
#     pragma warn -rch
#     pragma warn -inl
#  elif defined( HB_OS_WIN_CE ) && defined( __POCC__ )
#     define ABORT TerminateProcess( GetCurrentProcess(), 0 )
#  elif defined( __POCC__ ) && !defined( InterlockedCompareExchangePointer )
#     define InterlockedCompareExchangePointer
#  elif defined( __WATCOMC__ )
#     pragma warning 13 9
#     pragma warning 367 9
#     pragma warning 368 9
#     pragma warning 887 9
#     pragma disable_message ( 201 )
#     if !defined( USE_DL_PREFIX ) && !defined( HB_FM_DLMT_ALLOC )
#        define USE_DL_PREFIX
#     endif
#  elif defined( _MSC_VER )
#     if !defined( USE_DL_PREFIX ) && !defined( HB_FM_DLMT_ALLOC )
#        define USE_DL_PREFIX
#     endif
#     if defined( HB_OS_WIN_CE )
#        define ABORT TerminateProcess( GetCurrentProcess(), 0 )
#        define LACKS_FCNTL_H
#     endif
#     pragma warning( push )
#     pragma warning( disable : 4702 )
#     if defined( HB_OS_WIN_64 )
#        pragma warning( disable : 4267 )
#     endif
#  elif defined( __MINGW32__ )
#     if !defined( USE_DL_PREFIX ) && !defined( HB_FM_DLMT_ALLOC )
#        define USE_DL_PREFIX
#     endif
#  endif
#  if defined( __cplusplus ) && ! defined( USE_DL_PREFIX )
#     define USE_DL_PREFIX
#  endif
#  include "dlmalloc.c"
#  if defined( __BORLANDC__ )
#     pragma warn +aus
#     pragma warn +ccc
#     pragma warn +eff
#     pragma warn +ngu
#     pragma warn +prc
#     pragma warn +rch
#     pragma warn +inl
#  elif defined( __WATCOMC__ )
#     pragma warning 13 2
#     pragma warning 367 2
#     pragma warning 368 2
#     pragma warning 887 2
#     pragma warning 887 2
#     pragma enable_message ( 201 )
#  elif defined( _MSC_VER )
#     pragma warning( pop )
#  endif
#  if defined( HB_FM_DLMT_ALLOC )
#     define malloc( n )         mspace_malloc( hb_mspace(), ( n ) )
#     define realloc( p, n )     mspace_realloc( NULL, ( p ), ( n ) )
#     define free( p )           mspace_free( NULL, ( p ) )
#  elif defined( USE_DL_PREFIX )
#     define malloc( n )         dlmalloc( ( n ) )
#     define realloc( p, n )     dlrealloc( ( p ), ( n ) )
#     define free( p )           dlfree( ( p ) )
#  endif
#else
#  undef HB_FM_DLMT_ALLOC
#  if defined( HB_FM_WIN_ALLOC ) && defined( HB_OS_WIN )
#     if defined( HB_FM_LOCALALLOC )
#        define malloc( n )      ( void * ) LocalAlloc( LMEM_FIXED, ( n ) )
#        define realloc( p, n )  ( void * ) LocalReAlloc( ( HLOCAL ) ( p ), ( n ), LMEM_MOVEABLE )
#        define free( p )        LocalFree( ( HLOCAL ) ( p ) )
#     else
         static HANDLE s_hProcessHeap = NULL;
#        define HB_FM_NEED_INIT
#        define HB_FM_HEAP_INIT
#        define malloc( n )      ( void * ) HeapAlloc( s_hProcessHeap, 0, ( n ) )
#        define realloc( p, n )  ( void * ) HeapReAlloc( s_hProcessHeap, 0, ( void * ) ( p ), ( n ) )
#        define free( p )        HeapFree( s_hProcessHeap, 0, ( void * ) ( p ) )
#     endif
#  endif
#endif

#if defined( HB_MT_VM ) && \
    ( defined( HB_FM_STATISTICS ) || defined( HB_FM_DLMT_ALLOC ) || \
      !defined( HB_ATOM_INC ) || !defined( HB_ATOM_DEC ) )

   static HB_CRITICAL_NEW( s_fmMtx );
#  define HB_FM_LOCK()        hb_threadEnterCriticalSection( &s_fmMtx )
#  define HB_FM_UNLOCK()      hb_threadLeaveCriticalSection( &s_fmMtx )

#else

#  define HB_FM_LOCK()
#  define HB_FM_UNLOCK()

#endif

#if defined( HB_FM_STATISTICS )
#  if !defined( HB_FM_NEED_INIT )
#     define HB_FM_NEED_INIT
#  endif
#else
#  undef HB_PARANOID_MEM_CHECK
#endif

#ifdef HB_FM_NEED_INIT
static HB_BOOL s_fInitedFM = HB_FALSE;
#endif

#ifdef HB_FM_STATISTICS

#ifndef HB_MEMFILER
#  define HB_MEMFILER  0xff
#endif
#define HB_MEMINFO_SIGNATURE 0x19730403

typedef struct _HB_MEMINFO
{
   HB_U32      u32Signature;
   HB_SIZE     nSize;
   HB_USHORT   uiProcLine;
   char        szProcName[ HB_SYMBOL_NAME_LEN + 1 ];
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define _HB_MEMINFO_SIZE    ( ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                                  ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT ) + \
                                HB_COUNTER_OFFSET )
#else
#  define _HB_MEMINFO_SIZE    ( sizeof( HB_MEMINFO ) + HB_COUNTER_OFFSET )
#endif

#define HB_MEMINFO_SIZE       ( s_fStatistic ? sizeof( HB_MEMINFO ) + HB_COUNTER_OFFSET : HB_COUNTER_OFFSET )

#define HB_FM_GETSIG( p, n )  HB_GET_UINT32( ( HB_BYTE * ) ( p ) + ( n ) )
#define HB_FM_SETSIG( p, n )  HB_PUT_UINT32( ( HB_BYTE * ) ( p ) + ( n ), HB_MEMINFO_SIGNATURE )
#define HB_FM_CLRSIG( p, n )  HB_PUT_UINT32( ( HB_BYTE * ) ( p ) + ( n ), 0 )

#define HB_ALLOC_SIZE( n )    ( ( n ) + ( s_fStatistic ? _HB_MEMINFO_SIZE + sizeof( HB_U32 ) : HB_COUNTER_OFFSET ) )
#define HB_FM_PTR( p )        ( ( PHB_MEMINFO ) ( ( HB_BYTE * ) ( p ) - HB_MEMINFO_SIZE ) )

#define HB_FM_BLOCKSIZE( p )  ( s_fStatistic ? HB_FM_PTR( pMem )->nSize : 0 )

/* NOTE: we cannot use here HB_TRACE because it will overwrite the
 * function name/line number of code which called hb_xalloc/hb_xgrab
 */
#define HB_TRACE_FM           HB_TRACE_STEALTH

static HB_BOOL s_fStatistic = HB_FALSE;

static HB_ISIZ s_nMemoryBlocks = 0;      /* memory blocks used */
static HB_ISIZ s_nMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static HB_ISIZ s_nMemoryMaxConsumed = 0; /* memory size consumed */
static HB_ISIZ s_nMemoryConsumed = 0;    /* memory max size consumed */

static PHB_MEMINFO s_pFirstBlock = NULL;
static PHB_MEMINFO s_pLastBlock = NULL;

static char s_szFileName[ HB_PATH_MAX ] = { '\0' };
static char s_szInfo[ 256 ] = { '\0' };

#else /* ! HB_FM_STATISTICS */

typedef void * PHB_MEMINFO;
#define HB_MEMINFO_SIZE       HB_COUNTER_OFFSET
#define HB_ALLOC_SIZE( n )    ( ( n ) + HB_MEMINFO_SIZE )
#define HB_FM_PTR( p )        HB_COUNTER_PTR( p )
#define HB_TRACE_FM           HB_TRACE

#endif /* HB_FM_STATISTICS */

#define HB_MEM_PTR( p )       ( ( void * ) ( ( HB_BYTE * ) ( p ) + HB_MEMINFO_SIZE ) )


#if !defined( HB_MT_VM )

#  undef HB_ATOM_DEC
#  undef HB_ATOM_INC
#  undef HB_ATOM_GET
#  undef HB_ATOM_SET
#  define HB_ATOM_INC( p )    ( ++(*(p)) )
#  define HB_ATOM_DEC( p )    ( --(*(p)) )

#elif !defined( HB_ATOM_INC ) || !defined( HB_ATOM_DEC )

   /* HB_ATOM_INC and HB_ATOM_DEC have to be synced together */
#  undef HB_ATOM_DEC
#  undef HB_ATOM_INC
#  undef HB_ATOM_GET
#  undef HB_ATOM_SET
   static HB_FORCEINLINE void hb_counterIncrement( volatile HB_COUNTER * p )
   {
      HB_FM_LOCK();
      ++(*p);
      HB_FM_UNLOCK();
   }
#  define HB_ATOM_INC( p )    hb_counterIncrement( p )
   static HB_FORCEINLINE int hb_counterDecrement( volatile HB_COUNTER * p )
   {
      int iResult;
      HB_FM_LOCK();
      iResult = --(*p) != 0;
      HB_FM_UNLOCK();
      return iResult;
   }
#  define HB_ATOM_DEC( p )    hb_counterDecrement( p )
#endif

#ifndef HB_ATOM_GET
#  define HB_ATOM_GET( p )    (*(p))
#endif
#ifndef HB_ATOM_SET
#  define HB_ATOM_SET( p, n ) ( (*(p)) = (n) )
#endif


#if defined( HB_FM_DLMT_ALLOC )

#  if !defined( HB_MSPACE_COUNT )
#     define HB_MSPACE_COUNT  16
#  endif

typedef struct
{
   int      count;
   mspace   ms;
} HB_MSPACE, * PHB_MSPACE;

static mspace s_gm = NULL;
static HB_MSPACE s_mspool[ HB_MSPACE_COUNT ];


static mspace hb_mspace( void )
{
   PHB_MSPACE pm = ( PHB_MSPACE ) hb_stackAllocator();

   if( pm )
      return pm->ms;

   if( !s_gm )
      s_gm = create_mspace( 0, 1 );

   return s_gm;
}

static PHB_MSPACE hb_mspace_alloc( void )
{
   if( s_mspool[ 0 ].ms == NULL && s_gm )
   {
      s_mspool[ 0 ].count = 1;
      s_mspool[ 0 ].ms = s_gm;
      return &s_mspool[ 0 ];
   }
   else
   {
      int i, imin = 0;
      for( i = 1; i < HB_MSPACE_COUNT; ++i )
      {
         if( s_mspool[ i ].count < s_mspool[ imin ].count )
            imin = i;
      }
      if( s_mspool[ imin ].ms == NULL )
         s_mspool[ imin ].ms = create_mspace( 0, 1 );
      s_mspool[ imin ].count++;
      return &s_mspool[ imin ];
   }
}

static void * hb_mspace_update( void * pAlloc, int iCount )
{
   PHB_MSPACE pm = ( PHB_MSPACE ) pAlloc;

   if( pm && pm->count > iCount )
   {
      pAlloc = ( void * ) hb_mspace_alloc();
      pm->count--;
   }

   return pAlloc;
}

static void hb_mspace_cleanup( void )
{
   int i;

   s_gm = NULL;
   for( i = 0; i < HB_MSPACE_COUNT; ++i )
   {
      if( s_mspool[ i ].ms )
      {
         destroy_mspace( s_mspool[ i ].ms );
         s_mspool[ i ].ms = NULL;
         s_mspool[ i ].count = 0;
      }
   }
}

#elif defined( HB_FM_DL_ALLOC ) && defined( USE_DL_PREFIX )

#if defined( __WATCOMC__ )
#  pragma warning 367 9
#endif

static void dlmalloc_destroy( void )
{
   if( ok_magic(gm) )
   {
      msegmentptr sp = &gm->seg;
      while(sp != 0 )
      {
         char* base = sp->base;
         size_t size = sp->size;
         flag_t flag = sp->sflags;
         sp = sp->next;
         if( (flag & USE_MMAP_BIT) && !(flag & EXTERN_BIT) )
            CALL_MUNMAP(base, size);
      }
   }
}

#if defined( __WATCOMC__ )
#  pragma warning 367 2
#endif

#endif

void hb_xinit_thread( void )
{
#if defined( HB_FM_DLMT_ALLOC )
   HB_STACK_TLS_PRELOAD

   if( hb_stack.allocator == NULL )
   {
      HB_FM_LOCK();
      hb_stack.allocator = ( void * ) hb_mspace_alloc();
      HB_FM_UNLOCK();
   }
#endif
}

void hb_xexit_thread( void )
{
#if defined( HB_FM_DLMT_ALLOC )
   HB_STACK_TLS_PRELOAD
   PHB_MSPACE pm = ( PHB_MSPACE ) hb_stack.allocator;

   if( pm )
   {
      hb_stack.allocator = NULL;
      HB_FM_LOCK();
      if( --pm->count == 0 )
         mspace_trim( pm->ms, 0 );
      HB_FM_UNLOCK();
   }
#endif
}

void hb_xclean( void )
{
#if defined( HB_FM_DLMT_ALLOC )
   HB_FM_LOCK();
   {
      int i, imax, icount;

      if( s_gm )
         mspace_trim( s_gm, 0 );

      for( i = imax = icount = 0; i < HB_MSPACE_COUNT; ++i )
      {
         if( s_mspool[ i ].ms )
         {
            icount += s_mspool[ i ].count;
            if( imax < s_mspool[ i ].count )
               imax = s_mspool[ i ].count;
            mspace_trim( s_mspool[ i ].ms, 0 );
         }
      }
      icount = ( icount + HB_MSPACE_COUNT - 1 ) / HB_MSPACE_COUNT;
      if( imax > icount )
      {
         /* balance mspaces between running threads */
         hb_vmUpdateAllocator( hb_mspace_update, icount );
      }
   }
   HB_FM_UNLOCK();
#elif defined( HB_FM_DL_ALLOC )
   dlmalloc_trim( 0 );
#endif
}

void hb_xsetfilename( const char * szValue )
{
#ifdef HB_FM_STATISTICS
   if( szValue )
      hb_strncpy( s_szFileName, szValue, sizeof( s_szFileName ) - 1 );
   else
      s_szFileName[ 0 ] = '\0';
#else
   HB_SYMBOL_UNUSED( szValue );
#endif
}

void hb_xsetinfo( const char * szValue )
{
#ifdef HB_FM_STATISTICS
   hb_strncpy( s_szInfo, szValue, sizeof( s_szInfo ) - 1 );
#else
   HB_SYMBOL_UNUSED( szValue );
#endif
}

void * hb_xalloc( HB_SIZE nSize )         /* allocates fixed memory, returns NULL on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xalloc(%" HB_PFS "u)", nSize));

   if( nSize == 0 )
      hb_errInternal( HB_EI_XALLOCNULLSIZE, NULL, NULL, NULL );

#ifdef HB_FM_NEED_INIT
   if( !s_fInitedFM )
      hb_xinit();
#endif

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      return pMem;

#ifdef HB_FM_STATISTICS

   if( s_fStatistic )
   {
      PHB_TRACEINFO pTrace;

      HB_FM_LOCK();

      if( ! s_pFirstBlock )
      {
         pMem->pPrevBlock = NULL;
         s_pFirstBlock = pMem;
      }
      else
      {
         pMem->pPrevBlock = s_pLastBlock;
         s_pLastBlock->pNextBlock = pMem;
      }
      s_pLastBlock = pMem;

      pMem->pNextBlock = NULL;
      pMem->u32Signature = HB_MEMINFO_SIGNATURE;
      HB_FM_SETSIG( HB_MEM_PTR( pMem ), nSize );
      pMem->nSize = nSize;  /* size of the memory block */

      pTrace = hb_traceinfo();
      if( hb_tr_level() >= HB_TR_DEBUG || pTrace->level == HB_TR_FM )
      {
         /* NOTE: PRG line number/procname is not very useful during hunting
         * for memory leaks - this is why we are using the previously stored
         * function/line info - this is a location of code that called
         * hb_xalloc/hb_xgrab
         */
         pMem->uiProcLine = pTrace->line; /* C line number */
         if( pTrace->file )
            hb_strncpy( pMem->szProcName, pTrace->file, sizeof( pMem->szProcName ) - 1 );
         else
            pMem->szProcName[ 0 ] = '\0';
         pTrace->level = -1;
      }
      else
      {
         hb_stackBaseProcInfo( pMem->szProcName, &pMem->uiProcLine );
      }

      s_nMemoryConsumed += nSize + sizeof( HB_COUNTER );
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;

      HB_FM_UNLOCK();

#ifdef HB_PARANOID_MEM_CHECK
      memset( HB_MEM_PTR( pMem ), HB_MEMFILER, nSize );
#endif

   }

#endif /* HB_FM_STATISTICS */

   HB_ATOM_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );

   return HB_MEM_PTR( pMem );
}

void * hb_xgrab( HB_SIZE nSize )         /* allocates fixed memory, exits on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xgrab(%" HB_PFS "u)", nSize));

   if( nSize == 0 )
      hb_errInternal( HB_EI_XGRABNULLSIZE, NULL, NULL, NULL );

#ifdef HB_FM_NEED_INIT
   if( !s_fInitedFM )
      hb_xinit();
#endif

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      hb_errInternal( HB_EI_XGRABALLOC, NULL, NULL, NULL );

#ifdef HB_FM_STATISTICS

   if( s_fStatistic )
   {
      PHB_TRACEINFO pTrace;

      HB_FM_LOCK();

      if( ! s_pFirstBlock )
      {
         pMem->pPrevBlock = NULL;
         s_pFirstBlock = pMem;
      }
      else
      {
         pMem->pPrevBlock = s_pLastBlock;
         s_pLastBlock->pNextBlock = pMem;
      }
      s_pLastBlock = pMem;

      pMem->pNextBlock = NULL;
      pMem->u32Signature = HB_MEMINFO_SIGNATURE;
      HB_FM_SETSIG( HB_MEM_PTR( pMem ), nSize );
      pMem->nSize = nSize;  /* size of the memory block */

      pTrace = hb_traceinfo();
      if( hb_tr_level() >= HB_TR_DEBUG || pTrace->level == HB_TR_FM )
      {
         /* NOTE: PRG line number/procname is not very useful during hunting
         * for memory leaks - this is why we are using the previously stored
         * function/line info - this is a location of code that called
         * hb_xalloc/hb_xgrab
         */
         pMem->uiProcLine = pTrace->line; /* C line number */
         if( pTrace->file )
            hb_strncpy( pMem->szProcName, pTrace->file, sizeof( pMem->szProcName ) - 1 );
         else
            pMem->szProcName[ 0 ] = '\0';
         pTrace->level = -1;
      }
      else
      {
         hb_stackBaseProcInfo( pMem->szProcName, &pMem->uiProcLine );
      }

      s_nMemoryConsumed += nSize + sizeof( HB_COUNTER );
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;
      s_nMemoryBlocks++;
      if( s_nMemoryMaxBlocks < s_nMemoryBlocks )
         s_nMemoryMaxBlocks = s_nMemoryBlocks;

      HB_FM_UNLOCK();

#ifdef HB_PARANOID_MEM_CHECK
      memset( HB_MEM_PTR( pMem ), HB_MEMFILER, nSize );
#endif

   }

#endif /* HB_FM_STATISTICS */

   HB_ATOM_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );

   return HB_MEM_PTR( pMem );
}

void * hb_xrealloc( void * pMem, HB_SIZE nSize )       /* reallocates memory */
{
   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xrealloc(%p, %" HB_PFS "u)", pMem, nSize));

#if 0
   /* disabled to make hb_xrealloc() ANSI-C realloc() compatible */
   if( ! pMem )
      hb_errInternal( HB_EI_XREALLOCNULL, NULL, NULL, NULL );

   if( nSize == 0 )
      hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
#endif

#ifdef HB_FM_STATISTICS
   if( pMem == NULL )
   {
      if( nSize == 0 )
         hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      return hb_xgrab( nSize );
   }
   else if( nSize == 0 )
   {
      hb_xfree( pMem );
      return NULL;
   }
   else if( s_fStatistic )
   {
      PHB_MEMINFO pMemBlock;
      HB_SIZE nMemSize;

      pMemBlock = HB_FM_PTR( pMem );

      if( pMemBlock->u32Signature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XREALLOCINV, NULL, NULL, NULL );

      nMemSize = pMemBlock->nSize;

      if( HB_FM_GETSIG( pMem, nMemSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, NULL, NULL, NULL );

      HB_FM_CLRSIG( pMem, nMemSize );

      HB_FM_LOCK();

#ifdef HB_PARANOID_MEM_CHECK
      pMem = malloc( HB_ALLOC_SIZE( nSize ) );
      if( pMem )
      {
         HB_ATOM_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );
         if( nSize > nMemSize )
         {
            memcpy( pMem, pMemBlock, HB_ALLOC_SIZE( nMemSize ) );
            memset( ( HB_BYTE * ) pMem + HB_ALLOC_SIZE( nMemSize ), HB_MEMFILER, nSize - nMemSize );
         }
         else
            memcpy( pMem, pMemBlock, HB_ALLOC_SIZE( nSize ) );
      }
      memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( nMemSize ) );
      free( pMemBlock );
#else
      pMem = realloc( pMemBlock, HB_ALLOC_SIZE( nSize ) );
#endif

      s_nMemoryConsumed += ( nSize - nMemSize );
      if( s_nMemoryMaxConsumed < s_nMemoryConsumed )
         s_nMemoryMaxConsumed = s_nMemoryConsumed;

      if( pMem )
      {
         ( ( PHB_MEMINFO ) pMem )->nSize = nSize;  /* size of the memory block */
         HB_FM_SETSIG( HB_MEM_PTR( pMem ), nSize );
         if( ( ( PHB_MEMINFO ) pMem )->pPrevBlock )
            ( ( PHB_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
         if( ( ( PHB_MEMINFO ) pMem )->pNextBlock )
            ( ( PHB_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pMem;

         if( s_pFirstBlock == pMemBlock )
            s_pFirstBlock = ( PHB_MEMINFO ) pMem;
         if( s_pLastBlock == pMemBlock )
            s_pLastBlock = ( PHB_MEMINFO ) pMem;
      }

      HB_FM_UNLOCK();
   }
   else
      pMem = realloc( HB_FM_PTR( pMem ), HB_ALLOC_SIZE( nSize ) );

   if( ! pMem )
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );

#else

   if( pMem == NULL )
   {
      if( nSize == 0 )
         hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );
      pMem = malloc( HB_ALLOC_SIZE( nSize ) );
      if( pMem )
         HB_ATOM_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ), 1 );
   }
   else if( nSize == 0 )
   {
      free( HB_FM_PTR( pMem ) );
      return NULL;
   }
   else
   {
      pMem = realloc( HB_FM_PTR( pMem ), HB_ALLOC_SIZE( nSize ) );
   }

   if( !pMem )
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );

#endif

   return HB_MEM_PTR( pMem );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

   if( pMem )
   {
#ifdef HB_FM_STATISTICS

      PHB_MEMINFO pMemBlock = HB_FM_PTR( pMem );

      if( s_fStatistic )
      {
         if( pMemBlock->u32Signature != HB_MEMINFO_SIGNATURE )
            hb_errInternal( HB_EI_XFREEINV, NULL, NULL, NULL );

         if( HB_FM_GETSIG( pMem, pMemBlock->nSize ) != HB_MEMINFO_SIGNATURE )
            hb_errInternal( HB_EI_XMEMOVERFLOW, NULL, NULL, NULL );

         HB_FM_LOCK();

         s_nMemoryConsumed -= pMemBlock->nSize + sizeof( HB_COUNTER );
         s_nMemoryBlocks--;

         if( pMemBlock->pPrevBlock )
            pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
         else
            s_pFirstBlock = pMemBlock->pNextBlock;

         if( pMemBlock->pNextBlock )
            pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
         else
            s_pLastBlock = pMemBlock->pPrevBlock;

         HB_FM_UNLOCK();

         pMemBlock->u32Signature = 0;
         HB_FM_CLRSIG( pMem, pMemBlock->nSize );
#ifdef HB_PARANOID_MEM_CHECK
         memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( pMemBlock->nSize ) );
#endif
      }

      free( ( void * ) pMemBlock );

#else

      free( HB_FM_PTR( pMem ) );

#endif
   }
   else
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
}

/* increment reference counter */
#undef hb_xRefInc
void hb_xRefInc( void * pMem )
{
   HB_ATOM_INC( HB_COUNTER_PTR( pMem ) );
}

/* decrement reference counter, return HB_TRUE when 0 reached */
#undef hb_xRefDec
HB_BOOL hb_xRefDec( void * pMem )
{
   return HB_ATOM_DEC( HB_COUNTER_PTR( pMem ) ) == 0;
}

/* decrement reference counter and free the block when 0 reached */
#undef hb_xRefFree
void hb_xRefFree( void * pMem )
{
#ifdef HB_FM_STATISTICS

   if( s_fStatistic && HB_FM_PTR( pMem )->u32Signature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XFREEINV, NULL, NULL, NULL );

   if( HB_ATOM_DEC( HB_COUNTER_PTR( pMem ) ) == 0 )
      hb_xfree( pMem );

#else

   if( HB_ATOM_DEC( HB_COUNTER_PTR( pMem ) ) == 0 )
      free( HB_FM_PTR( pMem ) );

#endif
}

/* return number of references */
#undef hb_xRefCount
HB_COUNTER hb_xRefCount( void * pMem )
{
   return HB_ATOM_GET( HB_COUNTER_PTR( pMem ) );
}

/* reallocates memory, create copy if reference counter greater then 1 */
#undef hb_xRefResize
void * hb_xRefResize( void * pMem, HB_SIZE nSave, HB_SIZE nSize, HB_SIZE * pnAllocated )
{

#ifdef HB_FM_STATISTICS
   if( HB_ATOM_GET( HB_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = memcpy( hb_xgrab( nSize ), pMem, HB_MIN( nSave, nSize ) );

      if( HB_ATOM_DEC( HB_COUNTER_PTR( pMem ) ) == 0 )
         hb_xfree( pMem );

      *pnAllocated = nSize;
      return pMemNew;
   }
   else if( nSize <= *pnAllocated )
      return pMem;

   *pnAllocated = nSize;
   return hb_xrealloc( pMem, nSize );

#else

   if( HB_ATOM_GET( HB_COUNTER_PTR( pMem ) ) > 1 )
   {
      void * pMemNew = malloc( HB_ALLOC_SIZE( nSize ) );

      if( pMemNew )
      {
         HB_ATOM_SET( HB_COUNTER_PTR( HB_MEM_PTR( pMemNew ) ), 1 );
         memcpy( HB_MEM_PTR( pMemNew ), pMem, HB_MIN( nSave, nSize ) );
         if( HB_ATOM_DEC( HB_COUNTER_PTR( pMem ) ) == 0 )
            free( HB_FM_PTR( pMem ) );
         *pnAllocated = nSize;
         return HB_MEM_PTR( pMemNew );
      }
   }
   else if( nSize <= *pnAllocated )
      return pMem;
   else
   {
      *pnAllocated = nSize;
      pMem = realloc( HB_FM_PTR( pMem ), HB_ALLOC_SIZE( nSize ) );
      if( pMem )
         return HB_MEM_PTR( pMem );
   }

   hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   return NULL;
#endif
}

/* NOTE: Debug function, it will always return 0 when HB_FM_STATISTICS is
         not defined, don't use it for final code [vszakats] */

HB_SIZE hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xsize(%p)", pMem));

#ifdef HB_FM_STATISTICS
   return HB_FM_BLOCKSIZE( pMem );
#else
   HB_SYMBOL_UNUSED( pMem );

   return 0;
#endif
}

void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xinit()"));

#ifdef HB_FM_NEED_INIT
   if( !s_fInitedFM )
   {

#ifdef HB_FM_STATISTICS
      char buffer[ 5 ];

      if( hb_getenv_buffer( "HB_FM_STAT", buffer, sizeof( buffer ) ) )
      {
         if( hb_stricmp( "yes", buffer ) == 0 )
            s_fStatistic = HB_TRUE;
         else if( hb_stricmp( "no", buffer ) == 0 )
            s_fStatistic = HB_FALSE;
      }
#ifndef HB_FM_STATISTICS_DYN_OFF
      else
         s_fStatistic = HB_TRUE;  /* enabled by default */
#endif /* HB_FM_STATISTICS_DYN_OFF */
#endif /* HB_FM_STATISTICS */

#if defined( HB_FM_HEAP_INIT )
      s_hProcessHeap = GetProcessHeap();
#endif

      s_fInitedFM = HB_TRUE;
   }
#endif /* HB_FM_NEED_INIT */
}

/* Returns pointer to string containing printable version
   of pMem memory block */

#ifdef HB_FM_STATISTICS
static char * hb_mem2str( char * membuffer, void * pMem, HB_SIZE nSize )
{
   HB_BYTE * cMem = ( HB_BYTE * ) pMem;
   HB_SIZE nIndex, nPrintable;

   nPrintable = 0;
   for( nIndex = 0; nIndex < nSize; nIndex++ )
      if( ( cMem[ nIndex ] & 0x60 ) != 0 )
         nPrintable++;

   if( nPrintable * 100 / nSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( nIndex = 0; nIndex < nSize; nIndex++ )
         if( cMem[ nIndex ] >= ' ' )
            membuffer[ nIndex ] = cMem[ nIndex ];
         else
            membuffer[ nIndex ] = '.';
      membuffer[ nIndex ] = '\0';
   }
   else
   {
     /* format as hex */
      for( nIndex = 0; nIndex < nSize; nIndex++ )
      {
         int lownibble, hinibble;
         hinibble = cMem[ nIndex ] >> 4;
         lownibble = cMem[ nIndex ] & 0x0F;
         membuffer[ nIndex * 2 ]     = hinibble <= 9 ?
                               ( '0' + hinibble ) : ( 'A' + hinibble - 10 );
         membuffer[ nIndex * 2 + 1 ] = lownibble <= 9 ?
                               ( '0' + lownibble ) : ( 'A' + lownibble - 10 );
      }
      membuffer[ nIndex * 2 ] = '\0';
   }

   return membuffer;
}

#define HB_MAX_MEM2STR_BLOCK 256
void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));

   if( s_nMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      char membuffer[ HB_MAX_MEM2STR_BLOCK * 2 + 1 ]; /* multiplied by 2 to allow hex format */
      PHB_MEMINFO pMemBlock;
      HB_USHORT ui;
      char buffer[ 100 ];
      FILE * hLog = NULL;

      if( s_nMemoryBlocks )
         hLog = hb_fopen( s_szFileName[ 0 ] ? s_szFileName : "hb_out.log", "a+" );

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( buffer, sizeof( buffer ), HB_I_("Total memory allocated: %" HB_PFS "i bytes (%" HB_PFS "i block(s))"), s_nMemoryMaxConsumed, s_nMemoryMaxBlocks );
      hb_conOutErr( buffer, 0 );

      if( s_nMemoryBlocks )
      {
         if( hLog )
         {
            char szTime[ 9 ];
            int iYear, iMonth, iDay;

            hb_dateToday( &iYear, &iMonth, &iDay );
            hb_dateTimeStr( szTime );

            fprintf( hLog, HB_I_("Application Memory Allocation Report - %s\n"), hb_cmdargARGVN( 0 ) );
            fprintf( hLog, HB_I_("Terminated at: %04d.%02d.%02d %s\n"), iYear, iMonth, iDay, szTime );
            if( s_szInfo[ 0 ] )
               fprintf( hLog, HB_I_("Info: %s\n"), s_szInfo );
            fprintf( hLog, "%s\n", buffer );
         }

         hb_conOutErr( hb_conNewLine(), 0 );
         hb_snprintf( buffer, sizeof( buffer ), HB_I_("Warning, memory allocated but not released: %" HB_PFS "i bytes (%" HB_PFS "i block(s))"), s_nMemoryConsumed, s_nMemoryBlocks );
         hb_conOutErr( buffer, 0 );

         if( hLog )
            fprintf( hLog, "%s\n", buffer );
      }
      else
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_conOutErr( HB_I_("Memory allocated but not released: none"), 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock, ++ui )
      {
         HB_TRACE( HB_TR_ERROR, ( "Block %i (size %" HB_PFS "u) %s(%i), \"%s\"", ui,
            pMemBlock->nSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
            hb_mem2str( membuffer, ( char * ) HB_MEM_PTR( pMemBlock ),
                        HB_MIN( pMemBlock->nSize, HB_MAX_MEM2STR_BLOCK ) ) ) );

         if( hLog )
         {
            fprintf( hLog, HB_I_("Block %i %p (size %" HB_PFS "u) %s(%i), \"%s\"\n"), ui,
               ( char * ) HB_MEM_PTR( pMemBlock ),
               pMemBlock->nSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
               hb_mem2str( membuffer, ( char * ) HB_MEM_PTR( pMemBlock ),
                           HB_MIN( pMemBlock->nSize, HB_MAX_MEM2STR_BLOCK ) ) );
         }
      }

      if( hLog )
      {
         fprintf( hLog, "------------------------------------------------------------------------\n");
         fclose( hLog );
      }
   }

#if defined( HB_FM_DL_ALLOC )
#  if defined( HB_FM_DLMT_ALLOC )
      hb_mspace_cleanup();
#  elif defined( USE_DL_PREFIX )
      dlmalloc_destroy();
#  else
      malloc_trim( 0 );
#  endif
#endif
}

#else

void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));

#if defined( HB_FM_DL_ALLOC )
#  if defined( HB_FM_DLMT_ALLOC )
      hb_mspace_cleanup();
#  elif defined( USE_DL_PREFIX )
      dlmalloc_destroy();
#  else
      malloc_trim( 0 );
#  endif
#endif
}

#endif

HB_SIZE hb_xquery( int iMode )
{
   HB_SIZE nResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_xquery(%d)", iMode));

   /* TODO: Return the correct values instead of 9999 [vszakats] */

   switch( iMode )
   {
      case HB_MEM_CHAR:       /*               (Free Variable Space [KB]) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               nResult = 0;
            else
               nResult = ulSysInfo / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_BLOCK:      /*               (Largest String [KB]) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = HB_MIN( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               nResult = 0;
            else
               nResult = HB_MIN( ulSysInfo, ULONG_MAX ) / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_RUN:        /*               (RUN Memory [KB]) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               nResult = 0;
            else
               nResult = ulSysInfo / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_VM:         /* UNDOCUMENTED! (Virtual Memory [KB]) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailVirtual / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               nResult = 0;
            else
               nResult = ulSysInfo / 1024;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_EMS:        /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?) */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         nResult = 0;
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_FM:         /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwTotalPhys / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            ULONG ulSysInfo = 0;

            if( DosQuerySysInfo( QSV_MAXPRMEM, QSV_MAXPRMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
               nResult = 0;
            else
               nResult = ulSysInfo / 1024;
         }
#elif defined( HB_OS_DOS )
         {
            union REGS regs;
            regs.HB_XREGS.ax = 0;
            HB_DOS_INT86( 0x12, &regs, &regs );
            iMode = regs.h.al;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_FMSEGS:     /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         nResult = 1;
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_SWAP:       /* UNDOCUMENTED! (Free Swap Memory [KB]) */
#if defined( HB_OS_WIN )
         {
            MEMORYSTATUS memorystatus;
            GlobalMemoryStatus( &memorystatus );
            nResult = memorystatus.dwAvailPageFile / 1024;
         }
#elif defined( HB_OS_OS2 )
         {
            /* NOTE: There is no way to know how much a swap file can grow on an
                     OS/2 system. I think we should return free space on DASD
                     media which contains swap file [maurilio.longo] */
            nResult = 9999;
         }
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_CONV:       /* UNDOCUMENTED! (Free Conventional [KB]) */
#if defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
         nResult = 0;
#else
         nResult = 9999;
#endif
         break;

      case HB_MEM_EMSUSED:    /* UNDOCUMENTED! (Used Expanded Memory [KB]) (?) */
         nResult = 0;
         break;

      case HB_MEM_USED:       /* Harbour extension (Memory used [bytes]) */
#ifdef HB_FM_STATISTICS
         nResult = s_nMemoryConsumed;
#else
         nResult = 0;
#endif
         break;

      case HB_MEM_BLOCKS:     /* Harbour extension (Memory blocks used) */
#ifdef HB_FM_STATISTICS
         nResult = s_nMemoryBlocks;
#else
         nResult = 0;
#endif
         break;

      case HB_MEM_USEDMAX:    /* Harbour extension (Maximum memory used [bytes]) */
#ifdef HB_FM_STATISTICS
         nResult = s_nMemoryMaxConsumed;
#else
         nResult = 0;
#endif
         break;

      case HB_MEM_STACKITEMS: /* Harbour extension (Total items allocated for the stack) */
         nResult = hb_stackTotalItems();
         break;

      case HB_MEM_STACK:      /* Harbour extension (Total memory size used by the stack [bytes]) */
         nResult = hb_stackTotalItems() * sizeof( HB_ITEM );
         break;

      case HB_MEM_STACK_TOP : /* Harbour extension (Total items currently on the stack) */
      {
         HB_STACK_TLS_PRELOAD
         nResult = hb_stackTopOffset( );
         break;
      }
      default:
         nResult = 0;
   }

   return nResult;
}

HB_BOOL hb_xtraced( void )
{
#if HB_TR_LEVEL >= HB_TR_DEBUG
   return HB_TRUE;
#else
   return HB_FALSE;
#endif
}
