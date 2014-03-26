/*
 * Harbour Project source code:
 * The garbage collector for Harbour
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* NOTE: Need to have these before Harbour headers,
         because in MT mode, they will automatically #include <os2.h>. */
#define INCL_DOSPROCESS

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapicls.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "error.ch"

#if ! defined( HB_GC_PTR )

#if defined( HB_MT_VM )

#  include "hbthread.h"
#  include "hbatomic.h"

/* Use spinlock instead of mutex */

#  if defined( HB_SPINLOCK_INIT ) && 1

      HB_SPINLOCK_T s_gcSpinLock = HB_SPINLOCK_INIT;
#     define HB_GC_LOCK()       HB_SPINLOCK_ACQUIRE( &s_gcSpinLock )
#     define HB_GC_UNLOCK()     HB_SPINLOCK_RELEASE( &s_gcSpinLock )

#  else

      static HB_CRITICAL_NEW( s_gcMtx );
#     define HB_GC_LOCK()       hb_threadEnterCriticalSection( &s_gcMtx )
#     define HB_GC_UNLOCK()     hb_threadLeaveCriticalSection( &s_gcMtx )

#endif

#else

#  define HB_GC_LOCK()       do {} while( 0 )
#  define HB_GC_UNLOCK()     do {} while( 0 )

#endif /* HB_MT_VM */

/* holder of memory block information */
/* NOTE: HB_USHORT is used intentionally to fill up the structure to
 * full 16 bytes (on 16/32 bit environment)
 */
typedef struct HB_GARBAGE_
{
   struct HB_GARBAGE_ * pNext;   /* next memory block */
   struct HB_GARBAGE_ * pPrev;   /* previous memory block */
   const HB_GC_FUNCS *  pFuncs;  /* cleanup function called before memory releasing */
   HB_USHORT locked;             /* locking counter */
   HB_USHORT used;               /* used/unused block */
} HB_GARBAGE, * PHB_GARBAGE;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_GARBAGE_SIZE     ( ( sizeof( HB_GARBAGE ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                                ( sizeof( HB_GARBAGE ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT )
#else
#  define HB_GARBAGE_SIZE     sizeof( HB_GARBAGE )
#endif

#define HB_GC_PTR( p )        ( ( PHB_GARBAGE ) ( ( HB_BYTE * ) ( p ) - HB_GARBAGE_SIZE ) )

#endif /* ! defined( HB_GC_PTR ) */

#define HB_BLOCK_PTR( p )       ( ( void * ) ( ( HB_BYTE * ) ( p ) + HB_GARBAGE_SIZE ) )

/* we may use a cache later */
#define HB_GARBAGE_NEW( nSize )    ( ( PHB_GARBAGE ) hb_xgrab( HB_GARBAGE_SIZE + ( nSize ) ) )
#define HB_GARBAGE_FREE( pAlloc )   hb_xfree( ( void * ) ( pAlloc ) )

/* status of memory block */
/* flags stored in 'used' slot */
#define HB_GC_USED_FLAG    1  /* the bit for used/unused flag */
#define HB_GC_DELETE       2  /* item marked to delete */
#define HB_GC_DELETELST    4  /* item will be deleted during finalization */

#ifdef HB_GC_AUTO
#define HB_GC_AUTO_MAX        ( ( HB_PTRUINT ) ( -1 ) )
/* number of allocated memory blocks */
static HB_PTRUINT s_ulBlocks = 0;
/* number of allocated memory blocks after last GC activation */
static HB_PTRUINT s_ulBlocksMarked = 0;
/* number of memory blocks between automatic GC activation */
static HB_PTRUINT s_ulBlocksAuto = 0;
/* number of allocated memory blocks which should force next GC activation */
static HB_PTRUINT s_ulBlocksCheck = 0;

#  define HB_GC_AUTO_INC()    ++s_ulBlocks
#  define HB_GC_AUTO_DEC()    --s_ulBlocks
#else
#  define HB_GC_AUTO_INC()    do {} while( 0 )
#  define HB_GC_AUTO_DEC()    do {} while( 0 )
#endif

/* pointer to memory block that will be checked in next step */
static PHB_GARBAGE s_pCurrBlock = NULL;
/* memory blocks are stored in linked list with a loop */

/* pointer to locked memory blocks */
static PHB_GARBAGE s_pLockedBlock = NULL;

/* pointer to memory blocks that will be deleted */
static PHB_GARBAGE s_pDeletedBlock = NULL;

/* marks if block releasing is requested during garbage collecting */
static HB_BOOL s_bCollecting = HB_FALSE;

/* flag for used/unused blocks - the meaning of the HB_GC_USED_FLAG bit
 * is reversed on every collecting attempt
 */
static HB_USHORT s_uUsedFlag = HB_GC_USED_FLAG;


static void hb_gcLink( PHB_GARBAGE * pList, PHB_GARBAGE pAlloc )
{
   if( *pList )
   {
      /* add new block at the logical end of list */
      pAlloc->pNext = *pList;
      pAlloc->pPrev = ( *pList )->pPrev;
      pAlloc->pPrev->pNext = pAlloc;
      ( *pList )->pPrev = pAlloc;
   }
   else
   {
      *pList = pAlloc->pNext = pAlloc->pPrev = pAlloc;
   }
}

static void hb_gcUnlink( PHB_GARBAGE * pList, PHB_GARBAGE pAlloc )
{
   pAlloc->pPrev->pNext = pAlloc->pNext;
   pAlloc->pNext->pPrev = pAlloc->pPrev;
   if( *pList == pAlloc )
   {
      *pList = pAlloc->pNext;
      if( *pList == pAlloc )
         *pList = NULL;    /* this was the last block */
   }
}

/* allocates a memory block */
void * hb_gcAllocate( HB_SIZE nSize, const HB_GC_FUNCS * pFuncs )
{
   PHB_GARBAGE pAlloc;

   pAlloc = HB_GARBAGE_NEW( nSize );
   pAlloc->pFuncs = pFuncs;
   pAlloc->locked = 1;
   pAlloc->used   = s_uUsedFlag;
   HB_GC_LOCK();
   hb_gcLink( &s_pLockedBlock, pAlloc );
   HB_GC_UNLOCK();

   return HB_BLOCK_PTR( pAlloc );        /* hide the internal data */
}

/* allocates a memory block */
void * hb_gcAllocRaw( HB_SIZE nSize, const HB_GC_FUNCS * pFuncs )
{
   PHB_GARBAGE pAlloc;

   pAlloc = HB_GARBAGE_NEW( nSize );
   pAlloc->pFuncs = pFuncs;
   pAlloc->locked = 0;
   pAlloc->used   = s_uUsedFlag;

   HB_GC_LOCK();
#ifdef HB_GC_AUTO
   if( s_ulBlocks > s_ulBlocksCheck )
   {
      HB_GC_UNLOCK();
      hb_gcCollectAll( HB_TRUE );
      HB_GC_LOCK();
      pAlloc->used   = s_uUsedFlag;
   }
   HB_GC_AUTO_INC();
#endif
   hb_gcLink( &s_pCurrBlock, pAlloc );
   HB_GC_UNLOCK();

   return HB_BLOCK_PTR( pAlloc );        /* hide the internal data */
}

/* release a memory block allocated with hb_gcAlloc*() */
void hb_gcFree( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

      /* Don't release the block that will be deleted during finalization */
      if( ! ( pAlloc->used & HB_GC_DELETE ) )
      {
         HB_GC_LOCK();
         if( pAlloc->locked )
            hb_gcUnlink( &s_pLockedBlock, pAlloc );
         else
         {
            hb_gcUnlink( &s_pCurrBlock, pAlloc );
            HB_GC_AUTO_DEC();
         }
         HB_GC_UNLOCK();

         HB_GARBAGE_FREE( pAlloc );
      }
   }
   else
   {
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
   }
}

/* return cleanup function pointer */
const HB_GC_FUNCS * hb_gcFuncs( void * pBlock )
{
   return HB_GC_PTR( pBlock )->pFuncs;
}

/* increment reference counter */
#undef hb_gcRefInc
void hb_gcRefInc( void * pBlock )
{
   hb_xRefInc( HB_GC_PTR( pBlock ) );
}

/* decrement reference counter and free the block when 0 reached */
#undef hb_gcRefFree
void hb_gcRefFree( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

      if( hb_xRefDec( pAlloc ) )
      {
         /* Don't release the block that will be deleted during finalization */
         if( ! ( pAlloc->used & HB_GC_DELETE ) )
         {
            /* unlink the block first to avoid possible problems
             * if cleanup function activate GC
             */
            HB_GC_LOCK();
            if( pAlloc->locked )
               hb_gcUnlink( &s_pLockedBlock, pAlloc );
            else
            {
               hb_gcUnlink( &s_pCurrBlock, pAlloc );
               HB_GC_AUTO_DEC();
            }
            HB_GC_UNLOCK();

            pAlloc->used |= HB_GC_DELETE;

            /* execute clean-up function */
            pAlloc->pFuncs->clear( pBlock );

            if( pAlloc->used & HB_GC_DELETE )
               HB_GARBAGE_FREE( pAlloc );
         }
      }
   }
   else
   {
      hb_errInternal( HB_EI_XFREENULL, NULL, NULL, NULL );
   }
}


/* return number of references */
#undef hb_gcRefCount
HB_COUNTER hb_gcRefCount( void * pBlock )
{
   return hb_xRefCount( HB_GC_PTR( pBlock ) );
}


/*
 * Check if block still cannot be accessed after destructor execution
 */
void hb_gcRefCheck( void * pBlock )
{
   PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

   if( ! ( pAlloc->used & HB_GC_DELETELST ) )
   {
      if( hb_xRefCount( pAlloc ) != 0 )
      {
         pAlloc->used = s_uUsedFlag;
         pAlloc->locked = 0;

         HB_GC_LOCK();
         hb_gcLink( &s_pCurrBlock, pAlloc );
         HB_GC_AUTO_INC();
         HB_GC_UNLOCK();

         if( hb_vmRequestQuery() == 0 )
            hb_errRT_BASE( EG_DESTRUCTOR, 1301, NULL, "Reference to freed block", 0 );
      }
   }
}


HB_GARBAGE_FUNC( hb_gcDummyMark )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_GARBAGE_FUNC( hb_gcGripMark )
{
   hb_gcItemRef( ( PHB_ITEM ) Cargo );
}

static HB_GARBAGE_FUNC( hb_gcGripRelease )
{
   if( HB_IS_COMPLEX( ( PHB_ITEM ) Cargo ) )
      hb_itemClear( ( PHB_ITEM ) Cargo );
}

static const HB_GC_FUNCS s_gcGripFuncs =
{
   hb_gcGripRelease,
   hb_gcGripMark
};

PHB_ITEM hb_gcGripGet( PHB_ITEM pOrigin )
{
   PHB_GARBAGE pAlloc = HB_GARBAGE_NEW( sizeof( HB_ITEM ) );
   PHB_ITEM pItem = ( PHB_ITEM ) HB_BLOCK_PTR( pAlloc );

   pAlloc->pFuncs = &s_gcGripFuncs;
   pAlloc->locked = 1;
   pAlloc->used   = s_uUsedFlag;

   pItem->type = HB_IT_NIL;

   HB_GC_LOCK();
   hb_gcLink( &s_pLockedBlock, pAlloc );
   HB_GC_UNLOCK();

   if( pOrigin )
      hb_itemCopy( pItem, pOrigin );

   return pItem;
}

void hb_gcGripDrop( PHB_ITEM pItem )
{
   hb_gcRefFree( pItem );
}

/* Lock a memory pointer so it will not be released if stored
   outside of harbour variables
 */
void * hb_gcLock( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

      HB_GC_LOCK();
      if( ! pAlloc->locked )
      {
         hb_gcUnlink( &s_pCurrBlock, pAlloc );
         hb_gcLink( &s_pLockedBlock, pAlloc );
         HB_GC_AUTO_DEC();
      }
      ++pAlloc->locked;
      HB_GC_UNLOCK();
   }

   return pBlock;
}

/* Unlock a memory pointer so it can be released if there is no
   references inside of harbour variables
 */
void * hb_gcUnlock( void * pBlock )
{
   if( pBlock )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

      if( pAlloc->locked )
      {
         HB_GC_LOCK();
         if( pAlloc->locked )
         {
            if( --pAlloc->locked == 0 )
            {
               pAlloc->used = s_uUsedFlag;

               hb_gcUnlink( &s_pLockedBlock, pAlloc );
               hb_gcLink( &s_pCurrBlock, pAlloc );
               HB_GC_AUTO_INC();
            }
         }
         HB_GC_UNLOCK();
      }
   }
   return pBlock;
}

void hb_gcAttach( void * pBlock )
{
   PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

   if( pAlloc->locked )
   {
      HB_GC_LOCK();
      if( pAlloc->locked )
      {
         if( --pAlloc->locked == 0 )
         {
            pAlloc->used = s_uUsedFlag;

            hb_gcUnlink( &s_pLockedBlock, pAlloc );
            hb_gcLink( &s_pCurrBlock, pAlloc );
            HB_GC_AUTO_INC();
            pAlloc = NULL;
         }
      }
      HB_GC_UNLOCK();
   }
   if( pAlloc )
      hb_xRefInc( pAlloc );
}

/* mark passed memory block as used so it will be not released by the GC */
void hb_gcMark( void * pBlock )
{
   PHB_GARBAGE pAlloc = HB_GC_PTR( pBlock );

   if( pAlloc->used == s_uUsedFlag )
   {
      pAlloc->used ^= HB_GC_USED_FLAG;  /* mark this codeblock as used */
      pAlloc->pFuncs->mark( pBlock );
   }
}

/* Mark a passed item as used so it will be not released by the GC
 */
void hb_gcItemRef( PHB_ITEM pItem )
{
   while( HB_IS_BYREF( pItem ) )
   {
      if( HB_IS_ENUM( pItem ) )
         return;
      else if( HB_IS_EXTREF( pItem ) )
      {
         pItem->item.asExtRef.func->mark( pItem->item.asExtRef.value );
         return;
      }
      else if( ! HB_IS_MEMVAR( pItem ) &&
               pItem->item.asRefer.offset == 0 &&
               pItem->item.asRefer.value >= 0 )
      {
         /* array item reference */
         PHB_GARBAGE pAlloc = HB_GC_PTR( pItem->item.asRefer.BasePtr.array );
         if( pAlloc->used == s_uUsedFlag )
         {
            /* mark this array as used */
            pAlloc->used ^= HB_GC_USED_FLAG;
            /* mark also all array elements */
            pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
         }
         return;
      }
      pItem = hb_itemUnRefOnce( pItem );
   }

   if( HB_IS_ARRAY( pItem ) )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pItem->item.asArray.value );

      /* Check this array only if it was not checked yet */
      if( pAlloc->used == s_uUsedFlag )
      {
         /* mark this array as used so it will be no re-checked from
          * other references
          */
         pAlloc->used ^= HB_GC_USED_FLAG;
         /* mark also all array elements */
         pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( HB_IS_HASH( pItem ) )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pItem->item.asHash.value );

      /* Check this hash table only if it was not checked yet */
      if( pAlloc->used == s_uUsedFlag )
      {
         /* mark this hash table as used */
         pAlloc->used ^= HB_GC_USED_FLAG;
         /* mark also all hash elements */
         pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( HB_IS_BLOCK( pItem ) )
   {
      PHB_GARBAGE pAlloc = HB_GC_PTR( pItem->item.asBlock.value );

      if( pAlloc->used == s_uUsedFlag )
      {
         /* mark this codeblock as used */
         pAlloc->used ^= HB_GC_USED_FLAG;
         /* mark as used all detached variables in a codeblock */
         pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
      }
   }
   else if( HB_IS_POINTER( pItem ) )
   {
      if( pItem->item.asPointer.collect )
      {
         PHB_GARBAGE pAlloc = HB_GC_PTR( pItem->item.asPointer.value );

         if( pAlloc->used == s_uUsedFlag )
         {
            /* mark this memory block as used */
            pAlloc->used ^= HB_GC_USED_FLAG;
            /* mark also all internal user blocks attached to this block */
            pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
         }
      }
   }
   /* all other data types don't need the GC */
}

void hb_gcCollect( void )
{
   /* TODO: decrease the amount of time spend collecting */
   hb_gcCollectAll( HB_FALSE );
}

/* Check all memory block if they can be released
 */
void hb_gcCollectAll( HB_BOOL fForce )
{
   /* MTNOTE: it's not necessary to protect s_bCollecting with mutex
    *         because it can be changed at RT only inside this procedure
    *         when all other threads are stoped by hb_vmSuspendThreads(),
    *         [druzus]
    */
   if( ! s_bCollecting && hb_vmSuspendThreads( fForce ) )
   {
      PHB_GARBAGE pAlloc, pDelete;

      if( ! s_pCurrBlock || s_bCollecting )
      {
         hb_vmResumeThreads();
         return;
      }

      s_bCollecting = HB_TRUE;

      /* Step 1 - mark */
      /* All blocks are already marked because we are flipping
       * the used/unused flag
       */

      /* Step 2 - sweep */
      /* check all known places for blocks they are referring */
      hb_vmIsStackRef();
      hb_vmIsStaticRef();
      hb_clsIsClassRef();

      /* check list of locked block for blocks referenced from
       * locked block
       */
      if( s_pLockedBlock )
      {
         pAlloc = s_pLockedBlock;
         do
         {
            pAlloc->pFuncs->mark( HB_BLOCK_PTR( pAlloc ) );
            pAlloc = pAlloc->pNext;
         }
         while( s_pLockedBlock != pAlloc );
      }

      /* Step 3 - finalize */
      /* Release all blocks that are still marked as unused */

      /*
       * infinite loop can appear when we are executing clean-up functions
       * scanning s_pCurrBlock. It's possible that one of them will free
       * the GC block which we are using as stop condition. Only blocks
       * for which we set HB_GC_DELETE flag are guarded against releasing.
       * To avoid such situation first we are moving blocks which will be
       * deleted to separate list. It's additional operation but it can
       * even increase the speed when we are deleting only few percent
       * of all allocated blocks because in next passes we will scan only
       * deleted block list. [druzus]
       */

      pAlloc = NULL; /* for stop condition */
      do
      {
         if( s_pCurrBlock->used == s_uUsedFlag )
         {
            pDelete = s_pCurrBlock;
            pDelete->used |= HB_GC_DELETE | HB_GC_DELETELST;
            hb_gcUnlink( &s_pCurrBlock, pDelete );
            hb_gcLink( &s_pDeletedBlock, pDelete );
            HB_GC_AUTO_DEC();
         }
         else
         {
            /* at least one block will not be deleted, set new stop condition */
            if( ! pAlloc )
               pAlloc = s_pCurrBlock;
            s_pCurrBlock = s_pCurrBlock->pNext;
         }

      }
      while( pAlloc != s_pCurrBlock );

      /* Step 4 - flip flag */
      /* Reverse used/unused flag so we don't have to mark all blocks
       * during next collecting
       */
      s_uUsedFlag ^= HB_GC_USED_FLAG;

#ifdef HB_GC_AUTO
      /* store number of marked blocks for automatic GC activation */
      s_ulBlocksMarked = s_ulBlocks;
      if( s_ulBlocksAuto == 0 )
         s_ulBlocksCheck = HB_GC_AUTO_MAX;
      else
      {
         s_ulBlocksCheck = s_ulBlocksMarked + s_ulBlocksAuto;
         if( s_ulBlocksCheck <= s_ulBlocksMarked )
            s_ulBlocksCheck = HB_GC_AUTO_MAX;
      }
#endif

      /* call memory manager cleanup function */
      hb_xclean();

      /* resume suspended threads */
      hb_vmResumeThreads();

      /* do we have any deleted blocks? */
      if( s_pDeletedBlock )
      {
         /* call a cleanup function */
         pAlloc = s_pDeletedBlock;
         do
         {
            s_pDeletedBlock->pFuncs->clear( HB_BLOCK_PTR( s_pDeletedBlock ) );

            s_pDeletedBlock = s_pDeletedBlock->pNext;

         }
         while( pAlloc != s_pDeletedBlock );

         /* release all deleted blocks */
         do
         {
            pDelete = s_pDeletedBlock;
            hb_gcUnlink( &s_pDeletedBlock, pDelete );
            if( hb_xRefCount( pDelete ) != 0 )
            {
               pDelete->used = s_uUsedFlag;
               pDelete->locked = 0;
               HB_GC_LOCK();
               hb_gcLink( &s_pCurrBlock, pDelete );
               HB_GC_AUTO_INC();
               HB_GC_UNLOCK();
               if( hb_vmRequestQuery() == 0 )
                  hb_errRT_BASE( EG_DESTRUCTOR, 1302, NULL, "Reference to freed block", 0 );
            }
            else
               HB_GARBAGE_FREE( pDelete );

         }
         while( s_pDeletedBlock );
      }
      s_bCollecting = HB_FALSE;
   }
}


/* MTNOTE: It's executed at the end of HVM cleanup code just before
 *         application exit when other threads are destroyed, so it
 *         does not need additional protection code for MT mode, [druzus]
 */
void hb_gcReleaseAll( void )
{
   if( s_pCurrBlock )
   {
      PHB_GARBAGE pAlloc, pDelete;

      s_bCollecting = HB_TRUE;

      pAlloc = s_pCurrBlock;
      do
      {
         /* call a cleanup function */
         s_pCurrBlock->used |= HB_GC_DELETE | HB_GC_DELETELST;
         s_pCurrBlock->pFuncs->clear( HB_BLOCK_PTR( s_pCurrBlock ) );

         s_pCurrBlock = s_pCurrBlock->pNext;

      }
      while( s_pCurrBlock && pAlloc != s_pCurrBlock );

      do
      {
         HB_TRACE( HB_TR_INFO, ( "Release %p", s_pCurrBlock ) );
         pDelete = s_pCurrBlock;
         hb_gcUnlink( &s_pCurrBlock, pDelete );
         HB_GC_AUTO_DEC();
         HB_GARBAGE_FREE( pDelete );

      }
      while( s_pCurrBlock );
   }

   s_bCollecting = HB_FALSE;
}

/* service a single garbage collector step
 * Check a single memory block if it can be released
 */
HB_FUNC( HB_GCSTEP )
{
   hb_gcCollect();
}

/* Check all memory blocks if they can be released
 */
HB_FUNC( HB_GCALL )
{
   HB_STACK_TLS_PRELOAD

   /* call hb_ret() to clear stack return item, HVM does not clean
    * it before calling functions/procedures if caller does not
    * try to retrieve returned value. It's safe and cost nearly
    * nothing in whole GC scan process. It may help when previously
    * called function returned complex item with cross references.
    * It's quite common situation that people executes hb_gcAll()
    * immediately after such function. [druzus]
    */
   hb_ret();

   hb_gcCollectAll( hb_pcount() < 1 || hb_parl( 1 ) );
}

#ifdef HB_GC_AUTO
HB_FUNC( HB_GCSETAUTO )
{
   HB_STACK_TLS_PRELOAD

   HB_PTRUINT nBlocks, nPrevBlocks;
   HB_BOOL fSet = HB_ISNUM( 1 );

   nBlocks = fSet ? hb_parnint( 1 ) * 1000 : 0;

   HB_GC_LOCK();
   nPrevBlocks = s_ulBlocksAuto;
   if( fSet )
   {
      s_ulBlocksAuto = nBlocks;
      if( s_ulBlocksAuto == 0 )
         s_ulBlocksCheck = HB_GC_AUTO_MAX;
      else
      {
         s_ulBlocksCheck = s_ulBlocksMarked + s_ulBlocksAuto;
         if( s_ulBlocksCheck <= s_ulBlocksMarked )
            s_ulBlocksCheck = HB_GC_AUTO_MAX;
      }
   }
   HB_GC_UNLOCK();

   hb_retnint( nPrevBlocks / 1000 );
}
#endif
