/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Fixed Memory API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_xmemcpy()
 *    hb_xmemset()
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    hb_xquery()
 *    HB_MEMORY()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* TODO: hb_xrealloc() is not completely consistent now, since it will
         destroy the pointer when called with a zero parameter, and return NULL.
         But will not accept NULL pointer as a parameter. [vszel] */

/* NOTE: If you turn this on, the memory subsystem will collect information
         about several statistical data about memory management, it will show
         these on exit if memory seem to have leaked.
         This should be normally turned off in a final release */
//#define HB_FM_STATISTICS

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#ifndef __MPW__
   #include <malloc.h>
#endif

#include "extend.h"
#include "errorapi.h"
#include "hbmemory.ch"

#ifdef HB_FM_STATISTICS

#define HB_MEMINFO_SIGNATURE 0x19730403

typedef struct _HB_MEMINFO
{
   ULONG  ulSignature;
   ULONG  ulSize;
   USHORT uiProcLine;
   char   szProcName[ HB_SYMBOL_NAME_LEN + 1 ];
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
} HB_MEMINFO, * PHB_MEMINFO;

static LONG s_lMemoryBlocks = 0;      /* memory blocks used */
static LONG s_lMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static LONG s_lMemoryMaxConsumed = 0; /* memory size consumed */
static LONG s_lMemoryConsumed = 0;    /* memory max size consumed */

static PHB_MEMINFO s_pFirstBlock = NULL;
static PHB_MEMINFO s_pLastBlock = NULL;

#endif

void * hb_xalloc( ULONG ulSize )         /* allocates fixed memory, returns NULL on failure */
{
#ifdef HB_FM_STATISTICS

   void * pMem;

   HB_TRACE(HB_TR_DEBUG, ("hb_xalloc(%lu)", ulSize));

   pMem = malloc( ulSize + sizeof( HB_MEMINFO ) );

   if( ! pMem )
      return pMem;

   if( ! s_pFirstBlock )
   {
      s_pFirstBlock = ( PHB_MEMINFO ) pMem;
      s_pLastBlock  = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = s_pLastBlock;
      s_pLastBlock = ( PHB_MEMINFO ) pMem;
   }
   ( ( PHB_MEMINFO ) pMem )->pNextBlock = NULL;

   ( ( PHB_MEMINFO ) pMem )->ulSignature = HB_MEMINFO_SIGNATURE;
   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */
   if( hb_stack.pItems && ( hb_stack.pBase != hb_stack.pItems ) )
   {
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = hb_stack.pBase->item.asSymbol.lineno; /* PRG line number */
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName,
              hb_stack.pBase->item.asSymbol.value->szName ); /* PRG ProcName */
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = 0; /* PRG line number */
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, "" ); /* PRG ProcName */
   }

   s_lMemoryConsumed    += ulSize;
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

   HB_TRACE(HB_TR_DEBUG, ("hb_xalloc(%lu)", ulSize));

   return ( char * ) malloc( ulSize );

#endif
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem;

   HB_TRACE(HB_TR_DEBUG, ("hb_xgrab(%lu)", ulSize));

#ifdef HB_FM_STATISTICS

   pMem = malloc( ulSize + sizeof( HB_MEMINFO ) );

   if( ! pMem )
      hb_errInternal( 9999, "hb_xgrab can't allocate memory", NULL, NULL );

   if( ! s_pFirstBlock )
   {
      s_pFirstBlock = ( PHB_MEMINFO ) pMem;
      s_pLastBlock  = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = s_pLastBlock;
      s_pLastBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
      s_pLastBlock = ( PHB_MEMINFO ) pMem;
   }
   ( ( PHB_MEMINFO ) pMem )->pNextBlock = NULL;

   ( ( PHB_MEMINFO ) pMem )->ulSignature = HB_MEMINFO_SIGNATURE;
   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */
   if( hb_stack.pItems && ( hb_stack.pBase != hb_stack.pItems ) )
   {
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = hb_stack.pBase->item.asSymbol.lineno; /* PRG line number */
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName,
              hb_stack.pBase->item.asSymbol.value->szName ); /* PRG ProcName */
   }
   else
   {
      ( ( PHB_MEMINFO ) pMem )->uiProcLine = 0; /* PRG line number */
      strcpy( ( ( PHB_MEMINFO ) pMem )->szProcName, "" ); /* PRG ProcName */
   }

   s_lMemoryConsumed    += ulSize;
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

   pMem = malloc( ulSize );

   if( ! pMem )
      hb_errInternal( 9999, "hb_xgrab can't allocate memory", NULL, NULL );

   return ( char * ) pMem;

#endif
}

#ifdef HB_FM_STATISTICS

static void DeleteNode( PHB_MEMINFO pMemBlock )
{
   if( pMemBlock->pPrevBlock )
     pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
   else
     s_pFirstBlock = pMemBlock->pNextBlock;

   if( pMemBlock->pNextBlock )
      pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
   else
      s_pLastBlock = pMemBlock->pPrevBlock;
}

#endif

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
#ifdef HB_FM_STATISTICS

   PHB_MEMINFO pMemBlock;
   ULONG ulMemSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

   if( ! pMem )
      hb_errInternal( 9999, "hb_xrealloc called with a NULL pointer", NULL, NULL );

   pMemBlock = ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) );

   if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( 9999, "hb_xrealloc called with a bad pointer", NULL, NULL );

   ulMemSize = pMemBlock->ulSize;

   if( ulSize == 0 )
   {
      DeleteNode( pMemBlock );
      free( ( void * ) pMem );
      pMem = NULL;

      s_lMemoryBlocks--;
   }
   else
   {
      pMem = realloc( pMemBlock, ulSize + sizeof( HB_MEMINFO ) );

      if( pMem )
      {
         ( ( PHB_MEMINFO ) pMem )->pPrevBlock = pMemBlock->pPrevBlock;
         ( ( PHB_MEMINFO ) pMem )->pNextBlock = pMemBlock->pNextBlock;
         if( pMemBlock->pPrevBlock )
            ( ( PHB_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
         if( ( ( PHB_MEMINFO ) pMem )->pNextBlock )
            ( ( PHB_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pMem;
      }
   }

   s_lMemoryConsumed += ( ulSize - ulMemSize );
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;

   if( ! pMem )
      hb_errInternal( 9999, "hb_xrealloc can't reallocate memory", NULL, NULL );

   ( ( PHB_MEMINFO ) pMem )->ulSignature = HB_MEMINFO_SIGNATURE;
   ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */

   return ( char * ) pMem + sizeof( HB_MEMINFO );

#else

   HB_TRACE(HB_TR_DEBUG, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

   if( ! pMem )
      hb_errInternal( 9999, "hb_xrealloc called with a NULL pointer", NULL, NULL );

   if( ulSize == 0 )
   {
      free( ( void * ) pMem );
      pMem = NULL;
   }
   else
   {
      pMem = realloc( ( char * ) pMem, ulSize );

      if( ! pMem )
         hb_errInternal( 9999, "hb_xrealloc can't reallocate memory", NULL, NULL );
   }

   return ( char * ) pMem;

#endif
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
#ifdef HB_FM_STATISTICS

   PHB_MEMINFO pMemBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

   if( pMem )
   {
      pMemBlock = ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) );

      if( pMemBlock->ulSignature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( 9999, "hb_xfree called with a bad pointer", NULL, NULL );

      s_lMemoryConsumed -= ( ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) ) )->ulSize;
      s_lMemoryBlocks--;

      DeleteNode( pMemBlock );
      if( pMemBlock )
         free( ( void * ) pMemBlock );
   }
   else
      hb_errInternal( 9999, "hb_xfree called with a NULL pointer", NULL, NULL );

#else

   HB_TRACE(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

   if( pMem )
      free( ( char * ) pMem );
   else
      hb_errInternal( 9999, "hb_xfree called with a NULL pointer", NULL, NULL );

#endif
}

/* NOTE: Debug function, it will always return 0 when HB_FM_STATISTICS is
         not defined [vszel] */

ULONG hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xsize(%p)", pMem));

#ifdef HB_FM_STATISTICS
   return ( ( PHB_MEMINFO ) ( ( char * ) pMem - sizeof( HB_MEMINFO ) ) )->ulSize;
#else
   return 0;
#endif
}

void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xinit()"));
}

void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));

#ifdef HB_FM_STATISTICS
   if( s_lMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      /* NOTE: Put this in a separate block to optimize stack space usage [vszel] */
      {
         char buffer[ 100 ];

         hb_outerr( hb_consoleGetNewLine(), 0 );
         hb_outerr( "----------------------------------------", 0 );
         hb_outerr( hb_consoleGetNewLine(), 0 );
         sprintf( buffer, "Total memory allocated: %li bytes (%li blocks)", s_lMemoryMaxConsumed, s_lMemoryMaxBlocks );
         hb_outerr( buffer, 0 );

         if( s_lMemoryBlocks )
         {
            hb_outerr( hb_consoleGetNewLine(), 0 );
            sprintf( buffer, "WARNING! Memory allocated but not released: %li bytes (%li blocks)", s_lMemoryConsumed, s_lMemoryBlocks );
            hb_outerr( buffer, 0 );
         }
      }

      if( s_pFirstBlock )
      {
         char buffer[ HB_SYMBOL_NAME_LEN + 40 ];
         PHB_MEMINFO pMemBlock = s_pFirstBlock;
         USHORT ui = 1;

         do
         {
            sprintf( buffer, "%sblock %i: %s line %i", hb_consoleGetNewLine(), ui++, pMemBlock->szProcName,
                     pMemBlock->uiProcLine );
            HB_TRACE( HB_TR_ERROR, (buffer));
            pMemBlock = pMemBlock->pNextBlock;
         } while( pMemBlock );
      }
   }
#endif
}

#if UINT_MAX != ULONG_MAX

/* hb_xmemcpy and hb_xmemset are only needed when
   unsigned int and unsigned long differ in length */

void * hb_xmemcpy( void * pDestArg, void * pSourceArg, ULONG ulLen )
{
   BYTE * pDest;
   BYTE * pSource;
   ULONG  ulRemaining;
   int    iCopySize;

   HB_TRACE(HB_TR_DEBUG, ("hb_xmemcpy(%p, %p, %lu)", pDestArg, pSourceArg, ulLen));

   pDest = ( BYTE * ) pDestArg;
   pSource = ( BYTE * ) pSourceArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the memcpy() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iCopySize = UINT_MAX;
         ulRemaining -= ( ULONG ) iCopySize;
      }
      else
      {
         iCopySize = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      memcpy( pDest, pSource, iCopySize );
      pDest += iCopySize;
      pSource += iCopySize;
   }
   return pDestArg;
}

void * hb_xmemset( void * pDestArg, int iFill, ULONG ulLen )
{
   BYTE * pDest;
   ULONG  ulRemaining;
   int    iSetSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_xmemset(%p, %d, %lu)", pDestArg, iFill, ulLen));

   pDest = ( BYTE * ) pDestArg;
   ulRemaining = ulLen;

   while( ulRemaining )
   {
      /* Overcome the memset() size_t limitation */
      if( ulRemaining > UINT_MAX )
      {
         iSetSize = UINT_MAX;
         ulRemaining -= ( ULONG ) iSetSize;
      }
      else
      {
         iSetSize = ( int ) ulRemaining;
         ulRemaining = 0;
      }
      memset( pDest, iFill, iSetSize );
      pDest += iSetSize;
   }
   return pDestArg;
}

#endif

ULONG hb_xquery( USHORT uiMode )
{
   ULONG ulResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_xquery(%hu)", uiMode));

   /* TODO: Return the correct values instead of 9999 [vszel] */

   switch( uiMode )
   {
   case HB_MEM_CHAR:       /*               (Free Variable Space [KB])          */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_BLOCK:      /*               (Largest String [KB])               */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = HB_MIN_( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_RUN:        /*               (RUN Memory [KB])                   */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_VM:         /* UNDOCUMENTED! (Virtual Memory [KB])               */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailVirtual / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_EMS:        /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?)     */
      #if defined(_Windows) || defined(WINNT)
         ulResult = 0;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_FM:         /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?)        */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwTotalPhys / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_FMSEGS:     /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
      #if defined(_Windows) || defined(WINNT)
         ulResult = 1;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_SWAP:       /* UNDOCUMENTED! (Free Swap Memory [KB])             */
      #if defined(_Windows) || defined(WINNT)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPageFile / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_CONV:       /* UNDOCUMENTED! (Free Conventional [KB])            */
      #if defined(_Windows) || defined(WINNT)
         ulResult = 0;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_EMSUSED:    /* UNDOCUMENTED! (Used Expanded Memory [KB]) (?)     */
      ulResult = 0;
      break;

   case HB_MEM_USED:       /* Harbour extension (Memory used [bytes])           */
#ifdef HB_FM_STATISTICS
      ulResult = s_lMemoryConsumed;
#else
      ulResult = 0;
#endif
      break;

   case HB_MEM_USEDMAX:    /* Harbour extension (Maximum memory used [bytes])   */
#ifdef HB_FM_STATISTICS
      ulResult = s_lMemoryMaxConsumed;
#else
      ulResult = 0;
#endif
      break;

   case HB_MEM_STACKITEMS: /* Harbour extension (Total items on the stack)      */
      ulResult = hb_stack.wItems;
      break;

   case HB_MEM_STACK:      /* Harbour extension (Total memory size used by the stack [bytes]) */
      ulResult = hb_stack.wItems * sizeof( HB_ITEM );
      break;

   default:
      ulResult = 0;
   }

   return ulResult;
}

HARBOUR HB_MEMORY( void )
{
   hb_retnl( hb_xquery( hb_parni( 1 ) ) );
}

