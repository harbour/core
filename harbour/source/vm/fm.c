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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_xmemcpy()
 *    hb_xmemset()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_xquery()
 *    MEMORY()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSMISC
#define INCL_DOSERRORS
#define INCL_DOSPROCESS

/* malloc.h has been obsoleted by stdlib.h, which is included via
   hbvmpub.h, which is include via hbapi.h
   #include <malloc.h>
*/

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbmemory.ch"
#include "hbdate.h"

/* #define HB_PARANOID_MEM_CHECK */

/*#undef HB_FM_STATISTICS*/
#ifndef HB_FM_STATISTICS
#  undef HB_PARANOID_MEM_CHECK
#endif

#if defined(HB_FM_STATISTICS) && !defined(HB_TR_LEVEL)
   #define HB_TR_LEVEL HB_TR_ERROR
#endif


#ifdef HB_FM_STATISTICS

#ifndef HB_MEMFILER
#  define HB_MEMFILER  0xff
#endif
#define HB_MEMINFO_SIGNATURE 0x19730403

typedef struct _HB_MEMINFO
{
   UINT32      u32Signature;
   ULONG       ulSize;
   USHORT      uiProcLine;
   char        szProcName[ HB_SYMBOL_NAME_LEN + 1 ];
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE     ( ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                                  ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT ) + \
                                HB_COUNTER_OFFSET )
#else
#  define HB_MEMINFO_SIZE     ( sizeof( HB_MEMINFO ) + HB_COUNTER_OFFSET )
#endif

#define HB_ALLOC_SIZE( n )    ( ( n ) + HB_MEMINFO_SIZE + sizeof( UINT32 ) )
#define HB_FM_PTR( p )        ( ( PHB_MEMINFO ) ( ( BYTE * ) ( p ) - HB_MEMINFO_SIZE ) )

#define HB_FM_GETSIG( p, n )  HB_GET_UINT32( ( BYTE * )( p ) + ( n ) )
#define HB_FM_SETSIG( p, n )  HB_PUT_UINT32( ( BYTE * )( p ) + ( n ), HB_MEMINFO_SIGNATURE )
#define HB_FM_CLRSIG( p, n )  HB_PUT_UINT32( ( BYTE * )( p ) + ( n ), 0 )

/* NOTE: we cannot use here HB_TRACE because it will overwrite the
 * function name/line number of code which called hb_xalloc/hb_xgrab
 */
#define HB_TRACE_FM           HB_TRACE_STEALTH

static LONG s_lMemoryBlocks = 0;      /* memory blocks used */
static LONG s_lMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static LONG s_lMemoryMaxConsumed = 0; /* memory size consumed */
static LONG s_lMemoryConsumed = 0;    /* memory max size consumed */

static PHB_MEMINFO s_pFirstBlock = NULL;
static PHB_MEMINFO s_pLastBlock = NULL;

#else /* ! HB_FM_STATISTICS */

typedef void * PHB_MEMINFO;
#define HB_MEMINFO_SIZE       HB_COUNTER_OFFSET
#define HB_ALLOC_SIZE( n )    ( ( n ) + HB_MEMINFO_SIZE )
#define HB_FM_PTR( p )        HB_COUNTER_PTR( p )
#define HB_TRACE_FM           HB_TRACE

#endif /* HB_FM_STATISTICS */

#define HB_MEM_PTR( p )       ( ( void * ) ( ( BYTE * ) ( p ) + HB_MEMINFO_SIZE ) )


HB_EXPORT void * hb_xalloc( ULONG ulSize )         /* allocates fixed memory, returns NULL on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xalloc(%lu)", ulSize));

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XALLOCNULLSIZE, NULL, NULL, NULL );

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( ulSize ) );

   if( ! pMem )
      return pMem;

#ifdef HB_FM_STATISTICS
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
   HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
   pMem->ulSize = ulSize;  /* size of the memory block */

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
      * for memory leaks - this is why we are using the previously stored
      * function/line info - this is a location of code that called
      * hb_xalloc/hb_xgrab
      */
      pMem->uiProcLine = hb_tr_line_; /* C line number */
      strcpy( pMem->szProcName, hb_tr_file_ );
   }
   else
   {
      hb_stackBaseProcInfo( pMem->szProcName, &pMem->uiProcLine );
   }

   s_lMemoryConsumed += ulSize + sizeof( HB_COUNTER );
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

#ifdef HB_PARANOID_MEM_CHECK
   memset( HB_MEM_PTR( pMem ), HB_MEMFILER, ulSize );
#endif

#endif

   * HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ) = 1;

   return HB_MEM_PTR( pMem );
}

HB_EXPORT void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   PHB_MEMINFO pMem;

   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xgrab(%lu)", ulSize));

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XGRABNULLSIZE, NULL, NULL, NULL );

   pMem = ( PHB_MEMINFO ) malloc( HB_ALLOC_SIZE( ulSize ) );

   if( ! pMem )
      hb_errInternal( HB_EI_XGRABALLOC, NULL, NULL, NULL );

#ifdef HB_FM_STATISTICS
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
   HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
   pMem->ulSize = ulSize;  /* size of the memory block */

   if( hb_tr_level() >= HB_TR_DEBUG )
   {
      /* NOTE: PRG line number/procname is not very useful during hunting
      * for memory leaks - this is why we are using the previously stored
      * function/line info - this is a location of code that called
      * hb_xalloc/hb_xgrab
      */
      pMem->uiProcLine = hb_tr_line_; /* C line number */
      strcpy( pMem->szProcName, hb_tr_file_ );
   }
   else
   {
      hb_stackBaseProcInfo( pMem->szProcName, &pMem->uiProcLine );
   }

   s_lMemoryConsumed += ulSize + sizeof( HB_COUNTER );
   if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
      s_lMemoryMaxConsumed = s_lMemoryConsumed;
   s_lMemoryBlocks++;
   if( s_lMemoryMaxBlocks < s_lMemoryBlocks )
      s_lMemoryMaxBlocks = s_lMemoryBlocks;

#ifdef HB_PARANOID_MEM_CHECK
   memset( HB_MEM_PTR( pMem ), HB_MEMFILER, ulSize );
#endif

#endif

   * HB_COUNTER_PTR( HB_MEM_PTR( pMem ) ) = 1;

   return HB_MEM_PTR( pMem );
}

HB_EXPORT void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xrealloc(%p, %lu)", pMem, ulSize));

   if( ! pMem )
      hb_errInternal( HB_EI_XREALLOCNULL, NULL, NULL, NULL );

   if( ulSize == 0 )
      hb_errInternal( HB_EI_XREALLOCNULLSIZE, NULL, NULL, NULL );

#ifdef HB_FM_STATISTICS
   {
      PHB_MEMINFO pMemBlock;
      ULONG ulMemSize;

      pMemBlock = HB_FM_PTR( pMem );

      if( pMemBlock->u32Signature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XREALLOCINV, NULL, NULL, NULL );

      ulMemSize = pMemBlock->ulSize;

      if( HB_FM_GETSIG( pMem, ulMemSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, NULL, NULL, NULL );

      HB_FM_CLRSIG( pMem, ulMemSize );

#ifdef HB_PARANOID_MEM_CHECK
      pMem = malloc( HB_ALLOC_SIZE( ulSize ) );
      if( pMem )
      {
         if( ulSize > ulMemSize )
         {
            memcpy( pMem, pMemBlock, HB_ALLOC_SIZE( ulMemSize ) );
            memset( ( BYTE * ) pMem + HB_ALLOC_SIZE( ulMemSize ), HB_MEMFILER, ulSize - ulMemSize );
         }
         else
            memcpy( pMem, pMemBlock, HB_ALLOC_SIZE( ulSize ) );
      }
      memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( ulMemSize ) );
      free( pMemBlock );
#else
      pMem = realloc( pMemBlock, HB_ALLOC_SIZE( ulSize ) );
#endif

      s_lMemoryConsumed += ( ulSize - ulMemSize );
      if( s_lMemoryMaxConsumed < s_lMemoryConsumed )
         s_lMemoryMaxConsumed = s_lMemoryConsumed;

      if( ! pMem )
         hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );

      ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;  /* size of the memory block */
      HB_FM_SETSIG( HB_MEM_PTR( pMem ), ulSize );
      if( ( ( PHB_MEMINFO ) pMem )->pPrevBlock )
         ( ( PHB_MEMINFO ) pMem )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pMem;
      if( ( ( PHB_MEMINFO ) pMem )->pNextBlock )
         ( ( PHB_MEMINFO ) pMem )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pMem;

      if( s_pFirstBlock == pMemBlock )
         s_pFirstBlock = ( PHB_MEMINFO ) pMem;
      if( s_pLastBlock == pMemBlock )
         s_pLastBlock = ( PHB_MEMINFO ) pMem;
   }
#else

   pMem = realloc( HB_FM_PTR( pMem ), HB_ALLOC_SIZE( ulSize ) );
   if( !pMem )
      hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );

#endif

   return HB_MEM_PTR( pMem );
}

HB_EXPORT void hb_xfree( void * pMem )            /* frees fixed memory */
{
   HB_TRACE_FM(HB_TR_DEBUG, ("hb_xfree(%p)", pMem));

   if( pMem )
   {
#ifdef HB_FM_STATISTICS

      PHB_MEMINFO pMemBlock = HB_FM_PTR( pMem );

      if( pMemBlock->u32Signature != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XFREEINV, NULL, NULL, NULL );

      if( HB_FM_GETSIG( pMem, pMemBlock->ulSize ) != HB_MEMINFO_SIGNATURE )
         hb_errInternal( HB_EI_XMEMOVERFLOW, NULL, NULL, NULL );

      s_lMemoryConsumed -= pMemBlock->ulSize + sizeof( HB_COUNTER );
      s_lMemoryBlocks--;

      if( pMemBlock->pPrevBlock )
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;
      else
         s_pFirstBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;
      else
         s_pLastBlock = pMemBlock->pPrevBlock;

      pMemBlock->u32Signature = 0;
      HB_FM_CLRSIG( pMem, pMemBlock->ulSize );

#ifdef HB_PARANOID_MEM_CHECK
      memset( pMemBlock, HB_MEMFILER, HB_ALLOC_SIZE( pMemBlock->ulSize ) );
#endif

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
   ++( * HB_COUNTER_PTR( pMem ) );
}

/* decrement reference counter, return TRUE when 0 reached */
#undef hb_xRefDec
BOOL hb_xRefDec( void * pMem )
{
   return --( * HB_COUNTER_PTR( pMem ) ) == 0;
}

/* decrement reference counter and free the block when 0 reached */
#undef hb_xRefFree
void hb_xRefFree( void * pMem )
{
#ifdef HB_FM_STATISTICS

   if( HB_FM_PTR( pMem )->u32Signature != HB_MEMINFO_SIGNATURE )
      hb_errInternal( HB_EI_XFREEINV, NULL, NULL, NULL );

   if( --( * HB_COUNTER_PTR( pMem ) ) == 0 )
      hb_xfree( pMem );

#else

   if( --( * HB_COUNTER_PTR( pMem ) ) == 0 )
      free( HB_FM_PTR( pMem ) );

#endif
}

/* return number of references */
#undef hb_xRefCount
HB_COUNTER hb_xRefCount( void * pMem )
{
   return * HB_COUNTER_PTR( pMem );
}

/* reallocates memory, create copy if reference counter greater then 1 */
#undef hb_xRefResize
void * hb_xRefResize( void * pMem, ULONG ulSave, ULONG ulSize )
{

#ifdef HB_FM_STATISTICS
   if( * HB_COUNTER_PTR( pMem ) > 1 )
   {
      void * pMemNew = hb_xgrab( ulSize );

      --( * HB_COUNTER_PTR( pMem ) );
      memcpy( pMemNew, pMem, HB_MIN( ulSave, ulSize ) );
      return pMemNew;
   }

   return hb_xrealloc( pMem, ulSize );

#else

   if( * HB_COUNTER_PTR( pMem ) > 1 )
   {
      void * pMemNew = malloc( HB_ALLOC_SIZE( ulSize ) );

      if( pMemNew )
      {
         --( * HB_COUNTER_PTR( pMem ) );
         * HB_COUNTER_PTR( HB_MEM_PTR( pMemNew ) ) = 1;
         memcpy( HB_MEM_PTR( pMemNew ), pMem, HB_MIN( ulSave, ulSize ) );
         return HB_MEM_PTR( pMemNew );
      }
   }
   else
   {
      pMem = realloc( HB_FM_PTR( pMem ), HB_ALLOC_SIZE ( ulSize ) );
      if( pMem )
         return HB_MEM_PTR( pMem );
   }

   hb_errInternal( HB_EI_XREALLOC, NULL, NULL, NULL );
   return NULL;
#endif
}

/* NOTE: Debug function, it will always return 0 when HB_FM_STATISTICS is
         not defined, don't use it for final code [vszakats] */

HB_EXPORT ULONG  hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xsize(%p)", pMem));

#ifdef HB_FM_STATISTICS
   return HB_FM_PTR( pMem )->ulSize;
#else
   HB_SYMBOL_UNUSED( pMem );

   return 0;
#endif
}

HB_EXPORT void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xinit()"));
}

/* Returns pointer to string containing printable version
   of pMem memory block */

#ifdef HB_FM_STATISTICS
static char * hb_mem2str( char * membuffer, void * pMem, UINT uiSize )
{
   char *cMem = ( char * ) pMem;
   UINT uiIndex, uiPrintable;

   uiPrintable = 0;
   for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
      if( ( cMem[ uiIndex ] & 0x60 ) != 0 )
         uiPrintable++;

   if( uiPrintable * 100 / uiSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
         if( cMem[ uiIndex ] >= ' ' )
            membuffer[ uiIndex ] = cMem[ uiIndex ];
         else
            membuffer[ uiIndex ] = '.';
      membuffer[ uiIndex ] = '\0';
   }
   else
   {
     /* format as hex */
      for( uiIndex = 0; uiIndex < uiSize; uiIndex++ )
      {
         int lownibble, hinibble;
         hinibble = cMem[ uiIndex ] >> 4;
         lownibble = cMem[ uiIndex ] & 0x0F;
         membuffer[ uiIndex * 2 ]     = hinibble <= 9 ?
                               ( '0' + hinibble ) : ( 'A' + hinibble - 10 );
         membuffer[ uiIndex * 2 + 1 ] = lownibble <= 9 ?
                               ( '0' + lownibble ) : ( 'A' + lownibble - 10 );
      }
      membuffer[ uiIndex * 2 ] = '\0';
   }

   return membuffer;
}

#define HB_MAX_MEM2STR_BLOCK 256
HB_EXPORT void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));

   if( s_lMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      char membuffer[ HB_MAX_MEM2STR_BLOCK * 2 + 1 ]; /* multiplied by 2 to allow hex format */
      PHB_MEMINFO pMemBlock;
      USHORT ui;
      char buffer[ 100 ];
      FILE *hLog = NULL;

      if( s_lMemoryBlocks )
          hLog = fopen( "fm.log", "a+" );

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      sprintf( buffer, "Total memory allocated: %li bytes (%li blocks)", s_lMemoryMaxConsumed, s_lMemoryMaxBlocks );
      hb_conOutErr( buffer, 0 );

      if ( hLog )
      {
         char szTime[ 9 ];
         int iYear, iMonth, iDay;

         hb_dateToday( &iYear, &iMonth, &iDay );
         hb_dateTimeStr( szTime );

         fprintf( hLog, "Memory Allocation Report : Application => %s\n", hb_cmdargARGV()[0] );
         fprintf( hLog, "  Application terminated at : %04d.%02d.%02d %s\n", iYear, iMonth, iDay, szTime );
         fprintf( hLog, "%s\n", buffer );
      }

      if( s_lMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         sprintf( buffer, "WARNING! Memory allocated but not released: %li bytes (%li blocks)", s_lMemoryConsumed, s_lMemoryBlocks );
         hb_conOutErr( buffer, 0 );

         if ( hLog ) fprintf( hLog, "%s\n", buffer );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( ui = 1, pMemBlock = s_pFirstBlock; pMemBlock; pMemBlock = pMemBlock->pNextBlock, ++ui )
      {
         HB_TRACE( HB_TR_ERROR, ( "Block %i (size %lu) %s(%i), \"%s\"", ui,
            pMemBlock->ulSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
            hb_mem2str( membuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                        HB_MIN( pMemBlock->ulSize, HB_MAX_MEM2STR_BLOCK ) ) ) );

         if( hLog )
         {
            fprintf( hLog, "Block %i %p (size %lu) %s(%i), \"%s\"\n", ui,
               (char *) pMemBlock + HB_MEMINFO_SIZE,
               pMemBlock->ulSize, pMemBlock->szProcName, pMemBlock->uiProcLine,
               hb_mem2str( membuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                           HB_MIN( pMemBlock->ulSize, HB_MAX_MEM2STR_BLOCK ) ) );
         }
      }
      if( hLog )
      {
         fprintf( hLog, "------------------------------------------------------------------------\n");
         fclose( hLog );
      }
   }
}

#else

HB_EXPORT void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_xexit()"));
}

#endif

/* hb_xmemcpy and hb_xmemset are only needed when
   unsigned int and unsigned long differ in length */

/* unfortunately it's not true - on 64bit platforms int is 32 bit
   and long is 64.
   we need these functions only when max(size_t) < max(long)
   what could be detected and set in header files. Here check
   only for hb_xmem* macro definition

   #if UINT_MAX != ULONG_MAX
*/
#ifndef hb_xmemcpy
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
#endif

#ifndef hb_xmemset
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

   /* TODO: Return the correct values instead of 9999 [vszakats] */

   switch( uiMode )
   {
   case HB_MEM_CHAR:       /*               (Free Variable Space [KB])          */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         ULONG ulSysInfo = 0;

         if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
            ulResult = 0;
         else
            ulResult = ulSysInfo / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_BLOCK:      /*               (Largest String [KB])               */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = HB_MIN( memorystatus.dwAvailPhys, ULONG_MAX ) / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         ULONG ulSysInfo = 0;

         if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
            ulResult = 0;
         else
            ulResult = HB_MIN( ulSysInfo, ULONG_MAX ) / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_RUN:        /*               (RUN Memory [KB])                   */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         ULONG ulSysInfo = 0;

         if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
            ulResult = 0;
         else
            ulResult = ulSysInfo / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_VM:         /* UNDOCUMENTED! (Virtual Memory [KB])               */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailVirtual / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         ULONG ulSysInfo = 0;

         if( DosQuerySysInfo( QSV_TOTAVAILMEM, QSV_TOTAVAILMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
            ulResult = 0;
         else
            ulResult = ulSysInfo / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_EMS:        /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?)     */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         ulResult = 0;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_FM:         /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?)        */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwTotalPhys / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         ULONG ulSysInfo = 0;

         if( DosQuerySysInfo( QSV_MAXPRMEM, QSV_MAXPRMEM, &ulSysInfo, sizeof( ULONG ) ) != NO_ERROR )
            ulResult = 0;
         else
            ulResult = ulSysInfo / 1024;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_FMSEGS:     /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
         ulResult = 1;
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_SWAP:       /* UNDOCUMENTED! (Free Swap Memory [KB])             */
      #if defined(HB_OS_WIN_32)
      {
         MEMORYSTATUS memorystatus;
         GlobalMemoryStatus( &memorystatus );
         ulResult = memorystatus.dwAvailPageFile / 1024;
      }
      #elif defined(HB_OS_OS2)
      {
         /* NOTE: There is no way to know how much a swap file can grow on an
                  OS/2 system. I think we should return free space on DASD
                  media which contains swap file [maurilio.longo] */
         ulResult = 9999;
      }
      #else
         ulResult = 9999;
      #endif
      break;

   case HB_MEM_CONV:       /* UNDOCUMENTED! (Free Conventional [KB])            */
      #if defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
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

   case HB_MEM_STACKITEMS: /* Harbour extension (Total items allocated for the stack)      */
      ulResult = hb_stackTotalItems();
      break;

   case HB_MEM_STACK:      /* Harbour extension (Total memory size used by the stack [bytes]) */
      ulResult = hb_stackTotalItems() * sizeof( HB_ITEM );
      break;

   case HB_MEM_STACK_TOP : /* Harbour extension (Total items currently on the stack)      */
      ulResult = hb_stackTopOffset( );
      break;

   default:
      ulResult = 0;
   }

   return ulResult;
}

HB_FUNC( MEMORY )
{
   hb_retnl( hb_xquery( hb_parni( 1 ) ) );
}

#ifdef HB_FM_STATISTICS
HB_FUNC( HB_FM_STAT ) {}
#else
HB_FUNC( HB_FM_NOSTAT ) {}
#endif
