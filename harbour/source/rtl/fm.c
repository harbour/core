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
 *    HB_MEMORY()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: If you turn this on, the memory subsystem will collect information
         about several statistical data about memory management, it will show
         these on exit if memory seem to have leaked.
         This should be normally turned off in a final release */
#define HB_FM_STATISTICS

#ifndef __MPW__
   #include <malloc.h>
#endif

#include "extend.h"
#include "errorapi.h"

#ifdef HB_FM_STATISTICS
static ULONG s_ulMemoryBlocks = 0;      /* memory blocks used */
static ULONG s_ulMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static ULONG s_ulMemoryMaxConsumed = 0; /* memory size consumed */
static ULONG s_ulMemoryConsumed = 0;    /* memory max size consumed */
#endif

void * hb_xalloc( ULONG ulSize )         /* allocates fixed memory, returns NULL on failure */
{
   void * pMem;

   HB_TRACE(("hb_xalloc(%lu)", ulSize));

   pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      return pMem;
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   s_ulMemoryConsumed    += ulSize;
   if( s_ulMemoryConsumed > s_ulMemoryMaxConsumed )
      s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
   s_ulMemoryBlocks++;
   if( s_ulMemoryBlocks > s_ulMemoryMaxBlocks )
      s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
#endif

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem;

   HB_TRACE(("hb_xgrab(%lu)", ulSize));

   pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      hb_errInternal( 9999, "hb_xgrab can't allocate memory", NULL, NULL );
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   s_ulMemoryConsumed    += ulSize;
   if( s_ulMemoryConsumed > s_ulMemoryMaxConsumed )
      s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
   s_ulMemoryBlocks++;
   if( s_ulMemoryBlocks > s_ulMemoryMaxBlocks )
      s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
#endif

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   ULONG ulMemSize;
   void * pResult;

   HB_TRACE(("hb_xrealloc(%p, %lu)", pMem, ulSize));

   ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
   pResult = realloc( ( char * ) pMem - sizeof( ULONG ), ulSize + sizeof( ULONG ) );

   if( ! pResult )
   {
      hb_errInternal( 9999, "hb_xrealloc can't reallocate memory", NULL, NULL );
   }

   * ( ( ULONG * ) pResult ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   if( ! ulSize )
      s_ulMemoryBlocks--;

   s_ulMemoryConsumed += ( ulSize - ulMemSize );
   if( s_ulMemoryConsumed > s_ulMemoryMaxConsumed )
      s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
#endif

   return ( char * ) pResult + sizeof( ULONG );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize;

   HB_TRACE(("hb_xfree(%p)", pMem));

   ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

   if( pMem )
      free( ( char * ) pMem - sizeof( ULONG ) );
   else
      hb_errInternal( 9999, "hb_xfree called with a null pointer", NULL, NULL );

#ifdef HB_FM_STATISTICS
   s_ulMemoryConsumed -= ulMemSize;
   s_ulMemoryBlocks--;
#endif
}

ULONG hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   HB_TRACE(("hb_xsize(%p)", pMem));

   return * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
}

void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   HB_TRACE(("hb_xinit()"));
}

void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
   HB_TRACE(("hb_xexit()"));

#ifdef HB_FM_STATISTICS
   if( s_ulMemoryBlocks || hb_cmdargCheck( "INFO" ) )
   {
      char buffer[ 100 ];

      hb_outerr( hb_consoleGetNewLine(), 0 );
      hb_outerr( "----------------------------------------", 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
      sprintf( buffer, "Total memory allocated: %lu bytes (%lu blocks)", s_ulMemoryMaxConsumed, s_ulMemoryMaxBlocks );
      hb_outerr( buffer, 0 );

      if( s_ulMemoryBlocks )
      {
         hb_outerr( hb_consoleGetNewLine(), 0 );
         sprintf( buffer, "WARNING! Memory allocated but not released: %lu bytes (%lu blocks)", s_ulMemoryConsumed, s_ulMemoryBlocks );
         hb_outerr( buffer, 0 );
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

   HB_TRACE(("hb_xmemcpy(%p, %p, %lu)", pDestArg, pSourceArg, ulLen));

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

   HB_TRACE(("hb_xmemset(%p, %d, %lu)", pDestArg, iFill, ulLen));

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

HARBOUR HB_MEMORY( void )
{
   USHORT uiMode = hb_parni( 1 );
   ULONG ulResult;

   /* TODO: Return the correct values instead of 9999 [vszel] */

   switch( uiMode )
   {
   case 0:              /*               (Free Variable Space [KB])          */
      ulResult = 9999;
      break;

   case 1:              /*               (Largest String [KB])               */
      ulResult = 9999;
      break;

   case 2:              /*               (RUN Memory [KB])                   */
      ulResult = 9999;
      break;

   case 3:              /* UNDOCUMENTED! (Virtual Memory [KB])               */
      ulResult = 9999;
      break;

   case 4:              /* UNDOCUMENTED! (Free Expanded Memory [KB]) (?)     */
      ulResult = 9999;
      break;

   case 101:            /* UNDOCUMENTED! (Fixed Memory/Heap [KB]) (?)        */
      ulResult = 9999;
      break;

   case 102:            /* UNDOCUMENTED! (Segments in Fixed Memory/Heap) (?) */
      ulResult = 9999;
      break;

   case 103:            /* UNDOCUMENTED! (Free Swap Memory [KB])             */
      ulResult = 9999;
      break;

   case 104:            /* UNDOCUMENTED! (Free Conventional [KB])            */
      ulResult = 9999;
      break;

   case 105:            /* UNDOCUMENTED! (Used Expanded Memory [KB]) (?)     */
      ulResult = ( s_ulMemoryConsumed / 1024 );
      break;

   case 1001:           /* Harbour extension (Memory used [bytes])           */
      ulResult = s_ulMemoryConsumed;
      break;

   case 1002:           /* Harbour extension (Maximum memory used [bytes])   */
      ulResult = s_ulMemoryMaxConsumed;
      break;

   default:
      ulResult = 0;
   }

   hb_retnl( ulResult );
}

