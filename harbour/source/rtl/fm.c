/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
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
static ULONG s_ulMemoryBlocks = 0;	/* memory blocks used */
static ULONG s_ulMemoryMaxBlocks = 0;	/* maximum number of used memory blocks */
static ULONG s_ulMemoryMaxConsumed = 0;	/* memory size consumed */
static ULONG s_ulMemoryConsumed = 0;	/* memory max size consumed */
#endif

void * hb_xalloc( ULONG ulSize )         /* allocates fixed memory, returns NULL on failure */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      return pMem;
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   s_ulMemoryConsumed    += ulSize;
   s_ulMemoryMaxConsumed += ulSize;
   s_ulMemoryBlocks++;
   s_ulMemoryMaxBlocks++;
#endif

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      hb_errInternal( 9999, "hb_xgrab can't allocate memory", NULL, NULL );
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   s_ulMemoryConsumed    += ulSize;
   s_ulMemoryMaxConsumed += ulSize;
   s_ulMemoryBlocks++;
   s_ulMemoryMaxBlocks++;
#endif

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
   void * pResult = realloc( ( char * ) pMem - sizeof( ULONG ), ulSize + sizeof( ULONG ) );

   if( ! pResult )
   {
      hb_errInternal( 9999, "hb_xrealloc can't reallocate memory", NULL, NULL );
   }

   * ( ( ULONG * ) pResult ) = ulSize;  /* we store the block size into it */

#ifdef HB_FM_STATISTICS
   if( ! ulSize )
      s_ulMemoryBlocks--;

   s_ulMemoryConsumed += ( ulSize - ulMemSize );
   if( ulSize > ulMemSize )
      s_ulMemoryMaxConsumed += ulSize - ulMemSize;
#endif

   return ( char * ) pResult + sizeof( ULONG );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

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
   return * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
}

void hb_xinit( void ) /* Initialize fixed memory subsystem */
{
   ;
}

void hb_xexit( void ) /* Deinitialize fixed memory subsystem */
{
#ifdef HB_FM_STATISTICS
   if( s_ulMemoryBlocks )
   {
      printf( "\n\ntotal memory blocks allocated: %lu\n", s_ulMemoryMaxBlocks );
      printf( "memory maximum size consumed: %ld\n", s_ulMemoryMaxConsumed );
      printf( "memory blocks not released: %ld\n", s_ulMemoryBlocks );
      printf( "memory size not released: %ld\n", s_ulMemoryConsumed );
   }
#endif
}
