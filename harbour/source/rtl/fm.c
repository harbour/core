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

#ifndef __MPW__
 #include <malloc.h>
#endif
#include <stdlib.h>
#include "extend.h"

ULONG ulMemoryBlocks = 0;
ULONG ulMemoryMaxBlocks = 0;
ULONG ulMemoryMaxConsumed = 0;
ULONG ulMemoryConsumed = 0;

void * hb_xalloc( ULONG ulSize )         /* allocates fixed memory, returns NULL on failure */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      return pMem;
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

   ulMemoryConsumed    += ulSize;
   ulMemoryMaxConsumed += ulSize;
   ulMemoryBlocks++;
   ulMemoryMaxBlocks++;

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      printf( "\n_xgrab error: can't allocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

   ulMemoryConsumed    += ulSize;
   ulMemoryMaxConsumed += ulSize;
   ulMemoryBlocks++;
   ulMemoryMaxBlocks++;

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
   void * pResult = realloc( ( char * ) pMem - sizeof( ULONG ), ulSize + sizeof( ULONG ) );

   if( ! pResult )
   {
      printf( "\n_xrealloc error: can't reallocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pResult ) = ulSize;  /* we store the block size into it */

   if( ! ulSize )
      ulMemoryBlocks--;

   ulMemoryConsumed += ( ulSize - ulMemSize );
   if( ulSize > ulMemSize )
      ulMemoryMaxConsumed += ulSize - ulMemSize;

   return ( char * ) pResult + sizeof( ULONG );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

   if( pMem )
      free( ( char * ) pMem - sizeof( ULONG ) );
   else
      printf( "\nCalling hb_xfree() with a null pointer!\n" );

   ulMemoryConsumed -= ulMemSize;
   ulMemoryBlocks--;
}

ULONG hb_xsize( void * pMem ) /* returns the size of an allocated memory block */
{
   return * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
}

