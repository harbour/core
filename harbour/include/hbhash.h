/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour common hash table implementation
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#ifndef HB_HASH_H_
#define HB_HASH_H_

#include "hbapi.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

#define HB_HASH_FUNC( hbfunc )   ULONG hbfunc( void *Value, void *Cargo )
typedef HB_HASH_FUNC( HB_HASH_FUNC_ );
typedef HB_HASH_FUNC_ *HB_HASH_FUNC_PTR;

typedef struct HB_HASH_ITEM_
{
   void *cargo;        /* value stored in the hash table */
   ULONG key;
   struct HB_HASH_ITEM_ *next;
} HB_HASH_ITEM, *HB_HASH_ITEM_PTR;

typedef struct HB_HASH_TABLE_
{
   HB_HASH_ITEM_PTR *pItems;    /* pointer to items */
   ULONG ulTableSize;           /* the table size - number of slots */
   ULONG ulCount;               /* number of items stored in the table */
   ULONG ulUsed;                /* number of used slots */
   HB_HASH_FUNC_PTR pKeyFunc;   /* pointer to func that returns key value */
   HB_HASH_FUNC_PTR pDeleteItemFunc; /* ptr to func that deletes value stured in the table */
   HB_HASH_FUNC_PTR pCompFunc;       /* ptr to func that compares two itmes */
} HB_HASH_TABLE, *HB_HASH_TABLE_PTR;

extern HB_HASH_TABLE_PTR hb_hashTableCreate( ULONG ulSize, 
                                   HB_HASH_FUNC_PTR pHashFunc, 
                                   HB_HASH_FUNC_PTR pDelete,
                                   HB_HASH_FUNC_PTR pComp );
extern void hb_hashTableKill( HB_HASH_TABLE_PTR pTable ); /* release all items and the hash table */
extern BOOL hb_hashTableAdd( HB_HASH_TABLE_PTR pTable, void *pValue ); /* add a new item into the table */
extern void * hb_hashTableFind( HB_HASH_TABLE_PTR pTable, void *pValue ); /* return the pointer to item's value or NULL if not found */
extern HB_HASH_TABLE_PTR hb_hashTableResize( HB_HASH_TABLE_PTR pTable, ULONG ulNewSize ); /* resize the hash table */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_HASH_H_ */
