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
