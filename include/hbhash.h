/*
 * Harbour Project source code:
 * Harbour common hash table implementation
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

#ifndef HB_HASH_H_
#define HB_HASH_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

struct HB_HASH_TABLE_;

#define HB_HASH_FUNC( hbfunc )   HB_SIZE hbfunc( struct HB_HASH_TABLE_ * HashPtr, const void * Value, const void * Cargo )
typedef HB_HASH_FUNC( ( * PHB_HASH_FUNC ) );

typedef struct HB_HASH_ITEM_
{
   const void * ValPtr;          /* value stored in the hash table */
   const void * KeyPtr;
   HB_SIZE key;
   struct HB_HASH_ITEM_ *next;
} HB_HASH_ITEM, * PHB_HASH_ITEM;

typedef struct HB_HASH_TABLE_
{
   PHB_HASH_ITEM * pItems;        /* pointer to items */
   HB_SIZE nTableSize;            /* the table size - number of slots */
   HB_SIZE nCount;                /* number of items stored in the table */
   HB_SIZE nUsed;                 /* number of used slots */
   PHB_HASH_FUNC pKeyFunc;        /* pointer to func that returns key value */
   PHB_HASH_FUNC pDeleteItemFunc; /* ptr to func that deletes value stored in the table */
   PHB_HASH_FUNC pCompFunc;       /* ptr to func that compares two items */
} HB_HASH_TABLE, * PHB_HASH_TABLE;


extern HB_EXPORT_INT PHB_HASH_TABLE hb_hashTableCreate( HB_SIZE nSize,
                                                        PHB_HASH_FUNC pHashFunc,
                                                        PHB_HASH_FUNC pDelete,
                                                        PHB_HASH_FUNC pComp );
extern HB_EXPORT_INT void hb_hashTableKill( PHB_HASH_TABLE pTable ); /* release all items and the hash table */
extern HB_EXPORT_INT HB_BOOL hb_hashTableAdd( PHB_HASH_TABLE pTable, const void * pKey, const void * pValue ); /* add a new item into the table */
extern HB_EXPORT_INT HB_BOOL hb_hashTableDel( PHB_HASH_TABLE pTable, const void * pKey ); /* delete an item from the table  */
extern HB_EXPORT_INT const void * hb_hashTableFind( PHB_HASH_TABLE pTable, const void * pKey ); /* return the pointer to item's value or NULL if not found */
extern HB_EXPORT_INT PHB_HASH_TABLE hb_hashTableResize( PHB_HASH_TABLE pTable, HB_SIZE nNewSize ); /* resize the hash table */
extern HB_EXPORT_INT HB_SIZE hb_hashTableSize( PHB_HASH_TABLE pTable ); /* return the hash table size */

HB_EXTERN_END

#endif /* HB_HASH_H_ */
