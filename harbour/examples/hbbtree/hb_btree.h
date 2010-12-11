/*
   $Id$
 */

/*
 * Harbour Project source code:
 * HB_BTree Harbour C API header.
 *
 * Copyright 2002-2010 April White <april@users.sourceforge.net>
 * www - http://harbour-project.org
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

#ifndef HB_BTREE_API
#define HB_BTREE_API

HB_EXTERN_BEGIN

#include "hb_btree.ch"

struct hb_BTree;

struct hb_BTree * hb_BTreeNew( const char * FileName, HB_USHORT usPageSize, HB_USHORT usKeySize,
                               HB_ULONG ulFlags,
                               HB_ULONG ulBuffers );
struct hb_BTree * hb_BTreeOpen( const char * FileName, HB_ULONG ulFlags, HB_ULONG ulBuffers );
void hb_BTreeClose( struct hb_BTree * pBTree );
HB_BOOL hb_BTreeInsert( struct hb_BTree * pBTree, const char * szKey, PHB_ITEM pData );
HB_BOOL hb_BTreeDelete( struct hb_BTree * pBTree, const char * szKey, HB_LONG lData );
void hb_BTreeGoTop( struct hb_BTree * pBTree );
void hb_BTreeGoBottom( struct hb_BTree * pBTree );
HB_BOOL hb_BTreeSeek( struct hb_BTree * pBTree, const char * szKey, HB_LONG lData,
                      HB_BOOL bSoftSeek );
HB_LONG hb_BTreeSkip( struct hb_BTree * pBTree, HB_LONG records );
const char * hb_BTreeKey( struct hb_BTree * pBTree );
HB_LONG hb_BTreeData( struct hb_BTree * pBTree );
PHB_ITEM hb_BTreeDataItem( struct hb_BTree * pBTree );

HB_EXTERN_END

#endif
