/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_BTree class.
 *
 * Copyright 2002 April White <april@users.sourceforge.net>
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

#include "hbclass.ch"
#include "common.ch"
#include "hb_btree.ch"

CLASS TBTree STATIC

  HIDDEN:
    DATA nHandle  /* hb_btree handle */
    METHOD Open( cFileName, nFlags, nBuffers )

  EXPORTED:
    METHOD New( cFileName, nPageSize, nKeySize, nFlags, nBuffers )
    METHOD Close() INLINE hb_BTreeClose( ::nHandle ) , ::nHandle := 0
    METHOD Insert( cKey, lData ) INLINE hb_BTreeInsert( ::nHandle, cKey, lData )
    METHOD Delete( cKey, lData ) INLINE hb_BTreeDelete( ::nHandle, cKey, lData )
    METHOD Key() INLINE hb_BTreeKey( ::nHandle )
    METHOD Data() INLINE hb_BTreeData( ::nHandle )
    METHOD GoTop() INLINE hb_BTreeGoTop( ::nHandle )
    METHOD GoBottom() INLINE hb_BTreeGoBottom( ::nHandle )
    METHOD Skip( nRecords ) INLINE hb_BTreeSkip( ::nHandle, nRecords )
    METHOD Seek( cKey, lData, lSoftSeek ) INLINE hb_BTreeSeek( ::nHandle, cKey, lData, lSoftSeek )
    METHOD Info( nIndex ) INLINE hb_BTreeInfo( ::nHandle, nIndex )

ENDCLASS

METHOD New( FileName, PageSize, KeySize, nFlags, Buffers ) CLASS TBTree
  ::nHandle := hb_btreenew( FileName, PageSize, KeySize, nFlags, Buffers )
  if ::nHandle >= 1
    return SELF
  endif
return NIL

METHOD Open( FileName, nFlags, Buffers ) CLASS TBTree
  ::nHandle := hb_btreeopen( FileName, nFlags, Buffers )
  if ::nHandle >= 1
    return SELF
  endif
return NIL

function TBTreeNew( FileName, PageSize, KeySize, nFlags, Buffers )
return TBTree():New( FileName, PageSize, KeySize, nFlags, Buffers )

function TBTreeOpen( FileName, nFlags, Buffers )
return TBTree():Open( FileName, nFlags, Buffers )
