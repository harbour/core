/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_BTree class.
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

#include "hbclass.ch"
#include "hb_btree.ch"

CREATE CLASS TBTree STATIC

   HIDDEN:
      DATA nHandle  /* hb_btree handle */
      METHOD Open( cFileName, nFlags, nBuffers )
      METHOD New( cFileName, nPageSize, nKeySize, nFlags, nBuffers ) CONSTRUCTOR

   EXPORTED:
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

METHOD New( cFileName, nPageSize, nKeySize, nFlags, nBuffers ) CLASS TBTree
   ::nHandle := hb_btreenew( cFileName, nPageSize, nKeySize, nFlags, nBuffers )
   IF ::nHandle >= 1
      RETURN SELF
   ENDIF
   RETURN NIL

METHOD Open( cFileName, nFlags, nBuffers ) CLASS TBTree
   ::nHandle := hb_btreeopen( cFileName, nFlags, nBuffers )
   IF ::nHandle >= 1
      RETURN SELF
   ENDIF
   RETURN NIL

FUNCTION TBTreeNew( FileName, PageSize, KeySize, nFlags, Buffers )
   RETURN TBTree():New( FileName, PageSize, KeySize, nFlags, Buffers )

FUNCTION TBTreeOpen( FileName, nFlags, Buffers )
   RETURN TBTree():Open( FileName, nFlags, Buffers )
