/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


CREATE CLASS QTreeView INHERIT QAbstractItemView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTreeView_destroy( ::pPtr )

   METHOD  allColumnsShowFocus()               INLINE  Qt_QTreeView_allColumnsShowFocus( ::pPtr )
   METHOD  autoExpandDelay()                   INLINE  Qt_QTreeView_autoExpandDelay( ::pPtr )
   METHOD  columnAt( nX )                      INLINE  Qt_QTreeView_columnAt( ::pPtr, nX )
   METHOD  columnViewportPosition( nColumn )   INLINE  Qt_QTreeView_columnViewportPosition( ::pPtr, nColumn )
   METHOD  columnWidth( nColumn )              INLINE  Qt_QTreeView_columnWidth( ::pPtr, nColumn )
   METHOD  expandsOnDoubleClick()              INLINE  Qt_QTreeView_expandsOnDoubleClick( ::pPtr )
   METHOD  header()                            INLINE  Qt_QTreeView_header( ::pPtr )
   METHOD  indentation()                       INLINE  Qt_QTreeView_indentation( ::pPtr )
   METHOD  indexAbove( pIndex )                INLINE  Qt_QTreeView_indexAbove( ::pPtr, pIndex )
   METHOD  indexBelow( pIndex )                INLINE  Qt_QTreeView_indexBelow( ::pPtr, pIndex )
   METHOD  isAnimated()                        INLINE  Qt_QTreeView_isAnimated( ::pPtr )
   METHOD  isColumnHidden( nColumn )           INLINE  Qt_QTreeView_isColumnHidden( ::pPtr, nColumn )
   METHOD  isExpanded( pIndex )                INLINE  Qt_QTreeView_isExpanded( ::pPtr, pIndex )
   METHOD  isFirstColumnSpanned( nRow, pParent )  INLINE  Qt_QTreeView_isFirstColumnSpanned( ::pPtr, nRow, pParent )
   METHOD  isHeaderHidden()                    INLINE  Qt_QTreeView_isHeaderHidden( ::pPtr )
   METHOD  isRowHidden( nRow, pParent )        INLINE  Qt_QTreeView_isRowHidden( ::pPtr, nRow, pParent )
   METHOD  isSortingEnabled()                  INLINE  Qt_QTreeView_isSortingEnabled( ::pPtr )
   METHOD  itemsExpandable()                   INLINE  Qt_QTreeView_itemsExpandable( ::pPtr )
   METHOD  rootIsDecorated()                   INLINE  Qt_QTreeView_rootIsDecorated( ::pPtr )
   METHOD  scrollTo( pIndex, nHint )           INLINE  Qt_QTreeView_scrollTo( ::pPtr, pIndex, nHint )
   METHOD  setAllColumnsShowFocus( lEnable )   INLINE  Qt_QTreeView_setAllColumnsShowFocus( ::pPtr, lEnable )
   METHOD  setAnimated( lEnable )              INLINE  Qt_QTreeView_setAnimated( ::pPtr, lEnable )
   METHOD  setAutoExpandDelay( nDelay )        INLINE  Qt_QTreeView_setAutoExpandDelay( ::pPtr, nDelay )
   METHOD  setColumnHidden( nColumn, lHide )   INLINE  Qt_QTreeView_setColumnHidden( ::pPtr, nColumn, lHide )
   METHOD  setColumnWidth( nColumn, nWidth )   INLINE  Qt_QTreeView_setColumnWidth( ::pPtr, nColumn, nWidth )
   METHOD  setExpanded( pIndex, lExpanded )    INLINE  Qt_QTreeView_setExpanded( ::pPtr, pIndex, lExpanded )
   METHOD  setExpandsOnDoubleClick( lEnable )  INLINE  Qt_QTreeView_setExpandsOnDoubleClick( ::pPtr, lEnable )
   METHOD  setFirstColumnSpanned( nRow, pParent, lSpan )  INLINE  Qt_QTreeView_setFirstColumnSpanned( ::pPtr, nRow, pParent, lSpan )
   METHOD  setHeader( pHeader )                INLINE  Qt_QTreeView_setHeader( ::pPtr, pHeader )
   METHOD  setHeaderHidden( lHide )            INLINE  Qt_QTreeView_setHeaderHidden( ::pPtr, lHide )
   METHOD  setIndentation( nI )                INLINE  Qt_QTreeView_setIndentation( ::pPtr, nI )
   METHOD  setItemsExpandable( lEnable )       INLINE  Qt_QTreeView_setItemsExpandable( ::pPtr, lEnable )
   METHOD  setRootIsDecorated( lShow )         INLINE  Qt_QTreeView_setRootIsDecorated( ::pPtr, lShow )
   METHOD  setRowHidden( nRow, pParent, lHide )  INLINE  Qt_QTreeView_setRowHidden( ::pPtr, nRow, pParent, lHide )
   METHOD  setSortingEnabled( lEnable )        INLINE  Qt_QTreeView_setSortingEnabled( ::pPtr, lEnable )
   METHOD  setUniformRowHeights( lUniform )    INLINE  Qt_QTreeView_setUniformRowHeights( ::pPtr, lUniform )
   METHOD  setWordWrap( lOn )                  INLINE  Qt_QTreeView_setWordWrap( ::pPtr, lOn )
   METHOD  sortByColumn( nColumn, nOrder )     INLINE  Qt_QTreeView_sortByColumn( ::pPtr, nColumn, nOrder )
   METHOD  uniformRowHeights()                 INLINE  Qt_QTreeView_uniformRowHeights( ::pPtr )
   METHOD  visualRect( pIndex )                INLINE  Qt_QTreeView_visualRect( ::pPtr, pIndex )
   METHOD  wordWrap()                          INLINE  Qt_QTreeView_wordWrap( ::pPtr )
   METHOD  collapse( pIndex )                  INLINE  Qt_QTreeView_collapse( ::pPtr, pIndex )
   METHOD  collapseAll()                       INLINE  Qt_QTreeView_collapseAll( ::pPtr )
   METHOD  expand( pIndex )                    INLINE  Qt_QTreeView_expand( ::pPtr, pIndex )
   METHOD  expandAll()                         INLINE  Qt_QTreeView_expandAll( ::pPtr )
   METHOD  expandToDepth( nDepth )             INLINE  Qt_QTreeView_expandToDepth( ::pPtr, nDepth )
   METHOD  hideColumn( nColumn )               INLINE  Qt_QTreeView_hideColumn( ::pPtr, nColumn )
   METHOD  resizeColumnToContents( nColumn )   INLINE  Qt_QTreeView_resizeColumnToContents( ::pPtr, nColumn )
   METHOD  showColumn( nColumn )               INLINE  Qt_QTreeView_showColumn( ::pPtr, nColumn )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTreeView

   ::pParent := pParent

   ::pPtr := Qt_QTreeView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTreeView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

