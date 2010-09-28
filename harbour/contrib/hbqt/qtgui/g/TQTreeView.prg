/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QTreeView( ... )
   RETURN HB_QTreeView():new( ... )


CREATE CLASS QTreeView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QTreeView

   METHOD  new( ... )

   METHOD  allColumnsShowFocus()
   METHOD  autoExpandDelay()
   METHOD  columnAt( nX )
   METHOD  columnViewportPosition( nColumn )
   METHOD  columnWidth( nColumn )
   METHOD  expandsOnDoubleClick()
   METHOD  header()
   METHOD  indentation()
   METHOD  indexAbove( pIndex )
   METHOD  indexBelow( pIndex )
   METHOD  isAnimated()
   METHOD  isColumnHidden( nColumn )
   METHOD  isExpanded( pIndex )
   METHOD  isFirstColumnSpanned( nRow, pParent )
   METHOD  isHeaderHidden()
   METHOD  isRowHidden( nRow, pParent )
   METHOD  isSortingEnabled()
   METHOD  itemsExpandable()
   METHOD  rootIsDecorated()
   METHOD  scrollTo( pIndex, nHint )
   METHOD  setAllColumnsShowFocus( lEnable )
   METHOD  setAnimated( lEnable )
   METHOD  setAutoExpandDelay( nDelay )
   METHOD  setColumnHidden( nColumn, lHide )
   METHOD  setColumnWidth( nColumn, nWidth )
   METHOD  setExpanded( pIndex, lExpanded )
   METHOD  setExpandsOnDoubleClick( lEnable )
   METHOD  setFirstColumnSpanned( nRow, pParent, lSpan )
   METHOD  setHeader( pHeader )
   METHOD  setHeaderHidden( lHide )
   METHOD  setIndentation( nI )
   METHOD  setItemsExpandable( lEnable )
   METHOD  setRootIsDecorated( lShow )
   METHOD  setRowHidden( nRow, pParent, lHide )
   METHOD  setSortingEnabled( lEnable )
   METHOD  setUniformRowHeights( lUniform )
   METHOD  setWordWrap( lOn )
   METHOD  sortByColumn( nColumn, nOrder )
   METHOD  uniformRowHeights()
   METHOD  visualRect( pIndex )
   METHOD  wordWrap()
   METHOD  collapse( pIndex )
   METHOD  collapseAll()
   METHOD  expand( pIndex )
   METHOD  expandAll()
   METHOD  expandToDepth( nDepth )
   METHOD  hideColumn( nColumn )
   METHOD  resizeColumnToContents( nColumn )
   METHOD  showColumn( nColumn )

   ENDCLASS


METHOD QTreeView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeView( ... )
   RETURN Self


METHOD QTreeView:allColumnsShowFocus()
   RETURN Qt_QTreeView_allColumnsShowFocus( ::pPtr )


METHOD QTreeView:autoExpandDelay()
   RETURN Qt_QTreeView_autoExpandDelay( ::pPtr )


METHOD QTreeView:columnAt( nX )
   RETURN Qt_QTreeView_columnAt( ::pPtr, nX )


METHOD QTreeView:columnViewportPosition( nColumn )
   RETURN Qt_QTreeView_columnViewportPosition( ::pPtr, nColumn )


METHOD QTreeView:columnWidth( nColumn )
   RETURN Qt_QTreeView_columnWidth( ::pPtr, nColumn )


METHOD QTreeView:expandsOnDoubleClick()
   RETURN Qt_QTreeView_expandsOnDoubleClick( ::pPtr )


METHOD QTreeView:header()
   RETURN HB_QHeaderView():from( Qt_QTreeView_header( ::pPtr ) )


METHOD QTreeView:indentation()
   RETURN Qt_QTreeView_indentation( ::pPtr )


METHOD QTreeView:indexAbove( pIndex )
   RETURN HB_QModelIndex():from( Qt_QTreeView_indexAbove( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QTreeView:indexBelow( pIndex )
   RETURN HB_QModelIndex():from( Qt_QTreeView_indexBelow( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QTreeView:isAnimated()
   RETURN Qt_QTreeView_isAnimated( ::pPtr )


METHOD QTreeView:isColumnHidden( nColumn )
   RETURN Qt_QTreeView_isColumnHidden( ::pPtr, nColumn )


METHOD QTreeView:isExpanded( pIndex )
   RETURN Qt_QTreeView_isExpanded( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QTreeView:isFirstColumnSpanned( nRow, pParent )
   RETURN Qt_QTreeView_isFirstColumnSpanned( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QTreeView:isHeaderHidden()
   RETURN Qt_QTreeView_isHeaderHidden( ::pPtr )


METHOD QTreeView:isRowHidden( nRow, pParent )
   RETURN Qt_QTreeView_isRowHidden( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QTreeView:isSortingEnabled()
   RETURN Qt_QTreeView_isSortingEnabled( ::pPtr )


METHOD QTreeView:itemsExpandable()
   RETURN Qt_QTreeView_itemsExpandable( ::pPtr )


METHOD QTreeView:rootIsDecorated()
   RETURN Qt_QTreeView_rootIsDecorated( ::pPtr )


METHOD QTreeView:scrollTo( pIndex, nHint )
   RETURN Qt_QTreeView_scrollTo( ::pPtr, hbqt_ptr( pIndex ), nHint )


METHOD QTreeView:setAllColumnsShowFocus( lEnable )
   RETURN Qt_QTreeView_setAllColumnsShowFocus( ::pPtr, lEnable )


METHOD QTreeView:setAnimated( lEnable )
   RETURN Qt_QTreeView_setAnimated( ::pPtr, lEnable )


METHOD QTreeView:setAutoExpandDelay( nDelay )
   RETURN Qt_QTreeView_setAutoExpandDelay( ::pPtr, nDelay )


METHOD QTreeView:setColumnHidden( nColumn, lHide )
   RETURN Qt_QTreeView_setColumnHidden( ::pPtr, nColumn, lHide )


METHOD QTreeView:setColumnWidth( nColumn, nWidth )
   RETURN Qt_QTreeView_setColumnWidth( ::pPtr, nColumn, nWidth )


METHOD QTreeView:setExpanded( pIndex, lExpanded )
   RETURN Qt_QTreeView_setExpanded( ::pPtr, hbqt_ptr( pIndex ), lExpanded )


METHOD QTreeView:setExpandsOnDoubleClick( lEnable )
   RETURN Qt_QTreeView_setExpandsOnDoubleClick( ::pPtr, lEnable )


METHOD QTreeView:setFirstColumnSpanned( nRow, pParent, lSpan )
   RETURN Qt_QTreeView_setFirstColumnSpanned( ::pPtr, nRow, hbqt_ptr( pParent ), lSpan )


METHOD QTreeView:setHeader( pHeader )
   RETURN Qt_QTreeView_setHeader( ::pPtr, hbqt_ptr( pHeader ) )


METHOD QTreeView:setHeaderHidden( lHide )
   RETURN Qt_QTreeView_setHeaderHidden( ::pPtr, lHide )


METHOD QTreeView:setIndentation( nI )
   RETURN Qt_QTreeView_setIndentation( ::pPtr, nI )


METHOD QTreeView:setItemsExpandable( lEnable )
   RETURN Qt_QTreeView_setItemsExpandable( ::pPtr, lEnable )


METHOD QTreeView:setRootIsDecorated( lShow )
   RETURN Qt_QTreeView_setRootIsDecorated( ::pPtr, lShow )


METHOD QTreeView:setRowHidden( nRow, pParent, lHide )
   RETURN Qt_QTreeView_setRowHidden( ::pPtr, nRow, hbqt_ptr( pParent ), lHide )


METHOD QTreeView:setSortingEnabled( lEnable )
   RETURN Qt_QTreeView_setSortingEnabled( ::pPtr, lEnable )


METHOD QTreeView:setUniformRowHeights( lUniform )
   RETURN Qt_QTreeView_setUniformRowHeights( ::pPtr, lUniform )


METHOD QTreeView:setWordWrap( lOn )
   RETURN Qt_QTreeView_setWordWrap( ::pPtr, lOn )


METHOD QTreeView:sortByColumn( nColumn, nOrder )
   RETURN Qt_QTreeView_sortByColumn( ::pPtr, nColumn, nOrder )


METHOD QTreeView:uniformRowHeights()
   RETURN Qt_QTreeView_uniformRowHeights( ::pPtr )


METHOD QTreeView:visualRect( pIndex )
   RETURN HB_QRect():from( Qt_QTreeView_visualRect( ::pPtr, hbqt_ptr( pIndex ) ) )


METHOD QTreeView:wordWrap()
   RETURN Qt_QTreeView_wordWrap( ::pPtr )


METHOD QTreeView:collapse( pIndex )
   RETURN Qt_QTreeView_collapse( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QTreeView:collapseAll()
   RETURN Qt_QTreeView_collapseAll( ::pPtr )


METHOD QTreeView:expand( pIndex )
   RETURN Qt_QTreeView_expand( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QTreeView:expandAll()
   RETURN Qt_QTreeView_expandAll( ::pPtr )


METHOD QTreeView:expandToDepth( nDepth )
   RETURN Qt_QTreeView_expandToDepth( ::pPtr, nDepth )


METHOD QTreeView:hideColumn( nColumn )
   RETURN Qt_QTreeView_hideColumn( ::pPtr, nColumn )


METHOD QTreeView:resizeColumnToContents( nColumn )
   RETURN Qt_QTreeView_resizeColumnToContents( ::pPtr, nColumn )


METHOD QTreeView:showColumn( nColumn )
   RETURN Qt_QTreeView_showColumn( ::pPtr, nColumn )

