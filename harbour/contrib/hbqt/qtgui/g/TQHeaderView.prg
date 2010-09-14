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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QHeaderView( ... )
   RETURN HB_QHeaderView():new( ... )


CREATE CLASS QHeaderView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QHeaderView

   METHOD  new( ... )

   METHOD  cascadingSectionResizes()
   METHOD  count()
   METHOD  defaultAlignment()
   METHOD  defaultSectionSize()
   METHOD  hiddenSectionCount()
   METHOD  hideSection( nLogicalIndex )
   METHOD  highlightSections()
   METHOD  isClickable()
   METHOD  isMovable()
   METHOD  isSectionHidden( nLogicalIndex )
   METHOD  isSortIndicatorShown()
   METHOD  length()
   METHOD  logicalIndex( nVisualIndex )
   METHOD  logicalIndexAt( nPosition )
   METHOD  logicalIndexAt_1( nX, nY )
   METHOD  logicalIndexAt_2( pPos )
   METHOD  minimumSectionSize()
   METHOD  moveSection( nFrom, nTo )
   METHOD  offset()
   METHOD  orientation()
   METHOD  resizeMode( nLogicalIndex )
   METHOD  resizeSection( nLogicalIndex, nSize )
   METHOD  resizeSections( nMode )
   METHOD  restoreState( pState )
   METHOD  saveState()
   METHOD  sectionPosition( nLogicalIndex )
   METHOD  sectionSize( nLogicalIndex )
   METHOD  sectionSizeHint( nLogicalIndex )
   METHOD  sectionViewportPosition( nLogicalIndex )
   METHOD  sectionsHidden()
   METHOD  sectionsMoved()
   METHOD  setCascadingSectionResizes( lEnable )
   METHOD  setClickable( lClickable )
   METHOD  setDefaultAlignment( nAlignment )
   METHOD  setDefaultSectionSize( nSize )
   METHOD  setHighlightSections( lHighlight )
   METHOD  setMinimumSectionSize( nSize )
   METHOD  setMovable( lMovable )
   METHOD  setResizeMode( nMode )
   METHOD  setResizeMode_1( nLogicalIndex, nMode )
   METHOD  setSectionHidden( nLogicalIndex, lHide )
   METHOD  setSortIndicator( nLogicalIndex, nOrder )
   METHOD  setSortIndicatorShown( lShow )
   METHOD  setStretchLastSection( lStretch )
   METHOD  showSection( nLogicalIndex )
   METHOD  sizeHint()
   METHOD  sortIndicatorOrder()
   METHOD  sortIndicatorSection()
   METHOD  stretchLastSection()
   METHOD  stretchSectionCount()
   METHOD  swapSections( nFirst, nSecond )
   METHOD  visualIndex( nLogicalIndex )
   METHOD  visualIndexAt( nPosition )
   METHOD  headerDataChanged( nOrientation, nLogicalFirst, nLogicalLast )
   METHOD  setOffset( nOffset )
   METHOD  setOffsetToLastSection()
   METHOD  setOffsetToSectionPosition( nVisualIndex )

   ENDCLASS


METHOD QHeaderView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHeaderView( ... )
   RETURN Self


METHOD QHeaderView:cascadingSectionResizes()
   RETURN Qt_QHeaderView_cascadingSectionResizes( ::pPtr )


METHOD QHeaderView:count()
   RETURN Qt_QHeaderView_count( ::pPtr )


METHOD QHeaderView:defaultAlignment()
   RETURN Qt_QHeaderView_defaultAlignment( ::pPtr )


METHOD QHeaderView:defaultSectionSize()
   RETURN Qt_QHeaderView_defaultSectionSize( ::pPtr )


METHOD QHeaderView:hiddenSectionCount()
   RETURN Qt_QHeaderView_hiddenSectionCount( ::pPtr )


METHOD QHeaderView:hideSection( nLogicalIndex )
   RETURN Qt_QHeaderView_hideSection( ::pPtr, nLogicalIndex )


METHOD QHeaderView:highlightSections()
   RETURN Qt_QHeaderView_highlightSections( ::pPtr )


METHOD QHeaderView:isClickable()
   RETURN Qt_QHeaderView_isClickable( ::pPtr )


METHOD QHeaderView:isMovable()
   RETURN Qt_QHeaderView_isMovable( ::pPtr )


METHOD QHeaderView:isSectionHidden( nLogicalIndex )
   RETURN Qt_QHeaderView_isSectionHidden( ::pPtr, nLogicalIndex )


METHOD QHeaderView:isSortIndicatorShown()
   RETURN Qt_QHeaderView_isSortIndicatorShown( ::pPtr )


METHOD QHeaderView:length()
   RETURN Qt_QHeaderView_length( ::pPtr )


METHOD QHeaderView:logicalIndex( nVisualIndex )
   RETURN Qt_QHeaderView_logicalIndex( ::pPtr, nVisualIndex )


METHOD QHeaderView:logicalIndexAt( nPosition )
   RETURN Qt_QHeaderView_logicalIndexAt( ::pPtr, nPosition )


METHOD QHeaderView:logicalIndexAt_1( nX, nY )
   RETURN Qt_QHeaderView_logicalIndexAt_1( ::pPtr, nX, nY )


METHOD QHeaderView:logicalIndexAt_2( pPos )
   RETURN Qt_QHeaderView_logicalIndexAt_2( ::pPtr, hbqt_ptr( pPos ) )


METHOD QHeaderView:minimumSectionSize()
   RETURN Qt_QHeaderView_minimumSectionSize( ::pPtr )


METHOD QHeaderView:moveSection( nFrom, nTo )
   RETURN Qt_QHeaderView_moveSection( ::pPtr, nFrom, nTo )


METHOD QHeaderView:offset()
   RETURN Qt_QHeaderView_offset( ::pPtr )


METHOD QHeaderView:orientation()
   RETURN Qt_QHeaderView_orientation( ::pPtr )


METHOD QHeaderView:resizeMode( nLogicalIndex )
   RETURN Qt_QHeaderView_resizeMode( ::pPtr, nLogicalIndex )


METHOD QHeaderView:resizeSection( nLogicalIndex, nSize )
   RETURN Qt_QHeaderView_resizeSection( ::pPtr, nLogicalIndex, nSize )


METHOD QHeaderView:resizeSections( nMode )
   RETURN Qt_QHeaderView_resizeSections( ::pPtr, nMode )


METHOD QHeaderView:restoreState( pState )
   RETURN Qt_QHeaderView_restoreState( ::pPtr, hbqt_ptr( pState ) )


METHOD QHeaderView:saveState()
   RETURN Qt_QHeaderView_saveState( ::pPtr )


METHOD QHeaderView:sectionPosition( nLogicalIndex )
   RETURN Qt_QHeaderView_sectionPosition( ::pPtr, nLogicalIndex )


METHOD QHeaderView:sectionSize( nLogicalIndex )
   RETURN Qt_QHeaderView_sectionSize( ::pPtr, nLogicalIndex )


METHOD QHeaderView:sectionSizeHint( nLogicalIndex )
   RETURN Qt_QHeaderView_sectionSizeHint( ::pPtr, nLogicalIndex )


METHOD QHeaderView:sectionViewportPosition( nLogicalIndex )
   RETURN Qt_QHeaderView_sectionViewportPosition( ::pPtr, nLogicalIndex )


METHOD QHeaderView:sectionsHidden()
   RETURN Qt_QHeaderView_sectionsHidden( ::pPtr )


METHOD QHeaderView:sectionsMoved()
   RETURN Qt_QHeaderView_sectionsMoved( ::pPtr )


METHOD QHeaderView:setCascadingSectionResizes( lEnable )
   RETURN Qt_QHeaderView_setCascadingSectionResizes( ::pPtr, lEnable )


METHOD QHeaderView:setClickable( lClickable )
   RETURN Qt_QHeaderView_setClickable( ::pPtr, lClickable )


METHOD QHeaderView:setDefaultAlignment( nAlignment )
   RETURN Qt_QHeaderView_setDefaultAlignment( ::pPtr, nAlignment )


METHOD QHeaderView:setDefaultSectionSize( nSize )
   RETURN Qt_QHeaderView_setDefaultSectionSize( ::pPtr, nSize )


METHOD QHeaderView:setHighlightSections( lHighlight )
   RETURN Qt_QHeaderView_setHighlightSections( ::pPtr, lHighlight )


METHOD QHeaderView:setMinimumSectionSize( nSize )
   RETURN Qt_QHeaderView_setMinimumSectionSize( ::pPtr, nSize )


METHOD QHeaderView:setMovable( lMovable )
   RETURN Qt_QHeaderView_setMovable( ::pPtr, lMovable )


METHOD QHeaderView:setResizeMode( nMode )
   RETURN Qt_QHeaderView_setResizeMode( ::pPtr, nMode )


METHOD QHeaderView:setResizeMode_1( nLogicalIndex, nMode )
   RETURN Qt_QHeaderView_setResizeMode_1( ::pPtr, nLogicalIndex, nMode )


METHOD QHeaderView:setSectionHidden( nLogicalIndex, lHide )
   RETURN Qt_QHeaderView_setSectionHidden( ::pPtr, nLogicalIndex, lHide )


METHOD QHeaderView:setSortIndicator( nLogicalIndex, nOrder )
   RETURN Qt_QHeaderView_setSortIndicator( ::pPtr, nLogicalIndex, nOrder )


METHOD QHeaderView:setSortIndicatorShown( lShow )
   RETURN Qt_QHeaderView_setSortIndicatorShown( ::pPtr, lShow )


METHOD QHeaderView:setStretchLastSection( lStretch )
   RETURN Qt_QHeaderView_setStretchLastSection( ::pPtr, lStretch )


METHOD QHeaderView:showSection( nLogicalIndex )
   RETURN Qt_QHeaderView_showSection( ::pPtr, nLogicalIndex )


METHOD QHeaderView:sizeHint()
   RETURN Qt_QHeaderView_sizeHint( ::pPtr )


METHOD QHeaderView:sortIndicatorOrder()
   RETURN Qt_QHeaderView_sortIndicatorOrder( ::pPtr )


METHOD QHeaderView:sortIndicatorSection()
   RETURN Qt_QHeaderView_sortIndicatorSection( ::pPtr )


METHOD QHeaderView:stretchLastSection()
   RETURN Qt_QHeaderView_stretchLastSection( ::pPtr )


METHOD QHeaderView:stretchSectionCount()
   RETURN Qt_QHeaderView_stretchSectionCount( ::pPtr )


METHOD QHeaderView:swapSections( nFirst, nSecond )
   RETURN Qt_QHeaderView_swapSections( ::pPtr, nFirst, nSecond )


METHOD QHeaderView:visualIndex( nLogicalIndex )
   RETURN Qt_QHeaderView_visualIndex( ::pPtr, nLogicalIndex )


METHOD QHeaderView:visualIndexAt( nPosition )
   RETURN Qt_QHeaderView_visualIndexAt( ::pPtr, nPosition )


METHOD QHeaderView:headerDataChanged( nOrientation, nLogicalFirst, nLogicalLast )
   RETURN Qt_QHeaderView_headerDataChanged( ::pPtr, nOrientation, nLogicalFirst, nLogicalLast )


METHOD QHeaderView:setOffset( nOffset )
   RETURN Qt_QHeaderView_setOffset( ::pPtr, nOffset )


METHOD QHeaderView:setOffsetToLastSection()
   RETURN Qt_QHeaderView_setOffsetToLastSection( ::pPtr )


METHOD QHeaderView:setOffsetToSectionPosition( nVisualIndex )
   RETURN Qt_QHeaderView_setOffsetToSectionPosition( ::pPtr, nVisualIndex )

