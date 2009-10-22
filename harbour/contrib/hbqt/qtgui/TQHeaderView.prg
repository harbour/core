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


CREATE CLASS QHeaderView INHERIT QAbstractItemView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  cascadingSectionResizes()           INLINE  Qt_QHeaderView_cascadingSectionResizes( ::pPtr )
   METHOD  count()                             INLINE  Qt_QHeaderView_count( ::pPtr )
   METHOD  defaultAlignment()                  INLINE  Qt_QHeaderView_defaultAlignment( ::pPtr )
   METHOD  defaultSectionSize()                INLINE  Qt_QHeaderView_defaultSectionSize( ::pPtr )
   METHOD  hiddenSectionCount()                INLINE  Qt_QHeaderView_hiddenSectionCount( ::pPtr )
   METHOD  hideSection( nLogicalIndex )        INLINE  Qt_QHeaderView_hideSection( ::pPtr, nLogicalIndex )
   METHOD  highlightSections()                 INLINE  Qt_QHeaderView_highlightSections( ::pPtr )
   METHOD  isClickable()                       INLINE  Qt_QHeaderView_isClickable( ::pPtr )
   METHOD  isMovable()                         INLINE  Qt_QHeaderView_isMovable( ::pPtr )
   METHOD  isSectionHidden( nLogicalIndex )    INLINE  Qt_QHeaderView_isSectionHidden( ::pPtr, nLogicalIndex )
   METHOD  isSortIndicatorShown()              INLINE  Qt_QHeaderView_isSortIndicatorShown( ::pPtr )
   METHOD  length()                            INLINE  Qt_QHeaderView_length( ::pPtr )
   METHOD  logicalIndex( nVisualIndex )        INLINE  Qt_QHeaderView_logicalIndex( ::pPtr, nVisualIndex )
   METHOD  logicalIndexAt( nPosition )         INLINE  Qt_QHeaderView_logicalIndexAt( ::pPtr, nPosition )
   METHOD  logicalIndexAt_1( nX, nY )          INLINE  Qt_QHeaderView_logicalIndexAt_1( ::pPtr, nX, nY )
   METHOD  logicalIndexAt_2( pPos )            INLINE  Qt_QHeaderView_logicalIndexAt_2( ::pPtr, pPos )
   METHOD  minimumSectionSize()                INLINE  Qt_QHeaderView_minimumSectionSize( ::pPtr )
   METHOD  moveSection( nFrom, nTo )           INLINE  Qt_QHeaderView_moveSection( ::pPtr, nFrom, nTo )
   METHOD  offset()                            INLINE  Qt_QHeaderView_offset( ::pPtr )
   METHOD  orientation()                       INLINE  Qt_QHeaderView_orientation( ::pPtr )
   METHOD  resizeMode( nLogicalIndex )         INLINE  Qt_QHeaderView_resizeMode( ::pPtr, nLogicalIndex )
   METHOD  resizeSection( nLogicalIndex, nSize )  INLINE  Qt_QHeaderView_resizeSection( ::pPtr, nLogicalIndex, nSize )
   METHOD  resizeSections( nMode )             INLINE  Qt_QHeaderView_resizeSections( ::pPtr, nMode )
   METHOD  restoreState( pState )              INLINE  Qt_QHeaderView_restoreState( ::pPtr, pState )
   METHOD  saveState()                         INLINE  Qt_QHeaderView_saveState( ::pPtr )
   METHOD  sectionPosition( nLogicalIndex )    INLINE  Qt_QHeaderView_sectionPosition( ::pPtr, nLogicalIndex )
   METHOD  sectionSize( nLogicalIndex )        INLINE  Qt_QHeaderView_sectionSize( ::pPtr, nLogicalIndex )
   METHOD  sectionSizeHint( nLogicalIndex )    INLINE  Qt_QHeaderView_sectionSizeHint( ::pPtr, nLogicalIndex )
   METHOD  sectionViewportPosition( nLogicalIndex )  INLINE  Qt_QHeaderView_sectionViewportPosition( ::pPtr, nLogicalIndex )
   METHOD  sectionsHidden()                    INLINE  Qt_QHeaderView_sectionsHidden( ::pPtr )
   METHOD  sectionsMoved()                     INLINE  Qt_QHeaderView_sectionsMoved( ::pPtr )
   METHOD  setCascadingSectionResizes( lEnable )  INLINE  Qt_QHeaderView_setCascadingSectionResizes( ::pPtr, lEnable )
   METHOD  setClickable( lClickable )          INLINE  Qt_QHeaderView_setClickable( ::pPtr, lClickable )
   METHOD  setDefaultAlignment( nAlignment )   INLINE  Qt_QHeaderView_setDefaultAlignment( ::pPtr, nAlignment )
   METHOD  setDefaultSectionSize( nSize )      INLINE  Qt_QHeaderView_setDefaultSectionSize( ::pPtr, nSize )
   METHOD  setHighlightSections( lHighlight )  INLINE  Qt_QHeaderView_setHighlightSections( ::pPtr, lHighlight )
   METHOD  setMinimumSectionSize( nSize )      INLINE  Qt_QHeaderView_setMinimumSectionSize( ::pPtr, nSize )
   METHOD  setMovable( lMovable )              INLINE  Qt_QHeaderView_setMovable( ::pPtr, lMovable )
   METHOD  setResizeMode( nMode )              INLINE  Qt_QHeaderView_setResizeMode( ::pPtr, nMode )
   METHOD  setResizeMode_1( nLogicalIndex, nMode )  INLINE  Qt_QHeaderView_setResizeMode_1( ::pPtr, nLogicalIndex, nMode )
   METHOD  setSectionHidden( nLogicalIndex, lHide )  INLINE  Qt_QHeaderView_setSectionHidden( ::pPtr, nLogicalIndex, lHide )
   METHOD  setSortIndicator( nLogicalIndex, nOrder )  INLINE  Qt_QHeaderView_setSortIndicator( ::pPtr, nLogicalIndex, nOrder )
   METHOD  setSortIndicatorShown( lShow )      INLINE  Qt_QHeaderView_setSortIndicatorShown( ::pPtr, lShow )
   METHOD  setStretchLastSection( lStretch )   INLINE  Qt_QHeaderView_setStretchLastSection( ::pPtr, lStretch )
   METHOD  showSection( nLogicalIndex )        INLINE  Qt_QHeaderView_showSection( ::pPtr, nLogicalIndex )
   METHOD  sizeHint()                          INLINE  Qt_QHeaderView_sizeHint( ::pPtr )
   METHOD  sortIndicatorOrder()                INLINE  Qt_QHeaderView_sortIndicatorOrder( ::pPtr )
   METHOD  sortIndicatorSection()              INLINE  Qt_QHeaderView_sortIndicatorSection( ::pPtr )
   METHOD  stretchLastSection()                INLINE  Qt_QHeaderView_stretchLastSection( ::pPtr )
   METHOD  stretchSectionCount()               INLINE  Qt_QHeaderView_stretchSectionCount( ::pPtr )
   METHOD  swapSections( nFirst, nSecond )     INLINE  Qt_QHeaderView_swapSections( ::pPtr, nFirst, nSecond )
   METHOD  visualIndex( nLogicalIndex )        INLINE  Qt_QHeaderView_visualIndex( ::pPtr, nLogicalIndex )
   METHOD  visualIndexAt( nPosition )          INLINE  Qt_QHeaderView_visualIndexAt( ::pPtr, nPosition )
   METHOD  headerDataChanged( nOrientation, nLogicalFirst, nLogicalLast )  INLINE  Qt_QHeaderView_headerDataChanged( ::pPtr, nOrientation, nLogicalFirst, nLogicalLast )
   METHOD  setOffset( nOffset )                INLINE  Qt_QHeaderView_setOffset( ::pPtr, nOffset )
   METHOD  setOffsetToLastSection()            INLINE  Qt_QHeaderView_setOffsetToLastSection( ::pPtr )
   METHOD  setOffsetToSectionPosition( nVisualIndex )  INLINE  Qt_QHeaderView_setOffsetToSectionPosition( ::pPtr, nVisualIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QHeaderView

   ::pParent := pParent

   ::pPtr := Qt_QHeaderView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QHeaderView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
