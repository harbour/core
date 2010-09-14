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


FUNCTION QItemSelectionModel( ... )
   RETURN HB_QItemSelectionModel():new( ... )


CREATE CLASS QItemSelectionModel INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QItemSelectionModel

   METHOD  new( ... )

   METHOD  columnIntersectsSelection( nColumn, pParent )
   METHOD  currentIndex()
   METHOD  hasSelection()
   METHOD  isColumnSelected( nColumn, pParent )
   METHOD  isRowSelected( nRow, pParent )
   METHOD  isSelected( pIndex )
   METHOD  model()
   METHOD  rowIntersectsSelection( nRow, pParent )
   METHOD  selection()
   METHOD  clear()
   METHOD  clearSelection()
   METHOD  reset()
   METHOD  select( pIndex, nCommand )
   METHOD  select_1( pSelection, nCommand )
   METHOD  setCurrentIndex( pIndex, nCommand )

   ENDCLASS


METHOD QItemSelectionModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemSelectionModel( ... )
   RETURN Self


METHOD QItemSelectionModel:columnIntersectsSelection( nColumn, pParent )
   RETURN Qt_QItemSelectionModel_columnIntersectsSelection( ::pPtr, nColumn, hbqt_ptr( pParent ) )


METHOD QItemSelectionModel:currentIndex()
   RETURN Qt_QItemSelectionModel_currentIndex( ::pPtr )


METHOD QItemSelectionModel:hasSelection()
   RETURN Qt_QItemSelectionModel_hasSelection( ::pPtr )


METHOD QItemSelectionModel:isColumnSelected( nColumn, pParent )
   RETURN Qt_QItemSelectionModel_isColumnSelected( ::pPtr, nColumn, hbqt_ptr( pParent ) )


METHOD QItemSelectionModel:isRowSelected( nRow, pParent )
   RETURN Qt_QItemSelectionModel_isRowSelected( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QItemSelectionModel:isSelected( pIndex )
   RETURN Qt_QItemSelectionModel_isSelected( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QItemSelectionModel:model()
   RETURN Qt_QItemSelectionModel_model( ::pPtr )


METHOD QItemSelectionModel:rowIntersectsSelection( nRow, pParent )
   RETURN Qt_QItemSelectionModel_rowIntersectsSelection( ::pPtr, nRow, hbqt_ptr( pParent ) )


METHOD QItemSelectionModel:selection()
   RETURN Qt_QItemSelectionModel_selection( ::pPtr )


METHOD QItemSelectionModel:clear()
   RETURN Qt_QItemSelectionModel_clear( ::pPtr )


METHOD QItemSelectionModel:clearSelection()
   RETURN Qt_QItemSelectionModel_clearSelection( ::pPtr )


METHOD QItemSelectionModel:reset()
   RETURN Qt_QItemSelectionModel_reset( ::pPtr )


METHOD QItemSelectionModel:select( pIndex, nCommand )
   RETURN Qt_QItemSelectionModel_select( ::pPtr, hbqt_ptr( pIndex ), nCommand )


METHOD QItemSelectionModel:select_1( pSelection, nCommand )
   RETURN Qt_QItemSelectionModel_select_1( ::pPtr, hbqt_ptr( pSelection ), nCommand )


METHOD QItemSelectionModel:setCurrentIndex( pIndex, nCommand )
   RETURN Qt_QItemSelectionModel_setCurrentIndex( ::pPtr, hbqt_ptr( pIndex ), nCommand )

