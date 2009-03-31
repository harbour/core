/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QFormLayout INHERIT QLayout

   DATA    pPtr

   METHOD  New()

   METHOD  addRow( pLabel, pField )            INLINE  Qt_QFormLayout_addRow( ::pPtr, pLabel, pField )
   METHOD  addRow_1( pLabel, pField )          INLINE  Qt_QFormLayout_addRow_1( ::pPtr, pLabel, pField )
   METHOD  addRow_2( cLabelText, pField )      INLINE  Qt_QFormLayout_addRow_2( ::pPtr, cLabelText, pField )
   METHOD  addRow_3( cLabelText, pField )      INLINE  Qt_QFormLayout_addRow_3( ::pPtr, cLabelText, pField )
   METHOD  addRow_4( pWidget )                 INLINE  Qt_QFormLayout_addRow_4( ::pPtr, pWidget )
   METHOD  addRow_5( pLayout )                 INLINE  Qt_QFormLayout_addRow_5( ::pPtr, pLayout )
   METHOD  fieldGrowthPolicy()                 INLINE  Qt_QFormLayout_fieldGrowthPolicy( ::pPtr )
   METHOD  formAlignment()                     INLINE  Qt_QFormLayout_formAlignment( ::pPtr )
   METHOD  horizontalSpacing()                 INLINE  Qt_QFormLayout_horizontalSpacing( ::pPtr )
   METHOD  insertRow( nRow, pLabel, pField )   INLINE  Qt_QFormLayout_insertRow( ::pPtr, nRow, pLabel, pField )
   METHOD  insertRow_1( nRow, pLabel, pField )  INLINE  Qt_QFormLayout_insertRow_1( ::pPtr, nRow, pLabel, pField )
   METHOD  insertRow_2( nRow, cLabelText, pField )  INLINE  Qt_QFormLayout_insertRow_2( ::pPtr, nRow, cLabelText, pField )
   METHOD  insertRow_3( nRow, cLabelText, pField )  INLINE  Qt_QFormLayout_insertRow_3( ::pPtr, nRow, cLabelText, pField )
   METHOD  insertRow_4( nRow, pWidget )        INLINE  Qt_QFormLayout_insertRow_4( ::pPtr, nRow, pWidget )
   METHOD  insertRow_5( nRow, pLayout )        INLINE  Qt_QFormLayout_insertRow_5( ::pPtr, nRow, pLayout )
   METHOD  itemAt( nRow, nItemRole )           INLINE  Qt_QFormLayout_itemAt( ::pPtr, nRow, nItemRole )
   METHOD  labelAlignment()                    INLINE  Qt_QFormLayout_labelAlignment( ::pPtr )
   METHOD  labelForField( pField )             INLINE  Qt_QFormLayout_labelForField( ::pPtr, pField )
   METHOD  labelForField_1( pField )           INLINE  Qt_QFormLayout_labelForField_1( ::pPtr, pField )
   METHOD  rowCount()                          INLINE  Qt_QFormLayout_rowCount( ::pPtr )
   METHOD  rowWrapPolicy()                     INLINE  Qt_QFormLayout_rowWrapPolicy( ::pPtr )
   METHOD  setFieldGrowthPolicy( nFieldGrowthPolicy )  INLINE  Qt_QFormLayout_setFieldGrowthPolicy( ::pPtr, nFieldGrowthPolicy )
   METHOD  setFormAlignment( nAlignment )      INLINE  Qt_QFormLayout_setFormAlignment( ::pPtr, nAlignment )
   METHOD  setHorizontalSpacing( nSpacing )    INLINE  Qt_QFormLayout_setHorizontalSpacing( ::pPtr, nSpacing )
   METHOD  setItem( nRow, nItemRole, pItem )   INLINE  Qt_QFormLayout_setItem( ::pPtr, nRow, nItemRole, pItem )
   METHOD  setLabelAlignment( nAlignment )     INLINE  Qt_QFormLayout_setLabelAlignment( ::pPtr, nAlignment )
   METHOD  setLayout( nRow, nItemRole, pLayout )  INLINE  Qt_QFormLayout_setLayout( ::pPtr, nRow, nItemRole, pLayout )
   METHOD  setRowWrapPolicy( nRowWrapPolicy )  INLINE  Qt_QFormLayout_setRowWrapPolicy( ::pPtr, nRowWrapPolicy )
   METHOD  setSpacing( nSpacing )              INLINE  Qt_QFormLayout_setSpacing( ::pPtr, nSpacing )
   METHOD  setVerticalSpacing( nSpacing )      INLINE  Qt_QFormLayout_setVerticalSpacing( ::pPtr, nSpacing )
   METHOD  setWidget( nRow, nItemRole, pWidget )  INLINE  Qt_QFormLayout_setWidget( ::pPtr, nRow, nItemRole, pWidget )
   METHOD  spacing()                           INLINE  Qt_QFormLayout_spacing( ::pPtr )
   METHOD  verticalSpacing()                   INLINE  Qt_QFormLayout_verticalSpacing( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFormLayout

   ::pPtr := Qt_QFormLayout( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

