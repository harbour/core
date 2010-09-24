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


FUNCTION QFormLayout( ... )
   RETURN HB_QFormLayout():new( ... )


CREATE CLASS QFormLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QFormLayout

   METHOD  new( ... )

   METHOD  addRow( ... )
   METHOD  fieldGrowthPolicy()
   METHOD  formAlignment()
   METHOD  getItemPosition( nIndex, nRowPtr, nRolePtr )
   METHOD  getLayoutPosition( pLayout, nRowPtr, nRolePtr )
   METHOD  getWidgetPosition( pWidget, nRowPtr, nRolePtr )
   METHOD  horizontalSpacing()
   METHOD  insertRow( ... )
   METHOD  itemAt( nRow, nRole )
   METHOD  labelAlignment()
   METHOD  labelForField( ... )
   METHOD  rowCount()
   METHOD  rowWrapPolicy()
   METHOD  setFieldGrowthPolicy( nPolicy )
   METHOD  setFormAlignment( nAlignment )
   METHOD  setHorizontalSpacing( nSpacing )
   METHOD  setItem( nRow, nRole, pItem )
   METHOD  setLabelAlignment( nAlignment )
   METHOD  setLayout( nRow, nRole, pLayout )
   METHOD  setRowWrapPolicy( nPolicy )
   METHOD  setSpacing( nSpacing )
   METHOD  setVerticalSpacing( nSpacing )
   METHOD  setWidget( nRow, nRole, pWidget )
   METHOD  spacing()
   METHOD  verticalSpacing()

   ENDCLASS


METHOD QFormLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFormLayout( ... )
   RETURN Self


METHOD QFormLayout:addRow( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO"
                // void addRow ( const QString & labelText, QWidget * field )
                // C c QString, PO p QWidget
         RETURN Qt_QFormLayout_addRow_2( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // void addRow ( QWidget * label, QWidget * field )
                // PO p QWidget, PO p QWidget
         RETURN Qt_QFormLayout_addRow( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void addRow ( QWidget * widget )
                // PO p QWidget
         RETURN Qt_QFormLayout_addRow_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFormLayout:fieldGrowthPolicy()
   RETURN Qt_QFormLayout_fieldGrowthPolicy( ::pPtr )


METHOD QFormLayout:formAlignment()
   RETURN Qt_QFormLayout_formAlignment( ::pPtr )


METHOD QFormLayout:getItemPosition( nIndex, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getItemPosition( ::pPtr, nIndex, nRowPtr, nRolePtr )


METHOD QFormLayout:getLayoutPosition( pLayout, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getLayoutPosition( ::pPtr, hbqt_ptr( pLayout ), nRowPtr, nRolePtr )


METHOD QFormLayout:getWidgetPosition( pWidget, nRowPtr, nRolePtr )
   RETURN Qt_QFormLayout_getWidgetPosition( ::pPtr, hbqt_ptr( pWidget ), nRowPtr, nRolePtr )


METHOD QFormLayout:horizontalSpacing()
   RETURN Qt_QFormLayout_horizontalSpacing( ::pPtr )


METHOD QFormLayout:insertRow( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "PO"
                // void insertRow ( int row, const QString & labelText, QLayout * field )
                // N n int, C c QString, PO p QLayout
         RETURN Qt_QFormLayout_insertRow_4( ::pPtr, ... )
                // void insertRow ( int row, const QString & labelText, QWidget * field )
                // N n int, C c QString, PO p QWidget
         // RETURN Qt_QFormLayout_insertRow_3( ::pPtr, ... )
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // void insertRow ( int row, QWidget * label, QLayout * field )
                // N n int, PO p QWidget, PO p QLayout
         RETURN Qt_QFormLayout_insertRow_1( ::pPtr, ... )
                // void insertRow ( int row, QWidget * label, QWidget * field )
                // N n int, PO p QWidget, PO p QWidget
         // RETURN Qt_QFormLayout_insertRow( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void insertRow ( int row, QWidget * widget )
                // N n int, PO p QWidget
         RETURN Qt_QFormLayout_insertRow_2( ::pPtr, ... )
                // void insertRow ( int row, QLayout * layout )
                // N n int, PO p QLayout
         // RETURN Qt_QFormLayout_insertRow_5( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFormLayout:itemAt( nRow, nRole )
   RETURN Qt_QFormLayout_itemAt( ::pPtr, nRow, nRole )


METHOD QFormLayout:labelAlignment()
   RETURN Qt_QFormLayout_labelAlignment( ::pPtr )


METHOD QFormLayout:labelForField( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QWidget * labelForField ( QWidget * field ) const
                // PO p QWidget
         RETURN QWidget():from( Qt_QFormLayout_labelForField( ::pPtr, ... ) )
                // QWidget * labelForField ( QLayout * field ) const
                // PO p QLayout
         // RETURN QWidget():from( Qt_QFormLayout_labelForField_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFormLayout:rowCount()
   RETURN Qt_QFormLayout_rowCount( ::pPtr )


METHOD QFormLayout:rowWrapPolicy()
   RETURN Qt_QFormLayout_rowWrapPolicy( ::pPtr )


METHOD QFormLayout:setFieldGrowthPolicy( nPolicy )
   RETURN Qt_QFormLayout_setFieldGrowthPolicy( ::pPtr, nPolicy )


METHOD QFormLayout:setFormAlignment( nAlignment )
   RETURN Qt_QFormLayout_setFormAlignment( ::pPtr, nAlignment )


METHOD QFormLayout:setHorizontalSpacing( nSpacing )
   RETURN Qt_QFormLayout_setHorizontalSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setItem( nRow, nRole, pItem )
   RETURN Qt_QFormLayout_setItem( ::pPtr, nRow, nRole, hbqt_ptr( pItem ) )


METHOD QFormLayout:setLabelAlignment( nAlignment )
   RETURN Qt_QFormLayout_setLabelAlignment( ::pPtr, nAlignment )


METHOD QFormLayout:setLayout( nRow, nRole, pLayout )
   RETURN Qt_QFormLayout_setLayout( ::pPtr, nRow, nRole, hbqt_ptr( pLayout ) )


METHOD QFormLayout:setRowWrapPolicy( nPolicy )
   RETURN Qt_QFormLayout_setRowWrapPolicy( ::pPtr, nPolicy )


METHOD QFormLayout:setSpacing( nSpacing )
   RETURN Qt_QFormLayout_setSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setVerticalSpacing( nSpacing )
   RETURN Qt_QFormLayout_setVerticalSpacing( ::pPtr, nSpacing )


METHOD QFormLayout:setWidget( nRow, nRole, pWidget )
   RETURN Qt_QFormLayout_setWidget( ::pPtr, nRow, nRole, hbqt_ptr( pWidget ) )


METHOD QFormLayout:spacing()
   RETURN Qt_QFormLayout_spacing( ::pPtr )


METHOD QFormLayout:verticalSpacing()
   RETURN Qt_QFormLayout_verticalSpacing( ::pPtr )

