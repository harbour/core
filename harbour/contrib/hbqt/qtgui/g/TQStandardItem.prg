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


FUNCTION QStandardItem( ... )
   RETURN HB_QStandardItem():new( ... )


CREATE CLASS QStandardItem INHERIT HbQtObjectHandler FUNCTION HB_QStandardItem

   METHOD  new( ... )

   METHOD  accessibleDescription()
   METHOD  accessibleText()
   METHOD  appendRow( pItem )
   METHOD  background()
   METHOD  checkState()
   METHOD  child( nRow, nColumn )
   METHOD  clone()
   METHOD  column()
   METHOD  columnCount()
   METHOD  data( nRole )
   METHOD  flags()
   METHOD  font()
   METHOD  foreground()
   METHOD  hasChildren()
   METHOD  icon()
   METHOD  index()
   METHOD  insertColumns( nColumn, nCount )
   METHOD  insertRow( nRow, pItem )
   METHOD  insertRows( nRow, nCount )
   METHOD  isCheckable()
   METHOD  isDragEnabled()
   METHOD  isDropEnabled()
   METHOD  isEditable()
   METHOD  isEnabled()
   METHOD  isSelectable()
   METHOD  isTristate()
   METHOD  model()
   METHOD  parent()
   METHOD  read( pIn )
   METHOD  removeColumn( nColumn )
   METHOD  removeColumns( nColumn, nCount )
   METHOD  removeRow( nRow )
   METHOD  removeRows( nRow, nCount )
   METHOD  row()
   METHOD  rowCount()
   METHOD  setAccessibleDescription( cAccessibleDescription )
   METHOD  setAccessibleText( cAccessibleText )
   METHOD  setBackground( pBrush )
   METHOD  setCheckState( nState )
   METHOD  setCheckable( lCheckable )
   METHOD  setChild( nRow, nColumn, pItem )
   METHOD  setChild_1( nRow, pItem )
   METHOD  setColumnCount( nColumns )
   METHOD  setData( pValue, nRole )
   METHOD  setDragEnabled( lDragEnabled )
   METHOD  setDropEnabled( lDropEnabled )
   METHOD  setEditable( lEditable )
   METHOD  setEnabled( lEnabled )
   METHOD  setFlags( nFlags )
   METHOD  setFont( pFont )
   METHOD  setForeground( pBrush )
   METHOD  setIcon( pIcon )
   METHOD  setRowCount( nRows )
   METHOD  setSelectable( lSelectable )
   METHOD  setSizeHint( pSize )
   METHOD  setStatusTip( cStatusTip )
   METHOD  setText( cText )
   METHOD  setTextAlignment( nAlignment )
   METHOD  setToolTip( cToolTip )
   METHOD  setTristate( lTristate )
   METHOD  setWhatsThis( cWhatsThis )
   METHOD  sizeHint()
   METHOD  sortChildren( nColumn, nOrder )
   METHOD  statusTip()
   METHOD  takeChild( nRow, nColumn )
   METHOD  takeColumn( nColumn )
   METHOD  takeRow( nRow )
   METHOD  text()
   METHOD  textAlignment()
   METHOD  toolTip()
   METHOD  type()
   METHOD  whatsThis()
   METHOD  write( pOut )

   ENDCLASS


METHOD QStandardItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStandardItem( ... )
   RETURN Self


METHOD QStandardItem:accessibleDescription()
   RETURN Qt_QStandardItem_accessibleDescription( ::pPtr )


METHOD QStandardItem:accessibleText()
   RETURN Qt_QStandardItem_accessibleText( ::pPtr )


METHOD QStandardItem:appendRow( pItem )
   RETURN Qt_QStandardItem_appendRow( ::pPtr, hbqt_ptr( pItem ) )


METHOD QStandardItem:background()
   RETURN Qt_QStandardItem_background( ::pPtr )


METHOD QStandardItem:checkState()
   RETURN Qt_QStandardItem_checkState( ::pPtr )


METHOD QStandardItem:child( nRow, nColumn )
   RETURN Qt_QStandardItem_child( ::pPtr, nRow, nColumn )


METHOD QStandardItem:clone()
   RETURN Qt_QStandardItem_clone( ::pPtr )


METHOD QStandardItem:column()
   RETURN Qt_QStandardItem_column( ::pPtr )


METHOD QStandardItem:columnCount()
   RETURN Qt_QStandardItem_columnCount( ::pPtr )


METHOD QStandardItem:data( nRole )
   RETURN Qt_QStandardItem_data( ::pPtr, nRole )


METHOD QStandardItem:flags()
   RETURN Qt_QStandardItem_flags( ::pPtr )


METHOD QStandardItem:font()
   RETURN Qt_QStandardItem_font( ::pPtr )


METHOD QStandardItem:foreground()
   RETURN Qt_QStandardItem_foreground( ::pPtr )


METHOD QStandardItem:hasChildren()
   RETURN Qt_QStandardItem_hasChildren( ::pPtr )


METHOD QStandardItem:icon()
   RETURN Qt_QStandardItem_icon( ::pPtr )


METHOD QStandardItem:index()
   RETURN Qt_QStandardItem_index( ::pPtr )


METHOD QStandardItem:insertColumns( nColumn, nCount )
   RETURN Qt_QStandardItem_insertColumns( ::pPtr, nColumn, nCount )


METHOD QStandardItem:insertRow( nRow, pItem )
   RETURN Qt_QStandardItem_insertRow( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QStandardItem:insertRows( nRow, nCount )
   RETURN Qt_QStandardItem_insertRows( ::pPtr, nRow, nCount )


METHOD QStandardItem:isCheckable()
   RETURN Qt_QStandardItem_isCheckable( ::pPtr )


METHOD QStandardItem:isDragEnabled()
   RETURN Qt_QStandardItem_isDragEnabled( ::pPtr )


METHOD QStandardItem:isDropEnabled()
   RETURN Qt_QStandardItem_isDropEnabled( ::pPtr )


METHOD QStandardItem:isEditable()
   RETURN Qt_QStandardItem_isEditable( ::pPtr )


METHOD QStandardItem:isEnabled()
   RETURN Qt_QStandardItem_isEnabled( ::pPtr )


METHOD QStandardItem:isSelectable()
   RETURN Qt_QStandardItem_isSelectable( ::pPtr )


METHOD QStandardItem:isTristate()
   RETURN Qt_QStandardItem_isTristate( ::pPtr )


METHOD QStandardItem:model()
   RETURN Qt_QStandardItem_model( ::pPtr )


METHOD QStandardItem:parent()
   RETURN Qt_QStandardItem_parent( ::pPtr )


METHOD QStandardItem:read( pIn )
   RETURN Qt_QStandardItem_read( ::pPtr, hbqt_ptr( pIn ) )


METHOD QStandardItem:removeColumn( nColumn )
   RETURN Qt_QStandardItem_removeColumn( ::pPtr, nColumn )


METHOD QStandardItem:removeColumns( nColumn, nCount )
   RETURN Qt_QStandardItem_removeColumns( ::pPtr, nColumn, nCount )


METHOD QStandardItem:removeRow( nRow )
   RETURN Qt_QStandardItem_removeRow( ::pPtr, nRow )


METHOD QStandardItem:removeRows( nRow, nCount )
   RETURN Qt_QStandardItem_removeRows( ::pPtr, nRow, nCount )


METHOD QStandardItem:row()
   RETURN Qt_QStandardItem_row( ::pPtr )


METHOD QStandardItem:rowCount()
   RETURN Qt_QStandardItem_rowCount( ::pPtr )


METHOD QStandardItem:setAccessibleDescription( cAccessibleDescription )
   RETURN Qt_QStandardItem_setAccessibleDescription( ::pPtr, cAccessibleDescription )


METHOD QStandardItem:setAccessibleText( cAccessibleText )
   RETURN Qt_QStandardItem_setAccessibleText( ::pPtr, cAccessibleText )


METHOD QStandardItem:setBackground( pBrush )
   RETURN Qt_QStandardItem_setBackground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QStandardItem:setCheckState( nState )
   RETURN Qt_QStandardItem_setCheckState( ::pPtr, nState )


METHOD QStandardItem:setCheckable( lCheckable )
   RETURN Qt_QStandardItem_setCheckable( ::pPtr, lCheckable )


METHOD QStandardItem:setChild( nRow, nColumn, pItem )
   RETURN Qt_QStandardItem_setChild( ::pPtr, nRow, nColumn, hbqt_ptr( pItem ) )


METHOD QStandardItem:setChild_1( nRow, pItem )
   RETURN Qt_QStandardItem_setChild_1( ::pPtr, nRow, hbqt_ptr( pItem ) )


METHOD QStandardItem:setColumnCount( nColumns )
   RETURN Qt_QStandardItem_setColumnCount( ::pPtr, nColumns )


METHOD QStandardItem:setData( pValue, nRole )
   RETURN Qt_QStandardItem_setData( ::pPtr, hbqt_ptr( pValue ), nRole )


METHOD QStandardItem:setDragEnabled( lDragEnabled )
   RETURN Qt_QStandardItem_setDragEnabled( ::pPtr, lDragEnabled )


METHOD QStandardItem:setDropEnabled( lDropEnabled )
   RETURN Qt_QStandardItem_setDropEnabled( ::pPtr, lDropEnabled )


METHOD QStandardItem:setEditable( lEditable )
   RETURN Qt_QStandardItem_setEditable( ::pPtr, lEditable )


METHOD QStandardItem:setEnabled( lEnabled )
   RETURN Qt_QStandardItem_setEnabled( ::pPtr, lEnabled )


METHOD QStandardItem:setFlags( nFlags )
   RETURN Qt_QStandardItem_setFlags( ::pPtr, nFlags )


METHOD QStandardItem:setFont( pFont )
   RETURN Qt_QStandardItem_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QStandardItem:setForeground( pBrush )
   RETURN Qt_QStandardItem_setForeground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QStandardItem:setIcon( pIcon )
   RETURN Qt_QStandardItem_setIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QStandardItem:setRowCount( nRows )
   RETURN Qt_QStandardItem_setRowCount( ::pPtr, nRows )


METHOD QStandardItem:setSelectable( lSelectable )
   RETURN Qt_QStandardItem_setSelectable( ::pPtr, lSelectable )


METHOD QStandardItem:setSizeHint( pSize )
   RETURN Qt_QStandardItem_setSizeHint( ::pPtr, hbqt_ptr( pSize ) )


METHOD QStandardItem:setStatusTip( cStatusTip )
   RETURN Qt_QStandardItem_setStatusTip( ::pPtr, cStatusTip )


METHOD QStandardItem:setText( cText )
   RETURN Qt_QStandardItem_setText( ::pPtr, cText )


METHOD QStandardItem:setTextAlignment( nAlignment )
   RETURN Qt_QStandardItem_setTextAlignment( ::pPtr, nAlignment )


METHOD QStandardItem:setToolTip( cToolTip )
   RETURN Qt_QStandardItem_setToolTip( ::pPtr, cToolTip )


METHOD QStandardItem:setTristate( lTristate )
   RETURN Qt_QStandardItem_setTristate( ::pPtr, lTristate )


METHOD QStandardItem:setWhatsThis( cWhatsThis )
   RETURN Qt_QStandardItem_setWhatsThis( ::pPtr, cWhatsThis )


METHOD QStandardItem:sizeHint()
   RETURN Qt_QStandardItem_sizeHint( ::pPtr )


METHOD QStandardItem:sortChildren( nColumn, nOrder )
   RETURN Qt_QStandardItem_sortChildren( ::pPtr, nColumn, nOrder )


METHOD QStandardItem:statusTip()
   RETURN Qt_QStandardItem_statusTip( ::pPtr )


METHOD QStandardItem:takeChild( nRow, nColumn )
   RETURN Qt_QStandardItem_takeChild( ::pPtr, nRow, nColumn )


METHOD QStandardItem:takeColumn( nColumn )
   RETURN Qt_QStandardItem_takeColumn( ::pPtr, nColumn )


METHOD QStandardItem:takeRow( nRow )
   RETURN Qt_QStandardItem_takeRow( ::pPtr, nRow )


METHOD QStandardItem:text()
   RETURN Qt_QStandardItem_text( ::pPtr )


METHOD QStandardItem:textAlignment()
   RETURN Qt_QStandardItem_textAlignment( ::pPtr )


METHOD QStandardItem:toolTip()
   RETURN Qt_QStandardItem_toolTip( ::pPtr )


METHOD QStandardItem:type()
   RETURN Qt_QStandardItem_type( ::pPtr )


METHOD QStandardItem:whatsThis()
   RETURN Qt_QStandardItem_whatsThis( ::pPtr )


METHOD QStandardItem:write( pOut )
   RETURN Qt_QStandardItem_write( ::pPtr, hbqt_ptr( pOut ) )

