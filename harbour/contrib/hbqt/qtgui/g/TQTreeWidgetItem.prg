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


FUNCTION QTreeWidgetItem( ... )
   RETURN HB_QTreeWidgetItem():new( ... )


CREATE CLASS QTreeWidgetItem INHERIT HbQtObjectHandler FUNCTION HB_QTreeWidgetItem

   METHOD  new( ... )

   METHOD  addChild( pChild )
   METHOD  background( nColumn )
   METHOD  checkState( nColumn )
   METHOD  child( nIndex )
   METHOD  childCount()
   METHOD  childIndicatorPolicy()
   METHOD  clone()
   METHOD  columnCount()
   METHOD  data( nColumn, nRole )
   METHOD  flags()
   METHOD  font( nColumn )
   METHOD  foreground( nColumn )
   METHOD  icon( nColumn )
   METHOD  indexOfChild( pChild )
   METHOD  insertChild( nIndex, pChild )
   METHOD  isDisabled()
   METHOD  isExpanded()
   METHOD  isFirstColumnSpanned()
   METHOD  isHidden()
   METHOD  isSelected()
   METHOD  parent()
   METHOD  read( pIn )
   METHOD  removeChild( pChild )
   METHOD  setBackground( nColumn, pBrush )
   METHOD  setCheckState( nColumn, nState )
   METHOD  setChildIndicatorPolicy( nPolicy )
   METHOD  setData( nColumn, nRole, pValue )
   METHOD  setDisabled( lDisabled )
   METHOD  setExpanded( lExpand )
   METHOD  setFirstColumnSpanned( lSpan )
   METHOD  setFlags( nFlags )
   METHOD  setFont( nColumn, pFont )
   METHOD  setForeground( nColumn, pBrush )
   METHOD  setHidden( lHide )
   METHOD  setIcon( nColumn, pIcon )
   METHOD  setSelected( lSelect )
   METHOD  setSizeHint( nColumn, pSize )
   METHOD  setStatusTip( nColumn, cStatusTip )
   METHOD  setText( nColumn, cText )
   METHOD  setTextAlignment( nColumn, nAlignment )
   METHOD  setToolTip( nColumn, cToolTip )
   METHOD  setWhatsThis( nColumn, cWhatsThis )
   METHOD  sizeHint( nColumn )
   METHOD  sortChildren( nColumn, nOrder )
   METHOD  statusTip( nColumn )
   METHOD  takeChild( nIndex )
   METHOD  takeChildren()
   METHOD  text( nColumn )
   METHOD  textAlignment( nColumn )
   METHOD  toolTip( nColumn )
   METHOD  treeWidget()
   METHOD  type()
   METHOD  whatsThis( nColumn )

   ENDCLASS


METHOD QTreeWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTreeWidgetItem( ... )
   RETURN Self


METHOD QTreeWidgetItem:addChild( pChild )
   RETURN Qt_QTreeWidgetItem_addChild( ::pPtr, hbqt_ptr( pChild ) )


METHOD QTreeWidgetItem:background( nColumn )
   RETURN Qt_QTreeWidgetItem_background( ::pPtr, nColumn )


METHOD QTreeWidgetItem:checkState( nColumn )
   RETURN Qt_QTreeWidgetItem_checkState( ::pPtr, nColumn )


METHOD QTreeWidgetItem:child( nIndex )
   RETURN Qt_QTreeWidgetItem_child( ::pPtr, nIndex )


METHOD QTreeWidgetItem:childCount()
   RETURN Qt_QTreeWidgetItem_childCount( ::pPtr )


METHOD QTreeWidgetItem:childIndicatorPolicy()
   RETURN Qt_QTreeWidgetItem_childIndicatorPolicy( ::pPtr )


METHOD QTreeWidgetItem:clone()
   RETURN Qt_QTreeWidgetItem_clone( ::pPtr )


METHOD QTreeWidgetItem:columnCount()
   RETURN Qt_QTreeWidgetItem_columnCount( ::pPtr )


METHOD QTreeWidgetItem:data( nColumn, nRole )
   RETURN Qt_QTreeWidgetItem_data( ::pPtr, nColumn, nRole )


METHOD QTreeWidgetItem:flags()
   RETURN Qt_QTreeWidgetItem_flags( ::pPtr )


METHOD QTreeWidgetItem:font( nColumn )
   RETURN Qt_QTreeWidgetItem_font( ::pPtr, nColumn )


METHOD QTreeWidgetItem:foreground( nColumn )
   RETURN Qt_QTreeWidgetItem_foreground( ::pPtr, nColumn )


METHOD QTreeWidgetItem:icon( nColumn )
   RETURN Qt_QTreeWidgetItem_icon( ::pPtr, nColumn )


METHOD QTreeWidgetItem:indexOfChild( pChild )
   RETURN Qt_QTreeWidgetItem_indexOfChild( ::pPtr, hbqt_ptr( pChild ) )


METHOD QTreeWidgetItem:insertChild( nIndex, pChild )
   RETURN Qt_QTreeWidgetItem_insertChild( ::pPtr, nIndex, hbqt_ptr( pChild ) )


METHOD QTreeWidgetItem:isDisabled()
   RETURN Qt_QTreeWidgetItem_isDisabled( ::pPtr )


METHOD QTreeWidgetItem:isExpanded()
   RETURN Qt_QTreeWidgetItem_isExpanded( ::pPtr )


METHOD QTreeWidgetItem:isFirstColumnSpanned()
   RETURN Qt_QTreeWidgetItem_isFirstColumnSpanned( ::pPtr )


METHOD QTreeWidgetItem:isHidden()
   RETURN Qt_QTreeWidgetItem_isHidden( ::pPtr )


METHOD QTreeWidgetItem:isSelected()
   RETURN Qt_QTreeWidgetItem_isSelected( ::pPtr )


METHOD QTreeWidgetItem:parent()
   RETURN Qt_QTreeWidgetItem_parent( ::pPtr )


METHOD QTreeWidgetItem:read( pIn )
   RETURN Qt_QTreeWidgetItem_read( ::pPtr, hbqt_ptr( pIn ) )


METHOD QTreeWidgetItem:removeChild( pChild )
   RETURN Qt_QTreeWidgetItem_removeChild( ::pPtr, hbqt_ptr( pChild ) )


METHOD QTreeWidgetItem:setBackground( nColumn, pBrush )
   RETURN Qt_QTreeWidgetItem_setBackground( ::pPtr, nColumn, hbqt_ptr( pBrush ) )


METHOD QTreeWidgetItem:setCheckState( nColumn, nState )
   RETURN Qt_QTreeWidgetItem_setCheckState( ::pPtr, nColumn, nState )


METHOD QTreeWidgetItem:setChildIndicatorPolicy( nPolicy )
   RETURN Qt_QTreeWidgetItem_setChildIndicatorPolicy( ::pPtr, nPolicy )


METHOD QTreeWidgetItem:setData( nColumn, nRole, pValue )
   RETURN Qt_QTreeWidgetItem_setData( ::pPtr, nColumn, nRole, hbqt_ptr( pValue ) )


METHOD QTreeWidgetItem:setDisabled( lDisabled )
   RETURN Qt_QTreeWidgetItem_setDisabled( ::pPtr, lDisabled )


METHOD QTreeWidgetItem:setExpanded( lExpand )
   RETURN Qt_QTreeWidgetItem_setExpanded( ::pPtr, lExpand )


METHOD QTreeWidgetItem:setFirstColumnSpanned( lSpan )
   RETURN Qt_QTreeWidgetItem_setFirstColumnSpanned( ::pPtr, lSpan )


METHOD QTreeWidgetItem:setFlags( nFlags )
   RETURN Qt_QTreeWidgetItem_setFlags( ::pPtr, nFlags )


METHOD QTreeWidgetItem:setFont( nColumn, pFont )
   RETURN Qt_QTreeWidgetItem_setFont( ::pPtr, nColumn, hbqt_ptr( pFont ) )


METHOD QTreeWidgetItem:setForeground( nColumn, pBrush )
   RETURN Qt_QTreeWidgetItem_setForeground( ::pPtr, nColumn, hbqt_ptr( pBrush ) )


METHOD QTreeWidgetItem:setHidden( lHide )
   RETURN Qt_QTreeWidgetItem_setHidden( ::pPtr, lHide )


METHOD QTreeWidgetItem:setIcon( nColumn, pIcon )
   RETURN Qt_QTreeWidgetItem_setIcon( ::pPtr, nColumn, hbqt_ptr( pIcon ) )


METHOD QTreeWidgetItem:setSelected( lSelect )
   RETURN Qt_QTreeWidgetItem_setSelected( ::pPtr, lSelect )


METHOD QTreeWidgetItem:setSizeHint( nColumn, pSize )
   RETURN Qt_QTreeWidgetItem_setSizeHint( ::pPtr, nColumn, hbqt_ptr( pSize ) )


METHOD QTreeWidgetItem:setStatusTip( nColumn, cStatusTip )
   RETURN Qt_QTreeWidgetItem_setStatusTip( ::pPtr, nColumn, cStatusTip )


METHOD QTreeWidgetItem:setText( nColumn, cText )
   RETURN Qt_QTreeWidgetItem_setText( ::pPtr, nColumn, cText )


METHOD QTreeWidgetItem:setTextAlignment( nColumn, nAlignment )
   RETURN Qt_QTreeWidgetItem_setTextAlignment( ::pPtr, nColumn, nAlignment )


METHOD QTreeWidgetItem:setToolTip( nColumn, cToolTip )
   RETURN Qt_QTreeWidgetItem_setToolTip( ::pPtr, nColumn, cToolTip )


METHOD QTreeWidgetItem:setWhatsThis( nColumn, cWhatsThis )
   RETURN Qt_QTreeWidgetItem_setWhatsThis( ::pPtr, nColumn, cWhatsThis )


METHOD QTreeWidgetItem:sizeHint( nColumn )
   RETURN Qt_QTreeWidgetItem_sizeHint( ::pPtr, nColumn )


METHOD QTreeWidgetItem:sortChildren( nColumn, nOrder )
   RETURN Qt_QTreeWidgetItem_sortChildren( ::pPtr, nColumn, nOrder )


METHOD QTreeWidgetItem:statusTip( nColumn )
   RETURN Qt_QTreeWidgetItem_statusTip( ::pPtr, nColumn )


METHOD QTreeWidgetItem:takeChild( nIndex )
   RETURN Qt_QTreeWidgetItem_takeChild( ::pPtr, nIndex )


METHOD QTreeWidgetItem:takeChildren()
   RETURN Qt_QTreeWidgetItem_takeChildren( ::pPtr )


METHOD QTreeWidgetItem:text( nColumn )
   RETURN Qt_QTreeWidgetItem_text( ::pPtr, nColumn )


METHOD QTreeWidgetItem:textAlignment( nColumn )
   RETURN Qt_QTreeWidgetItem_textAlignment( ::pPtr, nColumn )


METHOD QTreeWidgetItem:toolTip( nColumn )
   RETURN Qt_QTreeWidgetItem_toolTip( ::pPtr, nColumn )


METHOD QTreeWidgetItem:treeWidget()
   RETURN Qt_QTreeWidgetItem_treeWidget( ::pPtr )


METHOD QTreeWidgetItem:type()
   RETURN Qt_QTreeWidgetItem_type( ::pPtr )


METHOD QTreeWidgetItem:whatsThis( nColumn )
   RETURN Qt_QTreeWidgetItem_whatsThis( ::pPtr, nColumn )

