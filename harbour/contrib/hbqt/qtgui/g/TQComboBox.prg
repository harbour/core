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


FUNCTION QComboBox( ... )
   RETURN HB_QComboBox():new( ... )


CREATE CLASS QComboBox INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QComboBox

   METHOD  new( ... )

   METHOD  addItem( cText, pUserData )
   METHOD  addItem_1( cIcon, cText, pUserData )
   METHOD  addItems( pTexts )
   METHOD  completer()
   METHOD  count()
   METHOD  currentIndex()
   METHOD  currentText()
   METHOD  duplicatesEnabled()
   METHOD  findData( pData, nRole, nFlags )
   METHOD  findText( cText, nFlags )
   METHOD  hasFrame()
   METHOD  hidePopup()
   METHOD  iconSize()
   METHOD  insertItem( nIndex, cText, pUserData )
   METHOD  insertItem_1( nIndex, cIcon, cText, pUserData )
   METHOD  insertItems( nIndex, pList )
   METHOD  insertPolicy()
   METHOD  insertSeparator( nIndex )
   METHOD  isEditable()
   METHOD  itemData( nIndex, nRole )
   METHOD  itemDelegate()
   METHOD  itemIcon( nIndex )
   METHOD  itemText( nIndex )
   METHOD  lineEdit()
   METHOD  maxCount()
   METHOD  maxVisibleItems()
   METHOD  minimumContentsLength()
   METHOD  model()
   METHOD  modelColumn()
   METHOD  removeItem( nIndex )
   METHOD  rootModelIndex()
   METHOD  setCompleter( pCompleter )
   METHOD  setDuplicatesEnabled( lEnable )
   METHOD  setEditable( lEditable )
   METHOD  setFrame( lBool )
   METHOD  setIconSize( pSize )
   METHOD  setInsertPolicy( nPolicy )
   METHOD  setItemData( nIndex, pValue, nRole )
   METHOD  setItemDelegate( pDelegate )
   METHOD  setItemIcon( nIndex, cIcon )
   METHOD  setItemText( nIndex, cText )
   METHOD  setLineEdit( pEdit )
   METHOD  setMaxCount( nMax )
   METHOD  setMaxVisibleItems( nMaxItems )
   METHOD  setMinimumContentsLength( nCharacters )
   METHOD  setModel( pModel )
   METHOD  setModelColumn( nVisibleColumn )
   METHOD  setRootModelIndex( pIndex )
   METHOD  setSizeAdjustPolicy( nPolicy )
   METHOD  setValidator( pValidator )
   METHOD  setView( pItemView )
   METHOD  showPopup()
   METHOD  sizeAdjustPolicy()
   METHOD  validator()
   METHOD  view()
   METHOD  clear()
   METHOD  clearEditText()
   METHOD  setCurrentIndex( nIndex )
   METHOD  setEditText( cText )

   ENDCLASS


METHOD QComboBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QComboBox( ... )
   RETURN Self


METHOD QComboBox:addItem( cText, pUserData )
   RETURN Qt_QComboBox_addItem( ::pPtr, cText, hbqt_ptr( pUserData ) )


METHOD QComboBox:addItem_1( cIcon, cText, pUserData )
   RETURN Qt_QComboBox_addItem_1( ::pPtr, cIcon, cText, hbqt_ptr( pUserData ) )


METHOD QComboBox:addItems( pTexts )
   RETURN Qt_QComboBox_addItems( ::pPtr, hbqt_ptr( pTexts ) )


METHOD QComboBox:completer()
   RETURN Qt_QComboBox_completer( ::pPtr )


METHOD QComboBox:count()
   RETURN Qt_QComboBox_count( ::pPtr )


METHOD QComboBox:currentIndex()
   RETURN Qt_QComboBox_currentIndex( ::pPtr )


METHOD QComboBox:currentText()
   RETURN Qt_QComboBox_currentText( ::pPtr )


METHOD QComboBox:duplicatesEnabled()
   RETURN Qt_QComboBox_duplicatesEnabled( ::pPtr )


METHOD QComboBox:findData( pData, nRole, nFlags )
   RETURN Qt_QComboBox_findData( ::pPtr, hbqt_ptr( pData ), nRole, nFlags )


METHOD QComboBox:findText( cText, nFlags )
   RETURN Qt_QComboBox_findText( ::pPtr, cText, nFlags )


METHOD QComboBox:hasFrame()
   RETURN Qt_QComboBox_hasFrame( ::pPtr )


METHOD QComboBox:hidePopup()
   RETURN Qt_QComboBox_hidePopup( ::pPtr )


METHOD QComboBox:iconSize()
   RETURN Qt_QComboBox_iconSize( ::pPtr )


METHOD QComboBox:insertItem( nIndex, cText, pUserData )
   RETURN Qt_QComboBox_insertItem( ::pPtr, nIndex, cText, hbqt_ptr( pUserData ) )


METHOD QComboBox:insertItem_1( nIndex, cIcon, cText, pUserData )
   RETURN Qt_QComboBox_insertItem_1( ::pPtr, nIndex, cIcon, cText, hbqt_ptr( pUserData ) )


METHOD QComboBox:insertItems( nIndex, pList )
   RETURN Qt_QComboBox_insertItems( ::pPtr, nIndex, hbqt_ptr( pList ) )


METHOD QComboBox:insertPolicy()
   RETURN Qt_QComboBox_insertPolicy( ::pPtr )


METHOD QComboBox:insertSeparator( nIndex )
   RETURN Qt_QComboBox_insertSeparator( ::pPtr, nIndex )


METHOD QComboBox:isEditable()
   RETURN Qt_QComboBox_isEditable( ::pPtr )


METHOD QComboBox:itemData( nIndex, nRole )
   RETURN Qt_QComboBox_itemData( ::pPtr, nIndex, nRole )


METHOD QComboBox:itemDelegate()
   RETURN Qt_QComboBox_itemDelegate( ::pPtr )


METHOD QComboBox:itemIcon( nIndex )
   RETURN Qt_QComboBox_itemIcon( ::pPtr, nIndex )


METHOD QComboBox:itemText( nIndex )
   RETURN Qt_QComboBox_itemText( ::pPtr, nIndex )


METHOD QComboBox:lineEdit()
   RETURN Qt_QComboBox_lineEdit( ::pPtr )


METHOD QComboBox:maxCount()
   RETURN Qt_QComboBox_maxCount( ::pPtr )


METHOD QComboBox:maxVisibleItems()
   RETURN Qt_QComboBox_maxVisibleItems( ::pPtr )


METHOD QComboBox:minimumContentsLength()
   RETURN Qt_QComboBox_minimumContentsLength( ::pPtr )


METHOD QComboBox:model()
   RETURN Qt_QComboBox_model( ::pPtr )


METHOD QComboBox:modelColumn()
   RETURN Qt_QComboBox_modelColumn( ::pPtr )


METHOD QComboBox:removeItem( nIndex )
   RETURN Qt_QComboBox_removeItem( ::pPtr, nIndex )


METHOD QComboBox:rootModelIndex()
   RETURN Qt_QComboBox_rootModelIndex( ::pPtr )


METHOD QComboBox:setCompleter( pCompleter )
   RETURN Qt_QComboBox_setCompleter( ::pPtr, hbqt_ptr( pCompleter ) )


METHOD QComboBox:setDuplicatesEnabled( lEnable )
   RETURN Qt_QComboBox_setDuplicatesEnabled( ::pPtr, lEnable )


METHOD QComboBox:setEditable( lEditable )
   RETURN Qt_QComboBox_setEditable( ::pPtr, lEditable )


METHOD QComboBox:setFrame( lBool )
   RETURN Qt_QComboBox_setFrame( ::pPtr, lBool )


METHOD QComboBox:setIconSize( pSize )
   RETURN Qt_QComboBox_setIconSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QComboBox:setInsertPolicy( nPolicy )
   RETURN Qt_QComboBox_setInsertPolicy( ::pPtr, nPolicy )


METHOD QComboBox:setItemData( nIndex, pValue, nRole )
   RETURN Qt_QComboBox_setItemData( ::pPtr, nIndex, hbqt_ptr( pValue ), nRole )


METHOD QComboBox:setItemDelegate( pDelegate )
   RETURN Qt_QComboBox_setItemDelegate( ::pPtr, hbqt_ptr( pDelegate ) )


METHOD QComboBox:setItemIcon( nIndex, cIcon )
   RETURN Qt_QComboBox_setItemIcon( ::pPtr, nIndex, cIcon )


METHOD QComboBox:setItemText( nIndex, cText )
   RETURN Qt_QComboBox_setItemText( ::pPtr, nIndex, cText )


METHOD QComboBox:setLineEdit( pEdit )
   RETURN Qt_QComboBox_setLineEdit( ::pPtr, hbqt_ptr( pEdit ) )


METHOD QComboBox:setMaxCount( nMax )
   RETURN Qt_QComboBox_setMaxCount( ::pPtr, nMax )


METHOD QComboBox:setMaxVisibleItems( nMaxItems )
   RETURN Qt_QComboBox_setMaxVisibleItems( ::pPtr, nMaxItems )


METHOD QComboBox:setMinimumContentsLength( nCharacters )
   RETURN Qt_QComboBox_setMinimumContentsLength( ::pPtr, nCharacters )


METHOD QComboBox:setModel( pModel )
   RETURN Qt_QComboBox_setModel( ::pPtr, hbqt_ptr( pModel ) )


METHOD QComboBox:setModelColumn( nVisibleColumn )
   RETURN Qt_QComboBox_setModelColumn( ::pPtr, nVisibleColumn )


METHOD QComboBox:setRootModelIndex( pIndex )
   RETURN Qt_QComboBox_setRootModelIndex( ::pPtr, hbqt_ptr( pIndex ) )


METHOD QComboBox:setSizeAdjustPolicy( nPolicy )
   RETURN Qt_QComboBox_setSizeAdjustPolicy( ::pPtr, nPolicy )


METHOD QComboBox:setValidator( pValidator )
   RETURN Qt_QComboBox_setValidator( ::pPtr, hbqt_ptr( pValidator ) )


METHOD QComboBox:setView( pItemView )
   RETURN Qt_QComboBox_setView( ::pPtr, hbqt_ptr( pItemView ) )


METHOD QComboBox:showPopup()
   RETURN Qt_QComboBox_showPopup( ::pPtr )


METHOD QComboBox:sizeAdjustPolicy()
   RETURN Qt_QComboBox_sizeAdjustPolicy( ::pPtr )


METHOD QComboBox:validator()
   RETURN Qt_QComboBox_validator( ::pPtr )


METHOD QComboBox:view()
   RETURN Qt_QComboBox_view( ::pPtr )


METHOD QComboBox:clear()
   RETURN Qt_QComboBox_clear( ::pPtr )


METHOD QComboBox:clearEditText()
   RETURN Qt_QComboBox_clearEditText( ::pPtr )


METHOD QComboBox:setCurrentIndex( nIndex )
   RETURN Qt_QComboBox_setCurrentIndex( ::pPtr, nIndex )


METHOD QComboBox:setEditText( cText )
   RETURN Qt_QComboBox_setEditText( ::pPtr, cText )

