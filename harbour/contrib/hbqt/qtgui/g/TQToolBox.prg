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


FUNCTION QToolBox( ... )
   RETURN HB_QToolBox():new( ... )


CREATE CLASS QToolBox INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QToolBox

   METHOD  new( ... )

   METHOD  addItem( pWidget, pIconSet, cText )
   METHOD  addItem_1( pW, cText )
   METHOD  count()
   METHOD  currentIndex()
   METHOD  currentWidget()
   METHOD  indexOf( pWidget )
   METHOD  insertItem( nIndex, pWidget, pIcon, cText )
   METHOD  insertItem_1( nIndex, pWidget, cText )
   METHOD  isItemEnabled( nIndex )
   METHOD  itemIcon( nIndex )
   METHOD  itemText( nIndex )
   METHOD  itemToolTip( nIndex )
   METHOD  removeItem( nIndex )
   METHOD  setItemEnabled( nIndex, lEnabled )
   METHOD  setItemIcon( nIndex, pIcon )
   METHOD  setItemText( nIndex, cText )
   METHOD  setItemToolTip( nIndex, cToolTip )
   METHOD  widget( nIndex )
   METHOD  setCurrentIndex( nIndex )
   METHOD  setCurrentWidget( pWidget )

   ENDCLASS


METHOD QToolBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBox( ... )
   RETURN Self


METHOD QToolBox:addItem( pWidget, pIconSet, cText )
   RETURN Qt_QToolBox_addItem( ::pPtr, hbqt_ptr( pWidget ), hbqt_ptr( pIconSet ), cText )


METHOD QToolBox:addItem_1( pW, cText )
   RETURN Qt_QToolBox_addItem_1( ::pPtr, hbqt_ptr( pW ), cText )


METHOD QToolBox:count()
   RETURN Qt_QToolBox_count( ::pPtr )


METHOD QToolBox:currentIndex()
   RETURN Qt_QToolBox_currentIndex( ::pPtr )


METHOD QToolBox:currentWidget()
   RETURN Qt_QToolBox_currentWidget( ::pPtr )


METHOD QToolBox:indexOf( pWidget )
   RETURN Qt_QToolBox_indexOf( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QToolBox:insertItem( nIndex, pWidget, pIcon, cText )
   RETURN Qt_QToolBox_insertItem( ::pPtr, nIndex, hbqt_ptr( pWidget ), hbqt_ptr( pIcon ), cText )


METHOD QToolBox:insertItem_1( nIndex, pWidget, cText )
   RETURN Qt_QToolBox_insertItem_1( ::pPtr, nIndex, hbqt_ptr( pWidget ), cText )


METHOD QToolBox:isItemEnabled( nIndex )
   RETURN Qt_QToolBox_isItemEnabled( ::pPtr, nIndex )


METHOD QToolBox:itemIcon( nIndex )
   RETURN Qt_QToolBox_itemIcon( ::pPtr, nIndex )


METHOD QToolBox:itemText( nIndex )
   RETURN Qt_QToolBox_itemText( ::pPtr, nIndex )


METHOD QToolBox:itemToolTip( nIndex )
   RETURN Qt_QToolBox_itemToolTip( ::pPtr, nIndex )


METHOD QToolBox:removeItem( nIndex )
   RETURN Qt_QToolBox_removeItem( ::pPtr, nIndex )


METHOD QToolBox:setItemEnabled( nIndex, lEnabled )
   RETURN Qt_QToolBox_setItemEnabled( ::pPtr, nIndex, lEnabled )


METHOD QToolBox:setItemIcon( nIndex, pIcon )
   RETURN Qt_QToolBox_setItemIcon( ::pPtr, nIndex, hbqt_ptr( pIcon ) )


METHOD QToolBox:setItemText( nIndex, cText )
   RETURN Qt_QToolBox_setItemText( ::pPtr, nIndex, cText )


METHOD QToolBox:setItemToolTip( nIndex, cToolTip )
   RETURN Qt_QToolBox_setItemToolTip( ::pPtr, nIndex, cToolTip )


METHOD QToolBox:widget( nIndex )
   RETURN Qt_QToolBox_widget( ::pPtr, nIndex )


METHOD QToolBox:setCurrentIndex( nIndex )
   RETURN Qt_QToolBox_setCurrentIndex( ::pPtr, nIndex )


METHOD QToolBox:setCurrentWidget( pWidget )
   RETURN Qt_QToolBox_setCurrentWidget( ::pPtr, hbqt_ptr( pWidget ) )

