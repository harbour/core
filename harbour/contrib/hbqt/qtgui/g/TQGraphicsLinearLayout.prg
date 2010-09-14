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


FUNCTION QGraphicsLinearLayout( ... )
   RETURN HB_QGraphicsLinearLayout():new( ... )


CREATE CLASS QGraphicsLinearLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayout FUNCTION HB_QGraphicsLinearLayout

   METHOD  new( ... )

   METHOD  addItem( pItem )
   METHOD  addStretch( nStretch )
   METHOD  alignment( pItem )
   METHOD  insertItem( nIndex, pItem )
   METHOD  insertStretch( nIndex, nStretch )
   METHOD  itemSpacing( nIndex )
   METHOD  orientation()
   METHOD  removeAt( nIndex )
   METHOD  removeItem( pItem )
   METHOD  setAlignment( pItem, nAlignment )
   METHOD  setItemSpacing( nIndex, nSpacing )
   METHOD  setOrientation( nOrientation )
   METHOD  setSpacing( nSpacing )
   METHOD  setStretchFactor( pItem, nStretch )
   METHOD  spacing()
   METHOD  stretchFactor( pItem )

   ENDCLASS


METHOD QGraphicsLinearLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsLinearLayout( ... )
   RETURN Self


METHOD QGraphicsLinearLayout:addItem( pItem )
   RETURN Qt_QGraphicsLinearLayout_addItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsLinearLayout:addStretch( nStretch )
   RETURN Qt_QGraphicsLinearLayout_addStretch( ::pPtr, nStretch )


METHOD QGraphicsLinearLayout:alignment( pItem )
   RETURN Qt_QGraphicsLinearLayout_alignment( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsLinearLayout:insertItem( nIndex, pItem )
   RETURN Qt_QGraphicsLinearLayout_insertItem( ::pPtr, nIndex, hbqt_ptr( pItem ) )


METHOD QGraphicsLinearLayout:insertStretch( nIndex, nStretch )
   RETURN Qt_QGraphicsLinearLayout_insertStretch( ::pPtr, nIndex, nStretch )


METHOD QGraphicsLinearLayout:itemSpacing( nIndex )
   RETURN Qt_QGraphicsLinearLayout_itemSpacing( ::pPtr, nIndex )


METHOD QGraphicsLinearLayout:orientation()
   RETURN Qt_QGraphicsLinearLayout_orientation( ::pPtr )


METHOD QGraphicsLinearLayout:removeAt( nIndex )
   RETURN Qt_QGraphicsLinearLayout_removeAt( ::pPtr, nIndex )


METHOD QGraphicsLinearLayout:removeItem( pItem )
   RETURN Qt_QGraphicsLinearLayout_removeItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsLinearLayout:setAlignment( pItem, nAlignment )
   RETURN Qt_QGraphicsLinearLayout_setAlignment( ::pPtr, hbqt_ptr( pItem ), nAlignment )


METHOD QGraphicsLinearLayout:setItemSpacing( nIndex, nSpacing )
   RETURN Qt_QGraphicsLinearLayout_setItemSpacing( ::pPtr, nIndex, nSpacing )


METHOD QGraphicsLinearLayout:setOrientation( nOrientation )
   RETURN Qt_QGraphicsLinearLayout_setOrientation( ::pPtr, nOrientation )


METHOD QGraphicsLinearLayout:setSpacing( nSpacing )
   RETURN Qt_QGraphicsLinearLayout_setSpacing( ::pPtr, nSpacing )


METHOD QGraphicsLinearLayout:setStretchFactor( pItem, nStretch )
   RETURN Qt_QGraphicsLinearLayout_setStretchFactor( ::pPtr, hbqt_ptr( pItem ), nStretch )


METHOD QGraphicsLinearLayout:spacing()
   RETURN Qt_QGraphicsLinearLayout_spacing( ::pPtr )


METHOD QGraphicsLinearLayout:stretchFactor( pItem )
   RETURN Qt_QGraphicsLinearLayout_stretchFactor( ::pPtr, hbqt_ptr( pItem ) )

