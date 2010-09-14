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


FUNCTION QTabBar( ... )
   RETURN HB_QTabBar():new( ... )


CREATE CLASS QTabBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabBar

   METHOD  new( ... )

   METHOD  addTab( cText )
   METHOD  count()
   METHOD  currentIndex()
   METHOD  documentMode()
   METHOD  drawBase()
   METHOD  elideMode()
   METHOD  expanding()
   METHOD  iconSize()
   METHOD  insertTab( nIndex, cText )
   METHOD  isMovable()
   METHOD  isTabEnabled( nIndex )
   METHOD  moveTab( nFrom, nTo )
   METHOD  removeTab( nIndex )
   METHOD  selectionBehaviorOnRemove()
   METHOD  setDocumentMode( lSet )
   METHOD  setDrawBase( lDrawTheBase )
   METHOD  setElideMode( nQt_TextElideMode )
   METHOD  setExpanding( lEnabled )
   METHOD  setIconSize( pSize )
   METHOD  setMovable( lMovable )
   METHOD  setSelectionBehaviorOnRemove( nBehavior )
   METHOD  setShape( nShape )
   METHOD  setTabButton( nIndex, nPosition, pWidget )
   METHOD  setTabData( nIndex, pData )
   METHOD  setTabEnabled( nIndex, lEnabled )
   METHOD  setTabText( nIndex, cText )
   METHOD  setTabTextColor( nIndex, pColor )
   METHOD  setTabToolTip( nIndex, cTip )
   METHOD  setTabWhatsThis( nIndex, cText )
   METHOD  setTabsClosable( lClosable )
   METHOD  setUsesScrollButtons( lUseButtons )
   METHOD  shape()
   METHOD  tabAt( pPosition )
   METHOD  tabButton( nIndex, nPosition )
   METHOD  tabData( nIndex )
   METHOD  tabIcon( nIndex )
   METHOD  tabRect( nIndex )
   METHOD  tabText( nIndex )
   METHOD  tabTextColor( nIndex )
   METHOD  tabToolTip( nIndex )
   METHOD  tabWhatsThis( nIndex )
   METHOD  tabsClosable()
   METHOD  usesScrollButtons()
   METHOD  setCurrentIndex( nIndex )

   ENDCLASS


METHOD QTabBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabBar( ... )
   RETURN Self


METHOD QTabBar:addTab( cText )
   RETURN Qt_QTabBar_addTab( ::pPtr, cText )


METHOD QTabBar:count()
   RETURN Qt_QTabBar_count( ::pPtr )


METHOD QTabBar:currentIndex()
   RETURN Qt_QTabBar_currentIndex( ::pPtr )


METHOD QTabBar:documentMode()
   RETURN Qt_QTabBar_documentMode( ::pPtr )


METHOD QTabBar:drawBase()
   RETURN Qt_QTabBar_drawBase( ::pPtr )


METHOD QTabBar:elideMode()
   RETURN Qt_QTabBar_elideMode( ::pPtr )


METHOD QTabBar:expanding()
   RETURN Qt_QTabBar_expanding( ::pPtr )


METHOD QTabBar:iconSize()
   RETURN Qt_QTabBar_iconSize( ::pPtr )


METHOD QTabBar:insertTab( nIndex, cText )
   RETURN Qt_QTabBar_insertTab( ::pPtr, nIndex, cText )


METHOD QTabBar:isMovable()
   RETURN Qt_QTabBar_isMovable( ::pPtr )


METHOD QTabBar:isTabEnabled( nIndex )
   RETURN Qt_QTabBar_isTabEnabled( ::pPtr, nIndex )


METHOD QTabBar:moveTab( nFrom, nTo )
   RETURN Qt_QTabBar_moveTab( ::pPtr, nFrom, nTo )


METHOD QTabBar:removeTab( nIndex )
   RETURN Qt_QTabBar_removeTab( ::pPtr, nIndex )


METHOD QTabBar:selectionBehaviorOnRemove()
   RETURN Qt_QTabBar_selectionBehaviorOnRemove( ::pPtr )


METHOD QTabBar:setDocumentMode( lSet )
   RETURN Qt_QTabBar_setDocumentMode( ::pPtr, lSet )


METHOD QTabBar:setDrawBase( lDrawTheBase )
   RETURN Qt_QTabBar_setDrawBase( ::pPtr, lDrawTheBase )


METHOD QTabBar:setElideMode( nQt_TextElideMode )
   RETURN Qt_QTabBar_setElideMode( ::pPtr, nQt_TextElideMode )


METHOD QTabBar:setExpanding( lEnabled )
   RETURN Qt_QTabBar_setExpanding( ::pPtr, lEnabled )


METHOD QTabBar:setIconSize( pSize )
   RETURN Qt_QTabBar_setIconSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QTabBar:setMovable( lMovable )
   RETURN Qt_QTabBar_setMovable( ::pPtr, lMovable )


METHOD QTabBar:setSelectionBehaviorOnRemove( nBehavior )
   RETURN Qt_QTabBar_setSelectionBehaviorOnRemove( ::pPtr, nBehavior )


METHOD QTabBar:setShape( nShape )
   RETURN Qt_QTabBar_setShape( ::pPtr, nShape )


METHOD QTabBar:setTabButton( nIndex, nPosition, pWidget )
   RETURN Qt_QTabBar_setTabButton( ::pPtr, nIndex, nPosition, hbqt_ptr( pWidget ) )


METHOD QTabBar:setTabData( nIndex, pData )
   RETURN Qt_QTabBar_setTabData( ::pPtr, nIndex, hbqt_ptr( pData ) )


METHOD QTabBar:setTabEnabled( nIndex, lEnabled )
   RETURN Qt_QTabBar_setTabEnabled( ::pPtr, nIndex, lEnabled )


METHOD QTabBar:setTabText( nIndex, cText )
   RETURN Qt_QTabBar_setTabText( ::pPtr, nIndex, cText )


METHOD QTabBar:setTabTextColor( nIndex, pColor )
   RETURN Qt_QTabBar_setTabTextColor( ::pPtr, nIndex, hbqt_ptr( pColor ) )


METHOD QTabBar:setTabToolTip( nIndex, cTip )
   RETURN Qt_QTabBar_setTabToolTip( ::pPtr, nIndex, cTip )


METHOD QTabBar:setTabWhatsThis( nIndex, cText )
   RETURN Qt_QTabBar_setTabWhatsThis( ::pPtr, nIndex, cText )


METHOD QTabBar:setTabsClosable( lClosable )
   RETURN Qt_QTabBar_setTabsClosable( ::pPtr, lClosable )


METHOD QTabBar:setUsesScrollButtons( lUseButtons )
   RETURN Qt_QTabBar_setUsesScrollButtons( ::pPtr, lUseButtons )


METHOD QTabBar:shape()
   RETURN Qt_QTabBar_shape( ::pPtr )


METHOD QTabBar:tabAt( pPosition )
   RETURN Qt_QTabBar_tabAt( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QTabBar:tabButton( nIndex, nPosition )
   RETURN Qt_QTabBar_tabButton( ::pPtr, nIndex, nPosition )


METHOD QTabBar:tabData( nIndex )
   RETURN Qt_QTabBar_tabData( ::pPtr, nIndex )


METHOD QTabBar:tabIcon( nIndex )
   RETURN Qt_QTabBar_tabIcon( ::pPtr, nIndex )


METHOD QTabBar:tabRect( nIndex )
   RETURN Qt_QTabBar_tabRect( ::pPtr, nIndex )


METHOD QTabBar:tabText( nIndex )
   RETURN Qt_QTabBar_tabText( ::pPtr, nIndex )


METHOD QTabBar:tabTextColor( nIndex )
   RETURN Qt_QTabBar_tabTextColor( ::pPtr, nIndex )


METHOD QTabBar:tabToolTip( nIndex )
   RETURN Qt_QTabBar_tabToolTip( ::pPtr, nIndex )


METHOD QTabBar:tabWhatsThis( nIndex )
   RETURN Qt_QTabBar_tabWhatsThis( ::pPtr, nIndex )


METHOD QTabBar:tabsClosable()
   RETURN Qt_QTabBar_tabsClosable( ::pPtr )


METHOD QTabBar:usesScrollButtons()
   RETURN Qt_QTabBar_usesScrollButtons( ::pPtr )


METHOD QTabBar:setCurrentIndex( nIndex )
   RETURN Qt_QTabBar_setCurrentIndex( ::pPtr, nIndex )

