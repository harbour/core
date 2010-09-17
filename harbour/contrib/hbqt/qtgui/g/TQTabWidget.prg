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


FUNCTION QTabWidget( ... )
   RETURN HB_QTabWidget():new( ... )


CREATE CLASS QTabWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabWidget

   METHOD  new( ... )

   METHOD  addTab( pPage, cLabel )
   METHOD  addTab_1( pPage, pIcon, cLabel )
   METHOD  clear()
   METHOD  cornerWidget( nCorner )
   METHOD  count()
   METHOD  currentIndex()
   METHOD  currentWidget()
   METHOD  documentMode()
   METHOD  elideMode()
   METHOD  iconSize()
   METHOD  indexOf( pW )
   METHOD  insertTab( nIndex, pPage, cLabel )
   METHOD  insertTab_1( nIndex, pPage, pIcon, cLabel )
   METHOD  isMovable()
   METHOD  isTabEnabled( nIndex )
   METHOD  removeTab( nIndex )
   METHOD  setCornerWidget( pWidget, nCorner )
   METHOD  setDocumentMode( lSet )
   METHOD  setElideMode( nQt_TextElideMode )
   METHOD  setIconSize( pSize )
   METHOD  setMovable( lMovable )
   METHOD  setTabEnabled( nIndex, lEnable )
   METHOD  setTabIcon( nIndex, pIcon )
   METHOD  setTabPosition( nTabPosition )
   METHOD  setTabShape( nS )
   METHOD  setTabText( nIndex, cLabel )
   METHOD  setTabToolTip( nIndex, cTip )
   METHOD  setTabWhatsThis( nIndex, cText )
   METHOD  setTabsClosable( lCloseable )
   METHOD  setUsesScrollButtons( lUseButtons )
   METHOD  tabIcon( nIndex )
   METHOD  tabPosition()
   METHOD  tabShape()
   METHOD  tabText( nIndex )
   METHOD  tabToolTip( nIndex )
   METHOD  tabWhatsThis( nIndex )
   METHOD  tabsClosable()
   METHOD  usesScrollButtons()
   METHOD  widget( nIndex )
   METHOD  setCurrentIndex( nIndex )
   METHOD  setCurrentWidget( pWidget )

   ENDCLASS


METHOD QTabWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabWidget( ... )
   RETURN Self


METHOD QTabWidget:addTab( pPage, cLabel )
   RETURN Qt_QTabWidget_addTab( ::pPtr, hbqt_ptr( pPage ), cLabel )


METHOD QTabWidget:addTab_1( pPage, pIcon, cLabel )
   RETURN Qt_QTabWidget_addTab_1( ::pPtr, hbqt_ptr( pPage ), hbqt_ptr( pIcon ), cLabel )


METHOD QTabWidget:clear()
   RETURN Qt_QTabWidget_clear( ::pPtr )


METHOD QTabWidget:cornerWidget( nCorner )
   RETURN Qt_QTabWidget_cornerWidget( ::pPtr, nCorner )


METHOD QTabWidget:count()
   RETURN Qt_QTabWidget_count( ::pPtr )


METHOD QTabWidget:currentIndex()
   RETURN Qt_QTabWidget_currentIndex( ::pPtr )


METHOD QTabWidget:currentWidget()
   RETURN Qt_QTabWidget_currentWidget( ::pPtr )


METHOD QTabWidget:documentMode()
   RETURN Qt_QTabWidget_documentMode( ::pPtr )


METHOD QTabWidget:elideMode()
   RETURN Qt_QTabWidget_elideMode( ::pPtr )


METHOD QTabWidget:iconSize()
   RETURN Qt_QTabWidget_iconSize( ::pPtr )


METHOD QTabWidget:indexOf( pW )
   RETURN Qt_QTabWidget_indexOf( ::pPtr, hbqt_ptr( pW ) )


METHOD QTabWidget:insertTab( nIndex, pPage, cLabel )
   RETURN Qt_QTabWidget_insertTab( ::pPtr, nIndex, hbqt_ptr( pPage ), cLabel )


METHOD QTabWidget:insertTab_1( nIndex, pPage, pIcon, cLabel )
   RETURN Qt_QTabWidget_insertTab_1( ::pPtr, nIndex, hbqt_ptr( pPage ), hbqt_ptr( pIcon ), cLabel )


METHOD QTabWidget:isMovable()
   RETURN Qt_QTabWidget_isMovable( ::pPtr )


METHOD QTabWidget:isTabEnabled( nIndex )
   RETURN Qt_QTabWidget_isTabEnabled( ::pPtr, nIndex )


METHOD QTabWidget:removeTab( nIndex )
   RETURN Qt_QTabWidget_removeTab( ::pPtr, nIndex )


METHOD QTabWidget:setCornerWidget( pWidget, nCorner )
   RETURN Qt_QTabWidget_setCornerWidget( ::pPtr, hbqt_ptr( pWidget ), nCorner )


METHOD QTabWidget:setDocumentMode( lSet )
   RETURN Qt_QTabWidget_setDocumentMode( ::pPtr, lSet )


METHOD QTabWidget:setElideMode( nQt_TextElideMode )
   RETURN Qt_QTabWidget_setElideMode( ::pPtr, nQt_TextElideMode )


METHOD QTabWidget:setIconSize( pSize )
   RETURN Qt_QTabWidget_setIconSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QTabWidget:setMovable( lMovable )
   RETURN Qt_QTabWidget_setMovable( ::pPtr, lMovable )


METHOD QTabWidget:setTabEnabled( nIndex, lEnable )
   RETURN Qt_QTabWidget_setTabEnabled( ::pPtr, nIndex, lEnable )


METHOD QTabWidget:setTabIcon( nIndex, pIcon )
   RETURN Qt_QTabWidget_setTabIcon( ::pPtr, nIndex, hbqt_ptr( pIcon ) )


METHOD QTabWidget:setTabPosition( nTabPosition )
   RETURN Qt_QTabWidget_setTabPosition( ::pPtr, nTabPosition )


METHOD QTabWidget:setTabShape( nS )
   RETURN Qt_QTabWidget_setTabShape( ::pPtr, nS )


METHOD QTabWidget:setTabText( nIndex, cLabel )
   RETURN Qt_QTabWidget_setTabText( ::pPtr, nIndex, cLabel )


METHOD QTabWidget:setTabToolTip( nIndex, cTip )
   RETURN Qt_QTabWidget_setTabToolTip( ::pPtr, nIndex, cTip )


METHOD QTabWidget:setTabWhatsThis( nIndex, cText )
   RETURN Qt_QTabWidget_setTabWhatsThis( ::pPtr, nIndex, cText )


METHOD QTabWidget:setTabsClosable( lCloseable )
   RETURN Qt_QTabWidget_setTabsClosable( ::pPtr, lCloseable )


METHOD QTabWidget:setUsesScrollButtons( lUseButtons )
   RETURN Qt_QTabWidget_setUsesScrollButtons( ::pPtr, lUseButtons )


METHOD QTabWidget:tabIcon( nIndex )
   RETURN Qt_QTabWidget_tabIcon( ::pPtr, nIndex )


METHOD QTabWidget:tabPosition()
   RETURN Qt_QTabWidget_tabPosition( ::pPtr )


METHOD QTabWidget:tabShape()
   RETURN Qt_QTabWidget_tabShape( ::pPtr )


METHOD QTabWidget:tabText( nIndex )
   RETURN Qt_QTabWidget_tabText( ::pPtr, nIndex )


METHOD QTabWidget:tabToolTip( nIndex )
   RETURN Qt_QTabWidget_tabToolTip( ::pPtr, nIndex )


METHOD QTabWidget:tabWhatsThis( nIndex )
   RETURN Qt_QTabWidget_tabWhatsThis( ::pPtr, nIndex )


METHOD QTabWidget:tabsClosable()
   RETURN Qt_QTabWidget_tabsClosable( ::pPtr )


METHOD QTabWidget:usesScrollButtons()
   RETURN Qt_QTabWidget_usesScrollButtons( ::pPtr )


METHOD QTabWidget:widget( nIndex )
   RETURN Qt_QTabWidget_widget( ::pPtr, nIndex )


METHOD QTabWidget:setCurrentIndex( nIndex )
   RETURN Qt_QTabWidget_setCurrentIndex( ::pPtr, nIndex )


METHOD QTabWidget:setCurrentWidget( pWidget )
   RETURN Qt_QTabWidget_setCurrentWidget( ::pPtr, hbqt_ptr( pWidget ) )

