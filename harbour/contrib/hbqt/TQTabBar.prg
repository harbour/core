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


CLASS QTabBar INHERIT QWidget

   DATA    pPtr

   METHOD  New()

   METHOD  addTab( cText )                     INLINE  Qt_QTabBar_addTab( ::pPtr, cText )
   METHOD  count()                             INLINE  Qt_QTabBar_count( ::pPtr )
   METHOD  currentIndex()                      INLINE  Qt_QTabBar_currentIndex( ::pPtr )
   METHOD  documentMode()                      INLINE  Qt_QTabBar_documentMode( ::pPtr )
   METHOD  drawBase()                          INLINE  Qt_QTabBar_drawBase( ::pPtr )
   METHOD  elideMode()                         INLINE  Qt_QTabBar_elideMode( ::pPtr )
   METHOD  expanding()                         INLINE  Qt_QTabBar_expanding( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QTabBar_iconSize( ::pPtr )
   METHOD  insertTab( nIndex, cText )          INLINE  Qt_QTabBar_insertTab( ::pPtr, nIndex, cText )
   METHOD  isMovable()                         INLINE  Qt_QTabBar_isMovable( ::pPtr )
   METHOD  isTabEnabled( nIndex )              INLINE  Qt_QTabBar_isTabEnabled( ::pPtr, nIndex )
   METHOD  moveTab( nFrom, nTo )               INLINE  Qt_QTabBar_moveTab( ::pPtr, nFrom, nTo )
   METHOD  removeTab( nIndex )                 INLINE  Qt_QTabBar_removeTab( ::pPtr, nIndex )
   METHOD  selectionBehaviorOnRemove()         INLINE  Qt_QTabBar_selectionBehaviorOnRemove( ::pPtr )
   METHOD  setDocumentMode( lSet )             INLINE  Qt_QTabBar_setDocumentMode( ::pPtr, lSet )
   METHOD  setDrawBase( lDrawTheBase )         INLINE  Qt_QTabBar_setDrawBase( ::pPtr, lDrawTheBase )
   METHOD  setElideMode( nQt_TextElideMode )   INLINE  Qt_QTabBar_setElideMode( ::pPtr, nQt_TextElideMode )
   METHOD  setExpanding( lEnabled )            INLINE  Qt_QTabBar_setExpanding( ::pPtr, lEnabled )
   METHOD  setIconSize( aSizeSize )            INLINE  Qt_QTabBar_setIconSize( ::pPtr, aSizeSize )
   METHOD  setMovable( lMovable )              INLINE  Qt_QTabBar_setMovable( ::pPtr, lMovable )
   METHOD  setSelectionBehaviorOnRemove( nSelectionBehavior )  INLINE  Qt_QTabBar_setSelectionBehaviorOnRemove( ::pPtr, nSelectionBehavior )
   METHOD  setShape( nShape )                  INLINE  Qt_QTabBar_setShape( ::pPtr, nShape )
   METHOD  setTabButton( nIndex, nButtonPosition, pWidget )  INLINE  Qt_QTabBar_setTabButton( ::pPtr, nIndex, nButtonPosition, pWidget )
   METHOD  setTabEnabled( nIndex, lEnabled )   INLINE  Qt_QTabBar_setTabEnabled( ::pPtr, nIndex, lEnabled )
   METHOD  setTabText( nIndex, cText )         INLINE  Qt_QTabBar_setTabText( ::pPtr, nIndex, cText )
   METHOD  setTabToolTip( nIndex, cTip )       INLINE  Qt_QTabBar_setTabToolTip( ::pPtr, nIndex, cTip )
   METHOD  setTabWhatsThis( nIndex, cText )    INLINE  Qt_QTabBar_setTabWhatsThis( ::pPtr, nIndex, cText )
   METHOD  setTabsClosable( lClosable )        INLINE  Qt_QTabBar_setTabsClosable( ::pPtr, lClosable )
   METHOD  setUsesScrollButtons( lUseButtons )  INLINE  Qt_QTabBar_setUsesScrollButtons( ::pPtr, lUseButtons )
   METHOD  shape()                             INLINE  Qt_QTabBar_shape( ::pPtr )
   METHOD  tabAt( aPointPosition )             INLINE  Qt_QTabBar_tabAt( ::pPtr, aPointPosition )
   METHOD  tabButton( nIndex, nButtonPosition )  INLINE  Qt_QTabBar_tabButton( ::pPtr, nIndex, nButtonPosition )
   METHOD  tabRect( nIndex )                   INLINE  Qt_QTabBar_tabRect( ::pPtr, nIndex )
   METHOD  tabText( nIndex )                   INLINE  Qt_QTabBar_tabText( ::pPtr, nIndex )
   METHOD  tabToolTip( nIndex )                INLINE  Qt_QTabBar_tabToolTip( ::pPtr, nIndex )
   METHOD  tabWhatsThis( nIndex )              INLINE  Qt_QTabBar_tabWhatsThis( ::pPtr, nIndex )
   METHOD  tabsClosable()                      INLINE  Qt_QTabBar_tabsClosable( ::pPtr )
   METHOD  usesScrollButtons()                 INLINE  Qt_QTabBar_usesScrollButtons( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTabBar

   ::pPtr := Qt_QTabBar( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

