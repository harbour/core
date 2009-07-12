/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


CREATE CLASS QTabWidget INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTabWidget_destroy( ::pPtr )

   METHOD  addTab( pPage, cLabel )             INLINE  Qt_QTabWidget_addTab( ::pPtr, pPage, cLabel )
   METHOD  addTab_1( pPage, cIcon, cLabel )    INLINE  Qt_QTabWidget_addTab_1( ::pPtr, pPage, cIcon, cLabel )
   METHOD  clear()                             INLINE  Qt_QTabWidget_clear( ::pPtr )
   METHOD  cornerWidget( nCorner )             INLINE  Qt_QTabWidget_cornerWidget( ::pPtr, nCorner )
   METHOD  count()                             INLINE  Qt_QTabWidget_count( ::pPtr )
   METHOD  currentIndex()                      INLINE  Qt_QTabWidget_currentIndex( ::pPtr )
   METHOD  currentWidget()                     INLINE  Qt_QTabWidget_currentWidget( ::pPtr )
   METHOD  documentMode()                      INLINE  Qt_QTabWidget_documentMode( ::pPtr )
   METHOD  elideMode()                         INLINE  Qt_QTabWidget_elideMode( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QTabWidget_iconSize( ::pPtr )
   METHOD  indexOf( pW )                       INLINE  Qt_QTabWidget_indexOf( ::pPtr, pW )
   METHOD  insertTab( nIndex, pPage, cLabel )  INLINE  Qt_QTabWidget_insertTab( ::pPtr, nIndex, pPage, cLabel )
   METHOD  insertTab_1( nIndex, pPage, cIcon, cLabel )  INLINE  Qt_QTabWidget_insertTab_1( ::pPtr, nIndex, pPage, cIcon, cLabel )
   METHOD  isMovable()                         INLINE  Qt_QTabWidget_isMovable( ::pPtr )
   METHOD  isTabEnabled( nIndex )              INLINE  Qt_QTabWidget_isTabEnabled( ::pPtr, nIndex )
   METHOD  removeTab( nIndex )                 INLINE  Qt_QTabWidget_removeTab( ::pPtr, nIndex )
   METHOD  setCornerWidget( pWidget, nCorner )  INLINE  Qt_QTabWidget_setCornerWidget( ::pPtr, pWidget, nCorner )
   METHOD  setDocumentMode( lSet )             INLINE  Qt_QTabWidget_setDocumentMode( ::pPtr, lSet )
   METHOD  setElideMode( nQt_TextElideMode )   INLINE  Qt_QTabWidget_setElideMode( ::pPtr, nQt_TextElideMode )
   METHOD  setIconSize( pSize )                INLINE  Qt_QTabWidget_setIconSize( ::pPtr, pSize )
   METHOD  setMovable( lMovable )              INLINE  Qt_QTabWidget_setMovable( ::pPtr, lMovable )
   METHOD  setTabEnabled( nIndex, lEnable )    INLINE  Qt_QTabWidget_setTabEnabled( ::pPtr, nIndex, lEnable )
   METHOD  setTabIcon( nIndex, cIcon )         INLINE  Qt_QTabWidget_setTabIcon( ::pPtr, nIndex, cIcon )
   METHOD  setTabPosition( nTabPosition )      INLINE  Qt_QTabWidget_setTabPosition( ::pPtr, nTabPosition )
   METHOD  setTabShape( nS )                   INLINE  Qt_QTabWidget_setTabShape( ::pPtr, nS )
   METHOD  setTabText( nIndex, cLabel )        INLINE  Qt_QTabWidget_setTabText( ::pPtr, nIndex, cLabel )
   METHOD  setTabToolTip( nIndex, cTip )       INLINE  Qt_QTabWidget_setTabToolTip( ::pPtr, nIndex, cTip )
   METHOD  setTabWhatsThis( nIndex, cText )    INLINE  Qt_QTabWidget_setTabWhatsThis( ::pPtr, nIndex, cText )
   METHOD  setTabsClosable( lCloseable )       INLINE  Qt_QTabWidget_setTabsClosable( ::pPtr, lCloseable )
   METHOD  setUsesScrollButtons( lUseButtons )  INLINE  Qt_QTabWidget_setUsesScrollButtons( ::pPtr, lUseButtons )
   METHOD  tabIcon( nIndex )                   INLINE  Qt_QTabWidget_tabIcon( ::pPtr, nIndex )
   METHOD  tabPosition()                       INLINE  Qt_QTabWidget_tabPosition( ::pPtr )
   METHOD  tabShape()                          INLINE  Qt_QTabWidget_tabShape( ::pPtr )
   METHOD  tabText( nIndex )                   INLINE  Qt_QTabWidget_tabText( ::pPtr, nIndex )
   METHOD  tabToolTip( nIndex )                INLINE  Qt_QTabWidget_tabToolTip( ::pPtr, nIndex )
   METHOD  tabWhatsThis( nIndex )              INLINE  Qt_QTabWidget_tabWhatsThis( ::pPtr, nIndex )
   METHOD  tabsClosable()                      INLINE  Qt_QTabWidget_tabsClosable( ::pPtr )
   METHOD  usesScrollButtons()                 INLINE  Qt_QTabWidget_usesScrollButtons( ::pPtr )
   METHOD  widget( nIndex )                    INLINE  Qt_QTabWidget_widget( ::pPtr, nIndex )
   METHOD  setCurrentIndex( nIndex )           INLINE  Qt_QTabWidget_setCurrentIndex( ::pPtr, nIndex )
   METHOD  setCurrentWidget( pWidget )         INLINE  Qt_QTabWidget_setCurrentWidget( ::pPtr, pWidget )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTabWidget

   ::pParent := pParent

   ::pPtr := Qt_QTabWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTabWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

