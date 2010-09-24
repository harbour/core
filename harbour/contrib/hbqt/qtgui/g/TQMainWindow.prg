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


FUNCTION QMainWindow( ... )
   RETURN HB_QMainWindow():new( ... )


CREATE CLASS QMainWindow INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMainWindow

   METHOD  new( ... )

   METHOD  addDockWidget( ... )
   METHOD  addToolBar( ... )
   METHOD  addToolBarBreak( nArea )
   METHOD  centralWidget()
   METHOD  corner( nCorner )
   METHOD  createPopupMenu()
   METHOD  dockOptions()
   METHOD  dockWidgetArea( pDockwidget )
   METHOD  documentMode()
   METHOD  iconSize()
   METHOD  insertToolBar( pBefore, pToolbar )
   METHOD  insertToolBarBreak( pBefore )
   METHOD  isAnimated()
   METHOD  isDockNestingEnabled()
   METHOD  menuBar()
   METHOD  menuWidget()
   METHOD  removeDockWidget( pDockwidget )
   METHOD  removeToolBar( pToolbar )
   METHOD  removeToolBarBreak( pBefore )
   METHOD  restoreDockWidget( pDockwidget )
   METHOD  restoreState( pState, nVersion )
   METHOD  saveState( nVersion )
   METHOD  setCentralWidget( pWidget )
   METHOD  setCorner( nCorner, nArea )
   METHOD  setDockOptions( nOptions )
   METHOD  setDocumentMode( lEnabled )
   METHOD  setIconSize( pIconSize )
   METHOD  setMenuBar( pMenuBar )
   METHOD  setMenuWidget( pMenuBar )
   METHOD  setStatusBar( pStatusbar )
   METHOD  setTabPosition( nAreas, nTabPosition )
   METHOD  setTabShape( nTabShape )
   METHOD  setToolButtonStyle( nToolButtonStyle )
   METHOD  setUnifiedTitleAndToolBarOnMac( lSet )
   METHOD  splitDockWidget( pFirst, pSecond, nOrientation )
   METHOD  statusBar()
   METHOD  tabPosition( nArea )
   METHOD  tabShape()
   METHOD  tabifiedDockWidgets( pDockwidget )
   METHOD  tabifyDockWidget( pFirst, pSecond )
   METHOD  toolBarArea( pToolbar )
   METHOD  toolBarBreak( pToolbar )
   METHOD  toolButtonStyle()
   METHOD  unifiedTitleAndToolBarOnMac()
   METHOD  setAnimated( lEnabled )
   METHOD  setDockNestingEnabled( lEnabled )

   ENDCLASS


METHOD QMainWindow:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMainWindow( ... )
   RETURN Self


METHOD QMainWindow:addDockWidget( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N"
                // void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget, Qt::Orientation orientation )
                // N n Qt::DockWidgetArea, PO p QDockWidget, N n Qt::Orientation
         RETURN Qt_QMainWindow_addDockWidget_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget )
                // N n Qt::DockWidgetArea, PO p QDockWidget
         RETURN Qt_QMainWindow_addDockWidget( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QMainWindow:addToolBar( ... )
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
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "PO"
                // void addToolBar ( Qt::ToolBarArea area, QToolBar * toolbar )
                // N n Qt::ToolBarArea, PO p QToolBar
         RETURN Qt_QMainWindow_addToolBar( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QToolBar * addToolBar ( const QString & title )               // NOT implemented
                // C c QString
         RETURN QToolBar():from( Qt_QMainWindow_addToolBar_2( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "PO"
                // void addToolBar ( QToolBar * toolbar )
                // PO p QToolBar
         RETURN Qt_QMainWindow_addToolBar_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QMainWindow:addToolBarBreak( nArea )
   RETURN Qt_QMainWindow_addToolBarBreak( ::pPtr, nArea )


METHOD QMainWindow:centralWidget()
   RETURN Qt_QMainWindow_centralWidget( ::pPtr )


METHOD QMainWindow:corner( nCorner )
   RETURN Qt_QMainWindow_corner( ::pPtr, nCorner )


METHOD QMainWindow:createPopupMenu()
   RETURN Qt_QMainWindow_createPopupMenu( ::pPtr )


METHOD QMainWindow:dockOptions()
   RETURN Qt_QMainWindow_dockOptions( ::pPtr )


METHOD QMainWindow:dockWidgetArea( pDockwidget )
   RETURN Qt_QMainWindow_dockWidgetArea( ::pPtr, hbqt_ptr( pDockwidget ) )


METHOD QMainWindow:documentMode()
   RETURN Qt_QMainWindow_documentMode( ::pPtr )


METHOD QMainWindow:iconSize()
   RETURN Qt_QMainWindow_iconSize( ::pPtr )


METHOD QMainWindow:insertToolBar( pBefore, pToolbar )
   RETURN Qt_QMainWindow_insertToolBar( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pToolbar ) )


METHOD QMainWindow:insertToolBarBreak( pBefore )
   RETURN Qt_QMainWindow_insertToolBarBreak( ::pPtr, hbqt_ptr( pBefore ) )


METHOD QMainWindow:isAnimated()
   RETURN Qt_QMainWindow_isAnimated( ::pPtr )


METHOD QMainWindow:isDockNestingEnabled()
   RETURN Qt_QMainWindow_isDockNestingEnabled( ::pPtr )


METHOD QMainWindow:menuBar()
   RETURN Qt_QMainWindow_menuBar( ::pPtr )


METHOD QMainWindow:menuWidget()
   RETURN Qt_QMainWindow_menuWidget( ::pPtr )


METHOD QMainWindow:removeDockWidget( pDockwidget )
   RETURN Qt_QMainWindow_removeDockWidget( ::pPtr, hbqt_ptr( pDockwidget ) )


METHOD QMainWindow:removeToolBar( pToolbar )
   RETURN Qt_QMainWindow_removeToolBar( ::pPtr, hbqt_ptr( pToolbar ) )


METHOD QMainWindow:removeToolBarBreak( pBefore )
   RETURN Qt_QMainWindow_removeToolBarBreak( ::pPtr, hbqt_ptr( pBefore ) )


METHOD QMainWindow:restoreDockWidget( pDockwidget )
   RETURN Qt_QMainWindow_restoreDockWidget( ::pPtr, hbqt_ptr( pDockwidget ) )


METHOD QMainWindow:restoreState( pState, nVersion )
   RETURN Qt_QMainWindow_restoreState( ::pPtr, hbqt_ptr( pState ), nVersion )


METHOD QMainWindow:saveState( nVersion )
   RETURN Qt_QMainWindow_saveState( ::pPtr, nVersion )


METHOD QMainWindow:setCentralWidget( pWidget )
   RETURN Qt_QMainWindow_setCentralWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QMainWindow:setCorner( nCorner, nArea )
   RETURN Qt_QMainWindow_setCorner( ::pPtr, nCorner, nArea )


METHOD QMainWindow:setDockOptions( nOptions )
   RETURN Qt_QMainWindow_setDockOptions( ::pPtr, nOptions )


METHOD QMainWindow:setDocumentMode( lEnabled )
   RETURN Qt_QMainWindow_setDocumentMode( ::pPtr, lEnabled )


METHOD QMainWindow:setIconSize( pIconSize )
   RETURN Qt_QMainWindow_setIconSize( ::pPtr, hbqt_ptr( pIconSize ) )


METHOD QMainWindow:setMenuBar( pMenuBar )
   RETURN Qt_QMainWindow_setMenuBar( ::pPtr, hbqt_ptr( pMenuBar ) )


METHOD QMainWindow:setMenuWidget( pMenuBar )
   RETURN Qt_QMainWindow_setMenuWidget( ::pPtr, hbqt_ptr( pMenuBar ) )


METHOD QMainWindow:setStatusBar( pStatusbar )
   RETURN Qt_QMainWindow_setStatusBar( ::pPtr, hbqt_ptr( pStatusbar ) )


METHOD QMainWindow:setTabPosition( nAreas, nTabPosition )
   RETURN Qt_QMainWindow_setTabPosition( ::pPtr, nAreas, nTabPosition )


METHOD QMainWindow:setTabShape( nTabShape )
   RETURN Qt_QMainWindow_setTabShape( ::pPtr, nTabShape )


METHOD QMainWindow:setToolButtonStyle( nToolButtonStyle )
   RETURN Qt_QMainWindow_setToolButtonStyle( ::pPtr, nToolButtonStyle )


METHOD QMainWindow:setUnifiedTitleAndToolBarOnMac( lSet )
   RETURN Qt_QMainWindow_setUnifiedTitleAndToolBarOnMac( ::pPtr, lSet )


METHOD QMainWindow:splitDockWidget( pFirst, pSecond, nOrientation )
   RETURN Qt_QMainWindow_splitDockWidget( ::pPtr, hbqt_ptr( pFirst ), hbqt_ptr( pSecond ), nOrientation )


METHOD QMainWindow:statusBar()
   RETURN Qt_QMainWindow_statusBar( ::pPtr )


METHOD QMainWindow:tabPosition( nArea )
   RETURN Qt_QMainWindow_tabPosition( ::pPtr, nArea )


METHOD QMainWindow:tabShape()
   RETURN Qt_QMainWindow_tabShape( ::pPtr )


METHOD QMainWindow:tabifiedDockWidgets( pDockwidget )
   RETURN Qt_QMainWindow_tabifiedDockWidgets( ::pPtr, hbqt_ptr( pDockwidget ) )


METHOD QMainWindow:tabifyDockWidget( pFirst, pSecond )
   RETURN Qt_QMainWindow_tabifyDockWidget( ::pPtr, hbqt_ptr( pFirst ), hbqt_ptr( pSecond ) )


METHOD QMainWindow:toolBarArea( pToolbar )
   RETURN Qt_QMainWindow_toolBarArea( ::pPtr, hbqt_ptr( pToolbar ) )


METHOD QMainWindow:toolBarBreak( pToolbar )
   RETURN Qt_QMainWindow_toolBarBreak( ::pPtr, hbqt_ptr( pToolbar ) )


METHOD QMainWindow:toolButtonStyle()
   RETURN Qt_QMainWindow_toolButtonStyle( ::pPtr )


METHOD QMainWindow:unifiedTitleAndToolBarOnMac()
   RETURN Qt_QMainWindow_unifiedTitleAndToolBarOnMac( ::pPtr )


METHOD QMainWindow:setAnimated( lEnabled )
   RETURN Qt_QMainWindow_setAnimated( ::pPtr, lEnabled )


METHOD QMainWindow:setDockNestingEnabled( lEnabled )
   RETURN Qt_QMainWindow_setDockNestingEnabled( ::pPtr, lEnabled )

