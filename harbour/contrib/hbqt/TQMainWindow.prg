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


CREATE CLASS QMainWindow INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QMainWindow_destroy( ::pPtr )

   METHOD  addDockWidget( nArea, pDockwidget )  INLINE  Qt_QMainWindow_addDockWidget( ::pPtr, nArea, pDockwidget )
   METHOD  addDockWidget_1( nArea, pDockwidget, nOrientation )  INLINE  Qt_QMainWindow_addDockWidget_1( ::pPtr, nArea, pDockwidget, nOrientation )
   METHOD  addToolBar( nArea, pToolbar )       INLINE  Qt_QMainWindow_addToolBar( ::pPtr, nArea, pToolbar )
   METHOD  addToolBar_1( pToolbar )            INLINE  Qt_QMainWindow_addToolBar_1( ::pPtr, pToolbar )
   METHOD  addToolBar_2( cTitle )              INLINE  Qt_QMainWindow_addToolBar_2( ::pPtr, cTitle )
   METHOD  addToolBarBreak( nArea )            INLINE  Qt_QMainWindow_addToolBarBreak( ::pPtr, nArea )
   METHOD  centralWidget()                     INLINE  Qt_QMainWindow_centralWidget( ::pPtr )
   METHOD  corner( nCorner )                   INLINE  Qt_QMainWindow_corner( ::pPtr, nCorner )
   METHOD  createPopupMenu()                   INLINE  Qt_QMainWindow_createPopupMenu( ::pPtr )
   METHOD  dockOptions()                       INLINE  Qt_QMainWindow_dockOptions( ::pPtr )
   METHOD  dockWidgetArea( pDockwidget )       INLINE  Qt_QMainWindow_dockWidgetArea( ::pPtr, pDockwidget )
   METHOD  documentMode()                      INLINE  Qt_QMainWindow_documentMode( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QMainWindow_iconSize( ::pPtr )
   METHOD  insertToolBar( pBefore, pToolbar )  INLINE  Qt_QMainWindow_insertToolBar( ::pPtr, pBefore, pToolbar )
   METHOD  insertToolBarBreak( pBefore )       INLINE  Qt_QMainWindow_insertToolBarBreak( ::pPtr, pBefore )
   METHOD  isAnimated()                        INLINE  Qt_QMainWindow_isAnimated( ::pPtr )
   METHOD  isDockNestingEnabled()              INLINE  Qt_QMainWindow_isDockNestingEnabled( ::pPtr )
   METHOD  menuBar()                           INLINE  Qt_QMainWindow_menuBar( ::pPtr )
   METHOD  menuWidget()                        INLINE  Qt_QMainWindow_menuWidget( ::pPtr )
   METHOD  removeDockWidget( pDockwidget )     INLINE  Qt_QMainWindow_removeDockWidget( ::pPtr, pDockwidget )
   METHOD  removeToolBar( pToolbar )           INLINE  Qt_QMainWindow_removeToolBar( ::pPtr, pToolbar )
   METHOD  removeToolBarBreak( pBefore )       INLINE  Qt_QMainWindow_removeToolBarBreak( ::pPtr, pBefore )
   METHOD  restoreDockWidget( pDockwidget )    INLINE  Qt_QMainWindow_restoreDockWidget( ::pPtr, pDockwidget )
   METHOD  restoreState( pState, nVersion )    INLINE  Qt_QMainWindow_restoreState( ::pPtr, pState, nVersion )
   METHOD  saveState( nVersion )               INLINE  Qt_QMainWindow_saveState( ::pPtr, nVersion )
   METHOD  setCentralWidget( pWidget )         INLINE  Qt_QMainWindow_setCentralWidget( ::pPtr, pWidget )
   METHOD  setCorner( nCorner, nArea )         INLINE  Qt_QMainWindow_setCorner( ::pPtr, nCorner, nArea )
   METHOD  setDockOptions( nOptions )          INLINE  Qt_QMainWindow_setDockOptions( ::pPtr, nOptions )
   METHOD  setDocumentMode( lEnabled )         INLINE  Qt_QMainWindow_setDocumentMode( ::pPtr, lEnabled )
   METHOD  setIconSize( pIconSize )            INLINE  Qt_QMainWindow_setIconSize( ::pPtr, pIconSize )
   METHOD  setMenuBar( pMenuBar )              INLINE  Qt_QMainWindow_setMenuBar( ::pPtr, pMenuBar )
   METHOD  setMenuWidget( pMenuBar )           INLINE  Qt_QMainWindow_setMenuWidget( ::pPtr, pMenuBar )
   METHOD  setStatusBar( pStatusbar )          INLINE  Qt_QMainWindow_setStatusBar( ::pPtr, pStatusbar )
   METHOD  setTabPosition( nAreas, nTabPosition )  INLINE  Qt_QMainWindow_setTabPosition( ::pPtr, nAreas, nTabPosition )
   METHOD  setTabShape( nTabShape )            INLINE  Qt_QMainWindow_setTabShape( ::pPtr, nTabShape )
   METHOD  setToolButtonStyle( nToolButtonStyle )  INLINE  Qt_QMainWindow_setToolButtonStyle( ::pPtr, nToolButtonStyle )
   METHOD  setUnifiedTitleAndToolBarOnMac( lSet )  INLINE  Qt_QMainWindow_setUnifiedTitleAndToolBarOnMac( ::pPtr, lSet )
   METHOD  splitDockWidget( pFirst, pSecond, nOrientation )  INLINE  Qt_QMainWindow_splitDockWidget( ::pPtr, pFirst, pSecond, nOrientation )
   METHOD  statusBar()                         INLINE  Qt_QMainWindow_statusBar( ::pPtr )
   METHOD  tabPosition( nArea )                INLINE  Qt_QMainWindow_tabPosition( ::pPtr, nArea )
   METHOD  tabShape()                          INLINE  Qt_QMainWindow_tabShape( ::pPtr )
   METHOD  tabifyDockWidget( pFirst, pSecond )  INLINE  Qt_QMainWindow_tabifyDockWidget( ::pPtr, pFirst, pSecond )
   METHOD  toolBarArea( pToolbar )             INLINE  Qt_QMainWindow_toolBarArea( ::pPtr, pToolbar )
   METHOD  toolBarBreak( pToolbar )            INLINE  Qt_QMainWindow_toolBarBreak( ::pPtr, pToolbar )
   METHOD  toolButtonStyle()                   INLINE  Qt_QMainWindow_toolButtonStyle( ::pPtr )
   METHOD  unifiedTitleAndToolBarOnMac()       INLINE  Qt_QMainWindow_unifiedTitleAndToolBarOnMac( ::pPtr )
   METHOD  setAnimated( lEnabled )             INLINE  Qt_QMainWindow_setAnimated( ::pPtr, lEnabled )
   METHOD  setDockNestingEnabled( lEnabled )   INLINE  Qt_QMainWindow_setDockNestingEnabled( ::pPtr, lEnabled )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QMainWindow

   ::pParent := pParent

   ::pPtr := Qt_QMainWindow( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QMainWindow

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

