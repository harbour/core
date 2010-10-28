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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QMainWindow( ... )
   RETURN HB_QMainWindow():new( ... )

FUNCTION QMainWindowFromPointer( ... )
   RETURN HB_QMainWindow():fromPointer( ... )


CREATE CLASS QMainWindow INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMainWindow

   METHOD  new( ... )

   METHOD  addDockWidget                 // ( nArea, oQDockWidget )                            -> NIL
                                         // ( nArea, oQDockWidget, nOrientation )              -> NIL
   METHOD  addToolBar                    // ( nArea, oQToolBar )                               -> NIL
                                         // ( oQToolBar )                                      -> NIL
                                         // ( cTitle )                                         -> oQToolBar
   METHOD  addToolBarBreak               // ( nArea )                                          -> NIL
   METHOD  centralWidget                 // (  )                                               -> oQWidget
   METHOD  corner                        // ( nCorner )                                        -> nQt_DockWidgetArea
   METHOD  createPopupMenu               // (  )                                               -> oQMenu
   METHOD  dockOptions                   // (  )                                               -> nDockOptions
   METHOD  dockWidgetArea                // ( oQDockWidget )                                   -> nQt_DockWidgetArea
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertToolBar                 // ( oQToolBar, oQToolBar )                           -> NIL
   METHOD  insertToolBarBreak            // ( oQToolBar )                                      -> NIL
   METHOD  isAnimated                    // (  )                                               -> lBool
   METHOD  isDockNestingEnabled          // (  )                                               -> lBool
   METHOD  menuBar                       // (  )                                               -> oQMenuBar
   METHOD  menuWidget                    // (  )                                               -> oQWidget
   METHOD  removeDockWidget              // ( oQDockWidget )                                   -> NIL
   METHOD  removeToolBar                 // ( oQToolBar )                                      -> NIL
   METHOD  removeToolBarBreak            // ( oQToolBar )                                      -> NIL
   METHOD  restoreDockWidget             // ( oQDockWidget )                                   -> lBool
   METHOD  restoreState                  // ( oQByteArray, nVersion )                          -> lBool
   METHOD  saveState                     // ( nVersion )                                       -> oQByteArray
   METHOD  setCentralWidget              // ( oQWidget )                                       -> NIL
   METHOD  setCorner                     // ( nCorner, nArea )                                 -> NIL
   METHOD  setDockOptions                // ( nOptions )                                       -> NIL
   METHOD  setDocumentMode               // ( lEnabled )                                       -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setMenuBar                    // ( oQMenuBar )                                      -> NIL
   METHOD  setMenuWidget                 // ( oQWidget )                                       -> NIL
   METHOD  setStatusBar                  // ( oQStatusBar )                                    -> NIL
   METHOD  setTabPosition                // ( nAreas, nTabPosition )                           -> NIL
   METHOD  setTabShape                   // ( nTabShape )                                      -> NIL
   METHOD  setToolButtonStyle            // ( nToolButtonStyle )                               -> NIL
   METHOD  setUnifiedTitleAndToolBarOnMac // ( lSet )                                           -> NIL
   METHOD  splitDockWidget               // ( oQDockWidget, oQDockWidget, nOrientation )       -> NIL
   METHOD  statusBar                     // (  )                                               -> oQStatusBar
   METHOD  tabPosition                   // ( nArea )                                          -> nQTabWidget_TabPosition
   METHOD  tabShape                      // (  )                                               -> nQTabWidget_TabShape
   METHOD  tabifiedDockWidgets           // ( oQDockWidget )                                   -> oQList_QDockWidget
   METHOD  tabifyDockWidget              // ( oQDockWidget, oQDockWidget )                     -> NIL
   METHOD  toolBarArea                   // ( oQToolBar )                                      -> nQt_ToolBarArea
   METHOD  toolBarBreak                  // ( oQToolBar )                                      -> lBool
   METHOD  toolButtonStyle               // (  )                                               -> nQt_ToolButtonStyle
   METHOD  unifiedTitleAndToolBarOnMac   // (  )                                               -> lBool
   METHOD  setAnimated                   // ( lEnabled )                                       -> NIL
   METHOD  setDockNestingEnabled         // ( lEnabled )                                       -> NIL

   ENDCLASS


METHOD QMainWindow:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMainWindow( ... )
   RETURN Self


METHOD QMainWindow:addDockWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QMainWindow_addDockWidget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_addDockWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:addToolBar( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_addToolBar( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QToolBarFromPointer( Qt_QMainWindow_addToolBar_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_addToolBar_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:addToolBarBreak( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_addToolBarBreak( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QMainWindow_addToolBarBreak( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:centralWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QMainWindow_centralWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:corner( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_corner( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:createPopupMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QMainWindow_createPopupMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:dockOptions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_dockOptions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:dockWidgetArea( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_dockWidgetArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QMainWindow_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:insertToolBar( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_insertToolBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:insertToolBarBreak( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_insertToolBarBreak( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:isAnimated( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_isAnimated( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:isDockNestingEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_isDockNestingEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:menuBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuBarFromPointer( Qt_QMainWindow_menuBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:menuWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QMainWindow_menuWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:removeDockWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_removeDockWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:removeToolBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_removeToolBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:removeToolBarBreak( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_removeToolBarBreak( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:restoreDockWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_restoreDockWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:restoreState( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:saveState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QMainWindow_saveState( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QMainWindow_saveState( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setCentralWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setCentralWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setCorner( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_setCorner( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setDockOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setDockOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setMenuBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setMenuBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setMenuWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setMenuWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setStatusBar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setStatusBar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setTabPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_setTabPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setTabShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setTabShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setToolButtonStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setToolButtonStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setUnifiedTitleAndToolBarOnMac( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setUnifiedTitleAndToolBarOnMac( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:splitDockWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QMainWindow_splitDockWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:statusBar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStatusBarFromPointer( Qt_QMainWindow_statusBar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:tabPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_tabPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:tabShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_tabShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:tabifiedDockWidgets( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QMainWindow_tabifiedDockWidgets( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:tabifyDockWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMainWindow_tabifyDockWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:toolBarArea( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_toolBarArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:toolBarBreak( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_toolBarBreak( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:toolButtonStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_toolButtonStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:unifiedTitleAndToolBarOnMac( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMainWindow_unifiedTitleAndToolBarOnMac( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setAnimated( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setAnimated( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMainWindow:setDockNestingEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMainWindow_setDockNestingEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

