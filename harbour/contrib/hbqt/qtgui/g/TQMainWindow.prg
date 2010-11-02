/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


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

