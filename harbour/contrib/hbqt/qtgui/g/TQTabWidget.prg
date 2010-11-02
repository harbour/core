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


FUNCTION QTabWidget( ... )
   RETURN HB_QTabWidget():new( ... )

FUNCTION QTabWidgetFromPointer( ... )
   RETURN HB_QTabWidget():fromPointer( ... )


CREATE CLASS QTabWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabWidget

   METHOD  new( ... )

   METHOD  addTab                        // ( oQWidget, cLabel )                               -> nInt
                                         // ( oQWidget, coQIcon, cLabel )                      -> nInt
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  cornerWidget                  // ( nCorner )                                        -> oQWidget
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  currentWidget                 // (  )                                               -> oQWidget
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  elideMode                     // (  )                                               -> nQt_TextElideMode
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  insertTab                     // ( nIndex, oQWidget, cLabel )                       -> nInt
                                         // ( nIndex, oQWidget, coQIcon, cLabel )              -> nInt
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isTabEnabled                  // ( nIndex )                                         -> lBool
   METHOD  removeTab                     // ( nIndex )                                         -> NIL
   METHOD  setCornerWidget               // ( oQWidget, nCorner )                              -> NIL
   METHOD  setDocumentMode               // ( lSet )                                           -> NIL
   METHOD  setElideMode                  // ( nQt::TextElideMode )                             -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setTabEnabled                 // ( nIndex, lEnable )                                -> NIL
   METHOD  setTabIcon                    // ( nIndex, coQIcon )                                -> NIL
   METHOD  setTabPosition                // ( nTabPosition )                                   -> NIL
   METHOD  setTabShape                   // ( nS )                                             -> NIL
   METHOD  setTabText                    // ( nIndex, cLabel )                                 -> NIL
   METHOD  setTabToolTip                 // ( nIndex, cTip )                                   -> NIL
   METHOD  setTabWhatsThis               // ( nIndex, cText )                                  -> NIL
   METHOD  setTabsClosable               // ( lCloseable )                                     -> NIL
   METHOD  setUsesScrollButtons          // ( lUseButtons )                                    -> NIL
   METHOD  tabIcon                       // ( nIndex )                                         -> oQIcon
   METHOD  tabPosition                   // (  )                                               -> nTabPosition
   METHOD  tabShape                      // (  )                                               -> nTabShape
   METHOD  tabText                       // ( nIndex )                                         -> cQString
   METHOD  tabToolTip                    // ( nIndex )                                         -> cQString
   METHOD  tabWhatsThis                  // ( nIndex )                                         -> cQString
   METHOD  tabsClosable                  // (  )                                               -> lBool
   METHOD  usesScrollButtons             // (  )                                               -> lBool
   METHOD  widget                        // ( nIndex )                                         -> oQWidget
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL
   METHOD  setCurrentWidget              // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QTabWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabWidget( ... )
   RETURN Self


METHOD QTabWidget:addTab( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QTabWidget_addTab_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_addTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:cornerWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QTabWidget_cornerWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QWidgetFromPointer( Qt_QTabWidget_cornerWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:currentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QTabWidget_currentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:elideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_elideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTabWidget_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:insertTab( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. ( hb_isObject( hb_pvalue( 3 ) ) .OR. hb_isChar( hb_pvalue( 3 ) ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN Qt_QTabWidget_insertTab_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QTabWidget_insertTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:isTabEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_isTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:removeTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_removeTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCornerWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setCornerWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCornerWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setElideMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setElideMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabIcon( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. ( hb_isObject( hb_pvalue( 2 ) ) .OR. hb_isChar( hb_pvalue( 2 ) ) )
         RETURN Qt_QTabWidget_setTabIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabWidget_setTabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setTabsClosable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setTabsClosable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setUsesScrollButtons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setUsesScrollButtons( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTabWidget_tabIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabShape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabShape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_tabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:tabsClosable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_tabsClosable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:usesScrollButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabWidget_usesScrollButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QTabWidget_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabWidget:setCurrentWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabWidget_setCurrentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

