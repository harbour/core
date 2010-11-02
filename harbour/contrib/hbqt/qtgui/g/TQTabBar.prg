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


FUNCTION QTabBar( ... )
   RETURN HB_QTabBar():new( ... )

FUNCTION QTabBarFromPointer( ... )
   RETURN HB_QTabBar():fromPointer( ... )


CREATE CLASS QTabBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QTabBar

   METHOD  new( ... )

   METHOD  addTab                        // ( cText )                                          -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentIndex                  // (  )                                               -> nInt
   METHOD  documentMode                  // (  )                                               -> lBool
   METHOD  drawBase                      // (  )                                               -> lBool
   METHOD  elideMode                     // (  )                                               -> nQt_TextElideMode
   METHOD  expanding                     // (  )                                               -> lBool
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertTab                     // ( nIndex, cText )                                  -> nInt
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isTabEnabled                  // ( nIndex )                                         -> lBool
   METHOD  moveTab                       // ( nFrom, nTo )                                     -> NIL
   METHOD  removeTab                     // ( nIndex )                                         -> NIL
   METHOD  selectionBehaviorOnRemove     // (  )                                               -> nSelectionBehavior
   METHOD  setDocumentMode               // ( lSet )                                           -> NIL
   METHOD  setDrawBase                   // ( lDrawTheBase )                                   -> NIL
   METHOD  setElideMode                  // ( nQt::TextElideMode )                             -> NIL
   METHOD  setExpanding                  // ( lEnabled )                                       -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setSelectionBehaviorOnRemove  // ( nBehavior )                                      -> NIL
   METHOD  setShape                      // ( nShape )                                         -> NIL
   METHOD  setTabButton                  // ( nIndex, nPosition, oQWidget )                    -> NIL
   METHOD  setTabData                    // ( nIndex, oQVariant )                              -> NIL
   METHOD  setTabEnabled                 // ( nIndex, lEnabled )                               -> NIL
   METHOD  setTabText                    // ( nIndex, cText )                                  -> NIL
   METHOD  setTabTextColor               // ( nIndex, oQColor )                                -> NIL
   METHOD  setTabToolTip                 // ( nIndex, cTip )                                   -> NIL
   METHOD  setTabWhatsThis               // ( nIndex, cText )                                  -> NIL
   METHOD  setTabsClosable               // ( lClosable )                                      -> NIL
   METHOD  setUsesScrollButtons          // ( lUseButtons )                                    -> NIL
   METHOD  shape                         // (  )                                               -> nShape
   METHOD  tabAt                         // ( oQPoint )                                        -> nInt
   METHOD  tabButton                     // ( nIndex, nPosition )                              -> oQWidget
   METHOD  tabData                       // ( nIndex )                                         -> oQVariant
   METHOD  tabIcon                       // ( nIndex )                                         -> oQIcon
   METHOD  tabRect                       // ( nIndex )                                         -> oQRect
   METHOD  tabText                       // ( nIndex )                                         -> cQString
   METHOD  tabTextColor                  // ( nIndex )                                         -> oQColor
   METHOD  tabToolTip                    // ( nIndex )                                         -> cQString
   METHOD  tabWhatsThis                  // ( nIndex )                                         -> cQString
   METHOD  tabsClosable                  // (  )                                               -> lBool
   METHOD  usesScrollButtons             // (  )                                               -> lBool
   METHOD  setCurrentIndex               // ( nIndex )                                         -> NIL

   ENDCLASS


METHOD QTabBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTabBar( ... )
   RETURN Self


METHOD QTabBar:addTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_addTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_currentIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:documentMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_documentMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:drawBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_drawBase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:elideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_elideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:expanding( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_expanding( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTabBar_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:insertTab( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_insertTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:isTabEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_isTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:moveTab( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_moveTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:removeTab( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_removeTab( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:selectionBehaviorOnRemove( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_selectionBehaviorOnRemove( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setDocumentMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setDocumentMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setDrawBase( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setDrawBase( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setElideMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setElideMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setExpanding( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setExpanding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setSelectionBehaviorOnRemove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setSelectionBehaviorOnRemove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setShape( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setShape( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabButton( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QTabBar_setTabButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabTextColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabTextColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabToolTip( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabWhatsThis( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTabBar_setTabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setTabsClosable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setTabsClosable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setUsesScrollButtons( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setUsesScrollButtons( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QTabBar_tabButton( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QTabBar_tabData( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QTabBar_tabIcon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QTabBar_tabRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabTextColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QTabBar_tabTextColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_tabWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:tabsClosable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_tabsClosable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:usesScrollButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTabBar_usesScrollButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTabBar:setCurrentIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTabBar_setCurrentIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

