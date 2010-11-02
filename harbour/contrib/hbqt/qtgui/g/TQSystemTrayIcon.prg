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


FUNCTION QSystemTrayIcon( ... )
   RETURN HB_QSystemTrayIcon():new( ... )

FUNCTION QSystemTrayIconFromPointer( ... )
   RETURN HB_QSystemTrayIcon():fromPointer( ... )


CREATE CLASS QSystemTrayIcon INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSystemTrayIcon

   METHOD  new( ... )

   METHOD  contextMenu                   // (  )                                               -> oQMenu
   METHOD  geometry                      // (  )                                               -> oQRect
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isVisible                     // (  )                                               -> lBool
   METHOD  setContextMenu                // ( oQMenu )                                         -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setToolTip                    // ( cTip )                                           -> NIL
   METHOD  showMessage                   // ( cTitle, cMessage, nIcon, nMillisecondsTimeoutHint ) -> NIL
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  isSystemTrayAvailable         // (  )                                               -> lBool
   METHOD  supportsMessages              // (  )                                               -> lBool
   METHOD  hide                          // (  )                                               -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  show                          // (  )                                               -> NIL

   ENDCLASS


METHOD QSystemTrayIcon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSystemTrayIcon( ... )
   RETURN Self


METHOD QSystemTrayIcon:contextMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QSystemTrayIcon_contextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QSystemTrayIcon_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QSystemTrayIcon_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:isVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_isVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:setContextMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSystemTrayIcon_setContextMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QSystemTrayIcon_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSystemTrayIcon_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:showMessage( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QSystemTrayIcon_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSystemTrayIcon_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QSystemTrayIcon_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:isSystemTrayAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_isSystemTrayAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:supportsMessages( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_supportsMessages( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:hide( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_hide( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSystemTrayIcon_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSystemTrayIcon:show( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSystemTrayIcon_show( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

