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


FUNCTION QDesktopWidget( ... )
   RETURN HB_QDesktopWidget():new( ... )

FUNCTION QDesktopWidgetFromPointer( ... )
   RETURN HB_QDesktopWidget():fromPointer( ... )


CREATE CLASS QDesktopWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesktopWidget

   METHOD  new( ... )

   METHOD  availableGeometry             // ( nScreen )                                        -> oQRect
                                         // ( oQWidget )                                       -> oQRect
                                         // ( oQPoint )                                        -> oQRect
   METHOD  isVirtualDesktop              // (  )                                               -> lBool
   METHOD  numScreens                    // (  )                                               -> nInt
   METHOD  primaryScreen                 // (  )                                               -> nInt
   METHOD  screen                        // ( nScreen )                                        -> oQWidget
   METHOD  screenGeometry                // ( nScreen )                                        -> oQRect
                                         // ( oQWidget )                                       -> oQRect
                                         // ( oQPoint )                                        -> oQRect
   METHOD  screenNumber                  // ( oQWidget )                                       -> nInt
                                         // ( oQPoint )                                        -> nInt

   ENDCLASS


METHOD QDesktopWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesktopWidget( ... )
   RETURN Self


METHOD QDesktopWidget:availableGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QDesktopWidget_availableGeometry( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN QRectFromPointer( Qt_QDesktopWidget_availableGeometry_1( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QRectFromPointer( Qt_QDesktopWidget_availableGeometry_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QDesktopWidget_availableGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:isVirtualDesktop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesktopWidget_isVirtualDesktop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:numScreens( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesktopWidget_numScreens( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:primaryScreen( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesktopWidget_primaryScreen( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:screen( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QDesktopWidget_screen( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDesktopWidget_screen( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:screenGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QDesktopWidget_screenGeometry( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN QRectFromPointer( Qt_QDesktopWidget_screenGeometry_1( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QRectFromPointer( Qt_QDesktopWidget_screenGeometry_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QDesktopWidget_screenGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesktopWidget:screenNumber( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN Qt_QDesktopWidget_screenNumber( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QDesktopWidget_screenNumber_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDesktopWidget_screenNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

