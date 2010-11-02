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


FUNCTION QToolButton( ... )
   RETURN HB_QToolButton():new( ... )

FUNCTION QToolButtonFromPointer( ... )
   RETURN HB_QToolButton():fromPointer( ... )


CREATE CLASS QToolButton INHERIT HbQtObjectHandler, HB_QAbstractButton FUNCTION HB_QToolButton

   METHOD  new( ... )

   METHOD  arrowType                     // (  )                                               -> nQt_ArrowType
   METHOD  autoRaise                     // (  )                                               -> lBool
   METHOD  defaultAction                 // (  )                                               -> oQAction
   METHOD  menu                          // (  )                                               -> oQMenu
   METHOD  popupMode                     // (  )                                               -> nToolButtonPopupMode
   METHOD  setArrowType                  // ( nType )                                          -> NIL
   METHOD  setAutoRaise                  // ( lEnable )                                        -> NIL
   METHOD  setMenu                       // ( oQMenu )                                         -> NIL
   METHOD  setPopupMode                  // ( nMode )                                          -> NIL
   METHOD  toolButtonStyle               // (  )                                               -> nQt_ToolButtonStyle
   METHOD  setDefaultAction              // ( oQAction )                                       -> NIL
   METHOD  setToolButtonStyle            // ( nStyle )                                         -> NIL
   METHOD  showMenu                      // (  )                                               -> NIL

   ENDCLASS


METHOD QToolButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolButton( ... )
   RETURN Self


METHOD QToolButton:arrowType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolButton_arrowType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:autoRaise( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolButton_autoRaise( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:defaultAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QToolButton_defaultAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:menu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QToolButton_menu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:popupMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolButton_popupMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setArrowType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setArrowType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setAutoRaise( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setAutoRaise( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setPopupMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setPopupMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:toolButtonStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolButton_toolButtonStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setDefaultAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setDefaultAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:setToolButtonStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolButton_setToolButtonStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolButton:showMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolButton_showMenu( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

