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


FUNCTION QMdiSubWindow( ... )
   RETURN HB_QMdiSubWindow():new( ... )

FUNCTION QMdiSubWindowFromPointer( ... )
   RETURN HB_QMdiSubWindow():fromPointer( ... )


CREATE CLASS QMdiSubWindow INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMdiSubWindow

   METHOD  new( ... )

   METHOD  isShaded                      // (  )                                               -> lBool
   METHOD  keyboardPageStep              // (  )                                               -> nInt
   METHOD  keyboardSingleStep            // (  )                                               -> nInt
   METHOD  mdiArea                       // (  )                                               -> oQMdiArea
   METHOD  setKeyboardPageStep           // ( nStep )                                          -> NIL
   METHOD  setKeyboardSingleStep         // ( nStep )                                          -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setSystemMenu                 // ( oQMenu )                                         -> NIL
   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  systemMenu                    // (  )                                               -> oQMenu
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  widget                        // (  )                                               -> oQWidget
   METHOD  showShaded                    // (  )                                               -> NIL
   METHOD  showSystemMenu                // (  )                                               -> NIL

   ENDCLASS


METHOD QMdiSubWindow:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMdiSubWindow( ... )
   RETURN Self


METHOD QMdiSubWindow:isShaded( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiSubWindow_isShaded( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:keyboardPageStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiSubWindow_keyboardPageStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:keyboardSingleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiSubWindow_keyboardSingleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:mdiArea( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMdiAreaFromPointer( Qt_QMdiSubWindow_mdiArea( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:setKeyboardPageStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_setKeyboardPageStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:setKeyboardSingleStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_setKeyboardSingleStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QMdiSubWindow_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:setSystemMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_setSystemMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:systemMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QMdiSubWindow_systemMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMdiSubWindow_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QMdiSubWindow_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:showShaded( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiSubWindow_showShaded( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMdiSubWindow:showSystemMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMdiSubWindow_showSystemMenu( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

