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


FUNCTION QAbstractButton( ... )
   RETURN HB_QAbstractButton():new( ... )

FUNCTION QAbstractButtonFromPointer( ... )
   RETURN HB_QAbstractButton():fromPointer( ... )


CREATE CLASS QAbstractButton INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractButton

   METHOD  new( ... )

   METHOD  autoExclusive                 // (  )                                               -> lBool
   METHOD  autoRepeat                    // (  )                                               -> lBool
   METHOD  autoRepeatDelay               // (  )                                               -> nInt
   METHOD  autoRepeatInterval            // (  )                                               -> nInt
   METHOD  group                         // (  )                                               -> oQButtonGroup
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  isCheckable                   // (  )                                               -> lBool
   METHOD  isChecked                     // (  )                                               -> lBool
   METHOD  isDown                        // (  )                                               -> lBool
   METHOD  setAutoExclusive              // ( lBool )                                          -> NIL
   METHOD  setAutoRepeat                 // ( lBool )                                          -> NIL
   METHOD  setAutoRepeatDelay            // ( nInt )                                           -> NIL
   METHOD  setAutoRepeatInterval         // ( nInt )                                           -> NIL
   METHOD  setCheckable                  // ( lBool )                                          -> NIL
   METHOD  setDown                       // ( lBool )                                          -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setShortcut                   // ( oQKeySequence )                                  -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  shortcut                      // (  )                                               -> oQKeySequence
   METHOD  text                          // (  )                                               -> cQString
   METHOD  animateClick                  // ( nMsec )                                          -> NIL
   METHOD  click                         // (  )                                               -> NIL
   METHOD  setChecked                    // ( lBool )                                          -> NIL
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  toggle                        // (  )                                               -> NIL

   ENDCLASS


METHOD QAbstractButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractButton( ... )
   RETURN Self


METHOD QAbstractButton:autoExclusive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoExclusive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:autoRepeat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:autoRepeatDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeatDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:autoRepeatInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_autoRepeatInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:group( ... )
   SWITCH PCount()
   CASE 0
      RETURN QButtonGroupFromPointer( Qt_QAbstractButton_group( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QAbstractButton_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QAbstractButton_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:isCheckable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isCheckable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:isChecked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isChecked( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:isDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_isDown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setAutoExclusive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoExclusive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setAutoRepeat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setAutoRepeatDelay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeatDelay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setAutoRepeatInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setAutoRepeatInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setCheckable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setCheckable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setDown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setDown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QAbstractButton_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setShortcut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:shortcut( ... )
   SWITCH PCount()
   CASE 0
      RETURN QKeySequenceFromPointer( Qt_QAbstractButton_shortcut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:animateClick( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_animateClick( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAbstractButton_animateClick( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:click( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_click( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setChecked( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setChecked( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractButton_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractButton:toggle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractButton_toggle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

