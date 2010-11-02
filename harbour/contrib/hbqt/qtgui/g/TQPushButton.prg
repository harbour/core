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


FUNCTION QPushButton( ... )
   RETURN HB_QPushButton():new( ... )

FUNCTION QPushButtonFromPointer( ... )
   RETURN HB_QPushButton():fromPointer( ... )


CREATE CLASS QPushButton INHERIT HbQtObjectHandler, HB_QAbstractButton FUNCTION HB_QPushButton

   METHOD  new( ... )

   METHOD  autoDefault                   // (  )                                               -> lBool
   METHOD  isDefault                     // (  )                                               -> lBool
   METHOD  isFlat                        // (  )                                               -> lBool
   METHOD  menu                          // (  )                                               -> oQMenu
   METHOD  setAutoDefault                // ( lBool )                                          -> NIL
   METHOD  setDefault                    // ( lBool )                                          -> NIL
   METHOD  setFlat                       // ( lBool )                                          -> NIL
   METHOD  setMenu                       // ( oQMenu )                                         -> NIL
   METHOD  showMenu                      // (  )                                               -> NIL

   ENDCLASS


METHOD QPushButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPushButton( ... )
   RETURN Self


METHOD QPushButton:autoDefault( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPushButton_autoDefault( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:isDefault( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPushButton_isDefault( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:isFlat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPushButton_isFlat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:menu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QPushButton_menu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:setAutoDefault( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPushButton_setAutoDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:setDefault( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPushButton_setDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:setFlat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPushButton_setFlat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:setMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPushButton_setMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPushButton:showMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPushButton_showMenu( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

