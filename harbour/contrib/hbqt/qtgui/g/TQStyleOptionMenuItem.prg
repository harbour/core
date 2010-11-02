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


FUNCTION QStyleOptionMenuItem( ... )
   RETURN HB_QStyleOptionMenuItem():new( ... )

FUNCTION QStyleOptionMenuItemFromPointer( ... )
   RETURN HB_QStyleOptionMenuItem():fromPointer( ... )


CREATE CLASS QStyleOptionMenuItem INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionMenuItem

   METHOD  new( ... )

   METHOD  checkType                     // (  )                                               -> nCheckType
   METHOD  checked                       // (  )                                               -> lBool
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  maxIconWidth                  // (  )                                               -> nInt
   METHOD  menuHasCheckableItems         // (  )                                               -> lBool
   METHOD  menuItemType                  // (  )                                               -> nMenuItemType
   METHOD  menuRect                      // (  )                                               -> oQRect
   METHOD  tabWidth                      // (  )                                               -> nInt
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QStyleOptionMenuItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionMenuItem( ... )
   RETURN Self


METHOD QStyleOptionMenuItem:checkType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_checkType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:checked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_checked( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QStyleOptionMenuItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionMenuItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:maxIconWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_maxIconWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:menuHasCheckableItems( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_menuHasCheckableItems( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:menuItemType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_menuItemType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:menuRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QStyleOptionMenuItem_menuRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:tabWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_tabWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionMenuItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionMenuItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

