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


FUNCTION QStyleOptionToolButton( ... )
   RETURN HB_QStyleOptionToolButton():new( ... )

FUNCTION QStyleOptionToolButtonFromPointer( ... )
   RETURN HB_QStyleOptionToolButton():fromPointer( ... )


CREATE CLASS QStyleOptionToolButton INHERIT HbQtObjectHandler, HB_QStyleOptionComplex FUNCTION HB_QStyleOptionToolButton

   METHOD  new( ... )

   METHOD  arrowType                     // (  )                                               -> nQt_ArrowType
   METHOD  features                      // (  )                                               -> nToolButtonFeatures
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  text                          // (  )                                               -> cQString
   METHOD  toolButtonStyle               // (  )                                               -> nQt_ToolButtonStyle

   ENDCLASS


METHOD QStyleOptionToolButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionToolButton( ... )
   RETURN Self


METHOD QStyleOptionToolButton:arrowType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolButton_arrowType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolButton_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QStyleOptionToolButton_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionToolButton_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionToolButton_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QStyleOptionToolButton_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolButton_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionToolButton:toolButtonStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionToolButton_toolButtonStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

