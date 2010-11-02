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


FUNCTION QStyleOptionTabWidgetFrame( ... )
   RETURN HB_QStyleOptionTabWidgetFrame():new( ... )

FUNCTION QStyleOptionTabWidgetFrameFromPointer( ... )
   RETURN HB_QStyleOptionTabWidgetFrame():fromPointer( ... )


CREATE CLASS QStyleOptionTabWidgetFrame INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionTabWidgetFrame

   METHOD  new( ... )

   METHOD  leftCornerWidgetSize          // (  )                                               -> oQSize
   METHOD  lineWidth                     // (  )                                               -> nInt
   METHOD  midLineWidth                  // (  )                                               -> nInt
   METHOD  rightCornerWidgetSize         // (  )                                               -> oQSize
   METHOD  shape                         // (  )                                               -> nQTabBar_Shape
   METHOD  tabBarSize                    // (  )                                               -> oQSize

   ENDCLASS


METHOD QStyleOptionTabWidgetFrame:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionTabWidgetFrame( ... )
   RETURN Self


METHOD QStyleOptionTabWidgetFrame:leftCornerWidgetSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionTabWidgetFrame_leftCornerWidgetSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabWidgetFrame:lineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTabWidgetFrame_lineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabWidgetFrame:midLineWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTabWidgetFrame_midLineWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabWidgetFrame:rightCornerWidgetSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionTabWidgetFrame_rightCornerWidgetSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabWidgetFrame:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTabWidgetFrame_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabWidgetFrame:tabBarSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionTabWidgetFrame_tabBarSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

