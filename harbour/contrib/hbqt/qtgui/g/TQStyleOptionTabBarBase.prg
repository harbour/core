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


FUNCTION QStyleOptionTabBarBase( ... )
   RETURN HB_QStyleOptionTabBarBase():new( ... )

FUNCTION QStyleOptionTabBarBaseFromPointer( ... )
   RETURN HB_QStyleOptionTabBarBase():fromPointer( ... )


CREATE CLASS QStyleOptionTabBarBase INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionTabBarBase

   METHOD  new( ... )

   METHOD  selectedTabRect               // (  )                                               -> oQRect
   METHOD  shape                         // (  )                                               -> nQTabBar_Shape
   METHOD  tabBarRect                    // (  )                                               -> oQRect

   ENDCLASS


METHOD QStyleOptionTabBarBase:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionTabBarBase( ... )
   RETURN Self


METHOD QStyleOptionTabBarBase:selectedTabRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QStyleOptionTabBarBase_selectedTabRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabBarBase:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTabBarBase_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTabBarBase:tabBarRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QStyleOptionTabBarBase_tabBarRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

