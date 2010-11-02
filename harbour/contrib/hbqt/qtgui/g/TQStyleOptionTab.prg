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


FUNCTION QStyleOptionTab( ... )
   RETURN HB_QStyleOptionTab():new( ... )

FUNCTION QStyleOptionTabFromPointer( ... )
   RETURN HB_QStyleOptionTab():fromPointer( ... )


CREATE CLASS QStyleOptionTab INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionTab

   METHOD  new( ... )

   METHOD  cornerWidgets                 // (  )                                               -> nCornerWidgets
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  position                      // (  )                                               -> nTabPosition
   METHOD  row                           // (  )                                               -> nInt
   METHOD  selectedPosition              // (  )                                               -> nSelectedPosition
   METHOD  shape                         // (  )                                               -> nQTabBar_Shape
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QStyleOptionTab:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionTab( ... )
   RETURN Self


METHOD QStyleOptionTab:cornerWidgets( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_cornerWidgets( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionTab_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:row( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_row( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:selectedPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_selectedPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:shape( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_shape( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionTab:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionTab_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

