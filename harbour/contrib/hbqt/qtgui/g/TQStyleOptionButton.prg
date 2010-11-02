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


FUNCTION QStyleOptionButton( ... )
   RETURN HB_QStyleOptionButton():new( ... )

FUNCTION QStyleOptionButtonFromPointer( ... )
   RETURN HB_QStyleOptionButton():fromPointer( ... )


CREATE CLASS QStyleOptionButton INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionButton

   METHOD  new( ... )

   METHOD  features                      // (  )                                               -> nButtonFeatures
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  text                          // (  )                                               -> cQString

   ENDCLASS


METHOD QStyleOptionButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionButton( ... )
   RETURN Self


METHOD QStyleOptionButton:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionButton_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionButton:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionButton_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionButton:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionButton_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionButton:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionButton_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

