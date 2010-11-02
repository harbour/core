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


FUNCTION QStyleOptionComboBox( ... )
   RETURN HB_QStyleOptionComboBox():new( ... )

FUNCTION QStyleOptionComboBoxFromPointer( ... )
   RETURN HB_QStyleOptionComboBox():fromPointer( ... )


CREATE CLASS QStyleOptionComboBox INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionComboBox

   METHOD  new( ... )

   METHOD  currentIcon                   // (  )                                               -> oQIcon
   METHOD  currentText                   // (  )                                               -> cQString
   METHOD  editable                      // (  )                                               -> lBool
   METHOD  frame                         // (  )                                               -> lBool
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  popupRect                     // (  )                                               -> oQRect

   ENDCLASS


METHOD QStyleOptionComboBox:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionComboBox( ... )
   RETURN Self


METHOD QStyleOptionComboBox:currentIcon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionComboBox_currentIcon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComboBox:currentText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionComboBox_currentText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComboBox:editable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionComboBox_editable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComboBox:frame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionComboBox_frame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComboBox:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionComboBox_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionComboBox:popupRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QStyleOptionComboBox_popupRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

