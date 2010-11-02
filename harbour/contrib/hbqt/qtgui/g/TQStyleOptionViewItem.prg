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


FUNCTION QStyleOptionViewItem( ... )
   RETURN HB_QStyleOptionViewItem():new( ... )

FUNCTION QStyleOptionViewItemFromPointer( ... )
   RETURN HB_QStyleOptionViewItem():fromPointer( ... )


CREATE CLASS QStyleOptionViewItem INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionViewItem

   METHOD  new( ... )

   METHOD  decorationAlignment           // (  )                                               -> nQt_Alignment
   METHOD  decorationPosition            // (  )                                               -> nPosition
   METHOD  decorationSize                // (  )                                               -> oQSize
   METHOD  displayAlignment              // (  )                                               -> nQt_Alignment
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  showDecorationSelected        // (  )                                               -> lBool
   METHOD  textElideMode                 // (  )                                               -> nQt_TextElideMode

   ENDCLASS


METHOD QStyleOptionViewItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionViewItem( ... )
   RETURN Self


METHOD QStyleOptionViewItem:decorationAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionViewItem_decorationAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:decorationPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionViewItem_decorationPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:decorationSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QStyleOptionViewItem_decorationSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:displayAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionViewItem_displayAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QStyleOptionViewItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:showDecorationSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionViewItem_showDecorationSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionViewItem:textElideMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionViewItem_textElideMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

