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


FUNCTION QStyleOptionHeader( ... )
   RETURN HB_QStyleOptionHeader():new( ... )

FUNCTION QStyleOptionHeaderFromPointer( ... )
   RETURN HB_QStyleOptionHeader():fromPointer( ... )


CREATE CLASS QStyleOptionHeader INHERIT HbQtObjectHandler, HB_QStyleOption FUNCTION HB_QStyleOptionHeader

   METHOD  new( ... )

   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconAlignment                 // (  )                                               -> nQt_Alignment
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  position                      // (  )                                               -> nSectionPosition
   METHOD  section                       // (  )                                               -> nInt
   METHOD  selectedPosition              // (  )                                               -> nSelectedPosition
   METHOD  sortIndicator                 // (  )                                               -> nSortIndicator
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nQt_Alignment

   ENDCLASS


METHOD QStyleOptionHeader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleOptionHeader( ... )
   RETURN Self


METHOD QStyleOptionHeader:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QStyleOptionHeader_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:iconAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_iconAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:section( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_section( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:selectedPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_selectedPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:sortIndicator( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_sortIndicator( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleOptionHeader:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStyleOptionHeader_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

