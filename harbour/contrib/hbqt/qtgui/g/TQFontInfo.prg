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


FUNCTION QFontInfo( ... )
   RETURN HB_QFontInfo():new( ... )

FUNCTION QFontInfoFromPointer( ... )
   RETURN HB_QFontInfo():fromPointer( ... )


CREATE CLASS QFontInfo INHERIT HbQtObjectHandler FUNCTION HB_QFontInfo

   METHOD  new( ... )

   METHOD  bold                          // (  )                                               -> lBool
   METHOD  exactMatch                    // (  )                                               -> lBool
   METHOD  family                        // (  )                                               -> cQString
   METHOD  fixedPitch                    // (  )                                               -> lBool
   METHOD  italic                        // (  )                                               -> lBool
   METHOD  pixelSize                     // (  )                                               -> nInt
   METHOD  pointSize                     // (  )                                               -> nInt
   METHOD  pointSizeF                    // (  )                                               -> nQreal
   METHOD  rawMode                       // (  )                                               -> lBool
   METHOD  style                         // (  )                                               -> nQFont_Style
   METHOD  styleHint                     // (  )                                               -> nQFont_StyleHint
   METHOD  weight                        // (  )                                               -> nInt

   ENDCLASS


METHOD QFontInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontInfo( ... )
   RETURN Self


METHOD QFontInfo:bold( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_bold( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:exactMatch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_exactMatch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:family( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_family( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:fixedPitch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_fixedPitch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:italic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_italic( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:pixelSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_pixelSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:pointSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_pointSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:pointSizeF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_pointSizeF( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:rawMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_rawMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:styleHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_styleHint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontInfo:weight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontInfo_weight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

