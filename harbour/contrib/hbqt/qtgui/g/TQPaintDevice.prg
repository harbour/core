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


FUNCTION QPaintDevice( ... )
   RETURN HB_QPaintDevice():new( ... )

FUNCTION QPaintDeviceFromPointer( ... )
   RETURN HB_QPaintDevice():fromPointer( ... )


CREATE CLASS QPaintDevice INHERIT HbQtObjectHandler FUNCTION HB_QPaintDevice

   METHOD  new( ... )

   METHOD  depth                         // (  )                                               -> nInt
   METHOD  height                        // (  )                                               -> nInt
   METHOD  heightMM                      // (  )                                               -> nInt
   METHOD  logicalDpiX                   // (  )                                               -> nInt
   METHOD  logicalDpiY                   // (  )                                               -> nInt
   METHOD  numColors                     // (  )                                               -> nInt
   METHOD  paintEngine                   // (  )                                               -> oQPaintEngine
   METHOD  paintingActive                // (  )                                               -> lBool
   METHOD  physicalDpiX                  // (  )                                               -> nInt
   METHOD  physicalDpiY                  // (  )                                               -> nInt
   METHOD  width                         // (  )                                               -> nInt
   METHOD  widthMM                       // (  )                                               -> nInt

   ENDCLASS


METHOD QPaintDevice:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPaintDevice( ... )
   RETURN Self


METHOD QPaintDevice:depth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_depth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:heightMM( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_heightMM( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:logicalDpiX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_logicalDpiX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:logicalDpiY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_logicalDpiY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:numColors( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_numColors( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:paintEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintEngineFromPointer( Qt_QPaintDevice_paintEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:paintingActive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_paintingActive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:physicalDpiX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_physicalDpiX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:physicalDpiY( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_physicalDpiY( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPaintDevice:widthMM( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPaintDevice_widthMM( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

