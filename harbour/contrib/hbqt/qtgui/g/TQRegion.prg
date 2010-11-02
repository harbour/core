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


FUNCTION QRegion( ... )
   RETURN HB_QRegion():new( ... )

FUNCTION QRegionFromPointer( ... )
   RETURN HB_QRegion():fromPointer( ... )


CREATE CLASS QRegion INHERIT HbQtObjectHandler FUNCTION HB_QRegion

   METHOD  new( ... )

   METHOD  boundingRect                  // (  )                                               -> oQRect
   METHOD  contains                      // ( oQPoint )                                        -> lBool
                                         // ( oQRect )                                         -> lBool
   METHOD  intersected                   // ( oQRegion )                                       -> oQRegion
                                         // ( oQRect )                                         -> oQRegion
   METHOD  intersects                    // ( oQRegion )                                       -> lBool
                                         // ( oQRect )                                         -> lBool
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  numRects                      // (  )                                               -> nInt
   METHOD  setRects                      // ( oQRect, nNumber )                                -> NIL
   METHOD  subtracted                    // ( oQRegion )                                       -> oQRegion
   METHOD  translate                     // ( nDx, nDy )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
   METHOD  translated                    // ( nDx, nDy )                                       -> oQRegion
                                         // ( oQPoint )                                        -> oQRegion
   METHOD  united                        // ( oQRegion )                                       -> oQRegion
                                         // ( oQRect )                                         -> oQRegion
   METHOD  xored                         // ( oQRegion )                                       -> oQRegion

   ENDCLASS


METHOD QRegion:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegion( ... )
   RETURN Self


METHOD QRegion:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QRegion_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINT"
            RETURN Qt_QRegion_contains( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QRegion_contains_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGION"
            RETURN QRegionFromPointer( Qt_QRegion_intersected( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRegionFromPointer( Qt_QRegion_intersected_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:intersects( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGION"
            RETURN Qt_QRegion_intersects( ::pPtr, ... )
         CASE "QRECT"
            RETURN Qt_QRegion_intersects_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegion_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:numRects( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRegion_numRects( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:setRects( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRegion_setRects( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:subtracted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRegionFromPointer( Qt_QRegion_subtracted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRegion_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRegion_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QRegionFromPointer( Qt_QRegion_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRegionFromPointer( Qt_QRegion_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGION"
            RETURN QRegionFromPointer( Qt_QRegion_united( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRegionFromPointer( Qt_QRegion_united_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRegion:xored( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRegionFromPointer( Qt_QRegion_xored( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

