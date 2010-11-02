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


FUNCTION QPolygonF( ... )
   RETURN HB_QPolygonF():new( ... )

FUNCTION QPolygonFFromPointer( ... )
   RETURN HB_QPolygonF():fromPointer( ... )


CREATE CLASS QPolygonF INHERIT HbQtObjectHandler FUNCTION HB_QPolygonF

   METHOD  new( ... )

   METHOD  boundingRect                  // (  )                                               -> oQRectF
   METHOD  containsPoint                 // ( oQPointF, nFillRule )                            -> lBool
   METHOD  intersected                   // ( oQPolygonF )                                     -> oQPolygonF
   METHOD  isClosed                      // (  )                                               -> lBool
   METHOD  subtracted                    // ( oQPolygonF )                                     -> oQPolygonF
   METHOD  toPolygon                     // (  )                                               -> oQPolygon
   METHOD  translate                     // ( oQPointF )                                       -> NIL
                                         // ( nDx, nDy )                                       -> NIL
   METHOD  united                        // ( oQPolygonF )                                     -> oQPolygonF

   ENDCLASS


METHOD QPolygonF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPolygonF( ... )
   RETURN Self


METHOD QPolygonF:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QPolygonF_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:containsPoint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPolygonF_containsPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFFromPointer( Qt_QPolygonF_intersected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:isClosed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPolygonF_isClosed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:subtracted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFFromPointer( Qt_QPolygonF_subtracted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:toPolygon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPolygonFromPointer( Qt_QPolygonF_toPolygon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPolygonF_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPolygonF_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygonF:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFFromPointer( Qt_QPolygonF_united( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

