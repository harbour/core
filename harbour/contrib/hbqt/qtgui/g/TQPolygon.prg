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


FUNCTION QPolygon( ... )
   RETURN HB_QPolygon():new( ... )

FUNCTION QPolygonFromPointer( ... )
   RETURN HB_QPolygon():fromPointer( ... )


CREATE CLASS QPolygon INHERIT HbQtObjectHandler FUNCTION HB_QPolygon

   METHOD  new( ... )

   METHOD  boundingRect                  // (  )                                               -> oQRect
   METHOD  containsPoint                 // ( oQPoint, nFillRule )                             -> lBool
   METHOD  intersected                   // ( oQPolygon )                                      -> oQPolygon
   METHOD  point                         // ( nIndex, @nX, @nY )                               -> NIL
                                         // ( nIndex )                                         -> oQPoint
   METHOD  putPoints                     // ( nIndex, nNPoints, oQPolygon, nFromIndex )        -> NIL
   METHOD  setPoint                      // ( nIndex, nX, nY )                                 -> NIL
                                         // ( nIndex, oQPoint )                                -> NIL
   METHOD  setPoints                     // ( nNPoints, @nPoints )                             -> NIL
   METHOD  subtracted                    // ( oQPolygon )                                      -> oQPolygon
   METHOD  translate                     // ( nDx, nDy )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
   METHOD  united                        // ( oQPolygon )                                      -> oQPolygon

   ENDCLASS


METHOD QPolygon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPolygon( ... )
   RETURN Self


METHOD QPolygon:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPolygon_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:containsPoint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPolygon_containsPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QPolygon_intersected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:point( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPolygon_point( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QPolygon_point_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:putPoints( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QPolygon_putPoints( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPolygon_putPoints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:setPoint( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPolygon_setPoint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPolygon_setPoint_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:setPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPolygon_setPoints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:subtracted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QPolygon_subtracted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPolygon_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPolygon_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPolygon:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QPolygon_united( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

