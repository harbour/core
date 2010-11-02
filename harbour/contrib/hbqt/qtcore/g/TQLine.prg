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


REQUEST __HBQTCORE


FUNCTION QLine( ... )
   RETURN HB_QLine():new( ... )

FUNCTION QLineFromPointer( ... )
   RETURN HB_QLine():fromPointer( ... )


CREATE CLASS QLine INHERIT HbQtObjectHandler FUNCTION HB_QLine

   METHOD  new( ... )

   METHOD  p1                            // (  )                                               -> oQPoint
   METHOD  p2                            // (  )                                               -> oQPoint
   METHOD  x1                            // (  )                                               -> nInt
   METHOD  x2                            // (  )                                               -> nInt
   METHOD  y1                            // (  )                                               -> nInt
   METHOD  y2                            // (  )                                               -> nInt
   METHOD  dx                            // (  )                                               -> nInt
   METHOD  dy                            // (  )                                               -> nInt
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  setP1                         // ( oQPoint )                                        -> NIL
   METHOD  setP2                         // ( oQPoint )                                        -> NIL
   METHOD  setLine                       // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  setPoints                     // ( oQPoint, oQPoint )                               -> NIL
   METHOD  translate                     // ( oQPoint )                                        -> NIL
                                         // ( nDx, nDy )                                       -> NIL
   METHOD  translated                    // ( oQPoint )                                        -> oQLine
                                         // ( nDx, nDy )                                       -> oQLine

   ENDCLASS


METHOD QLine:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLine( ... )
   RETURN Self


METHOD QLine:p1( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QLine_p1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:p2( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QLine_p2( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:x1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_x1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:x2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_x2( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:y1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_y1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:y2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_y2( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:dx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_dx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:dy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_dy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLine_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:setP1( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLine_setP1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:setP2( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLine_setP2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:setLine( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLine_setLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:setPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QLine_setPoints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLine_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLine_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLine:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QLineFromPointer( Qt_QLine_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QLineFromPointer( Qt_QLine_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

