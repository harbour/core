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


FUNCTION QSizeF( ... )
   RETURN HB_QSizeF():new( ... )

FUNCTION QSizeFFromPointer( ... )
   RETURN HB_QSizeF():fromPointer( ... )


CREATE CLASS QSizeF INHERIT HbQtObjectHandler FUNCTION HB_QSizeF

   METHOD  new( ... )

   METHOD  boundedTo                     // ( oQSizeF )                                        -> oQSizeF
   METHOD  expandedTo                    // ( oQSizeF )                                        -> oQSizeF
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  rheight                       // (  )                                               -> nQreal
   METHOD  rwidth                        // (  )                                               -> nQreal
   METHOD  scale                         // ( nWidth, nHeight, nMode )                         -> NIL
                                         // ( oQSizeF, nMode )                                 -> NIL
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  toSize                        // (  )                                               -> oQSize
   METHOD  transpose                     // (  )                                               -> NIL
   METHOD  width                         // (  )                                               -> nQreal

   ENDCLASS


METHOD QSizeF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSizeF( ... )
   RETURN Self


METHOD QSizeF:boundedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QSizeF_boundedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:expandedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QSizeF_expandedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:rheight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_rheight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:rwidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_rwidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:scale( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSizeF_scale( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSizeF_scale_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizeF_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizeF_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:toSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QSizeF_toSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:transpose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_transpose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

