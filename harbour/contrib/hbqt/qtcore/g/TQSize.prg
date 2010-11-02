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


FUNCTION QSize( ... )
   RETURN HB_QSize():new( ... )

FUNCTION QSizeFromPointer( ... )
   RETURN HB_QSize():fromPointer( ... )


CREATE CLASS QSize INHERIT HbQtObjectHandler FUNCTION HB_QSize

   METHOD  new( ... )

   METHOD  height                        // (  )                                               -> nInt
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  rheight                       // (  )                                               -> nInt
   METHOD  rwidth                        // (  )                                               -> nInt
   METHOD  scale                         // ( nWidth, nHeight, nMode )                         -> NIL
                                         // ( oQSize, nMode )                                  -> NIL
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  transpose                     // (  )                                               -> NIL
   METHOD  width                         // (  )                                               -> nInt
   METHOD  boundedTo                     // ( oQSize )                                         -> oQSize
   METHOD  expandedTo                    // ( oQSize )                                         -> oQSize

   ENDCLASS


METHOD QSize:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSize( ... )
   RETURN Self


METHOD QSize:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:rheight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_rheight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:rwidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_rwidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:scale( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSize_scale( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSize_scale_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSize_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSize_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:transpose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_transpose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSize_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:boundedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QSize_boundedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSize:expandedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFromPointer( Qt_QSize_expandedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

