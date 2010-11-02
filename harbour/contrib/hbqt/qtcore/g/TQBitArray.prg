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


FUNCTION QBitArray( ... )
   RETURN HB_QBitArray():new( ... )

FUNCTION QBitArrayFromPointer( ... )
   RETURN HB_QBitArray():fromPointer( ... )


CREATE CLASS QBitArray INHERIT HbQtObjectHandler FUNCTION HB_QBitArray

   METHOD  new( ... )

   METHOD  at                            // ( nI )                                             -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  clearBit                      // ( nI )                                             -> NIL
   METHOD  count                         // (  )                                               -> nInt
                                         // ( lOn )                                            -> nInt
   METHOD  fill                          // ( lValue, nSize )                                  -> lBool
                                         // ( lValue, nBegin, nEnd )                           -> NIL
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  resize                        // ( nSize )                                          -> NIL
   METHOD  setBit                        // ( nI )                                             -> NIL
                                         // ( nI, lValue )                                     -> NIL
   METHOD  size                          // (  )                                               -> nInt
   METHOD  testBit                       // ( nI )                                             -> lBool
   METHOD  toggleBit                     // ( nI )                                             -> lBool
   METHOD  truncate                      // ( nPos )                                           -> NIL

   ENDCLASS


METHOD QBitArray:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBitArray( ... )
   RETURN Self


METHOD QBitArray:at( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_at( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBitArray_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:clearBit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_clearBit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:count( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_count_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QBitArray_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:fill( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QBitArray_fill_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBitArray_fill( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_fill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBitArray_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBitArray_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:resize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:setBit( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QBitArray_setBit_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_setBit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBitArray_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:testBit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_testBit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:toggleBit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_toggleBit( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitArray:truncate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBitArray_truncate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

