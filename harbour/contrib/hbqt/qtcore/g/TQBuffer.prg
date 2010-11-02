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


FUNCTION QBuffer( ... )
   RETURN HB_QBuffer():new( ... )

FUNCTION QBufferFromPointer( ... )
   RETURN HB_QBuffer():fromPointer( ... )


CREATE CLASS QBuffer INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QBuffer

   METHOD  new( ... )

   METHOD  buffer                        // (  )                                               -> oQByteArray
   METHOD  data                          // (  )                                               -> oQByteArray
   METHOD  setBuffer                     // ( oQByteArray )                                    -> NIL
   METHOD  setData                       // ( cData, nSize )                                   -> NIL
                                         // ( oQByteArray )                                    -> NIL

   ENDCLASS


METHOD QBuffer:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBuffer( ... )
   RETURN Self


METHOD QBuffer:buffer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QBuffer_buffer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBuffer:data( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QBuffer_data( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBuffer:setBuffer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBuffer_setBuffer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBuffer:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBuffer_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBuffer_setData_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

