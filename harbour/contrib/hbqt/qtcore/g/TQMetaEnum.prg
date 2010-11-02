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


FUNCTION QMetaEnum( ... )
   RETURN HB_QMetaEnum():new( ... )

FUNCTION QMetaEnumFromPointer( ... )
   RETURN HB_QMetaEnum():fromPointer( ... )


CREATE CLASS QMetaEnum INHERIT HbQtObjectHandler FUNCTION HB_QMetaEnum

   METHOD  new( ... )

   METHOD  isFlag                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  key                           // ( nIndex )                                         -> cChar
   METHOD  keyCount                      // (  )                                               -> nInt
   METHOD  keyToValue                    // ( cKey )                                           -> nInt
   METHOD  keysToValue                   // ( cKeys )                                          -> nInt
   METHOD  name                          // (  )                                               -> cChar
   METHOD  scope                         // (  )                                               -> cChar
   METHOD  value                         // ( nIndex )                                         -> nInt
   METHOD  valueToKey                    // ( nValue )                                         -> cChar
   METHOD  valueToKeys                   // ( nValue )                                         -> oQByteArray

   ENDCLASS


METHOD QMetaEnum:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMetaEnum( ... )
   RETURN Self


METHOD QMetaEnum:isFlag( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaEnum_isFlag( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaEnum_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:key( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMetaEnum_key( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:keyCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaEnum_keyCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:keyToValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaEnum_keyToValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:keysToValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaEnum_keysToValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaEnum_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:scope( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaEnum_scope( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:value( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMetaEnum_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:valueToKey( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMetaEnum_valueToKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaEnum:valueToKeys( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QMetaEnum_valueToKeys( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

