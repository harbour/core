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


FUNCTION QMetaProperty( ... )
   RETURN HB_QMetaProperty():new( ... )

FUNCTION QMetaPropertyFromPointer( ... )
   RETURN HB_QMetaProperty():fromPointer( ... )


CREATE CLASS QMetaProperty INHERIT HbQtObjectHandler FUNCTION HB_QMetaProperty

   METHOD  new( ... )

   METHOD  enumerator                    // (  )                                               -> oQMetaEnum
   METHOD  hasNotifySignal               // (  )                                               -> lBool
   METHOD  isDesignable                  // ( oQObject )                                       -> lBool
   METHOD  isEnumType                    // (  )                                               -> lBool
   METHOD  isFlagType                    // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isResettable                  // (  )                                               -> lBool
   METHOD  isScriptable                  // ( oQObject )                                       -> lBool
   METHOD  isStored                      // ( oQObject )                                       -> lBool
   METHOD  isUser                        // ( oQObject )                                       -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  name                          // (  )                                               -> cChar
   METHOD  notifySignal                  // (  )                                               -> oQMetaMethod
   METHOD  notifySignalIndex             // (  )                                               -> nInt
   METHOD  read                          // ( oQObject )                                       -> oQVariant
   METHOD  reset                         // ( oQObject )                                       -> lBool
   METHOD  type                          // (  )                                               -> nQVariant_Type
   METHOD  typeName                      // (  )                                               -> cChar
   METHOD  userType                      // (  )                                               -> nInt
   METHOD  write                         // ( oQObject, oQVariant )                            -> lBool

   ENDCLASS


METHOD QMetaProperty:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMetaProperty( ... )
   RETURN Self


METHOD QMetaProperty:enumerator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMetaEnumFromPointer( Qt_QMetaProperty_enumerator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:hasNotifySignal( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_hasNotifySignal( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isDesignable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaProperty_isDesignable( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QMetaProperty_isDesignable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isEnumType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isEnumType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isFlagType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isFlagType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isResettable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isResettable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isScriptable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaProperty_isScriptable( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QMetaProperty_isScriptable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isStored( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaProperty_isStored( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QMetaProperty_isStored( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isUser( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaProperty_isUser( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QMetaProperty_isUser( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:notifySignal( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMetaMethodFromPointer( Qt_QMetaProperty_notifySignal( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:notifySignalIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_notifySignalIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QMetaProperty_read( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:reset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaProperty_reset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:typeName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_typeName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:userType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaProperty_userType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaProperty:write( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMetaProperty_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

