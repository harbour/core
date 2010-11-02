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


FUNCTION QMetaObject( ... )
   RETURN HB_QMetaObject():new( ... )

FUNCTION QMetaObjectFromPointer( ... )
   RETURN HB_QMetaObject():fromPointer( ... )


CREATE CLASS QMetaObject INHERIT HbQtObjectHandler FUNCTION HB_QMetaObject

   METHOD  new( ... )

   METHOD  classInfo                     // ( nIndex )                                         -> oQMetaClassInfo
   METHOD  classInfoCount                // (  )                                               -> nInt
   METHOD  classInfoOffset               // (  )                                               -> nInt
   METHOD  className                     // (  )                                               -> cChar
   METHOD  constructor                   // ( nIndex )                                         -> oQMetaMethod
   METHOD  constructorCount              // (  )                                               -> nInt
   METHOD  enumerator                    // ( nIndex )                                         -> oQMetaEnum
   METHOD  enumeratorCount               // (  )                                               -> nInt
   METHOD  enumeratorOffset              // (  )                                               -> nInt
   METHOD  indexOfClassInfo              // ( cName )                                          -> nInt
   METHOD  indexOfConstructor            // ( cConstructor )                                   -> nInt
   METHOD  indexOfEnumerator             // ( cName )                                          -> nInt
   METHOD  indexOfMethod                 // ( cMethod )                                        -> nInt
   METHOD  indexOfProperty               // ( cName )                                          -> nInt
   METHOD  indexOfSignal                 // ( cSignal )                                        -> nInt
   METHOD  indexOfSlot                   // ( cSlot )                                          -> nInt
   METHOD  method                        // ( nIndex )                                         -> oQMetaMethod
   METHOD  methodCount                   // (  )                                               -> nInt
   METHOD  methodOffset                  // (  )                                               -> nInt
   METHOD  property                      // ( nIndex )                                         -> oQMetaProperty
   METHOD  propertyCount                 // (  )                                               -> nInt
   METHOD  propertyOffset                // (  )                                               -> nInt
   METHOD  superClass                    // (  )                                               -> oQMetaObject
   METHOD  userProperty                  // (  )                                               -> oQMetaProperty
   METHOD  checkConnectArgs              // ( cSignal, cMethod )                               -> lBool
   METHOD  connectSlotsByName            // ( oQObject )                                       -> NIL
   METHOD  normalizedSignature           // ( cMethod )                                        -> oQByteArray
   METHOD  normalizedType                // ( cType )                                          -> oQByteArray

   ENDCLASS


METHOD QMetaObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMetaObject( ... )
   RETURN Self


METHOD QMetaObject:classInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMetaClassInfoFromPointer( Qt_QMetaObject_classInfo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:classInfoCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_classInfoCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:classInfoOffset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_classInfoOffset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:className( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_className( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:constructor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMetaMethodFromPointer( Qt_QMetaObject_constructor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:constructorCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_constructorCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:enumerator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMetaEnumFromPointer( Qt_QMetaObject_enumerator( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:enumeratorCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_enumeratorCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:enumeratorOffset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_enumeratorOffset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfClassInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfClassInfo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfConstructor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfConstructor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfEnumerator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfEnumerator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfMethod( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfMethod( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfSignal( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfSignal( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:indexOfSlot( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_indexOfSlot( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:method( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMetaMethodFromPointer( Qt_QMetaObject_method( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:methodCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_methodCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:methodOffset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_methodOffset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:property( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMetaPropertyFromPointer( Qt_QMetaObject_property( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:propertyCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_propertyCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:propertyOffset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaObject_propertyOffset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:superClass( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMetaObjectFromPointer( Qt_QMetaObject_superClass( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:userProperty( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMetaPropertyFromPointer( Qt_QMetaObject_userProperty( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:checkConnectArgs( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QMetaObject_checkConnectArgs( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:connectSlotsByName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMetaObject_connectSlotsByName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:normalizedSignature( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QMetaObject_normalizedSignature( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaObject:normalizedType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QMetaObject_normalizedType( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

