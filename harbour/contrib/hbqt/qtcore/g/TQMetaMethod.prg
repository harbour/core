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


FUNCTION QMetaMethod( ... )
   RETURN HB_QMetaMethod():new( ... )

FUNCTION QMetaMethodFromPointer( ... )
   RETURN HB_QMetaMethod():fromPointer( ... )


CREATE CLASS QMetaMethod INHERIT HbQtObjectHandler FUNCTION HB_QMetaMethod

   METHOD  new( ... )

   METHOD  access                        // (  )                                               -> nAccess
   METHOD  methodType                    // (  )                                               -> nMethodType
   METHOD  parameterNames                // (  )                                               -> oQList_QByteArray>
   METHOD  parameterTypes                // (  )                                               -> oQList_QByteArray>
   METHOD  signature                     // (  )                                               -> cChar
   METHOD  tag                           // (  )                                               -> cChar
   METHOD  typeName                      // (  )                                               -> cChar

   ENDCLASS


METHOD QMetaMethod:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMetaMethod( ... )
   RETURN Self


METHOD QMetaMethod:access( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaMethod_access( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:methodType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaMethod_methodType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:parameterNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QMetaMethod_parameterNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:parameterTypes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QMetaMethod_parameterTypes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:signature( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaMethod_signature( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:tag( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaMethod_tag( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaMethod:typeName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaMethod_typeName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

