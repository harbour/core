/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


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

