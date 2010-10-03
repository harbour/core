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


FUNCTION QObject( ... )
   RETURN HB_QObject():new( ... )


CREATE CLASS QObject INHERIT HbQtObjectHandler FUNCTION HB_QObject

   METHOD  new( ... )

   METHOD  blockSignals                  // ( lBlock )                                         -> lBool
   METHOD  dumpObjectInfo                // (  )                                               -> NIL
   METHOD  dumpObjectTree                // (  )                                               -> NIL
   METHOD  dynamicPropertyNames          // (  )                                               -> oQList_QByteArray>
   METHOD  event                         // ( oQEvent )                                        -> lBool
   METHOD  eventFilter                   // ( oQObject, oQEvent )                              -> lBool
   METHOD  inherits                      // ( cClassName )                                     -> lBool
   METHOD  installEventFilter            // ( oQObject )                                       -> NIL
   METHOD  isWidgetType                  // (  )                                               -> lBool
   METHOD  killTimer                     // ( nId )                                            -> NIL
   METHOD  moveToThread                  // ( oQThread )                                       -> NIL
   METHOD  objectName                    // (  )                                               -> cQString
   METHOD  parent                        // (  )                                               -> oQObject
   METHOD  property                      // ( cName )                                          -> oQVariant
   METHOD  removeEventFilter             // ( oQObject )                                       -> NIL
   METHOD  setObjectName                 // ( cName )                                          -> NIL
   METHOD  setParent                     // ( oQObject )                                       -> NIL
   METHOD  setProperty                   // ( cName, oQVariant )                               -> lBool
   METHOD  signalsBlocked                // (  )                                               -> lBool
   METHOD  startTimer                    // ( nInterval )                                      -> nInt
   METHOD  thread                        // (  )                                               -> oQThread
   METHOD  tr                            // ( cSourceText, cDisambiguation, nN )               -> cQString
   METHOD  trUtf8                        // ( cSourceText, cDisambiguation, nN )               -> cQString
   METHOD  deleteLater                   // (  )                                               -> NIL

   ENDCLASS


METHOD QObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QObject( ... )
   RETURN Self


METHOD QObject:blockSignals( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QObject_blockSignals( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:dumpObjectInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_dumpObjectInfo( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:dumpObjectTree( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_dumpObjectTree( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:dynamicPropertyNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QList():from( Qt_QObject_dynamicPropertyNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:event( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_event( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:eventFilter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QObject_eventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:inherits( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_inherits( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:installEventFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_installEventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:isWidgetType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_isWidgetType( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:killTimer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QObject_killTimer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:moveToThread( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_moveToThread( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:objectName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_objectName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QObject():from( Qt_QObject_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:property( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QVariant():from( Qt_QObject_property( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:removeEventFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_removeEventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:setObjectName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_setObjectName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:setParent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_setParent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:setProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QObject_setProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:signalsBlocked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_signalsBlocked( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:startTimer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QObject_startTimer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:thread( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QThread():from( Qt_QObject_thread( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:tr( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:trUtf8( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QObject:deleteLater( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_deleteLater( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

