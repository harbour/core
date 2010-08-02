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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


CREATE CLASS QObject INHERIT HbQtObjectHandler

   METHOD  new( ... )

   METHOD  blockSignals( lBlock )
   METHOD  connect( pSender, pSignal, pMethod, nType )
   METHOD  disconnect( pSignal, pReceiver, pMethod )
   METHOD  disconnect_1( pReceiver, pMethod )
   METHOD  dumpObjectInfo()
   METHOD  dumpObjectTree()
   METHOD  dynamicPropertyNames()
   METHOD  event( pE )
   METHOD  eventFilter( pWatched, pEvent )
   METHOD  inherits( pClassName )
   METHOD  installEventFilter( pFilterObj )
   METHOD  isWidgetType()
   METHOD  killTimer( nId )
   METHOD  moveToThread( pTargetThread )
   METHOD  objectName()
   METHOD  parent()
   METHOD  property( pName )
   METHOD  removeEventFilter( pObj )
   METHOD  setObjectName( cName )
   METHOD  setParent( pParent )
   METHOD  setProperty( pName, pValue )
   METHOD  signalsBlocked()
   METHOD  startTimer( nInterval )
   METHOD  thread()
   METHOD  connect_1( pSender, pSignal, pReceiver, pMethod, nType )
   METHOD  disconnect_2( pSender, pSignal, pReceiver, pMethod )
   METHOD  tr( pSourceText, pDisambiguation, nN )
   METHOD  trUtf8( pSourceText, pDisambiguation, nN )
   METHOD  deleteLater()

   ENDCLASS


METHOD QObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QObject( ... )
   RETURN Self


METHOD QObject:blockSignals( lBlock )
   RETURN Qt_QObject_blockSignals( ::pPtr, lBlock )


METHOD QObject:connect( pSender, pSignal, pMethod, nType )
   RETURN Qt_QObject_connect( ::pPtr, hbqt_ptr( pSender ), hbqt_ptr( pSignal ), hbqt_ptr( pMethod ), nType )


METHOD QObject:disconnect( pSignal, pReceiver, pMethod )
   RETURN Qt_QObject_disconnect( ::pPtr, hbqt_ptr( pSignal ), hbqt_ptr( pReceiver ), hbqt_ptr( pMethod ) )


METHOD QObject:disconnect_1( pReceiver, pMethod )
   RETURN Qt_QObject_disconnect_1( ::pPtr, hbqt_ptr( pReceiver ), hbqt_ptr( pMethod ) )


METHOD QObject:dumpObjectInfo()
   RETURN Qt_QObject_dumpObjectInfo( ::pPtr )


METHOD QObject:dumpObjectTree()
   RETURN Qt_QObject_dumpObjectTree( ::pPtr )


METHOD QObject:dynamicPropertyNames()
   RETURN Qt_QObject_dynamicPropertyNames( ::pPtr )


METHOD QObject:event( pE )
   RETURN Qt_QObject_event( ::pPtr, hbqt_ptr( pE ) )


METHOD QObject:eventFilter( pWatched, pEvent )
   RETURN Qt_QObject_eventFilter( ::pPtr, hbqt_ptr( pWatched ), hbqt_ptr( pEvent ) )


METHOD QObject:inherits( pClassName )
   RETURN Qt_QObject_inherits( ::pPtr, hbqt_ptr( pClassName ) )


METHOD QObject:installEventFilter( pFilterObj )
   RETURN Qt_QObject_installEventFilter( ::pPtr, hbqt_ptr( pFilterObj ) )


METHOD QObject:isWidgetType()
   RETURN Qt_QObject_isWidgetType( ::pPtr )


METHOD QObject:killTimer( nId )
   RETURN Qt_QObject_killTimer( ::pPtr, nId )


METHOD QObject:moveToThread( pTargetThread )
   RETURN Qt_QObject_moveToThread( ::pPtr, hbqt_ptr( pTargetThread ) )


METHOD QObject:objectName()
   RETURN Qt_QObject_objectName( ::pPtr )


METHOD QObject:parent()
   RETURN Qt_QObject_parent( ::pPtr )


METHOD QObject:property( pName )
   RETURN Qt_QObject_property( ::pPtr, hbqt_ptr( pName ) )


METHOD QObject:removeEventFilter( pObj )
   RETURN Qt_QObject_removeEventFilter( ::pPtr, hbqt_ptr( pObj ) )


METHOD QObject:setObjectName( cName )
   RETURN Qt_QObject_setObjectName( ::pPtr, cName )


METHOD QObject:setParent( pParent )
   RETURN Qt_QObject_setParent( ::pPtr, hbqt_ptr( pParent ) )


METHOD QObject:setProperty( pName, pValue )
   RETURN Qt_QObject_setProperty( ::pPtr, hbqt_ptr( pName ), hbqt_ptr( pValue ) )


METHOD QObject:signalsBlocked()
   RETURN Qt_QObject_signalsBlocked( ::pPtr )


METHOD QObject:startTimer( nInterval )
   RETURN Qt_QObject_startTimer( ::pPtr, nInterval )


METHOD QObject:thread()
   RETURN Qt_QObject_thread( ::pPtr )


METHOD QObject:connect_1( pSender, pSignal, pReceiver, pMethod, nType )
   RETURN Qt_QObject_connect_1( ::pPtr, hbqt_ptr( pSender ), hbqt_ptr( pSignal ), hbqt_ptr( pReceiver ), hbqt_ptr( pMethod ), nType )


METHOD QObject:disconnect_2( pSender, pSignal, pReceiver, pMethod )
   RETURN Qt_QObject_disconnect_2( ::pPtr, hbqt_ptr( pSender ), hbqt_ptr( pSignal ), hbqt_ptr( pReceiver ), hbqt_ptr( pMethod ) )


METHOD QObject:tr( pSourceText, pDisambiguation, nN )
   RETURN Qt_QObject_tr( ::pPtr, hbqt_ptr( pSourceText ), hbqt_ptr( pDisambiguation ), nN )


METHOD QObject:trUtf8( pSourceText, pDisambiguation, nN )
   RETURN Qt_QObject_trUtf8( ::pPtr, hbqt_ptr( pSourceText ), hbqt_ptr( pDisambiguation ), nN )


METHOD QObject:deleteLater()
   RETURN Qt_QObject_deleteLater( ::pPtr )

