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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QObject_destroy( ::pPtr )

   METHOD  blockSignals( lBlock )              INLINE  Qt_QObject_blockSignals( ::pPtr, lBlock )
   METHOD  children()                          INLINE  Qt_QObject_children( ::pPtr )
   METHOD  connect( pSender, pSignal, pMethod, nType )  INLINE  Qt_QObject_connect( ::pPtr, pSender, pSignal, pMethod, nType )
   METHOD  disconnect( pSignal, pReceiver, pMethod )  INLINE  Qt_QObject_disconnect( ::pPtr, pSignal, pReceiver, pMethod )
   METHOD  disconnect_1( pReceiver, pMethod )  INLINE  Qt_QObject_disconnect_1( ::pPtr, pReceiver, pMethod )
   METHOD  dumpObjectInfo()                    INLINE  Qt_QObject_dumpObjectInfo( ::pPtr )
   METHOD  dumpObjectTree()                    INLINE  Qt_QObject_dumpObjectTree( ::pPtr )
   METHOD  event( pE )                         INLINE  Qt_QObject_event( ::pPtr, pE )
   METHOD  eventFilter( pWatched, pEvent )     INLINE  Qt_QObject_eventFilter( ::pPtr, pWatched, pEvent )
   METHOD  inherits( pClassName )              INLINE  Qt_QObject_inherits( ::pPtr, pClassName )
   METHOD  installEventFilter( pFilterObj )    INLINE  Qt_QObject_installEventFilter( ::pPtr, pFilterObj )
   METHOD  isWidgetType()                      INLINE  Qt_QObject_isWidgetType( ::pPtr )
   METHOD  killTimer( nId )                    INLINE  Qt_QObject_killTimer( ::pPtr, nId )
   METHOD  metaObject()                        INLINE  Qt_QObject_metaObject( ::pPtr )
   METHOD  moveToThread( pTargetThread )       INLINE  Qt_QObject_moveToThread( ::pPtr, pTargetThread )
   METHOD  objectName()                        INLINE  Qt_QObject_objectName( ::pPtr )
   METHOD  parent()                            INLINE  Qt_QObject_parent( ::pPtr )
   METHOD  property( pName )                   INLINE  Qt_QObject_property( ::pPtr, pName )
   METHOD  removeEventFilter( pObj )           INLINE  Qt_QObject_removeEventFilter( ::pPtr, pObj )
   METHOD  setObjectName( cName )              INLINE  Qt_QObject_setObjectName( ::pPtr, cName )
   METHOD  setParent( pParent )                INLINE  Qt_QObject_setParent( ::pPtr, pParent )
   METHOD  setProperty( pName, pValue )        INLINE  Qt_QObject_setProperty( ::pPtr, pName, pValue )
   METHOD  signalsBlocked()                    INLINE  Qt_QObject_signalsBlocked( ::pPtr )
   METHOD  startTimer( nInterval )             INLINE  Qt_QObject_startTimer( ::pPtr, nInterval )
   METHOD  thread()                            INLINE  Qt_QObject_thread( ::pPtr )
   METHOD  connect_1( pSender, pSignal, pReceiver, pMethod, nType )  INLINE  Qt_QObject_connect_1( ::pPtr, pSender, pSignal, pReceiver, pMethod, nType )
   METHOD  disconnect_2( pSender, pSignal, pReceiver, pMethod )  INLINE  Qt_QObject_disconnect_2( ::pPtr, pSender, pSignal, pReceiver, pMethod )
   METHOD  tr( pSourceText, pDisambiguation, nN )  INLINE  Qt_QObject_tr( ::pPtr, pSourceText, pDisambiguation, nN )
   METHOD  trUtf8( pSourceText, pDisambiguation, nN )  INLINE  Qt_QObject_trUtf8( ::pPtr, pSourceText, pDisambiguation, nN )
   METHOD  deleteLater()                       INLINE  Qt_QObject_deleteLater( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QObject

   ::pParent := pParent

   ::pPtr := Qt_QObject( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QObject

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
