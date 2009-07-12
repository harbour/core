/*
 * $Id$
 */

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


CREATE CLASS QSignalMapper INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QSignalMapper_destroy( ::pPtr )

   METHOD  mapping( nId )                      INLINE  Qt_QSignalMapper_mapping( ::pPtr, nId )
   METHOD  mapping_1( cId )                    INLINE  Qt_QSignalMapper_mapping_1( ::pPtr, cId )
   METHOD  mapping_2( pWidget )                INLINE  Qt_QSignalMapper_mapping_2( ::pPtr, pWidget )
   METHOD  mapping_3( pObject )                INLINE  Qt_QSignalMapper_mapping_3( ::pPtr, pObject )
   METHOD  removeMappings( pSender )           INLINE  Qt_QSignalMapper_removeMappings( ::pPtr, pSender )
   METHOD  setMapping( pSender, nId )          INLINE  Qt_QSignalMapper_setMapping( ::pPtr, pSender, nId )
   METHOD  setMapping_1( pSender, cText )      INLINE  Qt_QSignalMapper_setMapping_1( ::pPtr, pSender, cText )
   METHOD  setMapping_2( pSender, pWidget )    INLINE  Qt_QSignalMapper_setMapping_2( ::pPtr, pSender, pWidget )
   METHOD  setMapping_3( pSender, pObject )    INLINE  Qt_QSignalMapper_setMapping_3( ::pPtr, pSender, pObject )
   METHOD  map()                               INLINE  Qt_QSignalMapper_map( ::pPtr )
   METHOD  map_1( pSender )                    INLINE  Qt_QSignalMapper_map_1( ::pPtr, pSender )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QSignalMapper

   ::pParent := pParent

   ::pPtr := Qt_QSignalMapper( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QSignalMapper

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

