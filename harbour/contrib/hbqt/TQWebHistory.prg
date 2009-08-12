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


CREATE CLASS QWebHistory

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QWebHistory_destroy( ::pPtr )

   METHOD  back()                              INLINE  Qt_QWebHistory_back( ::pPtr )
   METHOD  backItem()                          INLINE  Qt_QWebHistory_backItem( ::pPtr )
   METHOD  canGoBack()                         INLINE  Qt_QWebHistory_canGoBack( ::pPtr )
   METHOD  canGoForward()                      INLINE  Qt_QWebHistory_canGoForward( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QWebHistory_clear( ::pPtr )
   METHOD  count()                             INLINE  Qt_QWebHistory_count( ::pPtr )
   METHOD  currentItem()                       INLINE  Qt_QWebHistory_currentItem( ::pPtr )
   METHOD  currentItemIndex()                  INLINE  Qt_QWebHistory_currentItemIndex( ::pPtr )
   METHOD  forward()                           INLINE  Qt_QWebHistory_forward( ::pPtr )
   METHOD  forwardItem()                       INLINE  Qt_QWebHistory_forwardItem( ::pPtr )
   METHOD  goToItem( pItem )                   INLINE  Qt_QWebHistory_goToItem( ::pPtr, pItem )
   METHOD  itemAt( nI )                        INLINE  Qt_QWebHistory_itemAt( ::pPtr, nI )
   METHOD  maximumItemCount()                  INLINE  Qt_QWebHistory_maximumItemCount( ::pPtr )
   METHOD  setMaximumItemCount( nCount )       INLINE  Qt_QWebHistory_setMaximumItemCount( ::pPtr, nCount )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWebHistory

   ::pParent := pParent

   ::pPtr := Qt_QWebHistory( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QWebHistory

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
