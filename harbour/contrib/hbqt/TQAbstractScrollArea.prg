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


CREATE CLASS QAbstractScrollArea INHERIT QFrame

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QAbstractScrollArea_destroy( ::pPtr )

   METHOD  addScrollBarWidget( pWidget, nAlignment )  INLINE  Qt_QAbstractScrollArea_addScrollBarWidget( ::pPtr, pWidget, nAlignment )
   METHOD  cornerWidget()                      INLINE  Qt_QAbstractScrollArea_cornerWidget( ::pPtr )
   METHOD  horizontalScrollBar()               INLINE  Qt_QAbstractScrollArea_horizontalScrollBar( ::pPtr )
   METHOD  horizontalScrollBarPolicy()         INLINE  Qt_QAbstractScrollArea_horizontalScrollBarPolicy( ::pPtr )
   METHOD  maximumViewportSize()               INLINE  Qt_QAbstractScrollArea_maximumViewportSize( ::pPtr )
   METHOD  scrollBarWidgets( nAlignment )      INLINE  Qt_QAbstractScrollArea_scrollBarWidgets( ::pPtr, nAlignment )
   METHOD  setCornerWidget( pWidget )          INLINE  Qt_QAbstractScrollArea_setCornerWidget( ::pPtr, pWidget )
   METHOD  setHorizontalScrollBar( pScrollBar )  INLINE  Qt_QAbstractScrollArea_setHorizontalScrollBar( ::pPtr, pScrollBar )
   METHOD  setHorizontalScrollBarPolicy( nQt_ScrollBarPolicy )  INLINE  Qt_QAbstractScrollArea_setHorizontalScrollBarPolicy( ::pPtr, nQt_ScrollBarPolicy )
   METHOD  setVerticalScrollBar( pScrollBar )  INLINE  Qt_QAbstractScrollArea_setVerticalScrollBar( ::pPtr, pScrollBar )
   METHOD  setVerticalScrollBarPolicy( nQt_ScrollBarPolicy )  INLINE  Qt_QAbstractScrollArea_setVerticalScrollBarPolicy( ::pPtr, nQt_ScrollBarPolicy )
   METHOD  setViewport( pWidget )              INLINE  Qt_QAbstractScrollArea_setViewport( ::pPtr, pWidget )
   METHOD  verticalScrollBar()                 INLINE  Qt_QAbstractScrollArea_verticalScrollBar( ::pPtr )
   METHOD  verticalScrollBarPolicy()           INLINE  Qt_QAbstractScrollArea_verticalScrollBarPolicy( ::pPtr )
   METHOD  viewport()                          INLINE  Qt_QAbstractScrollArea_viewport( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAbstractScrollArea

   ::pParent := pParent

   ::pPtr := Qt_QAbstractScrollArea( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QAbstractScrollArea

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
