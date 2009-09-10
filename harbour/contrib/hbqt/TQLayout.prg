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


CREATE CLASS QLayout INHERIT QObject, QLayoutItem

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QLayout_destroy( ::pPtr )

   METHOD  activate()                          INLINE  Qt_QLayout_activate( ::pPtr )
   METHOD  addItem( pItem )                    INLINE  Qt_QLayout_addItem( ::pPtr, pItem )
   METHOD  addWidget( pW )                     INLINE  Qt_QLayout_addWidget( ::pPtr, pW )
   METHOD  contentsRect()                      INLINE  Qt_QLayout_contentsRect( ::pPtr )
   METHOD  count()                             INLINE  Qt_QLayout_count( ::pPtr )
   METHOD  expandingDirections()               INLINE  Qt_QLayout_expandingDirections( ::pPtr )
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QLayout_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  indexOf( pWidget )                  INLINE  Qt_QLayout_indexOf( ::pPtr, pWidget )
   METHOD  isEnabled()                         INLINE  Qt_QLayout_isEnabled( ::pPtr )
   METHOD  itemAt( nIndex )                    INLINE  Qt_QLayout_itemAt( ::pPtr, nIndex )
   METHOD  maximumSize()                       INLINE  Qt_QLayout_maximumSize( ::pPtr )
   METHOD  menuBar()                           INLINE  Qt_QLayout_menuBar( ::pPtr )
   METHOD  minimumSize()                       INLINE  Qt_QLayout_minimumSize( ::pPtr )
   METHOD  parentWidget()                      INLINE  Qt_QLayout_parentWidget( ::pPtr )
   METHOD  removeItem( pItem )                 INLINE  Qt_QLayout_removeItem( ::pPtr, pItem )
   METHOD  removeWidget( pWidget )             INLINE  Qt_QLayout_removeWidget( ::pPtr, pWidget )
   METHOD  setAlignment( pW, nAlignment )      INLINE  Qt_QLayout_setAlignment( ::pPtr, pW, nAlignment )
   METHOD  setAlignment_1( nAlignment )        INLINE  Qt_QLayout_setAlignment_1( ::pPtr, nAlignment )
   METHOD  setAlignment_2( pL, nAlignment )    INLINE  Qt_QLayout_setAlignment_2( ::pPtr, pL, nAlignment )
   METHOD  setContentsMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QLayout_setContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  setEnabled( lEnable )               INLINE  Qt_QLayout_setEnabled( ::pPtr, lEnable )
   METHOD  setMenuBar( pWidget )               INLINE  Qt_QLayout_setMenuBar( ::pPtr, pWidget )
   METHOD  setSizeConstraint( nSizeConstraint )  INLINE  Qt_QLayout_setSizeConstraint( ::pPtr, nSizeConstraint )
   METHOD  setSpacing( nInt )                  INLINE  Qt_QLayout_setSpacing( ::pPtr, nInt )
   METHOD  sizeConstraint()                    INLINE  Qt_QLayout_sizeConstraint( ::pPtr )
   METHOD  spacing()                           INLINE  Qt_QLayout_spacing( ::pPtr )
   METHOD  takeAt( nIndex )                    INLINE  Qt_QLayout_takeAt( ::pPtr, nIndex )
   METHOD  update()                            INLINE  Qt_QLayout_update( ::pPtr )
   METHOD  closestAcceptableSize( pWidget, pSize )  INLINE  Qt_QLayout_closestAcceptableSize( ::pPtr, pWidget, pSize )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QLayout

   ::pParent := pParent

   ::pPtr := Qt_QLayout( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QLayout

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
