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


CREATE CLASS QToolBar INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QToolBar_destroy( ::pPtr )

   METHOD  actionAt( pP )                      INLINE  Qt_QToolBar_actionAt( ::pPtr, pP )
   METHOD  actionAt_1( nX, nY )                INLINE  Qt_QToolBar_actionAt_1( ::pPtr, nX, nY )
   METHOD  addAction( pAction )                INLINE  Qt_QToolBar_addAction( ::pPtr, pAction )
   METHOD  addAction_1( cText )                INLINE  Qt_QToolBar_addAction_1( ::pPtr, cText )
   METHOD  addAction_2( cIcon, cText )         INLINE  Qt_QToolBar_addAction_2( ::pPtr, cIcon, cText )
   METHOD  addAction_3( cText, pReceiver, pMember )  INLINE  Qt_QToolBar_addAction_3( ::pPtr, cText, pReceiver, pMember )
   METHOD  addAction_4( cIcon, cText, pReceiver, pMember )  INLINE  Qt_QToolBar_addAction_4( ::pPtr, cIcon, cText, pReceiver, pMember )
   METHOD  addSeparator()                      INLINE  Qt_QToolBar_addSeparator( ::pPtr )
   METHOD  addWidget( pWidget )                INLINE  Qt_QToolBar_addWidget( ::pPtr, pWidget )
   METHOD  allowedAreas()                      INLINE  Qt_QToolBar_allowedAreas( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QToolBar_clear( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QToolBar_iconSize( ::pPtr )
   METHOD  insertSeparator( pBefore )          INLINE  Qt_QToolBar_insertSeparator( ::pPtr, pBefore )
   METHOD  insertWidget( pBefore, pWidget )    INLINE  Qt_QToolBar_insertWidget( ::pPtr, pBefore, pWidget )
   METHOD  isAreaAllowed( nArea )              INLINE  Qt_QToolBar_isAreaAllowed( ::pPtr, nArea )
   METHOD  isFloatable()                       INLINE  Qt_QToolBar_isFloatable( ::pPtr )
   METHOD  isFloating()                        INLINE  Qt_QToolBar_isFloating( ::pPtr )
   METHOD  isMovable()                         INLINE  Qt_QToolBar_isMovable( ::pPtr )
   METHOD  orientation()                       INLINE  Qt_QToolBar_orientation( ::pPtr )
   METHOD  setAllowedAreas( nAreas )           INLINE  Qt_QToolBar_setAllowedAreas( ::pPtr, nAreas )
   METHOD  setFloatable( lFloatable )          INLINE  Qt_QToolBar_setFloatable( ::pPtr, lFloatable )
   METHOD  setMovable( lMovable )              INLINE  Qt_QToolBar_setMovable( ::pPtr, lMovable )
   METHOD  setOrientation( nOrientation )      INLINE  Qt_QToolBar_setOrientation( ::pPtr, nOrientation )
   METHOD  toggleViewAction()                  INLINE  Qt_QToolBar_toggleViewAction( ::pPtr )
   METHOD  toolButtonStyle()                   INLINE  Qt_QToolBar_toolButtonStyle( ::pPtr )
   METHOD  widgetForAction( pAction )          INLINE  Qt_QToolBar_widgetForAction( ::pPtr, pAction )
   METHOD  setIconSize( pIconSize )            INLINE  Qt_QToolBar_setIconSize( ::pPtr, pIconSize )
   METHOD  setToolButtonStyle( nToolButtonStyle )  INLINE  Qt_QToolBar_setToolButtonStyle( ::pPtr, nToolButtonStyle )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QToolBar

   ::pParent := pParent

   ::pPtr := Qt_QToolBar( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QToolBar

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

