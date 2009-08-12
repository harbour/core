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


CREATE CLASS QSplitter INHERIT QFrame

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QSplitter_destroy( ::pPtr )

   METHOD  addWidget( pWidget )                INLINE  Qt_QSplitter_addWidget( ::pPtr, pWidget )
   METHOD  childrenCollapsible()               INLINE  Qt_QSplitter_childrenCollapsible( ::pPtr )
   METHOD  count()                             INLINE  Qt_QSplitter_count( ::pPtr )
   METHOD  getRange( nIndex, nMin, nMax )      INLINE  Qt_QSplitter_getRange( ::pPtr, nIndex, nMin, nMax )
   METHOD  handle( nIndex )                    INLINE  Qt_QSplitter_handle( ::pPtr, nIndex )
   METHOD  handleWidth()                       INLINE  Qt_QSplitter_handleWidth( ::pPtr )
   METHOD  indexOf( pWidget )                  INLINE  Qt_QSplitter_indexOf( ::pPtr, pWidget )
   METHOD  insertWidget( nIndex, pWidget )     INLINE  Qt_QSplitter_insertWidget( ::pPtr, nIndex, pWidget )
   METHOD  isCollapsible( nIndex )             INLINE  Qt_QSplitter_isCollapsible( ::pPtr, nIndex )
   METHOD  opaqueResize()                      INLINE  Qt_QSplitter_opaqueResize( ::pPtr )
   METHOD  orientation()                       INLINE  Qt_QSplitter_orientation( ::pPtr )
   METHOD  refresh()                           INLINE  Qt_QSplitter_refresh( ::pPtr )
   METHOD  restoreState( pState )              INLINE  Qt_QSplitter_restoreState( ::pPtr, pState )
   METHOD  saveState()                         INLINE  Qt_QSplitter_saveState( ::pPtr )
   METHOD  setChildrenCollapsible( lBool )     INLINE  Qt_QSplitter_setChildrenCollapsible( ::pPtr, lBool )
   METHOD  setCollapsible( nIndex, lCollapse )  INLINE  Qt_QSplitter_setCollapsible( ::pPtr, nIndex, lCollapse )
   METHOD  setHandleWidth( nInt )              INLINE  Qt_QSplitter_setHandleWidth( ::pPtr, nInt )
   METHOD  setOpaqueResize( lOpaque )          INLINE  Qt_QSplitter_setOpaqueResize( ::pPtr, lOpaque )
   METHOD  setOrientation( nQt_Orientation )   INLINE  Qt_QSplitter_setOrientation( ::pPtr, nQt_Orientation )
   METHOD  setStretchFactor( nIndex, nStretch )  INLINE  Qt_QSplitter_setStretchFactor( ::pPtr, nIndex, nStretch )
   METHOD  widget( nIndex )                    INLINE  Qt_QSplitter_widget( ::pPtr, nIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QSplitter

   ::pParent := pParent

   ::pPtr := Qt_QSplitter( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QSplitter

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
