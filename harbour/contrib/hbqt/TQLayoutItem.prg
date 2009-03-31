/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QLayoutItem

   DATA    pPtr

   METHOD  New()

   METHOD  alignment()                         INLINE  Qt_QLayoutItem_alignment( ::pPtr )
   METHOD  controlTypes()                      INLINE  Qt_QLayoutItem_controlTypes( ::pPtr )
   METHOD  expandingDirections()               INLINE  Qt_QLayoutItem_expandingDirections( ::pPtr )
   METHOD  geometry()                          INLINE  Qt_QLayoutItem_geometry( ::pPtr )
   METHOD  hasHeightForWidth()                 INLINE  Qt_QLayoutItem_hasHeightForWidth( ::pPtr )
   METHOD  heightForWidth( nW )                INLINE  Qt_QLayoutItem_heightForWidth( ::pPtr, nW )
   METHOD  invalidate()                        INLINE  Qt_QLayoutItem_invalidate( ::pPtr )
   METHOD  isEmpty()                           INLINE  Qt_QLayoutItem_isEmpty( ::pPtr )
   METHOD  layout()                            INLINE  Qt_QLayoutItem_layout( ::pPtr )
   METHOD  maximumSize()                       INLINE  Qt_QLayoutItem_maximumSize( ::pPtr )
   METHOD  minimumHeightForWidth( nW )         INLINE  Qt_QLayoutItem_minimumHeightForWidth( ::pPtr, nW )
   METHOD  minimumSize()                       INLINE  Qt_QLayoutItem_minimumSize( ::pPtr )
   METHOD  setAlignment( nAlignment )          INLINE  Qt_QLayoutItem_setAlignment( ::pPtr, nAlignment )
   METHOD  setGeometry( aRectR )               INLINE  Qt_QLayoutItem_setGeometry( ::pPtr, aRectR )
   METHOD  sizeHint()                          INLINE  Qt_QLayoutItem_sizeHint( ::pPtr )
   METHOD  spacerItem()                        INLINE  Qt_QLayoutItem_spacerItem( ::pPtr )
   METHOD  widget()                            INLINE  Qt_QLayoutItem_widget( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QLayoutItem

   ::pPtr := Qt_QLayoutItem( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

