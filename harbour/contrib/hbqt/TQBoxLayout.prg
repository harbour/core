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


CLASS QBoxLayout INHERIT QLayout

   DATA    pPtr

   METHOD  New()

   METHOD  addLayout( pLayout, nStretch )      INLINE  Qt_QBoxLayout_addLayout( ::pPtr, pLayout, nStretch )
   METHOD  addSpacerItem( pSpacerItem )        INLINE  Qt_QBoxLayout_addSpacerItem( ::pPtr, pSpacerItem )
   METHOD  addSpacing( nSize )                 INLINE  Qt_QBoxLayout_addSpacing( ::pPtr, nSize )
   METHOD  addStretch( nStretch )              INLINE  Qt_QBoxLayout_addStretch( ::pPtr, nStretch )
   METHOD  addStrut( nSize )                   INLINE  Qt_QBoxLayout_addStrut( ::pPtr, nSize )
   METHOD  addWidget( pWidget, nStretch, nAlignment )  INLINE  Qt_QBoxLayout_addWidget( ::pPtr, pWidget, nStretch, nAlignment )
   METHOD  direction()                         INLINE  Qt_QBoxLayout_direction( ::pPtr )
   METHOD  insertLayout( nIndex, pLayout, nStretch )  INLINE  Qt_QBoxLayout_insertLayout( ::pPtr, nIndex, pLayout, nStretch )
   METHOD  insertSpacerItem( nIndex, pSpacerItem )  INLINE  Qt_QBoxLayout_insertSpacerItem( ::pPtr, nIndex, pSpacerItem )
   METHOD  insertSpacing( nIndex, nSize )      INLINE  Qt_QBoxLayout_insertSpacing( ::pPtr, nIndex, nSize )
   METHOD  insertStretch( nIndex, nStretch )   INLINE  Qt_QBoxLayout_insertStretch( ::pPtr, nIndex, nStretch )
   METHOD  insertWidget( nIndex, pWidget, nStretch, nAlignment )  INLINE  Qt_QBoxLayout_insertWidget( ::pPtr, nIndex, pWidget, nStretch, nAlignment )
   METHOD  invalidate()                        INLINE  Qt_QBoxLayout_invalidate( ::pPtr )
   METHOD  setDirection( nDirection )          INLINE  Qt_QBoxLayout_setDirection( ::pPtr, nDirection )
   METHOD  setSpacing( nSpacing )              INLINE  Qt_QBoxLayout_setSpacing( ::pPtr, nSpacing )
   METHOD  setStretch( nIndex, nStretch )      INLINE  Qt_QBoxLayout_setStretch( ::pPtr, nIndex, nStretch )
   METHOD  setStretchFactor( pWidget, nStretch )  INLINE  Qt_QBoxLayout_setStretchFactor( ::pPtr, pWidget, nStretch )
   METHOD  setStretchFactor_1( pLayout, nStretch )  INLINE  Qt_QBoxLayout_setStretchFactor_1( ::pPtr, pLayout, nStretch )
   METHOD  spacing()                           INLINE  Qt_QBoxLayout_spacing( ::pPtr )
   METHOD  stretch( nIndex )                   INLINE  Qt_QBoxLayout_stretch( ::pPtr, nIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QBoxLayout

   ::pPtr := Qt_QBoxLayout( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

