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


FUNCTION QLayoutItem( ... )
   RETURN HB_QLayoutItem():new( ... )


CREATE CLASS QLayoutItem INHERIT HbQtObjectHandler FUNCTION HB_QLayoutItem

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  controlTypes()
   METHOD  expandingDirections()
   METHOD  geometry()
   METHOD  hasHeightForWidth()
   METHOD  heightForWidth( nW )
   METHOD  invalidate()
   METHOD  isEmpty()
   METHOD  layout()
   METHOD  maximumSize()
   METHOD  minimumHeightForWidth( nW )
   METHOD  minimumSize()
   METHOD  setAlignment( nAlignment )
   METHOD  setGeometry( pR )
   METHOD  sizeHint()
   METHOD  spacerItem()
   METHOD  widget()

   ENDCLASS


METHOD QLayoutItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLayoutItem( ... )
   RETURN Self


METHOD QLayoutItem:alignment()
   RETURN Qt_QLayoutItem_alignment( ::pPtr )


METHOD QLayoutItem:controlTypes()
   RETURN Qt_QLayoutItem_controlTypes( ::pPtr )


METHOD QLayoutItem:expandingDirections()
   RETURN Qt_QLayoutItem_expandingDirections( ::pPtr )


METHOD QLayoutItem:geometry()
   RETURN Qt_QLayoutItem_geometry( ::pPtr )


METHOD QLayoutItem:hasHeightForWidth()
   RETURN Qt_QLayoutItem_hasHeightForWidth( ::pPtr )


METHOD QLayoutItem:heightForWidth( nW )
   RETURN Qt_QLayoutItem_heightForWidth( ::pPtr, nW )


METHOD QLayoutItem:invalidate()
   RETURN Qt_QLayoutItem_invalidate( ::pPtr )


METHOD QLayoutItem:isEmpty()
   RETURN Qt_QLayoutItem_isEmpty( ::pPtr )


METHOD QLayoutItem:layout()
   RETURN Qt_QLayoutItem_layout( ::pPtr )


METHOD QLayoutItem:maximumSize()
   RETURN Qt_QLayoutItem_maximumSize( ::pPtr )


METHOD QLayoutItem:minimumHeightForWidth( nW )
   RETURN Qt_QLayoutItem_minimumHeightForWidth( ::pPtr, nW )


METHOD QLayoutItem:minimumSize()
   RETURN Qt_QLayoutItem_minimumSize( ::pPtr )


METHOD QLayoutItem:setAlignment( nAlignment )
   RETURN Qt_QLayoutItem_setAlignment( ::pPtr, nAlignment )


METHOD QLayoutItem:setGeometry( pR )
   RETURN Qt_QLayoutItem_setGeometry( ::pPtr, hbqt_ptr( pR ) )


METHOD QLayoutItem:sizeHint()
   RETURN Qt_QLayoutItem_sizeHint( ::pPtr )


METHOD QLayoutItem:spacerItem()
   RETURN Qt_QLayoutItem_spacerItem( ::pPtr )


METHOD QLayoutItem:widget()
   RETURN Qt_QLayoutItem_widget( ::pPtr )

