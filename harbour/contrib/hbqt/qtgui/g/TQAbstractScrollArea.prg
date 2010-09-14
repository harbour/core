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


FUNCTION QAbstractScrollArea( ... )
   RETURN HB_QAbstractScrollArea():new( ... )


CREATE CLASS QAbstractScrollArea INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QAbstractScrollArea

   METHOD  new( ... )

   METHOD  addScrollBarWidget( pWidget, nAlignment )
   METHOD  cornerWidget()
   METHOD  horizontalScrollBar()
   METHOD  horizontalScrollBarPolicy()
   METHOD  maximumViewportSize()
   METHOD  setCornerWidget( pWidget )
   METHOD  setHorizontalScrollBar( pScrollBar )
   METHOD  setHorizontalScrollBarPolicy( nQt_ScrollBarPolicy )
   METHOD  setVerticalScrollBar( pScrollBar )
   METHOD  setVerticalScrollBarPolicy( nQt_ScrollBarPolicy )
   METHOD  setViewport( pWidget )
   METHOD  verticalScrollBar()
   METHOD  verticalScrollBarPolicy()
   METHOD  viewport()

   ENDCLASS


METHOD QAbstractScrollArea:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractScrollArea( ... )
   RETURN Self


METHOD QAbstractScrollArea:addScrollBarWidget( pWidget, nAlignment )
   RETURN Qt_QAbstractScrollArea_addScrollBarWidget( ::pPtr, hbqt_ptr( pWidget ), nAlignment )


METHOD QAbstractScrollArea:cornerWidget()
   RETURN Qt_QAbstractScrollArea_cornerWidget( ::pPtr )


METHOD QAbstractScrollArea:horizontalScrollBar()
   RETURN Qt_QAbstractScrollArea_horizontalScrollBar( ::pPtr )


METHOD QAbstractScrollArea:horizontalScrollBarPolicy()
   RETURN Qt_QAbstractScrollArea_horizontalScrollBarPolicy( ::pPtr )


METHOD QAbstractScrollArea:maximumViewportSize()
   RETURN Qt_QAbstractScrollArea_maximumViewportSize( ::pPtr )


METHOD QAbstractScrollArea:setCornerWidget( pWidget )
   RETURN Qt_QAbstractScrollArea_setCornerWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QAbstractScrollArea:setHorizontalScrollBar( pScrollBar )
   RETURN Qt_QAbstractScrollArea_setHorizontalScrollBar( ::pPtr, hbqt_ptr( pScrollBar ) )


METHOD QAbstractScrollArea:setHorizontalScrollBarPolicy( nQt_ScrollBarPolicy )
   RETURN Qt_QAbstractScrollArea_setHorizontalScrollBarPolicy( ::pPtr, nQt_ScrollBarPolicy )


METHOD QAbstractScrollArea:setVerticalScrollBar( pScrollBar )
   RETURN Qt_QAbstractScrollArea_setVerticalScrollBar( ::pPtr, hbqt_ptr( pScrollBar ) )


METHOD QAbstractScrollArea:setVerticalScrollBarPolicy( nQt_ScrollBarPolicy )
   RETURN Qt_QAbstractScrollArea_setVerticalScrollBarPolicy( ::pPtr, nQt_ScrollBarPolicy )


METHOD QAbstractScrollArea:setViewport( pWidget )
   RETURN Qt_QAbstractScrollArea_setViewport( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QAbstractScrollArea:verticalScrollBar()
   RETURN Qt_QAbstractScrollArea_verticalScrollBar( ::pPtr )


METHOD QAbstractScrollArea:verticalScrollBarPolicy()
   RETURN Qt_QAbstractScrollArea_verticalScrollBarPolicy( ::pPtr )


METHOD QAbstractScrollArea:viewport()
   RETURN Qt_QAbstractScrollArea_viewport( ::pPtr )

