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


FUNCTION QDesignerFormWindowCursorInterface( ... )
   RETURN HB_QDesignerFormWindowCursorInterface():new( ... )


CREATE CLASS QDesignerFormWindowCursorInterface INHERIT HbQtObjectHandler FUNCTION HB_QDesignerFormWindowCursorInterface

   METHOD  new( ... )

   METHOD  current()
   METHOD  formWindow()
   METHOD  hasSelection()
   METHOD  isWidgetSelected( pWidget )
   METHOD  movePosition( nOperation, nMode )
   METHOD  position()
   METHOD  resetWidgetProperty( pWidget, cName )
   METHOD  selectedWidget( nIndex )
   METHOD  selectedWidgetCount()
   METHOD  setPosition( nPosition, nMode )
   METHOD  setProperty( cName, pValue )
   METHOD  setWidgetProperty( pWidget, cName, pValue )
   METHOD  widget( nIndex )
   METHOD  widgetCount()

   ENDCLASS


METHOD QDesignerFormWindowCursorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowCursorInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowCursorInterface:current()
   RETURN Qt_QDesignerFormWindowCursorInterface_current( ::pPtr )


METHOD QDesignerFormWindowCursorInterface:formWindow()
   RETURN Qt_QDesignerFormWindowCursorInterface_formWindow( ::pPtr )


METHOD QDesignerFormWindowCursorInterface:hasSelection()
   RETURN Qt_QDesignerFormWindowCursorInterface_hasSelection( ::pPtr )


METHOD QDesignerFormWindowCursorInterface:isWidgetSelected( pWidget )
   RETURN Qt_QDesignerFormWindowCursorInterface_isWidgetSelected( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesignerFormWindowCursorInterface:movePosition( nOperation, nMode )
   RETURN Qt_QDesignerFormWindowCursorInterface_movePosition( ::pPtr, nOperation, nMode )


METHOD QDesignerFormWindowCursorInterface:position()
   RETURN Qt_QDesignerFormWindowCursorInterface_position( ::pPtr )


METHOD QDesignerFormWindowCursorInterface:resetWidgetProperty( pWidget, cName )
   RETURN Qt_QDesignerFormWindowCursorInterface_resetWidgetProperty( ::pPtr, hbqt_ptr( pWidget ), cName )


METHOD QDesignerFormWindowCursorInterface:selectedWidget( nIndex )
   RETURN Qt_QDesignerFormWindowCursorInterface_selectedWidget( ::pPtr, nIndex )


METHOD QDesignerFormWindowCursorInterface:selectedWidgetCount()
   RETURN Qt_QDesignerFormWindowCursorInterface_selectedWidgetCount( ::pPtr )


METHOD QDesignerFormWindowCursorInterface:setPosition( nPosition, nMode )
   RETURN Qt_QDesignerFormWindowCursorInterface_setPosition( ::pPtr, nPosition, nMode )


METHOD QDesignerFormWindowCursorInterface:setProperty( cName, pValue )
   RETURN Qt_QDesignerFormWindowCursorInterface_setProperty( ::pPtr, cName, hbqt_ptr( pValue ) )


METHOD QDesignerFormWindowCursorInterface:setWidgetProperty( pWidget, cName, pValue )
   RETURN Qt_QDesignerFormWindowCursorInterface_setWidgetProperty( ::pPtr, hbqt_ptr( pWidget ), cName, hbqt_ptr( pValue ) )


METHOD QDesignerFormWindowCursorInterface:widget( nIndex )
   RETURN Qt_QDesignerFormWindowCursorInterface_widget( ::pPtr, nIndex )


METHOD QDesignerFormWindowCursorInterface:widgetCount()
   RETURN Qt_QDesignerFormWindowCursorInterface_widgetCount( ::pPtr )

