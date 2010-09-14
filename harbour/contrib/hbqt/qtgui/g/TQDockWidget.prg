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


FUNCTION QDockWidget( ... )
   RETURN HB_QDockWidget():new( ... )


CREATE CLASS QDockWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDockWidget

   METHOD  new( ... )

   METHOD  allowedAreas()
   METHOD  features()
   METHOD  isAreaAllowed( nArea )
   METHOD  isFloating()
   METHOD  setAllowedAreas( nAreas )
   METHOD  setFeatures( nFeatures )
   METHOD  setFloating( lFloating )
   METHOD  setTitleBarWidget( pWidget )
   METHOD  setWidget( pWidget )
   METHOD  titleBarWidget()
   METHOD  toggleViewAction()
   METHOD  widget()

   ENDCLASS


METHOD QDockWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDockWidget( ... )
   RETURN Self


METHOD QDockWidget:allowedAreas()
   RETURN Qt_QDockWidget_allowedAreas( ::pPtr )


METHOD QDockWidget:features()
   RETURN Qt_QDockWidget_features( ::pPtr )


METHOD QDockWidget:isAreaAllowed( nArea )
   RETURN Qt_QDockWidget_isAreaAllowed( ::pPtr, nArea )


METHOD QDockWidget:isFloating()
   RETURN Qt_QDockWidget_isFloating( ::pPtr )


METHOD QDockWidget:setAllowedAreas( nAreas )
   RETURN Qt_QDockWidget_setAllowedAreas( ::pPtr, nAreas )


METHOD QDockWidget:setFeatures( nFeatures )
   RETURN Qt_QDockWidget_setFeatures( ::pPtr, nFeatures )


METHOD QDockWidget:setFloating( lFloating )
   RETURN Qt_QDockWidget_setFloating( ::pPtr, lFloating )


METHOD QDockWidget:setTitleBarWidget( pWidget )
   RETURN Qt_QDockWidget_setTitleBarWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDockWidget:setWidget( pWidget )
   RETURN Qt_QDockWidget_setWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDockWidget:titleBarWidget()
   RETURN Qt_QDockWidget_titleBarWidget( ::pPtr )


METHOD QDockWidget:toggleViewAction()
   RETURN Qt_QDockWidget_toggleViewAction( ::pPtr )


METHOD QDockWidget:widget()
   RETURN Qt_QDockWidget_widget( ::pPtr )

