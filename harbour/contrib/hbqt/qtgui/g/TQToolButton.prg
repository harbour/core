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


FUNCTION QToolButton( ... )
   RETURN HB_QToolButton():new( ... )


CREATE CLASS QToolButton INHERIT HbQtObjectHandler, HB_QAbstractButton FUNCTION HB_QToolButton

   METHOD  new( ... )

   METHOD  arrowType()
   METHOD  autoRaise()
   METHOD  defaultAction()
   METHOD  menu()
   METHOD  popupMode()
   METHOD  setArrowType( nType )
   METHOD  setAutoRaise( lEnable )
   METHOD  setMenu( pMenu )
   METHOD  setPopupMode( nMode )
   METHOD  toolButtonStyle()
   METHOD  setDefaultAction( pAction )
   METHOD  setToolButtonStyle( nStyle )
   METHOD  showMenu()

   ENDCLASS


METHOD QToolButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolButton( ... )
   RETURN Self


METHOD QToolButton:arrowType()
   RETURN Qt_QToolButton_arrowType( ::pPtr )


METHOD QToolButton:autoRaise()
   RETURN Qt_QToolButton_autoRaise( ::pPtr )


METHOD QToolButton:defaultAction()
   RETURN Qt_QToolButton_defaultAction( ::pPtr )


METHOD QToolButton:menu()
   RETURN Qt_QToolButton_menu( ::pPtr )


METHOD QToolButton:popupMode()
   RETURN Qt_QToolButton_popupMode( ::pPtr )


METHOD QToolButton:setArrowType( nType )
   RETURN Qt_QToolButton_setArrowType( ::pPtr, nType )


METHOD QToolButton:setAutoRaise( lEnable )
   RETURN Qt_QToolButton_setAutoRaise( ::pPtr, lEnable )


METHOD QToolButton:setMenu( pMenu )
   RETURN Qt_QToolButton_setMenu( ::pPtr, hbqt_ptr( pMenu ) )


METHOD QToolButton:setPopupMode( nMode )
   RETURN Qt_QToolButton_setPopupMode( ::pPtr, nMode )


METHOD QToolButton:toolButtonStyle()
   RETURN Qt_QToolButton_toolButtonStyle( ::pPtr )


METHOD QToolButton:setDefaultAction( pAction )
   RETURN Qt_QToolButton_setDefaultAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QToolButton:setToolButtonStyle( nStyle )
   RETURN Qt_QToolButton_setToolButtonStyle( ::pPtr, nStyle )


METHOD QToolButton:showMenu()
   RETURN Qt_QToolButton_showMenu( ::pPtr )

