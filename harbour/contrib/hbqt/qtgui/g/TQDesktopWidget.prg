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


FUNCTION QDesktopWidget( ... )
   RETURN HB_QDesktopWidget():new( ... )


CREATE CLASS QDesktopWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesktopWidget

   METHOD  new( ... )

   METHOD  availableGeometry( nScreen )
   METHOD  availableGeometry_1( pWidget )
   METHOD  availableGeometry_2( pP )
   METHOD  isVirtualDesktop()
   METHOD  numScreens()
   METHOD  primaryScreen()
   METHOD  screen( nScreen )
   METHOD  screenGeometry( nScreen )
   METHOD  screenGeometry_1( pWidget )
   METHOD  screenGeometry_2( pP )
   METHOD  screenNumber( pWidget )
   METHOD  screenNumber_1( pPoint )

   ENDCLASS


METHOD QDesktopWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesktopWidget( ... )
   RETURN Self


METHOD QDesktopWidget:availableGeometry( nScreen )
   RETURN Qt_QDesktopWidget_availableGeometry( ::pPtr, nScreen )


METHOD QDesktopWidget:availableGeometry_1( pWidget )
   RETURN Qt_QDesktopWidget_availableGeometry_1( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesktopWidget:availableGeometry_2( pP )
   RETURN Qt_QDesktopWidget_availableGeometry_2( ::pPtr, hbqt_ptr( pP ) )


METHOD QDesktopWidget:isVirtualDesktop()
   RETURN Qt_QDesktopWidget_isVirtualDesktop( ::pPtr )


METHOD QDesktopWidget:numScreens()
   RETURN Qt_QDesktopWidget_numScreens( ::pPtr )


METHOD QDesktopWidget:primaryScreen()
   RETURN Qt_QDesktopWidget_primaryScreen( ::pPtr )


METHOD QDesktopWidget:screen( nScreen )
   RETURN Qt_QDesktopWidget_screen( ::pPtr, nScreen )


METHOD QDesktopWidget:screenGeometry( nScreen )
   RETURN Qt_QDesktopWidget_screenGeometry( ::pPtr, nScreen )


METHOD QDesktopWidget:screenGeometry_1( pWidget )
   RETURN Qt_QDesktopWidget_screenGeometry_1( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesktopWidget:screenGeometry_2( pP )
   RETURN Qt_QDesktopWidget_screenGeometry_2( ::pPtr, hbqt_ptr( pP ) )


METHOD QDesktopWidget:screenNumber( pWidget )
   RETURN Qt_QDesktopWidget_screenNumber( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesktopWidget:screenNumber_1( pPoint )
   RETURN Qt_QDesktopWidget_screenNumber_1( ::pPtr, hbqt_ptr( pPoint ) )

