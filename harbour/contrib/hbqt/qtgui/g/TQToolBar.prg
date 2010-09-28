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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QToolBar( ... )
   RETURN HB_QToolBar():new( ... )


CREATE CLASS QToolBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QToolBar

   METHOD  new( ... )

   METHOD  actionAt( ... )
   METHOD  addAction( ... )
   METHOD  addSeparator()
   METHOD  addWidget( pWidget )
   METHOD  allowedAreas()
   METHOD  clear()
   METHOD  iconSize()
   METHOD  insertSeparator( pBefore )
   METHOD  insertWidget( pBefore, pWidget )
   METHOD  isAreaAllowed( nArea )
   METHOD  isFloatable()
   METHOD  isFloating()
   METHOD  isMovable()
   METHOD  orientation()
   METHOD  setAllowedAreas( nAreas )
   METHOD  setFloatable( lFloatable )
   METHOD  setMovable( lMovable )
   METHOD  setOrientation( nOrientation )
   METHOD  toggleViewAction()
   METHOD  toolButtonStyle()
   METHOD  widgetForAction( pAction )
   METHOD  setIconSize( pIconSize )
   METHOD  setToolButtonStyle( nToolButtonStyle )

   ENDCLASS


METHOD QToolBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBar( ... )
   RETURN Self


METHOD QToolBar:actionAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QAction():from( Qt_QToolBar_actionAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QAction():from( Qt_QToolBar_actionAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QToolBar:addAction( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN HB_QAction():from( Qt_QToolBar_addAction_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QAction():from( Qt_QToolBar_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN HB_QAction():from( Qt_QToolBar_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QAction():from( Qt_QToolBar_addAction_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_addAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QToolBar:addSeparator()
   RETURN HB_QAction():from( Qt_QToolBar_addSeparator( ::pPtr ) )


METHOD QToolBar:addWidget( pWidget )
   RETURN HB_QAction():from( Qt_QToolBar_addWidget( ::pPtr, hbqt_ptr( pWidget ) ) )


METHOD QToolBar:allowedAreas()
   RETURN Qt_QToolBar_allowedAreas( ::pPtr )


METHOD QToolBar:clear()
   RETURN Qt_QToolBar_clear( ::pPtr )


METHOD QToolBar:iconSize()
   RETURN HB_QSize():from( Qt_QToolBar_iconSize( ::pPtr ) )


METHOD QToolBar:insertSeparator( pBefore )
   RETURN HB_QAction():from( Qt_QToolBar_insertSeparator( ::pPtr, hbqt_ptr( pBefore ) ) )


METHOD QToolBar:insertWidget( pBefore, pWidget )
   RETURN HB_QAction():from( Qt_QToolBar_insertWidget( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pWidget ) ) )


METHOD QToolBar:isAreaAllowed( nArea )
   RETURN Qt_QToolBar_isAreaAllowed( ::pPtr, nArea )


METHOD QToolBar:isFloatable()
   RETURN Qt_QToolBar_isFloatable( ::pPtr )


METHOD QToolBar:isFloating()
   RETURN Qt_QToolBar_isFloating( ::pPtr )


METHOD QToolBar:isMovable()
   RETURN Qt_QToolBar_isMovable( ::pPtr )


METHOD QToolBar:orientation()
   RETURN Qt_QToolBar_orientation( ::pPtr )


METHOD QToolBar:setAllowedAreas( nAreas )
   RETURN Qt_QToolBar_setAllowedAreas( ::pPtr, nAreas )


METHOD QToolBar:setFloatable( lFloatable )
   RETURN Qt_QToolBar_setFloatable( ::pPtr, lFloatable )


METHOD QToolBar:setMovable( lMovable )
   RETURN Qt_QToolBar_setMovable( ::pPtr, lMovable )


METHOD QToolBar:setOrientation( nOrientation )
   RETURN Qt_QToolBar_setOrientation( ::pPtr, nOrientation )


METHOD QToolBar:toggleViewAction()
   RETURN HB_QAction():from( Qt_QToolBar_toggleViewAction( ::pPtr ) )


METHOD QToolBar:toolButtonStyle()
   RETURN Qt_QToolBar_toolButtonStyle( ::pPtr )


METHOD QToolBar:widgetForAction( pAction )
   RETURN HB_QWidget():from( Qt_QToolBar_widgetForAction( ::pPtr, hbqt_ptr( pAction ) ) )


METHOD QToolBar:setIconSize( pIconSize )
   RETURN Qt_QToolBar_setIconSize( ::pPtr, hbqt_ptr( pIconSize ) )


METHOD QToolBar:setToolButtonStyle( nToolButtonStyle )
   RETURN Qt_QToolBar_setToolButtonStyle( ::pPtr, nToolButtonStyle )

